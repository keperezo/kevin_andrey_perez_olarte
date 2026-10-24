# =============================================================
# correr_todo.R
# SCRIPT 3 de 3: orquestador. Llama en orden a las funciones de
#   scraper.R      (recolección, limpieza, SQLite)
#   comparables.R  (comparables, tablas, registro, verificación)
# Cada paso va protegido: si uno falla, imprime el error y sigue con el siguiente.
# =============================================================

# ---------------- CONFIGURACIÓN ----------------
MODO       <- "offline"   # "offline" = reprocesa raw/ (sin internet) | "online" = scraping nuevo
DB         <- "comparador_precios.sqlite"
N_OBJ      <- 12          # comparables a registrar (mínimo exigido: 8)
IDX        <- NULL        # NULL = los primeros N_OBJ (nivel A, B, C...); o c(1,2,5,...) = los que usted validó
REGISTRAR  <- TRUE        # TRUE = marca los comparables en SQLite
USAR_NIVEL_E <- FALSE     # TRUE = agrega candidatos más dudosos (sin exigir tipo de producto)

paso <- function(titulo, expr) {
  cat("\n", strrep("=", 70), "\n", titulo, "\n", strrep("=", 70), "\n", sep = "")
  tryCatch(expr, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))
}

# ================= PARTE 1: scraper.R =================

# 1. Recolección ----------------------------------------------------------
datos <- NULL
paso("1. ejecutar_recoleccion()", {
  datos <<- ejecutar_recoleccion(modo = MODO, db = DB)
  cat("Filas:", nrow(datos), "| columnas:", ncol(datos), "\n")
})

# 2. Archivos crudos --------------------------------------------------------
paso("2. Copias crudas en raw/", {
  f <- list.files("raw", recursive = TRUE)
  cat("Archivos:", length(f), "\n")
  print(table(sub("/.*", "", f)))
  print(head(f, 6))
})

# 3. Base de datos --------------------------------------------------------
paso("3. SQLite: ejecuciones y observaciones por comercio", {
  con <- dbConnect(SQLite(), DB); on.exit(dbDisconnect(con))
  print(dbGetQuery(con, "SELECT * FROM ejecuciones ORDER BY ejecucion_id DESC"))
  print(dbGetQuery(con, "SELECT ejecucion_id, comercio, COUNT(*) filas, SUM(incluido=1) incluidos,
                          SUM(incluido=1 AND precio_cop IS NULL) incluidos_sin_precio
                          FROM observaciones GROUP BY ejecucion_id, comercio"))
})

# 4. Cobertura vs total del sitio ----------------------------------------
paso("4. cobertura_por_termino()", print(cobertura_por_termino(db = DB), n = 60))

# 5. Calidad de los datos incluidos -----------------------------------------
paso("5. Calidad (solo incluidos)", {
  inc <- dplyr::filter(datos, incluido == 1)
  print(inc %>% dplyr::group_by(comercio) %>% dplyr::summarise(
    n = dplyr::n(), sin_precio = sum(is.na(precio_cop)), sin_marca = sum(is.na(marca_std)),
    sin_contenido = sum(is.na(contenido_total)), sin_sku = sum(is.na(sku)),
    en_promocion = sum(es_promocion, na.rm = TRUE), externos = sum(vendedor_externo, na.rm = TRUE),
    atipicos = sum(precio_atipico, na.rm = TRUE)))
  cat("\nMotivos de exclusión:\n")
  print(datos %>% dplyr::count(comercio, motivo_exclusion) %>% dplyr::arrange(comercio, dplyr::desc(n)), n = 60)
})

# 6. Revisión de exclusiones -----------------------------------------------
paso("6. exportar_revision() -> carpeta revision/", exportar_revision(datos))

# ================= PARTE 2: comparables.R =================

# 7. Comparables: techo, candidatos por niveles, tablas, registro y verificación
res <- NULL
paso("7. ejecutar_comparables()", {
  res <<- ejecutar_comparables(db = DB, n_obj = N_OBJ, registrar = REGISTRAR, idx = IDX,
                               usar_nivel_e = USAR_NIVEL_E)
})

# 8. Verificación de cobertura obligatoria ---------------------------------
paso("8. Comparables: ¿24 observaciones con precio en una misma ejecución?", {
  con <- dbConnect(SQLite(), DB); on.exit(dbDisconnect(con))
  print(dbGetQuery(con, "SELECT ejecucion_id, COUNT(*) obs, COUNT(DISTINCT producto_id) productos,
                          COUNT(DISTINCT comercio) comercios
                          FROM observaciones WHERE comparable=1 AND precio_cop IS NOT NULL
                          GROUP BY ejecucion_id"))
  print(dbGetQuery(con, "SELECT producto_id, comercio, precio_cop, nombre_original
                          FROM observaciones WHERE comparable=1 AND ejecucion_id=(SELECT MAX(ejecucion_id) FROM observaciones)
                          ORDER BY producto_id, comercio"), width = Inf)
})

# 9. Exportar la base a CSV y ver las tablas ------------------------------------
paso("9. Exportar CSV y ver tablas", {
  datos_csv <<- exportar_database_csv(DB, "database_export.csv")
  if (interactive()) {
    View(datos_csv)
    if (!is.null(res$ancha)) View(res$ancha)   # los 3 artículos en la misma fila
    if (!is.null(res$larga)) View(res$larga)   # un artículo por fila
  }
})

cat("\nListo. Pegue la salida completa para revisarla.\n")