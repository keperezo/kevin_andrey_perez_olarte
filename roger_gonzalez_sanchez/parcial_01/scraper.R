#  Comparador de precios: Medicamentos para alergias y síntomas respiratorios

#
#  Módulo de extracción. Carga los módulos de R/ y define:
#    ejecutar_pipeline()        recolección completa (en línea o sin conexión)
#    scrape_farmatodo(), scrape_cruzverde(), scrape_larebaja()
#    evaluar_viabilidad_comercios(), limpiar_entorno_pruebas()


local({
  ofiles <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  args_archivo <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  dir_script <- if (length(ofiles)) dirname(normalizePath(ofiles[[length(ofiles)]]))
                else if (length(args_archivo)) dirname(normalizePath(sub("^--file=", "", args_archivo[1])))
                else getwd()
  if (!file.exists(file.path(dir_script, "R", "00_configuracion.R"))) dir_script <- getwd()
  assign("DIR_PROYECTO", dir_script, envir = globalenv())
})

for (modulo in c("00_configuracion.R", "01_utilidades_http_raw.R", "02_estandarizacion_filtro.R",
                 "03_scrapers.R", "04_comparables.R", "05_viabilidad.R",
                 "06_base_datos.R", "07_consultas_taller.R")) {
  source(file.path(DIR_PROYECTO, "R", modulo), encoding = "UTF-8")
}


local({
  usa <- function(f, texto) exists(f) && any(grepl(texto, deparse(get(f)), fixed = TRUE))
  requisitos <- c(
    "00_configuracion.R"          = exists("COLUMNAS_EXTRA") && "disponible" %in% COLUMNAS_EXTRA,
    "01_utilidades_http_raw.R"    = exists("leer_manifiestos") && exists("entorno_actual"),
    "02_estandarizacion_filtro.R" = exists("contenido_multiple") && usa("estandarizar_crudo", "disponible"),
    "03_scrapers.R"               = exists("obtener_config_algolia") && exists("unir_ficha"),
    "04_comparables.R"            = usa("seleccionar_comparables", "comercios_disponibles"),
    "05_viabilidad.R"             = exists("leer_ultima_viabilidad"),
    "06_base_datos.R"             = exists("cerrar_ejecuciones_interrumpidas") && usa("insertar_observaciones", "COLUMNAS_EXTRA"),
    "07_consultas_taller.R"       = exists("sql_auditoria_calidad") && "c4b_canasta" %in% names(CONSULTAS_TALLER)
  )
  if (!all(requisitos)) {
    stop("Hay módulos de R/ desactualizados. Reemplace: ",
         paste(names(requisitos)[!requisitos], collapse = ", "), call. = FALSE)
  }
})

# PIPELINE PRINCIPAL
ejecutar_pipeline <- function(comercios = COMERCIOS_SELECCIONADOS,
                              test_mode = FALSE,
                              offline = FALSE,
                              fecha_offline = NULL,
                              id_ejecucion_offline = NULL,
                              db_path = DB_PATH,
                              max_paginas = Inf,
                              ejecutar_consultas = TRUE) {
  stopifnot(all(comercios %in% names(COMERCIOS)))
  id_ejecucion <- nuevo_id_ejecucion()
  modo <- if (offline) "offline" else if (test_mode) "test" else "online"
  log_msg("===== PIPELINE (", modo, ") | comercios: ", paste(comercios, collapse = ", "), " =====")

  # ---------------- MODO DE PRUEBAS: sin SQLite ----------------
  if (test_mode) {
    partes <- map(comercios, function(cl) {
      df <- FUNCIONES_SCRAPE[[cl]](test_mode = TRUE, offline = offline, id_ejecucion = id_ejecucion)
      filtrar_categoria_respiratorios(df)
    })
    auditoria <- map(partes, ~ attr(.x, "auditoria_filtro")) |> list_rbind()
    df <- list_rbind(partes)
    attr(df, "auditoria_filtro") <- auditoria
    log_msg(sprintf("MODO PRUEBA: %d registros incluidos de %d revisados. Nada se insertó en SQLite.",
                    nrow(df), nrow(auditoria)))
    if (!offline) log_msg("JSON crudo guardado en ", DIR_RAW_TEST)
    return(df)
  }

#ONLINE / OFFLINE: con SQLite
  con <- conectar_bd(db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  crear_esquema(con)
  ids_usados <- character(0)
  bitacora <- list()

  for (cl in comercios) {
    nombre <- COMERCIOS[[cl]]$nombre
    inicio <- ahora_iso_co()
    resultado <- tryCatch({
      df <- FUNCIONES_SCRAPE[[cl]](offline = offline, fecha_offline = fecha_offline,
                                   id_ejecucion = id_ejecucion, max_paginas = max_paginas,
                                   id_ejecucion_offline = id_ejecucion_offline)
      id_cl <- attr(df, "id_ejecucion") %||% id_ejecucion
      entorno <- entorno_actual()
      if (offline) {   # hora y entorno de la captura original (manifiesto)
        inicio <- attr(df, "inicio_captura") %||% inicio
        entorno <- attr(df, "entorno_captura") %||% NA_character_
      }
      df_f <- filtrar_categoria_respiratorios(df)
      # Reprocesar offline una ejecución que ya está en la base no reescribe su
      # registro original (modo, fecha, entorno): sólo se completan columnas vacías.
      ya_registrada <- offline && ejecucion_existe(con, id_cl, nombre)
      if (!ya_registrada) registrar_ejecucion(con, id_cl, nombre, inicio, nrow(df_f), "EN_CURSO", modo, entorno = entorno)
      n_ins <- insertar_observaciones(con, df_f)
      # Estado de la corrida (sección 5 del taller): completa / parcial / fallida.
      res <- attr(df, "resumen_peticiones") %||% tibble(http_status = integer())
      fallidas <- sum(is.na(res$http_status) | res$http_status >= 400)
      estado <- if (nrow(df_f) == 0) "FALLIDA" else if (fallidas > 0) "PARCIAL" else "COMPLETA"
      detalle <- sprintf("peticiones=%d; fallidas=%d; crudos=%d; incluidos_filtro=%d; insertados=%d; ya_existentes_UNIQUE=%d",
                         nrow(res), fallidas, nrow(df), nrow(df_f), n_ins, nrow(df_f) - n_ins)
      if (!ya_registrada) registrar_ejecucion(con, id_cl, nombre, inicio, nrow(df_f), estado, modo, detalle, entorno)
      log_msg("  -> ", nombre, ": ", detalle, if (ya_registrada) " (ejecución ya registrada: se conserva su registro)")
      list(id = id_cl, comercio = nombre, estado = estado, detalle = detalle)
    }, error = function(e) {
      log_msg("  ! ERROR en ", nombre, ": ", conditionMessage(e))
      registrar_ejecucion(con, id_ejecucion, nombre, inicio, 0L, "FALLIDA", modo, conditionMessage(e),
                          entorno_actual())
      list(id = id_ejecucion, comercio = nombre, estado = "FALLIDA", detalle = conditionMessage(e))
    })
    ids_usados <- c(ids_usados, resultado$id)
    bitacora[[cl]] <- as_tibble(resultado)
  }
  dbDisconnect(con); on.exit()

  ids_usados <- unique(ids_usados)
  comparables <- map(ids_usados, ~ marcar_productos_comparables(db_path, .x))
  consultas <- if (ejecutar_consultas) ejecutar_consultas_taller(db_path, tail(ids_usados, 1)) else NULL
  invisible(list(id_ejecucion = ids_usados, bitacora = list_rbind(bitacora),
                 comparables = comparables, consultas = consultas))
}

# Utilidades del entorno de pruebas

# Ensaya esquema, marcado y consultas con un data frame de prueba en la base
# TEMPORAL comparador_precios_test.sqlite.
cargar_bd_prueba <- function(df, db_test = DB_TEST_PATH) {
  con <- conectar_bd(db_test); on.exit(dbDisconnect(con))
  crear_esquema(con)
  for (com in unique(df$comercio)) {
    sub <- filter(df, comercio == com)
    registrar_ejecucion(con, sub$id_ejecucion[1], com, ahora_iso_co(), nrow(sub), "COMPLETA", "test")
    insertar_observaciones(con, sub)
  }
  dbDisconnect(con); on.exit()
  suppressWarnings(marcar_productos_comparables(db_test, df$id_ejecucion[1]))
  ejecutar_consultas_taller(db_test, df$id_ejecucion[1])
}

limpiar_entorno_pruebas <- function() {
  objetivos <- c(DB_TEST_PATH, paste0(DB_TEST_PATH, c("-journal", "-wal", "-shm")))
  borrados <- objetivos[file.exists(objetivos)]
  unlink(borrados, force = TRUE)
  carpeta <- dir.exists(DIR_RAW_TEST)
  unlink(DIR_RAW_TEST, recursive = TRUE, force = TRUE)
  log_msg("Entorno de pruebas limpio. Eliminado: ",
          paste(c(basename(borrados), if (carpeta) "raw/test/"), collapse = ", ") %||% "nada",
          if (!length(borrados) && !carpeta) "(no había nada que eliminar)" else "")
  invisible(c(borrados, if (carpeta) DIR_RAW_TEST))
}


# Ejecución desde la terminal:  Rscript scraper.R <comando>
if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  comando <- if (length(args)) args[1] else "ayuda"
  switch(comando,
    viabilidad = evaluar_viabilidad_comercios(),
    prueba     = { df <- ejecutar_pipeline(test_mode = TRUE)
                   print(as.data.frame(head(df, 15)), row.names = FALSE) },
    completo   = ejecutar_pipeline(),
    offline    = ejecutar_pipeline(offline = TRUE, fecha_offline = if (length(args) > 1) args[2] else NULL),
    consultas  = ejecutar_consultas_taller(),
    limpiar    = limpiar_entorno_pruebas(),
    cat("Uso: Rscript scraper.R viabilidad | prueba | completo | offline [AAAA-MM-DD] | consultas | limpiar\n")
  )
}
