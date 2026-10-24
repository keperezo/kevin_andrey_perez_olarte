# 06_base_datos.R  -  FASE 4: SQLite normalizado + marcado de comparables
# Tablas:
#   comercios          (catálogo de comercios)
#   ejecuciones        (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado, etc)
#   productos_comunes  (los 8 productos comparables y la presentación elegida)
#   observaciones      (13 columnas estándar + 6 de trazabilidad + comparable +
#                       id_producto_comun + familia_producto)
#                       con UNIQUE(comercio, sku_comercio, fecha_hora_consulta)

conectar_bd <- function(db_path = DB_PATH) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

SQL_ESQUEMA <- c(
  "CREATE TABLE IF NOT EXISTS comercios (
     comercio    TEXT PRIMARY KEY,
     clave       TEXT NOT NULL UNIQUE,
     plataforma  TEXT,
     url_base    TEXT
   )",
  "CREATE TABLE IF NOT EXISTS ejecuciones (
     id_ejecucion        TEXT NOT NULL,
     fecha_inicio        TEXT NOT NULL,
     comercio            TEXT NOT NULL REFERENCES comercios(comercio),
     registros_obtenidos INTEGER NOT NULL DEFAULT 0,
     estado              TEXT NOT NULL CHECK (estado IN ('EN_CURSO','COMPLETA','PARCIAL','FALLIDA')),
     modo                TEXT,
     detalle             TEXT,
     entorno             TEXT,
     PRIMARY KEY (id_ejecucion, comercio)
   )",
  "CREATE TABLE IF NOT EXISTS productos_comunes (
     id_producto_comun   TEXT PRIMARY KEY,
     descripcion         TEXT NOT NULL,
     alternativa_elegida TEXT,
     marca_estandarizada TEXT,
     contenido_num       REAL,
     unidad_medida       TEXT
   )",
  sprintf("CREATE TABLE IF NOT EXISTS observaciones (
     id_observacion           INTEGER PRIMARY KEY AUTOINCREMENT,
     id_ejecucion             TEXT NOT NULL,
     comercio                 TEXT NOT NULL,
     sku_comercio             TEXT NOT NULL,
     producto_nombre_original TEXT NOT NULL,
     marca_estandarizada      TEXT,
     subcategoria_estandar    TEXT CHECK (subcategoria_estandar IN (%s)),
     contenido_num            REAL,
     unidad_medida            TEXT CHECK (unidad_medida IS NULL OR unidad_medida IN (%s)),
     precio_cop               REAL NOT NULL CHECK (precio_cop > 0),
     precio_regular_cop       REAL,
     es_promocion             INTEGER NOT NULL CHECK (es_promocion IN (0, 1)),
     url_producto             TEXT,
     fecha_hora_consulta      TEXT NOT NULL,
     comparable               INTEGER NOT NULL DEFAULT 0 CHECK (comparable IN (0, 1)),
     id_producto_comun        TEXT REFERENCES productos_comunes(id_producto_comun),
     familia_producto         TEXT,
     presentacion_original    TEXT,
     descripcion_original     TEXT,
     ficha_tecnica_original   TEXT,
     categoria_origen         TEXT,
     url_consulta             TEXT,
     archivo_raw              TEXT,
     disponible               INTEGER CHECK (disponible IS NULL OR disponible IN (0, 1)),
     FOREIGN KEY (id_ejecucion, comercio) REFERENCES ejecuciones(id_ejecucion, comercio),
     CONSTRAINT uq_observacion UNIQUE (comercio, sku_comercio, fecha_hora_consulta)
   )",
   paste0("'", SUBCATEGORIAS_VALIDAS, "'", collapse = ", "),
   paste0("'", UNIDADES_VALIDAS, "'", collapse = ", ")),
  "CREATE INDEX IF NOT EXISTS ix_obs_ejecucion ON observaciones(id_ejecucion)",
  "CREATE INDEX IF NOT EXISTS ix_obs_comparable ON observaciones(id_producto_comun, comparable)"
)

crear_esquema <- function(con) {
  migrar_estados_ejecucion(con)
  walk(SQL_ESQUEMA, ~ dbExecute(con, .x))
  # Migración de bases creadas con versiones anteriores: agrega columnas nuevas
  # sin tocar las existentes (ALTER TABLE ... ADD COLUMN).
  agregar <- function(tabla, columnas) {
    faltan <- setdiff(columnas, dbListFields(con, tabla))
    walk(faltan, ~ dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s TEXT", tabla, .x)))
  }
  agregar("observaciones", COLUMNAS_TRAZABILIDAD)
  agregar("ejecuciones", "entorno")
  cerrar_ejecuciones_interrumpidas(con)
  if (!"disponible" %in% dbListFields(con, "observaciones")) {
    dbExecute(con, "ALTER TABLE observaciones ADD COLUMN disponible INTEGER")
  }
  cat_com <- tibble(comercio = map_chr(COMERCIOS, "nombre"), clave = names(COMERCIOS),
                    plataforma = map_chr(COMERCIOS, "plataforma"), url_base = map_chr(COMERCIOS, "url_base"))
  dbExecute(con, "INSERT OR IGNORE INTO comercios (comercio, clave, plataforma, url_base) VALUES (?, ?, ?, ?)",
            params = unname(as.list(cat_com)))
  invisible(TRUE)
}

# Estados de versiones anteriores del código -> estados del taller.
normalizar_estado <- function(estado) {
  equivalencias <- c(OK = "COMPLETA", SIN_DATOS = "FALLIDA", ERROR = "FALLIDA")
  ifelse(estado %in% names(equivalencias), equivalencias[estado], estado)
}

# Una ejecución que quedó "EN_CURSO" se interrumpió antes de registrar su estado
# final (corte de red, error, sesión cerrada). Se cierra sin borrar nada:
# PARCIAL si alcanzó a guardar observaciones, FALLIDA si no.
cerrar_ejecuciones_interrumpidas <- function(con) {
  dbExecute(con, "
    UPDATE ejecuciones SET
      registros_obtenidos = (SELECT COUNT(*) FROM observaciones o
                             WHERE o.id_ejecucion = ejecuciones.id_ejecucion AND o.comercio = ejecuciones.comercio),
      estado = CASE WHEN (SELECT COUNT(*) FROM observaciones o
                          WHERE o.id_ejecucion = ejecuciones.id_ejecucion AND o.comercio = ejecuciones.comercio) > 0
                    THEN 'PARCIAL' ELSE 'FALLIDA' END,
      detalle = COALESCE(detalle || '; ', '') || 'ejecución interrumpida antes de registrar su estado final'
    WHERE estado = 'EN_CURSO'")
}

registrar_ejecucion <- function(con, id_ejecucion, comercio, fecha_inicio, registros = 0L,
                                estado = "EN_CURSO", modo = NA, detalle = NA, entorno = NA) {
  estado <- normalizar_estado(estado)
  dbExecute(con,
    "INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado, modo, detalle, entorno)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT(id_ejecucion, comercio) DO UPDATE SET
       registros_obtenidos = excluded.registros_obtenidos, estado = excluded.estado,
       modo = excluded.modo, detalle = excluded.detalle,
       entorno = COALESCE(excluded.entorno, ejecuciones.entorno)",
    params = list(id_ejecucion, fecha_inicio, comercio, as.integer(registros), estado,
                  as.character(modo), as.character(detalle), as.character(entorno)))
}

# Bases de versiones anteriores usaban los estados OK / SIN_DATOS / ERROR.
# El taller pide completa / parcial / fallida: se reconstruye la tabla
# ejecuciones (SQLite no permite cambiar un CHECK con ALTER TABLE).
migrar_estados_ejecucion <- function(con) {
  if (!"ejecuciones" %in% dbListTables(con)) return(invisible(FALSE))
  ddl <- dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'ejecuciones'")$sql
  if (!str_detect(ddl, "SIN_DATOS")) return(invisible(FALSE))
  columnas <- dbListFields(con, "ejecuciones")
  dbExecute(con, "PRAGMA foreign_keys = OFF")
  on.exit(dbExecute(con, "PRAGMA foreign_keys = ON"), add = TRUE)
  dbWithTransaction(con, {
    dbExecute(con, "CREATE TABLE ejecuciones_migracion (
       id_ejecucion TEXT NOT NULL, fecha_inicio TEXT NOT NULL,
       comercio TEXT NOT NULL REFERENCES comercios(comercio),
       registros_obtenidos INTEGER NOT NULL DEFAULT 0,
       estado TEXT NOT NULL CHECK (estado IN ('EN_CURSO','COMPLETA','PARCIAL','FALLIDA')),
       modo TEXT, detalle TEXT, entorno TEXT,
       PRIMARY KEY (id_ejecucion, comercio))")
    dbExecute(con, sprintf(
      "INSERT INTO ejecuciones_migracion (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado, modo, detalle, entorno)
       SELECT id_ejecucion, fecha_inicio, comercio, registros_obtenidos,
              CASE estado WHEN 'OK' THEN 'COMPLETA' WHEN 'EN_CURSO' THEN 'EN_CURSO' ELSE 'FALLIDA' END,
              modo, detalle, %s FROM ejecuciones",
      if ("entorno" %in% columnas) "entorno" else "NULL"))
    dbExecute(con, "DROP TABLE ejecuciones")
    dbExecute(con, "ALTER TABLE ejecuciones_migracion RENAME TO ejecuciones")
  })
  invisible(TRUE)
}

ejecucion_existe <- function(con, id_ejecucion, comercio) {
  dbGetQuery(con, "SELECT COUNT(*) AS n FROM ejecuciones WHERE id_ejecucion = ? AND comercio = ?",
             params = list(id_ejecucion, comercio))$n > 0
}

# Inserta observaciones nuevas. Si la observación ya existe (misma
# UNIQUE(comercio, sku_comercio, fecha_hora_consulta), p. ej. al reprocesar la
# misma corrida en modo offline) NO se sobrescribe: sólo se completan columnas
# de trazabilidad que estuvieran vacías (bases creadas antes de existir esas
# columnas). Devuelve el número de filas nuevas.
insertar_observaciones <- function(con, df) {
  if (nrow(df) == 0) return(0L)
  for (cc in setdiff(COLUMNAS_TRAZABILIDAD, names(df))) df[[cc]] <- NA_character_
  if (!"disponible" %in% names(df)) df$disponible <- NA_integer_
  columnas <- c(COLUMNAS_ESTANDAR, COLUMNAS_EXTRA)
  df <- df |> select(all_of(columnas)) |>
    mutate(across(c(contenido_num, precio_cop, precio_regular_cop), as.numeric),
           es_promocion = as.integer(es_promocion),
           disponible = as.integer(disponible),
           across(all_of(COLUMNAS_TRAZABILIDAD), as.character))
  antes <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n
  completar <- paste0(COLUMNAS_EXTRA, " = COALESCE(observaciones.", COLUMNAS_EXTRA,
                      ", excluded.", COLUMNAS_EXTRA, ")", collapse = ", ")
  sql <- sprintf("INSERT INTO observaciones (%s) VALUES (%s)
                  ON CONFLICT(comercio, sku_comercio, fecha_hora_consulta) DO UPDATE SET %s",
                 paste(columnas, collapse = ", "),
                 paste(rep("?", length(columnas)), collapse = ", "), completar)
  dbWithTransaction(con, dbExecute(con, sql, params = unname(as.list(df))))
  dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n - antes
}

ultima_ejecucion <- function(con) {
  r <- dbGetQuery(con, "SELECT id_ejecucion FROM ejecuciones
                        GROUP BY id_ejecucion ORDER BY MAX(fecha_inicio) DESC, id_ejecucion DESC LIMIT 1")
  if (nrow(r) == 0) stop("La base no tiene ejecuciones registradas.")
  r$id_ejecucion[1]
}


# Marca comparable = 1 e id_producto_comun para los 8 productos en la ejecución
# indicada (por defecto, la última). Es idempotente: primero limpia las marcas
# previas de esa ejecución. Además:
#   * familia_producto: todas las presentaciones del mismo producto/marca
#     (insumo de la consulta 5, precio por unidad de contenido);
#   * homologa la unidad dentro de la familia (p. ej. Pax "unidad" -> "sobres").

marcar_productos_comparables <- function(db_path = DB_PATH, id_ejecucion = NULL, verbose = TRUE) {
  con <- conectar_bd(db_path); on.exit(dbDisconnect(con))
  op <- options(width = max(getOption("width"), 200)); on.exit(options(op), add = TRUE)
  id <- id_ejecucion %||% ultima_ejecucion(con)
  obs <- dbGetQuery(con, "SELECT id_observacion, comercio, sku_comercio, producto_nombre_original,
                                 marca_estandarizada, contenido_num, unidad_medida, precio_cop, disponible
                          FROM observaciones WHERE id_ejecucion = ?", params = list(id))
  comercios_ej <- sort(unique(obs$comercio))
  cand <- candidatos_comparables(obs)
  sel <- seleccionar_comparables(cand, comercios_ej)
  elegidos <- sel$elegidos %||% tibble()
  familias <- sel$familias %||% tibble()

  prod <- map(REGLAS_COMPARABLES, function(r) {
    e <- filter(sel$resumen, id_producto_comun == r$id)
    alt <- if (nrow(e) && !is.na(e$prioridad)) r$alternativas[[e$prioridad]] else r$alternativas[[1]]
    tibble(id_producto_comun = r$id, descripcion = r$descripcion,
           alternativa_elegida = if (nrow(e)) e$alternativa else NA_character_,
           marca_estandarizada = if (nrow(e)) e$marca_estandarizada else NA_character_,
           contenido_num = alt$contenido, unidad_medida = alt$unidades[1])
  }) |> list_rbind()

  dbWithTransaction(con, {
    dbExecute(con, "UPDATE observaciones SET comparable = 0, id_producto_comun = NULL, familia_producto = NULL
                    WHERE id_ejecucion = ?", params = list(id))
    dbExecute(con,
      "INSERT INTO productos_comunes (id_producto_comun, descripcion, alternativa_elegida, marca_estandarizada, contenido_num, unidad_medida)
       VALUES (?, ?, ?, ?, ?, ?)
       ON CONFLICT(id_producto_comun) DO UPDATE SET descripcion = excluded.descripcion,
         alternativa_elegida = excluded.alternativa_elegida, marca_estandarizada = excluded.marca_estandarizada,
         contenido_num = excluded.contenido_num, unidad_medida = excluded.unidad_medida",
      params = unname(as.list(prod)))
    if (nrow(familias) > 0) {
      fam <- familias |>
        distinct(id_observacion, .keep_all = TRUE) |>
        mutate(familia = str_replace(id_producto_comun, "^COMP_", "FAM_"),
               unidad_nueva = if_else(map2_lgl(unidad_medida, unidades_validas,
                                               ~ !is.na(.x) && .x %in% str_split(.y, "\\|")[[1]]),
                                      unidad_canonica, unidad_medida))
      dbExecute(con, "UPDATE observaciones SET familia_producto = ?, unidad_medida = ? WHERE id_observacion = ?",
                params = list(fam$familia, fam$unidad_nueva, fam$id_observacion))
    }
    if (nrow(elegidos) > 0) {
      dbExecute(con, "UPDATE observaciones SET comparable = 1, id_producto_comun = ? WHERE id_observacion = ?",
                params = list(elegidos$id_producto_comun, elegidos$id_observacion))
    }
  })

  n_comp <- nrow(elegidos)
  objetivo <- length(IDS_COMPARABLES) * max(3, length(comercios_ej))
  if (verbose) {
    cat("\n================ FASE 4 - PRODUCTOS COMPARABLES (", id, ") ================\n", sep = "")
    if (n_comp > 0) {
      detalle <- elegidos |>
        mutate(precio = format(precio_cop, big.mark = ".", decimal.mark = ",", scientific = FALSE)) |>
        group_by(id_producto_comun) |>
        summarise(marca = first(marca_estandarizada), n_comercios = n_distinct(comercio),
                  precios = paste0(comercio, " $", precio, collapse = " | "), .groups = "drop")
      print(as.data.frame(detalle), row.names = FALSE, right = FALSE)
    }
    cat(sprintf("\nObservaciones comparables marcadas: %d (objetivo: >= %d = 8 productos x %d comercios) -> %s\n",
                n_comp, objetivo, max(3, length(comercios_ej)),
                if (n_comp >= objetivo && all(sel$resumen$completo)) "CUMPLE" else "NO CUMPLE"))
  }
  incompletos <- filter(sel$resumen, !completo)
  if (nrow(incompletos) > 0) {
    warning("Productos sin cobertura completa en ", id, ": ",
            paste0(incompletos$id_producto_comun, " (", incompletos$cobertura, "/",
                   incompletos$comercios_requeridos, ")", collapse = ", "), call. = FALSE)
  }
  invisible(list(id_ejecucion = id, resumen = sel$resumen, elegidos = elegidos))
}
