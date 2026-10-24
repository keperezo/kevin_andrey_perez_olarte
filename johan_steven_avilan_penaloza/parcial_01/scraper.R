if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(rvest, httr2, jsonlite, DBI, RSQLite, dplyr, stringr, purrr, tibble, chromote)

# ---- Constantes de Farmatodo -----------------------------------------

sitio_farmatodo <- "https://www.farmatodo.com.co"
base_farmatodo  <- paste0(sitio_farmatodo,
                          "/categorias/salud-y-medicamentos/botiquin-y-primeros-auxilios")

# Subcategorías a recolectar. Quedan FUERA (no son de la categoría asignada):
# "jeringas-guantes-y-material-hosp" y "aparatos-medicos-y-pastilleros".
# La exclusión fina (guantes, tapabocas, etc.) se hace después, en la limpieza.
slugs_farmatodo <- c(
  "curitas-algodones-adhesivos-y-botiquin",
  "antisepticos",
  "vendas",
  "compresas"
)

# ---- Utilidades -------------------------------------------------------

ahora_iso <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "America/Bogota")
  sub("(\\d{2})$", ":\\1", x)
}

# ---- Parser: de cada tarjeta de producto a una fila -------------------

parsear_farmatodo <- function(pagina, subcategoria, id_ejecucion, ts) {
  tarjetas <- html_elements(pagina, "div.product-card-handler")
  if (length(tarjetas) == 0) return(tibble())

  map_dfr(tarjetas, function(t) {
    enlace <- html_element(t, "a.product-card-handler__detail-description")
    href_det <- html_attr(enlace, "href")
    href_img <- html_attr(html_element(t, "a.product-card-handler__image-link"), "href")
    href     <- dplyr::coalesce(href_det, href_img)
    sku_card <- str_match(html_attr(t, "data-test"), "product-card-(\\d+)")[, 2]
    sku      <- dplyr::coalesce(str_match(href, "/producto/(\\d+)")[, 2], sku_card)

    if (is.na(href)) {   
      cat("\n!! Tarjeta SIN enlace (", subcategoria, ") SKU por data-test:", sku_card, "\n",
          substr(as.character(t), 1, 1500), "\n")
    }

    bloque <- trimws(html_text2(html_element(t, "app-product-all-price")))
    montos <- as.numeric(gsub("[^0-9]", "",
                              str_extract_all(bloque, "\\$\\s*[0-9.]+")[[1]]))

    tibble(
      comercio               = "Farmatodo",
      sku                    = sku,
      nombre_original        = trimws(html_text(enlace)),
      marca_original         = trimws(html_text(html_element(t, ".product-card-handler__brand-value"))),
      subcategoria_origen    = subcategoria,
      # Sin precio publicado (producto agotado) => NA
      precio                 = if (length(montos) == 0) NA_real_ else min(montos),
      precio_antes           = if (length(montos) >= 2) max(montos) else NA_real_,
      es_promocion           = as.integer(length(montos) >= 2),  # SUPOSICIÓN: verificar con una oferta real
      precio_bloque_original = bloque,
      pum_texto              = trimws(html_text(html_element(t, ".product-card-handler__pum"))),
      vendedor               = str_match(html_text2(t), "Vendido por:\\s*([^\\n]+)")[, 2],
      url                    = if (is.na(href)) NA_character_ else paste0(sitio_farmatodo, href),
      fecha_hora_consulta    = ts,
      id_ejecucion           = id_ejecucion
    )
  })
}

# ---- Una subcategoría completa, con Chrome (read_html_live) -----------

scrape_subcat_live <- function(slug, id_ejecucion = "prueba", max_clics = 60,
                               dir_raw = NULL) {
  url <- paste0(base_farmatodo, "/", slug)
  ts  <- ahora_iso()

  s <- rvest::read_html_live(url)
  on.exit(s$session$close(), add = TRUE)
  Sys.sleep(5)

  # productos únicos en pantalla
  contar <- function() {
    s |> html_elements("a.product-card-handler__detail-description") |>
      html_attr("href") |> unique() |> length()
  }

  # pulsa el botón "Cargar más" buscándolo por su texto
  clic_cargar_mas <- function() {
    r <- s$session$Runtime$evaluate("
      (() => {
        const b = [...document.querySelectorAll('button')]
          .find(x => x.textContent.trim() === 'Cargar más');
        if (!b) return false;
        b.scrollIntoView();
        b.click();
        return true;
      })()
    ")
    isTRUE(r$result$value)
  }

  n_prev <- contar()
  cat("[", slug, "] inicial:", n_prev, "productos\n")

  for (i in seq_len(max_clics)) {
    if (!clic_cargar_mas()) { cat("   ya no hay botón\n"); break }
    Sys.sleep(3)
    n_act <- contar()
    if (n_act == n_prev) {            # confirmar: esperar más y recontar
      Sys.sleep(6)
      n_act <- contar()
      if (n_act == n_prev) { cat("   sin más productos tras esperar\n"); break }
    }
    cat("   clic", i, "->", n_act, "productos\n")
    n_prev <- n_act
  }

  # Copia cruda
  if (!is.null(dir_raw)) {
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
    html_crudo <- s$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    con <- gzfile(file.path(dir_raw, paste0(slug, ".html.gz")), "w")
    writeLines(html_crudo, con, useBytes = TRUE)
    close(con)
    writeLines(ts, file.path(dir_raw, paste0(slug, ".ts.txt")))   # fecha-hora de captura
    cat("   copia cruda guardada en", dir_raw, "\n")
  }

  df <- parsear_farmatodo(s, slug, id_ejecucion, ts)
  if (nrow(df) > 0) df <- distinct(df, sku, nombre_original, .keep_all = TRUE)
  cat("   FILAS FINALES:", nrow(df), "\n")
  df
}

# ---- MODO SIN CONEXIÓN (reprocesar una copia cruda) ---------------------

parsear_desde_raw <- function(archivo, slug, id_ejecucion = "offline") {
  html <- paste(readLines(gzfile(archivo), encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  pagina <- read_html(charToRaw(html), encoding = "UTF-8")
  # la fecha-hora es la de la captura original, no la de hoy
  archivo_ts <- sub("\\.html\\.gz$", ".ts.txt", archivo)
  ts <- if (file.exists(archivo_ts)) readLines(archivo_ts, warn = FALSE)[1] else NA_character_
  df <- parsear_farmatodo(pagina, slug, id_ejecucion, ts)
  if (nrow(df) > 0) df <- distinct(df, sku, nombre_original, .keep_all = TRUE)
  df
}

# ---- Recolección completa de Farmatodo con una llamada -----------------

recolectar_farmatodo <- function(id_ejecucion, dir_raw,
                                 modo = c("online", "offline"),
                                 slugs = slugs_farmatodo) {
  modo <- match.arg(modo)

  if (modo == "online") {
    # un fallo en una subcategoría no frena las demás, pero se muestra el error
    seguro <- possibly(scrape_subcat_live, otherwise = tibble(), quiet = FALSE)
    res <- map(slugs, function(s) seguro(s, id_ejecucion = id_ejecucion, dir_raw = dir_raw))
  } else {
    res <- map(slugs, function(s) {
      f <- file.path(dir_raw, paste0(s, ".html.gz"))
      if (!file.exists(f)) {
        warning("No existe la copia cruda: ", f)
        return(tibble())
      }
      parsear_desde_raw(f, s, id_ejecucion)
    })
  }

  names(res) <- slugs
  bind_rows(res)
}


# SCRAPPING DE LA REBAJA Y LOCATEL

vtex_url <- function(dominio, ruta) {
  paste0("https://", dominio, "/api/catalog_system/pub/products/search/", ruta)
}

# Descarga un rango de productos
descargar_vtex_pagina <- function(dominio, ruta, desde, hasta) {
  resp <- request(vtex_url(dominio, ruta)) |>
    req_url_query(`_from` = desde, `_to` = hasta) |>
    req_user_agent("Mozilla/5.0 (taller universitario)") |>
    req_retry(max_tries = 3) |>
    req_perform()
  cab   <- resp_header(resp, "resources")
  total <- if (is.null(cab)) NA_integer_ else as.integer(sub("^.*/", "", cab))
  list(texto = resp_body_string(resp, encoding = "UTF-8"), total = total)
}

# De la lista de productos a una fila por SKU. Cada campo que no exista
# queda en NA en lugar de producir un error.
parsear_vtex <- function(productos, comercio, dominio, id_ejecucion, ts) {
  map_dfr(productos, function(p) {
    cats <- unlist(p$categories)

    # Campos de especificación del producto (varían entre tiendas). Devuelve el
    # texto ORIGINAL del primer campo cuyo nombre coincida, o NA si no existe.
    campo <- function(patron) {
      nm <- names(p)[str_detect(names(p), regex(patron, ignore_case = TRUE))]
      if (length(nm) == 0) return(NA_character_)
      paste(unlist(p[[nm[1]]]), collapse = " | ")
    }
    presentacion <- campo("^Presentaci[oó]n$")                    
    contenido_cant   <- dplyr::coalesce(campo("^Cantidadunidadesmedida$"), campo("^PUM - Medida$"))
    contenido_unidad <- dplyr::coalesce(campo("^Unidadmedida$"), campo("^PUM - Unidad de Medida$"))
    pres_texto       <- campo("^Presentacionunidadmedida$")
    unidades_pack    <- campo("^ML_UnidadesPorPack$")
    peso_origen      <- campo("^Peso$")                               

    map_dfr(p$items, function(it) {
      venta  <- pluck(it, "sellers", 1, .default = list())
      oferta <- pluck(venta, "commertialOffer", .default = list())

      precio_vtex <- suppressWarnings(as.numeric(pluck(oferta, "Price", .default = NA)))
      lista_vtex  <- suppressWarnings(as.numeric(pluck(oferta, "ListPrice", .default = NA)))
      stock       <- suppressWarnings(as.numeric(pluck(oferta, "AvailableQuantity", .default = NA)))

      # Sin precio vigente (agotado) => NA, nunca 0
      precio <- if (!is.na(precio_vtex) && precio_vtex > 0) precio_vtex else NA_real_
      # Promoción: el precio de lista es mayor que el precio de venta
      antes  <- if (!is.na(precio) && !is.na(lista_vtex) && lista_vtex > precio) lista_vtex else NA_real_

      tibble(
        comercio              = comercio,
        id_producto           = as.character(pluck(p, "productId", .default = NA)),
        sku                   = as.character(pluck(it, "itemId", .default = NA)),
        ean                   = as.character(pluck(it, "ean", .default = NA)),
        nombre_original       = as.character(pluck(p, "productName", .default = NA)),
        variante_original     = as.character(pluck(it, "name", .default = NA)),
        descripcion_original  = as.character(pluck(p, "description", .default = NA)),
        marca_original        = as.character(pluck(p, "brand", .default = NA)),
        presentacion_original = presentacion,
        presentacion_texto_origen = pres_texto,
        contenido_origen      = contenido_cant,
        contenido_unidad_origen = contenido_unidad,
        unidades_pack_origen  = unidades_pack,
        peso_origen           = peso_origen,
        subcategoria_origen   = if (length(cats) > 0) cats[1] else NA_character_,
        categorias_origen     = if (length(cats) > 0) paste(cats, collapse = " ; ") else NA_character_,
        precio                = precio,
        precio_antes          = antes,
        es_promocion          = as.integer(!is.na(antes)),  
        disponible            = as.integer(!is.na(stock) && stock > 0),
        vendedor              = as.character(pluck(venta, "sellerName", .default = NA)),
        url                   = as.character(pluck(p, "link", .default = NA)),
        fecha_hora_consulta   = ts,
        id_ejecucion          = id_ejecucion
      )
    })
  })
}

# Recolección completa de un comercio VTEX con UNA llamada.
#   modo "online":  consulta el servicio, guarda cada respuesta en dir_raw
#   modo "offline": reprocesa lo que haya en dir_raw, sin consultar el sitio
# El total que declara el sitio queda en attr(resultado, "total_sitio").
recolectar_vtex <- function(comercio, dominio, ruta, id_ejecucion, dir_raw,
                            modo = c("online", "offline"), paso = 50L) {
  modo  <- match.arg(modo)
  total <- NA_integer_

  if (modo == "online") {
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
    unlink(list.files(dir_raw, pattern = "^pag_\\d+\\.json(\\.gz)?$", full.names = TRUE))  # evita mezclar corridas
    ts <- ahora_iso()
    writeLines(ts, file.path(dir_raw, "ts.txt"))

    desde <- 0L
    repeat {
      hasta <- desde + paso - 1L
      r <- descargar_vtex_pagina(dominio, ruta, desde, hasta)
      total <- r$total
      con_gz <- gzfile(file.path(dir_raw, sprintf("pag_%04d.json.gz", desde)), "w")   # copia cruda comprimida
      writeLines(r$texto, con_gz, useBytes = TRUE)
      close(con_gz)
      cat("  [", comercio, "] ", desde, "-", hasta, " de ", total, "\n", sep = "")
      if (is.na(total) || nchar(r$texto) <= 2 || hasta + 1L >= total) break
      desde <- hasta + 1L
      Sys.sleep(1)
    }
    writeLines(as.character(total), file.path(dir_raw, "total_sitio.txt"))   # total declarado por el sitio
  } else {
    tot_file <- file.path(dir_raw, "total_sitio.txt")
    total <- if (file.exists(tot_file)) suppressWarnings(as.integer(readLines(tot_file, warn = FALSE)[1])) else NA_integer_
    ts_file <- file.path(dir_raw, "ts.txt")
    ts <- if (file.exists(ts_file)) readLines(ts_file, warn = FALSE)[1] else NA_character_
  }

  archivos <- sort(list.files(dir_raw, pattern = "^pag_\\d+\\.json(\\.gz)?$", full.names = TRUE))
  if (length(archivos) == 0) {
    warning("No hay copias crudas en ", dir_raw)
    return(tibble())
  }
  productos <- unlist(map(archivos, function(f) {
    txt <- paste(readLines(gzfile(f), encoding = "UTF-8", warn = FALSE), collapse = "\n") 
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }), recursive = FALSE)

  df <- parsear_vtex(productos, comercio, dominio, id_ejecucion, ts)
  attr(df, "total_sitio") <- total
  df
}

# Funciones por comercio

columnas_estandar <- c(
  "comercio", "id_producto", "sku", "ean",
  "nombre_original", "variante_original", "descripcion_original",
  "marca_original", "presentacion_original", "presentacion_texto_origen",
  "contenido_origen", "contenido_unidad_origen", "unidades_pack_origen", "peso_origen",
  "subcategoria_origen", "categorias_origen",
  "precio", "precio_antes", "es_promocion", "disponible", "vendedor",
  "precio_texto_origen", "url", "fecha_hora_consulta", "id_ejecucion"
)

estandarizar_columnas <- function(df) {
  total <- attr(df, "total_sitio")
  if (nrow(df) > 0) {
    if (!"id_producto" %in% names(df)) df$id_producto <- df$sku
    if (!"disponible"  %in% names(df)) df$disponible  <- as.integer(!is.na(df$precio))
    if (!"precio_texto_origen" %in% names(df)) {
      df$precio_texto_origen <- if ("precio_bloque_original" %in% names(df)) df$precio_bloque_original else NA_character_
    }
  }
  for (col in setdiff(columnas_estandar, names(df))) df[[col]] <- rep(NA, nrow(df))
  df <- df[, columnas_estandar]
  numericas <- c("precio", "precio_antes")
  enteras   <- c("es_promocion", "disponible")
  texto     <- setdiff(columnas_estandar, c(numericas, enteras))
  df[texto]     <- lapply(df[texto], as.character)
  df[numericas] <- lapply(df[numericas], as.numeric)
  df[enteras]   <- lapply(df[enteras], as.integer)
  attr(df, "total_sitio") <- total
  df
}

scrape_farmatodo <- function(id_ejecucion, dir_raw, modo = c("online", "offline")) {
  modo <- match.arg(modo)
  estandarizar_columnas(recolectar_farmatodo(id_ejecucion, dir_raw, modo))
}

scrape_la_rebaja <- function(id_ejecucion, dir_raw, modo = c("online", "offline")) {
  modo <- match.arg(modo)
  estandarizar_columnas(recolectar_vtex("La Rebaja", "www.larebajavirtual.com",
                                        "drogueria/cuidado-de-la-herida", id_ejecucion, dir_raw, modo))
}

scrape_locatel <- function(id_ejecucion, dir_raw, modo = c("online", "offline")) {
  modo <- match.arg(modo)
  estandarizar_columnas(recolectar_vtex("Locatel", "www.locatelcolombia.com",
                                        "drogueria/cuidado-heridas", id_ejecucion, dir_raw, modo))
}


# BASE DE DATOS SQLITE

crear_base <- function(ruta_db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_db)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  sentencias <- c(
    "CREATE TABLE IF NOT EXISTS comercios (
       id_comercio INTEGER PRIMARY KEY AUTOINCREMENT,
       nombre      TEXT NOT NULL UNIQUE
     )",

    "CREATE TABLE IF NOT EXISTS productos_comparables (
       id_comparable         INTEGER PRIMARY KEY AUTOINCREMENT,
       nombre_comun          TEXT NOT NULL,
       marca_comun           TEXT,
       contenido_valor       REAL,
       contenido_unidad      TEXT,
       regla_correspondencia TEXT
     )",

    "CREATE TABLE IF NOT EXISTS anuncios (
       id_anuncio                INTEGER PRIMARY KEY AUTOINCREMENT,
       id_comercio               INTEGER NOT NULL REFERENCES comercios(id_comercio),
       sku                       TEXT NOT NULL,
       ean                       TEXT,
       nombre_original           TEXT NOT NULL,
       descripcion_original      TEXT,
       marca_original            TEXT,
       presentacion_texto_origen TEXT,
       contenido_origen          TEXT,
       contenido_unidad_origen   TEXT,
       subcategoria_origen       TEXT,
       url                       TEXT,
       contenido_valor           REAL,
       contenido_unidad          TEXT,
       comparable                INTEGER NOT NULL DEFAULT 0,
       id_comparable             INTEGER REFERENCES productos_comparables(id_comparable),
       incluido                  INTEGER NOT NULL DEFAULT 1,
       motivo_exclusion          TEXT,
       nombre_estandar           TEXT,
       marca                     TEXT,
       subcategoria              TEXT,
       UNIQUE (id_comercio, sku)
     )",

    "CREATE TABLE IF NOT EXISTS ejecuciones (
       id_ejecucion      TEXT PRIMARY KEY,
       id_comercio       INTEGER NOT NULL REFERENCES comercios(id_comercio),
       fecha_hora_inicio TEXT NOT NULL,
       registros         INTEGER,
       productos         INTEGER,
       total_sitio       INTEGER,
       estado            TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida'))
     )",

    "CREATE TABLE IF NOT EXISTS observaciones (
       id_observacion      INTEGER PRIMARY KEY AUTOINCREMENT,
       id_anuncio          INTEGER NOT NULL REFERENCES anuncios(id_anuncio),
       id_ejecucion        TEXT NOT NULL REFERENCES ejecuciones(id_ejecucion),
       fecha_hora_consulta TEXT NOT NULL,
       precio              REAL,
       precio_antes        REAL,
       es_promocion        INTEGER,
       disponible          INTEGER,
       vendedor            TEXT,
       UNIQUE (id_anuncio, fecha_hora_consulta)
     )"
  )
  for (s in sentencias) DBI::dbExecute(con, s)
  con
}

# completa / parcial / fallida
estado_ejecucion <- function(df, total_sitio = NA_integer_, n_subcats_esperadas = NA_integer_) {
  if (nrow(df) == 0) return("fallida")
  if (!is.na(total_sitio) && "id_producto" %in% names(df) &&
      n_distinct(df$id_producto) < total_sitio) return("parcial")
  if (!is.na(n_subcats_esperadas) &&
      n_distinct(df$subcategoria_origen) < n_subcats_esperadas) return("parcial")
  "completa"
}

# Registra la corrida de un comercio y devuelve el id del comercio
registrar_ejecucion <- function(con, comercio, id_ejecucion, ts_inicio,
                                registros, total_sitio, estado, productos = NA_integer_) {
  DBI::dbExecute(con, "INSERT OR IGNORE INTO comercios (nombre) VALUES (?)",
                 params = list(comercio))
  id_com <- DBI::dbGetQuery(con, "SELECT id_comercio FROM comercios WHERE nombre = ?",
                            params = list(comercio))$id_comercio
  DBI::dbExecute(con, "
    INSERT INTO ejecuciones (id_ejecucion, id_comercio, fecha_hora_inicio, registros, productos, total_sitio, estado)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT (id_ejecucion) DO UPDATE SET
      fecha_hora_inicio = excluded.fecha_hora_inicio,
      registros         = excluded.registros,
      productos         = excluded.productos,
      total_sitio       = excluded.total_sitio,
      estado            = excluded.estado",
    params = list(id_ejecucion, id_com, ts_inicio, as.integer(registros),
                  as.integer(productos), as.integer(total_sitio), estado))
  id_com
}

# Inserta anuncios y observaciones de una tabla de productos.
# Devuelve cuántas observaciones NUEVAS se insertaron.
cargar_observaciones <- function(con, df, id_com, id_ejecucion) {
  cols <- c("sku", "ean", "nombre_original", "descripcion_original", "marca_original",
            "presentacion_texto_origen", "contenido_origen", "contenido_unidad_origen",
            "subcategoria_origen", "url", "precio", "precio_antes", "es_promocion",
            "disponible", "vendedor", "fecha_hora_consulta")
  for (f in setdiff(cols, names(df))) df[[f]] <- NA          # columnas que un comercio no trae
  if (all(is.na(df$disponible))) df$disponible <- as.integer(!is.na(df$precio))
  carga <- as.data.frame(df[, cols])
  carga$id_comercio  <- id_com
  carga$id_ejecucion <- id_ejecucion

  DBI::dbWriteTable(con, "tmp_carga", carga, temporary = TRUE, overwrite = TRUE)

  DBI::dbExecute(con, "
    INSERT OR IGNORE INTO anuncios
      (id_comercio, sku, ean, nombre_original, descripcion_original, marca_original,
       presentacion_texto_origen, contenido_origen, contenido_unidad_origen,
       subcategoria_origen, url)
    SELECT id_comercio, sku, ean, nombre_original, descripcion_original, marca_original,
           presentacion_texto_origen, contenido_origen, contenido_unidad_origen,
           subcategoria_origen, url
    FROM tmp_carga")

  antes <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n
  DBI::dbExecute(con, "
    INSERT OR IGNORE INTO observaciones
      (id_anuncio, id_ejecucion, fecha_hora_consulta, precio, precio_antes,
       es_promocion, disponible, vendedor)
    SELECT a.id_anuncio, t.id_ejecucion, t.fecha_hora_consulta, t.precio, t.precio_antes,
           t.es_promocion, t.disponible, t.vendedor
    FROM tmp_carga t
    JOIN anuncios a ON a.id_comercio = t.id_comercio AND a.sku = t.sku")
  despues <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n

  DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_carga")
  despues - antes
}

# Registra la ejecución y carga una tabla ya obtenida (en memoria) de un comercio
cargar_ejecucion <- function(con, df, comercio, ts_inicio = NULL, estado = NULL,
                             n_subcats_esperadas = NA_integer_) {
  total <- attr(df, "total_sitio")
  if (is.null(total)) total <- NA_integer_
  id_ejecucion <- if (nrow(df) > 0) df$id_ejecucion[1] else paste0(comercio, "_fallida_", format(Sys.time(), "%Y%m%d%H%M%S"))
  if (is.null(ts_inicio)) ts_inicio <- if (nrow(df) > 0) min(df$fecha_hora_consulta, na.rm = TRUE) else ahora_iso()
  if (is.null(estado)) estado <- estado_ejecucion(df, total, n_subcats_esperadas)

  productos <- if (nrow(df) > 0 && "id_producto" %in% names(df)) length(unique(df$id_producto)) else nrow(df)
  id_com <- registrar_ejecucion(con, comercio, id_ejecucion, ts_inicio, nrow(df), total, estado, productos)
  nuevas <- if (nrow(df) > 0) cargar_observaciones(con, df, id_com, id_ejecucion) else 0L
  cat("  [", comercio, "] ", estado, ": ", nrow(df), " filas, ", nuevas, " observaciones nuevas\n", sep = "")
  invisible(nuevas)
}


# FUNCIÓN GENERAL: recolecta los tres comercios, registra cada ejecución
# e inserta los datos en la base SQLite. UNA sola llamada.


ejecutar_recoleccion_completa <- function(dir_proyecto,
                                          ruta_db = file.path(dir_proyecto, "comparador_precios.sqlite"),
                                          modo = c("online", "offline"),
                                          fecha = format(Sys.time(), "%Y-%m-%d", tz = "America/Bogota")) {
  modo <- match.arg(modo)
  con  <- crear_base(ruta_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  trabajos <- list(
    list(comercio = "Farmatodo", carpeta = "farmatodo", id = paste0("farmatodo_", fecha),
         n_sub = length(slugs_farmatodo),
         f = function(id, dir) scrape_farmatodo(id, dir, modo)),
    list(comercio = "La Rebaja", carpeta = "la_rebaja", id = paste0("larebaja_", fecha),
         n_sub = NA_integer_,
         f = function(id, dir) scrape_la_rebaja(id, dir, modo)),
    list(comercio = "Locatel", carpeta = "locatel", id = paste0("locatel_", fecha),
         n_sub = NA_integer_,
         f = function(id, dir) scrape_locatel(id, dir, modo))
  )

  resultados <- list()
  for (tr in trabajos) {
    cat("\n== ", tr$comercio, " (", modo, ") ==\n", sep = "")
    ts_inicio <- ahora_iso()
    dir_raw   <- file.path(dir_proyecto, "raw", tr$carpeta, fecha)
    if (modo == "offline" && !dir.exists(dir_raw)) {
      cat("  sin copia cruda para esta fecha; se omite\n")
      next
    }
    df <- tryCatch(tr$f(tr$id, dir_raw), error = function(e) {
      message("FALLÓ ", tr$comercio, ": ", conditionMessage(e))
      tibble()
    })
    cargar_ejecucion(con, df, tr$comercio, ts_inicio = ts_inicio,
                     n_subcats_esperadas = tr$n_sub)
    resultados[[tr$comercio]] <- df
  }
  aplicar_exclusiones(con)        # delimitación de la categoría
  estandarizar_anuncios(con)      # marca, nombre y subcategoría estandarizados
  invisible(resultados)
}

# DELIMITACIÓN DE LA CATEGORÍA Y CORRESPONDENCIA DE PRODUCTOS

reglas_exclusion <- tibble::tribble(
  ~motivo,                                                  ~campo,                ~patron,
  "Ostomías (bolsas y accesorios), fuera de la categoría",  "subcategoria_origen", "ostomi",
  "Bolsas de drenaje o de ostomía",                         "nombre_original",     "bolsa.*(drenaje|orina|colostom|ileostom|ostom)|drenaje",
  "Guantes, tapabocas y mascarillas (protección personal)", "nombre_original",     "guante|tapaboca|mascarilla|cubrebocas",
  "Jeringas y agujas",                                      "nombre_original",     "jeringa|aguja",
  "Instrumentos de medición o diagnóstico",                 "nombre_original",     "pulsiox|term[oó]metro|tensi[oó]metro|nebuliz|glucom",
  "Removedores de adhesivo y limpiadores de piel",          "nombre_original",     "removedor|removed",
  "Copitos e hisopos (cuidado personal)",                   "nombre_original",     "copito|hisopo",
  "Antisépticos de manos (no son para heridas)",            "nombre_original",     "antibacterial|glicerinad|desinfectante de manos"
)

# Marca en anuncios.incluido / motivo_exclusion según las reglas anteriores
aplicar_exclusiones <- function(con) {
  a <- DBI::dbGetQuery(con, "SELECT id_anuncio, nombre_original, subcategoria_origen FROM anuncios")
  a$motivo <- NA_character_
  for (i in seq_len(nrow(reglas_exclusion))) {
    r   <- reglas_exclusion[i, ]
    txt <- dplyr::coalesce(a[[r$campo]], "")
    hit <- str_detect(txt, regex(r$patron, ignore_case = TRUE)) & is.na(a$motivo)
    a$motivo[hit] <- r$motivo
  }
  DBI::dbExecute(con, "UPDATE anuncios SET incluido = 1, motivo_exclusion = NULL")
  excl <- a[!is.na(a$motivo), ]
  if (nrow(excl) > 0) {
    DBI::dbExecute(con, "UPDATE anuncios SET incluido = 0, motivo_exclusion = ? WHERE id_anuncio = ?",
                   params = list(excl$motivo, excl$id_anuncio))
  }
  invisible(nrow(excl))
}

# Carga la tabla de correspondencia (comparables.csv, una fila por producto y
# comercio) en productos_comparables y la enlaza con anuncios.

# Columnas del CSV: id_comparable, nombre_comun, marca_comun, contenido_valor,
# contenido_unidad, comercio, sku, regla
cargar_comparables <- function(con, ruta_csv) {
  if (!file.exists(ruta_csv)) stop("Falta el archivo de correspondencia: ", ruta_csv)
  cmp <- utils::read.csv(ruta_csv, stringsAsFactors = FALSE, encoding = "UTF-8",
                         colClasses = c(sku = "character", ean = "character"),
                         na.strings = c("", "NA"))
  necesarias <- c("id_comparable", "nombre_comun", "marca_comun", "contenido_valor",
                  "contenido_unidad", "comercio", "sku", "ean", "regla")
  stopifnot(all(necesarias %in% names(cmp)))
  cargar_comparables_df(con, cmp)
}

# Enlaza una tabla de correspondencia (data.frame) con la base
cargar_comparables_df <- function(con, cmp) {
  DBI::dbExecute(con, "UPDATE anuncios SET id_comparable = NULL, comparable = 0,
                       contenido_valor = NULL, contenido_unidad = NULL")
  DBI::dbExecute(con, "DELETE FROM productos_comparables")

  prod <- cmp |>
    group_by(id_comparable) |>
    summarise(nombre_comun = first(nombre_comun), marca_comun = first(marca_comun),
              contenido_valor = first(contenido_valor), contenido_unidad = first(contenido_unidad),
              regla = paste(unique(regla), collapse = " | "), .groups = "drop")
  for (i in seq_len(nrow(prod))) {
    DBI::dbExecute(con, "INSERT INTO productos_comparables
        (id_comparable, nombre_comun, marca_comun, contenido_valor, contenido_unidad, regla_correspondencia)
        VALUES (?, ?, ?, ?, ?, ?)", params = unname(as.list(prod[i, ])))
  }

  # Cada fila se enlaza por SKU (si se indica) o, si no, por EAN
  for (i in seq_len(nrow(cmp))) {
    por_sku <- !is.na(cmp$sku[i]) && nzchar(cmp$sku[i])
    campo   <- if (por_sku) "sku" else "ean"
    valor   <- if (por_sku) cmp$sku[i] else cmp$ean[i]
    n <- DBI::dbExecute(con, paste0("UPDATE anuncios
        SET id_comparable = ?, comparable = 1, contenido_valor = ?, contenido_unidad = ?
        WHERE ", campo, " = ? AND id_comercio = (SELECT id_comercio FROM comercios WHERE nombre = ?)"),
        params = list(cmp$id_comparable[i], cmp$contenido_valor[i], cmp$contenido_unidad[i],
                      valor, cmp$comercio[i]))
    if (n == 0) warning("No se encontró ", toupper(campo), " ", valor, " en ", cmp$comercio[i])
    if (n > 1)  warning(toupper(campo), " ", valor, " en ", cmp$comercio[i],
                        " corresponde a ", n, " anuncios; debe ser uno solo")
  }

  cobertura <- DBI::dbGetQuery(con, "
    SELECT p.id_comparable, p.nombre_comun, COUNT(DISTINCT a.id_comercio) AS comercios
    FROM productos_comparables p
    LEFT JOIN anuncios a ON a.id_comparable = p.id_comparable
    GROUP BY p.id_comparable, p.nombre_comun")
  if (any(cobertura$comercios < 3)) warning("Hay comparables que no están en los 3 comercios")
  invisible(cobertura)
}


# CORRESPONDENCIA POR REGLAS 
# Cada producto comparable se define así:
#  - La Rebaja y Locatel: por EAN (código de barras). Mismo EAN = mismo producto.
#  - Farmatodo (no publica EAN en el listado): por una expresión regular sobre
#    el nombre original, que debe identificar UN solo anuncio (marca, producto
#    y medida). Si no identifica exactamente uno, el proceso se detiene.

especificacion_comparables <- tibble::tribble(
  ~id_comparable, ~nombre_comun,                                                  ~marca_comun, ~contenido_valor, ~contenido_unidad, ~ean,            ~patron_farmatodo,
  1, "Alcohol antiséptico MK botella 700 mL",                                     "MK",         700,              "mL",              "7702057075101", "alcohol mk.*700 ?ml",
  2, "Micropore Nexcare (3M) color piel 12 mm x 5 m",                             "Nexcare",    5,                "m",               "7702098400184", "micropore.*nexcare.*beige.*12 ?mm",
  3, "Micropore Nexcare (3M) color piel 24 mm x 5 m",                             "Nexcare",    5,                "m",               "7702098400191", "micropore.*nexcare.*beige.*24 ?mm",
  4, "Curas Cure Band Standard caja x 10",                                        "CureBand",   10,               "unidad",          "7702136646628", "cura cure band standard.*x 10 und",
  5, "Microporo Cure Band color piel 5 cm x 4,5 m (2 x 5 yd)",                    "CureBand",   4.5,              "m",               "7702136648325", "microporo cure band.*5 ?cm",
  6, "Cinta de fijación Fixo Roll Cure Band 10 cm x 2 m",                         "CureBand",   2,                "m",               "7702136663366", "fixo roll.*10 ?cm",
  7, "Gasa estéril no tejida Alfa Safe 7,5 x 7,5 cm (3 x 3 pulg) caja x 12 sobres","Alfa Safe",  12,               "sobre",           "7707228362288", "gasa alfa.*no tejida.*3x3",
  8, "Cinta Transpore Nexcare (3M) transparente 24 mm x 5 m",                     "Nexcare",    5,                "m",               "7702098400153", "nexcare transpore.*24"
)

# Aplica las reglas sobre los anuncios de la base y carga la correspondencia.
resolver_comparables <- function(con, especificacion = especificacion_comparables, ruta_csv = NULL) {
  anuncios <- DBI::dbGetQuery(con, "
    SELECT c.nombre AS comercio, a.sku, a.ean, a.nombre_original
    FROM anuncios a JOIN comercios c ON c.id_comercio = a.id_comercio")

  unico <- function(df, e, comercio, criterio) {
    if (nrow(df) != 1) {
      stop("Regla del comparable ", e$id_comparable, " (", e$nombre_comun, ") en ", comercio,
           ": se esperaba 1 anuncio y hay ", nrow(df), " con ", criterio,
           if (nrow(df) > 0) paste0("\n  ", df$sku, " | ", df$nombre_original, collapse = "") else "")
    }
    df
  }

  filas <- list()
  for (i in seq_len(nrow(especificacion))) {
    e <- especificacion[i, ]
    for (cm in c("La Rebaja", "Locatel")) {
      r <- anuncios[anuncios$comercio == cm & !is.na(anuncios$ean) & anuncios$ean == e$ean, ]
      r <- unico(r, e, cm, paste0("EAN ", e$ean))
      filas[[length(filas) + 1]] <- data.frame(
        e[, c("id_comparable", "nombre_comun", "marca_comun", "contenido_valor", "contenido_unidad")],
        comercio = cm, sku = r$sku, ean = r$ean,
        regla = paste0("EAN ", e$ean, " idéntico en La Rebaja y Locatel"),
        stringsAsFactors = FALSE)
    }
    f <- anuncios[anuncios$comercio == "Farmatodo" &
                  str_detect(anuncios$nombre_original, regex(e$patron_farmatodo, ignore_case = TRUE)), ]
    f <- unico(f, e, "Farmatodo", paste0("el patrón /", e$patron_farmatodo, "/"))
    filas[[length(filas) + 1]] <- data.frame(
      e[, c("id_comparable", "nombre_comun", "marca_comun", "contenido_valor", "contenido_unidad")],
      comercio = "Farmatodo", sku = f$sku, ean = NA_character_,
      regla = paste0("Farmatodo: el nombre coincide con /", e$patron_farmatodo, "/ (marca, producto y medida)"),
      stringsAsFactors = FALSE)
  }
  cmp <- do.call(rbind, filas)

  if (!is.null(ruta_csv)) utils::write.csv(cmp, ruta_csv, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(cargar_comparables_df(con, cmp))
}


# ESTANDARIZACIÓN: campos propios; el texto original NO se modifica

normalizar_texto <- function(x) {
  x |> stringi::stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9%.,/+ ]", " ") |> str_squish()
}

# "Alfa Safe" y "ALFASAFE" -> "ALFASAFE"; "Cure Band" y "CUREBAND" -> "CUREBAND"
estandarizar_marca <- function(x) {
  m <- x |> stringi::stri_trans_general("Latin-ASCII") |> str_to_upper() |> str_remove_all("[^A-Z0-9]")
  ifelse(is.na(m) | m == "", NA_character_, m)
}

# Subcategoría común a los tres comercios, por reglas sobre el nombre normalizado
# (cada sitio organiza sus secciones de forma distinta). Gana la primera regla.
reglas_subcategoria <- tibble::tribble(
  ~subcategoria,                         ~patron,
  "Gasas y compresas",                   "\\bgasas?\\b|compresas?",
  "Vendas",                              "\\bvendas?\\b|\\bcoban\\b",
  "Esparadrapos y cintas",               "esparadrapo|micropor|transpore|\\bcinta|fixomull|fixo ?roll|fixosafe|leukopor|tela adhesiva",
  "Curitas y apósitos",                  "\\bcuras?\\b|curitas?|venditas?|aposito|tegaderm|parche|hidrocoloide|duoderm|aquacel|cure ?band",
  "Antisépticos y limpieza de heridas",  "alcohol|isodine|yodopovidona|povidona|agua oxigenada|peroxido|clorhexidina|merthiolate|mertiolate|microdacyn|antisept|desinfect|suero fisiologico",
  "Algodón",                             "algodon|copito|hisopo",
  "Botiquines y kits",                   "botiquin|\\bkit\\b",
  "Cicatrizantes, geles y cremas",       "cicatri|\\bgel\\b|crema|pomada|spray|solucion"
)
subcategoria_por_defecto <- "Otros productos de curación"

# Llena anuncios.nombre_estandar, anuncios.marca y anuncios.subcategoria
estandarizar_anuncios <- function(con) {
  a <- DBI::dbGetQuery(con, "SELECT id_anuncio, nombre_original, marca_original FROM anuncios")
  if (nrow(a) == 0) return(invisible(0L))
  nombre_norm <- normalizar_texto(a$nombre_original)
  sub <- rep(NA_character_, nrow(a))
  for (i in seq_len(nrow(reglas_subcategoria))) {
    hit <- which(is.na(sub) & str_detect(nombre_norm, regex(reglas_subcategoria$patron[i], ignore_case = TRUE)))
    sub[hit] <- reglas_subcategoria$subcategoria[i]
  }
  sub[is.na(sub)] <- subcategoria_por_defecto
  DBI::dbExecute(con, "UPDATE anuncios SET nombre_estandar = ?, marca = ?, subcategoria = ?
                       WHERE id_anuncio = ?",
                 params = list(nombre_norm, estandarizar_marca(a$marca_original), sub, a$id_anuncio))
  invisible(nrow(a))
}
