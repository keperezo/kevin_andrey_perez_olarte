# scraper.R -- Taller 1, Minería de Datos (UNAL)
# Comparador de precios: Cuidado y decoración de uñas (Éxito, Jumbo, Olímpica)
#
# Uso:
#   source("scraper.R")
#   res <- recolectar_todo()   # exito, jumbo y olimpica
#   res$resumen   # conteos por categoría vs. total que reporta el sitio
#   res$datos     # una fila por SKU
#
# Éxito, Jumbo y Olímpica usan VTEX: se consulta la API pública de catálogo
#   /api/catalog_system/pub/products/search?fq=C:/<ids de categoría>/&_from=&_to=
# (50 productos por página; el total llega en el encabezado `resources`).
# Cada respuesta cruda se guarda en raw/<comercio>/<fecha>/ (json.gz).

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

SITIOS <- c(exito    = "https://www.exito.com",
            jumbo    = "https://www.jumbocolombia.com",
            olimpica = "https://www.olimpica.com")
UA <- "Mozilla/5.0 (taller academico UNAL)"
TZ <- "America/Bogota"

# Columnas comunes que TODA función por comercio debe devolver (sección 3.4)
COLUMNAS <- c(
  "comercio", "ejecucion_id", "fecha_hora_consulta", "product_id", "sku", "ean",
  "nombre_original", "nombre_sku_original", "marca_original",
  "descripcion_original", "ficha_tecnica_original", "categoria_api",
  "categoria_consulta", "precio_cop", "precio_lista_cop", "en_promocion",
  "disponible", "vendedor", "unidad_medida", "multiplicador_unidad", "url")

# Categorías equivalentes a "Cuidado y decoración de uñas" (ids de la ruta VTEX).
# Éxito: la rama "Uñas" completa (3152) supera el tope de 2500 resultados de la API,
#   por eso se consultan sus dos hijas por separado.
# Jumbo y Olímpica no tienen una categoría exclusiva de uñas: los esmaltes y
#   removedores viven en categorías mixtas, que se extraen completas y se filtran
#   después con reglas documentadas.
CATEGORIAS_VTEX <- data.frame(
  comercio = c("exito", "exito",
               "jumbo", "jumbo", "jumbo",
               "olimpica", "olimpica", "olimpica", "olimpica"),
  ruta_ids = c("/346098589/346098591/346098600/346098622/",
               "/346098589/346098591/346098600/346098623/",
               "/2000001/2000020/2000198/",
               "/2000001/2000020/2000204/",
               "/142/2000108/2000110/",
               "/8000000/8690000/8690600/",
               "/8000000/8690000/8690700/",
               "/8000000/8690000/8691000/",
               "/900000000/900110000/900110700/"),
  etiqueta = c("Belleza > Maquillaje y cosméticos > Uñas > Accesorios para uñas",
               "Belleza > Maquillaje y cosméticos > Uñas > Esmaltes y removedores",
               "Supermercado > Belleza > Accesorios De Belleza",
               "Supermercado > Belleza > Maquillaje",
               "Mundo Bebés > Aseo E Higiene Infantil > Corta Uñas",
               "Belleza > Accesorios de belleza > Cortaúñas",
               "Belleza > Accesorios de belleza > Esmaltes",
               "Belleza > Accesorios de belleza > Otros Accesorios Uñas",
               "Supermercado > Cuidado Personal y Belleza > Maquillaje Y Belleza"),
  stringsAsFactors = FALSE)

# ---------------------------------------------------------------- utilidades
nz <- function(x) if (is.null(x) || length(x) == 0) NA else x

ahora_iso <- function() {                      # ISO 8601, hora de Colombia
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = TZ)
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", x)       # -0500 -> -05:00
}

nueva_ejecucion <- function() format(Sys.time(), "run_%Y%m%d_%H%M%S", tz = TZ)

normalizar_columnas <- function(df) {
  if (is.null(df)) df <- data.frame()
  for (col in setdiff(COLUMNAS, names(df))) df[[col]] <- rep(NA, nrow(df))
  df[, COLUMNAS, drop = FALSE]
}

# ------------------------------------------------------------- API VTEX
pedir_pagina <- function(base, ruta_ids, desde, hasta) {
  url <- paste0(base, "/api/catalog_system/pub/products/search?fq=C:",
                ruta_ids, "&_from=", desde, "&_to=", hasta)
  resp <- tryCatch(
    request(url) |>
      req_user_agent(UA) |>
      req_timeout(60) |>
      req_retry(max_tries = 4,
                is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
                backoff = \(i) 2^i) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(),
    error = function(e) NULL)
  if (is.null(resp))
    return(list(status = NA_integer_, texto = NA_character_, url = url,
                total = NA_integer_))
  rs <- resp_header(resp, "resources")          # formato "0-49/2088"
  list(status = resp_status(resp), texto = resp_body_string(resp), url = url,
       total = if (is.null(rs)) NA_integer_ else as.integer(sub(".*/", "", rs)))
}

guardar_raw <- function(meta, texto, comercio, slug) {
  dir <- file.path("raw", comercio, format(Sys.time(), "%Y-%m-%d", tz = TZ))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, sprintf("%s_%s_%05d_%05d.json.gz", meta$ejecucion_id, slug, meta$desde, meta$hasta))
  con <- gzfile(f, "wt"); on.exit(close(con))
  writeLines(toJSON(c(meta, list(respuesta = texto)), auto_unbox = TRUE), con)
  f
}

# Una fila por SKU (presentación). Precio 0 o ausente -> NA (nunca 0).
parsear_producto <- function(p, comercio, ejecucion, ts, categoria_consulta) {
  if (!length(p$items)) return(NULL)
  espec <- unlist(p$allSpecifications)
  ficha <- if (length(espec))
    paste(vapply(espec, function(k)
      paste0(k, ": ", paste(unlist(p[[k]]), collapse = ", ")), ""),
      collapse = " | ") else NA_character_
  do.call(rbind, lapply(p$items, function(it) {
    disp <- Filter(function(s) isTRUE(s$commertialOffer$AvailableQuantity > 0),
                   it$sellers)
    s <- if (length(disp)) disp[[1]] else if (length(it$sellers)) it$sellers[[1]] else NULL
    of <- s$commertialOffer
    precio <- suppressWarnings(as.numeric(nz(of$Price)))
    lista  <- suppressWarnings(as.numeric(nz(of$ListPrice)))
    if (!is.na(precio) && precio <= 0) precio <- NA_real_
    if (!is.na(lista)  && lista  <= 0) lista  <- NA_real_
    data.frame(
      comercio = comercio, ejecucion_id = ejecucion, fecha_hora_consulta = ts,
      product_id = as.character(nz(p$productId)),
      sku = as.character(nz(it$itemId)), ean = as.character(nz(it$ean)),
      nombre_original = as.character(nz(p$productName)),
      nombre_sku_original = as.character(nz(it$name)),
      marca_original = as.character(nz(p$brand)),
      descripcion_original = as.character(nz(p$description)),
      ficha_tecnica_original = ficha,
      categoria_api = as.character(nz(unlist(p$categories)[1])),
      categoria_consulta = categoria_consulta,
      precio_cop = precio, precio_lista_cop = lista,
      en_promocion = if (is.na(precio) || is.na(lista)) NA else precio < lista,
      disponible = isTRUE(of$AvailableQuantity > 0),
      vendedor = as.character(nz(s$sellerName)),
      unidad_medida = as.character(nz(it$measurementUnit)),
      multiplicador_unidad = suppressWarnings(as.numeric(nz(it$unitMultiplier))),
      url = as.character(nz(p$link)),
      stringsAsFactors = FALSE)
  }))
}

# Si una página falla (HTTP 500), se parte en mitades hasta aislar el producto
traer_rango <- function(comercio, base, ruta_ids, slug, desde, hasta,
                        ejecucion, categoria_consulta) {
  ts <- ahora_iso()
  pg <- pedir_pagina(base, ruta_ids, desde, hasta)
  if (isTRUE(pg$status %in% c(200, 206))) {
    guardar_raw(list(comercio = comercio, url = pg$url, status = pg$status,
                     desde = desde, hasta = hasta, total_sitio = pg$total,
                     fecha_hora_consulta = ts, ejecucion_id = ejecucion),
                pg$texto, comercio, slug)
    prods <- fromJSON(pg$texto, simplifyVector = FALSE)
    datos <- if (length(prods)) do.call(rbind, lapply(
      prods, parsear_producto, comercio = comercio, ejecucion = ejecucion,
      ts = ts, categoria_consulta = categoria_consulta)) else NULL
    return(list(datos = datos, total = pg$total, omitidos = NULL))
  }
  if (hasta > desde) {
    medio <- desde + (hasta - desde) %/% 2
    a <- traer_rango(comercio, base, ruta_ids, slug, desde, medio, ejecucion, categoria_consulta)
    Sys.sleep(1)
    b <- traer_rango(comercio, base, ruta_ids, slug, medio + 1, hasta, ejecucion, categoria_consulta)
    return(list(datos = rbind(a$datos, b$datos),
                total = if (!is.na(a$total)) a$total else b$total,
                omitidos = c(a$omitidos, b$omitidos)))
  }
  list(datos = NULL, total = NA_integer_, omitidos = desde)  # irrecuperable
}

extraer_categoria <- function(comercio, ruta_ids, categoria_consulta, ejecucion) {
  base <- SITIOS[[comercio]]
  slug <- gsub("^_|_$", "", gsub("/", "_", ruta_ids))
  r <- traer_rango(comercio, base, ruta_ids, slug, 0, 49, ejecucion, categoria_consulta)
  total <- r$total; filas <- list(r$datos); omitidos <- r$omitidos
  if (is.na(total)) warning(comercio, " ", ruta_ids, ": no se pudo leer el total")
  if (!is.na(total) && total > 2500)
    warning("Total > 2500: dividir la consulta (p. ej. por marca)")
  desde <- 50
  while (!is.na(total) && desde < min(total, 2500)) {
    r <- traer_rango(comercio, base, ruta_ids, slug, desde,
                     min(desde + 49, total - 1, 2499), ejecucion, categoria_consulta)
    filas[[length(filas) + 1]] <- r$datos
    omitidos <- c(omitidos, r$omitidos)
    desde <- desde + 50; Sys.sleep(1)
  }
  datos <- do.call(rbind, filas)
  list(datos = datos, total_sitio = total,
       n_productos = if (is.null(datos)) 0L else length(unique(datos$product_id)),
       n_omitidos = length(omitidos))
}

# --------------------------------------------- una función por comercio
scrape_vtex <- function(comercio, ejecucion) {
  cats <- CATEGORIAS_VTEX[CATEGORIAS_VTEX$comercio == comercio, ]
  res <- lapply(seq_len(nrow(cats)), function(k) {
    message("-> ", comercio, " | ", cats$etiqueta[k])
    tryCatch(extraer_categoria(comercio, cats$ruta_ids[k], cats$etiqueta[k], ejecucion),
             error = function(e) {
               warning(comercio, " ", cats$etiqueta[k], ": ", conditionMessage(e)); NULL })
  })
  g <- function(campo) vapply(res, function(x) if (is.null(x)) NA_integer_ else as.integer(x[[campo]]), 1L)
  resumen <- data.frame(comercio = comercio, ejecucion_id = ejecucion,
                        categoria = cats$etiqueta, total_sitio = g("total_sitio"),
                        n_productos = g("n_productos"), n_omitidos = g("n_omitidos"),
                        stringsAsFactors = FALSE)
  datos <- do.call(rbind, lapply(res, function(x) x$datos))
  list(datos = normalizar_columnas(datos), resumen = resumen)
}

scrape_exito    <- function(ejecucion) scrape_vtex("exito", ejecucion)
scrape_jumbo    <- function(ejecucion) scrape_vtex("jumbo", ejecucion)
scrape_olimpica <- function(ejecucion) scrape_vtex("olimpica", ejecucion)

# ------------------------------------------------- recolección completa
recolectar_todo <- function(comercios = c("exito", "jumbo", "olimpica"), ejecucion = nueva_ejecucion()) {
  fun <- list(exito = scrape_exito, jumbo = scrape_jumbo, olimpica = scrape_olimpica)
  out <- lapply(comercios, function(cm) fun[[cm]](ejecucion))
  datos <- do.call(rbind, lapply(out, `[[`, "datos"))
  datos <- datos[!duplicated(datos[, c("comercio", "product_id", "sku")]), ]
  resumen <- do.call(rbind, lapply(out, `[[`, "resumen"))
  rownames(datos) <- NULL; rownames(resumen) <- NULL
  saveRDS(list(datos = datos, resumen = resumen, ejecucion = ejecucion),
          paste0("brutos_", ejecucion, ".rds"))
  list(datos = datos, resumen = resumen, ejecucion = ejecucion)
}
 

# ---- Modo sin conexión: reconstruye los datos desde raw/ sin consultar los sitios ----
reprocesar_raw <- function(carpeta = "raw", ejecucion = NULL) {
  archivos <- list.files(carpeta, pattern = "\\.json\\.gz$", recursive = TRUE, full.names = TRUE)
  if (!length(archivos)) stop("No hay archivos .json.gz en ", carpeta)
  leer <- function(f) {
    con <- gzfile(f, "rt", encoding = "UTF-8"); on.exit(close(con))
    fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  }
  paginas <- lapply(archivos, leer)
  ejec <- vapply(paginas, function(p) as.character(p$ejecucion_id), "")
  if (is.null(ejecucion)) ejecucion <- max(ejec)          # la corrida más reciente
  paginas <- paginas[ejec == ejecucion]
  
  etiqueta_de <- function(url) {                          # la categoría sale de la URL guardada
    ruta <- sub("&.*", "", sub(".*fq=C:", "", url))
    CATEGORIAS_VTEX$etiqueta[match(ruta, CATEGORIAS_VTEX$ruta_ids)]
  }
  filas <- lapply(paginas, function(p) {
    prods <- fromJSON(p$respuesta, simplifyVector = FALSE)
    if (!length(prods)) return(NULL)
    do.call(rbind, lapply(prods, parsear_producto, comercio = p$comercio,
                          ejecucion = p$ejecucion_id, ts = p$fecha_hora_consulta,
                          categoria_consulta = etiqueta_de(p$url)))
  })
  datos <- normalizar_columnas(do.call(rbind, filas))
  datos <- datos[!duplicated(datos[, c("comercio", "product_id", "sku")]), ]
  rownames(datos) <- NULL
  
  info <- do.call(rbind, lapply(paginas, function(p) data.frame(
    comercio = p$comercio, categoria = etiqueta_de(p$url),
    total_sitio = suppressWarnings(as.integer(p$total_sitio)), stringsAsFactors = FALSE)))
  tot    <- aggregate(total_sitio ~ comercio + categoria, info, max)
  n_prod <- aggregate(product_id ~ comercio + categoria_consulta, datos,
                      function(x) length(unique(x)))
  names(n_prod) <- c("comercio", "categoria", "n_productos")
  resumen <- merge(tot, n_prod, by = c("comercio", "categoria"), all = TRUE)
  resumen$n_productos[is.na(resumen$n_productos)] <- 0L
  resumen$ejecucion_id <- ejecucion
  resumen$n_omitidos <- pmax(resumen$total_sitio - resumen$n_productos, 0L)
  resumen <- resumen[, c("comercio", "ejecucion_id", "categoria", "total_sitio", "n_productos", "n_omitidos")]
  list(datos = datos, resumen = resumen, ejecucion = ejecucion)
}

# ---- Función general: recolecta (o reprocesa) y guarda en SQLite ----
ejecutar_y_guardar <- function(db = "comparador_precios.sqlite", sin_conexion = FALSE,
                               ejecucion = NULL, comercios = c("exito", "jumbo", "olimpica")) {
  source("limpieza_sqlite.R", encoding = "UTF-8")
  res <- if (sin_conexion) reprocesar_raw(ejecucion = ejecucion) else recolectar_todo(comercios)
  cargar_sqlite(limpiar_datos(res$datos), res$resumen, db)
  invisible(res)
}
