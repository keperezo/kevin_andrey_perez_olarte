# =============================================================================
# scraper.R · Módulo de extracción del comparador de precios de perfumes
# Taller 1 · Minería de Datos (2016325) · Universidad Nacional de Colombia
# =============================================================================
#
# Cargar este archivo con source("scraper.R") solo define funciones; no
# descarga nada. La recolección se lanza con una sola llamada:
#
#   ejecutar_recoleccion()                          # en línea: consulta los sitios
#   ejecutar_recoleccion(modo = "sin_conexion")     # reprocesa la última copia de raw/
#   ejecutar_recoleccion(modo = "sin_conexion", id_ejecucion = "20261010T150000")
#
# Comercios y estrategia (detalles en notas/fase1_seleccion_comercios.md):
#   - La Riviera: HTML del servidor (Wix), paginado con ?page=N. Se lee con rvest.
#   - Falabella:  JSON __NEXT_DATA__ incrustado en el HTML de cada página.
#                 Se recorre dos veces (precio ascendente y descendente) porque
#                 el orden por defecto repite y omite productos entre páginas.
#   - Farmatodo:  servicio interno de búsqueda que usa la página (POST con JSON).
#                 Devuelve máximo 80 resultados por consulta, así que se parte la
#                 categoría por subcategoría y por rangos de precio.
#
# Cada función por comercio devuelve las mismas columnas (ver COLUMNAS).
# Ninguna respuesta mala detiene la recolección: la página se marca como
# fallida y el estado de la corrida (completa, parcial o fallida) lo refleja.
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)     # peticiones HTTP (request, req_*, resp_*)
  library(rvest)     # lectura de HTML con selectores CSS
  library(jsonlite)  # JSON <-> listas de R
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(DBI)       # interfaz genérica con bases de datos
  library(RSQLite)   # motor SQLite
})

# -----------------------------------------------------------------------------
# 1. Configuración
# -----------------------------------------------------------------------------
# Todo lo que podría cambiar está aquí, para no buscarlo dentro del código.

CONFIG <- list(
  ruta_bd      = "comparador_precios.sqlite",
  ruta_raw     = "raw",
  ruta_esquema = file.path("sql", "esquema.sql"),
  zona_horaria = "America/Bogota",          # Colombia: UTC-05:00 todo el año
  # El User-Agent identifica el proyecto ("compatible; ...") en vez de hacerse
  # pasar por una persona. Se puede cambiar aquí si el docente lo pide.
  user_agent   = "Mozilla/5.0 (compatible; TallerMineriaDatosUNAL/1.0; +https://github.com/afflorezr/dm_2016325)",
  pausa_seg    = 2,                          # pausa entre peticiones a un mismo sitio
  timeout_seg  = 60,
  max_intentos = 3,
  urls = list(
    la_riviera         = "https://www.lariviera.co/fragancias",
    falabella          = "https://www.falabella.com.co/falabella-co/category/cat1461053/Perfumes",
    farmatodo_api      = "https://api-search.farmatodo.com/1/indexes/*/queries",
    farmatodo_producto = "https://www.farmatodo.com.co/producto/"
  ),
  max_paginas_la_riviera = 60,               # tope de seguridad (hoy son 26 + 1 vacía)
  falabella_ordenes = c("derived.price.search,asc", "derived.price.search,desc"),
  # Falabella informa un total: la corrida es "completa" solo si se obtienen
  # todas las páginas y al menos este porcentaje de ese total en productos únicos.
  cobertura_minima_falabella = 0.98,
  farmatodo = list(
    indice          = "products-colombia",
    ciudad          = "BOG",                 # los precios dependen de la ciudad
    # Mismo filtro que usa la página de la categoría Perfumes (verificado el 2026-10-09):
    # productos visibles (outofstore:false), sin fórmula médica y sin muestras.
    filtro_base     = "outofstore:false AND classification.id:211 AND NOT requirePrescription:true AND NOT rms_class:SAMPLING",
    subcategorias   = c("Hombre", "Mujer", "Fragancias"),
    tope_resultados = 80,                    # máximo que entrega el servicio por consulta
    precio_max      = 10000000
  )
)

# Datos fijos de cada comercio (se cargan en la tabla comercios).
COMERCIOS <- tibble(
  comercio_id = 1:3,
  nombre = c("La Riviera", "Falabella", "Farmatodo"),
  clave  = c("la_riviera", "falabella", "farmatodo"),
  sitio_web = c("https://www.lariviera.co", "https://www.falabella.com.co", "https://www.farmatodo.com.co"),
  url_categoria = c(
    "https://www.lariviera.co/fragancias",
    "https://www.falabella.com.co/falabella-co/category/cat1461053/Perfumes",
    "https://www.farmatodo.com.co/categorias/belleza/perfumes"
  ),
  metodo_extraccion = c(
    "HTML renderizado en el servidor (Wix); paginación ?page=N; rvest con atributos data-hook",
    "JSON __NEXT_DATA__ incrustado en el HTML; paginación ?page=N; dos recorridos ordenados por precio",
    "Servicio interno de búsqueda (POST JSON) que usa la página; partición por subcategoría y rango de precio"
  )
)

# -----------------------------------------------------------------------------
# 2. Formato común de salida
# -----------------------------------------------------------------------------
# Las tres funciones de comercio devuelven exactamente estas columnas y tipos.
# Un dato que el sitio no muestra queda como NA (en SQLite, NULL), nunca 0.

COLUMNAS <- c(
  comercio = "character", id_producto_comercio = "character", sku = "character",
  ean = "character", nombre = "character", marca = "character",
  subcategoria = "character", presentacion = "character", contenido = "numeric",
  unidad = "character", precio = "numeric", precio_antes = "numeric",
  promocion = "logical", etiqueta_promocion = "character",
  precio_condicionado = "numeric", condicion_precio = "character",
  vendedor = "character", disponible = "logical", url_producto = "character",
  url_extraccion = "character", texto_original = "character",
  fecha_hora_consulta = "character", id_ejecucion = "character",
  archivo_raw = "character"
)

# Tabla sin filas con las columnas correctas (idea de tabla_productos_vacia()
# del taller de scraping, parte 2).
tabla_vacia <- function() {
  as_tibble(map(COLUMNAS, \(tipo) vector(tipo, 0)))
}

# Agrega las columnas que falten (como NA), convierte tipos y fija el orden.
conformar <- function(df) {
  if (nrow(df) == 0) return(tabla_vacia())
  for (col in names(COLUMNAS)) {
    if (!col %in% names(df)) df[[col]] <- NA
    df[[col]] <- switch(COLUMNAS[[col]],
      character = as.character(df[[col]]),
      numeric   = as.numeric(df[[col]]),
      logical   = as.logical(df[[col]])
    )
  }
  df[names(COLUMNAS)]
}

# -----------------------------------------------------------------------------
# 3. Utilidades
# -----------------------------------------------------------------------------

# x %||% y devuelve y cuando x es NULL (campo ausente en un JSON). Es el mismo
# operador de rlang y de R >= 4.4; se define aquí para no depender de la versión.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Tamaño en bytes de una respuesta (NA si no hubo respuesta).
bytes_de <- function(texto) if (is.na(texto)) NA_real_ else nchar(texto, type = "bytes")

# Une textos no vacíos con " | "; si no queda ninguno devuelve NA.
unir_textos <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
}

# Fecha y hora actual en ISO 8601 con el desfase de Colombia, p. ej.
# 2026-10-10T15:30:12-05:00. Colombia no tiene horario de verano: el desfase
# es siempre -05:00, así que se escribe fijo (el formato %z no se comporta
# igual en todos los sistemas operativos).
ahora_iso <- function() {
  paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = CONFIG$zona_horaria), "-05:00")
}

# Identificador de la corrida: fecha y hora de inicio, p. ej. 20261010T153012.
nuevo_id_ejecucion <- function() {
  format(Sys.time(), "%Y%m%dT%H%M%S", tz = CONFIG$zona_horaria)
}

# "20261010T153012" -> "2026-10-10" (nombre de la carpeta en raw/).
fecha_de_id <- function(id_ejecucion) {
  paste(substr(id_ejecucion, 1, 4), substr(id_ejecucion, 5, 6), substr(id_ejecucion, 7, 8), sep = "-")
}

# Precio en texto -> número en COP. "$ 1.680.000" -> 1680000.
# Toma el primer número del texto, con sus separadores de miles, y luego quita
# los separadores. Así un rango "$ 199.900 - $ 299.900" da 199900 y no un
# número pegado, y "$ 159.990,50" da 159990 (en COP no se usan centavos).
# Vacío o 0 -> NA: un precio ausente no se reemplaza por cero.
texto_a_numero <- function(x) {
  numero <- str_extract(as.character(x), "\\d{1,3}(?:[.,]\\d{3})+|\\d+")
  valor <- suppressWarnings(as.numeric(str_remove_all(numero, "[.,]")))
  valor[!is.na(valor) & valor <= 0] <- NA_real_
  valor
}

# Número que viene de un JSON (puede ser NULL, texto o 0) -> número o NA.
numero_json <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  valor <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(valor) || valor <= 0) NA_real_ else valor
}

# Texto que viene de un JSON -> texto o NA (también si viene vacío).
texto_json <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  valor <- str_squish(as.character(x[[1]]))
  if (is.na(valor) || valor == "") NA_character_ else valor
}

# Registro JSON de un producto tal como lo entregó el sitio. Se lee con
# simplifyVector = FALSE y se vuelve a escribir sin cambiar nombres, valores
# ni orden (se probó que la ida y vuelta da el mismo objeto). Es el texto
# original del anuncio en los comercios que publican sus productos en JSON.
json_original <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
}

# Busca el primer tamaño del nombre: "100ML", "100 ml", "75M", "3.4 oz".
# Solo lo extrae tal cual; la estandarización a mL se hace en procesamiento.R.
extraer_presentacion <- function(nombre) {
  patron <- regex("(\\d+(?:[.,]\\d{1,2})?)\\s*(ML|MLS|M|FL\\.?\\s*OZ|OZ)\\b", ignore_case = TRUE)
  m <- str_match(nombre, patron)
  unidad <- str_to_lower(m[, 3])
  unidad <- case_when(
    is.na(unidad) ~ NA_character_,
    str_detect(unidad, "oz") ~ "oz",
    .default = "ml"
  )
  tibble(
    presentacion = m[, 1],
    contenido = suppressWarnings(as.numeric(str_replace(m[, 2], ",", "."))),
    unidad = unidad
  )
}

# -----------------------------------------------------------------------------
# 4. Peticiones HTTP
# -----------------------------------------------------------------------------
# Base de todas las peticiones (patrón del taller de scraping, parte 2):
#   - User-Agent que identifica el proyecto e idioma español.
#   - Tiempo máximo de espera.
#   - req_retry(): reintenta si el sitio responde 429 (demasiadas peticiones)
#     o 5xx, y si la conexión falla, esperando 5 y 10 segundos.
#   - req_error(): no convierte los 4xx/5xx en error de R; el estado se revisa
#     después y queda registrado.
crear_peticion <- function(url) {
  opciones_reintento <- list(
    max_tries = CONFIG$max_intentos,
    is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
    backoff = \(intento) 5 * intento
  )
  # Desde httr2 1.1 reintentar fallas de conexión es opcional (retry_on_failure);
  # en versiones anteriores ya era el comportamiento por defecto.
  if ("retry_on_failure" %in% names(formals(httr2::req_retry))) {
    opciones_reintento$retry_on_failure <- TRUE
  }
  req <- request(url) |>
    req_user_agent(CONFIG$user_agent) |>
    req_headers(`accept-language` = "es-CO,es;q=0.9") |>
    req_timeout(CONFIG$timeout_seg)
  req <- do.call(req_retry, c(list(req), opciones_reintento))
  req |> req_error(is_error = \(resp) FALSE)
}

# Ejecuta la petición y devuelve una lista con estado, cuerpo y hora de consulta.
# Si no hay respuesta después de los reintentos (sin red, sitio caído),
# devuelve estado NA con el mensaje de error, sin detener la recolección.
ejecutar_peticion <- function(req) {
  fecha_hora <- ahora_iso()
  resp <- tryCatch(req_perform(req), error = \(e) e)
  if (inherits(resp, "error")) {
    message("   Sin respuesta: ", conditionMessage(resp))
    return(list(estado = NA_integer_, cuerpo = NA_character_,
                fecha_hora = fecha_hora, error = conditionMessage(resp)))
  }
  cuerpo <- tryCatch(resp_body_string(resp, encoding = "UTF-8"), error = \(e) NA_character_)
  list(estado = resp_status(resp), cuerpo = cuerpo, fecha_hora = fecha_hora, error = NA_character_)
}

# ¿La respuesta llegó con un estado de éxito y con cuerpo?
respuesta_valida <- function(r) {
  !is.na(r$estado) && r$estado < 400 && !is.na(r$cuerpo)
}

# Resultado de una página que no se pudo usar (sin respuesta, estado >= 400 o
# estructura inesperada). Tiene la misma forma que el de un parser.
pagina_fallida <- function(motivo) {
  list(ok = FALSE, error_parseo = str_squish(motivo), datos = tabla_vacia(), info = list())
}

# Aplica un parser sin detener la recolección: si la respuesta no tiene la
# estructura esperada (página de bloqueo, cuerpo vacío, JSON roto), devuelve
# cero filas con el mensaje de error, y la página queda marcada con ok = FALSE.
parsear_seguro <- function(parser, texto, meta) {
  tryCatch(
    c(list(ok = TRUE, error_parseo = NA_character_), parser(texto, meta)),
    error = \(e) pagina_fallida(conditionMessage(e))
  )
}

# Motivo de falla legible para los avisos de consola.
motivo_falla <- function(r, resultado) {
  if (is.na(r$estado)) paste("sin respuesta:", r$error)
  else if (r$estado >= 400) paste("estado HTTP", r$estado)
  else paste("respuesta no válida:", resultado$error_parseo)
}

# -----------------------------------------------------------------------------
# 5. Copias crudas (raw/) y manifiesto
# -----------------------------------------------------------------------------
# Cada respuesta se guarda comprimida en raw/<comercio>/<fecha>/ y se anota en
# un manifiesto CSV (<id_ejecucion>_manifiesto.csv) con su URL, parámetros,
# estado HTTP y hora de consulta. El modo sin conexión lee ese manifiesto.

carpeta_raw <- function(clave, id_ejecucion, crear = TRUE) {
  ruta <- file.path(CONFIG$ruta_raw, clave, fecha_de_id(id_ejecucion))
  if (crear) dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  ruta
}

# Escribe el texto tal como llegó (bytes UTF-8) en un archivo .gz.
guardar_raw <- function(texto, ruta_archivo) {
  con <- gzfile(ruta_archivo, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(texto)), con)
  invisible(ruta_archivo)
}

# Lee un archivo .gz completo y devuelve el texto ("" si el archivo está vacío).
leer_raw <- function(ruta_archivo) {
  con <- gzfile(ruta_archivo, open = "rb")
  on.exit(close(con))
  partes <- list()
  repeat {
    bloque <- readBin(con, "raw", n = 1e6)
    if (length(bloque) == 0) break
    partes[[length(partes) + 1]] <- bloque
  }
  if (length(partes) == 0) return("")
  texto <- rawToChar(do.call(c, partes))
  Encoding(texto) <- "UTF-8"
  texto
}

COLUMNAS_MANIFIESTO <- c(
  "comercio", "id_ejecucion", "archivo", "url", "metodo", "parametros",
  "estado_http", "fecha_hora_consulta", "bytes", "error",
  "pagina", "orden", "tramo", "nb_hits", "hoja", "truncado"
)

ruta_manifiesto <- function(clave, id_ejecucion) {
  file.path(carpeta_raw(clave, id_ejecucion, crear = FALSE), paste0(id_ejecucion, "_manifiesto.csv"))
}

# Agrega una fila al manifiesto (se escribe en cada petición, así que si la
# corrida se interrumpe, lo descargado hasta ese momento queda documentado).
registrar_manifiesto <- function(clave, id_ejecucion, fila) {
  fila <- as.list(fila)
  fila <- map(set_names(COLUMNAS_MANIFIESTO), \(col) if (is.null(fila[[col]])) NA else fila[[col]])
  ruta <- ruta_manifiesto(clave, id_ejecucion)
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    as.data.frame(fila, stringsAsFactors = FALSE), ruta, sep = ",",
    row.names = FALSE, col.names = !file.exists(ruta),
    append = file.exists(ruta), qmethod = "double", fileEncoding = "UTF-8"
  )
}

leer_manifiesto <- function(clave, id_ejecucion) {
  ruta <- ruta_manifiesto(clave, id_ejecucion)
  if (!file.exists(ruta)) {
    stop("No existe el manifiesto ", ruta, ". ¿Se hizo una corrida en línea con ese id?")
  }
  utils::read.csv(ruta, stringsAsFactors = FALSE, encoding = "UTF-8",
                  colClasses = c(parametros = "character", tramo = "character",
                                 orden = "character", error = "character"))
}

# Ids de todas las corridas guardadas en raw/, de la más antigua a la más reciente.
ejecuciones_en_raw <- function() {
  archivos <- list.files(CONFIG$ruta_raw, pattern = "_manifiesto\\.csv$", recursive = TRUE)
  if (length(archivos) == 0) stop("No hay copias en ", CONFIG$ruta_raw, "/ para reprocesar.")
  sort(unique(na.omit(str_extract(basename(archivos), "^[0-9]{8}T[0-9]{6}"))))
}

# Id de la corrida más reciente guardada en raw/ (para el modo sin conexión).
ultima_ejecucion_raw <- function() {
  ids <- ejecuciones_en_raw()
  ids[length(ids)]
}

# Modo sin conexión: aplica el mismo parser del modo en línea a cada archivo
# del manifiesto, en el mismo orden en que se descargaron. Devuelve una lista
# de páginas con sus datos y metadatos, igual que las funciones de descarga.
reprocesar_raw <- function(clave, id_ejecucion, parser) {
  manifiesto <- leer_manifiesto(clave, id_ejecucion)
  # Las copias están en la misma carpeta que el manifiesto: se buscan ahí,
  # aunque el proyecto se haya copiado a otro computador o carpeta.
  carpeta <- dirname(ruta_manifiesto(clave, id_ejecucion))
  message(clave, " · modo sin conexión: ", nrow(manifiesto), " archivos en el manifiesto")
  pmap(manifiesto, function(...) {
    meta <- list(...)
    archivo <- file.path(carpeta, basename(meta$archivo))
    if (is.na(meta$estado_http) || meta$estado_http >= 400) {
      return(c(list(meta = meta), pagina_fallida(paste("estado HTTP", meta$estado_http))))
    }
    if (!file.exists(archivo)) {
      return(c(list(meta = meta), pagina_fallida(paste("falta la copia", archivo))))
    }
    c(list(meta = meta), parsear_seguro(parser, leer_raw(archivo), meta))
  })
}

# Para la vista previa en consola.
avisar <- function(...) message(format(Sys.time(), "%H:%M:%S"), " · ", ...)

# -----------------------------------------------------------------------------
# 6. La Riviera
# -----------------------------------------------------------------------------
# Selectores verificados en el HTML real (2026-10-09). Cada tarjeta de
# producto es un elemento con data-hook="product-item-root":
#   nombre ......... [data-hook="product-item-name"]
#   precio ......... [data-hook="product-item-price-to-pay"]
#   precio antes ... [data-hook="product-item-price-before-discount"] (solo ofertas)
#   etiqueta ....... [data-hook="RibbonDataHook.RibbonOnImage"] ("65% OFF", "Producto 2X1"...)
#   enlace ......... [data-hook="product-item-product-details-link"] (atributo href)
#   id ............. atributo data-slug de la tarjeta
# Cada página trae 30 tarjetas; la última trae menos y la siguiente, ninguna.

parsear_la_riviera <- function(texto, meta) {
  if (is.na(texto) || !nzchar(texto)) stop("respuesta vacía")
  if (!str_detect(texto, "<")) stop("la respuesta no es HTML")
  doc <- read_html(texto)
  tarjetas <- html_elements(doc, '[data-hook="product-item-root"]')
  if (length(tarjetas) == 0) return(list(datos = tabla_vacia(), info = list(n_tarjetas = 0)))

  texto_de <- function(hook) {
    tarjetas |> html_element(paste0('[data-hook="', hook, '"]')) |> html_text2()
  }
  nombre    <- texto_de("product-item-name")
  etiqueta  <- texto_de("RibbonDataHook.RibbonOnImage")
  enlace    <- tarjetas |> html_element('[data-hook="product-item-product-details-link"]') |> html_attr("href")
  slug      <- coalesce(html_attr(tarjetas, "data-slug"), basename(enlace))
  precio    <- texto_a_numero(texto_de("product-item-price-to-pay"))
  antes     <- texto_a_numero(texto_de("product-item-price-before-discount"))
  presen    <- extraer_presentacion(nombre)

  datos <- tibble(
    comercio = "La Riviera",
    id_producto_comercio = slug,
    sku = str_extract(nombre, "\\b[A-Z]{1,4}[0-9]{0,3}-[0-9]{3}\\b"),  # código tipo V115-016
    nombre = str_squish(nombre),
    subcategoria = "Fragancias",
    presentacion = presen$presentacion,
    contenido = presen$contenido,
    unidad = presen$unidad,
    precio = precio,
    precio_antes = antes,
    # Promoción: precio tachado mayor que el precio a pagar, o etiqueta de
    # descuento (OFF, 2X1, precio especial).
    promocion = coalesce(antes > precio, FALSE) |
      str_detect(coalesce(etiqueta, ""), regex("OFF|2X1|ESPECIAL", ignore_case = TRUE)),
    etiqueta_promocion = etiqueta,
    url_producto = enlace,
    url_extraccion = meta$url,
    texto_original = html_text2(tarjetas),   # texto completo de la tarjeta, sin limpiar
    fecha_hora_consulta = meta$fecha_hora_consulta,
    id_ejecucion = meta$id_ejecucion,
    archivo_raw = meta$archivo
  )
  list(datos = conformar(datos), info = list(n_tarjetas = length(tarjetas)))
}

# Tarjetas leídas en cada consulta (NA si la consulta falló).
tarjetas_por_pagina <- function(paginas) {
  map_int(paginas, \(p) if (isTRUE(p$ok)) nrow(p$datos) else NA_integer_)
}

# Recorre ?page=1, 2, ... hasta encontrar una página sin tarjetas.
#   - Si una página falla (sin respuesta, estado >= 400 o HTML inesperado),
#     se anota y se sigue con la siguiente; con dos fallas seguidas se detiene.
#   - Una página vacía justo después de una página llena (30 tarjetas) podría
#     ser una falla momentánea y no el final del catálogo: se consulta otra vez
#     esa misma página y solo se da por terminado si vuelve a venir vacía.
descargar_la_riviera <- function(id_ejecucion) {
  clave <- "la_riviera"
  carpeta <- carpeta_raw(clave, id_ejecucion)
  paginas <- list()
  pagina <- 1
  fallas_seguidas <- 0
  confirmando <- FALSE
  while (pagina <= CONFIG$max_paginas_la_riviera) {
    url <- paste0(CONFIG$urls$la_riviera, "?page=", pagina)
    r <- ejecutar_peticion(crear_peticion(url))
    archivo <- file.path(carpeta, sprintf("%s_pagina_%02d%s.html.gz", id_ejecucion, pagina,
                                          if (confirmando) "_confirmacion" else ""))
    if (!is.na(r$cuerpo)) guardar_raw(r$cuerpo, archivo)
    meta <- list(comercio = clave, id_ejecucion = id_ejecucion, archivo = archivo, url = url,
                 metodo = "GET", parametros = paste0("page=", pagina), estado_http = r$estado,
                 fecha_hora_consulta = r$fecha_hora, bytes = bytes_de(r$cuerpo),
                 error = r$error, pagina = pagina)
    registrar_manifiesto(clave, id_ejecucion, meta)
    resultado <- if (respuesta_valida(r)) parsear_seguro(parsear_la_riviera, r$cuerpo, meta) else pagina_fallida("sin respuesta válida")
    paginas[[length(paginas) + 1]] <- c(list(meta = meta), resultado)
    Sys.sleep(CONFIG$pausa_seg)

    if (!resultado$ok) {
      avisar("La Riviera · página ", pagina, ": falló (", motivo_falla(r, resultado), ")")
      fallas_seguidas <- fallas_seguidas + 1
      if (fallas_seguidas >= 2) break
      pagina <- pagina + 1
      confirmando <- FALSE
      next
    }
    fallas_seguidas <- 0
    n <- nrow(resultado$datos)
    avisar("La Riviera · página ", pagina, if (confirmando) " (segunda consulta)" else "", ": ", n, " productos")
    if (n > 0) {
      pagina <- pagina + 1
      confirmando <- FALSE
      next
    }
    # Página vacía: ¿la última página con datos venía llena?
    n_pag <- tarjetas_por_pagina(paginas)
    con_datos <- which(!is.na(n_pag) & n_pag > 0)
    anterior_llena <- length(con_datos) > 0 && n_pag[max(con_datos)] == max(n_pag[con_datos])
    if (anterior_llena && !confirmando) {
      confirmando <- TRUE            # repetir la misma página antes de darla por última
      next
    }
    break
  }
  paginas
}

# Une las páginas y define el estado de la corrida. El sitio no muestra un
# total, así que "completa" exige un final verificable:
#   1. ninguna consulta falló;
#   2. la última consulta respondió bien y vino vacía;
#   3. la última página con datos trajo menos tarjetas que una página llena
#      (el final natural del catálogo), o la página vacía se confirmó con una
#      segunda consulta;
#   4. ninguna página intermedia trajo menos tarjetas que una página llena.
resumir_la_riviera <- function(paginas) {
  datos <- map(paginas, "datos") |> list_rbind()
  if (is.null(datos) || nrow(datos) == 0) datos <- tabla_vacia()
  n_brutos <- nrow(datos)
  datos <- distinct(datos, id_producto_comercio, .keep_all = TRUE)

  n_pag <- tarjetas_por_pagina(paginas)
  pag_num <- map_int(paginas, \(p) as.integer(p$meta$pagina))
  errores <- sum(is.na(n_pag))
  con_datos <- which(!is.na(n_pag) & n_pag > 0)
  ultima <- length(paginas)
  llena <- if (length(con_datos) > 0) max(n_pag[con_datos]) else 0L
  n_ultima_con_datos <- if (length(con_datos) > 0) n_pag[max(con_datos)] else 0L
  termina_vacia <- ultima > 0 && !is.na(n_pag[ultima]) && n_pag[ultima] == 0
  confirmado <- ultima >= 2 && pag_num[ultima] == pag_num[ultima - 1] &&
    identical(n_pag[(ultima - 1):ultima], c(0L, 0L))
  intermedias_cortas <- if (length(con_datos) > 1) sum(n_pag[con_datos[-length(con_datos)]] < llena) else 0L
  fin_verificado <- termina_vacia && length(con_datos) > 0 && (n_ultima_con_datos < llena || confirmado)

  estado <- case_when(
    nrow(datos) == 0 ~ "fallida",
    fin_verificado && errores == 0 && intermedias_cortas == 0 ~ "completa",
    .default = "parcial"
  )
  fin <- if (fin_verificado) {
    sprintf("fin verificado: la página %d vino vacía y la última con datos tenía %d tarjetas (una página llena tiene %d)%s",
            pag_num[ultima], n_ultima_con_datos, llena,
            if (confirmado) "; el final se confirmó con una segunda consulta" else "")
  } else {
    "sin un final verificado"
  }
  detalle <- sprintf(
    "%d consultas (%d con error, %d páginas intermedias con menos tarjetas de lo normal); %d tarjetas, %d productos únicos; el sitio no muestra un total; %s",
    length(paginas), errores, intermedias_cortas, n_brutos, nrow(datos), fin
  )
  list(datos = datos, estado = estado, n_total_sitio = NA_integer_, detalle = detalle)
}

extraer_la_riviera <- function(id_ejecucion, modo = "en_linea") {
  paginas <- if (modo == "en_linea") {
    descargar_la_riviera(id_ejecucion)
  } else {
    reprocesar_raw("la_riviera", id_ejecucion, parsear_la_riviera)
  }
  resumir_la_riviera(paginas)
}

# -----------------------------------------------------------------------------
# 7. Falabella
# -----------------------------------------------------------------------------
# Cada página de la categoría trae un <script id="__NEXT_DATA__"> con un JSON:
#   props.pageProps.results ...... productos de la página (48 por página)
#   props.pageProps.pagination ... count (total que informa el sitio), perPage
# Campos usados por producto: skuId, productId, displayName, brand, sellerName,
# url, isSponsored, discountBadge y prices (lista con type, crossed y price).
#
# Precios: el precio a pagar es el menor precio vigente (eventPrice o
# internetPrice). El tachado (crossed = true) es el precio antes del descuento;
# se conserva tal como viene, aunque no sea mayor que el precio (ese caso se
# reporta como inconsistencia). cmrPrice solo aplica con la tarjeta CMR, así
# que se guarda aparte como precio condicionado y no se usa para comparar.

fila_falabella <- function(r, meta) {
  precios <- map(r$prices %||% list(), \(p) list(
    tipo    = texto_json(p$type),
    tachado = isTRUE(p$crossed),
    valor   = texto_a_numero(texto_json(p$price))
  ))
  tipos    <- map_chr(precios, \(p) p$tipo %||% NA_character_)
  tachados <- map_lgl(precios, "tachado")
  valores  <- map_dbl(precios, "valor")

  vigentes <- !tachados & !is.na(valores) & coalesce(tipos, "") != "cmrPrice"
  precio   <- if (any(vigentes)) min(valores[vigentes]) else NA_real_
  tipo_precio <- if (any(vigentes)) tipos[vigentes][which.min(valores[vigentes])] else NA_character_
  antes    <- if (any(tachados & !is.na(valores))) max(valores[tachados & !is.na(valores)]) else NA_real_
  cmr      <- valores[coalesce(tipos, "") == "cmrPrice" & !tachados]
  cmr      <- if (length(cmr) > 0 && !is.na(cmr[1])) cmr[1] else NA_real_
  es_evento <- identical(tipo_precio, "eventPrice")

  insignia <- texto_json(r$discountBadge$label)
  nombre   <- texto_json(r$displayName)
  presen   <- extraer_presentacion(nombre)

  tibble(
    comercio = "Falabella",
    id_producto_comercio = texto_json(r$skuId),
    sku = texto_json(r$skuId),
    nombre = nombre,
    marca = texto_json(r$brand),
    subcategoria = "Perfumes",
    presentacion = presen$presentacion,
    contenido = presen$contenido,
    unidad = presen$unidad,
    precio = precio,
    precio_antes = antes,
    # Promoción: precio tachado mayor que el precio a pagar, o precio de evento.
    promocion = coalesce(antes > precio, FALSE) | es_evento,
    etiqueta_promocion = unir_textos(c(insignia, if (es_evento) "eventPrice")),
    precio_condicionado = cmr,
    condicion_precio = if (!is.na(cmr)) "Precio con tarjeta CMR" else NA_character_,
    vendedor = texto_json(r$sellerName),
    url_producto = texto_json(r$url),
    url_extraccion = meta$url,
    texto_original = json_original(r),   # registro del producto tal como llegó
    fecha_hora_consulta = meta$fecha_hora_consulta,
    id_ejecucion = meta$id_ejecucion,
    archivo_raw = meta$archivo,
    patrocinado = isTRUE(r$isSponsored)
  )
}

parsear_falabella <- function(texto, meta) {
  if (is.na(texto) || !nzchar(texto)) stop("respuesta vacía")
  if (!str_detect(texto, "<")) stop("la respuesta no es HTML")
  json <- read_html(texto) |> html_element("script#__NEXT_DATA__") |> html_text()
  if (is.na(json)) stop("la página no trae el bloque __NEXT_DATA__")
  pp <- fromJSON(json, simplifyVector = FALSE)$props$pageProps
  if (is.null(pp$results)) stop("__NEXT_DATA__ no trae props.pageProps.results")
  filas <- map(pp$results, fila_falabella, meta = meta) |> list_rbind()
  if (is.null(filas) || nrow(filas) == 0) {
    return(list(datos = tabla_vacia(), info = list(paginacion = pp$pagination, n_patrocinados = 0)))
  }
  # Los resultados patrocinados (publicidad) se descartan: no son parte del
  # listado ordenado y repiten productos que ya aparecen en la categoría.
  n_patrocinados <- sum(filas$patrocinado)
  datos <- filas |> filter(!patrocinado) |> conformar()
  list(datos = datos, info = list(paginacion = pp$pagination, n_patrocinados = n_patrocinados))
}

# Dos recorridos completos, uno por cada orden. El número de páginas se toma de
# la primera página de cada recorrido (count / perPage); si esa página falla,
# se usa el del recorrido anterior.
descargar_falabella <- function(id_ejecucion) {
  clave <- "falabella"
  carpeta <- carpeta_raw(clave, id_ejecucion)
  paginas <- list()
  n_paginas <- NA_integer_
  for (orden in CONFIG$falabella_ordenes) {
    sufijo <- if (str_detect(orden, "desc$")) "desc" else "asc"
    pagina <- 1
    while (pagina <= coalesce(n_paginas, 1L)) {
      req <- crear_peticion(CONFIG$urls$falabella) |> req_url_query(page = pagina, sortBy = orden)
      url <- req$url
      r <- ejecutar_peticion(req)
      archivo <- file.path(carpeta, sprintf("%s_%s_pagina_%02d.html.gz", id_ejecucion, sufijo, pagina))
      if (!is.na(r$cuerpo)) guardar_raw(r$cuerpo, archivo)
      meta <- list(comercio = clave, id_ejecucion = id_ejecucion, archivo = archivo, url = url,
                   metodo = "GET", parametros = paste0("page=", pagina, "&sortBy=", orden),
                   estado_http = r$estado, fecha_hora_consulta = r$fecha_hora,
                   bytes = bytes_de(r$cuerpo), error = r$error,
                   pagina = pagina, orden = orden)
      registrar_manifiesto(clave, id_ejecucion, meta)
      resultado <- if (respuesta_valida(r)) parsear_seguro(parsear_falabella, r$cuerpo, meta) else pagina_fallida("sin respuesta válida")
      paginas[[length(paginas) + 1]] <- c(list(meta = meta), resultado)
      if (resultado$ok) {
        pag <- resultado$info$paginacion
        if (pagina == 1 && !is.null(pag$count) && !is.null(pag$perPage)) {
          n_paginas <- as.integer(ceiling(as.numeric(pag$count) / as.numeric(pag$perPage)))
        }
        avisar("Falabella · ", sufijo, " · página ", pagina, "/", n_paginas, ": ", nrow(resultado$datos), " productos")
      } else {
        avisar("Falabella · ", sufijo, " · página ", pagina, ": falló (", motivo_falla(r, resultado), ")")
      }
      pagina <- pagina + 1
      Sys.sleep(CONFIG$pausa_seg)
    }
  }
  paginas
}

# Productos únicos de un conjunto de páginas.
unicos_en <- function(paginas) {
  d <- map(paginas, "datos") |> list_rbind()
  if (is.null(d) || nrow(d) == 0) 0L else n_distinct(d$id_producto_comercio)
}

resumir_falabella <- function(paginas) {
  datos <- map(paginas, "datos") |> list_rbind()
  if (is.null(datos) || nrow(datos) == 0) datos <- tabla_vacia()
  n_brutos <- nrow(datos)
  # Un producto aparece en ambos recorridos (y a veces dos veces por empates de
  # precio en el borde de una página): se conserva su primera aparición.
  datos <- distinct(datos, id_producto_comercio, .keep_all = TRUE)
  n_patrocinados <- sum(map_dbl(paginas, \(p) p$info$n_patrocinados %||% 0))

  ok <- map_lgl(paginas, "ok")
  orden <- map_chr(paginas, \(p) as.character(p$meta$orden %||% NA_character_))
  unicos_por_orden <- map_int(CONFIG$falabella_ordenes, \(o) unicos_en(paginas[orden %in% o]))
  validas <- keep(paginas, \(p) isTRUE(p$ok))
  total <- validas |> map(\(p) p$info$paginacion$count) |> compact()
  total <- if (length(total) > 0) as.integer(total[[1]]) else NA_integer_
  por_pagina <- validas |> map(\(p) p$info$paginacion$perPage) |> compact()
  por_pagina <- if (length(por_pagina) > 0) as.numeric(por_pagina[[1]]) else 48
  esperadas <- if (is.na(total)) NA_integer_ else as.integer(ceiling(total / por_pagina)) * length(CONFIG$falabella_ordenes)
  obtenidas <- sum(ok)
  cobertura <- if (is.na(total) || total == 0) NA_real_ else nrow(datos) / total

  estado <- case_when(
    nrow(datos) == 0 ~ "fallida",
    !is.na(esperadas) && obtenidas >= esperadas && !is.na(cobertura) &&
      cobertura >= CONFIG$cobertura_minima_falabella ~ "completa",
    .default = "parcial"
  )
  detalle <- sprintf(
    "%d de %s páginas esperadas con datos válidos (%d recorridos ordenados por precio; productos únicos por recorrido: %s); %d filas, %d productos únicos frente a %s que informa el sitio (%s; para 'completa' se exige %s); %d resultados patrocinados descartados",
    obtenidas, ifelse(is.na(esperadas), "?", esperadas), length(CONFIG$falabella_ordenes),
    paste(paste0(str_extract(CONFIG$falabella_ordenes, "(asc|desc)$"), " ", unicos_por_orden), collapse = ", "),
    n_brutos, nrow(datos), ifelse(is.na(total), "?", total),
    ifelse(is.na(cobertura), "sin dato", sprintf("%.1f %%", 100 * cobertura)),
    sprintf("%.0f %%", 100 * CONFIG$cobertura_minima_falabella),
    n_patrocinados
  )
  list(datos = datos, estado = estado, n_total_sitio = total, detalle = detalle)
}

extraer_falabella <- function(id_ejecucion, modo = "en_linea") {
  paginas <- if (modo == "en_linea") {
    descargar_falabella(id_ejecucion)
  } else {
    reprocesar_raw("falabella", id_ejecucion, parsear_falabella)
  }
  resumir_falabella(paginas)
}

# -----------------------------------------------------------------------------
# 8. Farmatodo
# -----------------------------------------------------------------------------
# La página de la categoría solo trae los primeros productos; el resto lo
# carga con el botón "Cargar más", que llama a este servicio (pestaña Network):
#   POST https://api-search.farmatodo.com/1/indexes/*/queries
#   cuerpo: {"requests":[{"indexName":"products-colombia","params":"query=&filters=...&hitsPerPage=100&page=0"}]}
#   cabecera x-custom-city: BOG (los precios dependen de la ciudad)
# No requiere clave. El servicio responde como máximo 80 productos por
# consulta (nbHits también llega en 80), por eso la categoría se divide en
# tramos que tengan menos de 80.
# Campos: id, mediaDescription (nombre), marca, barcode (EAN), fullPrice,
# offerPrice, primePrice, subCategory, outofstore, url (ruta de la ficha).

consultar_farmatodo <- function(filtro) {
  params <- paste0("query=&filters=", utils::URLencode(filtro, reserved = TRUE),
                   "&hitsPerPage=100&page=0")
  cuerpo <- list(requests = list(list(indexName = CONFIG$farmatodo$indice, params = params)))
  # Las mismas cabeceras que envía la página al llamar el servicio (pestaña
  # Network): la ciudad y el sitio de origen. No se envía ninguna clave.
  req <- crear_peticion(CONFIG$urls$farmatodo_api) |>
    req_headers(
      `x-custom-city` = CONFIG$farmatodo$ciudad,
      origin = "https://www.farmatodo.com.co",
      referer = "https://www.farmatodo.com.co/"
    ) |>
    req_body_json(cuerpo)                 # convierte la lista a JSON y usa POST
  c(ejecutar_peticion(req), list(params = params))
}

fila_farmatodo <- function(h, meta) {
  # Precio de lista para Bogotá: primero el de fullPriceByCity, si existe.
  por_ciudad <- keep(h$fullPriceByCity %||% list(), \(x) identical(x$cityCode, CONFIG$farmatodo$ciudad))
  lista  <- if (length(por_ciudad) > 0) numero_json(por_ciudad[[1]]$fullPrice) else NA_real_
  lista  <- coalesce(lista, numero_json(h$fullPrice))
  oferta <- numero_json(h$offerPrice)
  prime  <- numero_json(h$primePrice)
  # Oferta: offerPrice menor que el precio de lista; entonces el precio a pagar
  # es la oferta y el de lista queda como precio antes del descuento.
  hay_oferta <- !is.na(oferta) && !is.na(lista) && oferta < lista

  nombre <- coalesce(texto_json(h$mediaDescription), texto_json(h$description))
  presen <- extraer_presentacion(nombre)
  ruta   <- texto_json(h$url)

  tibble(
    comercio = "Farmatodo",
    id_producto_comercio = coalesce(texto_json(h$id), texto_json(h$objectID)),
    sku = coalesce(texto_json(h$id), texto_json(h$objectID)),
    ean = texto_json(h$barcode),
    nombre = nombre,
    marca = texto_json(h$marca),
    subcategoria = texto_json(h$subCategory),
    presentacion = presen$presentacion,
    contenido = presen$contenido,
    unidad = presen$unidad,
    precio = if (hay_oferta) oferta else lista,
    precio_antes = if (hay_oferta) lista else NA_real_,
    promocion = hay_oferta,
    etiqueta_promocion = coalesce(texto_json(h$offerText), texto_json(h$offerDescription)),
    precio_condicionado = if (!is.na(prime) && !is.na(lista) && prime < lista) prime else NA_real_,
    condicion_precio = if (!is.na(prime) && !is.na(lista) && prime < lista) "Precio Prime (membresía)" else NA_character_,
    disponible = !isTRUE(h$outofstore),
    url_producto = if (!is.na(ruta)) paste0(CONFIG$urls$farmatodo_producto, ruta) else NA_character_,
    url_extraccion = meta$url,
    texto_original = json_original(h),   # registro del producto tal como llegó
    fecha_hora_consulta = meta$fecha_hora_consulta,
    id_ejecucion = meta$id_ejecucion,
    archivo_raw = meta$archivo
  )
}

parsear_farmatodo <- function(texto, meta) {
  if (is.na(texto) || !nzchar(texto)) stop("respuesta vacía")
  resultados <- fromJSON(texto, simplifyVector = FALSE)$results
  if (length(resultados) == 0 || is.null(resultados[[1]]$hits)) stop("la respuesta no trae results[[1]]$hits")
  respuesta <- resultados[[1]]
  hits <- respuesta$hits
  datos <- map(hits, fila_farmatodo, meta = meta) |> list_rbind()
  if (is.null(datos)) datos <- tabla_vacia()
  list(datos = conformar(datos),
       info = list(nb_hits = as.integer(respuesta$nbHits %||% length(hits)), n_hits = length(hits)))
}

# Recorre los tramos. Si un tramo llega al tope de 80 (o el servicio informa
# más resultados de los que entregó), se parte el rango de precios a la mitad y
# se consultan las dos mitades (búsqueda en profundidad). Solo los tramos
# finales (hojas) aportan productos.
descargar_farmatodo <- function(id_ejecucion) {
  clave <- "farmatodo"
  cfg <- CONFIG$farmatodo
  carpeta <- carpeta_raw(clave, id_ejecucion)
  subcats <- paste0('subCategory:"', cfg$subcategorias, '"')
  otros   <- paste(paste0("NOT ", subcats), collapse = " AND ")   # productos sin esas subcategorías
  pendientes <- map(c(subcats, otros), \(s) list(sub = s, desde = 0, hasta = cfg$precio_max))
  resultados <- list()
  n <- 0
  while (length(pendientes) > 0) {
    tramo <- pendientes[[1]]
    pendientes <- pendientes[-1]
    n <- n + 1
    rango  <- sprintf("fullPrice:%.0f TO %.0f", tramo$desde, tramo$hasta)
    filtro <- paste(cfg$filtro_base, "AND", tramo$sub, "AND", rango)
    r <- consultar_farmatodo(filtro)
    archivo <- file.path(carpeta, sprintf("%s_consulta_%03d.json.gz", id_ejecucion, n))
    if (!is.na(r$cuerpo)) guardar_raw(r$cuerpo, archivo)
    meta <- list(comercio = clave, id_ejecucion = id_ejecucion, archivo = archivo,
                 url = CONFIG$urls$farmatodo_api, metodo = "POST", parametros = r$params,
                 estado_http = r$estado, fecha_hora_consulta = r$fecha_hora,
                 bytes = bytes_de(r$cuerpo), error = r$error,
                 tramo = paste(tramo$sub, "|", rango))
    resultado <- if (respuesta_valida(r)) parsear_seguro(parsear_farmatodo, r$cuerpo, meta) else pagina_fallida("sin respuesta válida")
    if (!resultado$ok) {
      avisar("Farmatodo · consulta ", n, ": falló (", motivo_falla(r, resultado), ")")
      meta$hoja <- TRUE
      meta$truncado <- TRUE        # un tramo sin datos deja la categoría incompleta
      registrar_manifiesto(clave, id_ejecucion, meta)
      resultados[[n]] <- c(list(meta = meta), resultado)
    } else {
      nb <- resultado$info$nb_hits
      en_tope <- nb >= cfg$tope_resultados || nb > resultado$info$n_hits
      partir <- en_tope && tramo$hasta > tramo$desde
      meta$nb_hits  <- nb
      meta$hoja     <- !partir
      meta$truncado <- !partir && en_tope
      registrar_manifiesto(clave, id_ejecucion, meta)
      if (partir) {
        medio <- floor((tramo$desde + tramo$hasta) / 2)
        pendientes <- c(list(list(sub = tramo$sub, desde = tramo$desde, hasta = medio),
                             list(sub = tramo$sub, desde = medio + 1, hasta = tramo$hasta)),
                        pendientes)
        resultado$datos <- tabla_vacia()   # solo se usan los tramos finales (hojas)
      }
      resultados[[n]] <- c(list(meta = meta), resultado)
      avisar("Farmatodo · consulta ", n, " [", meta$tramo, "]: ", nb,
             if (partir) " resultados (se parte el tramo)" else " productos")
    }
    Sys.sleep(CONFIG$pausa_seg)
  }
  resultados
}

resumir_farmatodo <- function(consultas) {
  hojas <- keep(consultas, \(p) isTRUE(as.logical(p$meta$hoja)))
  datos <- map(hojas, "datos") |> list_rbind()
  if (is.null(datos) || nrow(datos) == 0) datos <- tabla_vacia()
  n_brutos <- nrow(datos)
  datos <- distinct(datos, id_producto_comercio, .keep_all = TRUE)
  errores   <- sum(!map_lgl(consultas, "ok"))
  truncadas <- sum(map_lgl(hojas, \(p) isTRUE(p$ok) && isTRUE(as.logical(p$meta$truncado))))
  estado <- case_when(
    nrow(datos) == 0 ~ "fallida",
    errores == 0 && truncadas == 0 ~ "completa",
    .default = "parcial"
  )
  detalle <- sprintf(
    "%d consultas (%d tramos finales, %d con error, %d tramos en el tope de %d); %d filas, %d productos únicos; el servicio no informa un total (nbHits llega como máximo en %d), así que la completitud se verifica porque cada tramo final tiene menos de %d resultados",
    length(consultas), length(hojas), errores, truncadas, CONFIG$farmatodo$tope_resultados,
    n_brutos, nrow(datos), CONFIG$farmatodo$tope_resultados, CONFIG$farmatodo$tope_resultados
  )
  list(datos = datos, estado = estado, n_total_sitio = NA_integer_, detalle = detalle)
}

extraer_farmatodo <- function(id_ejecucion, modo = "en_linea") {
  consultas <- if (modo == "en_linea") {
    descargar_farmatodo(id_ejecucion)
  } else {
    # En el modo sin conexión se parsean todas las respuestas; resumir_farmatodo
    # usa solo las marcadas como hoja en el manifiesto.
    reprocesar_raw("farmatodo", id_ejecucion, parsear_farmatodo)
  }
  resumir_farmatodo(consultas)
}

EXTRACTORES <- list(
  la_riviera = extraer_la_riviera,
  falabella  = extraer_falabella,
  farmatodo  = extraer_farmatodo
)

# -----------------------------------------------------------------------------
# 9. Base de datos SQLite
# -----------------------------------------------------------------------------

# Abre (o crea) la base, activa las llaves foráneas y asegura el esquema.
# busy_timeout: si otro programa tiene la base abierta, espera hasta 10 s en
# lugar de fallar de inmediato.
conectar_bd <- function(ruta_bd = CONFIG$ruta_bd) {
  con <- dbConnect(RSQLite::SQLite(), ruta_bd)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  dbExecute(con, "PRAGMA busy_timeout = 10000")
  crear_esquema(con)
  con
}

# Ejecuta sql/esquema.sql sentencia por sentencia y registra los comercios.
crear_esquema <- function(con) {
  lineas <- readLines(CONFIG$ruta_esquema, encoding = "UTF-8", warn = FALSE)
  lineas <- lineas[!str_detect(lineas, "^\\s*--")]          # quita comentarios de línea
  sentencias <- str_split(paste(lineas, collapse = "\n"), ";\\s*(\n|$)")[[1]] |> str_trim()
  walk(sentencias[sentencias != ""], \(s) dbExecute(con, s))
  dbExecute(con, "
    INSERT INTO comercios (comercio_id, nombre, clave, sitio_web, url_categoria, metodo_extraccion)
    VALUES (:comercio_id, :nombre, :clave, :sitio_web, :url_categoria, :metodo_extraccion)
    ON CONFLICT DO NOTHING",
    params = as.list(COMERCIOS))
  invisible(con)
}

id_comercio <- function(clave) COMERCIOS$comercio_id[COMERCIOS$clave == clave]

# Primera anotación de la corrida (estado provisional "fallida"): si R se
# detiene a mitad de camino, la base muestra que la corrida no terminó.
registrar_inicio <- function(con, id_ejecucion, clave, inicio, modo) {
  dbExecute(con, "
    INSERT INTO ejecuciones (id_ejecucion, comercio_id, fecha_hora_inicio, modo, n_registros, estado, detalle)
    VALUES (?, ?, ?, ?, 0, 'fallida', 'en curso')
    ON CONFLICT (id_ejecucion, comercio_id) DO NOTHING",
    params = list(id_ejecucion, id_comercio(clave), inicio, modo))
}

registrar_fin <- function(con, id_ejecucion, clave, resultado, n_registros) {
  dbExecute(con, "
    UPDATE ejecuciones
       SET fecha_hora_fin = ?, n_registros = ?, n_total_sitio = ?, estado = ?, detalle = ?
     WHERE id_ejecucion = ? AND comercio_id = ?",
    params = list(ahora_iso(), n_registros, resultado$n_total_sitio, resultado$estado,
                  resultado$detalle, id_ejecucion, id_comercio(clave)))
}

# Revisa cada fila antes de cargarla. Una fila que no cumple las reglas de la
# base se descarta aquí, con su motivo, en lugar de perderse en silencio.
motivo_descarte <- function(datos) {
  case_when(
    is.na(datos$id_producto_comercio) ~ "sin identificador",
    is.na(datos$nombre) ~ "sin nombre",
    is.na(datos$url_extraccion) ~ "sin URL de extracción",
    !str_detect(coalesce(datos$fecha_hora_consulta, ""),
                "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}-05:00$") ~ "fecha y hora inválida",
    coalesce(datos$precio <= 0, FALSE) ~ "precio no positivo",
    .default = NA_character_
  )
}

# Inserta anuncios y observaciones sin sobrescribir nada:
#   - anuncios: un producto ya conocido conserva su id (ON CONFLICT DO NOTHING
#     sobre la restricción UNIQUE (comercio_id, id_producto_comercio)).
#   - observaciones: la restricción UNIQUE (comercio, anuncio, fecha y hora de
#     consulta) impide cargar dos veces la misma observación. Se usa
#     ON CONFLICT DO NOTHING y no INSERT OR IGNORE, porque este último también
#     ignoraría en silencio las filas que violan otras restricciones (CHECK,
#     NOT NULL); así cualquier otro problema produce un error visible.
# Devuelve cuántas observaciones entraron, cuántas ya existían y cuántas
# filas se descartaron (con sus motivos).
insertar_observaciones <- function(con, datos, clave) {
  salida <- list(nuevas = 0L, existentes = 0L, descartadas = 0L, motivos = NA_character_)
  if (nrow(datos) == 0) return(salida)
  motivo <- motivo_descarte(datos)
  if (any(!is.na(motivo))) {
    conteo <- table(motivo[!is.na(motivo)])
    salida$descartadas <- sum(conteo)
    salida$motivos <- paste(paste0(names(conteo), ": ", as.integer(conteo)), collapse = ", ")
    message("   Se descartan ", salida$descartadas, " filas (", salida$motivos, ")")
  }
  carga <- datos[is.na(motivo), ] |>
    mutate(
      texto_original = coalesce(texto_original, nombre),
      comercio_id = id_comercio(clave),
      promocion = as.integer(promocion),
      disponible = as.integer(disponible)
    )
  if (nrow(carga) == 0) return(salida)
  n <- dbWithTransaction(con, {
    dbWriteTable(con, "tmp_carga", carga, temporary = TRUE, overwrite = TRUE)
    dbExecute(con, "
      INSERT INTO anuncios (comercio_id, id_producto_comercio, sku, ean, url_producto)
      SELECT comercio_id, id_producto_comercio, sku, ean, url_producto
        FROM tmp_carga
       WHERE true
       GROUP BY comercio_id, id_producto_comercio
      ON CONFLICT (comercio_id, id_producto_comercio) DO NOTHING")
    n <- dbExecute(con, "
      INSERT INTO observaciones (
        anuncio_id, comercio_id, id_ejecucion, fecha_hora_consulta,
        nombre_original, marca_original, subcategoria_original, presentacion_original,
        contenido_original, unidad_original, texto_original,
        precio, precio_antes, promocion, etiqueta_promocion,
        precio_condicionado, condicion_precio, vendedor, disponible,
        url_extraccion, archivo_raw)
      SELECT a.anuncio_id, t.comercio_id, t.id_ejecucion, t.fecha_hora_consulta,
             t.nombre, t.marca, t.subcategoria, t.presentacion,
             t.contenido, t.unidad, t.texto_original,
             t.precio, t.precio_antes, t.promocion, t.etiqueta_promocion,
             t.precio_condicionado, t.condicion_precio, t.vendedor, t.disponible,
             t.url_extraccion, t.archivo_raw
        FROM tmp_carga t
        JOIN anuncios a
          ON a.comercio_id = t.comercio_id
         AND a.id_producto_comercio = t.id_producto_comercio
       WHERE true
      ON CONFLICT (comercio_id, anuncio_id, fecha_hora_consulta) DO NOTHING")
    dbExecute(con, "DROP TABLE tmp_carga")
    n
  })
  salida$nuevas <- as.integer(n)
  salida$existentes <- nrow(carga) - as.integer(n)
  salida
}

# -----------------------------------------------------------------------------
# 10. Función general
# -----------------------------------------------------------------------------
# Ejecuta la recolección de los comercios indicados, guarda las copias crudas,
# registra cada corrida en la tabla ejecuciones e inserta los resultados.
# Un comercio que falle (al extraer o al cargar) no detiene a los demás.
#
#   modo = "en_linea"      consulta los sitios y guarda raw/
#   modo = "sin_conexion"  reprocesa raw/ de la corrida id_ejecucion (por
#                          defecto la más reciente) sin consultar los sitios

ejecutar_recoleccion <- function(comercios = COMERCIOS$clave,
                                 modo = c("en_linea", "sin_conexion"),
                                 id_ejecucion = NULL,
                                 ruta_bd = CONFIG$ruta_bd) {
  modo <- match.arg(modo)
  if (is.null(id_ejecucion)) {
    id_ejecucion <- if (modo == "en_linea") nuevo_id_ejecucion() else ultima_ejecucion_raw()
  }
  avisar("Ejecución ", id_ejecucion, " · modo ", modo)
  con <- conectar_bd(ruta_bd)
  on.exit(dbDisconnect(con))

  resumen <- map(comercios, function(clave) {
    nombre <- COMERCIOS$nombre[COMERCIOS$clave == clave]
    inicio <- ahora_iso()
    if (modo == "sin_conexion") {
      # Sin copias de este comercio para esa corrida no hay nada que reprocesar
      # (y no se registra una ejecución que nunca existió).
      if (!file.exists(ruta_manifiesto(clave, id_ejecucion))) {
        avisar(nombre, ": no hay copias de la corrida ", id_ejecucion, " en ", CONFIG$ruta_raw, "/; se omite")
        return(NULL)
      }
      # La hora de inicio real es la de la primera respuesta guardada.
      inicio <- min(leer_manifiesto(clave, id_ejecucion)$fecha_hora_consulta, na.rm = TRUE)
    }
    registrar_inicio(con, id_ejecucion, clave, inicio, modo)
    salida <- safely(EXTRACTORES[[clave]])(id_ejecucion, modo)
    resultado <- if (is.null(salida$error)) salida$result else
      list(datos = tabla_vacia(), estado = "fallida", n_total_sitio = NA_integer_,
           detalle = paste("Error:", conditionMessage(salida$error)))

    carga <- tryCatch(insertar_observaciones(con, resultado$datos, clave), error = \(e) e)
    if (inherits(carga, "error")) {
      n_nuevas <- 0L
      resultado$estado <- "fallida"
      resultado$detalle <- paste0(resultado$detalle, "; ERROR al cargar en SQLite: ", conditionMessage(carga),
                                  " (las copias de raw/ permiten reprocesar esta corrida)")
    } else {
      n_nuevas <- carga$nuevas
      if (carga$descartadas > 0) {
        resultado$detalle <- paste0(resultado$detalle, "; ", carga$descartadas,
                                    " filas descartadas antes de cargar (", carga$motivos, ")")
      }
      if (carga$existentes > 0) {
        resultado$detalle <- paste0(resultado$detalle, "; ", carga$existentes, " observaciones ya estaban en la base")
      }
    }

    tryCatch({
      if (modo == "en_linea") {
        registrar_fin(con, id_ejecucion, clave, resultado, nrow(resultado$datos))
      } else {
        # Sin conexión no se reescribe una corrida que ya estaba registrada; solo
        # se completa si la base era nueva (estado provisional 'en curso').
        dbExecute(con, "
          UPDATE ejecuciones SET fecha_hora_fin = ?, n_registros = ?, n_total_sitio = ?, estado = ?, detalle = ?
           WHERE id_ejecucion = ? AND comercio_id = ? AND detalle = 'en curso'",
          params = list(ahora_iso(), nrow(resultado$datos), resultado$n_total_sitio, resultado$estado,
                        paste("[reprocesado sin conexión]", resultado$detalle), id_ejecucion, id_comercio(clave)))
      }
    }, error = \(e) avisar(nombre, ": no se pudo actualizar la tabla ejecuciones (", conditionMessage(e), ")"))

    avisar(nombre, ": ", nrow(resultado$datos), " productos · estado ", resultado$estado,
           " · ", n_nuevas, " observaciones nuevas en la base")
    tibble(id_ejecucion = id_ejecucion, comercio = nombre, estado = resultado$estado,
           productos = nrow(resultado$datos), observaciones_nuevas = n_nuevas,
           total_sitio = resultado$n_total_sitio, detalle = resultado$detalle)
  }) |> list_rbind()

  resumen
}

# Reconstruye la base solo con las copias de raw/ (todas las corridas), sin
# consultar los sitios. Útil si un sitio cambió o bloquea el acceso:
#   reconstruir_desde_raw("comparador_reconstruido.sqlite")
reconstruir_desde_raw <- function(ruta_bd = CONFIG$ruta_bd) {
  map(ejecuciones_en_raw(), \(id) ejecutar_recoleccion(modo = "sin_conexion", id_ejecucion = id, ruta_bd = ruta_bd)) |>
    list_rbind()
}
