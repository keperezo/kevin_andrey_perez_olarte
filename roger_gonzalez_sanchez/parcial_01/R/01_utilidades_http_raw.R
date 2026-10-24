# 01_utilidades_http_raw.R
# Utilidades generales, peticiones HTTP (httr2), almacenamiento de respuestas
# crudas en raw/ y lectura en modo sin conexión (offline).


`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Primer valor escalar de un elemento JSON (NULL / lista vacía -> NA).
chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- unlist(x, use.names = FALSE)
  if (length(x) == 0 || is.na(x[[1]])) return(NA_character_)
  as.character(x[[1]])
}
num1 <- function(x) suppressWarnings(as.numeric(chr1(x)))

vacio_a_na <- function(x) { x[!is.na(x) & str_trim(x) == ""] <- NA_character_; x }

log_msg <- function(...) {
  message(format(Sys.time(), "[%H:%M:%S] ", tz = TZ_COLOMBIA), ...)
}

# Fecha-hora ISO 8601 de Colombia
ahora_iso_co <- function(t = Sys.time()) {
  paste0(format(t, "%Y-%m-%dT%H:%M:%S", tz = TZ_COLOMBIA), OFFSET_COLOMBIA)
}
fecha_hoy_co <- function() format(Sys.time(), "%Y-%m-%d", tz = TZ_COLOMBIA)
nuevo_id_ejecucion <- function(prefijo = "EJ") {
  paste0(prefijo, "_", format(Sys.time(), "%Y%m%dT%H%M%S", tz = TZ_COLOMBIA))
}

fecha_de_id <- function(id) {
  format(as.Date(str_extract(id, "\\d{8}"), format = "%Y%m%d"), "%Y-%m-%d")
}

# Entorno en el que se ejecuta el código (se guarda en el manifiesto y en la
# tabla ejecuciones para documentar pruebas en equipos distintos).
entorno_actual <- function() {
  so <- tryCatch(utils::osVersion, error = function(e) NULL) %||% Sys.info()[["sysname"]]
  paste0(R.version$version.string, " | ", so, " | ", Sys.info()[["machine"]])
}

slug <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- str_to_lower(str_replace_all(x, "[^A-Za-z0-9]+", "-"))
  str_sub(str_replace_all(x, "^-+|-+$", ""), 1, 40)
}


# Contexto de extracción: define el modo y la carpeta raw/ que corresponde.
#   online      -> raw/{comercio}/{fecha}/        (JSON comprimido .json.gz)
#   test        -> raw/test/{comercio}/           (JSON plano .json)
#   viabilidad  -> raw/viabilidad/{fecha}/{comercio}/
#   offline     -> lee de raw/{comercio}/{fecha}/ (o raw/test/{comercio}/)

crear_contexto <- function(clave, test_mode = FALSE, offline = FALSE,
                           id_ejecucion = NULL, fecha_offline = NULL,
                           modo_raw = NULL) {
  if (!clave %in% names(COMERCIOS)) stop("Comercio desconocido: ", clave)
  modo <- modo_raw %||% if (offline) "offline" else if (test_mode) "test" else "online"
  fecha <- fecha_hoy_co()
  dir_raw <- switch(
    modo,
    test       = file.path(DIR_RAW_TEST, clave),
    viabilidad = file.path(DIR_RAW_VIAB, fecha, clave),
    online     = file.path(DIR_RAW, clave, fecha),
    offline    = if (test_mode) file.path(DIR_RAW_TEST, clave)
                 else file.path(DIR_RAW, clave, fecha_offline %||% ultima_fecha_raw(clave))
  )
  ctx <- list(
    clave = clave,
    comercio = COMERCIOS[[clave]]$nombre,
    cfg = COMERCIOS[[clave]],
    modo = modo,
    test_mode = test_mode,
    id_ejecucion = id_ejecucion %||% nuevo_id_ejecucion(),
    dir_raw = dir_raw,
    comprimir = modo %in% c("online", "viabilidad"),
    estado = new.env()
  )
  ctx$estado$n <- 0L
  ctx$estado$cookies <- tempfile(paste0("cookies_", clave, "_"))
  ctx
}

ultima_fecha_raw <- function(clave) {
  d <- list.dirs(file.path(DIR_RAW, clave), recursive = FALSE, full.names = FALSE)
  d <- sort(d[str_detect(d, "^\\d{4}-\\d{2}-\\d{2}$")])
  if (length(d) == 0) stop("No hay carpetas raw/", clave, "/{fecha}/ para el modo offline.")
  tail(d, 1)
}


# Petición HTTP con httr2: user-agent identificable, reintentos con espera,
# pausa de cortesía y guardado del cuerpo crudo + manifiesto.

realizar_peticion <- function(ctx, url, cuerpo_json = NULL, headers = list(),
                              origen = "categoria", etiqueta = "", pagina = 1L,
                              guardar = TRUE, fn_total = NULL) {
  req <- request(url) |>
    req_user_agent(USER_AGENT) |>
    req_headers(Accept = "application/json", !!!headers) |>
    req_timeout(TIMEOUT_SEG) |>
    req_retry(max_tries = MAX_REINTENTOS, retry_on_failure = TRUE,
              backoff = function(i) 2 * i) |>
    req_error(is_error = function(resp) FALSE) |>
    req_cookie_preserve(ctx$estado$cookies)
  if (!is.null(cuerpo_json)) req <- req_body_raw(req, cuerpo_json, type = "application/json")

  Sys.sleep(PAUSA_ENTRE_PETICIONES)
  fecha_hora <- ahora_iso_co()
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    log_msg("  ! Error de red en ", ctx$comercio, ": ", conditionMessage(resp))
    return(list(ok = FALSE, status = NA_integer_, texto = NA_character_, meta = NULL))
  }
  status <- resp_status(resp)
  texto <- resp_body_string(resp, encoding = "UTF-8")
  total <- if (!is.null(fn_total)) tryCatch(fn_total(texto, resp), error = function(e) NA_real_) else NA_real_

  meta <- list(
    id_ejecucion = ctx$id_ejecucion, comercio = ctx$clave, origen = origen,
    etiqueta = etiqueta, pagina = as.integer(pagina), url = url,
    cuerpo_peticion = cuerpo_json %||% NA_character_, http_status = status,
    total_reportado = total, fecha_hora_consulta = fecha_hora, archivo = NA_character_,
    modo = ctx$modo, entorno = entorno_actual()
  )
  if (guardar && status < 400) meta <- guardar_raw(ctx, texto, meta)
  list(ok = status < 400, status = status, texto = texto, meta = meta)
}

guardar_raw <- function(ctx, texto, meta) {
  dir.create(ctx$dir_raw, recursive = TRUE, showWarnings = FALSE)
  ctx$estado$n <- ctx$estado$n + 1L
  base <- sprintf("%s_%03d_%s_%s_p%03d", ctx$id_ejecucion, ctx$estado$n,
                  meta$origen, slug(meta$etiqueta), meta$pagina)
  archivo <- paste0(base, if (ctx$comprimir) ".json.gz" else ".json")
  ruta <- file.path(ctx$dir_raw, archivo)
  con <- if (ctx$comprimir) gzfile(ruta, "wb") else file(ruta, "wb")
  writeBin(charToRaw(enc2utf8(texto)), con)
  close(con)
  meta$archivo <- archivo
  linea <- jsonlite::toJSON(meta, auto_unbox = TRUE, na = "null", null = "null")
  cat(linea, "\n", file = file.path(ctx$dir_raw, "manifiesto.jsonl"), append = TRUE, sep = "")
  meta
}

leer_texto_raw <- function(ruta) {
  if (str_detect(ruta, "\\.gz$")) {
    con <- gzfile(ruta, "rb"); on.exit(close(con))
    paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    x <- readChar(ruta, file.size(ruta), useBytes = TRUE)
    Encoding(x) <- "UTF-8"
    x
  }
}

# Lee el manifiesto de una carpeta raw y devuelve las respuestas de UNA
# ejecución (por defecto, la más reciente de esa fecha).
cargar_raw_offline <- function(ctx, id_ejecucion = NULL) {
  manif <- file.path(ctx$dir_raw, "manifiesto.jsonl")
  if (!file.exists(manif)) stop("No existe ", manif, " (¿se ejecutó antes en modo online/test?)")
  entradas <- readLines(manif, warn = FALSE, encoding = "UTF-8") |>
    discard(~ str_trim(.x) == "") |>
    map(~ jsonlite::fromJSON(.x, simplifyVector = TRUE))
  ids <- map_chr(entradas, "id_ejecucion")
  id_obj <- id_ejecucion %||% tail(sort(unique(ids)), 1)
  entradas <- keep(entradas, ~ .x$id_ejecucion == id_obj)
  if (length(entradas) == 0) stop("La ejecución ", id_obj, " no está en ", manif)
  log_msg("  [offline] ", ctx$comercio, ": ", length(entradas), " archivos de ",
          ctx$dir_raw, " (", id_obj, ")")
  map(entradas, function(m) {
    m <- map(m, ~ if (length(.x) == 0) NA else .x)
    list(meta = m, texto = leer_texto_raw(file.path(ctx$dir_raw, m$archivo)))
  })
}

# Todos los manifiestos de raw/{comercio}/{fecha}/ en una tabla (una fila por
# petición guardada) + tamaño en disco de cada archivo. Lo usa el informe.
leer_manifiestos <- function(dir_raw = DIR_RAW, comercios = names(COMERCIOS)) {
  rutas <- list.files(dir_raw, pattern = "^manifiesto\\.jsonl$", recursive = TRUE, full.names = TRUE)
  rutas <- rutas[!str_detect(rutas, "/(test|viabilidad)/")]
  if (length(rutas) == 0) return(tibble())
  map(rutas, function(ruta) {
    lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8") |> discard(~ str_trim(.x) == "")
    map(lineas, function(l) {
      m <- jsonlite::fromJSON(l, simplifyVector = TRUE)
      as_tibble(map(m, ~ if (length(.x) == 0) NA else .x))
    }) |>
      list_rbind() |>
      mutate(carpeta = dirname(ruta), fecha_carpeta = basename(dirname(ruta)),
             bytes = file.size(file.path(dirname(ruta), archivo)))
  }) |>
    list_rbind() |>
    filter(comercio %in% comercios) |>
    mutate(nombre_comercio = map_chr(comercio, ~ COMERCIOS[[.x]]$nombre))
}
