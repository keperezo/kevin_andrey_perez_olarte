# =============================================================================
# scraper.R  -  Módulo de extracción (Taller 1, Minería de Datos, UNAL)
# Categoría 29: Medicamentos para dolor e inflamación
#
# Comercios: La Rebaja (API VTEX), Locatel (API VTEX), Cafam (HTML paginado)
#
# USO (desde R, con el directorio de trabajo en la carpeta del taller):
#   source("scraper.R")
#   ejecutar_scraping()                                   # en línea, todos los comercios
#   ejecutar_scraping(comercios = "cafam")                # solo uno
#   ejecutar_scraping(modo = "offline")                   # reprocesa la última copia de raw/
#   ejecutar_scraping(modo = "offline", ejecucion_raw = "20261008T113000")
#   ejecutar_scraping(reanudar = "20261008T123757")       # completa una ejecución parcial:
#                                                         # reutiliza raw/ y pide solo lo que falta
#
# Cada ejecución:
#   - respeta el robots.txt (User-agent: *) y no consulta rutas prohibidas;
#   - guarda las respuestas originales comprimidas en raw/<comercio>/<ejecucion>/;
#   - inserta en SQLite (comparador_precios.sqlite) SIN sobrescribir observaciones;
#   - registra la corrida en la tabla `ejecuciones` (completa / parcial / fallida).
#
# Paquetes: httr, jsonlite, DBI, RSQLite
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
})

CFG <- list(
  ruta_db    = "comparador_precios.sqlite",
  dir_raw    = "raw",
  pausa      = 3,     # segundos entre peticiones (subida de 1.5: los sitios se saturaron)
  timeout    = 300,   # segundos máximos TOTALES por petición (las páginas de Cafam pesan ~2 MB y
                      # el sitio puede servirlas lento; antes 60)
  lento_bps  = 500,   # se aborta (y reintenta) solo si la transferencia se estanca:
  lento_seg  = 60,    #   menos de `lento_bps` bytes/s durante `lento_seg` segundos seguidos
  intentos   = 3,     # reintentos por petición, con espera exponencial
  zona       = "America/Bogota",
  user_agent = "TallerMineriaDatos-UNAL/1.0 (uso academico)"
)

# ---- Utilidades generales ---------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- x[[1]]
  if (is.null(v) || (length(v) == 1 && is.na(v)) || !nzchar(as.character(v))) return(NA_character_)
  as.character(v)
}
num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(v)) NA_real_ else v
}

# Fecha y hora en ISO 8601 con hora de Colombia, p. ej. 2026-10-08T11:35:00-05:00
ahora_iso <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = CFG$zona)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", x)
}

# Columnas que TODA función de comercio debe devolver, siempre en este orden
COLS <- c("comercio", "producto_id", "sku_comercio", "ean", "referencia",
          "nombre_original", "marca_original", "descripcion_original",
          "categoria_origen", "categoria_consulta", "url",
          "precio", "precio_lista", "en_promocion", "disponible", "moneda",
          "texto_original", "fecha_hora")

unico_por_sku <- function(df) df[!duplicated(df[, c("comercio", "sku_comercio")]), , drop = FALSE]

# ---- robots.txt -------------------------------------------------------------
leer_robots <- function(base) {
  vacio <- list(reglas = character(), delay = NA_real_)
  r <- tryCatch(GET(paste0(base, "/robots.txt"), user_agent(CFG$user_agent), timeout(30)),
                error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) return(vacio)
  lin <- trimws(strsplit(content(r, "text", encoding = "UTF-8"), "\r?\n")[[1]])
  reglas <- character(); delay <- NA_real_; grupo <- FALSE; prev_ua <- FALSE
  for (l in lin) {
    if (grepl("^user-agent:", l, ignore.case = TRUE)) {
      agente <- trimws(sub("^user-agent:", "", l, ignore.case = TRUE))
      if (!prev_ua) grupo <- FALSE
      grupo <- grupo || identical(agente, "*")
      prev_ua <- TRUE
    } else if (nzchar(l)) {
      prev_ua <- FALSE
      if (grupo && grepl("^disallow:", l, ignore.case = TRUE)) {
        v <- trimws(sub("^disallow:", "", l, ignore.case = TRUE))
        if (nzchar(v)) reglas <- c(reglas, v)
      } else if (grupo && grepl("^crawl-delay:", l, ignore.case = TRUE)) {
        delay <- suppressWarnings(as.numeric(trimws(sub("^crawl-delay:", "", l, ignore.case = TRUE))))
      }
    }
  }
  list(reglas = reglas, delay = delay)
}

regla_a_regex <- function(regla) {
  x <- gsub("([.+?(){}|^\\[\\]\\\\])", "\\\\\\1", regla, perl = TRUE)
  x <- gsub("*", ".*", x, fixed = TRUE)
  paste0("^", x)
}

# Se ignoran las reglas Allow: criterio conservador (más estricto que el robots.txt)
permitido <- function(reglas, ruta) {
  if (!length(reglas)) return(TRUE)
  !any(vapply(reglas, function(r) grepl(regla_a_regex(r), ruta, perl = TRUE), logical(1)))
}

.robots_cache <- new.env()
reglas_robots <- function(base) {
  if (is.null(.robots_cache[[base]])) .robots_cache[[base]] <- leer_robots(base)
  .robots_cache[[base]]
}

# ---- Copias crudas (raw/) y obtención de páginas ----------------------------
# Las copias se escriben y leen como BYTES UTF-8 (modo binario): así no dependen de la
# configuración regional de R ni del sistema operativo (Windows, macOS, Linux, Colab).
guardar_raw <- function(texto, archivo) {
  con <- gzfile(archivo, "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(texto)), con)
}
leer_raw <- function(archivo) {
  con <- gzfile(archivo, "rb")
  on.exit(close(con))
  partes <- list()
  repeat {
    b <- readBin(con, "raw", n = 1048576L)
    if (!length(b)) break
    partes[[length(partes) + 1L]] <- b
  }
  texto <- rawToChar(do.call(c, partes))
  Encoding(texto) <- "UTF-8"
  texto
}

# Reintenta con espera exponencial y respeta Retry-After. Un 400/401/403/404 NO se reintenta
# (no se intenta eludir bloqueos). El error final informa el motivo real del último intento.
get_con_reintentos <- function(url, intentos = CFG$intentos) {
  motivo <- "sin respuesta"
  for (i in seq_len(intentos)) {
    r <- tryCatch(GET(url, user_agent(CFG$user_agent),
                      add_headers(`Accept-Language` = "es-CO,es;q=0.9"),
                      config(connecttimeout = 30, low_speed_limit = CFG$lento_bps,
                             low_speed_time = CFG$lento_seg),
                      timeout(CFG$timeout)),
                  error = function(e) e)
    espera <- CFG$pausa * 2^i * 2
    if (inherits(r, "error")) {
      motivo <- paste("error de conexión:", conditionMessage(r))
    } else {
      st <- status_code(r)
      if (st %in% c(200, 206)) return(r)
      motivo <- sprintf("HTTP %d", st)
      if (st %in% c(400, 401, 403, 404)) stop(sprintf("HTTP %d en %s", st, url))
      ra <- suppressWarnings(as.numeric(headers(r)[["retry-after"]]))
      if (length(ra) == 1 && !is.na(ra)) espera <- max(espera, min(ra, 300))
    }
    message(sprintf("   ! intento %d/%d fallido (%s)", i, intentos, motivo))
    if (i < intentos) Sys.sleep(espera)
  }
  stop(sprintf("No se pudo obtener tras %d intentos (%s): %s", intentos, motivo, url))
}

# Devuelve list(texto, fecha_hora, extra).
#  - modo offline: lee raw/ (NULL si no hay más archivos).
#  - modo online con ctx$reanudar: si la página ya está en raw/ de esa ejecución, la reutiliza
#    sin tocar el sitio y solo consulta las páginas que faltan.
fuente <- function(ctx, comercio, clave, url, ext = "json") {
  dir_run <- file.path(CFG$dir_raw, comercio, ctx$ejecucion_id)
  archivo <- file.path(dir_run, paste0(clave, ".", ext, ".gz"))
  indice  <- file.path(dir_run, "indice.csv")

  hay_copia <- file.exists(archivo) && file.exists(indice)
  if (ctx$modo == "offline" && !hay_copia) return(NULL)
  if (hay_copia && (ctx$modo == "offline" || isTRUE(ctx$reanudar))) {
    idx <- read.csv(indice, stringsAsFactors = FALSE)
    fila <- idx[idx$clave == clave, , drop = FALSE][1, ]
    return(list(texto = leer_raw(archivo), fecha_hora = fila$fecha_hora,
                extra = if (is.na(fila$extra)) "" else fila$extra))
  }

  base <- sub("^(https?://[^/]+).*$", "\\1", url)
  ruta <- sub("^https?://[^/]+", "", url)
  rb <- reglas_robots(base)
  if (!is.na(rb$delay) && rb$delay > 30)
    stop(sprintf("%s pide Crawl-delay de %s s: no se consulta.", base, rb$delay))
  if (!permitido(rb$reglas, ruta))
    stop("robots.txt (User-agent: *) no permite la ruta: ", ruta)

  Sys.sleep(CFG$pausa)
  r <- get_con_reintentos(url)
  fecha_hora <- ahora_iso()
  texto <- content(r, "text", encoding = "UTF-8")

  dir.create(dir_run, recursive = TRUE, showWarnings = FALSE)
  guardar_raw(texto, archivo)
  extra <- headers(r)[["resources"]] %||% ""
  fila <- data.frame(clave = clave, url = url, fecha_hora = fecha_hora,
                     status = status_code(r), extra = extra, stringsAsFactors = FALSE)
  write.table(fila, indice, sep = ",", row.names = FALSE, qmethod = "double",
              col.names = !file.exists(indice), append = file.exists(indice))
  list(texto = texto, fecha_hora = fecha_hora, extra = extra)
}

# =============================================================================
# COMERCIOS VTEX CON API PÚBLICA: La Rebaja y Locatel
#   /api/catalog_system/pub/products/search?fq=C:/<ruta de ids>/&_from=0&_to=49
#   El total de la categoría viene en el header "resources" (p. ej. "0-49/280").
# =============================================================================
parsear_vtex_api <- function(texto, comercio, fecha_hora, categoria_consulta) {
  prods <- fromJSON(texto, simplifyVector = FALSE)
  filas <- list()
  for (p in prods) {
    espec <- vapply(unlist(p$allSpecifications), function(s)
      paste0(s, ": ", paste(unlist(p[[s]]), collapse = ", ")), character(1))
    for (it in p$items) {
      of <- tryCatch(it$sellers[[1]]$commertialOffer, error = function(e) NULL)
      precio <- num(of$Price); lista <- num(of$ListPrice)
      if (!is.na(precio) && precio <= 0) precio <- NA_real_   # 0 = sin precio, no es un precio
      if (!is.na(lista)  && lista  <= 0) lista  <- NA_real_
      promo <- if (!is.na(precio) && !is.na(lista)) as.integer(lista > precio) else NA_integer_
      disp_n <- num(of$AvailableQuantity)
      filas[[length(filas) + 1]] <- data.frame(
        comercio = comercio,
        producto_id = chr(p$productId),
        sku_comercio = chr(it$itemId %||% p$productId),
        ean = chr(it$ean),
        referencia = chr(p$productReference),
        nombre_original = chr(p$productName),
        marca_original = chr(p$brand),
        descripcion_original = chr(p$description),
        categoria_origen = chr(paste(unlist(p$categories), collapse = " || ")),
        categoria_consulta = categoria_consulta,
        url = chr(p$link),
        precio = precio, precio_lista = lista, en_promocion = promo,
        disponible = if (is.na(disp_n)) NA_integer_ else as.integer(disp_n > 0),
        moneda = "COP",
        texto_original = paste(c(p$productName, p$description, espec), collapse = " | "),
        fecha_hora = fecha_hora, stringsAsFactors = FALSE)
    }
  }
  list(datos = if (length(filas)) do.call(rbind, filas) else NULL, n_productos = length(prods))
}

scrape_vtex_api <- function(ctx, comercio, base, categorias, tam = 50L) {
  filas <- list(); completo <- TRUE; notas <- character(); total_sitio <- 0L
  cortado <- FALSE   # TRUE tras un fallo de red: no se sigue pidiendo al sitio
  for (cat in categorias) {
    if (cortado) {
      notas <- c(notas, paste("categoría", cat, ": no consultada (recolección detenida tras un fallo)"))
      next
    }
    desde <- 0L; total_cat <- NA_integer_; recibidos <- 0L
    repeat {
      ruta <- sprintf("/api/catalog_system/pub/products/search?fq=C:/%s/&_from=%d&_to=%d",
                      cat, desde, desde + tam - 1L)
      clave <- sprintf("cat_%s_%05d", gsub("/", "-", cat), desde)
      res <- tryCatch(fuente(ctx, comercio, clave, paste0(base, ruta), ext = "json"),
                      error = function(e) {
                        notas <<- c(notas, paste(clave, conditionMessage(e)))
                        completo <<- FALSE
                        cortado <<- TRUE
                        NULL
                      })
      if (is.null(res)) break
      if (is.na(total_cat) && nzchar(res$extra))
        total_cat <- suppressWarnings(as.integer(sub(".*/", "", res$extra)))
      pp <- parsear_vtex_api(res$texto, comercio, res$fecha_hora, cat)
      if (pp$n_productos == 0) break
      filas[[length(filas) + 1]] <- pp$datos
      recibidos <- recibidos + pp$n_productos
      desde <- desde + tam
      if (!is.na(total_cat) && desde >= total_cat) break
      if (desde >= 2500L) {   # límite de VTEX para el desplazamiento
        notas <- c(notas, paste("categoría", cat, ": límite de 2500 alcanzado")); completo <- FALSE; break
      }
    }
    if (is.na(total_cat) || recibidos != total_cat) completo <- FALSE
    if (!is.na(total_cat)) total_sitio <- total_sitio + total_cat
    notas <- c(notas, sprintf("categoría %s: sitio=%s, recibidos=%d",
                              cat, ifelse(is.na(total_cat), "NA", total_cat), recibidos))
  }
  datos <- if (length(filas)) unico_por_sku(do.call(rbind, filas)) else NULL
  list(datos = datos, total_sitio = total_sitio, completo = completo, notas = notas)
}

# La Rebaja: Droguería > Dolor e Inflamación (9/50, con sus subcategorías) y los tres
# "Droga blanca > dolor" (9/51/253, 931, 932). Se excluyen Naturales/Homeopáticos,
# genéricos con fórmula (0 productos) y Mascotas.
scrape_larebaja <- function(ctx) {
  scrape_vtex_api(ctx, "larebaja", "https://www.larebajavirtual.com",
                  categorias = c("9/50", "9/51/253", "9/51/931", "9/51/932"))
}

# Locatel clasifica por sistema/aparato (no hay categoría "dolor"). Se recolectan por
# completo estas cuatro categorías y la pertenencia a la categoría 29 se decide luego,
# en la limpieza, con reglas documentadas (principio activo):
#   4/25/165 Medicamentos > sistema-nervioso      4/25/160 Medicamentos > aparato-locomotor
#   4/28/190 Medicamentos Otc > sistema-nervioso  4/28/187 Medicamentos Otc > aparato-locomotor
# ORDEN: primero las categorías OTC y aparato locomotor (las más relevantes para el tema) y al
# final la de sistema nervioso con receta, que es la más grande. Si el sitio falla, la
# recolección se detiene (ver `cortado` en scrape_vtex_api) y lo importante ya quedó guardado.
scrape_locatel <- function(ctx) {
  scrape_vtex_api(ctx, "locatel", "https://www.locatelcolombia.com",
                  categorias = c("4/28/190", "4/28/187", "4/25/160", "4/25/165"))
}

# =============================================================================
# CAFAM: HTML de categoría con ?page=N. Los datos están en el HTML:
#   - window.__STATE__ = {...}  (caché de VTEX IO)  -> fuente principal
#   - <script type="application/ld+json"> ItemList  -> respaldo
#   - "recordsFiltered":N                           -> total de la categoría
# =============================================================================
extraer_objeto_json <- function(txt) {
  cod <- utf8ToInt(txt)
  if (anyNA(cod)) return(NULL)
  prof <- 0L; en_cadena <- FALSE; escape <- FALSE
  for (i in seq_along(cod)) {
    ch <- cod[i]
    if (en_cadena) {
      if (escape) escape <- FALSE
      else if (ch == 92L) escape <- TRUE
      else if (ch == 34L) en_cadena <- FALSE
    } else if (ch == 34L) en_cadena <- TRUE
    else if (ch == 123L) prof <- prof + 1L
    else if (ch == 125L) {
      prof <- prof - 1L
      if (prof == 0L) return(intToUtf8(cod[1:i]))
    }
  }
  NULL
}

extraer_state <- function(html) {
  m <- regexpr("__STATE__\\s*=\\s*\\{", html, perl = TRUE)
  if (m < 0) return(NULL)
  inicio <- m + attr(m, "match.length") - 1L
  # substr con límite explícito: substring() corta en 1.000.000 de caracteres por defecto
  resto <- substr(html, inicio, nchar(html))
  fin <- regexpr("</script>", resto, fixed = TRUE)
  if (fin > 0) resto <- substr(resto, 1, fin - 1L)
  obj <- extraer_objeto_json(resto)
  if (is.null(obj)) return(NULL)
  tryCatch(fromJSON(obj, simplifyVector = FALSE), error = function(e) NULL)
}

# Reemplaza las referencias {"type":"id","id":"..."} del caché por sus nodos
resolver <- function(x, st, prof = 0L) {
  if (prof > 8L || is.null(x)) return(x)
  if (is.list(x)) {
    if (identical(x[["type"]], "id") && !is.null(x[["id"]]))
      return(resolver(st[[x[["id"]]]], st, prof + 1L))
    if (identical(x[["type"]], "json") && !is.null(x[["json"]])) return(x[["json"]])
    return(lapply(x, resolver, st = st, prof = prof + 1L))
  }
  x
}

campo <- function(x, prefijo) {   # campos con argumentos, p. ej. items({"filter":"ALL"})
  if (!is.list(x) || is.null(names(x))) return(NULL)
  nm <- names(x)[startsWith(names(x), prefijo)]
  if (length(nm)) x[[nm[1]]] else NULL
}

productos_desde_state <- function(st, base, fecha_hora, categoria_consulta) {
  claves <- names(st)
  pk <- grep("^Product:[^.]+$", claves, value = TRUE)
  filas <- list()
  for (k in pk) {
    p <- resolver(st[[k]], st)
    items <- campo(p, "items")
    it <- if (length(items)) items[[1]] else NULL
    of <- tryCatch(it$sellers[[1]]$commertialOffer, error = function(e) NULL)
    precio <- num(p$priceRange$sellingPrice$lowPrice)
    lista  <- num(p$priceRange$listPrice$lowPrice)
    if (is.na(precio)) precio <- num(of$Price)
    if (is.na(lista))  lista  <- num(of$ListPrice)
    if (!is.na(precio) && precio <= 0) precio <- NA_real_
    if (!is.na(lista)  && lista  <= 0) lista  <- NA_real_
    promo <- if (!is.na(precio) && !is.na(lista)) as.integer(lista > precio) else NA_integer_
    disp_n <- num(of$AvailableQuantity)
    url <- chr(p$link)
    if (is.na(url) && !is.na(chr(p$linkText))) url <- paste0(base, "/", p$linkText, "/p")
    filas[[length(filas) + 1]] <- data.frame(
      comercio = "cafam",
      producto_id = chr(p$productId),
      sku_comercio = chr(it$itemId %||% p$productId),
      ean = chr(it$ean),
      referencia = chr(p$productReference),
      nombre_original = chr(p$productName),
      marca_original = chr(p$brand),
      descripcion_original = chr(p$description),
      categoria_origen = chr(paste(unlist(p$categories), collapse = " || ")),
      categoria_consulta = categoria_consulta,
      url = url,
      precio = precio, precio_lista = lista, en_promocion = promo,
      disponible = if (is.na(disp_n)) NA_integer_ else as.integer(disp_n > 0),
      moneda = "COP",
      texto_original = paste(c(p$productName, p$description), collapse = " | "),
      fecha_hora = fecha_hora, stringsAsFactors = FALSE)
  }
  if (length(filas)) do.call(rbind, filas) else NULL
}

productos_desde_ldjson <- function(html, fecha_hora, categoria_consulta) {
  bl <- regmatches(html, gregexpr('(?s)<script type="application/ld\\+json"[^>]*>.*?</script>',
                                  html, perl = TRUE))[[1]]
  filas <- list()
  for (b in bl) {
    j <- sub("</script>$", "", sub("(?s)^<script[^>]*>", "", b, perl = TRUE))
    obj <- tryCatch(fromJSON(j, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj) || !identical(obj[["@type"]], "ItemList")) next
    for (el in obj$itemListElement) {
      it <- el$item
      ofe <- it$offers
      precio <- num(ofe$lowPrice %||% ofe$price)
      if (is.na(precio)) precio <- num(tryCatch(ofe$offers[[1]]$price, error = function(e) NULL))
      if (!is.na(precio) && precio <= 0) precio <- NA_real_
      id <- chr(it[["@id"]])
      filas[[length(filas) + 1]] <- data.frame(
        comercio = "cafam", producto_id = NA_character_,
        sku_comercio = chr(it$sku %||% id), ean = chr(it$gtin13 %||% it$gtin),
        referencia = NA_character_,
        nombre_original = chr(it$name), marca_original = chr(it$brand$name),
        descripcion_original = chr(it$description),
        categoria_origen = NA_character_, categoria_consulta = categoria_consulta,
        url = id, precio = precio, precio_lista = NA_real_, en_promocion = NA_integer_,
        disponible = NA_integer_, moneda = "COP",
        texto_original = paste(c(it$name, it$description), collapse = " | "),
        fecha_hora = fecha_hora, stringsAsFactors = FALSE)
    }
  }
  if (length(filas)) do.call(rbind, filas) else NULL
}

parsear_cafam_html <- function(html, fecha_hora, base, categoria_consulta) {
  st <- extraer_state(html)
  datos <- if (!is.null(st) && !is.null(names(st)))
    productos_desde_state(st, base, fecha_hora, categoria_consulta) else NULL
  origen <- "state"
  if (is.null(datos) || nrow(datos) == 0) {
    datos <- productos_desde_ldjson(html, fecha_hora, categoria_consulta)
    origen <- "ld+json (respaldo)"
  }
  if (is.null(datos)) { datos <- data.frame(); origen <- "ninguno" }
  list(datos = datos, origen = origen)
}

scrape_cafam <- function(ctx) {
  base <- "https://www.drogueriascafam.com.co"
  ruta <- "/salud-y-medicamentos/dolor-y-malestar"
  cat_consulta <- "1/10"   # Salud y medicamentos > Dolor y malestar (sus 4 subcategorías)
  filas <- list(); total <- NA_integer_; completo <- TRUE; notas <- character()
  pagina <- 1L
  repeat {
    ruta_p <- if (pagina == 1L) ruta else paste0(ruta, "?page=", pagina)
    res <- tryCatch(fuente(ctx, "cafam", sprintf("pag_%03d", pagina), paste0(base, ruta_p), ext = "html"),
                    error = function(e) {
                      notas <<- c(notas, paste("pág.", pagina, conditionMessage(e)))
                      completo <<- FALSE
                      NULL
                    })
    if (is.null(res)) break
    if (is.na(total)) {
      m <- regmatches(res$texto, regexpr('"recordsFiltered"\\s*:\\s*[0-9]+', res$texto, perl = TRUE))
      if (length(m)) total <- as.integer(sub(".*:\\s*", "", m))
    }
    pp <- parsear_cafam_html(res$texto, res$fecha_hora, base, cat_consulta)
    notas <- c(notas, sprintf("pág.%d: %d filas desde %s", pagina, nrow(pp$datos), pp$origen))
    if (nrow(pp$datos) == 0) break
    filas[[length(filas) + 1]] <- pp$datos
    acumulado <- nrow(unico_por_sku(do.call(rbind, filas)))
    if (!is.na(total) && acumulado >= total) break
    pagina <- pagina + 1L
    if (pagina > 60L) { notas <- c(notas, "Máximo de 60 páginas alcanzado"); completo <- FALSE; break }
  }
  datos <- if (length(filas)) unico_por_sku(do.call(rbind, filas)) else NULL
  if (is.null(datos) || is.na(total) || nrow(datos) < total) completo <- FALSE
  notas <- c(notas, sprintf("Cafam: total del sitio=%s, productos obtenidos=%d",
                            ifelse(is.na(total), "NA", total), if (is.null(datos)) 0L else nrow(datos)))
  list(datos = datos, total_sitio = total, completo = completo, notas = notas)
}

SCRAPERS <- list(larebaja = scrape_larebaja, cafam = scrape_cafam, locatel = scrape_locatel)

COMERCIOS <- data.frame(
  comercio = c("larebaja", "cafam", "locatel"),
  nombre   = c("La Rebaja", "Droguerías Cafam", "Locatel Colombia"),
  base_url = c("https://www.larebajavirtual.com", "https://www.drogueriascafam.com.co",
               "https://www.locatelcolombia.com"),
  stringsAsFactors = FALSE)

# =============================================================================
# BASE DE DATOS SQLITE
# =============================================================================
crear_esquema <- function(con) {
  dbExecute(con, "PRAGMA foreign_keys = ON")
  sentencias <- c(
    "CREATE TABLE IF NOT EXISTS comercios (
       comercio TEXT PRIMARY KEY, nombre TEXT, base_url TEXT)",
    "CREATE TABLE IF NOT EXISTS ejecuciones (
       ejecucion_id TEXT NOT NULL, comercio TEXT NOT NULL REFERENCES comercios(comercio),
       inicio TEXT NOT NULL, fin TEXT, modo TEXT,
       n_registros INTEGER, n_productos INTEGER, total_sitio INTEGER,
       estado TEXT CHECK (estado IN ('completa','parcial','fallida')), notas TEXT,
       PRIMARY KEY (ejecucion_id, comercio))",
    "CREATE TABLE IF NOT EXISTS anuncios (
       anuncio_id INTEGER PRIMARY KEY AUTOINCREMENT,
       comercio TEXT NOT NULL REFERENCES comercios(comercio),
       producto_id TEXT, sku_comercio TEXT NOT NULL, ean TEXT, referencia TEXT,
       nombre_original TEXT, marca_original TEXT, descripcion_original TEXT,
       categoria_origen TEXT, categoria_consulta TEXT, url TEXT, texto_original TEXT,
       primera_ejecucion TEXT,
       UNIQUE (comercio, sku_comercio))",
    "CREATE TABLE IF NOT EXISTS observaciones (
       obs_id INTEGER PRIMARY KEY AUTOINCREMENT,
       ejecucion_id TEXT NOT NULL, comercio TEXT NOT NULL,
       anuncio_id INTEGER NOT NULL REFERENCES anuncios(anuncio_id),
       fecha_hora TEXT NOT NULL,
       precio REAL, precio_lista REAL, en_promocion INTEGER, disponible INTEGER,
       moneda TEXT DEFAULT 'COP', url TEXT,
       UNIQUE (comercio, anuncio_id, fecha_hora),
       FOREIGN KEY (ejecucion_id, comercio) REFERENCES ejecuciones(ejecucion_id, comercio))",
    # Las siguientes tablas se llenan en la limpieza (taller_1.Rmd), no en el scraper
    "CREATE TABLE IF NOT EXISTS productos (
       producto_id TEXT PRIMARY KEY, descripcion TEXT, marca_std TEXT, subcategoria_std TEXT,
       contenido_valor REAL, contenido_unidad TEXT, unidades_empaque INTEGER,
       concentracion TEXT, comparable INTEGER DEFAULT 0)",
    "CREATE TABLE IF NOT EXISTS correspondencia (
       anuncio_id INTEGER PRIMARY KEY REFERENCES anuncios(anuncio_id),
       producto_id TEXT REFERENCES productos(producto_id),
       comparable INTEGER DEFAULT 0, regla TEXT)")
  for (s in sentencias) dbExecute(con, s)
  dbExecute(con, "INSERT OR IGNORE INTO comercios (comercio, nombre, base_url) VALUES (?,?,?)",
            params = unname(as.list(COMERCIOS)))
  invisible(TRUE)
}

guardar_resultados <- function(con, datos, ctx, comercio) {
  datos <- datos[, COLS]
  dbWithTransaction(con, {
    a <- unique(datos[, c("comercio", "producto_id", "sku_comercio", "ean", "referencia",
                          "nombre_original", "marca_original", "descripcion_original",
                          "categoria_origen", "categoria_consulta", "url", "texto_original")])
    a$primera_ejecucion <- ctx$ejecucion_id
    dbExecute(con, "INSERT OR IGNORE INTO anuncios
      (comercio, producto_id, sku_comercio, ean, referencia, nombre_original, marca_original,
       descripcion_original, categoria_origen, categoria_consulta, url, texto_original, primera_ejecucion)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)", params = unname(as.list(a)))
    ids <- dbGetQuery(con, "SELECT anuncio_id, comercio, sku_comercio FROM anuncios WHERE comercio = ?",
                      params = list(comercio))
    o <- merge(datos, ids, by = c("comercio", "sku_comercio"))
    o$ejecucion_id <- ctx$ejecucion_id
    # Si la ejecución aún no existe (se registra después), SQLite exige la fila: se crea provisional
    dbExecute(con, "INSERT OR IGNORE INTO ejecuciones (ejecucion_id, comercio, inicio, estado)
                    VALUES (?,?,?, 'parcial')",
              params = list(ctx$ejecucion_id, comercio, min(o$fecha_hora)))
    dbExecute(con, "INSERT OR IGNORE INTO observaciones
      (ejecucion_id, comercio, anuncio_id, fecha_hora, precio, precio_lista, en_promocion,
       disponible, moneda, url) VALUES (?,?,?,?,?,?,?,?,?,?)",
      params = unname(as.list(o[, c("ejecucion_id", "comercio", "anuncio_id", "fecha_hora",
                                    "precio", "precio_lista", "en_promocion", "disponible",
                                    "moneda", "url")])))
  })
}

registrar_ejecucion <- function(con, ctx, comercio, inicio, fin, n, n_prod, total, estado, notas) {
  dbExecute(con, "INSERT INTO ejecuciones
    (ejecucion_id, comercio, inicio, fin, modo, n_registros, n_productos, total_sitio, estado, notas)
    VALUES (?,?,?,?,?,?,?,?,?,?)
    ON CONFLICT(ejecucion_id, comercio) DO UPDATE SET
      fin = excluded.fin, modo = excluded.modo, n_registros = excluded.n_registros,
      n_productos = excluded.n_productos, total_sitio = excluded.total_sitio,
      estado = excluded.estado, notas = excluded.notas",
    params = list(ctx$ejecucion_id, comercio, inicio, fin, ctx$modo, as.integer(n),
                  as.integer(n_prod), as.integer(total), estado, notas))
}

# =============================================================================
# FUNCIÓN GENERAL: ejecuta la recolección completa, la registra y la guarda
# =============================================================================
ultima_ejecucion_raw <- function(comercios) {
  ids <- unlist(lapply(comercios, function(nm)
    list.dirs(file.path(CFG$dir_raw, nm), full.names = FALSE, recursive = FALSE)))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) stop("No hay copias en ", CFG$dir_raw, "/ para reprocesar.")
  max(ids)
}

#  reanudar: (solo modo online) ID de una ejecución previa, o TRUE para la última. Continúa ESA
#            ejecución: reutiliza lo ya guardado en raw/ y consulta solo las páginas que faltan,
#            de modo que todos los comercios quedan en una misma ejecución.
ejecutar_scraping <- function(comercios = names(SCRAPERS),
                              modo = c("online", "offline"),
                              ejecucion_raw = NULL,
                              ruta_db = CFG$ruta_db,
                              reanudar = NULL) {
  modo <- match.arg(modo)
  if (modo == "offline" && is.null(ejecucion_raw)) ejecucion_raw <- ultima_ejecucion_raw(comercios)
  if (!is.null(reanudar)) {
    if (modo != "online") stop("`reanudar` solo aplica en modo online.")
    if (isTRUE(reanudar)) reanudar <- ultima_ejecucion_raw(comercios)
  }
  ejecucion_id <- if (!is.null(reanudar)) reanudar
                  else if (modo == "online") format(Sys.time(), "%Y%m%dT%H%M%S", tz = CFG$zona)
                  else ejecucion_raw
  ctx <- list(modo = modo, ejecucion_id = ejecucion_id, reanudar = !is.null(reanudar))

  con <- dbConnect(SQLite(), ruta_db)
  on.exit(dbDisconnect(con), add = TRUE)
  crear_esquema(con)

  resumen <- list()
  for (nm in comercios) {
    message(sprintf("[%s | %s] %s ...", ejecucion_id, modo, nm))
    inicio <- ahora_iso()
    res <- tryCatch(SCRAPERS[[nm]](ctx),
                    error = function(e) list(datos = NULL, total_sitio = NA_integer_, completo = FALSE,
                                             notas = paste("ERROR:", conditionMessage(e))))
    datos <- res$datos
    n <- if (is.null(datos)) 0L else nrow(datos)
    estado <- if (n == 0) "fallida" else if (isTRUE(res$completo)) "completa" else "parcial"
    insertados <- 0L
    if (n > 0) {
      inicio <- if (modo == "offline") min(datos$fecha_hora) else inicio
      insertados <- guardar_resultados(con, datos, ctx, nm)
    }
    registrar_ejecucion(con, ctx, nm, inicio, ahora_iso(), n,
                        length(unique(datos$producto_id %||% datos$sku_comercio)),
                        res$total_sitio, estado,
                        paste(c(sprintf("insertados=%d", insertados), res$notas), collapse = " | "))
    resumen[[nm]] <- data.frame(ejecucion = ejecucion_id, comercio = nm, registros = n,
                                total_sitio = as.integer(res$total_sitio %||% NA_integer_),
                                estado = estado, insertados = insertados, stringsAsFactors = FALSE)
    message(sprintf("   -> %s: %d registros (sitio: %s), estado %s",
                    nm, n, res$total_sitio %||% NA, estado))
  }
  out <- do.call(rbind, resumen)
  rownames(out) <- NULL
  out
}
