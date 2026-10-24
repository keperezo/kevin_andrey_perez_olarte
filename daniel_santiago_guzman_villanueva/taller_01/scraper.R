# =============================================================================
# scraper.R — Taller 1, Minería de Datos (UNAL)
# Categoría: Salud visual y lentes de contacto
#   Incluye: lentes de lectura, lentes de contacto, estuches y soluciones para
#            limpieza y conservación de lentes.
#   Excluye: medicamentos y lágrimas artificiales.
#
# Comercios y páginas de la categoría:
#   Locatel ............ https://www.locatelcolombia.com/salud-y-medicamentos-salud-visual
#   Farmatodo .......... https://www.farmatodo.com.co/categorias/salud-y-medicamentos/cuidado-de-la-vista
#   Drog. Colsubsidio .. https://www.drogueriascolsubsidio.com/salud-visual
#
# Estrategia general: los tres sitios arman sus listados con JavaScript a partir
# de servicios internos que responden JSON; el HTML descargado con read_html()
# no trae productos. Se consultan directamente esos servicios con httr2.
#
# Locatel (VTEX). La página "Salud Visual" no es una categoría del árbol sino
#   una colección: el estado de la página (window.__STATE__) muestra la consulta
#   "query":"/4147" con la faceta productClusterIds. Se consulta
#     /api/catalog_system/pub/products/search?fq=productClusterIds:4147
#   en páginas de 50. El encabezado "resources" (p. ej. "0-49/237") da el total.
#
# Drogerías Colsubsidio (VTEX). "Salud visual" es la categoría de primer nivel
#   con id 13 (árbol /api/catalog_system/pub/category/tree/3). Se consulta
#     /api/catalog_system/pub/products/search?fq=C:/13/
#   El servicio a veces responde 500 ("The operation was canceled"); por eso
#   se reintenta (ver solicitar()).
#
# Farmatodo (Angular + buscador Algolia). Índice "products-colombia", filtro
#     categorie:"Cuidado de la vista" AND status:"A"
#   La aplicación, el índice y la clave pública de solo búsqueda están en el
#   código JavaScript que carga la propia página.
#
# Las secciones se recolectan COMPLETAS. Lo que no pertenece a la delimitación
# de la planilla (gotas, medicamentos, gafas de sol, parches...) se marca como
# excluido en clasificar_productos(); no se filtra al descargar.
# =============================================================================

paquetes <- c("httr2", "jsonlite", "dplyr", "purrr", "stringr", "tibble",
              "tidyr", "DBI", "RSQLite")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)
invisible(lapply(paquetes, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Configuración general
# -----------------------------------------------------------------------------
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

PAUSA   <- 1.5                       # segundos entre solicitudes
DIR_RAW <- "raw"
DB_PATH <- "comparador_precios.sqlite"
ZONA    <- "America/Bogota"

COMERCIOS <- tibble::tribble(
  ~comercio,     ~nombre,                     ~url_categoria,
  "locatel",     "Locatel",                   "https://www.locatelcolombia.com/salud-y-medicamentos-salud-visual",
  "farmatodo",   "Farmatodo",                 "https://www.farmatodo.com.co/categorias/salud-y-medicamentos/cuidado-de-la-vista",
  "colsubsidio", "Droguerías Colsubsidio",    "https://www.drogueriascolsubsidio.com/salud-visual"
)

SECCIONES_LOCATEL     <- c(salud_visual = "productClusterIds:4147")
SECCIONES_COLSUBSIDIO <- c(salud_visual = "C:/13/")
SECCIONES_FARMATODO   <- c(
  cuidado_vista = 'categorie:"Cuidado de la vista" AND status:"A"'
)
FARMATODO <- list(
  url = "https://vcojeyd2po-dsn.algolia.net/1/indexes/products-colombia/query",
  app = "VCOJEYD2PO",
  key = "eb9544fe7bfe7ec4c1aa5e5bf7740feb",  # clave pública de solo búsqueda que publica el sitio
  web = "https://www.farmatodo.com.co"
)

# Hora de Colombia en ISO 8601, p. ej. 2026-10-10T10:35:00-05:00
ahora_co <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = ZONA)
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", x)
}

# Identificador de corrida: 20261011T190300
nueva_corrida <- function() format(Sys.time(), "%Y%m%dT%H%M%S", tz = ZONA)

# -----------------------------------------------------------------------------
# 2. Tabla vacía con las columnas comunes a todos los comercios
# -----------------------------------------------------------------------------
tabla_productos_vacia <- function() {
  tibble(
    comercio            = character(),
    seccion             = character(),  # sección del sitio de donde salió
    sku                 = character(),  # id interno del comercio
    ean                 = character(),  # código de barras principal
    ean_alt             = character(),  # otros códigos publicados, separados por "|"
    nombre              = character(),  # nombre + presentación, tal como los publica el sitio
    marca               = character(),  # marca tal como la publica el sitio
    subcategoria        = character(),  # categoría/subcategoría del sitio
    contenido           = numeric(),    # normalizado a g, mL o und
    unidad              = character(),
    n_unidades          = integer(),
    precio              = numeric(),    # precio de venta (COP)
    precio_lista        = numeric(),    # precio antes del descuento
    promocion           = integer(),
    disponible          = integer(),
    url                 = character(),
    texto_original      = character(),  # nombre | presentación | descripción, sin limpiar
    fecha_hora_consulta = character()
  )
}

# Contenido y unidad desde el texto, normalizados a g y mL.
# "Frasco x 120 ml" -> 120 mL ; "FRASCO X 105 ML" -> 105 mL ; "X 1Lt" -> 1000 mL
extraer_contenido <- function(texto) {
  m <- str_match(
    str_to_lower(texto),
    "(\\d+(?:[.,]\\d+)?)\\s*(kg|mg|grs?|g|gramos|lbs?|oz|ml|mlt|lts?|l)\\b"
  )
  valor  <- suppressWarnings(as.numeric(str_replace(m[, 2], ",", ".")))
  unidad <- m[, 3]
  tibble(
    contenido = case_when(
      unidad == "kg"                  ~ valor * 1000,
      unidad == "mg"                  ~ valor / 1000,
      unidad %in% c("lb", "lbs")      ~ round(valor * 453.592, 1),
      unidad == "oz"                  ~ round(valor * 28.3495, 1),
      unidad %in% c("l", "lt", "lts") ~ valor * 1000,
      TRUE                            ~ valor
    ),
    unidad = case_when(
      unidad %in% c("kg", "mg", "g", "gr", "grs", "gramos", "lb", "lbs", "oz") ~ "g",
      unidad %in% c("ml", "mlt", "l", "lt", "lts")                           ~ "mL",
      TRUE ~ NA_character_
    )
  )
}

# Número de unidades del empaque: "Caja x 30 und", "CAJA X 6 UN", "x 2und"
extraer_unidades <- function(texto) {
  n <- str_match(str_to_lower(texto),
                 "x\\s*(\\d+)\\s*(?:und|unds|un|uds|unidades|botellas|frascos|sobres|viales|lentes)\\b")[, 2]
  as.integer(ifelse(is.na(n), 1L, n))
}

# Normaliza un EAN: solo dígitos y sin ceros a la izquierda
# (Farmatodo publica "0300653610800" y Colsubsidio "300653610800")
normalizar_ean <- function(x) {
  x <- str_replace_all(as.character(x), "\\D", "")
  x <- str_remove(x, "^0+")
  ifelse(is.na(x) | nchar(x) < 8, NA_character_, x)
}

# -----------------------------------------------------------------------------
# 3. Copias crudas y modo sin conexión (sección 3.3 del enunciado)
#    raw/<comercio>/<corrida>/<pagina>__<fecha-hora>.json.gz
#    raw/<comercio>/<corrida>/totales.json   (total que informa el sitio)
#    La hora de consulta queda en el nombre del archivo: al reprocesar sin
#    conexión se conserva la hora original, no la del reprocesamiento.
# -----------------------------------------------------------------------------
guardar_crudo <- function(texto, comercio, corrida, nombre, fecha_hora) {
  dir <- file.path(DIR_RAW, comercio, corrida)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  marca_tiempo <- gsub(":", "", fecha_hora)          # ':' no es válido en Windows
  archivo <- file.path(dir, paste0(nombre, "__", marca_tiempo, ".json.gz"))
  con <- gzfile(archivo, "w", encoding = "UTF-8")
  writeLines(texto, con)
  close(con)
  invisible(archivo)
}

guardar_totales <- function(totales, comercio, corrida) {
  dir <- file.path(DIR_RAW, comercio, corrida)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_json(totales, file.path(dir, "totales.json"), auto_unbox = TRUE, pretty = TRUE)
}

leer_totales <- function(comercio, corrida) {
  f <- file.path(DIR_RAW, comercio, corrida, "totales.json")
  if (!file.exists(f)) return(NULL)
  as_tibble(fromJSON(f))
}

leer_crudos <- function(comercio, corrida) {
  archivos <- sort(list.files(file.path(DIR_RAW, comercio, corrida),
                              pattern = "\\.json\\.gz$", full.names = TRUE))
  if (length(archivos) == 0) {
    stop("No hay copias crudas en ", file.path(DIR_RAW, comercio, corrida))
  }
  paginas <- map(archivos, function(a) {
    partes <- str_match(basename(a), "^(.+)__(.+)\\.json\\.gz$")
    # 2026-10-10T103500-0500 -> 2026-10-10T10:35:00-05:00
    fh <- sub("T(\\d{2})(\\d{2})(\\d{2})([+-]\\d{2})(\\d{2})$",
              "T\\1:\\2:\\3\\4:\\5", partes[, 3])
    con <- gzfile(a, encoding = "UTF-8")
    on.exit(close(con))
    list(texto = paste(readLines(con, warn = FALSE), collapse = "\n"),
         fecha_hora = fh,
         nombre = partes[, 2],
         seccion = str_remove(partes[, 2], "_p\\d+$"))
  })
  attr(paginas, "totales") <- leer_totales(comercio, corrida)
  paginas
}

# Últimas corridas con copias crudas de un comercio
corridas_disponibles <- function(comercio) {
  sort(basename(list.dirs(file.path(DIR_RAW, comercio), recursive = FALSE)))
}

# -----------------------------------------------------------------------------
# 4. Solicitud HTTP con el patrón del taller (tryCatch + req_error + pausa).
#    Se reintenta ante 429, 500, 502, 503 y 504: Colsubsidio responde 500 de
#    forma intermitente y sin reintento una página completa se perdería.
# -----------------------------------------------------------------------------
solicitar <- function(req, etiqueta) {
  Sys.sleep(PAUSA)
  respuesta <- tryCatch(
    req %>%
      req_user_agent(user_agent_navegador) %>%
      req_headers(`accept-language` = "es-CO,es;q=0.9") %>%
      req_timeout(seconds = 40) %>%
      req_retry(max_tries = 5,
                is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
                backoff = function(i) 2 * i) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform(),
    error = function(e) {
      message(etiqueta, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(respuesta) || resp_status(respuesta) >= 400) {
    message(etiqueta, ": no se obtuvo una respuesta utilizable (",
            if (is.null(respuesta)) "sin respuesta" else resp_status(respuesta), ").")
    return(NULL)
  }
  respuesta
}

# Compara lo obtenido con el total que informa el sitio
verificar <- function(datos, paginas, comercio) {
  totales <- attr(paginas, "totales")
  if (is.null(totales)) return(datos)
  verificacion <- datos %>%
    count(seccion, name = "obtenidos") %>%
    right_join(totales, by = "seccion") %>%
    mutate(obtenidos = coalesce(obtenidos, 0L))
  message("Verificación de completitud (", comercio, "):")
  print(verificacion)
  attr(datos, "verificacion") <- verificacion
  datos
}

# =============================================================================
# 5. Plataforma VTEX (Locatel y Colsubsidio)
# =============================================================================
descargar_vtex <- function(comercio, dominio, secciones, corrida, por_pagina = 50) {
  paginas <- list()
  totales <- tibble(seccion = character(), total_sitio = integer())
  for (nombre_sec in names(secciones)) {
    fq <- secciones[[nombre_sec]]
    desde <- 0
    total <- NA_integer_
    repeat {
      url <- sprintf(
        "https://%s/api/catalog_system/pub/products/search?fq=%s&_from=%d&_to=%d",
        dominio, URLencode(fq, reserved = FALSE), desde, desde + por_pagina - 1
      )
      resp <- solicitar(request(url), paste(comercio, nombre_sec, "desde", desde))
      if (is.null(resp)) break
      texto <- resp_body_string(resp)
      fh <- ahora_co()
      guardar_crudo(texto, comercio, corrida,
                    sprintf("%s_p%04d", nombre_sec, desde), fh)
      paginas[[length(paginas) + 1]] <- list(texto = texto, fecha_hora = fh,
                                             seccion = nombre_sec)
      n <- length(fromJSON(texto, simplifyVector = FALSE))
      total <- as.integer(str_extract(resp_header(resp, "resources") %||% "", "\\d+$"))
      desde <- desde + por_pagina
      # VTEX no entrega más allá del resultado 2500 en una misma consulta
      if (n < por_pagina || (!is.na(total) && desde >= total) || desde >= 2500) break
    }
    totales <- bind_rows(totales, tibble(seccion = nombre_sec, total_sitio = total))
  }
  guardar_totales(totales, comercio, corrida)
  attr(paginas, "totales") <- totales
  paginas
}

# Convierte las páginas crudas VTEX en la tabla común (una fila por SKU).
# campo_presentacion: especificación con el tamaño del empaque. Colsubsidio
# lo publica en "Presentación" ("FRASCO X 120 ML"); en Locatel el tamaño va en
# el nombre y "Presentación" solo dice "FRASCO" o "ESTUCHE", así que no se usa.
parsear_vtex <- function(paginas, comercio, campo_presentacion = NULL) {
  espec <- function(p, campo) {
    if (is.null(campo)) return(NA_character_)
    # se busca por patrón para no depender de la codificación de la tilde
    clave <- names(p)[str_detect(names(p), regex(campo, ignore_case = TRUE))]
    if (length(clave) == 0) return(NA_character_)
    v <- p[[clave[1]]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
  }
  map_dfr(paginas, function(pag) {
    productos <- fromJSON(pag$texto, simplifyVector = FALSE)
    if (length(productos) == 0) return(tabla_productos_vacia())
    map_dfr(productos, function(p) {
      map_dfr(p$items, function(it) {
        vendedor <- keep(it$sellers, ~ isTRUE(.x$sellerDefault))
        vendedor <- if (length(vendedor) > 0) vendedor[[1]] else it$sellers[[1]]
        oferta   <- vendedor$commertialOffer
        presentacion <- espec(p, campo_presentacion)
        base <- if (length(p$items) > 1) it$nameComplete %||% p$productName else p$productName
        nombre <- str_squish(paste(base, ifelse(is.na(presentacion), "", presentacion)))
        cont <- extraer_contenido(nombre)
        precio <- oferta$Price %||% NA_real_
        lista  <- oferta$ListPrice %||% NA_real_
        tibble(
          comercio            = comercio,
          seccion             = pag$seccion,
          sku                 = as.character(it$itemId),
          ean                 = if (!is.null(it$ean) && nzchar(it$ean)) as.character(it$ean) else NA_character_,
          ean_alt             = NA_character_,
          nombre              = nombre,
          marca               = p$brand %||% NA_character_,
          subcategoria        = p$categories[[1]] %||% NA_character_,
          contenido           = cont$contenido,
          unidad              = cont$unidad,
          n_unidades          = extraer_unidades(nombre),
          # precio 0 = sin precio publicado: queda NA, nunca 0 (sección 3.4)
          precio              = if (isTRUE(precio > 0)) precio else NA_real_,
          precio_lista        = if (isTRUE(lista > 0)) lista else NA_real_,
          promocion           = as.integer(isTRUE(lista > precio & precio > 0)),
          disponible          = as.integer(isTRUE(oferta$IsAvailable)),
          url                 = p$link %||% NA_character_,
          texto_original      = paste(p$productName %||% "",
                                      ifelse(is.na(presentacion), "", presentacion),
                                      p$description %||% "", sep = " | "),
          fecha_hora_consulta = pag$fecha_hora
        )
      })
    })
  })
}

scrape_vtex <- function(comercio, dominio, secciones, corrida, offline = FALSE,
                        campo_presentacion = NULL) {
  paginas <- if (offline) {
    leer_crudos(comercio, corrida)
  } else {
    descargar_vtex(comercio, dominio, secciones, corrida)
  }
  datos <- parsear_vtex(paginas, comercio, campo_presentacion) %>%
    distinct(sku, .keep_all = TRUE)       # un SKU en dos secciones: una sola fila
  verificar(datos, paginas, comercio)
}

# =============================================================================
# 6. Funciones por comercio (todas devuelven las mismas columnas)
#    scrape_locatel("20261011T190300")                  # en línea
#    scrape_locatel("20261011T190300", offline = TRUE)  # desde raw/
# =============================================================================
scrape_locatel <- function(corrida, offline = FALSE) {
  scrape_vtex("locatel", "www.locatelcolombia.com", SECCIONES_LOCATEL,
              corrida, offline)
}

scrape_colsubsidio <- function(corrida, offline = FALSE) {
  scrape_vtex("colsubsidio", "www.drogueriascolsubsidio.com", SECCIONES_COLSUBSIDIO,
              corrida, offline, campo_presentacion = "^Presentaci.n$")
}

# -----------------------------------------------------------------------------
# Farmatodo (buscador Algolia que usa la propia página)
#   Hasta 100 productos por página; "nbHits" da el total y "nbPages" las páginas.
# -----------------------------------------------------------------------------
descargar_farmatodo <- function(corrida, secciones = SECCIONES_FARMATODO, por_pagina = 100) {
  paginas <- list()
  totales <- tibble(seccion = character(), total_sitio = integer())
  for (nombre_sec in names(secciones)) {
    pagina <- 0
    total <- NA_integer_
    repeat {
      req <- request(FARMATODO$url) %>%
        req_headers(`X-Algolia-Application-Id` = FARMATODO$app,
                    `X-Algolia-API-Key` = FARMATODO$key) %>%
        req_body_json(list(query = "", filters = secciones[[nombre_sec]],
                           hitsPerPage = por_pagina, page = pagina))
      resp <- solicitar(req, paste("farmatodo", nombre_sec, "página", pagina))
      if (is.null(resp)) break
      texto <- resp_body_string(resp)
      fh <- ahora_co()
      guardar_crudo(texto, "farmatodo", corrida,
                    sprintf("%s_p%04d", nombre_sec, pagina), fh)
      paginas[[length(paginas) + 1]] <- list(texto = texto, fecha_hora = fh,
                                             seccion = nombre_sec)
      js <- fromJSON(texto, simplifyVector = FALSE)
      total <- as.integer(js$nbHits %||% NA)
      pagina <- pagina + 1
      if (pagina >= (js$nbPages %||% 0)) break
    }
    totales <- bind_rows(totales, tibble(seccion = nombre_sec, total_sitio = total))
  }
  guardar_totales(totales, "farmatodo", corrida)
  attr(paginas, "totales") <- totales
  paginas
}

parsear_farmatodo <- function(paginas) {
  map_dfr(paginas, function(pag) {
    hits <- fromJSON(pag$texto, simplifyVector = FALSE)$hits %||% list()
    if (length(hits) == 0) return(tabla_productos_vacia())
    map_dfr(hits, function(h) {
      nombre <- str_squish(h$mediaDescription %||% NA_character_)
      cont   <- extraer_contenido(nombre)
      lleno  <- h$fullPrice  %||% NA_real_
      oferta <- h$offerPrice %||% 0
      if (!is.numeric(oferta) || length(oferta) == 0) oferta <- 0
      sku    <- as.character(h$id %||% h$objectID)
      # barcodeList mezcla el id interno con los códigos de barras
      alt <- unique(as.character(unlist(h$barcodeList %||% list())))
      alt <- setdiff(alt[nchar(alt) >= 8], c(sku, as.character(h$barcode %||% "")))
      tibble(
        comercio            = "farmatodo",
        seccion             = pag$seccion,
        sku                 = sku,
        ean                 = as.character(h$barcode %||% NA_character_),
        ean_alt             = if (length(alt) > 0) paste(alt, collapse = "|") else NA_character_,
        nombre              = nombre,
        marca               = h$marca %||% NA_character_,
        subcategoria        = paste(h$categorie %||% "", h$subCategory %||% "", sep = " > "),
        contenido           = cont$contenido,
        unidad              = cont$unidad,
        n_unidades          = extraer_unidades(nombre),
        # Precio de oferta (offerPrice > 0) o, si no hay, el precio pleno
        precio              = if (isTRUE(oferta > 0)) oferta else if (isTRUE(lleno > 0)) lleno else NA_real_,
        precio_lista        = if (isTRUE(lleno > 0)) lleno else NA_real_,
        promocion           = as.integer(isTRUE(oferta > 0 & oferta < lleno)),
        disponible          = as.integer(length(h$stores_with_stock %||% list()) > 0),
        url                 = if (is.null(h$url)) NA_character_ else
                                paste0(FARMATODO$web, "/producto/", h$url),
        texto_original      = paste(h$mediaDescription %||% "",
                                    h$largeDescription %||% "", sep = " | "),
        fecha_hora_consulta = pag$fecha_hora
      )
    })
  })
}

scrape_farmatodo <- function(corrida, offline = FALSE) {
  paginas <- if (offline) leer_crudos("farmatodo", corrida) else descargar_farmatodo(corrida)
  datos <- parsear_farmatodo(paginas) %>% distinct(sku, .keep_all = TRUE)
  verificar(datos, paginas, "farmatodo")
}

SCRAPERS <- list(
  locatel     = scrape_locatel,
  farmatodo   = scrape_farmatodo,
  colsubsidio = scrape_colsubsidio
)

# =============================================================================
# 7. Limpieza y clasificación según la delimitación de la planilla
# =============================================================================
quitar_tildes <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

# Marca estandarizada. Los sitios no publican la marca de la misma forma:
# Colsubsidio pone el fabricante (ALCON, HUMAX, TECNOQUIMICAS, "N/A") y Locatel
# o Farmatodo la línea (Opti-Free, Renu). Se usa la línea de producto cuando el
# nombre la identifica; si no, la marca publicada en mayúsculas.
LINEAS_MARCA <- c(
  "opti.?free"                   = "OPTI-FREE",
  "renu"                         = "RENU",
  "biotrue"                      = "BIOTRUE",
  "simplus"                      = "SIMPLUS",
  "ao.?sept"                     = "AOSEPT",
  "multi.?solu|multisolu|multi 20" = "MULTI SOLUTION",
  "hyfresh|prolub"               = "PROLUB HYFRESH",
  "multisoluter"                 = "MULTISOLUTER",
  "foster grant"                 = "FOSTER GRANT",
  "magnivision"                  = "MAGNIVISION",
  "acuvue|oasys"                 = "ACUVUE",
  "soflens"                      = "SOFLENS",
  "air optix"                    = "AIR OPTIX",
  "cleanpur"                     = "CLEANPUR"
)

estandarizar_marca <- function(nombre, marca) {
  n <- str_to_lower(quitar_tildes(nombre))
  out <- rep(NA_character_, length(n))
  for (patron in names(LINEAS_MARCA)) {
    sel <- is.na(out) & str_detect(n, patron)
    out[sel] <- LINEAS_MARCA[[patron]]
  }
  m <- str_squish(str_to_upper(quitar_tildes(marca)))
  m[m %in% c("", "N/A", "NA")] <- NA_character_
  coalesce(out, m)
}

# Graduación (dioptrías) de gafas y lentes: "+2.50", "+ 1.00", "-3.00",
# o "Blk 250" / "READERS & CASE 150" (Farmatodo codifica +2.50 como 250)
extraer_graduacion <- function(nombre) {
  n <- str_to_lower(nombre)
  g <- str_match(n, "([+-])\\s*(\\d{1,2}(?:[.,]\\d{1,2})?)\\b(?!\\s*(?:ml|mg|g|%))")
  val <- suppressWarnings(as.numeric(str_replace(g[, 3], ",", ".")))
  val <- ifelse(g[, 2] == "-", -val, val)
  cod <- suppressWarnings(as.numeric(
    str_match(n, "(?:readers & case|lectura.*\\b[a-z]{3})\\s+(100|125|150|175|200|225|250|275|300|325|350|400)\\b")[, 2]
  ))
  ifelse(is.na(val), cod / 100, val)
}

# Clasifica cada anuncio en una subcategoría estandarizada y decide si
# pertenece a la delimitación (en_categoria = 1) o se excluye (motivo).
clasificar_productos <- function(datos) {
  t  <- str_to_lower(quitar_tildes(paste(datos$nombre, datos$subcategoria)))
  nm <- str_to_lower(quitar_tildes(datos$nombre))

  es_sol <- str_detect(t, "solucion para lentes de contacto") |
    str_detect(nm, paste0("opti.?free|renu|biotrue|simplus|ao.?sept|multi.?solu|",
                          "multisolu|multi 20|hyfresh|multisoluter|hydrozyn|removedor prote|",
                          "solucion.*(lente|contacto|multiprop)|multiprop"))
  es_humect <- str_detect(nm, "lubricante|humectante|rewetting") |
    (!is.na(datos$contenido) & datos$unidad == "mL" & datos$contenido < 20)
  es_lectura <- str_detect(t, "lentes de lectura|gafas de lectura|lectura|readers|presbi") |
    (str_detect(nm, "gafas|lentes") & !is.na(extraer_graduacion(datos$nombre)) &
       !str_detect(t, "contacto|gafas de sol|sol "))
  es_contacto <- str_detect(t, "lentes? de contacto") & !es_sol
  es_sol_gaf  <- str_detect(t, "gafas? de sol|lentes de sol|gafas? sol\\b|\\bsol polariz")
  es_estuche  <- str_detect(nm, "estuche|porta ?lentes|porta ?gafas") & !es_sol & !es_lectura &
    !str_detect(nm, "gafas? (de )?sol")
  es_limp_gaf <- str_detect(nm, paste0("limpiador para anteojos|limpia ?gafas|limpiador de (gafas|lentes)|",
                                       "toallitas para lentes|kit de limpieza para gafas"))
  es_azul     <- str_detect(nm, "luz azul") & is.na(extraer_graduacion(datos$nombre))
  es_suplem   <- str_detect(t, "suplemento|capsula|emulsion oral")
  es_parche   <- str_detect(nm, "parche|aposito|cordon")

  datos %>%
    mutate(
      subcategoria_std = case_when(
        es_suplem                   ~ NA_character_,
        es_sol & !es_humect         ~ "Solución para lentes de contacto",
        es_contacto                 ~ "Lentes de contacto",
        es_lectura & !es_sol_gaf    ~ "Lentes de lectura",
        es_estuche                  ~ "Estuches",
        es_limp_gaf                 ~ "Limpieza de gafas",
        TRUE                        ~ NA_character_
      ),
      en_categoria = as.integer(!is.na(subcategoria_std)),
      motivo_exclusion = case_when(
        en_categoria == 1 ~ NA_character_,
        es_suplem         ~ "Suplemento nutricional",
        es_sol & es_humect ~ "Gotas humectantes/lubricantes (lágrima artificial)",
        es_parche         ~ "Parche, apósito o accesorio no incluido",
        es_sol_gaf        ~ "Gafas de sol (fuera de la delimitación)",
        es_azul           ~ "Gafas de luz azul sin graduación",
        TRUE              ~ "Medicamento, lágrima artificial u otro cuidado ocular"
      ),
      marca_std   = estandarizar_marca(nombre, marca),
      graduacion  = if_else(subcategoria_std %in% c("Lentes de lectura", "Lentes de contacto"),
                            extraer_graduacion(nombre), NA_real_),
      # Artículos contables: gafas = 1 und; lentes de contacto = unidades de la caja
      contenido = case_when(
        subcategoria_std == "Lentes de lectura"                      ~ 1,
        subcategoria_std == "Lentes de contacto" & is.na(contenido)  ~ as.numeric(n_unidades),
        TRUE ~ contenido
      ),
      unidad = case_when(
        subcategoria_std %in% c("Lentes de lectura", "Lentes de contacto") ~ "und",
        TRUE ~ unidad
      ),
      nombre_std = str_squish(str_to_lower(quitar_tildes(
        str_remove_all(nombre, regex("^\\W+|\\boferta\\b", ignore_case = TRUE))
      ))),
      ean_norm = normalizar_ean(ean),
      # la descripción es el último segmento de texto_original
      tiene_descripcion = as.integer(nchar(str_squish(str_remove_all(
        str_extract(texto_original, "[^|]*$"), "<[^>]+>|&nbsp;"))) >= 10)
    )
}

# =============================================================================
# 8. Correspondencia de productos entre comercios
#    Regla 1 (automática): mismo EAN normalizado -> misma presentación.
#      En Farmatodo también se usan los códigos alternos (barcodeList).
#    Regla 2 (manual): equivalencias revisadas a mano cuando un comercio usa
#      un código interno distinto. Se exige misma marca, línea, contenido y
#      número de unidades.
#    Un producto es comparable si está en_categoria en los TRES comercios.
# =============================================================================
CORRESPONDENCIA_MANUAL <- tibble::tribble(
  ~comercio,   ~ean,            ~ean_ref,        ~nota,
  # Locatel publica Multi Solution 1 x 360 mL con el código 7703197050089;
  # Farmatodo y Colsubsidio con 7702057707767. Mismo producto, marca y tamaño.
  "locatel",   "7703197050089", "7702057707767", "Multi Solution 1 x 360 mL: Locatel usa otro código"
)

# Códigos de cada anuncio (principal + alternos), uno por fila
codigos_anuncio <- function(anuncios) {
  anuncios %>%
    select(comercio, sku, ean, ean_alt) %>%
    mutate(cod = paste(coalesce(ean, ""), coalesce(ean_alt, ""), sep = "|")) %>%
    select(comercio, sku, cod) %>%
    separate_rows(cod, sep = "\\|") %>%
    mutate(cod = normalizar_ean(cod)) %>%
    filter(!is.na(cod)) %>%
    distinct()
}

# Valor más frecuente (NA si no hay datos)
moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

construir_correspondencia <- function(anuncios) {
  n_comercios <- n_distinct(anuncios$comercio)
  base <- anuncios %>% filter(en_categoria == 1)
  cods <- codigos_anuncio(base)

  # Regla 2: el código del comercio se reemplaza por el de referencia
  manual <- CORRESPONDENCIA_MANUAL %>%
    mutate(ean = normalizar_ean(ean), ean_ref = normalizar_ean(ean_ref))
  cods <- cods %>%
    left_join(manual %>% select(comercio, cod = ean, ean_ref), by = c("comercio", "cod")) %>%
    mutate(regla = if_else(is.na(ean_ref), "EAN idéntico", "Equivalencia manual"),
           cod = coalesce(ean_ref, cod)) %>%
    select(-ean_ref)

  comunes <- cods %>%
    group_by(cod) %>%
    summarise(n_com = n_distinct(comercio), .groups = "drop") %>%
    filter(n_com == n_comercios)

  asignacion <- cods %>%
    semi_join(comunes, by = "cod") %>%
    group_by(comercio, sku) %>% slice(1) %>% ungroup() %>%      # un código por anuncio
    mutate(id_producto = paste0("SV-", cod))

  productos <- asignacion %>%
    left_join(base %>% select(comercio, sku, marca_std, subcategoria_std,
                              contenido, unidad, n_unidades, graduacion),
              by = c("comercio", "sku")) %>%
    group_by(id_producto) %>%
    summarise(
      ean              = first(cod),
      marca_std        = as.character(moda(marca_std)),
      subcategoria_std = first(subcategoria_std),
      contenido        = as.numeric(moda(contenido)),
      unidad           = as.character(moda(unidad)),
      n_unidades       = as.integer(moda(n_unidades)),
      graduacion       = as.numeric(moda(graduacion)),
      regla_correspondencia = if_else(any(regla == "Equivalencia manual"),
                                      "EAN + equivalencia manual", "EAN idéntico"),
      .groups = "drop"
    )
  list(asignacion = asignacion %>% select(comercio, sku, id_producto), productos = productos)
}

# =============================================================================
# 9. Base SQLite
# =============================================================================
crear_esquema <- function(con) {
  dbExecute(con, "PRAGMA foreign_keys = ON")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS comercios (
      comercio      TEXT PRIMARY KEY,
      nombre        TEXT NOT NULL,
      url_categoria TEXT NOT NULL
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ejecuciones (
      id_ejecucion  TEXT NOT NULL,
      comercio      TEXT NOT NULL REFERENCES comercios(comercio),
      inicio        TEXT NOT NULL,          -- ISO 8601, hora de Colombia
      fin           TEXT,
      modo          TEXT NOT NULL,          -- en_linea | sin_conexion
      n_registros   INTEGER NOT NULL,       -- anuncios obtenidos (toda la sección)
      total_sitio   INTEGER,                -- total que informa el propio sitio
      n_en_categoria INTEGER,               -- anuncios dentro de la delimitación
      estado        TEXT NOT NULL CHECK (estado IN ('completa','parcial','fallida')),
      detalle       TEXT,
      PRIMARY KEY (id_ejecucion, comercio)
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS productos (
      id_producto           TEXT PRIMARY KEY,   -- identificador común entre comercios
      ean                   TEXT NOT NULL,
      nombre_std            TEXT,
      linea                 TEXT,             -- mismo producto y formulación, distinto tamaño
      marca_std             TEXT,
      subcategoria_std      TEXT,
      contenido             REAL,
      unidad                TEXT,
      n_unidades            INTEGER,
      graduacion            REAL,
      regla_correspondencia TEXT
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS anuncios (
      comercio          TEXT NOT NULL REFERENCES comercios(comercio),
      sku               TEXT NOT NULL,
      ean               TEXT,
      ean_alt           TEXT,
      nombre_original   TEXT NOT NULL,
      marca_original    TEXT,
      subcategoria_sitio TEXT,
      nombre_std        TEXT,
      marca_std         TEXT,
      subcategoria_std  TEXT,
      en_categoria      INTEGER NOT NULL,
      motivo_exclusion  TEXT,
      contenido         REAL,
      unidad            TEXT,
      n_unidades        INTEGER,
      graduacion        REAL,
      tiene_descripcion INTEGER,
      id_producto       TEXT REFERENCES productos(id_producto),
      comparable        INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (comercio, sku)
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS observaciones (
      id_observacion      INTEGER PRIMARY KEY AUTOINCREMENT,
      id_ejecucion        TEXT NOT NULL,
      comercio            TEXT NOT NULL,
      sku                 TEXT NOT NULL,
      seccion             TEXT,
      precio              REAL,            -- NULL si el sitio no publica precio
      precio_lista        REAL,
      promocion           INTEGER NOT NULL,
      disponible          INTEGER,
      url                 TEXT NOT NULL,
      texto_original      TEXT NOT NULL,
      fecha_hora_consulta TEXT NOT NULL,
      FOREIGN KEY (comercio, sku) REFERENCES anuncios(comercio, sku),
      FOREIGN KEY (id_ejecucion, comercio) REFERENCES ejecuciones(id_ejecucion, comercio),
      UNIQUE (comercio, sku, fecha_hora_consulta)
    )")
  # Vista de trabajo: cada observación con su anuncio, producto común y bandera comparable
  dbExecute(con, "DROP VIEW IF EXISTS v_observaciones")
  dbExecute(con, "
    CREATE VIEW v_observaciones AS
    SELECT o.id_observacion, o.id_ejecucion, o.comercio, o.sku,
           a.id_producto, a.comparable, a.en_categoria, a.motivo_exclusion,
           COALESCE(p.nombre_std, a.nombre_std) AS producto,
           a.nombre_original, a.marca_std, a.subcategoria_std,
           a.contenido, a.unidad, a.n_unidades, a.graduacion,
           o.precio, o.precio_lista, o.promocion, o.disponible,
           CASE WHEN o.precio IS NOT NULL AND a.contenido > 0
                THEN o.precio / a.contenido END AS precio_por_unidad,
           o.url, o.texto_original, o.fecha_hora_consulta
    FROM observaciones o
    JOIN anuncios a ON a.comercio = o.comercio AND a.sku = o.sku
    LEFT JOIN productos p ON p.id_producto = a.id_producto")
  invisible(con)
}

conectar_db <- function(ruta = DB_PATH) {
  con <- dbConnect(RSQLite::SQLite(), ruta)
  crear_esquema(con)
  dbExecute(con, "INSERT OR IGNORE INTO comercios VALUES (:comercio, :nombre, :url_categoria)",
            params = as.list(COMERCIOS))
  con
}

# Inserta o actualiza los anuncios (atributos estandarizados del producto en
# cada comercio). El texto original de cada momento queda en observaciones.
guardar_anuncios <- function(con, datos) {
  a <- datos %>%
    transmute(comercio, sku, ean, ean_alt, nombre_original = nombre,
              marca_original = marca, subcategoria_sitio = subcategoria,
              nombre_std, marca_std, subcategoria_std, en_categoria, motivo_exclusion,
              contenido, unidad, n_unidades, graduacion, tiene_descripcion)
  dbExecute(con, "
    INSERT INTO anuncios (comercio, sku, ean, ean_alt, nombre_original, marca_original,
                          subcategoria_sitio, nombre_std, marca_std, subcategoria_std,
                          en_categoria, motivo_exclusion, contenido, unidad, n_unidades,
                          graduacion, tiene_descripcion)
    VALUES (:comercio, :sku, :ean, :ean_alt, :nombre_original, :marca_original,
            :subcategoria_sitio, :nombre_std, :marca_std, :subcategoria_std,
            :en_categoria, :motivo_exclusion, :contenido, :unidad, :n_unidades,
            :graduacion, :tiene_descripcion)
    ON CONFLICT(comercio, sku) DO UPDATE SET
      ean = excluded.ean, ean_alt = excluded.ean_alt,
      nombre_original = excluded.nombre_original, marca_original = excluded.marca_original,
      subcategoria_sitio = excluded.subcategoria_sitio, nombre_std = excluded.nombre_std,
      marca_std = excluded.marca_std, subcategoria_std = excluded.subcategoria_std,
      en_categoria = excluded.en_categoria, motivo_exclusion = excluded.motivo_exclusion,
      contenido = excluded.contenido, unidad = excluded.unidad,
      n_unidades = excluded.n_unidades, graduacion = excluded.graduacion,
      tiene_descripcion = excluded.tiene_descripcion",
    params = as.list(a))
}

# Nunca sobrescribe: si la misma observación (comercio, sku, fecha-hora) ya
# existe, se ignora. Devuelve cuántas filas nuevas entraron.
guardar_observaciones <- function(con, datos, id_ejecucion) {
  o <- datos %>%
    transmute(id_ejecucion = id_ejecucion, comercio, sku, seccion, precio,
              precio_lista, promocion, disponible,
              url = coalesce(url, ""), texto_original, fecha_hora_consulta)
  dbExecute(con, "
    INSERT OR IGNORE INTO observaciones
      (id_ejecucion, comercio, sku, seccion, precio, precio_lista, promocion,
       disponible, url, texto_original, fecha_hora_consulta)
    VALUES (:id_ejecucion, :comercio, :sku, :seccion, :precio, :precio_lista,
            :promocion, :disponible, :url, :texto_original, :fecha_hora_consulta)",
    params = as.list(o))
}

registrar_ejecucion <- function(con, fila) {
  dbExecute(con, "
    INSERT INTO ejecuciones (id_ejecucion, comercio, inicio, fin, modo, n_registros,
                             total_sitio, n_en_categoria, estado, detalle)
    VALUES (:id_ejecucion, :comercio, :inicio, :fin, :modo, :n_registros,
            :total_sitio, :n_en_categoria, :estado, :detalle)
    ON CONFLICT(id_ejecucion, comercio) DO UPDATE SET
      fin = excluded.fin, modo = excluded.modo, n_registros = excluded.n_registros,
      total_sitio = excluded.total_sitio, n_en_categoria = excluded.n_en_categoria,
      estado = excluded.estado, detalle = excluded.detalle",
    params = as.list(fila))
}

# Línea de producto: agrupa presentaciones del MISMO producto y formulación
# que solo difieren en tamaño, para comparar precio por mL. Las soluciones
# Multi Solution 1, Plus y Oxi son formulaciones distintas y no se mezclan.
# ReNu Plus (No Rub) y ReNu Fresh son la misma solución multipropósito de
# Bausch + Lomb con dos nombres comerciales; los sitios usan ambos para los
# mismos códigos (p. ej. 310119033227 es "Renu Plus" en Locatel y
# "RenuFresh" en Farmatodo).
LINEAS_PRODUCTO <- c(
  "opti.?free.*(pure ?moist)?|desinfectante opti" = "Opti-Free PureMoist",
  "renu"                              = "ReNu multipropósito",
  "multi solution oxi|multisolution oxi|multi solucion oxi" = "Multi Solution Oxi",
  "multi solution plus|multisolution plus" = "Multi Solution Plus",
  "multi solution 1|multi solucion 1" = "Multi Solution 1",
  "simplus"                           = "Simplus",
  "ao.?sept"                          = "AoSept Plus",
  "hyfresh"                           = "Prolub Hyfresh"
)
asignar_linea <- function(nombre, marca) {
  n <- str_to_lower(quitar_tildes(nombre))
  out <- rep(NA_character_, length(n))
  for (patron in names(LINEAS_PRODUCTO)) {
    sel <- is.na(out) & str_detect(n, patron)
    out[sel] <- LINEAS_PRODUCTO[[patron]]
  }
  out                                   # NA si el nombre no identifica una línea conocida
}

# Recalcula productos comunes y marca anuncios comparables
actualizar_correspondencia <- function(con) {
  anuncios <- dbGetQuery(con, "SELECT * FROM anuncios") %>% as_tibble() %>%
    select(-id_producto, -comparable)
  corr <- construir_correspondencia(anuncios)
  if (nrow(corr$productos) == 0) {
    message("Aún no hay productos presentes en los tres comercios.")
    return(invisible(corr))
  }
  nombres <- anuncios %>%
    inner_join(corr$asignacion, by = c("comercio", "sku")) %>%
    group_by(id_producto) %>%
    summarise(nombre_base = nombre_std[which.min(nchar(nombre_std))], .groups = "drop")
  # Nombre común: el nombre más corto, sin tamaño, + contenido normalizado
  prods <- corr$productos %>%
    left_join(nombres, by = "id_producto") %>%
    mutate(
      nombre_base = str_squish(str_remove_all(
        nombre_base,
        "\\b(frasco|fco|x)\\b|\\d+(?:[.,]\\d+)?\\s*(ml|mlt|g)\\b|\\b(solucion|soluciones)\\b"
      )),
      linea_conocida = asignar_linea(nombre_base, marca_std),
      linea = coalesce(linea_conocida, str_to_title(nombre_base)),
      nombre_std = case_when(
        !is.na(linea_conocida)            ~ paste0(linea_conocida, " ", contenido, " ", unidad),
        unidad == "und" & !is.na(unidad)  ~ str_to_title(nombre_base),
        TRUE                              ~ paste0(nombre_base, " ", contenido, " ", unidad)
      )
    )

  dbWithTransaction(con, {
    dbExecute(con, "UPDATE anuncios SET id_producto = NULL, comparable = 0")
    dbExecute(con, "DELETE FROM productos")
    dbExecute(con, "
      INSERT INTO productos (id_producto, ean, nombre_std, linea, marca_std, subcategoria_std,
                             contenido, unidad, n_unidades, graduacion, regla_correspondencia)
      VALUES (:id_producto, :ean, :nombre_std, :linea, :marca_std, :subcategoria_std,
              :contenido, :unidad, :n_unidades, :graduacion, :regla_correspondencia)",
      params = as.list(prods %>% select(id_producto, ean, nombre_std, linea, marca_std,
                                        subcategoria_std, contenido, unidad, n_unidades,
                                        graduacion, regla_correspondencia)))
    dbExecute(con, "
      UPDATE anuncios SET id_producto = :id_producto, comparable = 1
      WHERE comercio = :comercio AND sku = :sku",
      params = as.list(corr$asignacion))
  })
  message(nrow(prods), " productos/presentaciones comunes a los tres comercios.")
  invisible(corr)
}

# =============================================================================
# 10. Función general: recolección completa + registro + carga en SQLite
#     ejecutar_recoleccion()                                  # en línea
#     ejecutar_recoleccion(offline = TRUE, corrida = "20261011T190300")
# =============================================================================
ejecutar_recoleccion <- function(corrida = NULL, offline = FALSE,
                                 comercios = names(SCRAPERS), db = DB_PATH) {
  if (is.null(corrida)) {
    if (offline) {
      # la corrida más reciente con copias crudas de todos los comercios
      comunes <- Reduce(intersect, lapply(comercios, corridas_disponibles))
      if (length(comunes) == 0) stop("No hay una corrida con copias crudas de todos los comercios.")
      corrida <- max(comunes)
    } else {
      corrida <- nueva_corrida()
    }
  }
  message("Ejecución ", corrida, if (offline) " (sin conexión)" else " (en línea)")
  con <- conectar_db(db)
  on.exit(dbDisconnect(con), add = TRUE)

  resumen <- map_dfr(comercios, function(com) {
    inicio <- ahora_co()
    datos <- tryCatch(SCRAPERS[[com]](corrida, offline = offline),
                      error = function(e) { message(com, ": ", conditionMessage(e)); NULL })
    if (is.null(datos) || nrow(datos) == 0) {
      fila <- tibble(id_ejecucion = corrida, comercio = com, inicio = inicio, fin = ahora_co(),
                     modo = if (offline) "sin_conexion" else "en_linea",
                     n_registros = 0L, total_sitio = NA_integer_, n_en_categoria = 0L,
                     estado = "fallida", detalle = "Sin datos")
      registrar_ejecucion(con, fila)
      return(fila)
    }
    verif <- attr(datos, "verificacion")
    total <- if (is.null(verif)) NA_integer_ else as.integer(sum(verif$total_sitio))
    datos <- clasificar_productos(datos)
    fila <- tibble(
      id_ejecucion = corrida, comercio = com, inicio = inicio, fin = ahora_co(),
      modo = if (offline) "sin_conexion" else "en_linea",
      n_registros = nrow(datos), total_sitio = total,
      n_en_categoria = sum(datos$en_categoria),
      estado = if (!is.na(total) && nrow(datos) < total) "parcial" else "completa",
      detalle = if (is.na(total)) "El sitio no informa total" else
        sprintf("%d de %d anuncios que informa el sitio", nrow(datos), total)
    )
    dbWithTransaction(con, {
      registrar_ejecucion(con, fila)
      guardar_anuncios(con, datos)
      nuevas <- guardar_observaciones(con, datos, corrida)
      message(com, ": ", nrow(datos), " anuncios, ", sum(datos$en_categoria),
              " en la categoría, ", nuevas, " observaciones nuevas.")
    })
    fila
  })
  actualizar_correspondencia(con)
  print(resumen %>% select(comercio, n_registros, total_sitio, n_en_categoria, estado))
  invisible(resumen)
}
