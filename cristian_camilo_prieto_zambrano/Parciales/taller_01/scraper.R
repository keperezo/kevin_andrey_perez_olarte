# =============================================================================
# scraper.R — Taller parcial 1, Minería de Datos (UNAL)
# Cristian Camilo Prieto Zambrano
# Categoría: Nutrición clínica
#
# Contenido:
#   1-4.  Configuración y utilidades (tabla común, copias crudas, solicitudes)
#   5-8.  Funciones por comercio: scrape_locatel(), scrape_cruzverde(),
#         scrape_farmatodo()
#   9.    Limpieza y estandarización (sección 3.5 del enunciado)
#   10.   Base SQLite (sección 5)
#   11.   Función general: ejecutar_recoleccion() (sección 3.2)
#
# Uso:
#   source("scraper.R")
#   ejecutar_recoleccion()                                        # en línea
#   ejecutar_recoleccion(offline = TRUE, corrida = "20261010T183000")  # desde raw/
#
# Estrategia general: los sitios arman sus listados con JavaScript a partir de
# servicios internos que responden JSON. Como pasó con Éxito, Carulla y Jumbo
# en el taller, el HTML descargado con read_html() no trae productos; por eso
# se consultan directamente esos servicios con httr2.
#
# Locatel (plataforma VTEX):
#   https://www.locatelcolombia.com/api/catalog_system/pub/products/search
#     ?fq=C:/<id de categoría>/&_from=<desde>&_to=<hasta>
#   Hasta 50 productos por página. El encabezado HTTP "resources"
#   (p. ej. "0-49/383") informa el total del sitio.
#
# Cruz Verde (Salesforce Commerce Cloud, API propia en api.cruzverde.com.co):
#   1) POST customer-service/login       -> abre una sesión de invitado (cookie)
#   2) GET  product-service/products/search?refine[]=cgid=<categoría>
#                                          -> listado y precios, 48 por página;
#                                             el campo "total" da el total del sitio
#   3) GET  product-service/products/product-summary?ids=...
#                                          -> descripción de cada producto
#
# Farmatodo (Angular + buscador Algolia):
#   POST https://vcojeyd2po-dsn.algolia.net/1/indexes/products-colombia/query
#   con el filtro de la sección. La aplicación, el índice y la clave pública de
#   solo búsqueda están en el código JavaScript que carga la propia página.
# =============================================================================

paquetes <- c("httr2", "jsonlite", "dplyr", "purrr", "stringr", "tibble", "readr",
              "DBI", "RSQLite")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)
invisible(lapply(paquetes, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# 1. Configuración general
# -----------------------------------------------------------------------------

# User-agent de navegador, igual que en el taller
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

PAUSA   <- 1.5             # segundos entre solicitudes
DIR_RAW <- "raw"
ZONA    <- "America/Bogota"

# Secciones de Locatel consideradas equivalentes a la categoría de la planilla.
# Se recolectan COMPLETAS; lo que no pertenezca a la delimitación se marca
# como excluido en la limpieza, no se filtra aquí.
SECCIONES_LOCATEL <- c(
  suplementos = "C:/6/41/",     # Alimentos nutricionales > Suplementos (383 en el sitio)
  cereales_regulares = "C:/6/40/251/"  # Locatel clasifica aquí Ensure Advance líquido (11)
)

# Cruz Verde: Bienestar y Nutrición > Nutrición (243 en el sitio)
SECCIONES_CRUZVERDE <- c(nutricion = "bienynut-nutricion")
CRUZVERDE <- list(
  api  = "https://api.cruzverde.com.co",
  web  = "https://www.cruzverde.com.co",
  zona = "COCV_zona64"   # zona de inventario que el sitio asigna por defecto
)

# Farmatodo: Bienestar y nutrición deportiva > Nutrición (166 activos).
# Filtro equivalente a la página /categorias/bienestar-y-nutricion-deportiva/nutricion
# (verificado: todos los productos que muestra la página están en este filtro).
SECCIONES_FARMATODO <- c(
  nutricion = paste0('departments:"Bienestar y nutrición deportiva" AND ',
                     'categorie:"Nutrición" AND status:"A"')
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

# -----------------------------------------------------------------------------
# 2. Tabla vacía con las columnas comunes a todos los comercios
# -----------------------------------------------------------------------------
tabla_productos_vacia <- function() {
  tibble(
    comercio            = character(),
    seccion             = character(),  # sección del sitio de donde salió
    sku                 = character(),  # id interno del comercio
    ean                 = character(),  # código de barras
    nombre              = character(),
    marca               = character(),
    subcategoria        = character(),
    contenido           = numeric(),    # normalizado a g o mL
    unidad              = character(),
    n_unidades          = integer(),
    precio              = numeric(),    # precio de venta (COP)
    precio_lista        = numeric(),    # precio antes del descuento
    precio_club         = numeric(),    # solo Cruz Verde: precio para afiliados (no se compara)
    promocion           = integer(),
    disponible          = integer(),
    url                 = character(),
    texto_original      = character(),  # nombre | descripción, sin limpiar
    fecha_hora_consulta = character()
  )
}

# Contenido y unidad desde el nombre, normalizados a g y mL.
# "Tarro X 400Gr" -> 400 g ; "X 237 ml" -> 237 mL ; "X 2lb" -> 907.2 g
extraer_contenido <- function(texto) {
  m <- str_match(
    str_to_lower(texto),
    "(\\d+(?:[.,]\\d+)?)\\s*(kg|mg|gramos|gramo|grs?|g|lbs?|oz|mililitros|ml|lts?|l)\\b"
  )
  valor  <- suppressWarnings(as.numeric(str_replace(m[, 2], ",", ".")))
  unidad <- recode(m[, 3], gramos = "g", gramo = "g", mililitros = "ml")
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
      unidad %in% c("kg", "mg", "g", "gr", "grs", "lb", "lbs", "oz") ~ "g",
      unidad %in% c("ml", "l", "lt", "lts")                          ~ "mL",
      TRUE ~ NA_character_
    )
  )
}

# Número de unidades del empaque: "X 6Und", "X 8 Botellas"
extraer_unidades <- function(texto) {
  n <- str_match(str_to_lower(texto),
                 "(?:x|por)\\s*(\\d+)\\s*(?:und|unds|uni|unidades|botellas|frascos|sobres|tetrapack)")[, 2]
  as.integer(ifelse(is.na(n), 1L, n))
}

# -----------------------------------------------------------------------------
# 3. Copias crudas y modo sin conexión (sección 3.3 del enunciado)
#    raw/<comercio>/<corrida>/<pagina>__<fecha-hora>.json.gz
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

leer_crudos <- function(comercio, corrida) {
  archivos <- sort(list.files(file.path(DIR_RAW, comercio, corrida),
                              pattern = "\\.json\\.gz$", full.names = TRUE))
  if (length(archivos) == 0) {
    stop("No hay copias crudas en ", file.path(DIR_RAW, comercio, corrida))
  }
  map(archivos, function(a) {
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
}

# El total que informa el sitio se guarda junto a las copias crudas
# (raw/<comercio>/<corrida>/totales.csv). Así el modo sin conexión puede
# repetir la verificación de completitud: en VTEX ese total solo viene en
# un encabezado HTTP, que no queda dentro del JSON.
guardar_totales <- function(totales, comercio, corrida) {
  dir <- file.path(DIR_RAW, comercio, corrida)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(totales, file.path(dir, "totales.csv"))
}

leer_totales <- function(comercio, corrida) {
  archivo <- file.path(DIR_RAW, comercio, corrida, "totales.csv")
  if (!file.exists(archivo)) return(NULL)
  read_csv(archivo, col_types = "ci")
}

# Compara lo obtenido con lo que informa el sitio, por sección
verificar_completitud <- function(datos, totales, comercio) {
  if (is.null(totales) || nrow(totales) == 0) {
    message("Verificación de completitud (", comercio, "): sin total del sitio.")
    return(datos)
  }
  verificacion <- datos %>%
    count(seccion, name = "obtenidos") %>%
    right_join(totales, by = "seccion")
  message("Verificación de completitud (", comercio, "):")
  print(verificacion)
  attr(datos, "verificacion") <- verificacion
  datos
}

# -----------------------------------------------------------------------------
# 4. Solicitud HTTP 
# -----------------------------------------------------------------------------
solicitar <- function(req, etiqueta) {
  Sys.sleep(PAUSA)
  respuesta <- tryCatch(
    req %>%
      req_user_agent(user_agent_navegador) %>%
      req_headers(`accept-language` = "es-CO,es;q=0.9") %>%
      req_timeout(seconds = 30) %>%
      req_retry(max_tries = 3) %>%
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

# =============================================================================
# 5. Plataforma VTEX (Locatel)
# =============================================================================

# Recorre todas las páginas de cada sección y guarda la copia cruda.
# Devuelve la lista de páginas y una tabla con el total que informa el sitio.
descargar_vtex <- function(comercio, dominio, secciones, corrida) {
  paginas <- list()
  totales <- tibble(seccion = character(), total_sitio = integer())
  for (nombre_sec in names(secciones)) {
    fq <- secciones[[nombre_sec]]
    desde <- 0
    total <- NA_integer_
    repeat {
      url <- sprintf(
        "https://%s/api/catalog_system/pub/products/search?fq=%s&_from=%d&_to=%d",
        dominio, fq, desde, desde + 49
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
      total <- as.integer(str_extract(resp_header(resp, "resources"), "\\d+$"))
      desde <- desde + 50
      # VTEX no entrega más allá del resultado 2500 en una misma consulta
      if (n < 50 || (!is.na(total) && desde >= total) || desde >= 2500) break
    }
    totales <- bind_rows(totales, tibble(seccion = nombre_sec, total_sitio = total))
  }
  attr(paginas, "totales") <- totales
  paginas
}

# Convierte las páginas crudas VTEX en la tabla común (una fila por SKU).
parsear_vtex <- function(paginas, comercio) {
  espec <- function(p, campo) {      # las especificaciones vienen como listas
    v <- p[[campo]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
  }
  map_dfr(paginas, function(pag) {
    productos <- fromJSON(pag$texto, simplifyVector = FALSE)
    if (length(productos) == 0) return(tabla_productos_vacia())
    map_dfr(productos, function(p) {
      # Un producto puede tener varios SKU (variantes): una fila por SKU
      map_dfr(p$items, function(it) {
        vendedor <- keep(it$sellers, ~ isTRUE(.x$sellerDefault))
        vendedor <- if (length(vendedor) > 0) vendedor[[1]] else it$sellers[[1]]
        oferta   <- vendedor$commertialOffer
        # La Rebaja pone el tamaño en "Presentacionunidadmedida"; Locatel en el nombre
        presentacion <- espec(p, "Presentacionunidadmedida")
        # Con un solo SKU se usa el nombre del producto (La Rebaja agrega
        # "SKU12345" a nameComplete); con variantes, el nombre de cada SKU
        base <- if (length(p$items) > 1) it$nameComplete %||% p$productName else p$productName
        nombre <- str_squish(paste(base, ifelse(is.na(presentacion), "", presentacion)))
        ean <- if (!is.null(it$ean) && nzchar(it$ean)) it$ean else espec(p, "ML_EAN")
        cont <- extraer_contenido(nombre)
        precio <- oferta$Price
        lista  <- oferta$ListPrice
        tibble(
          comercio            = comercio,
          seccion             = pag$seccion,
          sku                 = as.character(it$itemId),
          ean                 = as.character(ean),
          nombre              = nombre,
          marca               = p$brand %||% NA_character_,
          subcategoria        = p$categories[[1]] %||% NA_character_,
          contenido           = cont$contenido,
          unidad              = cont$unidad,
          n_unidades          = extraer_unidades(nombre),
          # precio 0 = sin precio publicado: queda NA, nunca 0 (sección 3.4)
          precio              = if (isTRUE(precio > 0)) precio else NA_real_,
          precio_lista        = if (isTRUE(lista > 0)) lista else NA_real_,
          precio_club         = NA_real_,
          promocion           = as.integer(isTRUE(lista > precio & precio > 0)),
          disponible          = as.integer(isTRUE(oferta$IsAvailable)),
          url                 = p$link %||% NA_character_,
          texto_original      = paste(p$productName, p$description %||% "", sep = " | "),
          fecha_hora_consulta = pag$fecha_hora
        )
      })
    })
  })
}

# Recolección completa de un comercio VTEX (en línea o desde raw/)
scrape_vtex <- function(comercio, dominio, secciones, corrida, offline = FALSE) {
  if (offline) {
    paginas <- leer_crudos(comercio, corrida)
    totales <- leer_totales(comercio, corrida)
  } else {
    paginas <- descargar_vtex(comercio, dominio, secciones, corrida)
    totales <- attr(paginas, "totales")
    guardar_totales(totales, comercio, corrida)
  }
  datos <- parsear_vtex(paginas, comercio)

  # Un mismo SKU puede aparecer en dos secciones: se conserva una sola fila
  datos <- datos %>% distinct(sku, .keep_all = TRUE)

  # Verificación de completitud: lo obtenido frente a lo que informa el sitio
  verificar_completitud(datos, totales, comercio)
}

# =============================================================================
# 6. Funciones por comercio (todas devuelven las mismas columnas)
#    scrape_locatel("20261010T163000")                  # en línea
#    scrape_locatel("20261010T163000", offline = TRUE)  # desde raw/
# =============================================================================
scrape_locatel <- function(corrida, offline = FALSE) {
  scrape_vtex("locatel", "www.locatelcolombia.com", SECCIONES_LOCATEL,
              corrida, offline)
}

# =============================================================================
# 7. Cruz Verde
# =============================================================================

# Solicitud a la API de Cruz Verde, conservando la cookie de sesión
req_cruzverde <- function(url, cookies) {
  request(url) %>%
    req_cookie_preserve(cookies) %>%
    req_headers(Origin = CRUZVERDE$web, Referer = paste0(CRUZVERDE$web, "/"))
}

descargar_cruzverde <- function(corrida, secciones = SECCIONES_CRUZVERDE, por_pagina = 48) {
  cookies <- tempfile(fileext = ".txt")       # cookie de sesión, solo para esta corrida
  on.exit(unlink(cookies), add = TRUE)

  # 1) Sesión de invitado (sin ella el servicio responde 401)
  login <- solicitar(
    req_cruzverde(paste0(CRUZVERDE$api, "/customer-service/login"), cookies) %>%
      req_method("POST") %>%
      req_body_raw("{}", type = "application/json"),
    "cruzverde login"
  )
  if (is.null(login)) stop("Cruz Verde: no se pudo abrir la sesión de invitado.")

  paginas <- list()
  totales <- tibble(seccion = character(), total_sitio = integer())
  for (nombre_sec in names(secciones)) {
    cgid <- secciones[[nombre_sec]]
    desde <- 0
    total <- NA_integer_
    repeat {
      # 2) Listado con precios
      url <- sprintf(
        "%s/product-service/products/search?limit=%d&offset=%d&sort=&q=&refine[]=cgid=%s&inventoryId=%s&inventoryZone=%s",
        CRUZVERDE$api, por_pagina, desde, cgid, CRUZVERDE$zona, CRUZVERDE$zona
      )
      resp <- solicitar(req_cruzverde(url, cookies),
                        paste("cruzverde", nombre_sec, "desde", desde))
      if (is.null(resp)) break
      texto_busqueda <- resp_body_string(resp)
      fh <- ahora_co()
      js <- fromJSON(texto_busqueda, simplifyVector = FALSE)
      total <- as.integer(js$total %||% NA)
      hits <- js$hits %||% list()

      # 3) Descripción de los productos de esta página
      texto_resumen <- "{}"
      if (length(hits) > 0) {
        ids <- map_chr(hits, "productId")
        url_res <- paste0(
          CRUZVERDE$api, "/product-service/products/product-summary?",
          paste0("ids=", ids, collapse = "&"),
          "&fields=name&fields=description&fields=brand&fields=pum&fields=pageURL",
          "&inventoryId=", CRUZVERDE$zona
        )
        resp_res <- solicitar(req_cruzverde(url_res, cookies),
                              paste("cruzverde resumen", nombre_sec, "desde", desde))
        if (!is.null(resp_res)) texto_resumen <- resp_body_string(resp_res)
      }

      base <- sprintf("%s_p%04d", nombre_sec, desde)
      guardar_crudo(texto_busqueda, "cruzverde", corrida, paste0(base, "_busqueda"), fh)
      guardar_crudo(texto_resumen,  "cruzverde", corrida, paste0(base, "_resumen"),  fh)
      paginas[[length(paginas) + 1]] <- list(busqueda = texto_busqueda,
                                             resumen = texto_resumen,
                                             fecha_hora = fh, seccion = nombre_sec)
      desde <- desde + por_pagina
      if (length(hits) < por_pagina || (!is.na(total) && desde >= total)) break
    }
    totales <- bind_rows(totales, tibble(seccion = nombre_sec, total_sitio = total))
  }
  attr(paginas, "totales") <- totales
  paginas
}

# Reconstruye las páginas (búsqueda + resumen) desde raw/
leer_crudos_cruzverde <- function(corrida) {
  crudos <- leer_crudos("cruzverde", corrida)
  nombres <- map_chr(crudos, "nombre")
  bases <- unique(str_remove(nombres, "_(busqueda|resumen)$"))
  map(bases, function(b) {
    bus <- crudos[[which(nombres == paste0(b, "_busqueda"))]]
    i_res <- which(nombres == paste0(b, "_resumen"))
    list(busqueda = bus$texto,
         resumen = if (length(i_res) == 1) crudos[[i_res]]$texto else "{}",
         fecha_hora = bus$fecha_hora,
         seccion = str_remove(b, "_p\\d+$"))
  })
}

parsear_cruzverde <- function(paginas) {
  map_dfr(paginas, function(pag) {
    hits <- fromJSON(pag$busqueda, simplifyVector = FALSE)$hits %||% list()
    if (length(hits) == 0) return(tabla_productos_vacia())
    resumen <- fromJSON(pag$resumen, simplifyVector = FALSE)   # lista con nombre = id
    map_dfr(hits, function(h) {
      id     <- h$productId
      det    <- resumen[[id]] %||% list()
      nombre <- str_squish(h$productName %||% det$name %||% NA_character_)
      cont   <- extraer_contenido(nombre)
      pr     <- h$prices %||% list()
      lista  <- pr[["price-list-col"]] %||% NA_real_
      oferta <- pr[["price-sale-col"]] %||% NA_real_
      club   <- pr[["price-club-col"]] %||% NA_real_
      # Precio comparable: oferta abierta a todos si existe; si no, el de lista.
      # El precio Club exige afiliación y se guarda aparte.
      precio <- if (isTRUE(oferta > 0)) oferta else if (isTRUE(lista > 0)) lista else NA_real_
      slug   <- h$pageURL %||% det$pageURL
      tibble(
        comercio            = "cruzverde",
        seccion             = pag$seccion,
        sku                 = as.character(id),
        ean                 = NA_character_,          # la API no lo publica
        nombre              = nombre,
        marca               = h$brand %||% det$brand %||% NA_character_,
        subcategoria        = SECCIONES_CRUZVERDE[[pag$seccion]] %||% pag$seccion,
        contenido           = cont$contenido,
        unidad              = cont$unidad,
        n_unidades          = extraer_unidades(nombre),
        precio              = precio,
        precio_lista        = if (isTRUE(lista > 0)) lista else NA_real_,
        precio_club         = if (isTRUE(club > 0)) club else NA_real_,
        promocion           = as.integer(isTRUE(oferta > 0 & oferta < lista)),
        disponible          = as.integer(isTRUE((h$stock %||% 0) > 0)),
        url                 = if (is.null(slug)) NA_character_ else
                                sprintf("%s/%s/%s.html", CRUZVERDE$web, slug, id),
        texto_original      = paste(h$productName %||% "", det$description %||% "",
                                    h$pum %||% "", sep = " | "),
        fecha_hora_consulta = pag$fecha_hora
      )
    })
  })
}

scrape_cruzverde <- function(corrida, offline = FALSE) {
  if (offline) {
    paginas <- leer_crudos_cruzverde(corrida)
    # Si la corrida no tiene totales.csv, el total está en el JSON ("total")
    totales <- leer_totales("cruzverde", corrida) %||% total_desde_json(
      paginas, function(p) fromJSON(p$busqueda, simplifyVector = FALSE)$total)
  } else {
    paginas <- descargar_cruzverde(corrida)
    totales <- attr(paginas, "totales")
    guardar_totales(totales, "cruzverde", corrida)
  }
  datos <- parsear_cruzverde(paginas) %>% distinct(sku, .keep_all = TRUE)
  verificar_completitud(datos, totales, "cruzverde")
}

# Total del sitio leído de las copias crudas (Cruz Verde y Farmatodo lo
# incluyen en el cuerpo de la respuesta; se toma el de la primera página)
total_desde_json <- function(paginas, extraer) {
  map_dfr(paginas, function(p) {
    tibble(seccion = p$seccion, total_sitio = as.integer(extraer(p) %||% NA))
  }) %>%
    distinct(seccion, .keep_all = TRUE)
}

# =============================================================================
# 8. Farmatodo (buscador Algolia que usa la propia página)
#    POST al índice "products-colombia" con el filtro de la sección.
#    Hasta 100 productos por página; "nbHits" da el total y "nbPages" las páginas.
# =============================================================================
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
      tibble(
        comercio            = "farmatodo",
        seccion             = pag$seccion,
        sku                 = as.character(h$id %||% h$objectID),
        ean                 = as.character(h$barcode %||% NA_character_),
        nombre              = nombre,
        marca               = h$marca %||% NA_character_,
        subcategoria        = paste(h$categorie %||% "", h$subCategory %||% "", sep = " > "),
        contenido           = cont$contenido,
        unidad              = cont$unidad,
        n_unidades          = extraer_unidades(nombre),
        # Precio base (fullPrice) y precio de oferta (offerPrice > 0)
        precio              = if (isTRUE(oferta > 0)) oferta else if (isTRUE(lleno > 0)) lleno else NA_real_,
        precio_lista        = if (isTRUE(lleno > 0)) lleno else NA_real_,
        precio_club         = NA_real_,
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
  if (offline) {
    paginas <- leer_crudos("farmatodo", corrida)
    # Si la corrida no tiene totales.csv, el total está en el JSON ("nbHits")
    totales <- leer_totales("farmatodo", corrida) %||% total_desde_json(
      paginas, function(p) fromJSON(p$texto, simplifyVector = FALSE)$nbHits)
  } else {
    paginas <- descargar_farmatodo(corrida)
    totales <- attr(paginas, "totales")
    guardar_totales(totales, "farmatodo", corrida)
  }
  datos <- parsear_farmatodo(paginas) %>% distinct(sku, .keep_all = TRUE)
  verificar_completitud(datos, totales, "farmatodo")
}

# Versiones seguras: si un comercio falla, devuelven la tabla vacía y
# no detienen la corrida de los demás
scrape_locatel_seguro   <- possibly(scrape_locatel,   otherwise = tabla_productos_vacia())
scrape_cruzverde_seguro <- possibly(scrape_cruzverde, otherwise = tabla_productos_vacia())
scrape_farmatodo_seguro <- possibly(scrape_farmatodo, otherwise = tabla_productos_vacia())

# =============================================================================
# 9. Limpieza y estandarización (sección 3.5 del enunciado)
#    Las versiones estandarizadas van en columnas propias; nombre, marca,
#    subcategoría y texto_original quedan tal como los publicó cada sitio.
# =============================================================================

# Marcas conocidas de la categoría, buscadas en el nombre del producto.
# Se busca en el nombre porque el campo "marca" de cada sitio no es uniforme:
# el mismo Glucerna llega como "Glucerna", "GLUCERNA" o "Abbott".
MARCAS_STD <- c(
  "ensure" = "Ensure", "glucerna" = "Glucerna", "pediasure" = "PediaSure",
  "prowhey" = "ProWhey", "proklein" = "Proklein", "diben" = "Diben",
  "nutren" = "Nutren", "fresubin" = "Fresubin", "fortisip" = "Fortisip",
  "ascenda" = "Ascenda", "enterex" = "Enterex", "boost" = "Boost",
  "kalori" = "ProWhey", "nutrione" = "Nutrione", "supportan" = "Supportan",
  "glutament" = "Glutament", "ketocal" = "Ketocal", "monogen" = "Monogen",
  "prosure" = "Prosure", "pulmocare" = "Pulmocare", "nepro" = "Nepro",
  "casilan" = "Nutricia Casilan", "prosource" = "Prosource", "frebin" = "Frebini",
  "fortini" = "Fortini", "anamix" = "Anamix", "ensoy" = "Ensoy",
  "gerifood" = "Gerifood", "aukena" = "Aukena", "espezante" = "ProWhey"
)

# Lista blanca de la categoría "Nutrición clínica" (delimitación de la planilla):
# fórmulas nutricionales completas o especializadas, suplementos para soporte
# nutricional, módulos de proteína y espesantes, incluidas fórmulas pediátricas
# clínicas. Se construyó revisando los nombres de las secciones extraídas de
# los tres comercios. Todo lo que no coincide se marca como excluido.
NUTRICION_CLINICA <- paste(
  "ensure|glucerna|pediasure|prowhey|proklein|diben|nutren|fresubin|frebin",
  "fortisip|fortini|ascenda|enterex|glytrol|supportan|glutament|ketocal|monogen",
  "nutrione|boost|kalori|prosure|oncare|pulmocare|nepro|casilan|prosource|anamix",
  "ensoy|gerifood|aukena|espezante|espesante|thicken",
  "f[oó]rmula nutricional|f[oó]rmula especializada|nutrici[oó]n especializada",
  "alimento l[ií]quido|complemento nutricional",
  sep = "|"
)

estandarizar_marca <- function(nombre, marca) {
  n <- str_to_lower(nombre)
  encontrada <- map_chr(n, function(x) {
    i <- which(str_detect(x, fixed(names(MARCAS_STD))))
    if (length(i) > 0) MARCAS_STD[[i[1]]] else NA_character_
  })
  # Si el nombre no trae una marca conocida, se usa el campo del sitio en formato título
  coalesce(encontrada, str_to_title(str_squish(marca)))
}

# Subcategoría común a los tres comercios, por reglas sobre el nombre.
# El orden importa: la primera regla que se cumple es la que se asigna.
estandarizar_subcategoria <- function(nombre) {
  n <- str_to_lower(nombre)
  case_when(
    str_detect(n, "espesante|thicken|spezante")                          ~ "Espesante / módulo de carbohidrato",
    str_detect(n, "pediasure|kids|niñ|infantil|fortini|frebin|anamix|junior") ~ "Fórmula pediátrica",
    str_detect(n, "glucerna|diben|glytrol|diabet|prowhey dm|\\bdm\\b")   ~ "Fórmula para diabetes",
    str_detect(n, "renal|nepro")                                         ~ "Fórmula renal",
    str_detect(n, "pulmocare|prosure|oncare|neumo|f[oó]rmula especializada|nutrici[oó]n especializada") ~
      "Fórmula especializada (otra patología)",
    # Las fórmulas completas van antes que los módulos: "Ensure ... con Proteína"
    # sigue siendo una fórmula completa, no un módulo de proteína
    str_detect(n, "ensure|nutren|fresubin|fortisip|ascenda|enterex|boost|nutrione|supportan|gerifood|aukena") ~
      "Fórmula completa adulto",
    str_detect(n, "prowhey|proklein|casilan|prosource|prote[ií]na|protein|whey") ~ "Módulo de proteína",
    TRUE ~ "Otro (revisar)"
  )
}

# Exclusiones según la delimitación de la planilla. Se marcan; no se borran.
# Regla: lo que está en la lista blanca NUTRICION_CLINICA pertenece a la
# categoría; lo demás se excluye y el motivo indica a qué tipo de producto
# corresponde. Devuelve NA si el producto pertenece a la categoría.
motivo_exclusion <- function(nombre, seccion) {
  n <- str_to_lower(nombre)
  case_when(
    str_detect(n, "jeringa|sonda|equipo de alimentaci") ~
      "Dispositivo médico (no es un alimento)",
    str_detect(n, NUTRICION_CLINICA) ~ NA_character_,
    str_detect(n, "creatin|crea stack|gainer|mass\\b|burner|termog|pre.?workout|pre.?entreno|bcaa|amino|isolate|aislada|whey|bipro|beef|carnitin|hidratante|energ|waffles|recuperante|sports") ~
      "Nutrición deportiva (otra categoría de la planilla)",
    str_detect(n, "vitamin|multivit|centrum|magnes|calci|col[aá]geno|collagen|omega|biotin|zinc|hierro|selenio|gomas|gomitas|c[aá]psul|cap\\b|tableta|comprimido|grageas|jarabe|emulsi[oó]n scott|suplemento dietario|resveratrol|melena de le|espirulina|maca|inositol|probi[oó]tic|prebi[oó]tic|fibra|cranberry|ar[aá]ndano") ~
      "Vitaminas o suplemento dietario (otra categoría de la planilla)",
    str_detect(n, "lubricante|vaginal|[oó]vulo|crema|shampoo|gel\\b|aciclovir|postday|postinor|evinet|seguret|anticoncep|buscapina|spray|loci[oó]n") ~
      "Medicamento o producto de cuidado personal (otra categoría de la planilla)",
    str_detect(n, "similac|enfagrow|leche|infantil.*f[oó]rmula") ~
      "Leche o fórmula infantil de consumo general (otra categoría de la planilla)",
    seccion == "cereales_regulares" ~
      "Cereal o alimento regular (sección de Locatel incluida solo por Ensure líquido)",
    TRUE ~ "Alimento, bebida o suplemento que no es nutrición clínica (fuera de la delimitación)"
  )
}

# Paquetes y ofertas compuestas: no son la presentación individual
es_paquete <- function(nombre, n_unidades) {
  n <- str_to_lower(nombre)
  # "+" solo cuenta si suma otro producto ("850G + 400G", "+ 1 Tarro"), no "+ HMB";
  # "pack" solo como palabra, no dentro de "Tetrapack"
  as.integer(n_unidades > 1 |
             str_detect(n, "\\+\\s*(\\d|botella|lata|tarro|frasco|gratis)") |
             str_detect(n, "oferta|promo|pague|lleve|gratis|\\boft\\b|\\bpaca\\b|\\bpack\\b|\\bkit\\b|combo"))
}

limpiar <- function(datos) {
  datos %>%
    mutate(
      marca_std        = estandarizar_marca(nombre, marca),
      subcategoria_std = estandarizar_subcategoria(nombre),
      es_paquete       = es_paquete(nombre, n_unidades),
      motivo_exclusion = motivo_exclusion(nombre, seccion),
      excluido         = as.integer(!is.na(motivo_exclusion))
    )
}

# =============================================================================
# 10. Base SQLite 
#
#   comercios      las tiendas
#   ejecuciones    cada corrida por comercio: inicio, fin, registros, estado
#   anuncios       cada producto tal como aparece en una tienda (texto original
#                  y versiones estandarizadas); UNIQUE(comercio, sku)
#   productos      identificador común de los comparables (P01, P02, …)
#   observaciones  cada precio observado; UNIQUE(anuncio, fecha y hora)
# =============================================================================
crear_base <- function(db = "comparador_precios.sqlite") {
  con <- dbConnect(SQLite(), db)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  sentencias <- c(
    "CREATE TABLE IF NOT EXISTS comercios (
       id_comercio TEXT PRIMARY KEY,
       nombre      TEXT NOT NULL,
       url_base    TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS productos (
       id_producto      TEXT PRIMARY KEY,
       nombre_std       TEXT NOT NULL,
       marca_std        TEXT NOT NULL,
       subcategoria_std TEXT,
       variante         TEXT,
       contenido        REAL NOT NULL,
       unidad           TEXT NOT NULL,
       n_unidades       INTEGER NOT NULL)",
    "CREATE TABLE IF NOT EXISTS ejecuciones (
       id_ejecucion INTEGER PRIMARY KEY AUTOINCREMENT,
       corrida      TEXT NOT NULL,
       id_comercio  TEXT NOT NULL REFERENCES comercios(id_comercio),
       inicio       TEXT NOT NULL,
       fin          TEXT,
       n_registros  INTEGER NOT NULL,
       total_sitio  INTEGER,
       estado       TEXT NOT NULL CHECK (estado IN ('completa','parcial','fallida')),
       modo         TEXT NOT NULL CHECK (modo IN ('en_linea','sin_conexion')))",
    "CREATE TABLE IF NOT EXISTS anuncios (
       id_anuncio       INTEGER PRIMARY KEY AUTOINCREMENT,
       id_comercio      TEXT NOT NULL REFERENCES comercios(id_comercio),
       sku              TEXT NOT NULL,
       seccion          TEXT,
       ean              TEXT,
       nombre           TEXT,
       marca            TEXT,
       subcategoria     TEXT,
       texto_original   TEXT,
       url              TEXT,
       contenido        REAL,
       unidad           TEXT,
       n_unidades       INTEGER,
       marca_std        TEXT,
       subcategoria_std TEXT,
       es_paquete       INTEGER NOT NULL DEFAULT 0,
       excluido         INTEGER NOT NULL DEFAULT 0,
       motivo_exclusion TEXT,
       id_producto      TEXT REFERENCES productos(id_producto),
       comparable       INTEGER NOT NULL DEFAULT 0,
       UNIQUE (id_comercio, sku))",
    "CREATE TABLE IF NOT EXISTS observaciones (
       id_obs              INTEGER PRIMARY KEY AUTOINCREMENT,
       id_anuncio          INTEGER NOT NULL REFERENCES anuncios(id_anuncio),
       id_ejecucion        INTEGER NOT NULL REFERENCES ejecuciones(id_ejecucion),
       precio              REAL,
       precio_lista        REAL,
       precio_club         REAL,
       promocion           INTEGER,
       disponible          INTEGER,
       fecha_hora_consulta TEXT NOT NULL,
       UNIQUE (id_anuncio, fecha_hora_consulta))"
  )
  walk(sentencias, ~ dbExecute(con, .x))
  dbExecute(con, "INSERT OR IGNORE INTO comercios VALUES
    ('locatel',   'Locatel',    'https://www.locatelcolombia.com'),
    ('cruzverde', 'Cruz Verde', 'https://www.cruzverde.com.co'),
    ('farmatodo', 'Farmatodo',  'https://www.farmatodo.com.co')")
  con
}

# Inserta los datos limpios de un comercio sin sobrescribir observaciones.
# Devuelve el número de observaciones nuevas.
insertar_lote <- function(con, datos, id_ejecucion) {
  if (nrow(datos) == 0) return(0L)
  dbWithTransaction(con, {
    # 1) Anuncios: si el SKU ya existe se actualizan sus datos descriptivos,
    #    pero se conservan id_anuncio, id_producto y comparable.
    anuncios <- datos %>%
      transmute(id_comercio = comercio, sku, seccion, ean, nombre, marca,
                subcategoria, texto_original, url, contenido, unidad, n_unidades,
                marca_std, subcategoria_std, es_paquete, excluido, motivo_exclusion)
    dbWriteTable(con, "tmp_anuncios", anuncios, temporary = TRUE, overwrite = TRUE)
    dbExecute(con, "
      INSERT INTO anuncios (id_comercio, sku, seccion, ean, nombre, marca,
        subcategoria, texto_original, url, contenido, unidad, n_unidades,
        marca_std, subcategoria_std, es_paquete, excluido, motivo_exclusion)
      SELECT * FROM tmp_anuncios WHERE true
      ON CONFLICT (id_comercio, sku) DO UPDATE SET
        seccion = excluded.seccion, ean = excluded.ean, nombre = excluded.nombre,
        marca = excluded.marca, subcategoria = excluded.subcategoria,
        texto_original = excluded.texto_original, url = excluded.url,
        contenido = excluded.contenido, unidad = excluded.unidad,
        n_unidades = excluded.n_unidades, marca_std = excluded.marca_std,
        subcategoria_std = excluded.subcategoria_std, es_paquete = excluded.es_paquete,
        excluido = excluded.excluido, motivo_exclusion = excluded.motivo_exclusion")

    # 2) Observaciones: una por anuncio y momento de consulta (no se duplican)
    obs <- datos %>%
      transmute(id_comercio = comercio, sku, precio, precio_lista, precio_club,
                promocion, disponible, fecha_hora_consulta)
    dbWriteTable(con, "tmp_obs", obs, temporary = TRUE, overwrite = TRUE)
    n <- dbExecute(con, "
      INSERT OR IGNORE INTO observaciones (id_anuncio, id_ejecucion, precio,
        precio_lista, precio_club, promocion, disponible, fecha_hora_consulta)
      SELECT a.id_anuncio, ?, t.precio, t.precio_lista, t.precio_club,
             t.promocion, t.disponible, t.fecha_hora_consulta
      FROM tmp_obs t
      JOIN anuncios a ON a.id_comercio = t.id_comercio AND a.sku = t.sku",
      params = list(id_ejecucion))
    dbExecute(con, "DROP TABLE tmp_anuncios")
    dbExecute(con, "DROP TABLE tmp_obs")
    n
  })
}

# Registra los productos comparables y su correspondencia (correspondencia.csv).
# Devuelve las filas del archivo que no encontraron su anuncio en la base.
aplicar_correspondencia <- function(con, archivo = "correspondencia.csv") {
  corr <- read_csv(archivo, col_types = cols(.default = col_character()),
                   locale = locale(encoding = "UTF-8"))
  productos <- corr %>%
    distinct(id_producto, nombre_std, marca_std, subcategoria_std, variante,
             contenido, unidad, n_unidades) %>%
    mutate(contenido = as.numeric(contenido), n_unidades = as.integer(n_unidades))

  dbWithTransaction(con, {
    dbWriteTable(con, "tmp_prod", productos, temporary = TRUE, overwrite = TRUE)
    dbExecute(con, "INSERT OR REPLACE INTO productos
                    SELECT id_producto, nombre_std, marca_std, subcategoria_std,
                           variante, contenido, unidad, n_unidades FROM tmp_prod")
    dbWriteTable(con, "tmp_corr", select(corr, id_producto, id_comercio, sku),
                 temporary = TRUE, overwrite = TRUE)
    # Se reinicia la marca para que quitar una fila del archivo también la quite de la base
    dbExecute(con, "UPDATE anuncios SET comparable = 0, id_producto = NULL")
    dbExecute(con, "
      UPDATE anuncios
      SET id_producto = (SELECT c.id_producto FROM tmp_corr c
                         WHERE c.id_comercio = anuncios.id_comercio
                           AND c.sku = anuncios.sku),
          comparable = 1
      WHERE EXISTS (SELECT 1 FROM tmp_corr c
                    WHERE c.id_comercio = anuncios.id_comercio
                      AND c.sku = anuncios.sku)")
    sin_anuncio <- dbGetQuery(con, "
      SELECT c.* FROM tmp_corr c
      LEFT JOIN anuncios a ON a.id_comercio = c.id_comercio AND a.sku = c.sku
      WHERE a.id_anuncio IS NULL")
    dbExecute(con, "DROP TABLE tmp_prod")
    dbExecute(con, "DROP TABLE tmp_corr")
    sin_anuncio
  })
}

# Revisa que cada anuncio comparable coincida con su producto en
# contenido, unidad, número de unidades y marca (sección 3.5, punto 4).
verificar_correspondencia <- function(con) {
  dbGetQuery(con, "
    SELECT p.id_producto, a.id_comercio, a.sku, a.nombre,
           p.marca_std AS marca_esperada, a.marca_std AS marca_anuncio,
           p.contenido AS contenido_esperado, a.contenido AS contenido_anuncio,
           p.unidad AS unidad_esperada, a.unidad AS unidad_anuncio,
           p.n_unidades AS unidades_esperadas, a.n_unidades AS unidades_anuncio,
           CASE WHEN a.contenido = p.contenido AND a.unidad = p.unidad
                 AND a.n_unidades = p.n_unidades AND a.marca_std = p.marca_std
                THEN 1 ELSE 0 END AS coincide
    FROM anuncios a
    JOIN productos p ON p.id_producto = a.id_producto
    WHERE a.comparable = 1
    ORDER BY p.id_producto, a.id_comercio")
}

# =============================================================================
# 11. Función general: una sola llamada ejecuta todo
#
#   ejecutar_recoleccion()                       # en línea, corrida nueva
#   ejecutar_recoleccion(offline = TRUE,
#                        corrida = "20261010T183000")  # reprocesa raw/
#
#   En modo sin conexión las observaciones ya cargadas no se duplican
#   (restricción UNIQUE), pero queda registrada la ejecución de reprocesamiento.
#   Para verificar sin tocar la base de entrega, use otra base:
#   ejecutar_recoleccion(db = "prueba_offline.sqlite", offline = TRUE, corrida = "...")
# =============================================================================
SCRAPERS <- list(
  locatel   = scrape_locatel_seguro,
  cruzverde = scrape_cruzverde_seguro,
  farmatodo = scrape_farmatodo_seguro
)

ejecutar_recoleccion <- function(db = "comparador_precios.sqlite",
                                 offline = FALSE, corrida = NULL,
                                 comercios = names(SCRAPERS),
                                 correspondencia = "correspondencia.csv") {
  if (offline && is.null(corrida)) {
    stop("En modo sin conexión indique la corrida: el nombre de la carpeta en raw/<comercio>/.")
  }
  if (is.null(corrida)) corrida <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = ZONA)

  con <- crear_base(db)
  on.exit(dbDisconnect(con), add = TRUE)

  resumen <- map_dfr(comercios, function(cm) {
    inicio <- ahora_co()
    message("== ", cm, " (", if (offline) "sin conexión" else "en línea", ")")
    datos <- SCRAPERS[[cm]](corrida = corrida, offline = offline)
    verif <- attr(datos, "verificacion")
    datos <- limpiar(datos)

    total_sitio <- if (is.null(verif)) NA_integer_ else sum(verif$total_sitio, na.rm = TRUE)
    estado <- case_when(
      nrow(datos) == 0 ~ "fallida",
      !is.null(verif) && any(is.na(verif$obtenidos) |
                             verif$obtenidos < verif$total_sitio) ~ "parcial",
      TRUE ~ "completa"
    )

    dbExecute(con,
      "INSERT INTO ejecuciones (corrida, id_comercio, inicio, n_registros,
                                total_sitio, estado, modo)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(corrida, cm, inicio, nrow(datos), total_sitio, estado,
                    if (offline) "sin_conexion" else "en_linea"))
    id_ej <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    nuevas <- insertar_lote(con, datos, id_ej)
    dbExecute(con, "UPDATE ejecuciones SET fin = ? WHERE id_ejecucion = ?",
              params = list(ahora_co(), id_ej))

    tibble(comercio = cm, registros = nrow(datos), total_sitio = total_sitio,
           excluidos = sum(datos$excluido), observaciones_nuevas = nuevas,
           estado = estado)
  })

  if (file.exists(correspondencia)) {
    sin_anuncio <- aplicar_correspondencia(con, correspondencia)
    if (nrow(sin_anuncio) > 0) {
      message("Filas de la correspondencia sin anuncio en la base:")
      print(sin_anuncio)
    }
    verif_corr <- verificar_correspondencia(con)
    if (any(verif_corr$coincide == 0)) {
      message("Comparables que NO coinciden con su producto (revisar):")
      print(filter(verif_corr, coincide == 0))
    }
  }

  message("Corrida ", corrida, " terminada.")
  print(resumen)
  invisible(list(corrida = corrida, resumen = resumen))
}
