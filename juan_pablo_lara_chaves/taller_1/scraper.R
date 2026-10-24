# Scraper.R, Taller 1 - Mineria de datos, Categoria: Ortopedia y rehabilitacion
library(rvest)
library(httr2)
library(dplyr)
library(purrr)
library(stringr)

# Parser de precios colombianos
parsear_precio_cop <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") return(NA_real_)
  x <- str_remove_all(x, "[^0-9,]")
  if (x == "") return(NA_real_)
  x <- str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# Funcion de scraping para ortopedicos futuro y locatel----
scrapear_categoria <- function(url_base,
                               comercio,
                               max_paginas    = 100,
                               pausa_carga    = 8,
                               pausa_scroll   = 1.5,
                               scrolls        = 8,
                               pausa_estab    = 3) {
  
  categoria <- str_extract(url_base, "[^/]+$")
  todos <- list()
  
  for (page in 1:max_paginas) {
    url <- if (page == 1) url_base else paste0(url_base, "?page=", page)
    message("[", comercio, " | ", categoria, "] Página ", page)
    
    pagina <- tryCatch(read_html_live(url), error = function(e) NULL)
    if (is.null(pagina)) break
    Sys.sleep(pausa_carga)
    
    for (s in 1:scrolls) {
      pagina$session$Runtime$evaluate("window.scrollBy(0, 1000);")
      Sys.sleep(pausa_scroll)
    }
    Sys.sleep(pausa_estab)
    
    # Estabilización
    n_prev <- 0
    for (k in 1:10) {
      n_ahora <- pagina %>% html_elements("article.vtex-product-summary-2-x-element") %>% length()
      if (n_ahora == n_prev && n_ahora > 0) break
      n_prev <- n_ahora
      Sys.sleep(1)
    }
    
    articles <- pagina %>% html_elements("article.vtex-product-summary-2-x-element")
    n <- length(articles)
    message("  Productos cargados: ", n)
    
    # PARAR SOLO SI 0 PRODUCTOS
    if (n == 0) {
      pagina$session$close()
      message("  Página vacía. Fin.")
      break
    }
    
    lista_datos <- map(articles, function(a) {
      nombre <- a %>% html_element(".vtex-product-summary-2-x-productBrand") %>% html_text2()
      if (length(nombre) == 0 || is.na(nombre) || nombre == "") {
        nombre <- a %>% html_element(".vtex-product-summary-2-x-nameContainer") %>% html_attr("aria-label")
      }
      
      p_txt <- a %>% html_element(".vtex-product-price-1-x-sellingPrice") %>% html_text2()
      l_txt <- a %>% html_element(".vtex-product-price-1-x-listPrice") %>% html_text2()
      if (length(p_txt) == 0 || is.na(p_txt) || p_txt == "") p_txt <- l_txt
      
      tibble(
        comercio     = comercio,
        categoria    = categoria,
        nombre       = nombre,
        precio       = parsear_precio_cop(p_txt),
        precio_lista = parsear_precio_cop(l_txt),
        url          = url,
        fecha_hora   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
      )
    })
    
    datos <- bind_rows(compact(lista_datos))
    pagina$session$close()
    
    message("  Extraídos: ", nrow(datos))
    if (nrow(datos) == 0) break
    
    todos <- c(todos, list(datos))
    Sys.sleep(1)
  }
  
  bind_rows(todos)
}

# Funcion de scraping para farmatodo ----

scrapear_categoria_farmatodo <- function(url_base,
                                         max_paginas    = 100,
                                         pausa_carga    = 8,
                                         pausa_scroll   = 1.5,
                                         scrolls        = 10,
                                         pausa_estab    = 3) {
  
  comercio  <- "farmatodo"
  dominio   <- "https://www.farmatodo.com.co"
  categoria <- str_extract(url_base, "[^/]+$")
  todos <- list()
  
  for (page in 1:max_paginas) {
    # Primera iteración sin ?pag, luego ?pag=1, ?pag=2, ...
    url <- if (page == 1) url_base else paste0(url_base, "?pag=", page - 1)
    message("[", comercio, " | ", categoria, "] Iteración ", page)
    
    pagina <- tryCatch(read_html_live(url), error = function(e) NULL)
    if (is.null(pagina)) break
    Sys.sleep(pausa_carga)
    
    # Scroll sobre el contenedor correcto de Farmatodo
    for (s in 1:scrolls) {
      pagina$session$Runtime$evaluate(
        "document.querySelector('#app-component-router-outlet').scrollBy(0, 500);"
      )
      Sys.sleep(pausa_scroll)
    }
    Sys.sleep(pausa_estab)
    
    # Selector de tarjeta de Farmatodo
    cards <- pagina %>% html_elements("div.card-ftd")
    n <- length(cards)
    message("  Productos cargados: ", n)
    
    if (n == 0) {
      pagina$session$close()
      message("  Página vacía. Fin.")
      break
    }
    
    lista_datos <- map(cards, function(c) {
      # Nombre y URL desde el link del detalle
      link <- c %>% html_element("a.product-card-handler__detail-description")
      nombre <- if (length(link) > 0) link %>% html_attr("title") else NA_character_
      href   <- if (length(link) > 0) link %>% html_attr("href") else NA_character_
      url_producto <- if (!is.na(href) && length(href) > 0) paste0(dominio, href) else NA_character_
      
      # ID interno del producto (SKU)
      sku <- c %>% html_element("[data-cuf]") %>% html_attr("data-cuf")
      
      # Marca
      marca <- c %>% html_element("p.product-card-handler__brand-value") %>% html_text2()
      
      # Precio de oferta y precio normal
      p_txt <- c %>% html_element(".product-all-price__offer") %>% html_text2()
      l_txt <- c %>% html_element(".product-all-price__normal") %>% html_text2()
      if (length(p_txt) == 0 || is.na(p_txt) || p_txt == "") p_txt <- l_txt
      
      # Descuento
      desc <- c %>% html_element(".product-offer-description__band-text--bold") %>% html_text2()
      
      tibble(
        comercio     = comercio,
        categoria    = categoria,
        nombre       = nombre,
        marca        = marca,
        sku          = sku,
        precio       = parsear_precio_cop(p_txt),
        precio_lista = parsear_precio_cop(l_txt),
        descuento    = desc,
        url          = url_producto,
        url_origen   = url,
        fecha_hora   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
      )
    })
    
    datos <- bind_rows(compact(lista_datos))
    pagina$session$close()
    
    message("  Extraídos: ", nrow(datos))
    if (nrow(datos) == 0) break
    
    todos <- c(todos, list(datos))
    Sys.sleep(1)
  }
  
  bind_rows(todos)
}

# sección adicional que no se aplicó dado que los datos ya habían sido scapeados

URLS_CATEGORIAS <- list(
  ortopedicos_futuro = c(
    "https://www.ortopedicosfuturo.com/cuidado-corporal/tratamiento-de-lesiones/rodilla",
    "https://www.ortopedicosfuturo.com/cuidado-corporal/tratamiento-de-lesiones/tobillo",
    "https://www.ortopedicosfuturo.com/cuidado-corporal/tratamiento-de-lesiones/brazo-y-mano",
    "https://www.ortopedicosfuturo.com/cuidado-corporal/prendas-de-vestir/fajas",
    "https://www.ortopedicosfuturo.com/medias-de-compresion",
    "https://www.ortopedicosfuturo.com/bienestar-en-casa/movilidad-asistida/bastones-y-caminador-y-muletas",
    "https://www.ortopedicosfuturo.com/deporte-y-fitness/terapia-fisica"
  ),
  locatel = c(
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/ortopedia-traumatologia/extremidades-inferiores",
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/ortopedia-traumatologia/extremidades-superiores",
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/ortopedia-traumatologia/tronco",
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/ortopedia-traumatologia/prendas-vestir",
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/aparatos-movilidad-asistida/bastones",
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/aparatos-movilidad-asistida/muletas",
    "https://www.locatelcolombia.com/dispositivos-medicos-en-casa/ortopedia-traumatologia/fisioterapia"
  ),
  farmatodo = paste0(
    "https://www.farmatodo.com.co/categorias/salud-y-medicamentos/rehabilitacion-y-equipos-medicos/",
    c("rodilleras", "tobilleras", "munequeras", "fajas-lumbares-y-abdominales",
      "calcetines-y-medias-terapeuticas", "andadores", "productos-para-rehabilitacion")
  )
)

## --- Una función por comercio (devuelven siempre el mismo esquema) ---------
scrapear_ortopedicos_futuro <- function(urls = URLS_CATEGORIAS$ortopedicos_futuro) {
  map_dfr(urls, ~ scrapear_categoria(url_base = .x, comercio = "ortopedicos_futuro"))
}
scrapear_locatel <- function(urls = URLS_CATEGORIAS$locatel) {
  map_dfr(urls, ~ scrapear_categoria(url_base = .x, comercio = "locatel"))
}
scrapear_farmatodo <- function(urls = URLS_CATEGORIAS$farmatodo) {
  map_dfr(urls, scrapear_categoria_farmatodo)
}

## --- Copia del HTML original de una página (llámala dentro de tus funciones
##     de scraping, justo después de cargar y desplazar la página) -----------
## `pagina` es el objeto de read_html_live(). Verifica que as.character() sobre
## html_element(pagina, "html") te devuelva el HTML completo en tu versión de rvest.
guardar_html_crudo <- function(pagina, comercio, etiqueta, raw_dir = "raw",
                               fecha = format(Sys.Date(), "%Y-%m-%d")) {
  dir <- file.path(raw_dir, comercio, fecha, "html")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  html <- as.character(rvest::html_element(pagina, "html"))
  con <- gzfile(file.path(dir, paste0(etiqueta, ".html.gz")), "w")
  on.exit(close(con))
  writeLines(html, con)
  invisible(file.path(dir, paste0(etiqueta, ".html.gz")))
}

## --- Función general: recolección completa con una sola llamada ------------
## Guarda una copia por comercio en raw/<comercio>/productos_<fecha>.csv y
## devuelve el registro de la corrida (comercio, inicio, fin, n, estado).
## Un comercio que falla queda como "fallida" sin detener a los demás.
ejecutar_recoleccion <- function(comercios = c("farmatodo", "locatel", "ortopedicos_futuro"),
                                 raw_dir = "raw") {
  fns   <- list(farmatodo = scrapear_farmatodo, locatel = scrapear_locatel,
                ortopedicos_futuro = scrapear_ortopedicos_futuro)
  fecha <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  
  map_dfr(comercios, function(cm) {
    inicio <- Sys.time()
    r <- tryCatch(fns[[cm]](), error = function(e) {
      message("[", cm, "] falló: ", conditionMessage(e)); NULL
    })
    if (!is.null(r) && nrow(r) > 0) {
      dir.create(file.path(raw_dir, cm), recursive = TRUE, showWarnings = FALSE)
      write_csv(r, file.path(raw_dir, cm, paste0("productos_", fecha, ".csv")))
    }
    tibble(
      comercio = cm,
      inicio   = format(inicio, "%Y-%m-%dT%H:%M:%S-05:00"),
      fin      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00"),
      n_registros = if (is.null(r)) 0L else nrow(r),
      estado   = if (is.null(r) || nrow(r) == 0) "fallida" else "completa"
    )
  })
}

