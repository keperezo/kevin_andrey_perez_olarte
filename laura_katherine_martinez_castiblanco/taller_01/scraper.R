# ==============================================================================
# TALLER 1 - MÓDULO PRINCIPAL DE EXTRACCIÓN Y SCRAPING (KDD)
# ARCHIVO: scraper.R
# COBERTURA: Éxito, Carulla, PriceSmart, Supermercado Comunal (4 Tiendas)
# ==============================================================================

# Configuración defensiva del directorio de trabajo (RStudio o Consola/Colab)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(chromote)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
  library(fs)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(digest)
})

# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN GLOBAL Y CONSTANTES
# ------------------------------------------------------------------------------
try({
  chromote::set_chrome_args(c(
    "--ignore-certificate-errors",
    "--ignore-ssl-errors",
    "--allow-running-insecure-content",
    "--no-sandbox",
    "--disable-gpu"
  ))
}, silent = TRUE)

log_info <- function(tienda, mensaje, tipo = "INFO") {
  cat(sprintf("[%s] [%s] [%-20s] %s\n", format(Sys.time(), "%H:%M:%S"), tipo, tienda, mensaje))
  flush.console()
}

BUSQUEDAS_GLOBALES <- c(
  "gaseosa", "jugo", "agua", "te", "energizante", 
  "coca cola", "pepsi", "postobon", "hit", "hatsu", "red bull", "monster", "cerveza"
)

PATRON_EXCLUSIONES <- paste0(
  "(?i)\\b(",
  "teclado|telon|proyector|parlante|bluetooth|teatro|celular|televisor|woofer|fuse|block|circuitos|bateria|terminal|",
  "lego|carro|balon|peluche|juego|figura|coleccionable|piñata|minix|r/c|maisto|boing|",
  "colchon|cuadro|cojin|lampara|portaretrato|mug|taza|dispensador|tenedores|canguro|tenis|bota|libro|agenda|clapes|ishida|llenas|papa francisco|",
  "micelar|ampolletas|hidratante|shampoo|detergente|mascotas|cuidado|aseo|vinagreta|nachos|dulces|arequipe|manjar|",
  "molido|liofilizado|en grano|tostado|grano|polvo|mezcla en polvo|bolsas /|yerba mate|matcha|infusión|pre entreno|crema en polvo",
  ")\\b"
)

# ------------------------------------------------------------------------------
# 2. FUNCIONES AUXILIARES DE LIMPIEZA Y EXTRACCIÓN MÉTRICA
# ------------------------------------------------------------------------------
limpiar_precio_cop <- function(val) {
  if (is.null(val) || is.na(val) || length(val) == 0) return(NA_real_)
  if (is.numeric(val)) return(as.numeric(val))
  
  val_str <- as.character(val)
  m <- str_extract(val_str, "\\$\\s*\\d+(?:[.,]\\d+)*|\\b\\d{1,3}(?:[.,]\\d{3})+\\b")
  if (!is.na(m)) {
    num_clean <- str_replace_all(m, "[^0-9]", "")
    if (nchar(num_clean) > 0) return(as.numeric(num_clean))
  }
  
  m_fallback <- str_extract(val_str, "\\d+")
  if (!is.na(m_fallback) && nchar(m_fallback) > 0) {
    return(as.numeric(m_fallback))
  }
  return(NA_real_)
}

extraer_volumen_avanzado <- function(texto) {
  if (is.na(texto) || texto == "") return(list(num = NA_real_, und = NA_character_, ml = NA_real_))
  
  txt <- str_replace_all(tolower(texto), "(?<=\\d)[.,](?=\\d{3}\\b)", "")
  
  # A. Multipacks (ej. "x6und x 269ml", "6x330ml", "330 ml x 6")
  m_mult1 <- str_match(txt, "x?\\s*(\\d+)\\s*(?:und|unds|unidades|latas|botellas)?\\s*x\\s*(\\d+)\\s*(ml|cc)")
  if (!is.na(m_mult1[1, 1])) {
    cant <- as.numeric(m_mult1[1, 2])
    vol  <- as.numeric(m_mult1[1, 3])
    return(list(num = cant * vol, und = "ml", ml = cant * vol))
  }
  
  m_mult2 <- str_match(txt, "(\\d+)\\s*(ml|cc)\\s*x\\s*(\\d+)")
  if (!is.na(m_mult2[1, 1])) {
    vol  <- as.numeric(m_mult2[1, 2])
    cant <- as.numeric(m_mult2[1, 4])
    return(list(num = vol * cant, und = "ml", ml = vol * cant))
  }
  
  # B. Litros
  m_litros <- str_match(txt, "\\b(\\d+(?:[.,]\\d+)?)\\s*(l|lt|lts|litro|litros)\\b")
  if (!is.na(m_litros[1, 1])) {
    val_num <- as.numeric(str_replace(m_litros[1, 2], ",", "."))
    return(list(num = val_num, und = "L", ml = val_num * 1000))
  }
  
  # C. Mililitros
  m_ml <- str_match(txt, "\\b(\\d+)\\s*(ml|cc|cm3|mililitros)\\b")
  if (!is.na(m_ml[1, 1])) {
    val_num <- as.numeric(m_ml[1, 2])
    return(list(num = val_num, und = "ml", ml = val_num))
  }
  
  # D. Onzas
  m_oz <- str_match(txt, "\\b(\\d+(?:[.,]\\d+)?)\\s*(oz|onza|onzas)\\b")
  if (!is.na(m_oz[1, 1])) {
    val_num <- as.numeric(str_replace(m_oz[1, 2], ",", "."))
    val_ml  <- val_num * 29.5735
    return(list(num = round(val_ml, 1), und = "ml", ml = val_ml))
  }
  
  return(list(num = NA_real_, und = NA_character_, ml = NA_real_))
}

determinar_subcategoria <- function(texto) {
  txt <- tolower(texto)
  if (str_detect(txt, "agua|manantial|cristal|brisa|pellegrino")) return("Agua")
  if (str_detect(txt, "gaseosa|coca-cola|coca cola|pepsi|postobon|colombiana|quatro|sprite|soda")) return("Gaseosas")
  if (str_detect(txt, "jugo|hit|del valle|pulpa|nectar|tropicana")) return("Jugos")
  if (str_detect(txt, "te|hatsu|fuze|mr tea")) return("Té Preparado")
  if (str_detect(txt, "energizante|red bull|monster|vive 100")) return("Energizantes")
  if (str_detect(txt, "cerveza|pilsen|poker|club colombia|heineken|corona")) return("Cervezas")
  return("Otros / Sin Categorizar")
}

preparar_sesion_chromote <- function(modo_interactivo = FALSE) {
  b <- ChromoteSession$new()
  if (modo_interactivo) try(b$view(), silent = TRUE)
  try(b$Emulation$setDeviceMetricsOverride(width = 1920, height = 1080, deviceScaleFactor = 1, mobile = FALSE), silent = TRUE)
  try(b$Security$enable(), silent = TRUE)
  try(b$Security$setIgnoreCertificateErrors(ignore = TRUE), silent = TRUE)
  return(b)
}

# ------------------------------------------------------------------------------
# 3. SCRAPERS ESPECÍFICOS POR TIENDA (MODO ONLINE Y OFFLINE VÍA RAW)
# ------------------------------------------------------------------------------

### --- A. ÉXITO ---
scrapear_exito <- function(id_ejecucion, fecha_iso, modo_interactivo = FALSE, modo_offline = FALSE) {
  tienda <- "Éxito"
  log_info(tienda, sprintf("Iniciando extracción (%s)...", ifelse(modo_offline, "OFFLINE DESDE RAW", "ONLINE")))
  dir_create("raw/exito")
  resultados <- tibble()
  
  if (modo_offline) {
    archivos_raw <- dir_ls("raw/exito", glob = "*.html")
    if (length(archivos_raw) == 0) return(resultados)
    
    for (f_path in archivos_raw) {
      tryCatch({
        html_content <- readLines(f_path, warn = FALSE, encoding = "UTF-8") %>% paste(collapse = "\n")
        doc <- read_html(html_content)
        nodos <- html_elements(doc, "[class*='productCard_contentInfo'], article")
        if (length(nodos) == 0) next
        
        df_f <- map_dfr(nodos, function(nodo) {
          nombre <- html_element(nodo, "h3[class*='styles_name'], [class*='brandName']") %>% html_text2()
          if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) return(NULL)
          
          precio_str <- html_element(nodo, "[class*='Price'], [class*='price']") %>% html_text2()
          precio_val <- limpiar_precio_cop(precio_str)
          vol_info   <- extraer_volumen_avanzado(paste(nombre, html_text2(nodo)))
          img_url    <- html_element(nodo, "img") %>% html_attr("src")
          
          tibble(
            id_ejecucion           = id_ejecucion,
            comercio               = tienda,
            fecha_hora_consulta    = fecha_iso,
            producto_raw           = str_trim(nombre),
            texto_original_anuncio = html_text2(nodo),
            precio_cop             = precio_val,
            es_promocion           = 0L,
            url_extraccion         = paste0("local://raw/exito/", basename(f_path)),
            sku_codigo             = paste0("EXT-", digest(nombre, algo = "md5")),
            marca_raw              = str_to_title(str_extract(nombre, "^\\w+")),
            subcategoria           = determinar_subcategoria(nombre),
            presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
            contenido_num          = vol_info$num,
            unidad_medida          = vol_info$und,
            comparable             = 0L,
            id_producto_comun      = NA_character_,
            marca_estandarizada    = str_to_upper(str_trim(str_extract(nombre, "^\\w+"))),
            precio_lista_cop       = precio_val,
            precio_por_ml          = NA_real_,
            precio_por_litro       = NA_real_,
            disponibilidad         = "En Stock",
            url_imagen             = ifelse(is.na(img_url), "", img_url)
          )
        })
        resultados <- bind_rows(resultados, df_f)
      }, error = function(e) NULL)
    }
    return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
  }
  
  # Modo Online Chromote
  b <- tryCatch({ preparar_sesion_chromote(modo_interactivo) }, error = function(e) return(NULL))
  if (is.null(b)) return(tibble())
  on.exit({ try(b$close(), silent = TRUE) }, add = TRUE)
  
  for (term in BUSQUEDAS_GLOBALES) {
    tryCatch({
      url_search <- paste0("https://www.exito.com/s?q=", URLencode(term), "&sort=score_desc")
      log_info(tienda, paste("Buscando en vivo:", term))
      b$Page$navigate(url_search)
      Sys.sleep(3.5)
      
      for (s in 1:3) { b$Runtime$evaluate("window.scrollBy(0, 800);"); Sys.sleep(1) }
      
      res <- b$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)
      html_content <- res$result$value
      
      if (!is.null(html_content) && nchar(html_content) > 0) {
        writeLines(html_content, paste0("raw/exito/exito_", str_replace_all(term, " ", "_"), "_", round(id_ejecucion), ".html"), useBytes = TRUE)
        doc <- read_html(html_content)
        nodos <- html_elements(doc, "[class*='productCard_contentInfo'], article")
        
        if (length(nodos) > 0) {
          df_term <- map_dfr(nodos, function(nodo) {
            nombre <- html_element(nodo, "h3[class*='styles_name'], [class*='brandName']") %>% html_text2()
            if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) return(NULL)
            
            precio_str <- html_element(nodo, "[class*='Price'], [class*='price']") %>% html_text2()
            precio_val <- limpiar_precio_cop(precio_str)
            vol_info   <- extraer_volumen_avanzado(paste(nombre, html_text2(nodo)))
            img_url    <- html_element(nodo, "img") %>% html_attr("src")
            
            tibble(
              id_ejecucion           = id_ejecucion,
              comercio               = tienda,
              fecha_hora_consulta    = fecha_iso,
              producto_raw           = str_trim(nombre),
              texto_original_anuncio = html_text2(nodo),
              precio_cop             = precio_val,
              es_promocion           = 0L,
              url_extraccion         = url_search,
              sku_codigo             = paste0("EXT-", digest(nombre, algo = "md5")),
              marca_raw              = str_to_title(term),
              subcategoria           = determinar_subcategoria(nombre),
              presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
              contenido_num          = vol_info$num,
              unidad_medida          = vol_info$und,
              comparable             = 0L,
              id_producto_comun      = NA_character_,
              marca_estandarizada    = str_to_upper(str_trim(term)),
              precio_lista_cop       = precio_val,
              precio_por_ml          = NA_real_,
              precio_por_litro       = NA_real_,
              disponibilidad         = "En Stock",
              url_imagen             = ifelse(is.na(img_url), "", img_url)
            )
          })
          resultados <- bind_rows(resultados, df_term)
        }
      }
    }, error = function(e) NULL)
  }
  return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
}

### --- B. CARULLA ---
scrapear_carulla <- function(id_ejecucion, fecha_iso, modo_interactivo = FALSE, modo_offline = FALSE) {
  tienda <- "Carulla"
  log_info(tienda, sprintf("Iniciando extracción (%s)...", ifelse(modo_offline, "OFFLINE DESDE RAW", "ONLINE")))
  dir_create("raw/carulla")
  resultados <- tibble()
  
  if (modo_offline) {
    archivos_raw <- dir_ls("raw/carulla", glob = "*.html")
    if (length(archivos_raw) == 0) return(resultados)
    
    for (f_path in archivos_raw) {
      tryCatch({
        html_content <- readLines(f_path, warn = FALSE, encoding = "UTF-8") %>% paste(collapse = "\n")
        doc <- read_html(html_content)
        nodos <- html_elements(doc, "[class*='productCard_contentInfo'], article")
        if (length(nodos) == 0) next
        
        df_f <- map_dfr(nodos, function(nodo) {
          nombre <- html_element(nodo, "h3[class*='styles_name'], [class*='brandName']") %>% html_text2()
          if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) return(NULL)
          
          precio_str <- html_element(nodo, "[class*='Price'], [class*='price'], [class*='productCard_priceOfferButton']") %>% html_text2()
          precio_val <- limpiar_precio_cop(precio_str)
          vol_info   <- extraer_volumen_avanzado(paste(nombre, html_text2(nodo)))
          img_url    <- html_element(nodo, "img") %>% html_attr("src")
          
          tibble(
            id_ejecucion           = id_ejecucion,
            comercio               = tienda,
            fecha_hora_consulta    = fecha_iso,
            producto_raw           = str_trim(nombre),
            texto_original_anuncio = html_text2(nodo),
            precio_cop             = precio_val,
            es_promocion           = 0L,
            url_extraccion         = paste0("local://raw/carulla/", basename(f_path)),
            sku_codigo             = paste0("CAR-", digest(nombre, algo = "md5")),
            marca_raw              = str_to_title(str_extract(nombre, "^\\w+")),
            subcategoria           = determinar_subcategoria(nombre),
            presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
            contenido_num          = vol_info$num,
            unidad_medida          = vol_info$und,
            comparable             = 0L,
            id_producto_comun      = NA_character_,
            marca_estandarizada    = str_to_upper(str_trim(str_extract(nombre, "^\\w+"))),
            precio_lista_cop       = precio_val,
            precio_por_ml          = NA_real_,
            precio_por_litro       = NA_real_,
            disponibilidad         = "En Stock",
            url_imagen             = ifelse(is.na(img_url), "", img_url)
          )
        })
        resultados <- bind_rows(resultados, df_f)
      }, error = function(e) NULL)
    }
    return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
  }
  
  # Modo Online Chromote
  b <- tryCatch({ preparar_sesion_chromote(modo_interactivo) }, error = function(e) return(NULL))
  if (is.null(b)) return(tibble())
  on.exit({ try(b$close(), silent = TRUE) }, add = TRUE)
  
  for (term in BUSQUEDAS_GLOBALES) {
    tryCatch({
      url_search <- paste0("https://www.carulla.com/s?q=", URLencode(term), "&sort=score_desc&page=0")
      log_info(tienda, paste("Buscando en vivo:", term))
      b$Page$navigate(url_search)
      Sys.sleep(3.5)
      
      for (s in 1:3) { b$Runtime$evaluate("window.scrollBy(0, 800);"); Sys.sleep(1) }
      
      res <- b$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)
      html_content <- res$result$value
      
      if (!is.null(html_content) && nchar(html_content) > 0) {
        writeLines(html_content, paste0("raw/carulla/carulla_", str_replace_all(term, " ", "_"), "_", round(id_ejecucion), ".html"), useBytes = TRUE)
        doc <- read_html(html_content)
        nodos <- html_elements(doc, "[class*='productCard_contentInfo'], article")
        
        if (length(nodos) > 0) {
          df_term <- map_dfr(nodos, function(nodo) {
            nombre <- html_element(nodo, "h3[class*='styles_name'], [class*='brandName']") %>% html_text2()
            if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) return(NULL)
            
            precio_str <- html_element(nodo, "[class*='Price'], [class*='price'], [class*='productCard_priceOfferButton']") %>% html_text2()
            precio_val <- limpiar_precio_cop(precio_str)
            vol_info   <- extraer_volumen_avanzado(paste(nombre, html_text2(nodo)))
            img_url    <- html_element(nodo, "img") %>% html_attr("src")
            
            tibble(
              id_ejecucion           = id_ejecucion,
              comercio               = tienda,
              fecha_hora_consulta    = fecha_iso,
              producto_raw           = str_trim(nombre),
              texto_original_anuncio = html_text2(nodo),
              precio_cop             = precio_val,
              es_promocion           = 0L,
              url_extraccion         = url_search,
              sku_codigo             = paste0("CAR-", digest(nombre, algo = "md5")),
              marca_raw              = str_to_title(term),
              subcategoria           = determinar_subcategoria(nombre),
              presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
              contenido_num          = vol_info$num,
              unidad_medida          = vol_info$und,
              comparable             = 0L,
              id_producto_comun      = NA_character_,
              marca_estandarizada    = str_to_upper(str_trim(term)),
              precio_lista_cop       = precio_val,
              precio_por_ml          = NA_real_,
              precio_por_litro       = NA_real_,
              disponibilidad         = "En Stock",
              url_imagen             = ifelse(is.na(img_url), "", img_url)
            )
          })
          resultados <- bind_rows(resultados, df_term)
        }
      }
    }, error = function(e) NULL)
  }
  return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
}

### --- C. PRICESMART ---
scrapear_pricesmart <- function(id_ejecucion, fecha_iso, modo_interactivo = FALSE, modo_offline = FALSE) {
  tienda <- "PriceSmart"
  log_info(tienda, sprintf("Iniciando extracción (%s)...", ifelse(modo_offline, "OFFLINE DESDE RAW", "ONLINE")))
  dir_create("raw/pricesmart")
  resultados <- tibble()
  
  if (modo_offline) {
    archivos_raw <- dir_ls("raw/pricesmart", glob = "*.html")
    if (length(archivos_raw) == 0) return(resultados)
    
    for (f_path in archivos_raw) {
      tryCatch({
        html_content <- readLines(f_path, warn = FALSE, encoding = "UTF-8") %>% paste(collapse = "\n")
        doc <- read_html(html_content)
        nodos <- html_elements(doc, "[class*='info-wrapper__product-info'], [class*='product-card'], .product-tile, article")
        if (length(nodos) == 0) next
        
        df_f <- map_dfr(nodos, function(nodo) {
          nombre <- html_element(nodo, "[class*='product-title'], [class*='info-wrapper'], h3, a") %>% html_text2()
          if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) return(NULL)
          
          precio_str <- html_element(nodo, "[class*='sf-price_regular'], [class*='sf-price'], [class*='price']") %>% html_text2()
          precio_val <- limpiar_precio_cop(precio_str)
          vol_info   <- extraer_volumen_avanzado(paste(nombre, html_text2(nodo)))
          img_url    <- html_element(nodo, "img") %>% html_attr("src")
          
          tibble(
            id_ejecucion           = id_ejecucion,
            comercio               = tienda,
            fecha_hora_consulta    = fecha_iso,
            producto_raw           = str_trim(nombre),
            texto_original_anuncio = html_text2(nodo),
            precio_cop             = precio_val,
            es_promocion           = 0L,
            url_extraccion         = paste0("local://raw/pricesmart/", basename(f_path)),
            sku_codigo             = paste0("PS-", digest(nombre, algo = "md5")),
            marca_raw              = str_to_title(str_extract(nombre, "^\\w+")),
            subcategoria           = determinar_subcategoria(nombre),
            presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
            contenido_num          = vol_info$num,
            unidad_medida          = vol_info$und,
            comparable             = 0L,
            id_producto_comun      = NA_character_,
            marca_estandarizada    = str_to_upper(str_trim(str_extract(nombre, "^\\w+"))),
            precio_lista_cop       = precio_val,
            precio_por_ml          = NA_real_,
            precio_por_litro       = NA_real_,
            disponibilidad         = "En Stock",
            url_imagen             = ifelse(is.na(img_url), "", img_url)
          )
        })
        resultados <- bind_rows(resultados, df_f)
      }, error = function(e) NULL)
    }
    return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
  }
  
  # Modo Online Chromote
  b <- tryCatch({ preparar_sesion_chromote(modo_interactivo) }, error = function(e) return(NULL))
  if (is.null(b)) return(tibble())
  on.exit({ try(b$close(), silent = TRUE) }, add = TRUE)
  
  for (term in BUSQUEDAS_GLOBALES) {
    tryCatch({
      url_search <- paste0("https://www.pricesmart.com/es-co/busqueda?q=", URLencode(term))
      log_info(tienda, paste("Buscando en vivo:", term))
      b$Page$navigate(url_search)
      Sys.sleep(4.5)
      
      for (s in 1:3) { b$Runtime$evaluate("window.scrollBy(0, 800);"); Sys.sleep(1) }
      
      res <- b$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)
      html_content <- res$result$value
      
      if (!is.null(html_content) && nchar(html_content) > 0) {
        writeLines(html_content, paste0("raw/pricesmart/ps_", str_replace_all(term, " ", "_"), "_", round(id_ejecucion), ".html"), useBytes = TRUE)
        doc <- read_html(html_content)
        nodos <- html_elements(doc, "[class*='info-wrapper__product-info'], [class*='product-card'], .product-tile, article")
        
        if (length(nodos) > 0) {
          df_term <- map_dfr(nodos, function(nodo) {
            nombre <- html_element(nodo, "[class*='product-title'], [class*='info-wrapper'], h3, a") %>% html_text2()
            if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) return(NULL)
            
            precio_str <- html_element(nodo, "[class*='sf-price_regular'], [class*='sf-price'], [class*='price']") %>% html_text2()
            precio_val <- limpiar_precio_cop(precio_str)
            vol_info   <- extraer_volumen_avanzado(paste(nombre, html_text2(nodo)))
            img_url    <- html_element(nodo, "img") %>% html_attr("src")
            
            tibble(
              id_ejecucion           = id_ejecucion,
              comercio               = tienda,
              fecha_hora_consulta    = fecha_iso,
              producto_raw           = str_trim(nombre),
              texto_original_anuncio = html_text2(nodo),
              precio_cop             = precio_val,
              es_promocion           = 0L,
              url_extraccion         = url_search,
              sku_codigo             = paste0("PS-", digest(nombre, algo = "md5")),
              marca_raw              = str_to_title(term),
              subcategoria           = determinar_subcategoria(nombre),
              presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
              contenido_num          = vol_info$num,
              unidad_medida          = vol_info$und,
              comparable             = 0L,
              id_producto_comun      = NA_character_,
              marca_estandarizada    = str_to_upper(str_trim(term)),
              precio_lista_cop       = precio_val,
              precio_por_ml          = NA_real_,
              precio_por_litro       = NA_real_,
              disponibilidad         = "En Stock",
              url_imagen             = ifelse(is.na(img_url), "", img_url)
            )
          })
          resultados <- bind_rows(resultados, df_term)
        }
      }
    }, error = function(e) NULL)
  }
  return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
}

### --- D. SUPERMERCADO COMUNAL ---
scrapear_supercomunal <- function(id_ejecucion, fecha_iso, modo_interactivo = FALSE, modo_offline = FALSE) {
  tienda <- "Supermercado Comunal"
  log_info(tienda, sprintf("Iniciando extracción (%s)...", ifelse(modo_offline, "OFFLINE DESDE RAW", "ONLINE")))
  dir_create("raw/supercomunal")
  resultados <- tibble()
  
  if (modo_offline) {
    archivos_raw <- dir_ls("raw/supercomunal", glob = "*.html")
    if (length(archivos_raw) == 0) return(resultados)
    
    for (f_path in archivos_raw) {
      tryCatch({
        html_content <- readLines(f_path, warn = FALSE, encoding = "UTF-8") %>% paste(collapse = "\n")
        doc <- read_html(html_content)
        
        # Evaluar si el HTML corresponde a una Ficha de Producto o a un Listado
        nombre <- html_element(doc, "h3.product-title, h1.product-title, [class*='product-title'], h1") %>% html_text2()
        
        if (!is.na(nombre) && str_trim(nombre) != "" && !str_detect(nombre, PATRON_EXCLUSIONES)) {
          # Ficha de Producto Individual
          precio_elem <- html_element(doc, "[itemprop='price'], .current-price span[itemprop='price'], .sync-product-price .price, .current-price .price, .product-price .price, span.price")
          precio_str  <- if (!is.na(precio_elem)) html_attr(precio_elem, "content") else NA_character_
          if (is.na(precio_str) || str_trim(precio_str) == "") precio_str <- if (!is.na(precio_elem)) html_text2(precio_elem) else NA_character_
          
          precio_val <- limpiar_precio_cop(precio_str)
          sku_ref    <- html_element(doc, "[itemprop='sku'], .reference span, .reference") %>% html_text2()
          sku_clean  <- str_extract(sku_ref, "\\d+")
          if (is.na(sku_clean) || sku_clean == "") sku_clean <- str_extract(basename(f_path), "\\d+")
          
          sku_codigo_val <- if (!is.na(sku_clean) && sku_clean != "") paste0("SC-", sku_clean) else paste0("SC-", digest(nombre, algo = "md5"))
          desc_str       <- html_element(doc, ".description, .sync-content") %>% html_text2()
          texto_completo <- paste(nombre, coalesce(desc_str, ""))
          img_url        <- html_element(doc, ".images-container img, img[src*='jpg'], img[src*='png']") %>% html_attr("src")
          vol_info       <- extraer_volumen_avanzado(texto_completo)
          
          df_prod <- tibble(
            id_ejecucion           = id_ejecucion,
            comercio               = tienda,
            fecha_hora_consulta    = fecha_iso,
            producto_raw           = str_trim(nombre),
            texto_original_anuncio = str_trim(texto_completo),
            precio_cop             = precio_val,
            es_promocion           = 0L,
            url_extraccion         = paste0("local://raw/supercomunal/", basename(f_path)),
            sku_codigo             = sku_codigo_val,
            marca_raw              = str_to_title(str_extract(nombre, "^\\w+")),
            subcategoria           = determinar_subcategoria(nombre),
            presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
            contenido_num          = vol_info$num,
            unidad_medida          = vol_info$und,
            comparable             = 0L,
            id_producto_comun      = NA_character_,
            marca_estandarizada    = str_to_upper(str_trim(str_extract(nombre, "^\\w+"))),
            precio_lista_cop       = precio_val,
            precio_por_ml          = NA_real_,
            precio_por_litro       = NA_real_,
            disponibilidad         = "En Stock",
            url_imagen             = ifelse(is.na(img_url), "", img_url)
          )
          resultados <- bind_rows(resultados, df_prod)
        } else {
          # Listado de productos
          nodos <- html_elements(doc, ".single-slide-item, .product_item")
          if (length(nodos) > 0) {
            df_f <- map_dfr(nodos, function(nodo) {
              nom <- html_element(nodo, "h4, .product-title, a[href*='.html']") %>% html_text2()
              if (is.na(nom) || str_trim(nom) == "" || str_detect(nom, PATRON_EXCLUSIONES)) return(NULL)
              
              p_str <- html_element(nodo, ".product-price .price, span.price, .price") %>% html_text2()
              p_val <- limpiar_precio_cop(p_str)
              v_info <- extraer_volumen_avanzado(paste(nom, html_text2(nodo)))
              
              id_attr <- html_attr(nodo, "data-id-product")
              sku_val <- if (!is.na(id_attr) && str_trim(id_attr) != "") paste0("SC-", str_trim(id_attr)) else paste0("SC-", digest(nom, algo = "md5"))
              
              tibble(
                id_ejecucion           = id_ejecucion,
                comercio               = tienda,
                fecha_hora_consulta    = fecha_iso,
                producto_raw           = str_trim(nom),
                texto_original_anuncio = html_text2(nodo),
                precio_cop             = p_val,
                es_promocion           = 0L,
                url_extraccion         = paste0("local://raw/supercomunal/", basename(f_path)),
                sku_codigo             = sku_val,
                marca_raw              = str_to_title(str_extract(nom, "^\\w+")),
                subcategoria           = determinar_subcategoria(nom),
                presentacion_raw       = if (!is.na(v_info$num)) paste(v_info$num, v_info$und) else NA_character_,
                contenido_num          = v_info$num,
                unidad_medida          = v_info$und,
                comparable             = 0L,
                id_producto_comun      = NA_character_,
                marca_estandarizada    = str_to_upper(str_trim(str_extract(nom, "^\\w+"))),
                precio_lista_cop       = p_val,
                precio_por_ml          = NA_real_,
                precio_por_litro       = NA_real_,
                disponibilidad         = "En Stock",
                url_imagen             = ""
              )
            })
            resultados <- bind_rows(resultados, df_f)
          }
        }
      }, error = function(e) NULL)
    }
    return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
  }
  
  # Modo Online Chromote
  b <- tryCatch({ preparar_sesion_chromote(modo_interactivo) }, error = function(e) return(NULL))
  if (is.null(b)) return(tibble())
  on.exit({ try(b$close(), silent = TRUE) }, add = TRUE)
  
  busquedas_comunal <- c("gaseosa", "jugo", "agua", "te", "energizante", "coca cola", "pepsi", "postobon", "hit")
  objetivos <- c("13-bebidas", busquedas_comunal)
  urls_productos_unicos <- character()
  
  for (obj in objetivos) {
    es_categoria <- (obj == "13-bebidas")
    for (pagina in 1:4) {
      url_target <- if (es_categoria) paste0("https://supermercadocomunal.com/13-bebidas?page=", pagina) else paste0("https://supermercadocomunal.com/jolisearch?s=", URLencode(obj), "&page=", pagina)
      tryCatch({
        b$Page$navigate(url_target)
        Sys.sleep(3)
        res <- b$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)
        html_content <- res$result$value
        if (is.null(html_content) || nchar(html_content) == 0) break
        
        doc <- read_html(html_content)
        todos_links <- html_elements(doc, "a") %>% html_attr("href")
        todos_links <- todos_links[!is.na(todos_links)]
        
        links_prod <- todos_links[str_detect(todos_links, "/\\d+-[a-z0-9-]+\\.html|\\d+\\.html")]
        links_prod <- links_prod[!str_detect(links_prod, "/13-bebidas|/jolisearch|/content/|/contactenos")]
        if (length(links_prod) == 0) break
        
        links_prod <- sapply(links_prod, function(l) if (str_starts(l, "http")) l else paste0("https://supermercadocomunal.com", if (str_starts(l, "/")) "" else "/", l), USE.NAMES = FALSE)
        urls_productos_unicos <- unique(c(urls_productos_unicos, links_prod))
      }, error = function(e) NULL)
    }
  }
  
  for (i in seq_along(urls_productos_unicos)) {
    p_url <- urls_productos_unicos[i]
    tryCatch({
      b$Page$navigate(p_url)
      Sys.sleep(2)
      res <- b$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE)
      html_content <- res$result$value
      if (is.null(html_content) || nchar(html_content) == 0) next
      
      writeLines(html_content, paste0("raw/supercomunal/prod_", round(id_ejecucion), "_", i, ".html"), useBytes = TRUE)
      doc <- read_html(html_content)
      
      nombre <- html_element(doc, "h3.product-title, h1.product-title, [class*='product-title'], h1") %>% html_text2()
      if (is.na(nombre) || str_trim(nombre) == "" || str_detect(nombre, PATRON_EXCLUSIONES)) next
      
      precio_elem <- html_element(doc, "[itemprop='price'], .current-price span[itemprop='price'], .sync-product-price .price, .current-price .price, .product-price .price, span.price")
      precio_str  <- if (!is.na(precio_elem)) html_attr(precio_elem, "content") else NA_character_
      if (is.na(precio_str) || str_trim(precio_str) == "") precio_str <- if (!is.na(precio_elem)) html_text2(precio_elem) else NA_character_
      
      precio_val <- limpiar_precio_cop(precio_str)
      sku_ref    <- html_element(doc, "[itemprop='sku'], .reference span, .reference") %>% html_text2()
      sku_clean  <- str_extract(sku_ref, "\\d+")
      if (is.na(sku_clean) || sku_clean == "") sku_clean <- str_extract(p_url, "(?<=/)\\d+(?=-)")
      
      sku_codigo_val <- if (!is.na(sku_clean) && sku_clean != "") paste0("SC-", sku_clean) else paste0("SC-", digest(nombre, algo = "md5"))
      desc_str       <- html_element(doc, ".description, .sync-content") %>% html_text2()
      texto_completo <- paste(nombre, coalesce(desc_str, ""))
      img_url        <- html_element(doc, ".images-container img, img[src*='jpg'], img[src*='png']") %>% html_attr("src")
      vol_info       <- extraer_volumen_avanzado(texto_completo)
      
      df_prod <- tibble(
        id_ejecucion           = id_ejecucion,
        comercio               = tienda,
        fecha_hora_consulta    = fecha_iso,
        producto_raw           = str_trim(nombre),
        texto_original_anuncio = str_trim(texto_completo),
        precio_cop             = precio_val,
        es_promocion           = 0L,
        url_extraccion         = p_url,
        sku_codigo             = sku_codigo_val,
        marca_raw              = str_to_title(str_extract(nombre, "^\\w+")),
        subcategoria           = determinar_subcategoria(nombre),
        presentacion_raw       = if (!is.na(vol_info$num)) paste(vol_info$num, vol_info$und) else NA_character_,
        contenido_num          = vol_info$num,
        unidad_medida          = vol_info$und,
        comparable             = 0L,
        id_producto_comun      = NA_character_,
        marca_estandarizada    = str_to_upper(str_trim(str_extract(nombre, "^\\w+"))),
        precio_lista_cop       = precio_val,
        precio_por_ml          = NA_real_,
        precio_por_litro       = NA_real_,
        disponibilidad         = "En Stock",
        url_imagen             = ifelse(is.na(img_url), "", img_url)
      )
      resultados <- bind_rows(resultados, df_prod)
    }, error = function(e) NULL)
  }
  return(if (nrow(resultados) > 0) distinct(resultados, sku_codigo, .keep_all = TRUE) else resultados)
}

# ------------------------------------------------------------------------------
# 4. HOMOLOGACIÓN Y ASIGNACIÓN DE PRODUCTOS COMPARABLES (2.4)
# ------------------------------------------------------------------------------
marcar_productos_comparables <- function(df_consolidado) {
  normalizar_texto <- function(txt) {
    if (is.na(txt)) return("")
    txt <- stri_trans_general(txt, "Latin-ASCII")
    txt <- str_to_upper(txt)
    txt <- str_replace_all(txt, "[^A-Z0-9 ]", " ")
    txt <- str_squish(txt)
    return(txt)
  }
  
  df_procesado <- df_consolidado %>%
    mutate(
      marca_clean = map_chr(marca_estandarizada, normalizar_texto),
      prod_clean  = map_chr(producto_raw, normalizar_texto),
      volumen_ml_std = case_when(
        str_to_lower(unidad_medida) %in% c("l", "lt", "lts", "litro", "litros") ~ contenido_num * 1000,
        TRUE ~ contenido_num
      ),
      variante_std = case_when(
        str_detect(prod_clean, "ZERO|SIN AZUCAR|CERO") ~ "ZERO",
        str_detect(prod_clean, "LIGHT") ~ "LIGHT",
        str_detect(prod_clean, "ORIGINAL|SABOR ORIGINAL") ~ "ORIGINAL",
        str_detect(prod_clean, "NARANJA") ~ "NARANJA",
        str_detect(prod_clean, "MORA") ~ "MORA",
        str_detect(prod_clean, "MANZANA") ~ "MANZANA",
        TRUE ~ "REGULAR"
      ),
      clave_producto = paste(marca_clean, subcategoria, variante_std, volumen_ml_std, sep = "_")
    )
  
  # Selección de productos presentes en >= 3 comercios
  comparables_info <- df_procesado %>%
    group_by(clave_producto, marca_clean, subcategoria, variante_std, volumen_ml_std) %>%
    summarise(
      n_comercios = n_distinct(comercio),
      lista_comercios = paste(sort(unique(comercio)), collapse = " | "),
      .groups = "drop"
    ) %>%
    filter(n_comercios >= 3) %>%
    arrange(clave_producto) %>%
    mutate(id_comun_asignado = sprintf("PROD_COM_%02d", row_number()))
  
  df_final <- df_procesado %>%
    left_join(
      comparables_info %>% select(clave_producto, id_comun_asignado),
      by = "clave_producto"
    ) %>%
    mutate(
      comparable = if_else(!is.na(id_comun_asignado), 1L, 0L),
      id_producto_comun = if_else(!is.na(id_comun_asignado), id_comun_asignado, NA_character_)
    ) %>%
    select(-marca_clean, -prod_clean, -volumen_ml_std, -variante_std, -clave_producto, -id_comun_asignado)
  
  return(list(df_final = df_final, comparables_info = comparables_info))
}

# ------------------------------------------------------------------------------
# 5. GESTIÓN BD SQLITE (STAGING TABLE E INSERT OR IGNORE)
# ------------------------------------------------------------------------------
inicializar_bd_sqlite <- function(db_path = "comparador_precios.sqlite") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ejecuciones (
      id_ejecucion REAL PRIMARY KEY,
      fecha_inicio TEXT,
      fecha_fin TEXT,
      total_registros INTEGER,
      estado TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS observaciones (
      id_ejecucion REAL,
      comercio TEXT,
      fecha_hora_consulta TEXT,
      producto_raw TEXT,
      texto_original_anuncio TEXT,
      precio_cop REAL,
      es_promocion INTEGER,
      url_extraccion TEXT,
      sku_codigo TEXT,
      marca_raw TEXT,
      subcategoria TEXT,
      presentacion_raw TEXT,
      contenido_num REAL,
      unidad_medida TEXT,
      comparable INTEGER,
      id_producto_comun TEXT,
      marca_estandarizada TEXT,
      precio_lista_cop REAL,
      precio_por_ml REAL,
      precio_por_litro REAL,
      disponibilidad TEXT,
      url_imagen TEXT,
      PRIMARY KEY (comercio, sku_codigo, fecha_hora_consulta)
    )
  ")
}

guardar_en_sqlite <- function(df, db_path = "comparador_precios.sqlite") {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  df_limpio <- df %>% distinct(comercio, sku_codigo, fecha_hora_consulta, .keep_all = TRUE)
  dbWriteTable(con, "temp_staging", df_limpio, overwrite = TRUE)
  
  cols_destino <- dbListFields(con, "observaciones")
  cols_origen  <- dbListFields(con, "temp_staging")
  cols_comunes <- intersect(cols_destino, cols_origen)
  cols_str     <- paste(cols_comunes, collapse = ", ")
  
  query <- sprintf("INSERT OR IGNORE INTO observaciones (%s) SELECT %s FROM temp_staging", cols_str, cols_str)
  
  dbExecute(con, query)
  dbRemoveTable(con, "temp_staging")
}

# ------------------------------------------------------------------------------
# 6. ORQUESTADOR PRINCIPAL (ejecutar_scraper_maestro)
# ------------------------------------------------------------------------------
ejecutar_scraper_maestro <- function(modo_offline = TRUE, modo_interactivo = FALSE, db_path = "comparador_precios.sqlite") {
  id_ejecucion_num <- as.numeric(Sys.time())
  fecha_inicio     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
  
  cat("\n==================================================================\n")
  cat("🚀 INICIANDO MÓDULO DE EXTRACCIÓN KDD (4 COMERCIOS)\n")
  cat("ID Ejecución:", id_ejecucion_num, "\n")
  cat("Modo de Operación:", ifelse(modo_offline, "OFFLINE (Carga desde raw/)", "ONLINE (Captura Web en Vivo)"), "\n")
  cat("==================================================================\n\n")
  
  inicializar_bd_sqlite(db_path)
  
  # Extracción en las 4 tiendas
  df_exito      <- scrapear_exito(id_ejecucion_num, fecha_inicio, modo_interactivo, modo_offline)
  df_carulla    <- scrapear_carulla(id_ejecucion_num, fecha_inicio, modo_interactivo, modo_offline)
  df_pricesmart <- scrapear_pricesmart(id_ejecucion_num, fecha_inicio, modo_interactivo, modo_offline)
  df_comunal    <- scrapear_supercomunal(id_ejecucion_num, fecha_inicio, modo_interactivo, modo_offline)
  
  df_raw_total <- bind_rows(df_exito, df_carulla, df_pricesmart, df_comunal)
  
  if (nrow(df_raw_total) == 0 && file.exists("df_resultado_final.rds")) {
    cat("ℹ️ Sin archivos nuevos en raw. Cargando contenedor respaldado 'df_resultado_final.rds'...\n")
    datos_rds <- readRDS("df_resultado_final.rds")
    return(datos_rds$df_resultado_final)
  }
  
  # Homologación y marcado de comparables
  res_comparables <- marcar_productos_comparables(df_raw_total)
  df_consolidado  <- res_comparables$df_final
  info_comp       <- res_comparables$comparables_info
  
  # Métricas unitarias
  df_consolidado <- df_consolidado %>%
    distinct(comercio, sku_codigo, fecha_hora_consulta, .keep_all = TRUE) %>%
    mutate(
      precio_por_ml = case_when(
        is.na(precio_cop) | is.na(contenido_num) | contenido_num == 0 ~ NA_real_,
        unidad_medida == "ml" ~ round(precio_cop / contenido_num, 4),
        unidad_medida == "L"  ~ round(precio_cop / (contenido_num * 1000), 4),
        TRUE ~ NA_real_
      ),
      precio_por_litro = if_else(!is.na(precio_por_ml), round(precio_por_ml * 1000, 2), NA_real_)
    )
  
  # Guardado en SQLite, CSV y RDS
  file_csv <- paste0("datos_consolidados_", round(id_ejecucion_num), ".csv")
  write_csv(df_consolidado, file_csv)
  guardar_en_sqlite(df_consolidado, db_path)
  
  matriz_comp <- df_consolidado %>%
    filter(comparable == 1) %>%
    count(id_producto_comun, producto_raw, comercio) %>%
    pivot_wider(names_from = comercio, values_from = n, values_fill = 0)
  
  resumen_na <- df_consolidado %>%
    map_df(~ sum(is.na(.x))) %>%
    pivot_longer(everything(), names_to = "columna", values_to = "n_na") %>%
    mutate(pct_na = round((n_na / nrow(df_consolidado)) * 100, 2)) %>%
    filter(n_na > 0)
  
  saveRDS(
    list(
      df_resultado_final         = df_consolidado,
      productos_comparables_info = info_comp,
      matriz_comparables         = matriz_comp,
      resumen_na                 = resumen_na
    ),
    file = "df_resultado_final.rds"
  )
  
  fecha_fin <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
  con <- dbConnect(SQLite(), db_path)
  try({
    dbExecute(con, 
              "INSERT OR IGNORE INTO ejecuciones (id_ejecucion, fecha_inicio, fecha_fin, total_registros, estado) VALUES (?, ?, ?, ?, ?)", 
              list(id_ejecucion_num, fecha_inicio, fecha_fin, nrow(df_consolidado), "COMPLETADA")
    )
  }, silent = TRUE)
  dbDisconnect(con)
  
  cat("\n==================================================================\n")
  cat("✅ EXTRACCIÓN Y HOMOLOGACIÓN COMPLETA\n")
  cat("Total de observaciones recolectadas:", nrow(df_consolidado), "\n")
  cat("Total de productos comparables únicos (>=3 comercios):", n_distinct(df_consolidado$id_producto_comun, na.rm = TRUE), "\n")
  cat("Total de observaciones comparables (comparable = 1):", sum(df_consolidado$comparable), "\n")
  cat("==================================================================\n")
  
  return(df_consolidado)
}

# ------------------------------------------------------------------------------
# MODO DE EJECUCIÓN (Por defecto en modo offline)
# ------------------------------------------------------------------------------
# df_resultado <- ejecutar_scraper_maestro(modo_offline = TRUE)

# ------------------------------------------------------------------------------
# Llamado de base de datos guardada para evitar demora de 30 min
# ------------------------------------------------------------------------------
# saveRDS(df_resultado, file = "df_resultado.rds")
 df_resultado <- readRDS("df_resultado.rds")





# ==============================================================================
# AUDITORÍA DE DATOS, FILTRADO Y VALIDACIÓN DE COMPARABLES (FASE KDD)
# ARCHIVO: evaluar_y_limpiar_datos.R
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 1. CARGA DE OBSERVACIONES DESDE EL ENTORNO O DISCO
# ------------------------------------------------------------------------------
if (exists("df_resultado")) {
  df_input <- df_resultado
} else if (file.exists("df_resultado_final.rds")) {
  datos_rds <- readRDS("df_resultado_final.rds")
  df_input <- if (is.list(datos_rds) && "df_resultado_final" %in% names(datos_rds)) datos_rds$df_resultado_final else datos_rds
} else if (file.exists("df_resultado.rds")) {
  df_input <- readRDS("df_resultado.rds")
} else {
  stop("❌ No se encontró 'df_resultado' en memoria ni los archivos RDS en disco.")
}

cat("📊 Observaciones totales a evaluar:", nrow(df_input), "\n")

# ------------------------------------------------------------------------------
# 2. EVALUACIÓN Y DIAGNÓSTICO DE VALORES FALTANTES (NAs)
# ------------------------------------------------------------------------------
resumen_na <- df_input %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "n_na") %>%
  mutate(
    pct_na   = round((n_na / nrow(df_input)) * 100, 2),
    tiene_na = n_na > 0
  ) %>%
  arrange(desc(n_na))

cat("\n=== 1. DIAGNÓSTICO DE VALORES FALTANTES (NAs) ===\n")
print(resumen_na)

# ------------------------------------------------------------------------------
# 3. CONTROL DE CALIDAD: NO-BEBIDAS Y ELIMINACIÓN DE NAs (EXCEPTO id_producto_comun)
# ------------------------------------------------------------------------------

df_depurado <- df_input %>%
  filter(!is.na(precio_cop) & precio_cop > 0) %>%
  # 3. ELIMINAR NAs EN TODAS LAS COLUMNAS EXCEPTO EN id_producto_comun
  filter(if_all(-id_producto_comun, ~ !is.na(.))) %>%
  # 4. Eliminar duplicados exactos por comercio y SKU
  distinct(comercio, sku_codigo, fecha_hora_consulta, .keep_all = TRUE)

cat("\n=== 2. RESULTADO DE LA DEPURACIÓN Y FILTRADO DE NAs ===\n")
cat("Observaciones iniciales:", nrow(df_input), "\n")
cat("Observaciones eliminadas (NAs en métricas/no-bebidas):", nrow(df_input) - nrow(df_depurado), "\n")
cat("Observaciones depuradas finales:", nrow(df_depurado), "\n")

# ------------------------------------------------------------------------------
# 4. VALIDACIÓN DE CORRESPONDENCIA Y ASIGNACIÓN DE PRODUCTOS COMPARABLES
# ------------------------------------------------------------------------------
normalizar_cadena <- function(txt) {
  if (is.na(txt)) return("")
  txt <- stri_trans_general(txt, "Latin-ASCII")
  txt <- str_to_upper(txt)
  txt <- str_replace_all(txt, "[^A-Z0-9 ]", " ")
  txt <- str_squish(txt)
  return(txt)
}

df_evaluado <- df_depurado %>%
  mutate(
    marca_clean = map_chr(marca_estandarizada, normalizar_cadena),
    prod_clean  = map_chr(producto_raw, normalizar_cadena),
    
    volumen_ml_std = case_when(
      str_to_lower(unidad_medida) %in% c("l", "lt", "lts", "litro", "litros") ~ contenido_num * 1000,
      TRUE ~ contenido_num
    ),
    
    variante_std = case_when(
      str_detect(prod_clean, "ZERO|SIN AZUCAR|CERO") ~ "ZERO",
      str_detect(prod_clean, "LIGHT") ~ "LIGHT",
      str_detect(prod_clean, "ORIGINAL|SABOR ORIGINAL") ~ "ORIGINAL",
      str_detect(prod_clean, "NARANJA") ~ "NARANJA",
      str_detect(prod_clean, "MORA") ~ "MORA",
      str_detect(prod_clean, "MANZANA") ~ "MANZANA",
      TRUE ~ "REGULAR"
    ),
    
    clave_producto = paste(marca_clean, subcategoria, variante_std, volumen_ml_std, sep = "_")
  )

# Agrupar e identificar únicamente los productos presentes en >= 3 comercios
productos_comparables_info <- df_evaluado %>%
  group_by(clave_producto, marca_clean, subcategoria, variante_std, volumen_ml_std) %>%
  summarise(
    n_comercios = n_distinct(comercio),
    lista_comercios = paste(sort(unique(comercio)), collapse = " | "),
    total_observaciones = n(),
    .groups = "drop"
  ) %>%
  filter(n_comercios >= 3) %>%
  arrange(clave_producto) %>%
  mutate(id_comun_asignado = sprintf("PROD_COM_%02d", row_number()))

# Asignación final al dataframe
df_resultado_final <- df_evaluado %>%
  left_join(
    productos_comparables_info %>% select(clave_producto, id_comun_asignado),
    by = "clave_producto"
  ) %>%
  mutate(
    comparable = if_else(!is.na(id_comun_asignado), 1L, 0L),
    id_producto_comun = if_else(!is.na(id_comun_asignado), id_comun_asignado, NA_character_)
  ) %>%
  select(-marca_clean, -prod_clean, -volumen_ml_std, -variante_std, -clave_producto, -id_comun_asignado)

# Matriz de coincidencia
matriz_comparables <- df_resultado_final %>%
  filter(comparable == 1) %>%
  count(id_producto_comun, producto_raw, comercio) %>%
  pivot_wider(names_from = comercio, values_from = n, values_fill = 0)

cat("\n=== 3. PRODUCTOS COMPARABLES METODOLÓGICAMENTE VALIDADOS ===\n")
cat("Total de productos comparables únicos (>= 3 comercios):", nrow(productos_comparables_info), "\n")
cat("Total de observaciones marcadas como comparables (comparable = 1):", sum(df_resultado_final$comparable), "\n")

# ------------------------------------------------------------------------------
# 5. GUARDADO DEL PAQUETE COMPLETO EN ARCHIVO RDS
# ------------------------------------------------------------------------------
saveRDS(
  list(
    df_resultado_final         = df_resultado_final,
    productos_comparables_info = productos_comparables_info,
    matriz_comparables         = matriz_comparables,
    resumen_na                 = resumen_na
  ),
  file = "df_resultado_final.rds"
)
