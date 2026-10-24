# =======================================================================
# CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
# Propósito: Prepara el entorno de trabajo verificando la disponibilidad 
#            del navegador Chrome (necesario para chromote), configurando 
#            el directorio de trabajo e instalando/cargando los paquetes 
#            requeridos dinámicamente.
# =======================================================================
chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)
options(chromote.timeout=60)
#setwd("brayan_camilo_moreno_villalobos/Parcial_01") 

paquetes <- c("rvest", "xml2", "dplyr", "stringr", "purrr", "tibble", "janitor", "readr", "DBI", "RSQLite", "stringi")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) install.packages(pendientes)
invisible(lapply(paquetes, library, character.only = TRUE))

# ==============================================================================
# PARÁMETROS Y DICCIONARIO DE SUPERMERCADOS 
# ==============================================================================


config_sitios <- list(
  
  olimpica = list(
    nombre = "Olimpica",
    url_template = "https://www.olimpica.com/supermercado/para-tu-mascota",
    param_paginacion = "?page=",
    selector_item = "section.vtex-product-summary-2-x-container", 
    selector_nombre = "span.vtex-product-summary-2-x-productBrand",
    selector_precio = "span.olimpica-dinamic-flags-0-x-currencyContainer",
    selector_marca = "span.vtex-product-summary-2-x-productBrandName"
  ),
  exito = list(
    nombre = "Exito",
    url_template = "https://www.exito.com/mercado/mascotas?category-1=mercado&category-2=mascotas&facets=category-1%2Ccategory-2&sort=score_desc",
    param_paginacion = "&page=",
    selector_item = "div.product-grid_fs-product-grid___qKN2 ul li",
    selector_nombre = "h3",
    selector_precio = "div.ProductPrice_container__JKbri  p",
    selector_marca = "no existe selector" # en este comercio no existe este selector
  ),
  jumbo = list(
    nombre = "Jumbo",
    url_template = "https://www.jumbocolombia.com/co/mascotas",
    param_paginacion = "?page=",
    selector_item = "div.tiendasjumboqaio-custom-search-0-x-product__container ",
    selector_nombre = "h3",
    selector_precio = 'span.jumbo-ds-ProductCardstyles-1szk4on',
    selector_marca = "p.jumbo-ds-ProductCardstyles-1szk4om"
  )
)
# =======================================================================
# Función: extraer_texto_seguro
# Propósito: Extrae el texto de un nodo HTML de forma segura, evitando 
#            que selectores inexistentes, nulos o nodos vacíos rompan 
#            la ejecución del bucle principal.
# 
# Parámetros:
#   - nodo (xml_node): El fragmento HTML del cual se extraerá la información.
#   - selector (character): El selector CSS que apunta al dato específico.
#
# Retorna:
#   - character: El texto extraído limpio de espacios, o NA_character_ 
#                si la extracción falla o el selector es inválido.
# =======================================================================
extraer_texto_seguro <- function(nodo, selector) {
  if (is.na(selector) || selector == "no existe selector") return(NA_character_)
  texto <- html_element(nodo, selector) %>% html_text(trim = TRUE)
  if (length(texto) == 0 || is.na(texto)) return(NA_character_)
  return(texto)
}
# =======================================================================
# Función: .motor_scraping (Uso interno)
# Propósito: Motor core que gestiona la recolección de datos, la paginación,
#            el manejo del DOM y la lógica de respaldo (fallback recursivo) 
#            entre los modos Offline y Online.
# 
# Parámetros:
#   - sitio_cfg (list): Diccionario con la configuración del comercio (URLs, CSS).
#   - max_paginas (numeric): Límite de iteraciones para evitar bucles infinitos.
#   - modo_offline (logical): Define si lee archivos locales (TRUE) o usa Chrome (FALSE).
#   - fecha_offline (character): Fecha de formato "YYYYMMDD" para buscar archivos locales.
#
# Retorna:
#   - data.frame: Tabla unificada con los datos crudos extraídos de un comercio.
# =======================================================================
.motor_scraping <- function(sitio_cfg, max_paginas , modo_offline = FALSE, fecha_offline = NULL) {
  ruta_raw <- file.path("raw")
  fecha_hoy <- format(Sys.Date(), "%Y%m%d")
  resultados_comercio <- list()
  
  # ==========================================
  # MODO SIN CONEXIÓN (OFFLINE)
  # ==========================================
  if (modo_offline) {
    message("--> [OFFLINE] Extrayendo ", sitio_cfg$nombre, " desde archivos locales...")
    
    if (is.null(fecha_offline)) {
      
      todos_archivos <- list.files(ruta_raw, pattern = paste0("^", sitio_cfg$nombre, "_"), full.names = FALSE)
      fechas_disponibles <- stringr::str_extract(todos_archivos, "\\d{8}") 
      fechas_validas <- fechas_disponibles[!is.na(fechas_disponibles)] #usamos recursividad para el caso en el que no hay documentos html, o no tienen un formato correcto
      if (length(todos_archivos) == 0 || length(fechas_validas) == 0) {
        message("    ⚠️ No se encontraron copias crudas válidas o con fecha para ", sitio_cfg$nombre, ".")
        message("    🔄 Cambiando automáticamente a modo ONLINE...")
        
        # Llamamos a la misma función pero forzando modo_offline = FALSE
        return(.motor_scraping(sitio_cfg, max_paginas, modo_offline = FALSE))
      }
      
      fecha_busqueda <- max(fechas_disponibles, na.rm = TRUE) # Tomamos la mayor (más reciente)
      
      message("    ℹ️ No se especificó fecha. Usando la más reciente encontrada: ", fecha_busqueda)
    } else {
      fecha_busqueda <- fecha_offline
    }
    
    patron_busqueda <- paste0("^", sitio_cfg$nombre, "_", fecha_busqueda, "_pag_\\d+\\.html$")
    archivos_locales <- list.files(ruta_raw, pattern = patron_busqueda, full.names = TRUE)
    
    if (length(archivos_locales) == 0) {
      stop("No se encontraron archivos crudos para ", sitio_cfg$nombre, " en la fecha ", fecha_busqueda)
    }
    
    for (archivo in archivos_locales) {
      
      pagina_estatica <- rvest::read_html(archivo)
      nodos <- html_elements(pagina_estatica, sitio_cfg$selector_item)
      pag_num <- stringr::str_extract(archivo, "(?<=pag_)\\d+")
      url_paginada <- paste0(sitio_cfg$url_template, sitio_cfg$param_paginacion, pag_num)
      df_pagina <- map_dfr(nodos, function(nodo) {
        nombre <- extraer_texto_seguro(nodo, sitio_cfg$selector_nombre)
        precio <- extraer_texto_seguro(nodo, sitio_cfg$selector_precio)
        marca <- extraer_texto_seguro(nodo, sitio_cfg$selector_marca)
        tibble(
          precio = precio,
          supermercado = sitio_cfg$nombre,
          pagina_origen = url_paginada,
          presentacion = nombre,
          marca = marca,
          fecha_consulta = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z") 
        )
      })
      resultados_comercio[[length(resultados_comercio) + 1]] <- df_pagina
    }
    df_final <- dplyr::bind_rows(resultados_comercio)
    
    return(df_final)
    # ==========================================
    # MODO CON CONEXIÓN (ONLINE)
    # ==========================================
  } else {
    message("--> [ONLINE] Conectando a ", sitio_cfg$nombre, " y extrayendo ...")
    
    for (pag in 1:max_paginas) {
      if (sitio_cfg$nombre=="Olímpica"){pag_a<- pag}else{pag_a <- pag-1} 
      url_paginada <- paste0(sitio_cfg$url_template, sitio_cfg$param_paginacion, pag_a)
      
      Sys.sleep(runif(1, 4, 7))
      
      Sys.sleep(4)
      sess <- tryCatch({
        pagina_viva <- rvest::read_html_live(url_paginada)
        
        pagina_viva$session$Emulation$setDeviceMetricsOverride(
          width = 1920L,
          height = 5000L,
          deviceScaleFactor = 1,
          mobile = FALSE
        )
        Sys.sleep(4)
        pagina_viva$session$Runtime$evaluate("window.scrollBy(0, 600);")
        js_script <- paste0("
              document.querySelectorAll('", sitio_cfg$selector_item, "').forEach(el => {
                el.dispatchEvent(new MouseEvent('mouseover', {bubbles: true}));
                el.dispatchEvent(new MouseEvent('mousemove', {bubbles: true}));
              });
              ")
        pagina_viva$session$Runtime$evaluate(js_script)
        pagina_viva
      }, error = function(e) { return(NULL) }) 
      
      if (is.null(sess)) {
        message("    ⚠️ Error de sesión en página ", pag, ". Saltando...")
        next
        return(tibble())
      }
      
      tryCatch({
        doc_root <- pagina_viva$session$DOM$getDocument()
        html_string <- pagina_viva$session$DOM$getOuterHTML(nodeId = doc_root$root$nodeId)$outerHTML
        
        nombre_archivo <- paste0(sitio_cfg$nombre, "_", fecha_hoy, "_pag_", pag, ".html")
        writeLines(text = html_string, con = file.path(ruta_raw, nombre_archivo), useBytes = TRUE)
      }, error = function(e) {
      })
      
      nodos <- tryCatch({
        html_elements(pagina_viva, sitio_cfg$selector_item)
      }, error = function(e) {
        return(list()) 
      })
      
      if (length(nodos) == 0) {
        # Silenciamos el mensaje de fin de nodos
        break 
      }
      # message(paste("    ✓ Nodos capturados:", length(nodos))) # Silenciado
      
      df_pagina <- map_dfr(nodos, function(nodo) {
        nombre <- extraer_texto_seguro(nodo, sitio_cfg$selector_nombre)
        precio <- extraer_texto_seguro(nodo, sitio_cfg$selector_precio)
        marca <- extraer_texto_seguro(nodo, sitio_cfg$selector_marca)
        
        tibble(
          precio = precio,
          supermercado = sitio_cfg$nombre,
          pagina_origen = url_paginada,
          presentacion = nombre,
          marca = marca,
          fecha_consulta = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z") 
        )
      })
      nombres_limpios <- trimws(df_pagina$presentacion)
      nombres_validos <- nombres_limpios[!is.na(nombres_limpios) & nombres_limpios != ""]
      
      if (length(nombres_validos) == 0) {
        break 
      }
      
      resultados_comercio[[length(resultados_comercio) + 1]] <- df_pagina
    }
    
    
    df_final_comercio <- dplyr::bind_rows(resultados_comercio)
    
    
    total_paginas_extraidas <- length(resultados_comercio)
    total_productos <- nrow(df_final_comercio)
    

    message("--> [", sitio_cfg$nombre, "] Extracción finalizada: ", 
            total_paginas_extraidas, " páginas recorridas, ", 
            total_productos, " productos obtenidos.")
    
    return(df_final_comercio)
  }
}

# =======================================================================
# Funciones Envolventes (Wrappers): scrapear_olimpica, scrapear_exito, scrapear_jumbo
# Propósito: Inyectan la configuración específica de cada supermercado al 
#            motor principal sin necesidad de reescribir código.
# 
# Parámetros compartidos:
#   - max_paginas (numeric), offline (logical), fecha (character).
# =======================================================================

scrapear_olimpica <- function(max_paginas , offline = FALSE, fecha = NULL) {
  .motor_scraping(config_sitios$olimpica, max_paginas, modo_offline = offline, fecha_offline = fecha)
}
scrapear_exito <- function(max_paginas , offline = FALSE, fecha = NULL) {
  .motor_scraping(config_sitios$exito, max_paginas, modo_offline = offline, fecha_offline = fecha)
}
scrapear_jumbo <- function(max_paginas , offline = FALSE, fecha = NULL) {
  .motor_scraping(config_sitios$jumbo, max_paginas, modo_offline = offline, fecha_offline = fecha)
}



# =======================================================================
# Función: ejecutar_recoleccion_completa
# Propósito: Orquesta la recolección en bloque de todos los supermercados
#            definidos, ejecutándolos secuencialmente y unificando los 
#            resultados en un solo consolidado.
# 
# Parámetros:
#   - max_paginas (numeric): Límite global de iteración por comercio (por defecto 55).
#   - modo_offline (logical): Interruptor maestro para extracción local vs en vivo.
#   - fecha_offline (character): Fecha específica para el modo sin conexión.
#
# Retorna:
#   - data.frame (tibble): Base de datos unificada (df_consolidado) que contiene 
#                          las extracciones crudas de todos los comercios.
# =======================================================================
ejecutar_recoleccion_completa <- function(max_paginas = 55, 
                                          modo_offline = FALSE, 
                                          fecha_offline = NULL) {
  
  message("Iniciando recolección de datos...")
  
  # 1. Extracción por cada comercio
  datos_exito    <- scrapear_exito(max_paginas, offline = modo_offline, fecha = fecha_offline)
  datos_olimpica <- scrapear_olimpica(max_paginas, offline = modo_offline, fecha = fecha_offline)
  datos_jumbo    <- scrapear_jumbo(max_paginas, offline = modo_offline, fecha = fecha_offline)
  
  # 2. Unir todo en una sola tabla cruda
  df_crudo <- dplyr::bind_rows(datos_exito, datos_olimpica, datos_jumbo)
  
  message("Recolección finalizada. Total registros crudos: ", nrow(df_crudo))
  
  # Devolvemos la tabla intacta al Quarto
  return(df_crudo)
}
# =============================================================================
# Función: limpiar_datos
# Propósito: Transforma los datos crudos extraídos mediante web scraping, 
#            estandarizando textos y convirtiendo precios a formato numérico.
# 
# Parámetros:
#   - df_crudo (data.frame/tibble): La tabla unificada con los datos 
#     crudos de los tres supermercados.
#
# Retorna:
#   - data.frame: Una tabla filtrada y limpia lista para ser insertada 
#     en SQLite, con precios numéricos y marcas homologadas.
# =============================================================================

limpiar_datos <- function(df_crudo, id_ejecucion) {
  
  if (nrow(df_crudo) == 0) {
    warning("El dataframe está vacío. No hay datos para limpiar.")
    return(df_crudo)
  }
  
  marcas_conocidas <- c("CHUNKY", "DOGOURMET", "AGILITY GOLD", "MONELLO", 
                        "MIRRINGO", "RINGO", "TASTE OF THE WILD", "PEDIGREE", 
                        "DOG CHOW", "CAT CHOW", "WHISKAS", "FELIX", 
                        "ROYAL CANIN", "HILL'S", "NUTRECAN", "MAX", "BIOCAT", 
                        "CALABAZA", "BENTYSANDY", "MICHIKO", "PUKI", "DONKAT", "DONCAN")
  patron_marcas <- paste(marcas_conocidas, collapse = "|")
  
  df_procesado <- df_crudo %>%
    dplyr::mutate(
      `Texto original del anuncio` = presentacion,
      
      presentacion_limpia = stringr::str_squish(stringi::stri_trans_general(stringr::str_to_lower(presentacion), "Latin-ASCII")),
      marca_limpia = stringr::str_squish(stringi::stri_trans_general(stringr::str_to_upper(marca), "Latin-ASCII")),
      
      marca_limpia = dplyr::if_else(
        is.na(marca_limpia),
        stringr::str_extract(stringr::str_to_upper(presentacion_limpia), patron_marcas),
        marca_limpia
      ),
      presentacion_limpia = stringr::str_replace_all(presentacion_limpia, "x\\s*([0-9]+)", " \\1"),
      
      marca_limpia = tidyr::replace_na(marca_limpia, "OTRA/SIN MARCA"),
      
      cantidad_unidad = stringr::str_extract(presentacion_limpia,"[0-9]+[.,]?[0-9]*\\s*(kg|Kg|kG|KG|G|g|GR|Gr|gR|gr|LB|Lb|lB|lb|ML|Ml|mL|ml|l|L)\\b" ),
      cantidad_unidad = stringr::str_replace(cantidad_unidad, "([0-9.,]+)\\s*(GR|Gr|gR|gr)$", "\\1 gr"),
      cantidad_unidad = stringr::str_replace(cantidad_unidad, "([0-9.,]+)\\s*(kg|Kg|kG|K)$", "\\1 kg"),
      cantidad_unidad = stringr::str_replace(cantidad_unidad, "([0-9.,]+)\\s*(LB|Lb|lB|lb)$", "\\1 lb"),
      cantidad_unidad = stringr::str_replace(cantidad_unidad, "([0-9.,]+)\\s*(ML|Ml|mL|ml)$", "\\1 ml"),
      cantidad_unidad = stringr::str_replace(cantidad_unidad, "([0-9.,]+)\\s*(l|L)$", "\\1 l"),
      
      # ==============================================================
      # NUEVO: CONVERSIÓN MATEMÁTICA ESTANDARIZADA (kg a gr, l a ml)
      # ==============================================================
      
      # 1. Separamos el número de la unidad
      num_temp = as.numeric(stringr::str_replace(stringr::str_extract(cantidad_unidad, "[0-9.,]+"), ",", ".")),
      uni_temp = stringr::str_extract(stringr::str_to_lower(cantidad_unidad), "[a-z]+"),
      
      # 2. Hacemos la multiplicación según la unidad
      num_temp = dplyr::case_when(
        uni_temp == "kg" ~ num_temp * 1000,
        uni_temp == "lb" ~ num_temp * 500, 
        uni_temp == "l"  ~ num_temp * 1000,
        TRUE ~ num_temp
      ),
      
      # 3. Unificamos la etiqueta final (todo queda en 'gr' o 'ml')
      uni_temp = dplyr::case_when(
        uni_temp %in% c("kg", "g", "gr", "lb") ~ "gr",
        uni_temp %in% c("l", "ml") ~ "ml",
        TRUE ~ uni_temp
      ),
      
      # 4. Volvemos a unir el número y la etiqueta en la variable cantidad_unidad
      cantidad_unidad = dplyr::if_else(!is.na(num_temp), paste(num_temp, uni_temp), cantidad_unidad),
      
      # ==============================================================
      
      precio_limpio = stringr::str_remove_all(precio, "[^0-9,]"),
      precio_limpio = stringr::str_replace(precio_limpio, ",", "."),
      precio_numerico = as.numeric(precio_limpio),
      
      Comercio = supermercado,
      `Marca y subcategoría` = marca_limpia,
      Presentación = cantidad_unidad, # Aquí ya entra matemáticamente estandarizada ("1000 gr")
      Precio = precio_numerico,
      Promoción = 0, 
      `URL de extracción` = pagina_origen,
      `Fecha y hora de consulta` = fecha_consulta,
      Ejecución = id_ejecucion,
      
      Producto = NA_character_,
      Comparable = 0
    ) %>%
    dplyr::filter(!is.na(Precio)) %>%
    
    # Este select elimina automáticamente num_temp, uni_temp y demás variables sueltas
    dplyr::select(
      Comercio,
      Producto,
      `Marca y subcategoría`,
      Presentación,
      Precio,
      Promoción,
      `URL de extracción`,
      `Texto original del anuncio`,
      `Fecha y hora de consulta`,
      Comparable,
      Ejecución
    )
  
  return(df_procesado)
}