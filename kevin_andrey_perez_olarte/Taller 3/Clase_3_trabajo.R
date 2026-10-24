# 1. Cargar las librerías necesarias
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidyr)

# 2. Definir parámetros de búsqueda
cervezas <- c("michelob", "stella artois", "club colombia")
supermercados <- c("exito", "carulla", "jumbo")

# 3. Función para construir la URL de búsqueda (Ajustada a estructuras de e-commerce)
construir_url <- function(super, cerveza) {
  termino <- str_replace_all(cerveza, " ", "%20")
  if (super == "exito") return(paste0("https://www.exito.com/s?q=cerveza%20", termino))
  if (super == "carulla") return(paste0("https://www.carulla.com/s?q=cerveza%20", termino))
  if (super == "jumbo") return(paste0("https://www.tiendasjumbo.co/s?q=cerveza%20", termino))
}

# 4. Función principal de Scraping con read_html_live y manejo de errores
scraping_supermercados <- function(super, cerveza) {
  url <- construir_url(super, cerveza)
  cat("Consultando:", super, "-", cerveza, "\n")
  
  # Pausa obligatoria entre solicitudes para no saturar los servidores
  Sys.sleep(5)
  
  # tryCatch evita que el proceso se detenga si una página falla o rechaza la conexión
  resultado <- tryCatch({
    
    # Abrimos sesión dinámica (navegador en segundo plano)
    # read_html_live() se encarga de ejecutar el JavaScript
    sesion <- read_html_live(url)
    
    # Pausa adicional para darle tiempo a la página web de "armar" el catálogo
    Sys.sleep(5)
    
    # ⚠️ NOTA: Selectores genéricos. En la vida real, debes hacer "Inspeccionar" 
    # en la página para ajustarlos a la estructura actual del sitio.
    nombres <- sesion %>% 
      html_elements("h2, h3, .vtex-product-summary-2-x-nameContainer, .product-name") %>% 
      html_text2()
    
    precios <- sesion %>% 
      html_elements(".vtex-product-price-1-x-sellingPrice, .price, .product-price") %>% 
      html_text2()
    
    # Aseguramos que tengan la misma longitud antes de unir
    n_items <- min(length(nombres), length(precios))
    
    if (n_items == 0) {
      cat("   -> No se encontraron productos o faltó cargar JS.\n")
      return(tibble())
    }
    
    tibble(
      supermercado = super,
      marca_buscada = cerveza,
      presentacion = nombres[1:n_items],
      precio_texto = precios[1:n_items]
    )
    
  }, error = function(e) {
    cat("   -> Error de red o página en", super, ":", e$message, "\n")
    return(tibble()) # Devuelve tabla vacía para no dañar el loop general
  })
  
  return(resultado)
}

# 5. Ejecutar el scraping masivo cruzando supermercados y cervezas
# expand_grid crea todas las combinaciones posibles (9 iteraciones)
grid_busqueda <- expand_grid(super = supermercados, cerveza = cervezas)

# map2_dfr recorre las combinaciones y pega los resultados hacia abajo (bind_rows)
datos_crudos <- map2_dfr(grid_busqueda$super, grid_busqueda$cerveza, scraping_supermercados)

# 6. Preprocesamiento, limpieza de texto y Expresiones Regulares
datos_limpios <- datos_crudos %>%
  # A. Filtrar resultados irrelevantes (ej. llantas Michelin)
  filter(
    str_detect(tolower(presentacion), "cerveza|michelob|stella|club"),
    !str_detect(tolower(presentacion), "llanta|neumático|rin|michelin")
  ) %>%
  # B. Limpiar precios (quitar signo $, puntos y convertir a número matemático)
  mutate(
    precio_total = as.numeric(str_remove_all(precio_texto, "[^0-9]"))
  ) %>%
  # C. Expresión regular para volumen (ej: extrae "330" de "Cerveza Lata 330 ml")
  mutate(
    vol_numero = as.numeric(str_extract(tolower(presentacion), "\\d+(?=\\s*(ml|cm3|cc))")),
    
    # D. Detectar si es un six-pack o paca (ej: "x 6", "6 und")
    multiplicador = str_extract(tolower(presentacion), "(\\d+)\\s*(und|unidades|pack|x)"),
    multiplicador_num = as.numeric(str_extract(multiplicador, "\\d+")),
    # Si no tiene multiplicador, asumimos que es 1 unidad
    multiplicador_num = ifelse(is.na(multiplicador_num), 1, multiplicador_num), 
    
    # E. Calcular precio estandarizado (Precio por mililitro)
    volumen_total_ml = vol_numero * multiplicador_num,
    precio_por_ml = precio_total / volumen_total_ml
  ) %>%
  # Filtramos errores donde no se pudo detectar volumen (datos sucios)
  filter(!is.na(precio_por_ml)) 

# 7. Guardar resultados
write_csv(datos_limpios, "precios_cervezas.csv")
cat("\n¡Scraping finalizado! Datos guardados en precios_cervezas.csv\n")
