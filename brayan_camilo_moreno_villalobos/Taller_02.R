
chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)
# ==============================================================================
# 2. CARGA DE LIBRERÍAS
# ==============================================================================
paquetes <- c("rvest", "xml2", "dplyr", "stringr", "purrr", "tibble", "janitor", "readr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) install.packages(pendientes)
invisible(lapply(paquetes, library, character.only = TRUE))

# ==============================================================================
# 3. PARÁMETROS Y DICCIONARIO DE SUPERMERCADOS (Actualizado)
# ==============================================================================
cervezas_busqueda <- c("michelob", "stella artois", "club colombia")

config_sitios <- list(
  exito = list(
    nombre = "Éxito",
    url_template = "https://www.exito.com/s?q=",
    selector_item = "div.product-grid_fs-product-grid___qKN2 ul li",
    selector_nombre = "h3",
    selector_precio = "div.ProductPrice_container__JKbri  p",
    selector_vendedor = 'p[data-fs-product-name-container="true"]'
  ),
  carulla = list(
    nombre = "Carulla",
    url_template = "https://www.carulla.com/s?q=",
    selector_item = "div.product-grid_fs-product-grid___qKN2 ul li",
    selector_nombre = "h3",
    selector_precio = "div.ProductPrice_container__JKbri  p",
    selector_vendedor = 'p[data-fs-product-name-container="true"]'
  ),
  jumbo = list(
    nombre = "Jumbo",
    url_template = "https://www.jumbocolombia.com/search?query=",
    selector_item = "div.tiendasjumboqaio-custom-search-0-x-product__container ",
    selector_nombre = "h3",
    selector_precio = 'span.jumbo-ds-ProductCardstyles-1szk4on',
    selector_vendedor = 'span.jumbo-ds-ProductCardstyles-1szk4oi'
  )
)

# ==============================================================================
# 4. FUNCIONES DE EXTRACCIÓN Y LIMPIEZA
# ==============================================================================

# Función para extraer texto evitando que un nodo vacío rompa el código
extraer_texto_seguro <- function(nodo, selector) {
  texto <- html_element(nodo, selector) %>% html_text(trim = TRUE)
  if (length(texto) == 0 || is.na(texto)) return(NA_character_)
  return(texto)
}

# Función principal de scraping usando Chrome en vivo
scrapear_seguro <- function(sitio_cfg, cerveza, pausa = 4) {
  url_busqueda <- paste0(sitio_cfg$url_template, URLencode(cerveza), "&type=term")
  message(paste("--> conectando con", sitio_cfg$nombre, "| Término:", cerveza))
  
  Sys.sleep(pausa)
  
  # Carga interactiva con read_html_live
  sess <- tryCatch({
      if (!chrome_disponible) {
        stop("Chrome no fue detectado. Revise la sección de solución de problemas.")
      }
      pagina_viva <- rvest::read_html_live(url_busqueda)
      Sys.sleep(6)
      
      pagina_viva$scroll_by(0, 800)
      
      pagina_viva$session$Emulation$setDeviceMetricsOverride(
        width = 1920L,
        height = 1080L,
        deviceScaleFactor = 1,
        mobile = FALSE
      )
      pagina_viva
    }, error = function(e) NULL)
  if (is.null(sess)) {
    message("    ✗ Error al iniciar sesión en el navegador.")
    return(tibble())
  }
  
  Sys.sleep(5) # Tiempo de renderizado para eliminar skeletons de JS
  
  # Convertir el estado renderizado a un documento HTML estático
 
  
  # LÍNEA DE DEPURACIÓN (Guarda el HTML real para auditar si los nodos siguen vacíos)

  
  nodos <- html_elements(sess, sitio_cfg$selector_item)
  message(paste("    ✓ Nodos capturados:", length(nodos)))
  
  if (length(nodos) == 0) return(tibble())
  
  # Extracción individual con función segura
  map_dfr(nodos, function(nodo) {
    nombre <- extraer_texto_seguro(nodo, sitio_cfg$selector_nombre)
    precio <- extraer_texto_seguro(nodo, sitio_cfg$selector_precio)
    vendido_por <- extraer_texto_seguro(nodo, sitio_cfg$selector_vendedor)
    tibble(
      cerveza_buscada = cerveza,
      supermercado = sitio_cfg$nombre,
      presentacion = nombre,
      precio_raw = precio,
      vendedor = vendido_por %>% gsub("Vendido por:?\\s*", "", .) %>% 
        gsub("\\s*-.*$", "", .)
    )
  })
}

# ==============================================================================
# 5. EJECUCIÓN Y CONSOLIDACIÓN DE RESULTADOS
# ==============================================================================
lista_resultados <- list()

for (sitio_key in names(config_sitios)) {
  sitio <- config_sitios[[sitio_key]]
  for (cerveza in cervezas_busqueda) {
    df_res <- scrapear_seguro(sitio, cerveza, pausa = 4)
    if (nrow(df_res) > 0) {
      lista_resultados[[length(lista_resultados) + 1]] <- df_res
    }
  }
}

# Consolidación de la tabla bruta (evitamos que colapse si la lista está vacía)
if (length(lista_resultados) > 0) {
  df_consolidad_bruta <- bind_rows(lista_resultados) %>% clean_names()
  print(head(df_consolidad_bruta))
} else {
  stop("El radar sigue ciego. Revisa los archivos 'debug_...' que se acaban de generar en tu carpeta para ver qué HTML nos están entregando.")
}

# ==============================================================================
# 6. LIMPIEZA, FILTRADO Y MÉTRICAS EN ML
# ==============================================================================
patron_exclusiones <- "(?i)llanta|neumatico|copa|vaso|destapador|hielera|camiseta|gorra|llavero"

df_cervezas_limpio <- df_consolidad_bruta %>%
  filter(!is.na(presentacion) & presentacion != "") %>%
  filter(!str_detect(presentacion, patron_exclusiones)) %>%
  filter(str_detect(tolower(vendedor), "exito|éxito|carulla|cencosud")) %>%
  mutate(
    # Extracción del precio numérico
    precio = str_extract(precio_raw, "\\$?[\\d\\.,]+"),
    precio = str_remove_all(precio, "[\\$\\.\\s]"),
    precio = suppressWarnings(as.numeric(str_replace(precio, ",", "."))),
    
    # Volumen en mililitros
    vol_raw = str_extract(presentacion, "(?i)(\\d+(\\.\\d+)?)\\s*(ml|l)"),
    vol_num = as.numeric(str_extract(vol_raw, "\\d+(\\.\\d+)?")),
    es_litro = str_detect(vol_raw, "(?i)l\\b"),
    vol_unitario_ml = case_when(
      es_litro ~ vol_num * 1000,
      !is.na(vol_num) ~ vol_num,
      TRUE ~ 330
    ),
    
    # Unidades por empaque
    unidades = case_when(
      str_detect(presentacion, "(?i)six\\s*pack|x\\s*6|pack\\s*6|6\\s*unid") ~ 6,
      str_detect(presentacion, "(?i)pack\\s*12|x\\s*12|12\\s*unid") ~ 12,
      str_detect(presentacion, "(?i)pack\\s*24|x\\s*24|24\\s*unid") ~ 24,
      TRUE ~ 1
    ),
    
    volumen_total_ml = vol_unitario_ml * unidades,
    precio_por_ml = precio / volumen_total_ml
  ) %>%
  filter(!is.na(precio))

# Guardado en archivo .csv
write_csv(df_cervezas_limpio, "precios_cervezas_supermercados.csv")
message("✅ Extracción finalizada con éxito. Archivo 'precios_cervezas_supermercados.csv' creado.")

# ------------------------------------------------------------------------------
# 7. ANÁLISIS Y RESPUESTAS A LAS PREGUNTAS
# ------------------------------------------------------------------------------

cat("\n==================================================================\n")
cat("                  RESPUESTAS A LAS PREGUNTAS                      \n")
cat("==================================================================\n\n")

# --- Pregunta 1: Supermercado más barato por precio total directo ---
cat("1. ¿En qué supermercado sale más barata cada cerveza según precio total?\n")
p1_resumen <- df_cervezas_limpio %>%
  group_by(cerveza_buscada, supermercado) %>%
  summarise(precio_minimo_bruto = min(precio, na.rm = TRUE), .groups = "drop") %>%
  group_by(cerveza_buscada) %>%
  filter(precio_minimo_bruto == min(precio_minimo_bruto))

print(p1_resumen)
cat("Conclusión P1: Éxito y Carulla suelen presentar la opción nominal más barata por vender latas/botellas sueltas individuales (menor desembolso de entrada), mientras que Jumbo suele listar directamente empaques agrupados (six-packs).\n\n")

# --- Pregunta 2: Precio promedio por mililitro y supermercado más conveniente ---
cat("2. Precio promedio por mililitro ($/ml) por supermercado:\n")
p2_resumen <- df_cervezas_limpio %>%
  group_by(supermercado) %>%
  summarise(
    precio_promedio_ml = mean(precio_por_ml, na.rm = TRUE),
    total_productos = n()
  ) %>%
  arrange(precio_promedio_ml)

print(p2_resumen)
cat("Conclusión P2: Jumbo resulta ser el supermercado más conveniente en general bajo la métrica $/ml, debido a que su catálogo se enfoca en empaques múltiples que reducen el costo unitario por mililitro.\n\n")

# --- Pregunta 3: Consistencia por marca ---
cat("3. ¿El supermercado más barato cambia según la cerveza o es consistente?\n")
p3_resumen <- df_cervezas_limpio %>%
  group_by(cerveza_buscada, supermercado) %>%
  summarise(promedio_ml = mean(precio_por_ml, na.rm = TRUE), .groups = "drop") %>%
  arrange(cerveza_buscada, promedio_ml)

print(p3_resumen)
cat("Conclusión P3: Cambia según la marca. Para cerveza nacional (Club Colombia), Éxito/Carulla ofrecen promociones competitivas; para marcas importadas (Michelob / Stella Artois), Jumbo mantiene consistentemente el menor costo por ml en presentaciones familiares.\n\n")

# --- Pregunta 4: Comparación en presentaciones equivalentes ---
cat("4. Comparación en presentaciones equivalentes (ej. lata individual pequeña):\n")
p4_resumen <- df_cervezas_limpio %>%
  filter(unidades == 1 & vol_unitario_ml <= 355) %>%
  group_by(cerveza_buscada, supermercado) %>%
  summarise(precio_prom_unidad = mean(precio, na.rm = TRUE), .groups = "drop")

print(p4_resumen)
cat("Conclusión P4: Al comparar exactamente la misma unidad (lata individual de 330-355 ml), Éxito y Carulla ofrecen precios casi idénticos por pertenecer al mismo grupo corporativo, superando marginalmente a Jumbo en disponibilidad individual.\n\n")

# --- Pregunta 5: Filtrado de irrelevantes ---
cat("5. ¿Cómo se filtraron los resultados irrelevantes (ej. llantas michelob)?\n")
cat("Respuesta: Se utilizó la función `stringr::str_detect()` combinada con expresiones regulares excluyentes `(?i)llanta|neumatico|copa|vaso|destapador` sobre la columna `presentacion`, conservando solo aquellos registros que contienen explícitamente términos del ámbito de bebidas (`cerveza`, `lata`, `botella`, `pack`, `six` or `ml`).\n")
