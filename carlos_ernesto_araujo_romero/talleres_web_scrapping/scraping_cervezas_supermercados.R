library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(chromote)

# Configuracion de Brave para chromote
rutas_brave <- c(
  file.path(Sys.getenv("LOCALAPPDATA"), "BraveSoftware/Brave-Browser/Application/brave.exe"),
  "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
  "C:/Program Files (x86)/BraveSoftware/Brave-Browser/Application/brave.exe",
  "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
  "/usr/bin/brave-browser"
)

brave_path <- rutas_brave[file.exists(rutas_brave)][1]
if (!is.na(brave_path)) {
  Sys.setenv(CHROMOTE_CHROME = brave_path)
}

# Tiendas y marcas a buscar
supermercados <- c(
  "Exito"   = "https://www.exito.com/s?q=",
  "Carulla" = "https://www.carulla.com/s?q=",
  "Jumbo"   = "https://www.tiendasjumbo.co/s?q="
)

cervezas <- c("michelob", "stella artois", "club colombia")

busquedas <- expand.grid(
  tienda = names(supermercados),
  cerveza = cervezas,
  stringsAsFactors = FALSE
) %>%
  mutate(url = paste0(supermercados[tienda], URLencode(cerveza)))

# Funcion para extraer productos usando live browser
scrapear_tienda <- function(tienda, cerveza, url) {
  message("Consultando: ", tienda, " - ", cerveza)
  
  sesion <- tryCatch(read_html_live(url), error = function(e) NULL)
  if (is.null(sesion)) return(tibble())
  
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  Sys.sleep(4)
  
  # Scroll para cargar productos diferidos
  try({
    sesion$scroll_by(top = 1000)
    Sys.sleep(2)
  }, silent = TRUE)
  
  nodos <- sesion %>%
    html_elements("article, [data-fs-product-card='true'], .product-card, div[class*='product-summary']")
  
  if (length(nodos) == 0) return(tibble())
  
  map_dfr(nodos, function(n) {
    nombre <- n %>%
      html_element("h3, [data-fs-product-card-title='true'], span[class*='product-name']") %>%
      html_text2()
    
    precio <- n %>%
      html_element("[data-fs-price='true'], [class*='Price'], [class*='sellingPrice']") %>%
      html_text2() %>%
      str_remove_all("[^0-9]") %>%
      as.numeric()
    
    tibble(
      supermercado = tienda,
      marca = cerveza,
      presentacion = nombre,
      precio = precio,
      url_origen = url
    )
  })
}

scrapear_safe <- possibly(scrapear_tienda, otherwise = tibble())

# Ejecucion del scraping
resultados <- pmap_dfr(
  list(busquedas$tienda, busquedas$cerveza, busquedas$url),
  function(t, c, u) {
    res <- scrapear_safe(t, c, u)
    Sys.sleep(runif(1, 2, 3))
    res
  }
)

# Limpieza y filtrado de ruido (eliminar productos que no sean cerveza)
palabras_validas <- "cerveza|beer|pack|lata|botella|ultra|dorada|roja|negra"
ruido <- "llanta|neumatico|copa|vaso|termo"

df_limpio <- resultados %>%
  filter(!is.na(presentacion), !is.na(precio), precio > 0) %>%
  mutate(nombre_min = str_to_lower(presentacion)) %>%
  filter(str_detect(nombre_min, palabras_validas)) %>%
  filter(!str_detect(nombre_min, ruido))

# Extraccion de mililitros y calculo de precio por ml
datos_cervezas <- df_limpio %>%
  mutate(
    # Extraer ml unitario
    ml_unitario = str_extract(nombre_min, "([0-9]+)\\s*(ml|cc)"),
    ml_unitario = as.numeric(str_extract(ml_unitario, "[0-9]+")),
    ml_unitario = coalesce(ml_unitario, 330),
    
    # Extraer cantidad de unidades si es pack
    unidades = case_when(
      str_detect(nombre_min, "six\\s*pack|sixpack|x\\s*6|6\\s*und|pack.*6") ~ 6,
      str_detect(nombre_min, "pack.*12|12\\s*und|x\\s*12") ~ 12,
      str_detect(nombre_min, "pack.*24|24\\s*und|x\\s*24") ~ 24,
      str_detect(nombre_min, "pack.*4|4\\s*und|x\\s*4") ~ 4,
      TRUE ~ 1
    ),
    
    volumen_total_ml = ml_unitario * unidades,
    precio_por_ml = round(precio / volumen_total_ml, 4)
  )

# Guardar base consolidada
write_csv(datos_cervezas, "cervezas_supermercados.csv")

# Respuestas a las preguntas del taller

# 1. Supermercado con menor precio total bruto por cerveza
print("1. Menor precio total por marca:")
datos_cervezas %>%
  group_by(marca, supermercado) %>%
  summarise(min_precio = min(precio), .groups = "drop") %>%
  arrange(marca, min_precio) %>%
  print()

# 2. Precio promedio por ml por supermercado
print("2. Precio promedio por mililitro:")
p_ml <- datos_cervezas %>%
  group_by(supermercado) %>%
  summarise(
    n = n(),
    precio_ml_prom = mean(precio_por_ml)
  ) %>%
  arrange(precio_ml_prom)
print(p_ml)

# 3. Supermercado mas barato por marca (segun precio/ml)
print("3. Supermercado mas barato segun $/ml por marca:")
datos_cervezas %>%
  group_by(marca, supermercado) %>%
  summarise(precio_ml_prom = mean(precio_por_ml), .groups = "drop") %>%
  group_by(marca) %>%
  slice_min(precio_ml_prom, n = 1) %>%
  print()

# 4. Comparacion de presentaciones equivalentes (lata 330ml individual)
print("4. Comparacion latas individuales 330ml:")
datos_cervezas %>%
  filter(unidades == 1, ml_unitario == 330) %>%
  select(supermercado, marca, presentacion, precio, precio_por_ml) %>%
  arrange(marca, precio) %>%
  print()

# 5. Nota sobre el filtro de ruido:
# Se uso str_detect combinando una condicion positiva (debe contener palabras como cerveza/lata/pack)
# y una negativa (excluir 'llanta', 'neumatico', etc. que salen al buscar michelob).
