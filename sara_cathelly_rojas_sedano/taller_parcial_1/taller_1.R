## SETUP INICIAL
paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr", "chromote", "jsonlite"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  tryCatch(
    install.packages(pendientes),
    error = function(e) stop(
      "No fue posible instalar: ", paste(pendientes, collapse = ", "),
    )
  )
}

if (requireNamespace("rvest", quietly = TRUE) && packageVersion("rvest") < "1.0.5") {
  install.packages("rvest")
}

invisible(lapply(paquetes, library, character.only = TRUE))

## URLS DE BUSQUEDA

##crear URL de búsqueda Cafam
b <- ChromoteSession$new()
termino_busqueda <- "protección%20femenina?_q"
url_cafam <- paste0("https://www.drogueriascafam.com.co/protecci%C3%B3n%20femenina?_q&page=all")
url_cafam
b$Page$navigate(url_cafam)
b$Page$loadEventFired()
Sys.sleep(2)
html_cafam <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
b$close()


##crear URL de búsqueda LaRebaja
b <- ChromoteSession$new()
termino_busqueda <- "protección%20femenina?_q"
url_rebaja <- paste0("https://www.larebajavirtual.com/proteccion%20femenina?_q=proteccion%20femenina&map=ft&page=all")
url_rebaja
b$Page$navigate(url_rebaja)
b$Page$loadEventFired()

for(i in 1:3) {
  b$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
}

html_rebaja <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
b$close()


##crear URL de Búsqueda Olimpica
b <- ChromoteSession$new()
termino_busqueda <- "protección%20femenina?_q"
url_olimpica <- paste0("https://www.olimpica.com/proteccion%20femenina?_q=proteccion%20femenina&map=ft&page=all")
url_olimpica
b$Page$navigate(url_olimpica)
b$Page$loadEventFired()

for(i in 1:3) {
  b$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
}

html_olimpica <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
b$close()


##LECTURA DE PAGINAS

## Leer pag Cafam
pagina_simple_cafam <- tryCatch(
  read_html(html_cafam),
  error = function(e) NULL
)

if (is.null(pagina_simple_cafam)) {
  "La lectura directa falló."
} else {
  pagina_simple_cafam
}


## Leer pag LaRebaja
pagina_simple_rebaja <- tryCatch(
  read_html(url_rebaja),
  error = function(e) NULL
)

if (is.null(pagina_simple_rebaja)) {
  "La lectura directa falló."
} else {
  pagina_simple_rebaja
}


## Leer pag Olimpica
pagina_simple_olimpica <- tryCatch(
  read_html(url_olimpica),
  error = function(e) NULL
)

if (is.null(pagina_simple_olimpica)) {
  "La lectura directa falló."
} else {
  pagina_simple_olimpica
}


##EXTRACCION DATOS
tabla_productos_vacia <- function() {
  tibble(
    comercio = character(),
    marca = character(),
    texto_original = character(),
    precio = character(),
    es_promocion = logical(),
    precio_antes = character(),
    url = character(),
    fecha_consulta = character(),
    precio_unidad = character()
  )
}

## CAFAM

extraer_producto_cafam <- function(nodo) {
  
  # Comercio fijo
  comercio <- "Cafam"
  
  #marca
  marca <- nodo %>%
    html_element("div.vtex-product-summary-2-x-productBrandContainer") %>%
    html_text2()
  
  # Texto original / Nombre del producto
  texto_original <- nodo %>%
    html_element("h3.vtex-product-summary-2-x-productNameContainer") %>%
    html_text2()
  
  # URL absoluta del producto
  enlace_relativo <- nodo %>%
    html_element(xpath = "ancestor-or-self::a | .//a") %>%
    html_attr("href")
  
  url <- if (!is.na(enlace_relativo)) {
    paste0("https://www.drogueriascafam.com.co", enlace_relativo)
  } else {
    NA_character_
  }
  
  # Precio actual en COP
  precio <- nodo %>%
    html_element(xpath = ".//span[contains(@class, 'convenioNormalPrice')]") %>%
    html_text2()
  
  # Precio anterior (si está en promoción)
  precio_antes <- nodo %>%
    html_element(xpath = ".//span[contains(@class, 'price')]") %>%
    html_text2()
  
  es_promocion <- !is.na(precio_antes) && precio_antes != ""
  
  # Fecha y hora de consulta en formato ISO 8601 con zona horaria de Colombia
  fecha_consulta <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
  
  # Precio unidad
  precio_unidad <- nodo %>%
    html_element(xpath = ".//span[contains(@class, 'valuePum')]") %>%
    html_text2()
  
  # Construir el tibble con la fila de datos
  tibble(
    comercio = comercio,
    marca = ifelse(is.na(marca) || marca == "", NA_character_, marca),
    texto_original = ifelse(is.na(texto_original) || texto_original == "", NA_character_, texto_original),
    precio = ifelse(is.na(precio) || precio == "", NA_character_, precio),
    es_promocion = es_promocion,
    precio_antes = ifelse(is.na(precio_antes) || precio_antes == "", NA_character_, precio_antes),
    url = url,
    fecha_consulta = fecha_consulta,
    precio_unidad = precio_unidad
  )
}

nodos_productos <- read_html(html_cafam) %>% 
  html_elements("section.vtex-product-summary-2-x-container")

# Mapear la función sobre cada nodo para consolidar la tabla final
tabla_productos_cafam <- nodos_productos %>% 
  map_dfr(~ extraer_producto_cafam(.x))

# Visualizar el resultado obtenido
print(tabla_productos_cafam)



## La rebaja

tabla_productos_vacia_1 <- function() {
  tibble(
    comercio = character(),
    marca = character(),
    texto_original = character(),
    precio = character(),
    es_promocion = logical(),
    precio_antes = character(),
    url = character(),
    fecha_consulta = character(),
    precio_unidad = character()
  )
}

extraer_producto_rebaja <- function(nodo) {
  
  # Comercio fijo
  comercio <- "Rebaja"
  
  #marca
  marca <- nodo %>%
    html_element("span.vtex-store-components-3-x-productBrandName") %>%
    html_text2()
  
  # Texto original / Nombre del producto
  texto_original <- nodo %>%
    html_element("h3.vtex-product-summary-2-x-productNameContainer") %>%
    html_text2()
  
  # URL absoluta del producto
  enlace_relativo <- nodo %>%
    html_element(xpath = "ancestor-or-self::a | .//a") %>%
    html_attr("href")
  
  url <- if (!is.na(enlace_relativo)) {
    paste0("https://www.larebajavirtual.com/", enlace_relativo)
  } else {
    NA_character_
  }
  
  # Precio actual en COP
  precio <- nodo %>%
    html_element("span.vtex-product-price-1-x-sellingPrice") %>%
    html_text2()
  
  # Precio anterior (si está en promoción)
  precio_antes <- NA
  
  es_promocion <- !is.na(precio_antes) && precio_antes != ""
  
  # Fecha y hora de consulta en formato ISO 8601 con zona horaria de Colombia
  fecha_consulta <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
  
  # Precio unidad
  precio_unidad <- nodo %>%
    html_element("div.copservir-larebaja-theme-9-x-PUM") %>%
    html_text2()
  
  # Construir el tibble con la fila de datos
  tibble(
    comercio = comercio,
    marca = ifelse(is.na(marca) || marca == "", NA_character_, marca),
    texto_original = ifelse(is.na(texto_original) || texto_original == "", NA_character_, texto_original),
    precio = ifelse(is.na(precio) || precio == "", NA_character_, precio),
    es_promocion = es_promocion,
    precio_antes = ifelse(is.na(precio_antes) || precio_antes == "", NA_character_, precio_antes),
    url = url,
    fecha_consulta = fecha_consulta,
    precio_unidad = precio_unidad
  )
}

nodos_productos <- read_html(html_rebaja) %>% 
  html_elements("section.vtex-product-summary-2-x-container")

# Mapear la función sobre cada nodo para consolidar la tabla final
tabla_productos_rebaja <- nodos_productos %>% 
  map_dfr(~ extraer_producto_rebaja(.x))

# Visualizar el resultado obtenido
print(tabla_productos_rebaja)


## Olimpica

tabla_productos_vacia_2 <- function() {
  tibble(
    comercio = character(),
    marca = character(),
    texto_original = character(),
    precio = character(),
    es_promocion = logical(),
    precio_antes = character(),
    url = character(),
    fecha_consulta = character(),
    precio_unidad = character()
  )
}

extraer_producto_olimpica <- function(nodo) {
  
  # Comercio fijo
  comercio <- "olimpica"
  
  #marca
  marca <- nodo %>%
    html_element("div.vtex-product-summary-2-x-productBrandContainer") %>%
    html_text2()
  
  # Texto original / Nombre del producto
  texto_original <- nodo %>%
    html_element("h3.vtex-product-summary-2-x-productNameContainer") %>%
    html_text2()
  
  # URL absoluta del producto
  enlace_relativo <- nodo %>%
    html_element(xpath = "ancestor-or-self::a | .//a") %>%
    html_attr("href")
  
  url <- if (!is.na(enlace_relativo)) {
    paste0("https://www.olimpica.com/", enlace_relativo)
  } else {
    NA_character_
  }
  
  # Precio actual en COP
  precio <- nodo %>%
    html_element("span.olimpica-dinamic-flags-0-x-currencyContainer") %>%
    html_text2()
  
  # Precio anterior (si está en promoción)
  precio_antes <- NA
  
  es_promocion <- !is.na(precio_antes) && precio_antes != ""
  
  # Fecha y hora de consulta en formato ISO 8601 con zona horaria de Colombia
  fecha_consulta <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00")
  
  # Precio unidad
  precio_unidad <- nodo %>%
    html_element("div.copservir-olimpica-theme-9-x-PUM") %>%
    html_text2()
  
  # Construir el tibble con la fila de datos
  tibble(
    comercio = comercio,
    marca = ifelse(is.na(marca) || marca == "", NA_character_, marca),
    texto_original = ifelse(is.na(texto_original) || texto_original == "", NA_character_, texto_original),
    precio = ifelse(is.na(precio) || precio == "", NA_character_, precio),
    es_promocion = es_promocion,
    precio_antes = ifelse(is.na(precio_antes) || precio_antes == "", NA_character_, precio_antes),
    url = url,
    fecha_consulta = fecha_consulta,
    precio_unidad = precio_unidad
  )
}

nodos_productos <- read_html(html_olimpica) %>% 
  html_elements("section.vtex-product-summary-2-x-container")

# Mapear la función sobre cada nodo para consolidar la tabla final
tabla_productos_olimpica <- nodos_productos %>% 
  map_dfr(~ extraer_producto_olimpica(.x))

# Visualizar el resultado obtenido
print(tabla_productos_olimpica)

##LIMPIEZA DE DATOS
#cafam
#cambiar precio y precio unidad de string a num
tabla_productos_cafam <- tabla_productos_cafam %>%
  mutate(
    precio = str_replace_all(precio, "[^0-9]", ""),
    precio = as.numeric(precio),
    precio_unidad = str_replace_all(precio_unidad, "[^0-9]", ""),
    precio_unidad = as.numeric(precio_unidad),
    
    #calcular unidades
    unidades = round(precio / precio_unidad)
  )

#la rebaja
#cambiar precio y precio unidad de string a num
tabla_productos_rebaja <- tabla_productos_rebaja %>%
  mutate(
    precio = str_replace_all(precio, "[^0-9]", ""),
    precio = as.numeric(precio),
    unidades = as.numeric(str_extract(precio_unidad, "\\d+")),
    
    #calcular precio/unidad
    precio_unidad = round(precio / unidades)
  )

#olimpica
#cambiar precio y precio unidad de string a num
tabla_productos_olimpica <- tabla_productos_olimpica %>%
  mutate(
    precio = str_replace_all(precio, "[^0-9]", ""),
    precio = as.numeric(precio),
    unidades = as.numeric(str_extract(texto_original, "(?i)(?<=x)\\d+")),
    
    #calcular precio/unidad
    precio_unidad = round(precio / unidades)
  )

##juntar tablas
tabla_consolidada <- bind_rows(tabla_productos_cafam, tabla_productos_rebaja, tabla_productos_olimpica)
tabla_consolidada <- tabla_consolidada %>%
  mutate(
    marca = str_to_lower(marca)
  )

#group by ID
tabla_con_ids <- tabla_consolidada %>%
  mutate(
    # 1. Normalizar la marca
    marca_norm = str_to_lower(str_trim(marca)),
    
    # 2. Normalizar el texto original
    texto_norm = str_to_lower(texto_original) %>%
      str_replace_all("[^a-z0-9\\s]", "") %>% 
      str_squish()                            
  ) %>%
  
  group_by(marca_norm, unidades, texto_norm) %>%
  
  mutate(producto_id = paste0("ID_", cur_group_id())) %>%
  ungroup() %>%
  
  select(-marca_norm, -texto_norm)

#comparacion de precios
resumen_ahorros <- tabla_con_ids %>%
  filter(!is.na(producto_id)) %>%
  group_by(producto_id) %>%
  # Filtrar solo productos que aparecen en más de una tienda para poder comparar
  filter(n_distinct(comercio) > 1) %>%
  
  summarise(
    producto = first(texto_original),
    unidades = first(unidades),
    precio_min = min(precio, na.rm = TRUE),
    precio_max = max(precio, na.rm = TRUE),
    tienda_economica = comercio[which.min(precio)],
    tienda_costosa = comercio[which.max(precio)],
    #calculos de ahorro
    ahorro_pesos = precio_max - precio_min,
    ahorro_porcentaje = round(((precio_max - precio_min) / precio_max) * 100, 1),
    .groups = "drop"
  ) %>%
  #ordenar
  arrange(desc(ahorro_pesos))

print("=== RESUMEN DE AHORROS POTENCIALES ===")
print(resumen_ahorros)

##guardar en SQLlite
install.packages(c("DBI", "RSQLite"))

library(DBI)
library(RSQLite)

conexion <- dbConnect(SQLite(), dbname = "scraping_protecion_femenina.db")

dbWriteTable(conexion, name = "comparacion", value = resumen_ahorros, overwrite = TRUE)
dbWriteTable(conexion, name = "productos_scraping", value = tabla_con_ids, overwrite = TRUE)

#test
consulta_prueba <- dbGetQuery(conexion, "SELECT comercio, marca, precio, unidades FROM productos_scraping LIMIT 5")
print(consulta_prueba)

dbDisconnect(conexion)

#profe solo tengo 1 producto repetido :(


#visualizacion
install.packages(c("ggplot2", "forcats"))

library(ggplot2)
library(dplyr)
library(forcats)

top_ahorros <- resumen_ahorros %>%
  head(10) %>%
  mutate(
    producto_corto = str_trunc(producto, 40),
    producto_corto = fct_reorder(producto_corto, ahorro_pesos)
  )

ggplot(top_ahorros, aes(x = ahorro_pesos, y = producto_corto, fill = ahorro_pesos)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ".")) +
  labs(
    title = "Top 10 Productos con Mayor Ahorro Potencial",
    subtitle = "Diferencia en pesos (COP) entre la opción más cara y la más económica",
    x = "Ahorro Potencial en Dinero",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 9),
    axis.text.y = element_text(size = 9, face = "bold")
  )

