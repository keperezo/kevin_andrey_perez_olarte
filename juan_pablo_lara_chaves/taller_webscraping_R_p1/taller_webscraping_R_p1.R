paquetes <- c(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr", "knitr"
)
# Verificamos qué paquetes faltan. La instalación se hace por fuera de la
# compilación para evitar cambios inesperados en el entorno del estudiante.
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  stop(
    "Faltan paquetes: ", paste(pendientes, collapse = ", "),
    ". Instálelos con install.packages(c(",
    paste(sprintf('"%s"', pendientes), collapse = ", "), "))"
  )
}

# Cargamos los paquetes sin mostrar mensajes
invisible(lapply(paquetes, library, character.only = TRUE))


# Ejemplo ----
url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)


pagina_quotes %>%
  html_element("title") %>%
  html_text2()

citas <- pagina_quotes %>%
  html_elements(".quote .text") %>%
  html_text2()

citas

autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores

# Creamos la tabla
tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

# Mostramos las citas en una tabla

mostrar_tabla <- function(tabla, n = 10, caption = NULL) {
  tabla <- head(tabla, n)
  
  if (!is.null(caption)) {
    cat(caption, "\n\n")
  }
  
  print(tabla)
}

mostrar_tabla(tabla_citas, n = 10, caption = "Primeras citas extraídas")

nodos_citas <- pagina_quotes %>%
  html_elements(".quote")
length(nodos_citas)

tabla_citas_completa <- map_dfr(nodos_citas, function(nodo) {
  cita <- nodo %>% html_element(".text") %>% html_text2()
  autor <- nodo %>% html_element(".author") %>% html_text2()
  tags <- nodo %>% html_elements(".tags .tag") %>% html_text2()
  
  tibble(
    cita = cita,
    autor = autor,
    tags = paste(tags, collapse = ", ")
  )
})

mostrar_tabla(tabla_citas_completa, n = 10, caption = "Citas, autores y etiquetas")

tabla_citas_limpia <- tabla_citas_completa %>%
  mutate(
    # Quitamos las comillas tipográficas de la cita
    cita = str_replace_all(cita, "[\u201c\u201d]", ""),
    # Eliminamos espacios sobrantes en el texto de la cita
    cita = str_squish(cita),
    # Limpiamos espacios innecesarios en el nombre del autor
    autor = str_squish(autor),
    # Calculamos cuántas etiquetas tiene cada cita
    n_tags = if_else(tags == "", 0L, str_count(tags, ",") + 1L)
  )

mostrar_tabla(tabla_citas_limpia, n = 10, caption = "Citas limpias con número de etiquetas")

tabla_citas_limpia %>%
  count(autor, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Autores con más citas")

# seccion 7, varias url

urls_paginas_quotes <- c(
  "https://quotes.toscrape.com/page/1/",
  "https://quotes.toscrape.com/page/2/",
  "https://quotes.toscrape.com/page/3/"
)

urls_paginas_quotes

# Definimos una función que recibe la URL de una página
scrapear_quotes <- function(url) {
  # Leemos el HTML de la página
  pagina <- read_html(url)
  # Seleccionamos todos los bloques que contienen citas
  nodos <- pagina %>% html_elements(".quote")
  
  if (length(nodos) == 0) {
    stop("No se encontraron citas en: ", url)
  }
  
  # Recorremos cada bloque de cita y unimos los resultados en una tabla
  map_dfr(nodos, function(nodo) {
    # Extraemos el texto de la cita
    cita <- nodo %>% html_element(".text") %>% html_text2()
    # Extraemos el nombre del autor
    autor <- nodo %>% html_element(".author") %>% html_text2()
    # Extraemos todas las etiquetas asociadas a la cita
    tags <- nodo %>% html_elements(".tag") %>% html_text2()
    
    # Construimos una fila con la información extraída
    tibble(
      # Guardamos la URL de origen
      url_origen = url,
      # Limpiamos comillas y espacios del texto de la cita
      cita = str_squish(str_replace_all(cita, "[\u201c\u201d]", "")),
      # Limpiamos espacios extra en el nombre del autor
      autor = str_squish(autor),
      # Unimos las etiquetas en un solo texto separado por comas
      tags = paste(tags, collapse = ", ")
    )
  })
}

quotes_tres_paginas <- map_dfr(urls_paginas_quotes, scrapear_quotes)
mostrar_tabla(quotes_tres_paginas, n = 10, caption = "Resultados combinados de varias páginas")

quotes_tres_paginas %>%
  summarise(
    filas = n(),
    autores_unicos = n_distinct(autor),
    citas_duplicadas = sum(duplicated(cita))
  ) %>%
  mostrar_tabla(n = 10, caption = "Validación básica del scraping de citas")

url_books <- "https://books.toscrape.com/catalogue/page-1.html"
pagina_books <- read_html(url_books)
pagina_books

nodos_libros <- pagina_books %>%
  html_elements("article.product_pod")
length(nodos_libros)

# Recorremos cada bloque de libro y unimos los resultados en una tabla
libros_pagina_1 <- map_dfr(nodos_libros, function(nodo) {
  # Extraemos el título del libro desde el atributo title del enlace
  titulo <- nodo %>%
    html_element("h3 a") %>%
    html_attr("title")
  
  # Extraemos el precio como texto
  precio <- nodo %>%
    html_element(".price_color") %>%
    html_text2()
  
  # Extraemos el texto de disponibilidad del libro
  disponibilidad <- nodo %>%
    html_element(".availability") %>%
    html_text2()
  
  # Extraemos la clase CSS donde está codificado el rating
  rating_clase <- nodo %>%
    html_element("p.star-rating") %>%
    html_attr("class")
  
  # Extraemos el enlace relativo hacia el detalle del libro
  enlace_relativo <- nodo %>%
    html_element("h3 a") %>%
    html_attr("href")
  
  # Construimos una fila con las variables extraídas
  tibble(
    titulo          = titulo,
    precio_texto    = precio,
    disponibilidad  = disponibilidad,
    rating_raw      = rating_clase,
    enlace_relativo = enlace_relativo
  )
})

mostrar_tabla(libros_pagina_1, n = 10, caption = "Libros extraídos de una página")


# Definimos una función para limpiar la clase CSS del rating
limpiar_rating <- function(x) {
  # Quitamos el prefijo "star-rating" y dejamos solo la categoría
  x %>%
    str_remove("star-rating\\s+")
}

# Transformamos las variables extraídas a un formato más útil para análisis
libros_pagina_1_limpia <- libros_pagina_1 %>%
  mutate(
    # Convertimos el precio de texto a número
    precio = precio_texto %>%
      str_replace_all("[^0-9\\.]", "") %>%
      as.numeric(),
    
    # Limpiamos la clase del rating para dejar solo su valor
    rating = limpiar_rating(rating_raw),
    
    # Creamos una variable lógica que indica si el libro está disponible
    disponible = str_detect(str_to_lower(disponibilidad), "in stock"),
    
    # Construimos el enlace completo hacia el detalle del libro
    enlace = paste0("https://books.toscrape.com/catalogue/", enlace_relativo)) %>%
  
  # Seleccionamos las variables finales que queremos conservar
  select(titulo, precio, disponibilidad, disponible, rating, enlace)

# Mostramos la tabla con las variables ya transformadas
mostrar_tabla(libros_pagina_1_limpia, n = 10, caption = "Libros con variables transformadas")

# seccion 10, automatizar scrapping en varias paginas

urls_books <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:3,
  ".html"
)

urls_books

scrapear_libros <- function(url) {
  
  # Leemos el HTML de la página indicada
  pagina <- read_html(url)
  
  # Seleccionamos todos los bloques que representan libros
  nodos  <- pagina %>%
    html_elements("article.product_pod")
  
  if (length(nodos) == 0) {
    stop("No se encontraron libros en: ", url)
  }
  
  # Recorremos cada bloque y unimos los resultados en una sola tabla
  map_dfr(nodos, function(nodo) {
    # Extraemos el título del libro desde el atributo title
    titulo <- nodo %>%
      html_element("h3 a") %>%
      html_attr("title")
    
    # Extraemos el precio tal como aparece en la página
    precio <- nodo %>%
      html_element(".price_color") %>%
      html_text2()
    
    # Extraemos el texto de disponibilidad
    disponibilidad <- nodo %>%
      html_element(".availability") %>%
      html_text2()
    # Extraemos la clase CSS que contiene la información del rating
    rating_raw <- nodo %>%
      html_element("p.star-rating") %>%
      html_attr("class")
    
    # Extraemos el enlace relativo hacia la página de detalle
    enlace_relativo <- nodo %>%
      html_element("h3 a") %>%
      html_attr("href")
    
    # Construimos una fila con los datos limpios y listos para analizar
    tibble(
      url_origen     = url,
      titulo         = titulo,
      precio         = precio %>% str_replace_all("[^0-9\\.]", "") %>% as.numeric(),
      disponibilidad = str_squish(disponibilidad),
      rating         = rating_raw %>% str_remove("star-rating\\s+"),
      enlace         = paste0("https://books.toscrape.com/catalogue/", enlace_relativo)
    )
  })
}

libros_varias_paginas <- map_dfr(urls_books, scrapear_libros)
mostrar_tabla(libros_varias_paginas, n = 10, caption = "Libros combinados de varias páginas")

# Cuántos libros hay en cada categoría
libros_varias_paginas %>%
  count(rating, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Conteo de libros por rating")

# Resumen de precios
libros_varias_paginas %>%
  summarise(
    n_libros        = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_min      = min(precio, na.rm = TRUE),
    precio_max      = max(precio, na.rm = TRUE)
  ) %>%
  mostrar_tabla(n = 10, caption = "Resumen de precios del catálogo")

# Definimos la URL de una página que contiene tablas HTML
url_tabla <- "https://www.w3schools.com/html/html_tables.asp"
# Leemos el HTML de la página
pagina_tabla <- read_html(url_tabla)

# Extraemos todas las tablas HTML y las convertimos en data frames
tablas <- pagina_tabla %>%
  html_table(fill = TRUE)

if (length(tablas) == 0) {
  stop("No se encontraron tablas en: ", url_tabla)
}
# Contamos cuántas tablas fueron encontradas en la página
length(tablas)

# Tomamos la primera tabla extraída y limpiamos sus nombres de columnas
tabla_ejemplo <- tablas[[1]] %>%
  janitor::clean_names()

# Mostramos la tabla con formato amigable para el reporte
mostrar_tabla(tabla_ejemplo, n = 10, caption = "Ejemplo de tabla HTML extraída directamente")

# Calculamos un resumen rápido para validar la calidad del scraping
libros_varias_paginas %>%
  summarise(
    filas             = n(),
    titulos_unicos    = n_distinct(titulo),
    faltantes_precio  = sum(is.na(precio)),
    duplicados_titulo = sum(duplicated(titulo))
  )



# desarrollo ejercicio ----

# el ejercicio pide tomar los años 2024 y 2025, sin embargo las temperaturas estan desde 2025

# construccion de las URLs correspondientes a cada mes

urls_temps <- paste0(
  "https://www.accuweather.com/es/co/bogota/107487/",
  c(
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december"
  ),
  "-weather/107487?year=2025"
)

urls_temps

## prueba de extraer el html de un solo mes ----

url <- urls_temps[1]
# prueba 1 (no sirve) 
enero2025 <- read_html(urls_temps[1])

download.file(
  url,
  tempfile(),
  mode = "wb"
)

# aqui el error radica en que la página no puede ser trabajada unicamente con rvest debido a que 
# la pagina nos bloqueaba el acceso, usamos la libreria httr2 para solventar esto

browseURL(url)

# la url esta bien

#prueba 2 (tampoco)
respuesta <- request(urls_temps[1]) %>%
  req_user_agent("Mozilla/5.0") %>%
  req_perform()

# parece no ser suficiente

#prueba 3
library(httr2)

respuesta <- request(url) %>%
  req_user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36"
  ) %>%
  req_headers(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    `Accept-Language` = "es-CO,es;q=0.9,en;q=0.8"
  ) %>%
  req_perform()

resp_status(respuesta)

enero2025 <- resp_body_html(respuesta)

# se logro

# definimos los elementos que vamos a usar: temperaturas máximas y mínimas de cada mes, fecha

mes <- enero2025 %>%
  html_element("div.monthly-dropdowns div.map-dropdown h2") %>%
  html_text2()

##
extraer_mes <- function(html, nombre_mes) {
  # 1. Extraer TODOS los paneles tal cual aparecen en orden
  paneles <- html %>% html_elements("a.monthly-daypanel")
  
  dias <- tibble(
    orden = seq_along(paneles),
    dia   = paneles %>% html_element(".date") %>% html_text2() %>% str_trim() %>% as.integer(),
    max   = paneles %>% html_element(".high") %>% html_text2() %>% str_remove("°") %>% as.numeric(),
    min   = paneles %>% html_element(".low")  %>% html_text2() %>% str_remove("°") %>% as.numeric()
  )
  
  # 2. Encontrar el índice del primer "1" (inicio del mes real)
  inicio <- which(dias$dia == 1)[1]
  
  # 3. Encontrar el último día "consecutivo" antes de que vuelva a bajar
  #    Usamos el patrón: cortar en el primer punto donde el día actual < día anterior
  #    y ya llevamos > 15 días (para no cortar en el salto 30→31 de un mes de 31).
  from_inicio <- dias[inicio:nrow(dias), ]
  corte_rel   <- which(diff(from_inicio$dia) < 0 & from_inicio$dia[-1] < 15)[1]
  n_dias      <- if (is.na(corte_rel)) nrow(from_inicio) else corte_rel
  
  from_inicio[seq_len(n_dias), ] %>% select(-orden)
}

extraer_mes(enero2025, enero2025 %>%
              html_element("div.monthly-dropdowns div.map-dropdown h2") %>%
              html_text2())
##


# version final ----

# Scraping temperaturas diarias Bogotá 2025 - AccuWeather


library(httr2)
library(rvest)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(readr)


# 1. URLs de los 12 meses


anio <- 2025

meses_ing <- c(
  "january", "february", "march",     "april",
  "may",     "june",     "july",      "august",
  "september","october", "november",  "december"
)

urls_temps <- paste0(
  "https://www.accuweather.com/es/co/bogota/107487/",
  meses_ing,
  "-weather/107487?year=", anio
)


# 2. Extracción de los días de un mes (tu función, depurada)
#    - localiza el primer "1" del calendario
#    - corta en el primer descenso del contador pasado el día 15
#    - devuelve solo: dia, max, min


extraer_mes <- function(html) {
  
  paneles <- html %>% html_elements("a.monthly-daypanel")
  
  dias <- tibble(
    orden = seq_along(paneles),
    dia   = paneles %>% html_element(".date") %>% html_text2() %>%
      str_trim() %>% as.integer(),
    max   = paneles %>% html_element(".high") %>% html_text2() %>%
      str_remove("°") %>% as.numeric(),
    min   = paneles %>% html_element(".low")  %>% html_text2() %>%
      str_remove("°") %>% as.numeric()
  )
  
  # inicio del mes: primer "1"
  inicio <- which(dias$dia == 1)[1]
  if (is.na(inicio)) return(NULL)
  
  from_inicio <- dias[inicio:nrow(dias), ]
  
  # corte: primer descenso del contador con día < 15
  # (evita cortar el salto normal 30 → 31 de un mes de 31 días)
  corte_rel <- which(diff(from_inicio$dia) < 0 & from_inicio$dia[-1] < 15)[1]
  n_dias    <- if (is.na(corte_rel)) nrow(from_inicio) else corte_rel
  
  from_inicio[seq_len(n_dias), ] %>% select(dia, max, min)
}


# 3. Scraping de un mes completo
#    - pausa entre peticiones
#    - obtiene mes_num desde la URL
#    - agrega fecha (Date) y url_origen


scrapear_mes <- function(u, anio = 2025) {
  
  Sys.sleep(runif(1, 1.5, 3))   # pausa 1.5–3 s
  
  # metadatos desde la URL: nombre del mes en inglés y número
  mes_ing <- str_match(u, "/([a-z]+)-weather/")[, 2]
  mes_num <- match(mes_ing, meses_ing)
  
  html <- tryCatch(
    request(u) %>%
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36") %>%
      req_headers(
        Accept            = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
        `Accept-Language` = "es-CO,es;q=0.9,en;q=0.8"
      ) %>%
      req_retry(max_tries = 3, backoff = ~ 3) %>%
      req_perform() %>%
      resp_body_html(),
    error = function(e) {
      message("  Falló: ", u, " → ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(html)) return(NULL)
  
  dias <- extraer_mes(html)
  if (is.null(dias) || nrow(dias) == 0) return(NULL)
  
  dias %>%
    mutate(
      fecha       = make_date(year = anio, month = mes_num, day = dia),
      url_origen  = u,
      .before     = dia
    ) %>%
    select(fecha, high = max, low = min, url_origen)
}


# 4. Bucle por los 12 meses


datos_clima <- map_dfr(urls_temps, scrapear_mes, anio = anio)


# 5. Verificaciones de calidad


cat("\n--- Resumen del scraping ---\n")
cat("Filas totales:   ", nrow(datos_clima), "\n")
cat("Rango de fechas: ", format(min(datos_clima$fecha)),
    "→", format(max(datos_clima$fecha)), "\n\n")

# 5.1 duplicados
dups <- datos_clima %>% count(fecha) %>% filter(n > 1)
if (nrow(dups) == 0) cat("✔ Sin fechas duplicadas\n") else {
  cat("  Fechas duplicadas:\n"); print(dups)
}

# 5.2 valores faltantes
cat("\nNA por columna:\n"); print(colSums(is.na(datos_clima)))

# 5.3 cobertura del año
esperado <- seq.Date(
  as.Date(paste0(anio, "-01-01")),
  as.Date(paste0(anio, "-12-31")),
  by = "day"
)
faltan <- setdiff(esperado, datos_clima$fecha)
if (length(faltan) == 0) {
  cat("\n✔ Cobertura completa: 365 días de", anio, "presentes\n")
} else {
  cat("\n  Faltan", length(faltan), "días:\n"); print(faltan)
}

# 5.4 resumen térmico
cat("\nResumen de temperaturas (°C):\n")
print(summary(datos_clima %>% select(high, low)))


# 6. Guardar CSV


dir.create("salidas", showWarnings = FALSE)
write_csv(
  datos_clima,
  file = file.path("salidas", paste0("bogota_temperaturas_", anio, ".csv"))
)
cat("\n✔ Guardado en salidas/bogota_temperaturas_", anio, ".csv\n", sep = "")

print(head(datos_clima, 10))

# finalizado
