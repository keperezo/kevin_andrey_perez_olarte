#### Sergio Martinez Vallejo   -  Mineria de datos  -  Taller 1  ####
### Para leer una página web, necesitamos cargar los sigueintes paquetes ###


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


### Página usada para hacer ejercicio de "Scraping"  ###

url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)
pagina_quotes


### Extraer el titulo ###
pagina_quotes %>%
  html_element("title") %>%
  html_text2()


### Extraer las citas que contiene la pagina  ###
citas <- pagina_quotes %>%
  html_elements(".quote .text") %>%
  html_text2()

citas


### Extraer los autores del material biblografico  ####
autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores


#### 4.) Construir una tabla que contengan las citas biblograficas ####
# Creamos la tabla
tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

# Mostramos las citas en una tabla
print(tabla_citas, n = 10, caption = "Primeras citas extraídas")


#### 5.) Ademas de texto y autor, se añaden la etiquetas asociadas a cada cita
nodos_citas <- pagina_quotes %>%
  html_elements(".quote")
length(nodos_citas)


#### Extraer información por bloque ####
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

print(tabla_citas_completa, n = 10, caption = "Citas, autores y etiquetas")


#### 6.) Limpieza basica del texto extraido ####
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

print(tabla_citas_limpia, n = 10, caption = "Citas limpias con número de etiquetas")

### Resumen rapido ####
tabla_citas_limpia %>%
  count(autor, sort = TRUE) %>%
  print(n = 10, caption = "Autores con más citas")


#### 7.) Navegar varias paginas (sirve para analizar varias paginas) ####
urls_paginas_quotes <- c(
  "https://quotes.toscrape.com/page/1/",
  "https://quotes.toscrape.com/page/2/",
  "https://quotes.toscrape.com/page/3/"
)

urls_paginas_quotes

#### Se crea una funcion que scrapea una pagina ####
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

#### Con la funcion creada se le aplica a cada una de las paginas de interes  ####
quotes_tres_paginas <- map_dfr(urls_paginas_quotes, scrapear_quotes)
print(quotes_tres_paginas, n = 10, caption = "Resultados combinados de varias páginas")

#### Validacion basica ####
quotes_tres_paginas %>%
  summarise(
    filas = n(),
    autores_unicos = n_distinct(autor),
    citas_duplicadas = sum(duplicated(cita))
  ) %>%
  print(n = 10, caption = "Validación básica del scraping de citas")


#### 8.) Scraping de un catalogo de libros ####
url_books <- "https://books.toscrape.com/catalogue/page-1.html"
pagina_books <- read_html(url_books)
pagina_books

#### Identificar los bloques de un producto ####
nodos_libros <- pagina_books %>%
  html_elements("article.product_pod")
length(nodos_libros)

#### Extraemos datos de cada uno de los libros  ####
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

print(libros_pagina_1, n = 10, caption = "Libros extraídos de una página")


#### 9.) Limpieza del catalogo  ####
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
print(libros_pagina_1_limpia, n = 10, caption = "Libros con variables transformadas")


#### 10.) Scrapear varias paginas del catalogo ####
urls_books <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:3,
  ".html"
)

urls_books

#### Funcion general para libros ####
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

#### Se ejecuta un multi-scraping ####
libros_varias_paginas <- map_dfr(urls_books, scrapear_libros)
print(libros_varias_paginas, n = 10, caption = "Libros combinados de varias páginas")

#### Conteo por rating (cuantos libros hay por categoria) ####
# Cuántos libros hay en cada categoría
libros_varias_paginas %>%
  count(rating, sort = TRUE) %>%
  print(n = 10, caption = "Conteo de libros por rating")

#### Resumen de los precios ####
# Resumen de precios
libros_varias_paginas %>%
  summarise(
    n_libros        = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_min      = min(precio, na.rm = TRUE),
    precio_max      = max(precio, na.rm = TRUE)
  ) %>%
  print(n = 10, caption = "Resumen de precios del catálogo")


#### 11.) Extraer tablas HTML directamente (cuando las paginas web lo publican) ####
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

#### Para poder ver la primera tabla ####
# Tomamos la primera tabla extraída y limpiamos sus nombres de columnas
tabla_ejemplo <- tablas[[1]] %>%
  janitor::clean_names()

# Mostramos la tabla con formato amigable para el reporte
print(tabla_ejemplo, n = 10, caption = "Ejemplo de tabla HTML extraída directamente")


#### 12.) Buenas practicas en scraping con R ####
#### 12.2) Calculamos un resumen rápido para validar la calidad del scraping ####
libros_varias_paginas %>%
  summarise(
    filas             = n(),
    titulos_unicos    = n_distinct(titulo),
    faltantes_precio  = sum(is.na(precio)),
    duplicados_titulo = sum(duplicated(titulo))
  )


#### 12.3) Respetar el sitio y limitar las solicitudes ####
# Recorremos cada URL del catálogo
for (u in urls_books) {
  # Hacemos una pausa de 1 segundo entre peticiones
  Sys.sleep(1)
  # Mostramos en consola la URL que se está procesando
  print(u)
  # Aquí iría la lectura de la página
  # read_html(u)
}

#### 12.4) Guardar resultados intermedios ####
# Guardamos las citas extraídas en un archivo CSV
write_csv(quotes_tres_paginas, "quotes_tres_paginas.csv")
# Guardamos los libros extraídos en otro archivo CSV
write_csv(libros_varias_paginas, "libros_varias_paginas.csv")

#### 12.5) Manejo básico de errores ####
# Capturamos tanto el resultado como el error para poder diagnosticarlo
scrapear_libros_seguro <- purrr::safely(scrapear_libros)
respuestas <- map(urls_books, scrapear_libros_seguro)

# Unimos las respuestas exitosas
resultado_seguro <- respuestas %>%
  map("result") %>%
  compact() %>%
  list_rbind()

# Registramos la URL y el mensaje de cada error
errores_scraping <- tibble(
  url = urls_books,
  error = map_chr(
    respuestas,
    ~ if (is.null(.x$error)) NA_character_ else conditionMessage(.x$error)
  )
) %>%
  filter(!is.na(error))

resultado_seguro

errores_scraping



#### 13.) Ejercicio en clase ####
url_weather = "https://www.accuweather.com/"
