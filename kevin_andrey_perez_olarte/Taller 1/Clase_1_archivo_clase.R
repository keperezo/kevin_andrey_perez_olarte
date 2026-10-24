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

url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)
pagina_quotes

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
library(knitr)
View(tabla_citas)
mostrar_tabla <- kable
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
View(tabla_citas_completa)

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
View(tabla_citas_limpia)

tabla_citas_limpia %>%
  count(autor, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Autores con más citas")
