mostrar_tabla <- function(x, n = 10, caption = NULL) {
  x_mostrar <- head(x, n)
  
  tabla_html <- knitr::kable(
    x_mostrar,
    format = "html",
    caption = caption,
    table.attr = 'class="table table-striped table-hover table-condensed"'
  )
  
  knitr::asis_output(
    paste0(
      '<div class="table-scroll">',
      as.character(tabla_html),
      '</div>'
    )
  )
}
paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr", "chromote"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  tryCatch(
    install.packages(pendientes),
    error = function(e) stop(
      "No fue posible instalar: ", paste(pendientes, collapse = ", "),
      ". Solicite apoyo al docente. Detalle: ", conditionMessage(e)
    )
  )
}

if (requireNamespace("rvest", quietly = TRUE) && packageVersion("rvest") < "1.0.5") {
  install.packages("rvest")
}

invisible(lapply(paquetes, library, character.only = TRUE))
diagnostico <- tibble(
  elemento = c("Sistema operativo", "Versión de R", "Versión de rvest", "Versión de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)

mostrar_tabla(diagnostico, n = nrow(diagnostico), caption = "Entorno de ejecución")
if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior. Ejecute install.packages('rvest') y reinicie RStudio.")
}

chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)

chrome_disponible

termino_busqueda <- "laptop"
url_amazon <- paste0("https://www.amazon.com/s?k=", URLencode(termino_busqueda))
url_amazon

pagina_simple <- tryCatch(
  read_html(url_amazon),
  error = function(e) NULL
)

if (is.null(pagina_simple)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple
}
# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal
respuesta_amazon <- tryCatch(
  request(url_amazon) %>%
    req_user_agent(user_agent_navegador) %>%
    req_headers(`accept-language` = "en-US,en;q=0.9") %>%
    req_timeout(seconds = 30) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform(),
  error = function(e) NULL
)

estado_http <- if (is.null(respuesta_amazon)) NA_integer_ else resp_status(respuesta_amazon)

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
pagina_amazon <- tryCatch(
  if (is.null(respuesta_amazon)) stop("No hubo respuesta") else resp_body_html(respuesta_amazon),
  error = function(e) read_html("<html><head><title>Sin respuesta</title></head><body></body></html>")
)

titulo_respuesta <- pagina_amazon %>%
  html_element("title") %>%
  html_text2()

posible_bloqueo <- is.na(estado_http) || estado_http >= 400 ||
  str_detect(str_to_lower(titulo_respuesta), "captcha|robot check|sorry|verific|sin respuesta")

tibble(
  estado_http = estado_http,
  titulo_pagina = titulo_respuesta,
  posible_bloqueo = posible_bloqueo
)
pagina_amazon
productos <- pagina_amazon %>%
  html_elements(".s-result-item[data-component-type='s-search-result']")

length(productos)
# Mantiene la estructura esperada cuando una página no devuelve productos.
tabla_productos_vacia <- function() {
  tibble(
    titulo = character(),
    enlace = character(),
    precio = character(),
    rating = character(),
    n_resenas = character(),
    pagina = integer()
  )
}

# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_amazon <- function(nodo) {
  # Extraemos el título visible del producto
  titulo <- nodo %>%
    html_element("h2 span") %>%
    html_text2()
  
  # Extraemos el enlace relativo hacia la página del producto
  enlace_relativo <- nodo %>%
    html_element("h2 a") %>%
    html_attr("href")
  
  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".a-price .a-offscreen") %>%
    html_text2()
  
  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  rating <- nodo %>%
    html_element(".a-icon-alt") %>%
    html_text2()
  
  # Extraemos el número de reseñas o valoraciones asociadas al producto
  n_resenas <- nodo %>%
    html_element("[aria-label$='ratings'], .s-link-style .s-underline-text") %>%
    html_text2()
  
  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    # Si el enlace no existe, dejamos un valor faltante; si existe, construimos la URL completa
    enlace = ifelse(
      is.na(enlace_relativo),
      NA_character_,
      xml2::url_absolute(enlace_relativo, "https://www.amazon.com")
    ),
    precio = precio,
    rating = rating,
    n_resenas = n_resenas
  )
}
tabla_productos <- if (length(productos) == 0) {
  tabla_productos_vacia() %>% select(-pagina)
} else {
  map_dfr(productos, extraer_producto_amazon) %>% clean_names()
}

if (nrow(tabla_productos) == 0) {
  message("No se encontraron productos. Revise el estado HTTP, un posible bloqueo o los selectores.")
}

mostrar_tabla(tabla_productos, n = 10, caption = "Primeros resultados extraídos desde Amazon")
tabla_productos_limpia <- tabla_productos %>%
  mutate(
    titulo = str_squish(titulo),
    precio = str_remove_all(precio, "[$,]"),
    precio = suppressWarnings(as.numeric(precio)),
    rating = str_extract(rating, "[0-9]+\\.?[0-9]*"),
    rating = suppressWarnings(as.numeric(rating)),
    n_resenas = str_remove_all(n_resenas, "[,]"),
    n_resenas = str_extract(n_resenas, "[0-9]+"),
    n_resenas = suppressWarnings(as.integer(n_resenas))
  ) %>%
  filter(!is.na(titulo), titulo != "")
mostrar_tabla(tabla_productos_limpia, n = 10, caption = "Resultados con limpieza básica")
resumen_na <- tibble(
  variable = names(tabla_productos_limpia),
  n_na = sapply(tabla_productos_limpia, function(x) sum(is.na(x)))
)

mostrar_tabla(resumen_na, n = nrow(resumen_na), caption = "Valores faltantes por variable")
construir_url_amazon <- function(termino, pagina = 1) {
  paste0(
    "https://www.amazon.com/s?k=", URLencode(termino),
    "&page=", pagina
  )
}

construir_url_amazon("laptop", 3)
dir.create("resultados", showWarnings = FALSE)
ruta_salida <- file.path("resultados", "amazon_laptop_resultados.csv")
write_csv(productos_varias_paginas, ruta_salida)
ruta_salida
