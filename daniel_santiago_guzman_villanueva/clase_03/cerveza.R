url_exito <- c("https://www.exito.com/s?q=cerbeza+michelob&sort=score_desc&page=0",
               "https://www.exito.com/s?q=cerbeza+Stella+Artois&sort=score_desc&page=0",
               "https://www.exito.com/s?q=cerbeza+club+colombia&sort=score_desc&page=0")

url_carulla <- c("https://www.carulla.com/s?q=cerbeza+michelob&sort=score_desc&page=0",
                 "https://www.carulla.com/s?q=cerbeza+Stella+Artois&sort=score_desc&page=0",
                 "https://www.carulla.com/s?q=cerbeza+club+colombia&sort=score_desc&page=0")

url_jumbo <- c("https://www.jumbocolombia.com/search?query=cerveza%20michelob&type=term",
               "https://www.jumbocolombia.com/search?query=cerveza%20Stella%20Artois&type=term",
               "https://www.jumbocolombia.com/search?query=cerveza%20club%20colombia&type=term")

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

# paquetes ----------------------------------------------------------------


paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr", "chromote", "httr"
)

library(rvest)
library(stringr)
library(tibble)

# primer intento lectura --------------------------------------------------


pagina_simple <- tryCatch(
  read_html(url_exito[1]),
  error = function(e) NULL
)

if (is.null(pagina_simple)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple
}


# agente ------------------------------------------------------------------


agente_p <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/139.0.0.0 Safari/537.36",
  "Edg/139.0.3405.86"
)

# respuesta_exito <- tryCatch(
#     rerequest(url_exito[1]) %>%
#     req_user_agent(agente_p) %>%
#     req_headers(`accept-language` = "en-US,en;q=0.9") %>%
#     req_timeout(seconds = 30) %>%
#     req_error(is_error = function(resp) FALSE) %>%
#     req_perform(),
#   error = function(e) NULL
# )


respuesta_exito <- GET(
  url_exito[1],
  user_agent(agente_p),
  add_headers(
    "accept-language" = "en-US,en;q=0.9"
  ),
  timeout(30)
)

estado_http <- if (is.null(respuesta_exito)) NA_integer_ else resp_status(respuesta_exito)

status_code(respuesta_exito)


# trabajar con el html adquirido ------------------------------------------



pagina_exito <- tryCatch(
  if (is.null(respuesta_exito)) {
    stop("No hubo respuesta")
  } else {
    read_html(content(respuesta_exito, as = "text", encoding = "UTF-8"))
  },
  error = function(e) {
    read_html("<html><head><title>Sin respuesta</title></head><body></body></html>")
  }
)

estado_http <- if (!is.null(respuesta_exito)) {
  status_code(respuesta_exito)
} else {
  NA_integer_
}

titulo_respuesta <- pagina_exito %>%
  html_element("title") %>%
  html_text2()

posible_bloqueo <- is.na(estado_http) ||
  estado_http >= 400 ||
  str_detect(
    str_to_lower(titulo_respuesta),
    "captcha|robot check|sorry|verific|sin respuesta"
  )

tibble(
  estado_http = estado_http,
  titulo_pagina = titulo_respuesta,
  posible_bloqueo = posible_bloqueo
)

productos <- pagina_exito %>% 
html_elements(".s-result-item[data-component-type='s-search-result']")

length(productos)


# inspeccion html ---------------------------------------------------------



html_elements(pagina_exito, "*") |> html_name() |> unique()


length(html_elements(pagina_exito, "[data-testid]"))

html_elements(pagina_exito, "[data-testid]") |>
  html_attr("data-testid") |>
  unique()

html_elements(pagina_exito, "[class]") |>
  html_attr("class") |>
  unique() |>
  head(100)


# codigo de chat  ---------------------------------------------------------
titulo_respuesta

html_text2(html_element(pagina_exito, "body")) |>
  substr(1, 1000)

# chat dice que usar chromote, pero es la libreria que menciono el profe
# hoy en clase