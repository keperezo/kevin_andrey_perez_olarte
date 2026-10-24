
# version sin debuggear

# v0.1

setwd("C:/Users/thetr/Desktop/Minería/dm_2016325_2026_2/daniel_mauricio_vanegas_oliveros/parcial_1")

# ponganle un system dormir a las consultas o si no, no carga

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

#mostrar_tabla(diagnostico, n = nrow(diagnostico), caption = "Entorno de ejecución")
if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior. Ejecute install.packages('rvest') y reinicie RStudio.")
}

chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)

chrome_disponible


# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_carulla <- function(cerveza) {
  
  cerveza = stringr::str_replace_all(cerveza, "_","+")

  pagina_consulta = paste0("https://www.carulla.com/s?q=",cerveza,"&sort=score_desc&page=0")
  
  pagina_viva <- rvest::read_html_live(pagina_consulta)
  pagina_viva$view()
  
  nodo <- pagina_viva %>%
    html_elements(".productCard_contentInfo__CBBA7")
  # Extraemos el título visible del producto
  titulo <- nodo %>%
    html_element(".styles_name__qQJiK") %>%
    html_text2()
  
  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".ProductPrice_container__price__XmMWA") %>%
    html_text2()
  
  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  pagina <- "Carulla"
  
  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    precio = precio,
    pagina = pagina,
    hora_consulta = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

extraer_producto_exito <- function(cerveza) {
  
  cerveza = stringr::str_replace_all(cerveza, "_","+")
  
  pagina_consulta = paste0("https://www.exito.com/s?q=",cerveza,"&sort=score_desc&page=0")
  
  pagina_viva <- rvest::read_html_live(pagina_consulta)
  pagina_viva$view()
  
  nodo <- pagina_viva %>%
    html_elements(".productCard_contentInfo__CBBA7")
  # Extraemos el título visible del producto
  titulo <- nodo %>%
    html_element(".styles_name__qQJiK") %>%
    html_text2()
  
  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".ProductPrice_container__price__XmMWA") %>%
    html_text2()
  
  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  pagina <- "Exito"
  
  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    precio = precio,
    pagina = pagina,
    hora_consulta = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

extraer_producto_jumbo <- function(cerveza) {
  
  cerveza = stringr::str_replace_all(cerveza, "_","%20")
  
  pagina_consulta = paste0("https://www.jumbocolombia.com/search?query=",cerveza,"&type=term")
  
  pagina_viva <- rvest::read_html_live(pagina_consulta)
  pagina_viva$view()
  
  nodo <- pagina_viva %>%
    html_elements(".jumbo-ds-ProductCardstyles-1szk4ok")
  # Extraemos el título visible del producto
  titulo <- nodo %>%
    html_element(".jumbo-ds-ProductCardstyles-1szk4ol") %>%
    html_text2()
  
  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".jumbo-ds-ProductCardstyles-1szk4on") %>%
    html_text2()
  
  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  pagina <- "Jumbo"
  
  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    precio = precio,
    pagina = pagina,
    hora_consulta = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

saver_cervezas = function(cerveza){
  pola = list()
  j = 1
  for (i in cerveza) {
    pola[[j]] = extraer_producto_jumbo(i)
    Sys.sleep(9)
    j = j + 1 
    pola[[j]] = extraer_producto_carulla(i)
    Sys.sleep(9)
    j = j + 1 
    pola[[j]] = extraer_producto_exito(i)
    Sys.sleep(9)
    j = j + 1 
  }
  # cat(j,"\n")
  return(bind_rows(pola))
}

cerveza = c("chicle", "gomitas", "mentas", "frutos_secos","conserva", "cereales")

guardado = saver_cervezas(cerveza)

View(guardado)

guardado = guardado[-35,]

write.csv(guardado, file = "~/raw/productos.csv")
