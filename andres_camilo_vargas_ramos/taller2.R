library(rvest)
library(chromote)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)
library(ggplot2)

navegador <- chromote::default_chromote_object()
navegador$default_timeout <- 60
chromote::set_default_chromote_object(navegador)

marcas <- c("michelob", "stella artois", "club colombia")

tabla_base <- function() {
  tibble(
    supermercado = character(),
    busqueda = character(),
    producto = character(),
    precio = character(),
    pagina = character()
  )
}

tiendas <- list(
  exito = list(
    nombre = "Éxito",
    url = function(busqueda) {
      paste0(
        "https://www.exito.com/s?q=",
        URLencode(busqueda),
        "&sort=score_desc&page=0"
      )
    },
    forma = "css",
    tarjeta = "article[class*='productCard_productCard']",
    nombre_producto = "h3[class*='styles_name']",
    precio_producto = "p[data-fs-container-price-otros='true']"
  ),
  
  carulla = list(
    nombre = "Carulla",
    url = function(busqueda) {
      paste0(
        "https://www.carulla.com/s?q=",
        URLencode(busqueda),
        "&sort=score_desc&page=0"
      )
    },
    forma = "css",
    tarjeta = "article[class*='productCard_productCard']",
    nombre_producto = "h3[class*='styles_name']",
    precio_producto = "p[data-fs-container-price-otros='true']"
  ),
  
  jumbo = list(
    nombre = "Jumbo",
    url = function(busqueda) {
      paste0(
        "https://www.jumbocolombia.com/search?query=",
        URLencode(busqueda),
        "&type=term"
      )
    },
    forma = "atributo",
    tarjeta = "div[class*='custom-search-0-x-product__container']",
    atributo_producto = "data-cnstrc-item-name",
    atributo_precio = "data-cnstrc-item-price"
  )
)

obtener_productos <- function(id_tienda, busqueda, espera = 5) {
  
  datos_tienda <- tiendas[[id_tienda]]
  direccion <- datos_tienda$url(busqueda)
  
  message(
    sprintf(
      "Consultando %s | búsqueda: %s",
      datos_tienda$nombre,
      busqueda
    )
  )
  
  pagina <- read_html_live(direccion)
  on.exit(pagina$session$close(), add = TRUE)
  
  Sys.sleep(espera)
  
  elementos <- pagina %>%
    html_elements(datos_tienda$tarjeta)
  
  if (length(elementos) == 0) {
    return(tabla_base())
  }
  
  if (datos_tienda$forma == "css") {
    nombres <- elementos %>%
      html_element(datos_tienda$nombre_producto) %>%
      html_text2()
    
    valores <- elementos %>%
      html_element(datos_tienda$precio_producto) %>%
      html_text2()
  } else {
    nombres <- elementos %>%
      html_attr(datos_tienda$atributo_producto)
    
    valores <- elementos %>%
      html_attr(datos_tienda$atributo_precio)
  }
  
  tibble(
    supermercado = datos_tienda$nombre,
    busqueda = busqueda,
    producto = nombres,
    precio = valores,
    pagina = direccion
  )
}

obtener_seguro <- purrr::possibly(
  obtener_productos,
  otherwise = tabla_base()
)

resultados_lista <- list()

for (tienda_actual in names(tiendas)) {
  for (marca_actual in marcas) {
    
    datos <- obtener_seguro(
      tienda_actual,
      marca_actual,
      espera = 5
    )
    
    resultados_lista[[length(resultados_lista) + 1]] <- datos
    
    Sys.sleep(runif(1, 2, 4))
  }
}

datos_cerveza <- bind_rows(resultados_lista)

if (nrow(datos_cerveza) > 0) {
  
  datos_cerveza <- datos_cerveza %>%
    filter(
      str_detect(
        str_to_lower(producto),
        "cerveza|michelob|stella artois|club colombia"
      )
    ) %>%
    mutate(
      precio_num = precio %>%
        str_remove_all("[^0-9]") %>%
        as.numeric()
    )
}

dir.create("resultados", showWarnings = FALSE)

archivo_csv <- "resultados/cervezas_exito_carulla_jumbo.csv"

write_csv(datos_cerveza, archivo_csv)

print(datos_cerveza)

datos <- read_csv(
  archivo_csv,
  show_col_types = FALSE
)

limpiar_valor <- function(x) {
  x %>%
    str_remove_all("[^0-9]") %>%
    as.numeric()
}

extraer_ml <- function(texto) {
  
  texto <- str_to_lower(texto)
  
  cantidad_paquete <- str_match(
    texto,
    "([0-9\\.]+)\\s*ml\\s*x\\s*([0-9]+)"
  )
  
  cantidad_ml <- str_match(
    texto,
    "([0-9\\.]+)\\s*ml"
  )
  
  cantidad_litros <- str_match(
    texto,
    "([0-9]+(?:,[0-9]+)?)\\s*l\\b"
  )
  
  resultado <- case_when(
    !is.na(cantidad_paquete[, 2]) ~
      as.numeric(str_remove_all(cantidad_paquete[, 2], "\\.")) *
      as.numeric(cantidad_paquete[, 3]),
    
    !is.na(cantidad_ml[, 2]) ~
      as.numeric(str_remove_all(cantidad_ml[, 2], "\\.")),
    
    !is.na(cantidad_litros[, 2]) ~
      as.numeric(str_replace(cantidad_litros[, 2], ",", ".")) * 1000,
    
    TRUE ~ NA_real_
  )
  
  resultado
}

analisis <- datos %>%
  mutate(
    precio_num = limpiar_valor(precio),
    volumen_ml = extraer_ml(producto),
    precio_ml = precio_num / volumen_ml
  )

cat("\n====================================\n")
cat("PREGUNTA 1\n")
cat("====================================\n")

comparacion_precio <- analisis %>%
  filter(!is.na(precio_num)) %>%
  group_by(busqueda, supermercado) %>%
  summarise(
    precio_promedio = mean(precio_num, na.rm = TRUE),
    productos = n(),
    .groups = "drop"
  ) %>%
  arrange(busqueda, precio_promedio)

print(comparacion_precio)

ganador_precio <- comparacion_precio %>%
  group_by(busqueda) %>%
  slice_min(precio_promedio, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("\nRespuesta pregunta 1:\n")

ganador_precio %>%
  mutate(
    respuesta = paste0(
      "Para ",
      str_to_title(busqueda),
      ", el supermercado más barato por precio total promedio es ",
      supermercado,
      " con $",
      round(precio_promedio, 0)
    )
  ) %>%
  pull(respuesta) %>%
  cat(sep = "\n")

cat("\n\n====================================\n")
cat("PREGUNTA 2\n")
cat("====================================\n")

comparacion_ml <- analisis %>%
  filter(
    !is.na(volumen_ml),
    !is.na(precio_ml),
    is.finite(precio_ml)
  ) %>%
  group_by(supermercado) %>%
  summarise(
    precio_promedio_ml = mean(precio_ml, na.rm = TRUE),
    productos = n(),
    .groups = "drop"
  ) %>%
  arrange(precio_promedio_ml)

print(comparacion_ml)

mejor_tienda_ml <- comparacion_ml %>%
  slice_min(precio_promedio_ml, n = 1, with_ties = FALSE)

cat("\nRespuesta pregunta 2:\n")
cat(
  "El supermercado más conveniente en general es",
  mejor_tienda_ml$supermercado,
  "porque presenta el menor precio promedio por mililitro:",
  round(mejor_tienda_ml$precio_promedio_ml, 2),
  "COP por ml.\n"
)

cat("\n====================================\n")
cat("PREGUNTA 3\n")
cat("====================================\n")

ml_por_marca <- analisis %>%
  filter(
    !is.na(volumen_ml),
    !is.na(precio_ml),
    is.finite(precio_ml)
  ) %>%
  group_by(busqueda, supermercado) %>%
  summarise(
    precio_ml_promedio = mean(precio_ml, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(busqueda, precio_ml_promedio)

print(ml_por_marca)

ganadores_ml <- ml_por_marca %>%
  group_by(busqueda) %>%
  slice_min(
    precio_ml_promedio,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

cat("\nRespuesta pregunta 3:\n")

ganadores_ml %>%
  mutate(
    respuesta = paste0(
      str_to_title(busqueda),
      ": ",
      supermercado,
      " es el más económico por ml."
    )
  ) %>%
  pull(respuesta) %>%
  cat(sep = "\n")

cantidad_ganadores <- n_distinct(ganadores_ml$supermercado)

if (cantidad_ganadores == 1) {
  cat(
    "\nEl supermercado más barato es consistente en las tres marcas:",
    unique(ganadores_ml$supermercado),
    "\n"
  )
} else {
  cat(
    "\nEl supermercado más barato cambia dependiendo de la marca.\n"
  )
}

cat("\n====================================\n")
cat("PREGUNTA 4\n")
cat("====================================\n")

presentaciones_pequenas <- analisis %>%
  filter(
    !is.na(volumen_ml),
    !is.na(precio_ml),
    is.finite(precio_ml)
  ) %>%
  group_by(busqueda, supermercado) %>%
  slice_min(
    volumen_ml,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    busqueda,
    supermercado,
    producto,
    volumen_ml,
    precio_num,
    precio_ml
  ) %>%
  arrange(busqueda, precio_ml)

print(presentaciones_pequenas)

ganadores_equivalentes <- presentaciones_pequenas %>%
  group_by(busqueda) %>%
  slice_min(
    precio_ml,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

cat("\nRespuesta pregunta 4:\n")

ganadores_equivalentes %>%
  mutate(
    respuesta = paste0(
      str_to_title(busqueda),
      ": ",
      supermercado,
      " tiene la presentación comparable más económica, con ",
      volumen_ml,
      " ml y un precio de ",
      round(precio_ml, 2),
      " COP/ml."
    )
  ) %>%
  pull(respuesta) %>%
  cat(sep = "\n")

cat("\n\n====================================\n")
cat("PREGUNTA 5\n")
cat("====================================\n")

cat(
  "Los resultados irrelevantes se eliminaron antes del análisis.\n",
  "El filtro conserva productos relacionados con cerveza, Michelob,\n",
  "Stella Artois o Club Colombia y descarta resultados que no\n",
  "correspondan a las cervezas buscadas.\n"
)

cat("\n====================================\n")
cat("RESUMEN FINAL\n")
cat("====================================\n")

resumen_final <- ganador_precio %>%
  select(
    cerveza = busqueda,
    supermercado_mas_barato = supermercado,
    precio_promedio = precio_promedio
  ) %>%
  left_join(
    ganadores_ml %>%
      select(
        cerveza = busqueda,
        supermercado_por_ml = supermercado,
        precio_por_ml = precio_ml_promedio
      ),
    by = "cerveza"
  )

print(resumen_final)

grafico <- ggplot(
  ml_por_marca,
  aes(
    x = busqueda,
    y = precio_ml_promedio,
    fill = supermercado
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Precio promedio por mililitro",
    x = "Cerveza",
    y = "Precio por ml (COP)",
    fill = "Supermercado"
  ) +
  theme_minimal()

print(grafico)

ggsave(
  "resultados/comparativa_precio_por_ml.png",
  grafico,
  width = 8,
  height = 5
)

cat(
  "\nProceso terminado.\n",
  "Archivo CSV: ",
  archivo_csv,
  "\n"
)
