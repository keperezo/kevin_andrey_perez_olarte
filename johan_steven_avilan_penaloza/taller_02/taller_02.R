paquetes <- c(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "tidyr"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

invisible(lapply(paquetes, library, character.only = TRUE))

cervezas <- c("michelob", "stella artois", "club colombia")

construir_url_super <- function(supermercado, termino) {
  termino_url <- URLencode(termino)
  switch(
    supermercado,
    "exito"   = paste0("https://www.exito.com/s?q=", termino_url),
    "carulla" = paste0("https://www.carulla.com/s?q=", termino_url),
    "jumbo"   = paste0("https://www.tiendasjumbo.co/", gsub(" ", "-", termino_url),
                        "?_q=", termino_url, "&map=ct")
  )
}

tabla_cervezas_vacia <- function() {
  tibble(
    presentacion = character(),
    precio = character(),
    cerveza = character(),
    supermercado = character()
  )
}

extraer_bloques_texto <- function(texto_body) {
  bloques <- str_split(texto_body, "\\nAgregar\\n?")[[1]]
  bloques[str_detect(bloques, "\\$")]
}

extraer_producto_bloque <- function(bloque) {
  presentacion <- str_split(bloque, "\\n")[[1]][1]
  precios <- str_extract_all(bloque, "\\$\\s?[0-9\\.]+")[[1]]
  precio <- if (length(precios) > 0) tail(precios, 1) else NA_character_
  tibble(presentacion = presentacion, precio = precio)
}

scrapear_super <- function(supermercado, cerveza, pausa = 3) {
  url <- construir_url_super(supermercado, cerveza)

  Sys.sleep(pausa)

  sesion <- read_html_live(url)
  on.exit(sesion$session$close(), add = TRUE)

  Sys.sleep(6)

  texto_body <- sesion %>%
    html_element("body") %>%
    html_text2()

  bloques <- extraer_bloques_texto(texto_body)

  cat(sprintf("%-8s | %-15s | bloques encontrados: %d\n",
              supermercado, cerveza, length(bloques)))

  if (length(bloques) == 0) {
    return(tabla_cervezas_vacia())
  }

  map_dfr(bloques, extraer_producto_bloque) %>%
    mutate(
      cerveza = cerveza,
      supermercado = supermercado
    )
}

scrapear_super_seguro <- purrr::possibly(
  scrapear_super,
  otherwise = tabla_cervezas_vacia()
)

combinaciones <- expand_grid(
  supermercado = c("exito", "carulla", "jumbo"),
  cerveza = cervezas
)

cat("=== Progreso del scraping ===\n")

resultados_crudos <- map2_dfr(
  combinaciones$supermercado,
  combinaciones$cerveza,
  ~ scrapear_super_seguro(.x, .y, pausa = 3)
)

cat("\n=== Diagnóstico: filas obtenidas por supermercado y cerveza ===\n")
diagnostico_scraping <- combinaciones %>%
  left_join(
    resultados_crudos %>% count(supermercado, cerveza, name = "filas_obtenidas"),
    by = c("supermercado", "cerveza")
  ) %>%
  mutate(filas_obtenidas = coalesce(filas_obtenidas, 0L))
print(diagnostico_scraping, n = Inf)

cat("\n=== Primeras filas crudas (antes de limpiar) ===\n")
print(head(resultados_crudos, 15))

if (nrow(resultados_crudos) == 0) {
  cat("\nNo se obtuvo ningún producto de ningún supermercado.\n")
  cat("Vuelva a correr el diagnóstico manual de una sola URL con Sys.sleep\n")
  cat("más largo, y confirme con html_text2() si el patrón de bloques\n")
  cat("sigue terminando en la palabra 'Agregar'.\n")
}

palabras_excluir <- "llanta|neumatico|tire|rin|moto|carro|repuesto"

cervezas_limpio <- resultados_crudos %>%
  filter(!is.na(presentacion), !is.na(precio)) %>%
  mutate(
    presentacion_low = str_to_lower(presentacion),
    es_relevante = str_detect(presentacion_low, str_to_lower(cerveza)) &
      !str_detect(presentacion_low, palabras_excluir),
    precio_numerico = precio %>%
      str_remove_all("[^0-9,\\.]") %>%
      str_remove_all("\\.") %>%
      str_replace(",", ".") %>%
      as.numeric(),
    unidades_pack = str_extract(presentacion_low, "^[0-9]+(?=\\s*(x|un|pack))") %>%
      as.numeric(),
    unidades_pack = coalesce(unidades_pack, 1),
    volumen_texto = str_extract(presentacion_low, "[0-9]+([\\.,][0-9]+)?\\s*(ml|l\\b)"),
    volumen_valor = str_extract(volumen_texto, "[0-9]+([\\.,][0-9]+)?") %>%
      str_replace(",", ".") %>%
      as.numeric(),
    es_litros = str_detect(volumen_texto, "l\\b") & !str_detect(volumen_texto, "ml"),
    volumen_ml_unidad = if_else(es_litros, volumen_valor * 1000, volumen_valor),
    volumen_ml_total = volumen_ml_unidad * unidades_pack,
    precio_por_ml = precio_numerico / volumen_ml_total
  ) %>%
  filter(es_relevante, !is.na(precio_numerico), !is.na(volumen_ml_total)) %>%
  select(cerveza, supermercado, presentacion, precio = precio_numerico,
         volumen_ml_total, precio_por_ml) %>%
  clean_names()

cat("\n=== Tabla limpia (cervezas_limpio) ===\n")
print(cervezas_limpio, n = Inf)

dir.create("resultados", showWarnings = FALSE)
ruta_salida <- file.path("resultados", "cervezas_supermercados.csv")
write_csv(cervezas_limpio, ruta_salida)
cat("\nArchivo guardado en:", ruta_salida, "\n")

if (nrow(cervezas_limpio) > 0) {

  mas_barata_por_precio_total <- cervezas_limpio %>%
    group_by(cerveza, supermercado) %>%
    summarise(precio_min = min(precio, na.rm = TRUE), .groups = "drop") %>%
    group_by(cerveza) %>%
    slice_min(precio_min, n = 1)

  precio_promedio_ml <- cervezas_limpio %>%
    group_by(supermercado) %>%
    summarise(precio_ml_promedio = mean(precio_por_ml, na.rm = TRUE)) %>%
    arrange(precio_ml_promedio)

  mas_barata_por_ml <- cervezas_limpio %>%
    group_by(cerveza, supermercado) %>%
    summarise(precio_ml_promedio = mean(precio_por_ml, na.rm = TRUE), .groups = "drop") %>%
    group_by(cerveza) %>%
    slice_min(precio_ml_promedio, n = 1)

  latas_individuales <- cervezas_limpio %>%
    filter(volumen_ml_total <= 400) %>%
    group_by(cerveza, supermercado) %>%
    slice_min(volumen_ml_total, n = 1) %>%
    ungroup() %>%
    group_by(cerveza) %>%
    slice_min(precio_por_ml, n = 1)

  cat("\n=== Más barata por precio total (por cerveza) ===\n")
  print(mas_barata_por_precio_total)

  cat("\n=== Precio promedio por ml (por supermercado) ===\n")
  print(precio_promedio_ml)

  cat("\n=== Más barata por precio/ml (por cerveza) ===\n")
  print(mas_barata_por_ml)

  cat("\n=== Comparación de latas individuales (<= 400 ml) ===\n")
  print(latas_individuales)

} else {
  cat("\ncervezas_limpio quedó vacío: revise la tabla cruda de arriba.\n")
}

