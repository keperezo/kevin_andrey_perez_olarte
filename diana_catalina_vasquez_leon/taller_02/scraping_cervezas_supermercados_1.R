# ==============================================================================
# Taller práctico de Web Scraping - Precios de cerveza
# Michelob, Stella Artois y Club Colombia en Éxito, Carulla y Jumbo
# ==============================================================================
#
# Verificaciones previas realizadas antes de este script (documentar en el
# reporte / entregable):
#
#   - robots.txt de www.exito.com: la regla "Disallow: /*?" bloquea
#     cualquier URL con "?", incluida la ruta de búsqueda "/s?q=...".
#     Ninguna de las excepciones (Allow: /*?idsku=, /*?skuid=, /*?page=)
#     aplica a la URL de búsqueda usada aquí.
#   - robots.txt de www.carulla.com: la regla "Disallow: /s?" bloquea
#     explícitamente la ruta de búsqueda usada en este script.
#   - robots.txt de www.jumbocolombia.com: no se encontró ninguna regla
#     Disallow que aplique a la ruta "/search?query=...&type=term" usada
#     aquí; el buscador de Jumbo está permitido.
#
#   LIMITACIÓN DOCUMENTADA: se decidió continuar con el scraping de los
#   3 supermercados para completar el ejercicio asignado por el docente,
#   pero se deja constancia de que Éxito y Carulla restringen su buscador
#   vía robots.txt, a diferencia de Jumbo. Se recomienda consultar con el
#   docente sobre el manejo de esta situación en un contexto no académico.
#
#   Todos los selectores usados fueron verificados manualmente contra el
#   HTML real (renderizado con read_html_live/chromote), no asumidos.
# ==============================================================================

# 0. Cargar paquetes -------------------------------------------------------
paquetes <- c("rvest", "xml2", "httr2", "dplyr", "stringr", "tidyr",
              "purrr", "tibble", "janitor", "readr", "chromote")

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)
invisible(lapply(paquetes, library, character.only = TRUE))

chrome_disponible <- tryCatch({ chromote::chromote_info(); TRUE }, error = function(e) FALSE)
if (!chrome_disponible) {
  stop("Chrome no está disponible. Ábralo una vez manualmente y reinicie RStudio.")
}

cervezas <- c("michelob", "stella artois", "club colombia")


# ==============================================================================
# 1. JUMBO
# ==============================================================================

construir_url_jumbo <- function(termino) {
  paste0("https://www.jumbocolombia.com/search?query=", URLencode(termino), "&type=term")
}

extraer_productos_jumbo <- function(nodo) {
  marca        <- nodo %>% html_element(".jumbo-ds-ProductCardstyles-1szk4om") %>% html_text2()
  presentacion <- nodo %>% html_element(".jumbo-ds-ProductCardstyles-1szk4ol") %>% html_text2()
  precio_texto <- nodo %>% html_element(".jumbo-ds-ProductCardstyles-1szk4on") %>% html_text2()
  tibble(presentacion = presentacion, precio_texto = precio_texto)
}

scrapear_jumbo <- function(termino, pausa = 4) {
  url <- construir_url_jumbo(termino)

  sesion <- tryCatch(read_html_live(url), error = function(e) {
    message("Jumbo (", termino, "): error al abrir la página - ", conditionMessage(e))
    NULL
  })
  if (is.null(sesion)) return(tibble(presentacion = character(), precio_texto = character()))

  on.exit(tryCatch(sesion$session$close(), error = function(e) NULL), add = TRUE)
  Sys.sleep(pausa)

  nodos <- sesion %>% html_elements(".jumbo-ds-ProductCardstyles-1szk4o4")
  if (length(nodos) == 0) {
    message("Jumbo (", termino, "): no se encontraron productos.")
    return(tibble(presentacion = character(), precio_texto = character()))
  }

  map_dfr(nodos, extraer_productos_jumbo) %>%
    filter(str_detect(str_to_lower(presentacion), "cerveza")) %>%
    mutate(termino_busqueda = termino, supermercado = "Jumbo")
}


# ==============================================================================
# 2. ÉXITO y CARULLA (misma plataforma FastStore/VTEX, mismos selectores)
# ==============================================================================

construir_url_exito <- function(termino) {
  paste0("https://www.exito.com/s?q=", URLencode(termino), "&sort=score_desc&page=0")
}

construir_url_carulla <- function(termino) {
  paste0("https://www.carulla.com/s?q=", URLencode(termino), "&sort=score_desc&page=0")
}

extraer_productos_faststore <- function(pagina) {
  nombres       <- pagina %>% html_elements(".styles_name__qQJiK") %>% html_text2()
  precios_texto <- pagina %>% html_elements(".ProductPrice_container__price__XmMWA") %>% html_text2()

  if (length(nombres) == 0 || length(precios_texto) == 0) {
    return(tibble(presentacion = character(), precio_texto = character()))
  }
  if (length(nombres) != length(precios_texto)) {
    warning("Nombres (", length(nombres), ") y precios (", length(precios_texto),
            ") no coinciden; se omite esta página para evitar emparejar mal.")
    return(tibble(presentacion = character(), precio_texto = character()))
  }
  tibble(presentacion = nombres, precio_texto = precios_texto)
}

scrapear_faststore <- function(termino, construir_url, nombre_super, pausa = 4) {
  url <- construir_url(termino)

  sesion <- tryCatch(read_html_live(url), error = function(e) {
    message(nombre_super, " (", termino, "): error al abrir la página - ", conditionMessage(e))
    NULL
  })
  if (is.null(sesion)) return(tibble(presentacion = character(), precio_texto = character()))

  on.exit(tryCatch(sesion$session$close(), error = function(e) NULL), add = TRUE)
  Sys.sleep(pausa)

  extraer_productos_faststore(sesion) %>%
    filter(str_detect(str_to_lower(presentacion), "cerveza")) %>%
    mutate(termino_busqueda = termino, supermercado = nombre_super)
}

scrapear_exito   <- function(termino, pausa = 4) scrapear_faststore(termino, construir_url_exito,   "Éxito",   pausa)
scrapear_carulla <- function(termino, pausa = 4) scrapear_faststore(termino, construir_url_carulla, "Carulla", pausa)


# ==============================================================================
# 3. Ejecutar el scraping para las 3 cervezas x 3 supermercados
#    (con manejo de errores por sitio: si uno falla, los demás continúan)
# ==============================================================================

scrapear_super_seguro <- function(fn, termino) {
  tryCatch(
    fn(termino),
    error = function(e) {
      message("Fallo no controlado: ", conditionMessage(e))
      tibble(presentacion = character(), precio_texto = character(),
             termino_busqueda = character(), supermercado = character())
    }
  )
}

resultados <- list()

for (cerveza in cervezas) {
  cat("\n=== Buscando:", cerveza, "===\n")

  cat("  -> Jumbo...\n")
  resultados[[paste("jumbo", cerveza)]]   <- scrapear_super_seguro(scrapear_jumbo, cerveza)

  cat("  -> Éxito...\n")
  resultados[[paste("exito", cerveza)]]   <- scrapear_super_seguro(scrapear_exito, cerveza)

  cat("  -> Carulla...\n")
  resultados[[paste("carulla", cerveza)]] <- scrapear_super_seguro(scrapear_carulla, cerveza)
}

datos_crudos <- list_rbind(resultados)

cat("\nTotal de filas obtenidas:", nrow(datos_crudos), "\n")


# ==============================================================================
# 4. Limpieza: precio a numérico, extracción de volumen en ml (regex)
# ==============================================================================

datos_limpios <- datos_crudos %>%
  mutate(
    precio = precio_texto %>%
      str_replace_all("[^0-9]", "") %>%
      as.numeric(),

    # Extrae el primer número seguido de "ml" (con o sin espacio), p.ej.
    # "sixpack (1980 ml)" -> 1980 ; "lata (269ml)" -> 269
    volumen_ml_base = str_extract(presentacion, "[0-9]+(?=\\s*ml)") %>%
      as.numeric(),

    # Detectar si la presentación es un pack de 6 unidades ("x6", "sixpack",
    # "six pack"). IMPORTANTE: Éxito y Carulla expresan el volumen YA como el
    # TOTAL del pack (ej. "sixpack (1980 ml)" = 6 x 330 ml), mientras que
    # Jumbo expresa el volumen de UNA unidad dentro del pack (ej. "Sixpack
    # Lata x269ml" = 6 x 269 ml = 1614 ml reales). Por eso el mismo texto
    # "269ml"/"330ml" en Jumbo describe la unidad, no el total, y hay que
    # multiplicarlo por 6 para que sea comparable con el precio del pack.
    es_pack_x6 = str_detect(str_to_lower(presentacion), "x\\s*6|sixpack|six\\s*pack"),
    volumen_parece_unitario = volumen_ml_base <= 400,  # 250-400 ml es tamaño típico de unidad

    volumen_ml = case_when(
      supermercado == "Jumbo" & es_pack_x6 & volumen_parece_unitario ~ volumen_ml_base * 6,
      TRUE ~ volumen_ml_base
    ),

    precio_por_ml = round(precio / volumen_ml, 2)
  ) %>%
  filter(!is.na(precio), !is.na(volumen_ml), volumen_ml > 0)

cat("\nFilas después de limpieza (con precio y volumen válidos):", nrow(datos_limpios), "\n")


# ==============================================================================
# 5. Guardar resultados en CSV
# ==============================================================================

dir.create("resultados", showWarnings = FALSE)
write_csv(datos_crudos,  file.path("resultados", "cervezas_crudo.csv"))
write_csv(datos_limpios, file.path("resultados", "cervezas_limpio.csv"))
cat("\nArchivos guardados en la carpeta 'resultados/'.\n")


# ==============================================================================
# 6. PREGUNTAS DE ANÁLISIS
# ==============================================================================

# Ayuda para identificar a qué cerveza pertenece cada producto, ya que el
# nombre de "presentacion" no siempre repite literalmente el término buscado
# tal cual (p.ej. "Cerveza Belgica Sixpack STELLA ARTOIS...")
datos_limpios <- datos_limpios %>%
  mutate(
    cerveza = case_when(
      str_detect(str_to_lower(presentacion), "michelob")        ~ "Michelob",
      str_detect(str_to_lower(presentacion), "stella")           ~ "Stella Artois",
      str_detect(str_to_lower(presentacion), "club colombia")    ~ "Club Colombia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cerveza))

cat("\n\n=====================================================\n")
cat("PREGUNTA 1: supermercado más barato por cerveza (precio TOTAL)\n")
cat("=====================================================\n")
pregunta1 <- datos_limpios %>%
  group_by(cerveza, supermercado) %>%
  summarise(precio_min = min(precio), .groups = "drop") %>%
  group_by(cerveza) %>%
  slice_min(precio_min, n = 1)
print(pregunta1)

cat("\n=====================================================\n")
cat("PREGUNTA 2: precio promedio por mililitro, por supermercado\n")
cat("=====================================================\n")
pregunta2 <- datos_limpios %>%
  group_by(supermercado) %>%
  summarise(
    precio_ml_promedio = round(mean(precio_por_ml, na.rm = TRUE), 2),
    n_productos = n(),
    .groups = "drop"
  ) %>%
  arrange(precio_ml_promedio)
print(pregunta2)
cat("\nSupermercado más conveniente por precio/ml:", pregunta2$supermercado[1], "\n")

cat("\n=====================================================\n")
cat("PREGUNTA 3: ¿el más barato cambia según la cerveza?\n")
cat("=====================================================\n")
print(pregunta1)
n_supers_distintos <- n_distinct(pregunta1$supermercado)
if (n_supers_distintos == 1) {
  cat("\nUn solo supermercado es consistentemente el más barato en las 3 cervezas:",
      unique(pregunta1$supermercado), "\n")
} else {
  cat("\nEl supermercado más barato SÍ cambia según la cerveza (", n_supers_distintos,
      "supermercados distintos ganan en al menos una cerveza).\n")
}
cat("\nNota importante: esta comparación usa precio TOTAL sin controlar por\n")
cat("presentación, por lo que puede favorecer a un supermercado simplemente\n")
cat("porque su resultado más barato es un six-pack grande, no porque su\n")
cat("cerveza individual sea más económica. Ver Pregunta 4 para la comparación\n")
cat("correcta por presentaciones equivalentes.\n")

cat("\n=====================================================\n")
cat("PREGUNTA 4: comparando SOLO presentaciones individuales equivalentes\n")
cat("            (se excluyen six-packs; se compara la lata/botella\n")
cat("            individual más pequeña, solo si es de un tamaño real\n")
cat("            de unidad, no un pack completo)\n")
cat("=====================================================\n")

# Excluimos explícitamente los packs (x6/sixpack) para no repetir el
# problema de comparar un six-pack de Jumbo contra latas sueltas de otros.
pregunta4 <- datos_limpios %>%
  filter(!es_pack_x6) %>%
  group_by(cerveza, supermercado) %>%
  slice_min(volumen_ml, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cerveza, supermercado, presentacion, volumen_ml, precio)
print(pregunta4, n = Inf)

if (any(table(pregunta4$cerveza) < 3)) {
  cat("\nNota: al menos un supermercado no ofrece presentación individual\n")
  cat("(no-pack) para alguna cerveza, por lo que la comparación para esa\n")
  cat("marca queda incompleta en ese supermercado.\n")
}

pregunta4_ganador <- pregunta4 %>%
  group_by(cerveza) %>%
  slice_min(precio, n = 1)
cat("\nSupermercado más barato en presentación individual equivalente, por cerveza:\n")
print(pregunta4_ganador)

cat("\n=====================================================\n")
cat("PREGUNTA 5: filtrado de resultados irrelevantes (ej. llantas Michelin)\n")
cat("=====================================================\n")
cat("
Se filtraron los resultados no relacionados con cerveza exigiendo que la
palabra 'cerveza' apareciera en el nombre/presentación del producto
(str_detect(str_to_lower(presentacion), 'cerveza')). Esto se aplicó DENTRO
de cada función scrapear_*() antes de consolidar resultados.

Evidencia concreta encontrada durante el desarrollo: al buscar \"michelob\"
en Éxito y Carulla, los resultados incluían llantas de las marcas Michelin,
Pirelli y Kontrol (ej. 'Llanta Michelin 185/65 R15 Energy Xm2 +'), ya que
comparten la cadena de texto 'michel' con 'Michelob'. El filtro por la
palabra exacta 'cerveza' eliminó estos falsos positivos sin excluir
ninguna presentación real de cerveza.
")

