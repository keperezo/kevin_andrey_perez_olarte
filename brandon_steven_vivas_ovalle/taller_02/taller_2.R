# =============================================================================
# Ejercicio 7.2 - Taller de Web Scraping con R (Parte 2)
# Comparación de precios de cerveza (Michelob, Stella Artois, Club Colombia)
# entre Éxito, Carulla y Jumbo
#
# Los tres supermercados pertenecen al mismo grupo empresarial y corren sobre
# la plataforma VTEX, así que comparten una estructura de búsqueda y de HTML
# muy similar. Aun así, revise con el inspector del navegador (F12) si los
# selectores CSS siguen vigentes: en sitios reales pueden cambiar sin aviso.
#
# Requiere: R, RStudio y Google Chrome instalado (para read_html_live()).
# =============================================================================

library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# ---- 0. Configuración general ----------------------------------------------

cervezas <- c("michelob", "stella artois", "club colombia")

supermercados <- tibble::tibble(
  supermercado = c("Exito", "Carulla", "Jumbo"),
  base_url = c(
    "https://www.exito.com",
    "https://www.carulla.com",
    "https://www.tiendasjumbo.co"
  )
)

chrome_disponible <- function() {
  tryCatch({
    chromote::default_chromote_object()
    TRUE
  }, error = function(e) FALSE)
}

if (!chrome_disponible()) {
  stop("Chrome no fue detectado. Revise que Google Chrome esté instalado.")
}

# ---- 1. Construcción de la URL de búsqueda (patrón típico de VTEX) --------

construir_url_busqueda <- function(base_url, termino) {
  termino_url <- utils::URLencode(termino, reserved = TRUE)
  paste0(base_url, "/", termino_url, "?_q=", termino_url, "&map=ft")
}

# ---- 2. Tabla vacía (misma estructura que un resultado exitoso) -----------

tabla_productos_vacia <- function() {
  tibble::tibble(
    presentacion    = character(),
    precio          = character(),
    supermercado    = character(),
    cerveza_buscada = character()
  )
}

# ---- 3. Extraer un producto individual -------------------------------------
# Nota: los selectores de VTEX suelen tener el patrón
# "vtex-<modulo>-<version>-x-<elemento>". Verifíquelos antes de correr el
# script, ya que pueden variar levemente entre las tres tiendas.

extraer_producto_vtex <- function(nodo, supermercado, cerveza_buscada) {

  presentacion <- nodo %>%
    html_element(
      ".vtex-product-summary-2-x-productBrand, .vtex-product-summary-2-x-productNameContainer, .vtex-product-summary-2-x-nameContainer"
    ) %>%
    html_text2()

  precio <- nodo %>%
    html_element(
      ".vtex-product-price-1-x-sellingPriceValue, .vtex-product-price-1-x-sellingPrice"
    ) %>%
    html_text2()

  tibble::tibble(
    presentacion    = presentacion,
    precio          = precio,
    supermercado    = supermercado,
    cerveza_buscada = cerveza_buscada
  )
}

# ---- 4. Scrapear una búsqueda (un supermercado, una cerveza) ---------------

scrapear_busqueda_vtex <- function(base_url, supermercado, cerveza, pausa = 4) {

  url <- construir_url_busqueda(base_url, cerveza)
  sesion <- rvest::read_html_live(url)
  on.exit(sesion$session$close(), add = TRUE)

  Sys.sleep(pausa)         # esperar a que cargue el JavaScript
  sesion$scroll_by(top = 1200)
  Sys.sleep(2)             # esperar a que cargue contenido perezoso (lazy load)

  nodos <- sesion %>%
    html_elements(".vtex-search-result-3-x-galleryItem")

  if (length(nodos) == 0) {
    return(tabla_productos_vacia())
  }

  purrr::map_dfr(
    nodos,
    extraer_producto_vtex,
    supermercado    = supermercado,
    cerveza_buscada = cerveza
  )
}

# Versión seguRA: si un supermercado falla (timeout, bloqueo, cambio de HTML,
# etc.) no interrumpe el resto del proceso; devuelve una tabla vacía.
scrapear_busqueda_segura <- purrr::possibly(
  scrapear_busqueda_vtex,
  otherwise = tabla_productos_vacia()
)

# ---- 5. Recorrer supermercados x cervezas ----------------------------------

combinaciones <- tidyr::crossing(supermercados, cerveza = cervezas)

resultados <- purrr::pmap_dfr(combinaciones, function(supermercado, base_url, cerveza) {
  mensaje <- sprintf("Buscando '%s' en %s...", cerveza, supermercado)
  message(mensaje)

  res <- scrapear_busqueda_segura(base_url, supermercado, cerveza, pausa = 4)

  Sys.sleep(3)  # pausa entre solicitudes, entre combinación y combinación
  res
})

# ---- 6. Guardar resultados crudos ------------------------------------------

dir.create("resultados", showWarnings = FALSE)
ruta_salida <- file.path("resultados", "cervezas_supermercados.csv")
write_csv(resultados, ruta_salida)
ruta_salida

# =============================================================================
# ANÁLISIS
# =============================================================================

# ---- 7. Filtrar resultados irrelevantes ------------------------------------
# Al buscar "michelob" pueden aparecer productos que no son cerveza (por
# ejemplo, llantas de una marca con nombre parecido). Filtramos por palabras
# clave típicas de presentación de cerveza y excluimos categorías que no
# tienen nada que ver (llantas, neumáticos, cascos, ropa, etc.).

palabras_cerveza <- c("cerveza|lata|botella|six\\s*pack|pack x|ml|litro|l\\b")
palabras_excluir  <- c("llanta|neumatico|neumático|caucho|moto|carro|rin|casco")

resultados_filtrados <- resultados %>%
  filter(!is.na(presentacion), !is.na(precio)) %>%
  mutate(presentacion_norm = tolower(presentacion)) %>%
  filter(str_detect(presentacion_norm, regex(palabras_cerveza, ignore_case = TRUE))) %>%
  filter(!str_detect(presentacion_norm, regex(palabras_excluir, ignore_case = TRUE)))

# ---- 8. Limpiar el precio a numérico ----------------------------------------
# Los precios en estos sitios suelen venir como "$ 12.500" o "COP 12.500".

resultados_limpios <- resultados_filtrados %>%
  mutate(
    precio_num = precio %>%
      str_remove_all("[^0-9,\\.]") %>%   # dejar solo dígitos y separadores
      str_remove_all("\\.") %>%          # quitar separador de miles (punto)
      str_replace(",", ".") %>%          # si hay coma decimal, pasar a punto
      as.numeric()
  )

# ---- 9. Extraer el volumen en mililitros con expresión regular ------------
# Cubre patrones como "355 ml", "355ml", "1 l", "1.5 l", "1L", "six pack x6 355ml".

extraer_volumen_ml <- function(texto) {
  texto <- tolower(texto)

  # buscar mL directo
  m_ml <- str_match(texto, "(\\d+[\\.,]?\\d*)\\s*ml")
  # buscar litros
  m_l  <- str_match(texto, "(\\d+[\\.,]?\\d*)\\s*l\\b")
  # buscar "x N" (unidades en un six pack) para multiplicar
  m_x  <- str_match(texto, "x\\s*(\\d+)")

  volumen_unidad <- if (!is.na(m_ml[1, 2])) {
    as.numeric(str_replace(m_ml[1, 2], ",", "."))
  } else if (!is.na(m_l[1, 2])) {
    as.numeric(str_replace(m_l[1, 2], ",", ".")) * 1000
  } else {
    NA_real_
  }

  unidades <- if (!is.na(m_x[1, 2])) as.numeric(m_x[1, 2]) else 1

  volumen_unidad * unidades
}

resultados_limpios <- resultados_limpios %>%
  mutate(volumen_ml = map_dbl(presentacion, extraer_volumen_ml))

# ---- 10. Precio total más barato por cerveza y supermercado ---------------

comparacion_precio_total <- resultados_limpios %>%
  filter(!is.na(precio_num)) %>%
  group_by(cerveza_buscada, supermercado) %>%
  summarise(precio_min = min(precio_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(cerveza_buscada) %>%
  slice_min(precio_min, n = 1) %>%
  ungroup()

print(comparacion_precio_total)

# ---- 11. Precio promedio por mililitro (comparación justa) ----------------

resultados_por_ml <- resultados_limpios %>%
  filter(!is.na(precio_num), !is.na(volumen_ml), volumen_ml > 0) %>%
  mutate(precio_por_ml = precio_num / volumen_ml)

comparacion_precio_ml <- resultados_por_ml %>%
  group_by(supermercado) %>%
  summarise(precio_ml_promedio = mean(precio_por_ml, na.rm = TRUE), .groups = "drop") %>%
  arrange(precio_ml_promedio)

print(comparacion_precio_ml)

comparacion_precio_ml_por_cerveza <- resultados_por_ml %>%
  group_by(cerveza_buscada, supermercado) %>%
  summarise(precio_ml_promedio = mean(precio_por_ml, na.rm = TRUE), .groups = "drop") %>%
  arrange(cerveza_buscada, precio_ml_promedio)

print(comparacion_precio_ml_por_cerveza)

# ---- 12. ¿Cambia el supermercado más barato según la cerveza? --------------

mas_barato_por_cerveza <- comparacion_precio_ml_por_cerveza %>%
  group_by(cerveza_buscada) %>%
  slice_min(precio_ml_promedio, n = 1) %>%
  ungroup()

print(mas_barato_por_cerveza)
# Si la columna 'supermercado' es la misma para las tres cervezas,
# hay un supermercado consistentemente más económico; si cambia,
# la respuesta depende de la marca.

# ---- 13. Comparar solo presentaciones equivalentes (ej. lata individual) --
# Definimos "lata individual pequeña" como una presentación de una sola
# unidad (no six pack) con volumen entre 269 y 375 mL, que es el rango
# típico de latas individuales de cerveza en Colombia.

latas_individuales <- resultados_por_ml %>%
  filter(volumen_ml >= 269, volumen_ml <= 375) %>%
  filter(!str_detect(tolower(presentacion), "pack|six|x\\s*[2-9]"))

comparacion_lata_individual <- latas_individuales %>%
  group_by(cerveza_buscada, supermercado) %>%
  summarise(precio_promedio = mean(precio_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(cerveza_buscada) %>%
  slice_min(precio_promedio, n = 1) %>%
  ungroup()

print(comparacion_lata_individual)
# Compare esta tabla con 'comparacion_precio_total': si el supermercado
# ganador coincide, la conclusión inicial se mantiene; si no, la diferencia
# de tamaños/presentaciones estaba distorsionando la comparación.

# ---- Nota final -------------------------------------------------------------
# Este script debe ejecutarse en un entorno local con R, RStudio y Google
# Chrome instalado, ya que read_html_live() necesita abrir un navegador real
# para renderizar el JavaScript de estos sitios. Los selectores CSS incluidos
# son los típicos de tiendas VTEX, pero verifíquelos con el inspector del
# navegador antes de correr el script completo, porque pueden cambiar.
