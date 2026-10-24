# 1. Librerías -------------------------------------------------
libs_necesarias <- c("rvest", "dplyr", "stringr", "purrr", "readr", "tibble")

ya_instaladas <- rownames(installed.packages())
faltan        <- setdiff(libs_necesarias, ya_instaladas)
if (length(faltan) > 0) install.packages(faltan)

invisible(lapply(libs_necesarias, library, character.only = TRUE))

chrome_disponible <- tryCatch({
  chromote::default_chromote_object()
  TRUE
}, error = function(e) FALSE)

if (!chrome_disponible) {
  stop("Chrome no fue detectado. Revisa la instalación de chromote antes de seguir.")
}

# 2. Qué buscamos y dónde ---------------------------------------
cervezas <- c("michelob", "stella artois", "club colombia")

# Un supermercado = un nombre + una función que arma la URL de
# búsqueda + los selectores CSS de ese sitio en particular.
# TODO: confirma la URL de búsqueda real de cada sitio (probando
# manualmente en el navegador qué URL genera al buscar "cerveza").
supermercados <- list(
  exito = list(
    nombre = "Éxito",
    url_busqueda = function(termino) {
      sprintf("https://www.exito.com/s?_q=%s&map=ft", URLencode(termino))
    },
    selector_tarjeta      = ".product-card",          # TODO: verificar
    selector_presentacion = ".product-card__name",     # TODO: verificar
    selector_precio       = ".product-price"           # TODO: verificar
  ),
  carulla = list(
    nombre = "Carulla",
    url_busqueda = function(termino) {
      sprintf("https://www.carulla.com/s?_q=%s&map=ft", URLencode(termino))
    },
    selector_tarjeta      = ".product-card",           # TODO: verificar
    selector_presentacion = ".product-card__name",      # TODO: verificar
    selector_precio       = ".product-price"            # TODO: verificar
  ),
  jumbo = list(
    nombre = "Jumbo",
    url_busqueda = function(termino) {
      sprintf("https://www.tiendasjumbo.co/s?_q=%s&map=ft", URLencode(termino))
    },
    selector_tarjeta      = ".product-card",            # TODO: verificar
    selector_presentacion = ".product-card__name",       # TODO: verificar
    selector_precio       = ".product-price"             # TODO: verificar
  )
)

# 3. Tabla vacía, por si un sitio no devuelve nada ---------------
tabla_productos_vacia <- function() {
  tibble(
    presentacion = character(),
    precio_texto = character(),
    cerveza      = character(),
    supermercado = character(),
    url_origen   = character()
  )
}

# 4. Sacar presentación + precio de UNA tarjeta de producto ------
extraer_producto <- function(nodo, sel_presentacion, sel_precio) {
  presentacion <- nodo %>% html_element(sel_presentacion) %>% html_text2() %>% str_squish()
  precio_texto <- nodo %>% html_element(sel_precio) %>% html_text2() %>% str_squish()
  tibble(presentacion = presentacion, precio_texto = precio_texto)
}

# 5. Scrapear una cerveza en un supermercado ---------------------
scrapear_busqueda <- function(cerveza, super_id, pausa = 4) {
  
  info_super <- supermercados[[super_id]]
  url <- info_super$url_busqueda(cerveza)
  
  sesion <- rvest::read_html_live(url)
  on.exit(sesion$session$close(), add = TRUE)
  
  # Le damos tiempo a que cargue el JavaScript antes de leer el HTML
  Sys.sleep(pausa)
  
  # Si el sitio carga resultados adicionales al hacer scroll,
  # esto ayuda a que aparezcan todos los productos
  sesion$scroll_by(top = 1200)
  Sys.sleep(2)
  
  nodos <- sesion %>% html_elements(info_super$selector_tarjeta)
  
  if (length(nodos) == 0) {
    message("  [SIN RESULTADOS] ", info_super$nombre, " - ", cerveza)
    return(tabla_productos_vacia())
  }
  
  map_dfr(nodos, extraer_producto,
          sel_presentacion = info_super$selector_presentacion,
          sel_precio       = info_super$selector_precio) %>%
    mutate(
      cerveza      = cerveza,
      supermercado = info_super$nombre,
      url_origen   = url
    )
}

# Versión "segura": si un supermercado falla, no se cae todo el script
scrapear_busqueda_segura <- purrr::possibly(scrapear_busqueda, otherwise = tabla_productos_vacia())

# 6. Recorrer las 3 cervezas x 3 supermercados -------------------
combinaciones <- expand.grid(
  cerveza  = cervezas,
  super_id = names(supermercados),
  stringsAsFactors = FALSE
)

cat("Consultando", nrow(combinaciones), "combinaciones de cerveza x supermercado...\n")

resultados_lista <- pmap(
  list(combinaciones$cerveza, combinaciones$super_id),
  function(cerveza, super_id) {
    cat("Buscando:", cerveza, "en", supermercados[[super_id]]$nombre, "\n")
    resultado <- scrapear_busqueda_segura(cerveza, super_id, pausa = 4)
    Sys.sleep(2)  # pausa extra entre solicitudes, para no saturar
    resultado
  }
)

datos_crudos <- list_rbind(resultados_lista)

if (nrow(datos_crudos) == 0) {
  stop("No se obtuvo ningún producto. Revisa los selectores CSS (marcados con TODO) inspeccionando los sitios en el navegador.")
}

# 7. Filtrar resultados irrelevantes -----------------------------
# Al buscar "michelob" pueden salir cosas que no son cerveza (por
# ejemplo llantas de una marca parecida). Nos quedamos solo con lo
# que realmente suena a cerveza, y descartamos por palabras clave
# que delaten que NO es cerveza.
palabras_cerveza   <- "cerveza|beer|lager|pilsen|stout|ale"
palabras_a_excluir <- "llanta|neumatico|neumático|caucho|repuesto"

datos_filtrados <- datos_crudos %>%
  filter(
    str_detect(str_to_lower(presentacion), palabras_cerveza) |
      !str_detect(str_to_lower(presentacion), palabras_a_excluir)
  ) %>%
  filter(!str_detect(str_to_lower(presentacion), palabras_a_excluir))

# 8. Limpiar el precio a numérico ---------------------------------
datos_filtrados <- datos_filtrados %>%
  mutate(
    precio = precio_texto %>%
      str_remove_all("[^0-9]") %>%
      as.numeric()
  )

# 9. Extraer el volumen en mililitros con una expresión regular ---
# Cubre casos como "355 ml", "330ml", "1 L", "1.5L", y six packs
# tipo "x6 330ml" (en ese caso multiplicamos por la cantidad).
extraer_ml <- function(texto) {
  texto_low <- str_to_lower(texto)
  
  # cantidad de unidades (six pack, x6, etc.) - si no hay, es 1
  cantidad <- str_extract(texto_low, "x\\s*(\\d+)") %>%
    str_extract("\\d+") %>%
    as.numeric()
  cantidad <- ifelse(is.na(cantidad), 1, cantidad)
  
  # volumen en litros
  litros <- str_extract(texto_low, "(\\d+[.,]?\\d*)\\s*l\\b") %>%
    str_extract("\\d+[.,]?\\d*") %>%
    str_replace(",", ".") %>%
    as.numeric()
  
  # volumen en mililitros
  mililitros <- str_extract(texto_low, "(\\d+[.,]?\\d*)\\s*ml") %>%
    str_extract("\\d+[.,]?\\d*") %>%
    str_replace(",", ".") %>%
    as.numeric()
  
  volumen_unitario <- case_when(
    !is.na(mililitros) ~ mililitros,
    !is.na(litros)     ~ litros * 1000,
    TRUE               ~ NA_real_
  )
  
  volumen_unitario * cantidad
}

datos_finales <- datos_filtrados %>%
  mutate(
    volumen_ml     = extraer_ml(presentacion),
    precio_por_ml  = precio / volumen_ml
  ) %>%
  select(cerveza, supermercado, presentacion, precio, volumen_ml, precio_por_ml, url_origen)

# 10. Guardar el resultado consolidado en CSV ----------------------
dir.create("resultados", showWarnings = FALSE)
write_csv(datos_finales, file.path("resultados", "precios_cerveza_supermercados.csv"))
cat("\n✅ Guardado 'resultados/precios_cerveza_supermercados.csv'\n")

# ============================================================
# 11. Respuestas a las preguntas del ejercicio
# ============================================================

# Pregunta 1: más barato por precio total, por cerveza -----------
mas_barato_total <- datos_finales %>%
  filter(!is.na(precio)) %>%
  group_by(cerveza) %>%
  slice_min(precio, n = 1, with_ties = FALSE) %>%
  select(cerveza, supermercado, presentacion, precio)

cat("\n--- Más barato por precio total (sin ver presentación) ---\n")
print(mas_barato_total)

# Pregunta 2: precio promedio por mililitro, por supermercado -----
precio_promedio_ml <- datos_finales %>%
  filter(!is.na(precio_por_ml)) %>%
  group_by(supermercado) %>%
  summarise(precio_promedio_ml = mean(precio_por_ml), .groups = "drop") %>%
  arrange(precio_promedio_ml)

cat("\n--- Precio promedio por ml, por supermercado ---\n")
print(precio_promedio_ml)

# Pregunta 3: ¿el más barato cambia según la cerveza? -------------
mas_barato_por_ml_y_cerveza <- datos_finales %>%
  filter(!is.na(precio_por_ml)) %>%
  group_by(cerveza, supermercado) %>%
  summarise(precio_promedio_ml = mean(precio_por_ml), .groups = "drop") %>%
  group_by(cerveza) %>%
  slice_min(precio_promedio_ml, n = 1, with_ties = FALSE)

cat("\n--- Supermercado más barato por ml, cerveza por cerveza ---\n")
print(mas_barato_por_ml_y_cerveza)

# Pregunta 4: comparar solo presentaciones equivalentes -----------
# Ejemplo: la lata individual más pequeña (330-355 ml) de cada marca
presentaciones_equivalentes <- datos_finales %>%
  filter(!is.na(volumen_ml), volumen_ml <= 400) %>%   # ~lata individual
  group_by(cerveza, supermercado) %>%
  slice_min(volumen_ml, n = 1, with_ties = FALSE) %>%
  select(cerveza, supermercado, presentacion, precio, volumen_ml)

cat("\n--- Comparación de latas individuales equivalentes ---\n")
print(presentaciones_equivalentes)

# Vista previa final
print(head(datos_finales, 10))
