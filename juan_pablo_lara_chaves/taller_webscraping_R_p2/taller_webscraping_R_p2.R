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

diagnostico <- tibble(
  elemento = c("Sistema operativo", "Versión de R", "Versión de rvest", "Versión de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)

if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior. Ejecute install.packages('rvest') y reinicie RStudio.")
}

chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)

chrome_disponible

# inicio ejercicio ----
# sacar urls

# 1. Definir los vectores
cervezas <- c("cerveza michelob", "stella artois", "club colombia")
supermercados <- c("exito", "carulla", "jumbo")

# 2. Crear todas las combinaciones
combinaciones <- expand.grid(
  supermercado = supermercados,
  cerveza      = cervezas,
  stringsAsFactors = FALSE
)

# 3. Construir las URLs según la estructura y codificación de cada sitio
combinaciones <- combinaciones %>%
  mutate(
    # Éxito/Carulla usan '+' para espacios; Jumbo usa '%20'
    termino_plus = str_replace_all(cerveza, " ", "+"),   # "stella+artois"
    termino_pct  = URLencode(cerveza, reserved = FALSE), # "stella%20artois"
    
    url = case_when(
      supermercado %in% c("exito", "carulla") ~
        paste0(
          "https://www.", supermercado, ".com/s?q=",
          termino_plus,
          "&sort=score_desc&page=0"
        ),
      supermercado == "jumbo" ~
        paste0(
          "https://www.jumbocolombia.com/search?query=",
          termino_pct,
          "&type=term"
        )
    )
  )

# 4. Ver el resultado
combinaciones %>%
  select(supermercado, cerveza, url) %>%
  print(row.names = FALSE)

chrome <- "C:/Program Files/Google/Chrome/Application/chrome.exe"

for (i in seq_along(combinaciones$url)) {
  system2(chrome, args = c("--new-tab", combinaciones$url[i]), wait = FALSE)
  Sys.sleep(0.5)
}

# Aseguramos que Chrome sea visible (no headless)
Sys.setenv(CHROMOTE_HEADLESS = "false")

# Tomamos la primera URL de nuestra tabla: Éxito + cerveza michelob
url_prueba <- combinaciones$url[
  combinaciones$supermercado == "exito" &
    combinaciones$cerveza      == "cerveza michelob"
]
url_prueba

# Abrimos la página con el Chrome automatizado
pagina <- read_html_live(url_prueba)

# Traemos la ventana al frente para inspeccionarla
pagina$view()

# Damos tiempo a que cargue JavaScript y aparezcan los productos
Sys.sleep(6)

# Tanteo 1: article (típico en VTEX moderno)
nodos_a <- pagina %>% html_elements("article")
length(nodos_a)

# Tanteo 2: cualquier elemento cuya clase contenga 'productCard'
nodos_b <- pagina %>% html_elements("[class*='productCard']")
length(nodos_b)

# Tanteo 3: cualquier elemento cuya clase contenga 'product-summary'
nodos_c <- pagina %>% html_elements("[class*='product-summary']")
length(nodos_c)

# primer pagina: exito y cerveza michelob ----

# Tomamos el primer producto
primer_producto <- pagina %>% html_elements("article.productCard_productCard__M0677") %>% .[[1]]

# 1. Enlace del producto
primer_producto %>% html_element("a[data-testid='product-link']") %>% html_attr("href")

# 2. Título / presentación
primer_producto %>% html_element("h3[class*='styles_name']") %>% html_text2()

# 3. Precio actual (con descuento)
primer_producto %>% html_element("p[data-fs-container-price-otros='true']") %>% html_text2()

# 4. Precio tachado (opcional, por si lo quieres después)
primer_producto %>% html_element("[class*='price-dashed']") %>% html_text2()

# 5. Descuento (opcional)
primer_producto %>% html_element("span[data-percentage='true']") %>% html_text2()

# 6. Precio por ml ya calculado (dato útil del sitio, no lo pediremos)
primer_producto %>% html_element("[class*='product-unit_price-unit']") %>% html_text2()

# revisando con los demas productos de la pagina

nodos_producto <- pagina %>% html_elements("article[class*='productCard_productCard']")
length(nodos_producto)   # debe dar 8

# Aplicamos cada selector a TODAS las tarjetas
titulos <- nodos_producto %>% html_element("h3[class*='styles_name']") %>% html_text2()
precios <- nodos_producto %>% html_element("p[data-fs-container-price-otros='true']") %>% html_text2()
enlaces <- nodos_producto %>% html_element("a[data-testid='product-link']") %>% html_attr("href")

# Vemos el resultado
resutlado <- tibble(titulo = titulos, precio = precios, enlace = enlaces)

# todo bien!

# caso supermercados exito ----

extraer_producto_vtex <- function(nodo) {
  tibble(
    presentacion   = nodo %>% html_element("h3[class*='styles_name']") %>% html_text2(),
    precio_actual  = nodo %>% html_element("p[data-fs-container-price-otros='true']") %>% html_text2(),
    precio_lista   = nodo %>% html_element("[class*='price-dashed']") %>% html_text2(),
    descuento_pct  = nodo %>% html_element("span[data-percentage='true']") %>% html_text2(),
    enlace         = nodo %>% html_element("a[data-testid='product-link']") %>% html_attr("href")
  )
}

resultados_exito <- pagina %>%
  html_elements("article[class*='productCard_productCard']") %>%
  map_dfr(extraer_producto_vtex) %>%
  mutate(
    supermercado = "exito",
    cerveza      = "cerveza michelob"
  )

mostrar_tabla(resultados_exito, n = 8)
pagina$session$close()

# caso supermercado carulla ----

url_carulla <- combinaciones$url[
  combinaciones$supermercado == "carulla" &
    combinaciones$cerveza      == "cerveza michelob"
]

pagina_carulla <- read_html_live(url_carulla)
pagina_carulla$view()
Sys.sleep(6)

nodos_carulla <- pagina_carulla %>% html_elements("article[class*='productCard_productCard']")
length(nodos_carulla)   # esperamos ~8 o similar

# Extracción con la misma función de Éxito
resultados_carulla <- nodos_carulla %>%
  map_dfr(extraer_producto_vtex)

resultados_carulla
# Cerrar la sesión
pagina_carulla$session$close()

# caso supermercado jumbo ----
url_jumbo <- combinaciones$url[
  combinaciones$supermercado == "jumbo" &
    combinaciones$cerveza      == "cerveza michelob"
]
url_jumbo

pagina_jumbo <- read_html_live(url_jumbo)
pagina_jumbo$view()
Sys.sleep(6)

extraer_producto_jumbo <- function(nodo) {
  tibble(
    # Atributo limpio: nombre del producto (Jumbo lo expone directo)
    presentacion  = nodo %>% html_attr("data-cnstrc-item-name"),
    
    # Precio que paga el cliente (del span visible)
    precio_actual = nodo %>% html_element("span[class*='1szk4on']") %>% html_text2(),
    
    # Precio tachado (del span oculto cuando NO hay promo)
    precio_lista  = nodo %>% html_element("span[class*='1szk4oo']") %>% html_text2(),
    
    # Descuento (solo cuando hay promo)
    descuento_pct = nodo %>% html_element("span[class*='Badge']") %>% html_text2(),
    
    enlace        = nodo %>% html_element("a") %>% html_attr("href")
  )
}
# Contenedor: cualquier div con data-cnstrc-item-name
nodos_jumbo <- pagina_jumbo %>% html_elements("div[data-cnstrc-item-name]")
length(nodos_jumbo)

# Extracción
resultados_jumbo <- nodos_jumbo %>% map_dfr(extraer_producto_jumbo)

resultados_jumbo

pagina_jumbo$session$close()

# compactando todo ----

# ── VTEX (Éxito y Carulla) ──────────────────────────────────
extraer_producto_vtex <- function(nodo) {
  tibble(
    presentacion  = nodo %>% html_element("h3[class*='styles_name']") %>% html_text2(),
    precio_actual = nodo %>% html_element("p[data-fs-container-price-otros='true']") %>% html_text2(),
    precio_lista  = nodo %>% html_element("[class*='price-dashed']") %>% html_text2(),
    descuento_pct = nodo %>% html_element("span[data-percentage='true']") %>% html_text2(),
    enlace        = nodo %>% html_element("a[data-testid='product-link']") %>% html_attr("href")
  )
}

# ── Jumbo ───────────────────────────────────────────────────
extraer_producto_jumbo <- function(nodo) {
  tibble(
    presentacion  = nodo %>% html_attr("data-cnstrc-item-name"),
    precio_actual = nodo %>% html_element("span[class*='1szk4on']") %>% html_text2(),
    precio_lista  = nodo %>% html_element("span[class*='1szk4oo']") %>% html_text2(),
    descuento_pct = nodo %>% html_element("span[class*='Badge']") %>% html_text2(),
    enlace        = nodo %>% html_element("a") %>% html_attr("href")
  )
}

# ── Scraper VTEX (Éxito y Carulla) ──────────────────────────
scrapear_vtex <- function(url, pausa = 4, scrolls = 3) {
  sesion <- rvest::read_html_live(url)
  on.exit(sesion$session$close(), add = TRUE)
  
  Sys.sleep(pausa)
  for (i in seq_len(scrolls)) {
    sesion$scroll_by(top = 1200)
    Sys.sleep(1)
  }
  
  nodos <- sesion %>% html_elements("article[class*='productCard_productCard']")
  
  if (length(nodos) == 0) {
    return(tibble(presentacion = character(), precio_actual = character(),
                  precio_lista = character(), descuento_pct = character(),
                  enlace = character()))
  }
  map_dfr(nodos, extraer_producto_vtex)
}

scrapear_exito   <- scrapear_vtex
scrapear_carulla <- scrapear_vtex

# ── Scraper Jumbo ───────────────────────────────────────────
scrapear_jumbo <- function(url, pausa = 6, scrolls = 3) {
  sesion <- rvest::read_html_live(url)
  on.exit(sesion$session$close(), add = TRUE)
  
  Sys.sleep(pausa)
  for (i in seq_len(scrolls)) {
    sesion$scroll_by(top = 1500)
    Sys.sleep(1.5)
  }
  
  nodos <- sesion %>% html_elements("div[data-cnstrc-item-name]")
  
  if (length(nodos) == 0) {
    return(tibble(presentacion = character(), precio_actual = character(),
                  precio_lista = character(), descuento_pct = character(),
                  enlace = character()))
  }
  map_dfr(nodos, extraer_producto_jumbo)
}

scrapers <- list(
  exito   = scrapear_exito,
  carulla = scrapear_carulla,
  jumbo   = scrapear_jumbo
)

scrapear_supermercado <- function(supermercado, url, ...) {
  if (!supermercado %in% names(scrapers)) {
    stop("Supermercado no reconocido: ", supermercado)
  }
  scrapers[[supermercado]](url, ...)
}

scrapear_todo <- function(combinaciones) {
  map_dfr(seq_len(nrow(combinaciones)), function(i) {
    fila <- combinaciones[i, ]
    message(sprintf("[%d/%d] %s - %s",
                    i, nrow(combinaciones), fila$supermercado, fila$cerveza))
    
    # possibly() evita que un fallo detenga todo el proceso
    resultado <- possibly(scrapear_supermercado, otherwise = NULL)(
      fila$supermercado, fila$url
    )
    
    if (is.null(resultado) || nrow(resultado) == 0) {
      message("  → sin resultados")
      return(NULL)
    }
    
    resultado %>%
      mutate(supermercado = fila$supermercado,
             cerveza      = fila$cerveza)
  })
}

# Tarda ~1.5-2 min (abre y cierra Chrome 9 veces)
productos_crudos <- scrapear_todo(combinaciones)

# Guarda el crudo a disco por si necesitas reiniciar sin re-scrapear
dir.create("resultados", showWarnings = FALSE)
write_csv(productos_crudos, "resultados/cervezas_crudas.csv")

mostrar_tabla(productos_crudos, n = 15, caption = "Datos crudos consolidados")

# tratamiento estadistico para la resolucion de las preguntas ----

productos_filtrados <- productos_crudos %>%
  mutate(presentacion_lower = str_to_lower(presentacion)) %>%
  # 1. Excluir combos, promociones cruzadas y productos no-cerveza
  filter(!str_detect(presentacion_lower,
                     "combo|kit|pasabocas|maní|buchanan|piñata|clob|clobetasol")) %>%
  # 2. Excluir cualquier cosa sin la palabra "cerveza"
  filter(str_detect(presentacion_lower, "cerveza|beer|cerveja")) %>%
  # 3. Quedarse SOLO con productos de la marca buscada
  filter(
    (cerveza == "cerveza michelob" & str_detect(presentacion_lower, "michelob")) |
      (cerveza == "stella artois"    & str_detect(presentacion_lower, "stella"))   |
      (cerveza == "club colombia"    & str_detect(presentacion_lower, "club colombia"))
  ) %>%
  select(-presentacion_lower)

# tenemos los productos, ahora hay que extraer la informacion de ml totales para
# sacar precio por ml

productos_extraidos <- productos_filtrados %>%
  mutate(
    numeros = str_extract_all(presentacion, "\\d[\\d.,]*") %>%
      map(~ str_remove_all(.x, "[.,]")) %>%
      map(~ suppressWarnings(as.numeric(.x))) %>%
      map(~ .x[!is.na(.x)]),
    
    tiene_six = str_detect(str_to_lower(presentacion), "six\\s*pack|\\bsix\\b")
  )

calcular_ml_total <- function(numeros, tiene_six) {
  # Sin números → no se puede calcular
  if (length(numeros) == 0) return(NA_real_)
  
  max_num <- max(numeros)
  
  # Regla 1: número > 500 → ya es el total (paréntesis o pack grande)
  if (max_num > 500) return(max_num)
  
  # Regla 2: "six" → multiplicar por 6
  if (tiene_six) return(max_num * 6)
  
  # Regla 3: 2 números, ambos < 500 → multiplicarlos
  if (length(numeros) == 2) return(prod(numeros))
  
  # Regla 4: un solo número < 500 sin six → es el volumen individual
  max_num
}

productos_con_ml <- productos_extraidos %>%
  mutate(
    ml_total = map2_dbl(numeros, tiene_six, calcular_ml_total)
  )

precio_a_numero <- function(x) {
  x <- str_replace_all(x, "\\s|\\u00a0", "")
  x <- str_remove(x, "\\$")
  x <- ifelse(
    str_detect(x, "\\.\\d{3}$"),
    str_remove_all(x, "\\."),
    str_replace_all(x, "[.,]", ".")
  )
  suppressWarnings(as.numeric(x))
}

productos_final <- productos_con_ml %>%
  mutate(
    across(c(precio_actual, precio_lista), precio_a_numero),
    precio_lista = coalesce(precio_lista, precio_actual)
  )

productos_final <- productos_final %>%
  select(presentacion, ml_total, precio_actual, precio_lista, cerveza, supermercado) %>%
  mutate(pml_actual = precio_actual/ml_total,
       pml_lista = precio_lista/ml_total)

# resolucion de preguntas ----

# pregunta 1: precio total, mejor supermercado por cerveza

# con descuentos

precio_total <- productos_final %>%
  group_by(cerveza, supermercado) %>%
  summarise(
    precio_total_prom = mean(precio_actual, na.rm = TRUE),
    n_productos       = n(),
    .groups = "drop"
  ) %>%
  arrange(cerveza, precio_total_prom)

ganador_total <- precio_total %>%
  group_by(cerveza) %>%
  slice_min(precio_total_prom, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(cerveza)

ganador_total

# michelob: carulla
# club colombia: jumbo
# stella artois: carulla

# sin descuentos

precio_total <- productos_final %>%
  group_by(cerveza, supermercado) %>%
  summarise(
    precio_total_prom = mean(precio_lista, na.rm = TRUE),
    n_productos       = n(),
    .groups = "drop"
  ) %>%
  arrange(cerveza, precio_total_prom)

ganador_total <- precio_total %>%
  group_by(cerveza) %>%
  slice_min(precio_total_prom, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(cerveza)

ganador_total

# michelob: carulla
# club colombia: jumbo
# stella artois: jumbo

# pregunta 2: por ml, en general

precio_ml <- productos_final %>%
  filter(!is.na(ml_total), !is.na(pml_actual)) %>%
  group_by(cerveza, supermercado) %>%
  summarise(
    precio_ml_prom_con_desc = mean(pml_actual, na.rm = TRUE),
    precio_ml_prom_sin_desc = mean(pml_lista,  na.rm = TRUE),
    n_productos             = n(),
    .groups = "drop"
  ) %>%
  arrange(cerveza, precio_ml_prom_con_desc)

precio_ml_general <- precio_ml %>%
  group_by(supermercado) %>%
  summarise(
    precio_ml_prom_con_desc = mean(precio_ml_prom_con_desc, na.rm = TRUE),
    precio_ml_prom_sin_desc = mean(precio_ml_prom_sin_desc, na.rm = TRUE),
    n_marcas                = n(),
    n_productos_total       = sum(n_productos),
    .groups = "drop"
  ) %>%
  arrange(precio_ml_prom_con_desc)

precio_ml_general

# con descuento sale mejor comprar en carulla
# sin descuento sale mejor comprar en jumbo

# pregunta 3

precio_ml

# con base en los resultado de precio_ml vemos que independiente de si hay 
# descuento o no, el supermercado más barato cambia según la cerveza que se compare

# pregunta 4: comparacion equivalente

# para que sea justo solo se tendran en cuenta los precios de lista, es decir,
# sin descuentos

comparacion_equiv <- productos_final %>%
  filter(!is.na(ml_total), !is.na(pml_lista)) %>%
  # Solo (marca, ml) presentes en los 3 supermercados
  group_by(cerveza, ml_total) %>%
  filter(n_distinct(supermercado) >= 3) %>%
  ungroup() %>%
  # Promedio de precio/ml por celda
  group_by(cerveza, ml_total, supermercado) %>%
  summarise(precio_ml = mean(pml_lista, na.rm = TRUE), .groups = "drop") %>%
  arrange(cerveza, ml_total, precio_ml)

comparacion_equiv %>%
  group_by(supermercado) %>%
  summarise(precio_ml_prom = mean(precio_ml))

# se mantiene la conclusion de que SIN DESCUENTOS jumbo es el mas eco en general 

# pregunta 5:

# (1) se acotó la búsqueda usando "cerveza michelob" en lugar de solo "michelob"
# (2) se excluyeron por regex combos, promociones cruzadas y productos no-cerveza 
# (3) se exigió que el título contuviera la palabra "cerveza"
# (4) se exigió que contuviera la marca buscada, lo que descartó los resultados contaminantes de Jumbo (Heineken, Mahou).