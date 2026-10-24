# ==============================================================================
# TALLER 2 - WEB SCRAPING CON R
# Precios de cerveza en Exito, Carulla y Jumbo
# Juan Felipe Rojas Casallas
# ==============================================================================
# Los buscadores de los tres supermercados arman sus resultados con JavaScript
# despues de la carga inicial: con read_html() solo se obtienen bloques vacios
# de "carga" (skeletons). Por eso todo el ejercicio usa read_html_live().
# ==============================================================================


# ------------------------------------------------------------------------------
# Funcion auxiliar del curso
# ------------------------------------------------------------------------------
# Es la misma mostrar_tabla() del taller, adaptada para imprimir en la consola
# (en el taller devolvia HTML porque el documento se tejia con knitr).
mostrar_tabla <- function(x, n = 10, caption = NULL) {
  x_mostrar <- head(x, n)
  print(knitr::kable(x_mostrar, caption = caption))
}


# ------------------------------------------------------------------------------
# PASO 1: PAQUETES
# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# PASO 2: VERIFICACION RAPIDA DEL EQUIPO
# ------------------------------------------------------------------------------
diagnostico <- tibble(
  elemento = c("Sistema operativo", "Version de R", "Version de rvest", "Version de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)

mostrar_tabla(diagnostico, n = nrow(diagnostico), caption = "Entorno de ejecucion")

if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior. Ejecute install.packages('rvest') y reinicie RStudio.")
}

chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)

if (!chrome_disponible) {
  stop("Chrome no fue detectado. Abra Google Chrome una vez, cierrelo y reinicie RStudio.")
}


# ------------------------------------------------------------------------------
# PASO 3: PARAMETROS DE LA BUSQUEDA
# ------------------------------------------------------------------------------
marcas <- c("michelob", "stella artois", "club colombia")
supermercados <- c("exito", "carulla", "jumbo")


# ------------------------------------------------------------------------------
# PASO 4: CONSTRUCCION DE LA URL DE BUSQUEDA
# ------------------------------------------------------------------------------
# Igual que construir_url_amazon() del taller: el termino va codificado en la URL.
construir_url_super <- function(supermercado, termino) {
  termino_enc <- URLencode(termino)

  if (supermercado == "exito") {
    paste0("https://www.exito.com/s?q=", termino_enc, "&sort=score_desc&page=0")
  } else if (supermercado == "carulla") {
    paste0("https://www.carulla.com/s?q=", termino_enc, "&sort=score_desc&page=0")
  } else if (supermercado == "jumbo") {
    paste0("https://www.tiendasjumbo.co/search?query=", termino_enc, "&type=term")
  } else {
    stop("Supermercado no reconocido: ", supermercado)
  }
}

construir_url_super("exito", "club colombia")


# ------------------------------------------------------------------------------
# PASO 5: SELECTORES DE CADA SITIO
# ------------------------------------------------------------------------------
# Los selectores se revisaron sobre el HTML real de cada sitio.
# Exito y Carulla comparten plantilla; Jumbo usa clases propias.
selectores <- list(
  exito = list(
    contenedor = "article",
    titulo     = "h3",
    precio     = "[class*='ProductPrice_container__price']"
  ),
  carulla = list(
    contenedor = "article",
    titulo     = "h3",
    precio     = "[class*='ProductPrice_container__price']"
  ),
  jumbo = list(
    contenedor = "a[class*='ProductCardstyles']",
    titulo     = "h3",
    precio     = "span[class*='ProductCardstyles']"
  )
)


# ------------------------------------------------------------------------------
# PASO 6: TABLA VACIA DE RESPALDO
# ------------------------------------------------------------------------------
# Mantiene la estructura esperada cuando una busqueda no devuelve productos.
tabla_productos_vacia <- function() {
  tibble(
    presentacion = character(),
    precio       = character(),
    marca        = character(),
    supermercado = character()
  )
}


# ------------------------------------------------------------------------------
# PASO 7: EXTRAER LOS CAMPOS DE UN PRODUCTO
# ------------------------------------------------------------------------------
# Recibe el nodo HTML de un producto y la lista de selectores del sitio.
extraer_producto_super <- function(nodo, sel) {
  # Extraemos el nombre o presentacion del producto tal como aparece en el sitio
  presentacion <- nodo %>%
    html_element(sel$titulo) %>%
    html_text2()

  # Extraemos el precio mostrado en el resultado de busqueda
  precio <- nodo %>%
    html_element(sel$precio) %>%
    html_text2()

  tibble(
    presentacion = presentacion,
    precio       = precio
  )
}


# ------------------------------------------------------------------------------
# PASO 8: ABRIR LA PAGINA CON CHROME
# ------------------------------------------------------------------------------
# read_html_live() falla de forma intermitente en estos tres sitios
# ("timed out waiting for event Page.loadEventFired"), asi que reintentamos.
abrir_pagina_live <- function(url, intentos = 5, pausa = 5) {
  for (i in seq_len(intentos)) {
    sesion <- tryCatch(
      rvest::read_html_live(url),
      error = function(e) {
        message("   intento ", i, " fallido: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(sesion)) return(sesion)
    Sys.sleep(pausa)
  }
  NULL
}


# ------------------------------------------------------------------------------
# PASO 9: SCRAPEAR UN SUPERMERCADO PARA UNA MARCA
# ------------------------------------------------------------------------------
scrapear_super_live <- function(supermercado, marca, pausa = 8) {
  url <- construir_url_super(supermercado, marca)
  message("-> ", toupper(supermercado), " | ", toupper(marca))

  sesion <- abrir_pagina_live(url)

  if (is.null(sesion)) {
    message("   (!) no fue posible abrir el sitio")
    return(tabla_productos_vacia())
  }

  # Cerramos Chrome aunque la funcion termine con error
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)

  # Esperamos a que el JavaScript pinte la galeria de productos
  Sys.sleep(pausa)

  # El scroll dispara la carga del resto de resultados
  for (i in 1:3) {
    try(sesion$scroll_by(top = 1200), silent = TRUE)
    Sys.sleep(2)
  }

  sel <- selectores[[supermercado]]

  # Seleccionamos los nodos que corresponden a productos reales
  nodos <- sesion %>%
    html_elements(sel$contenedor)

  if (length(nodos) == 0) {
    message("   (!) no se detectaron productos")
    return(tabla_productos_vacia())
  }

  tabla <- map_dfr(nodos, ~ extraer_producto_super(.x, sel)) %>%
    clean_names() %>%
    mutate(
      marca        = marca,
      supermercado = supermercado
    )

  # Los contenedores sin titulo o sin precio son banners o carruseles
  tabla <- tabla %>%
    filter(!is.na(presentacion), presentacion != "", !is.na(precio))

  message("   ", nrow(tabla), " productos extraidos")
  tabla
}


# ------------------------------------------------------------------------------
# PASO 10: VERSION SEGURA
# ------------------------------------------------------------------------------
# Si una combinacion falla, devuelve una tabla vacia y el proceso continua.
scrapear_super_seguro <- purrr::possibly(
  scrapear_super_live,
  otherwise = tabla_productos_vacia()
)


# ------------------------------------------------------------------------------
# PASO 11: RECORRER LOS TRES SUPERMERCADOS Y LAS TRES CERVEZAS
# ------------------------------------------------------------------------------
productos_crudos <- map_dfr(supermercados, function(super) {
  map_dfr(marcas, function(marca) {
    tabla <- scrapear_super_seguro(super, marca, pausa = 8)
    Sys.sleep(2)  # pausa etica entre solicitudes
    tabla
  })
})

message("\nTotal de filas crudas: ", nrow(productos_crudos))

mostrar_tabla(productos_crudos, n = 10, caption = "Primeros resultados extraidos")


# ------------------------------------------------------------------------------
# PASO 12: LIMPIEZA
# ------------------------------------------------------------------------------
productos_limpios <- productos_crudos %>%
  mutate(
    presentacion = str_squish(presentacion),
    # El precio llega como "$ 22.800": el punto es separador de miles
    precio = str_remove_all(precio, "[^0-9]"),
    precio = suppressWarnings(as.numeric(precio)),
    titulo_min = str_to_lower(presentacion)
  ) %>%
  filter(!is.na(precio), precio > 0)

mostrar_tabla(productos_limpios, n = 10, caption = "Resultados con limpieza basica")


# ------------------------------------------------------------------------------
# PASO 13: DIAGNOSTICO DE VALORES FALTANTES
# ------------------------------------------------------------------------------
resumen_na <- tibble(
  variable = names(productos_limpios),
  n_na = sapply(productos_limpios, function(x) sum(is.na(x)))
)

mostrar_tabla(resumen_na, n = nrow(resumen_na), caption = "Valores faltantes por variable")


# ------------------------------------------------------------------------------
# PASO 14: FILTRAR EL RUIDO DEL BUSCADOR  (PREGUNTA 5)
# ------------------------------------------------------------------------------
# Los buscadores devuelven coincidencias difusas: al buscar "michelob" aparecen
# llantas Michelin y al buscar "club colombia" aparece Clobetasol y pinatas.
# Nos quedamos solo con lo que dice "cerveza" y descartamos esas categorias.
patron_ruido <- "llanta|neumatico|michelin|pirelli|clobetasol|pinata|gratis"

productos <- productos_limpios %>%
  filter(str_detect(titulo_min, "cerveza")) %>%
  filter(!str_detect(titulo_min, patron_ruido))

message("Filas despues del filtro: ", nrow(productos))

descartadas <- productos_limpios %>%
  filter(!str_detect(titulo_min, "cerveza") | str_detect(titulo_min, patron_ruido))

mostrar_tabla(descartadas, n = 10, caption = "Filas descartadas por el filtro de ruido")


# ------------------------------------------------------------------------------
# PASO 15: GUARDAR LOS RESULTADOS
# ------------------------------------------------------------------------------
dir.create("resultados", showWarnings = FALSE)

tabla_final <- productos %>%
  select(supermercado, marca, presentacion, precio)

ruta_salida <- file.path("resultados", "cervezas_supermercados.csv")
write_csv(tabla_final, ruta_salida)

message("Archivo guardado en: ", ruta_salida)

mostrar_tabla(tabla_final, n = 15, caption = "Tabla consolidada")


# ==============================================================================
# ANALISIS: RESPUESTAS A LAS PREGUNTAS DEL ENUNCIADO
# ==============================================================================

# ------------------------------------------------------------------------------
# PREGUNTA 1: precio total mas bajo por cerveza y supermercado
# ------------------------------------------------------------------------------
precio_minimo <- productos %>%
  group_by(marca, supermercado) %>%
  summarise(
    n_productos  = n(),
    precio_minimo = min(precio),
    .groups = "drop"
  )

mostrar_tabla(precio_minimo, n = nrow(precio_minimo),
              caption = "Precio total mas bajo por marca y supermercado")


# ------------------------------------------------------------------------------
# PREGUNTA 2: precio por mililitro
# ------------------------------------------------------------------------------
# Ojo con la semantica del volumen, que cambia entre sitios:
#   Exito / Carulla -> "(1980 ml)" es el volumen TOTAL del empaque
#   Jumbo           -> "x6und x330ml" es el volumen POR UNIDAD
productos_ml <- productos %>%
  mutate(
    # Texto del volumen, por ejemplo "330 ml", "1980 ml" o "7.920 ml"
    texto_ml = str_extract(titulo_min, "[0-9][0-9\\.]*\\s*ml"),
    ml = str_remove_all(texto_ml, "\\."),
    ml = str_extract(ml, "[0-9]+"),
    ml = suppressWarnings(as.numeric(ml)),

    # Las unidades se escriben de tres formas distintas segun el sitio:
    #   "x6und" o "x 24 unds"  |  "6 Unidades"  |  "330 ml x6" (al final)
    texto_und1 = str_extract(titulo_min, "x\\s*[0-9]+\\s*und"),
    texto_und2 = str_extract(titulo_min, "[0-9]+\\s*unidades"),
    texto_und3 = str_extract(titulo_min, "x\\s*[0-9]+$"),

    # Nos quedamos con el primer patron que si aparezca
    texto_und = ifelse(!is.na(texto_und1), texto_und1,
                ifelse(!is.na(texto_und2), texto_und2, texto_und3)),

    unidades = suppressWarnings(as.numeric(str_extract(texto_und, "[0-9]+"))),
    # "six pack" y "sixpack" siempre son 6
    unidades = ifelse(str_detect(titulo_min, "six\\s*pack"), 6, unidades),
    # Si no se declara cantidad, asumimos una sola unidad
    unidades = ifelse(is.na(unidades), 1, unidades),

    # En Jumbo el volumen del titulo es por unidad; en los otros dos ya es total
    ml_total = ifelse(supermercado == "jumbo", ml * unidades, ml),
    precio_por_ml = precio / ml_total
  )

precio_por_ml_super <- productos_ml %>%
  filter(!is.na(precio_por_ml)) %>%
  group_by(supermercado) %>%
  summarise(
    n_productos            = n(),
    precio_por_ml_promedio = mean(precio_por_ml),
    .groups = "drop"
  )

mostrar_tabla(precio_por_ml_super, n = nrow(precio_por_ml_super),
              caption = "Precio promedio por mililitro en cada supermercado")


# ------------------------------------------------------------------------------
# PREGUNTA 3: el mas barato cambia segun la cerveza?
# ------------------------------------------------------------------------------
precio_por_ml_marca <- productos_ml %>%
  filter(!is.na(precio_por_ml)) %>%
  group_by(marca, supermercado) %>%
  summarise(
    precio_por_ml_promedio = mean(precio_por_ml),
    .groups = "drop"
  )

mostrar_tabla(precio_por_ml_marca, n = nrow(precio_por_ml_marca),
              caption = "Precio por mililitro por marca y supermercado")


# ------------------------------------------------------------------------------
# PREGUNTA 4: presentaciones equivalentes (lata individual mas pequena)
# ------------------------------------------------------------------------------
lata_individual <- productos_ml %>%
  filter(unidades == 1, !is.na(ml_total)) %>%
  group_by(marca, supermercado) %>%
  summarise(
    ml_minimo     = min(ml_total),
    precio_minimo = min(precio),
    .groups = "drop"
  )

mostrar_tabla(lata_individual, n = nrow(lata_individual),
              caption = "Presentacion individual mas pequena por marca y supermercado")


# ------------------------------------------------------------------------------
# RESUMEN GENERAL
# ------------------------------------------------------------------------------
resumen_general <- productos %>%
  summarise(
    n_registros        = n(),
    n_presentaciones   = n_distinct(presentacion),
    n_supermercados    = n_distinct(supermercado),
    precio_promedio    = mean(precio)
  )

mostrar_tabla(resumen_general, n = 1, caption = "Resumen general")

message("\nProceso completado.")
