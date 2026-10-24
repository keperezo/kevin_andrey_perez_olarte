### 1. LIBRERÍAS

paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr", "chromote",
  "lubridate", "DBI", "RSQLite"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  tryCatch(
    install.packages(pendientes, repos = "https://cloud.r-project.org"),
    error = function(e) stop(
      "No fue posible instalar: ", paste(pendientes, collapse = ", "),
      ". Detalle: ", conditionMessage(e)
    )
  )
}

invisible(lapply(paquetes, library, character.only = TRUE))

dir.create("raw", showWarnings = FALSE)

##### 2. FUNCIONES BASE Y DE LIMPIEZA DE PRECIOS

tabla_productos_vacia <- function() {
  tibble(
    comercio = character(),
    n_producto = character(),
    enlace = character(),
    precio = character(),
    marca = character(),
    contenido = character(),
    precio_sin_promo = character(),
    pagina = integer(),
    consulta = character(),
    fecha_extraccion = as.POSIXct(character())
  )
}

# URL de la búsqueda para una página: "&page=" si el término ya trae parámetros, "?page=" si no
construir_url_page <- function(link, termino, pagina) {
  separador <- if (str_detect(termino, fixed("?"))) "&" else "?"
  paste0(link, URLencode(termino), separador, "page=", pagina)
}

# Convierte el precio publicado ("$ 18.280") a número; un precio de $0 queda como NA
limpiar_precio <- function(col_precio) {
  valor <- col_precio %>%
    str_remove_all("[^0-9,.]") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", ".") %>%
    as.numeric()
  if_else(valor > 0, valor, NA_real_)
}

# Fecha y hora en ISO 8601 con hora de Colombia, p. ej. 2026-10-11T22:28:13-05:00
fecha_iso_colombia <- function(x) {
  if_else(
    is.na(x),
    NA_character_,
    paste0(format(with_tz(x, "America/Bogota"), "%Y-%m-%dT%H:%M:%S"), "-05:00")
  )
}

##### 3. CARGA Y LECTURA DE LAS PÁGINAS

# Abre la página con Chromote, baja en pasos para que carguen los productos y devuelve el HTML
cargar_pagina <- function(url, pausa = 4, pasos_scroll = 8, px_paso = 700, scroll_final = FALSE) {

  # 1. Crear sesión de Chromote sin esperar el evento load
  pagina_viva <- tryCatch({
    b <- chromote::ChromoteSession$new()
    b$Page$navigate(url, wait_ = FALSE)
    b
  }, error = function(e) {
    message("Error al abrir ", url, ": ", conditionMessage(e))
    NULL
  })

  if (is.null(pagina_viva)) return(NULL)
  on.exit(try(pagina_viva$close(), silent = TRUE), add = TRUE)

  Sys.sleep(pausa)

  # 2. Scroll usando JavaScript
  for (i in seq_len(pasos_scroll)) {
    try(pagina_viva$Runtime$evaluate(paste0("window.scrollBy(0, ", px_paso, ");")), silent = TRUE)
    Sys.sleep(1.5)
  }
  if (scroll_final) {
    try(pagina_viva$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);"), silent = TRUE)
  }
  Sys.sleep(3)

  # 3. Extraer el HTML
  doc <- pagina_viva$DOM$getDocument()
  pagina_viva$DOM$getOuterHTML(doc$root$nodeId)$outerHTML
}

# Lee las tarjetas de producto de una página con la función de extracción del comercio
parsear_pagina <- function(html_str, extraer, link, pagina, termino, fecha) {
  nodos <- read_html(html_str) %>%
    html_elements("[data-af-element='search-result']")

  if (length(nodos) == 0) return(tabla_productos_vacia())

  map_dfr(nodos, ~ extraer(link = link, nodo = .x)) %>%
    clean_names() %>%
    mutate(pagina = as.integer(pagina), consulta = termino, fecha_extraccion = fecha)
}

# Total de productos que muestra el buscador ("400 productos"); NA si el sitio no lo muestra
leer_total_sitio <- function(html_str) {
  texto <- read_html(html_str) %>%
    html_element("[class*='totalProducts']") %>%
    html_text2()
  if (is.na(texto)) NA_integer_ else as.integer(str_remove_all(texto, "[^0-9]"))
}

##### 4. FUNCIONES DE BASE DE DATOS SQLITE

inicializar_bd_sqlite <- function(db_path = "comparador_precios.sqlite") {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))

  # Tabla 1: Ejecuciones (una fila por corrida y comercio)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ejecuciones (
      id_ejecucion TEXT NOT NULL,
      comercio TEXT NOT NULL,
      fecha_inicio TEXT NOT NULL,
      fecha_fin TEXT,
      modo TEXT NOT NULL,
      paginas_consultadas INTEGER,
      paginas_sin_datos INTEGER,
      n_registros INTEGER NOT NULL,
      productos_unicos INTEGER,
      total_sitio INTEGER,
      estado TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
      PRIMARY KEY (id_ejecucion, comercio)
    )
  ")

  # Tabla 2: Histórico de productos (una fila por observación de precio)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS historico_productos (
      id_observacion INTEGER PRIMARY KEY AUTOINCREMENT,
      id_ejecucion TEXT NOT NULL,
      comercio TEXT NOT NULL,
      n_producto TEXT,
      enlace TEXT NOT NULL,
      precio TEXT,
      marca TEXT,
      contenido TEXT,
      precio_sin_promo TEXT,
      pagina INTEGER,
      consulta TEXT,
      fecha_extraccion TEXT NOT NULL,
      precio_num REAL,
      precio_sin_promo_num REAL,
      promocion INTEGER,
      UNIQUE (comercio, enlace, fecha_extraccion),
      FOREIGN KEY (id_ejecucion, comercio) REFERENCES ejecuciones (id_ejecucion, comercio)
    )
  ")

  invisible(db_path)
}

# Inserta las observaciones de un comercio sin repetir (comercio, enlace, fecha_extraccion)
insertar_en_sqlite <- function(con, df, id_ejecucion) {
  if (nrow(df) == 0) return(0L)

  df_preparado <- df %>%
    filter(!is.na(enlace), !is.na(fecha_extraccion)) %>%
    mutate(
      id_ejecucion = !!id_ejecucion,
      pagina = as.integer(pagina),
      precio_num = limpiar_precio(precio),
      precio_sin_promo_num = limpiar_precio(precio_sin_promo),
      promocion = as.integer(!is.na(precio_sin_promo_num) & precio_sin_promo_num > precio_num),
      fecha_extraccion = fecha_iso_colombia(fecha_extraccion)
    ) %>%
    select(id_ejecucion, comercio, n_producto, enlace, precio, marca, contenido,
           precio_sin_promo, pagina, consulta, fecha_extraccion, precio_num, precio_sin_promo_num,
           promocion)

  columnas <- paste(names(df_preparado), collapse = ", ")
  dbWriteTable(con, "tmp_carga", df_preparado, temporary = TRUE, overwrite = TRUE)
  on.exit(dbExecute(con, "DROP TABLE IF EXISTS tmp_carga"), add = TRUE)

  dbExecute(con, paste0(
    "INSERT OR IGNORE INTO historico_productos (", columnas, ") ",
    "SELECT ", columnas, " FROM tmp_carga"
  ))
}

# Registra la ejecución de un comercio y carga sus observaciones.
# Estado:
#  - fallida: no hubo registros
#  - parcial: alguna página quedó sin datos, se obtuvieron menos productos que el total del sitio
#    o, si el sitio no muestra total, la última página está llena (el listado puede continuar)
#  - completa: en otro caso
cargar_comercio <- function(db_path, df, comercio, id_ejecucion, inicio, fin, paginas, modo,
                            total_sitio = NA_integer_) {
  paginas_sin_datos <- length(setdiff(paginas, df$pagina))
  productos_unicos <- n_distinct(df$enlace[str_detect(df$enlace, "/p(\\?|$)") %in% TRUE])
  por_pagina <- count(df, pagina)$n
  fin_listado <- length(por_pagina) > 0 && por_pagina[length(por_pagina)] < max(por_pagina)

  estado <- if (nrow(df) == 0) {
    "fallida"
  } else if (paginas_sin_datos > 0) {
    "parcial"
  } else if (!is.na(total_sitio)) {
    if (productos_unicos >= total_sitio) "completa" else "parcial"
  } else if (fin_listado) {
    "completa"
  } else {
    "parcial"
  }

  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))

  dbExecute(con, "
    INSERT OR IGNORE INTO ejecuciones
      (id_ejecucion, comercio, fecha_inicio, fecha_fin, modo, paginas_consultadas,
       paginas_sin_datos, n_registros, productos_unicos, total_sitio, estado)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    id_ejecucion, comercio, fecha_iso_colombia(inicio), fecha_iso_colombia(fin), modo,
    length(paginas), paginas_sin_datos, nrow(df), productos_unicos, as.integer(total_sitio), estado
  ))

  n_nuevos <- insertar_en_sqlite(con, df, id_ejecucion)
  message(comercio, " | ejecución ", id_ejecucion, " (", estado, "): ",
          nrow(df), " registros obtenidos, ", n_nuevos, " nuevos en SQLite")
  n_nuevos
}

##### FUNCIONES POR COMERCIO

##### CAFAM

# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_cafam <- function(link, nodo) {

  # Función interna para asegurar que devuelva NA si el nodo no existe
  obtener_texto <- function(selector) {
    res <- nodo %>% html_element(selector) %>% html_text2()
    if (length(res) == 0 || is.na(res)) NA_character_ else res
  }
  comercio = "CAFAM"

  # 1. Título del producto
  N_producto <- obtener_texto("h3 span")

  # 2. Enlace hacia el producto
  enlace_r <- nodo %>%
    html_element("a[class*='clearLink'], a") %>%
    html_attr("href")

  # 3. Precio actual / de venta
  precio <- obtener_texto(".drogueriascafam-discounts-management-0-x-convenioNormalPrice")

  # 4. Marca del producto
  marca <- obtener_texto(".vtex-product-summary-2-x-productBrandName")

  # 5. Contenido / Presentación
  contenido <- obtener_texto("[class*='PUM-content-wrapper'], [class*='specification'], [class*='productDescription']")

  # 6. Precio sin oferta / de lista
  precio_sin_promo <- obtener_texto(".drogueriascafam-discounts-management-0-x-price, [class*='listPriceValue']")

  # Construimos la fila para este producto
  tibble(
    comercio = comercio,
    N_producto = N_producto,
    enlace = if_else(
      is.na(enlace_r),
      NA_character_,
      xml2::url_absolute(enlace_r, link)
    ),
    precio = precio,
    marca = marca,
    contenido = contenido,
    precio_sin_promo = precio_sin_promo
  )
}

scrapear_pagina_cafam <- function(link, termino, pagina = 1, pausa = 4, carpeta_raw = NULL) {
  url <- construir_url_page(link, termino, pagina)
  message("Procesando página ", pagina, ": ", url)

  html_str <- cargar_pagina(url, pausa = pausa, pasos_scroll = 8, px_paso = 700)
  if (is.null(html_str)) return(tabla_productos_vacia())

  guardar_html_crudo(html_str, carpeta_raw, pagina)
  productos <- parsear_pagina(html_str, extraer_producto_cafam, link, pagina, termino, fecha = now())
  message("Página ", pagina, ": ", nrow(productos), " productos")
  productos
}

##### REBAJA

# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_rebaja <- function(link, nodo) {

  # Función interna para asegurar que devuelva NA si el nodo no existe
  obtener_texto <- function(selector) {
    res <- nodo %>% html_element(selector) %>% html_text2()
    if (length(res) == 0 || is.na(res)) NA_character_ else res
  }

  comercio <- "LA REBAJA"
  # Extraemos el título visible del producto
  N_producto <- obtener_texto("h3 span")

  # Extraemos el enlace relativo hacia la página del producto
  enlace_r <- nodo %>%
    html_element("a.vtex-product-summary-2-x-clearLink") %>%
    html_attr("href")

  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- obtener_texto(".vtex-product-price-1-x-sellingPriceValue")

  # Extraemos la marca de la vitamina
  marca <- obtener_texto(".vtex-store-components-3-x-productBrandName")

  # Extraemos el texto del contenido del producto
  contenido <- obtener_texto("[class*='PUM-content-wrapper'] span")

  # Extraemos el precio sin promo o de lista
  precio_sin_promo <- obtener_texto(".vtex-product-price-1-x-listPriceValue")

  # Construimos una fila con los campos extraídos para este producto
  tibble(
    comercio = comercio,
    N_producto = N_producto,
    enlace = if_else(
      is.na(enlace_r),
      NA_character_,
      xml2::url_absolute(enlace_r, link)
    ),
    precio = precio,
    marca = marca,
    contenido = contenido,
    precio_sin_promo = precio_sin_promo
  )
}

scrapear_pagina_rebaja <- function(link, termino, pagina = 1, pausa = 4, carpeta_raw = NULL) {
  url <- construir_url_page(link, termino, pagina)
  message("Procesando página ", pagina, ": ", url)

  html_str <- cargar_pagina(url, pausa = pausa, pasos_scroll = 8, px_paso = 700)
  if (is.null(html_str)) return(tabla_productos_vacia())

  guardar_html_crudo(html_str, carpeta_raw, pagina)
  productos <- parsear_pagina(html_str, extraer_producto_rebaja, link, pagina, termino, fecha = now())
  message("Página ", pagina, ": ", nrow(productos), " productos")
  productos
}

##### ALEMANA

# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_alemana <- function(link, nodo) {

  # Función interna para asegurar que devuelva NA si el nodo no existe
  obtener_texto <- function(selector) {
    res <- nodo %>% html_element(selector) %>% html_text2()
    if (length(res) == 0 || is.na(res)) NA_character_ else res
  }

  comercio <- "LA ALEMANA"

  # 1. Título del producto
  N_producto <- obtener_texto("h3 span")

  # 2. Enlace hacia el producto
  enlace_r <- nodo %>%
    html_element("a[class*='clearLink'], a") %>%
    html_attr("href")

  # 3. Precio actual / de venta
  precio <- obtener_texto(".vtex-product-price-1-x-sellingPrice")

  # 4. Marca del producto
  marca <- obtener_texto(".vtex-product-summary-2-x-productBrandName")

  # 5. Contenido / Presentación (el listado de este sitio no lo publica)
  contenido <- obtener_texto("[class*='PUM-content-wrapper'], [class*='specification'], [class*='productDescription']")

  # 6. Precio sin oferta / de lista
  precio_sin_promo <- obtener_texto(".vtex-product-price-1-x-listPrice")

  # Construimos la fila para este producto
  tibble(
    comercio = comercio,
    N_producto = N_producto,
    enlace = if_else(
      is.na(enlace_r),
      NA_character_,
      xml2::url_absolute(enlace_r, link)
    ),
    precio = precio,
    marca = marca,
    contenido = contenido,
    precio_sin_promo = precio_sin_promo
  )
}

scrapear_pagina_alemana <- function(link, termino, pagina = 1, pausa = 4, carpeta_raw = NULL) {
  url <- construir_url_page(link, termino, pagina)
  message("Procesando página ", pagina, ": ", url)

  # Esta página muestra hasta 50 productos: 18 pasos de scroll y un scroll final al fondo
  html_str <- cargar_pagina(url, pausa = pausa, pasos_scroll = 18, px_paso = 800, scroll_final = TRUE)
  if (is.null(html_str)) return(tabla_productos_vacia())

  guardar_html_crudo(html_str, carpeta_raw, pagina)
  productos <- parsear_pagina(html_str, extraer_producto_alemana, link, pagina, termino, fecha = now())
  message("Página ", pagina, ": ", nrow(productos), " productos")
  productos
}

# Datos de cada comercio para la recolección en línea y para releer las copias crudas
FUENTES <- list(
  list(comercio = "CAFAM", url = "https://www.drogueriascafam.com.co/", termino = "vitaminas?_q",
       scrapear = scrapear_pagina_cafam, extraer = extraer_producto_cafam),
  list(comercio = "LA REBAJA", url = "https://www.larebajavirtual.com/", termino = "vitaminas?_q",
       scrapear = scrapear_pagina_rebaja, extraer = extraer_producto_rebaja),
  list(comercio = "LA ALEMANA", url = "https://www.tudrogueriavirtual.com/", termino = "vitaminas",
       scrapear = scrapear_pagina_alemana, extraer = extraer_producto_alemana)
)

##### 5. COPIAS CRUDAS (raw/) Y MODO SIN CONEXIÓN

carpeta_comercio <- function(comercio) str_to_lower(str_remove(comercio, "^LA "))

archivo_pagina <- function(pagina) sprintf("pagina_%02d.html.gz", pagina)

# Guarda el HTML de una página en raw/<comercio>/<id_ejecucion>/pagina_NN.html.gz
guardar_html_crudo <- function(html_str, carpeta, pagina) {
  if (is.null(carpeta)) return(invisible(NULL))
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  archivo <- gzfile(file.path(carpeta, archivo_pagina(pagina)), "wb")
  on.exit(close(archivo))
  writeLines(enc2utf8(html_str), archivo, useBytes = TRUE)
}

leer_html_crudo <- function(ruta) {
  archivo <- gzfile(ruta, "rb")
  on.exit(close(archivo))
  paste(readLines(archivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# Recorre las páginas del buscador de un comercio. Sin páginas fijas, avanza hasta que una
# página no trae productos nuevos (fin del listado). Las páginas usadas quedan en paginas.csv
recorrer_paginas <- function(fuente, carpeta, paginas = NULL, max_paginas = 60, pausa = 4) {
  automatico <- is.null(paginas)
  if (automatico) paginas <- seq_len(max_paginas)

  resultados <- list()
  enlaces_vistos <- character()
  indice <- tibble(pagina = integer(), archivo = character(), url = character(),
                   fecha_consulta = character())

  for (pagina in paginas) {
    df <- fuente$scrapear(fuente$url, fuente$termino, pagina, pausa, carpeta)
    if (nrow(df) == 0) {
      message("Página ", pagina, " sin productos, se intenta de nuevo")
      df <- fuente$scrapear(fuente$url, fuente$termino, pagina, pausa * 2, carpeta)
    }

    if (automatico && length(setdiff(df$enlace, enlaces_vistos)) == 0) {
      unlink(file.path(carpeta, archivo_pagina(pagina)))
      break
    }

    resultados[[length(resultados) + 1]] <- df
    enlaces_vistos <- union(enlaces_vistos, df$enlace)
    indice <- add_row(
      indice,
      pagina = as.integer(pagina),
      archivo = if (file.exists(file.path(carpeta, archivo_pagina(pagina)))) archivo_pagina(pagina) else NA_character_,
      url = construir_url_page(fuente$url, fuente$termino, pagina),
      fecha_consulta = if (nrow(df) > 0) fecha_iso_colombia(df$fecha_extraccion[1]) else NA_character_
    )
  }

  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  write_csv(indice, file.path(carpeta, "paginas.csv"), na = "")
  list(datos = bind_rows(tabla_productos_vacia(), resultados), paginas = indice$pagina)
}

# Primera recolección (11-oct-2026): raw/<comercio>_vitaminas_resultados.csv, con los productos
# ya extraídos. Su identificador se toma de la hora de la primera observación
leer_copias_csv <- function(dir_raw = "raw") {
  rutas <- list.files(dir_raw, pattern = "_vitaminas_resultados\\.csv$", full.names = TRUE)
  datos <- map_dfr(rutas, function(ruta) {
    read_csv(ruta, col_types = cols(.default = col_character()), progress = FALSE) %>%
      mutate(fecha_extraccion = ymd_hms(fecha_extraccion, quiet = TRUE), pagina = as.integer(pagina))
  })
  if (nrow(datos) > 0) {
    datos <- datos %>%
      mutate(id_ejecucion = format(
        with_tz(min(fecha_extraccion, na.rm = TRUE), "America/Bogota"), "%Y%m%d_%H%M%S"
      ))
  }
  datos
}

# Ejecuciones guardadas como HTML: vuelve a extraer los productos de cada página con la misma
# función del comercio y con la fecha y hora registradas en paginas.csv
leer_copias_html <- function(dir_raw = "raw") {
  indices <- list.files(dir_raw, pattern = "^paginas\\.csv$", recursive = TRUE, full.names = TRUE)

  map(indices, function(ruta_indice) {
    carpeta <- dirname(ruta_indice)
    fuente <- keep(FUENTES, ~ carpeta_comercio(.x$comercio) == basename(dirname(carpeta)))[[1]]
    indice <- read_csv(ruta_indice, col_types = cols(.default = col_character()), progress = FALSE) %>%
      mutate(pagina = as.integer(pagina), fecha = ymd_hms(fecha_consulta, quiet = TRUE))

    con_archivo <- filter(indice, !is.na(archivo))
    datos <- map_dfr(seq_len(nrow(con_archivo)), function(i) {
      html_str <- leer_html_crudo(file.path(carpeta, con_archivo$archivo[i]))
      parsear_pagina(html_str, fuente$extraer, fuente$url, con_archivo$pagina[i],
                     fuente$termino, con_archivo$fecha[i])
    })

    primera <- file.path(carpeta, archivo_pagina(1))
    list(
      id_ejecucion = basename(carpeta),
      comercio = fuente$comercio,
      datos = bind_rows(tabla_productos_vacia(), datos),
      paginas = indice$pagina,
      total_sitio = if (file.exists(primera)) leer_total_sitio(leer_html_crudo(primera)) else NA_integer_,
      inicio = min(indice$fecha, na.rm = TRUE),
      fin = max(indice$fecha, na.rm = TRUE)
    )
  })
}

##### 6. FUNCIÓN ORQUESTADORA GENERAL

# modo_offline = TRUE : reprocesa las copias de raw/ sin consultar los sitios
# modo_offline = FALSE: recolección en línea; crea la ejecución AAAAMMDD_HHMMSS (hora de Colombia)
# Las páginas de cada comercio se recorren hasta el fin del listado, salvo que se indiquen
ejecutar_recoleccion_completa <- function(db_path = "comparador_precios.sqlite",
                                          modo_offline = FALSE,
                                          paginas_cafam = NULL,
                                          paginas_rebaja = NULL,
                                          paginas_alemana = NULL) {

  message("=== INICIANDO RECOLECCIÓN COMPLETA: ", now(), " ===")

  inicializar_bd_sqlite(db_path)

  if (modo_offline) {
    message("=== EJECUTANDO EN MODO OFFLINE (REPROCESANDO COPIAS RAW) ===")

    previas <- leer_copias_csv()
    for (com in unique(previas$comercio)) {
      df <- filter(previas, comercio == com)
      cargar_comercio(
        db_path, select(df, -id_ejecucion), com, df$id_ejecucion[1],
        inicio = min(df$fecha_extraccion, na.rm = TRUE),
        fin = max(df$fecha_extraccion, na.rm = TRUE),
        paginas = seq_len(max(df$pagina, na.rm = TRUE)),
        modo = "copias_crudas"
      )
    }

    for (lote in leer_copias_html()) {
      cargar_comercio(db_path, lote$datos, lote$comercio, lote$id_ejecucion,
                      inicio = lote$inicio, fin = lote$fin, paginas = lote$paginas,
                      modo = "copias_crudas", total_sitio = lote$total_sitio)
    }

    df_general <- previas

  } else {
    message("=== EJECUTANDO EN MODO ONLINE (WEBSCRAPING VIVO) ===")

    id_ejecucion <- format(with_tz(now(), "America/Bogota"), "%Y%m%d_%H%M%S")
    paginas_fijas <- list(CAFAM = paginas_cafam, `LA REBAJA` = paginas_rebaja,
                          `LA ALEMANA` = paginas_alemana)

    df_general <- map_dfr(FUENTES, function(fuente) {
      message("\n--- Extrayendo ", fuente$comercio, " ---")
      inicio <- now()
      carpeta <- file.path("raw", carpeta_comercio(fuente$comercio), id_ejecucion)

      resultado <- recorrer_paginas(fuente, carpeta, paginas_fijas[[fuente$comercio]])
      primera <- file.path(carpeta, archivo_pagina(1))
      total_sitio <- if (file.exists(primera)) leer_total_sitio(leer_html_crudo(primera)) else NA_integer_

      cargar_comercio(db_path, resultado$datos, fuente$comercio, id_ejecucion,
                      inicio = inicio, fin = now(), paginas = resultado$paginas,
                      modo = "en_linea", total_sitio = total_sitio)
      mutate(resultado$datos, id_ejecucion = id_ejecucion)
    })
  }

  message("\n=== RECOLECCIÓN FINALIZADA ===")
  message("Base de datos en: ", db_path)

  invisible(df_general)
}
