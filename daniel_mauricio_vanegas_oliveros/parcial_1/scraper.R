# v1.0 - Versión final.

# Novedades:

# v0.2: Se cambio el codigo del scrapper para mayor estabilidad.
# v0.3: Se procedió a guardar precios originales y de descuento (incluyendo indicadores de descuento)
# v0.4: Se adicionan productos para realizar el scraping.

# v0.5: Se completa el craping para Jumbo, Exito y Carulla.
# No se realiza limpieza de datos (correspondiente al script sql.)
# Solamente Jumbo muestra la marca del producto directamente (exito y carulla revisados posteriormente). 
# 

# v1.0: Se incorporaron gramaje y unidades por producto. 
# Listo para análisis con SQLite.

# v1.1: Se agregó análisis offline del archivo.

# ---- 1. Paquetes ----------------------------------------------------------

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr",
  "chromote", "jsonlite", "DBI", "RSQLite"
)

pendientes <- setdiff(paquetes, rownames(installed.packages()))

if (length(pendientes) > 0) {
  tryCatch(
    install.packages(pendientes),
    error = function(e) stop(
      "No fue posible instalar: ", paste(pendientes, collapse = ", "),
      ". Solicite apoyo al docente. Detalle: ", conditionMessage(e)
    )
  )
}

if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior. Ejecute install.packages('rvest') y reinicie RStudio.")
}

invisible(lapply(paquetes, library, character.only = TRUE))

# ---- 2. Diagnostico del entorno -------------------------------------------

diagnostico <- tibble(
  elemento = c("Sistema operativo", "Version de R", "Version de rvest", "Version de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)
print(diagnostico)

chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)

if (!chrome_disponible) {
  stop("Chrome no esta disponible. Instale Google Chrome y reinicie R.")
}

# ---- 3. Configuracion por sitio -------------------------------------------

# precio_final: selectores en orden de prioridad. Gana el primero que exista.
# precio_previo: precio antes del descuento. Es NA si no hay descuento.
# previo_solo_con_primero: si es TRUE, el precio previo solo cuenta
#   cuando existe el primer selector de precio_final (caso Exito).
sitios <- list(
  Carulla = list(
    url      = function(q) paste0("https://www.carulla.com/s?q=", q, "&sort=score_desc&page=0"),
    tarjeta  = "[class*='productCard_contentInfo']",
    titulo   = "[class*='styles_name']",
    precio_final = c("[class*='ProductPrice_container__price']"),
    precio_previo = "[class*='priceSection_container-promotion_price-dashed']",
    marca_producto = "NA",
    previo_solo_con_primero = FALSE,
    scroll   = FALSE
  ),
  Exito = list(
    url      = function(q) paste0("https://www.exito.com/s?q=", q, "&sort=score_desc&page=0"),
    tarjeta  = "[class*='productCard_contentInfo']",
    titulo   = "[class*='styles_name']",
    # Con descuento: price_fs-price es el final y ProductPrice es el original.
    # Sin descuento: ProductPrice es el final.
    precio_final = c("[class*='price_fs-price']",
                     "[class*='ProductPrice_container__price']"),
    precio_previo = "[class*='ProductPrice_container__price']",
    marca_producto = "NA",
    previo_solo_con_primero = TRUE,
    scroll   = FALSE
  ),
  Jumbo = list(
    url      = function(q) paste0("https://www.jumbocolombia.com/search?query=", q, "&type=term"),
    # Estas clases llevan hash. Actualicelas si el sitio cambia.
    tarjeta  = ".jumbo-ds-ProductCardstyles-1szk4ok",
    titulo   = ".jumbo-ds-ProductCardstyles-1szk4ol",
    precio_final = c(".jumbo-ds-ProductCardstyles-1szk4on"),
    precio_previo = ".jumbo-ds-ProductCardstyles-1szk4oo",
    marca_producto = ".jumbo-ds-ProductCardstyles-1szk4om",
    previo_solo_con_primero = FALSE,
    scroll   = TRUE
  )
)

# Extrae el texto de un selector dentro de cada tarjeta. Devuelve NA si falta.
texto_selector <- function(nodo, selector) {
  nodo %>%
    rvest::html_element(selector) %>%
    rvest::html_text2()
}

# Convierte texto de precio a numero. Formato colombiano: 12.900,50
precio_a_numero <- function(x) {
  readr::parse_number(
    x,
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
}

# ---- 4. Funciones ---------------------------------------------------------

# Copia el HTML renderizado a un documento estatico.
# Evita el error "No node with given id found": la pagina re-renderiza
# y los nodos vivos de Chrome quedan obsoletos.
# Repite la copia hasta encontrar nodos o vencer el tiempo.
obtener_doc_estable <- function(pagina_viva, selector, timeout = 20) {
  inicio <- Sys.time()
  doc <- NULL
  repeat {
    html_texto <- tryCatch(
      pagina_viva$session$Runtime$evaluate(
        "document.documentElement.outerHTML"
      )$result$value,
      error = function(e) NULL
    )
    
    if (is.character(html_texto) && nchar(html_texto) > 0) {
      doc <- xml2::read_html(html_texto)
      if (length(rvest::html_elements(doc, selector)) > 0) return(doc)
    }
    
    if (as.numeric(difftime(Sys.time(), inicio, units = "secs")) > timeout) return(doc)
    Sys.sleep(1)
  }
}

extraer_producto <- function(nombre_sitio, consulta, timeout = 20) {
  cfg <- sitios[[nombre_sitio]]
  
  termino <- utils::URLencode(stringr::str_replace_all(consulta, "_", " "), reserved = TRUE)
  pagina_consulta <- cfg$url(termino)
  
  pagina_viva <- rvest::read_html_live(pagina_consulta)
  on.exit(try(pagina_viva$session$close(), silent = TRUE), add = TRUE)
  
  if (cfg$scroll) {
    Sys.sleep(3)
    pagina_viva$scroll_by(top = 2000)
    Sys.sleep(2)
  }
  
  doc <- obtener_doc_estable(pagina_viva, cfg$tarjeta, timeout)
  
  # Guarda el HTML de la pagina consultada en la carpeta raw.
  if (!is.null(doc)) {
    dir.create("raw", showWarnings = FALSE, recursive = TRUE)
    archivo_html <- file.path("raw", paste0(nombre_sitio, "_", consulta, ".html"))
    xml2::write_html(doc, archivo_html)
  }
  
  nodo <- if (is.null(doc)) list() else rvest::html_elements(doc, cfg$tarjeta)
  
  if (length(nodo) == 0) {
    warning("Sin nodos en ", nombre_sitio, " para la consulta '", consulta, "'.", call. = FALSE)
    return(tibble(
      titulo = character(),
      precio_texto = character(), precio = numeric(),
      precio_previo_texto = character(), precio_previo = numeric(),
      tiene_descuento = logical(), descuento_pct = numeric(), marca_producto = character(),
      consulta = character(), pagina = character(), hora_consulta = character()
    ))
  }
  
  titulo <- texto_selector(nodo, cfg$titulo)
  
  # Precio final: el primer selector con valor.
  finales <- lapply(cfg$precio_final, function(sel) texto_selector(nodo, sel))
  precio_texto <- dplyr::coalesce(!!!finales)
  
  # Precio previo al descuento.
  precio_previo_texto <- texto_selector(nodo, cfg$precio_previo)
  if (cfg$previo_solo_con_primero) {
    precio_previo_texto[is.na(finales[[1]])] <- NA_character_
  }
  
  precio <- precio_a_numero(precio_texto)
  precio_previo <- precio_a_numero(precio_previo_texto)
  
  # Marca del producto
  marca_producto = texto_selector(nodo, cfg$marca_producto)
  
  tibble(
    titulo              = titulo,
    precio_texto        = precio_texto,
    precio              = precio,
    precio_previo_texto = precio_previo_texto,
    precio_previo       = precio_previo,
    marca_producto      = marca_producto,
    tiene_descuento     = !is.na(precio_previo),
    descuento_pct       = round((1 - precio / precio_previo) * 100, 1),
    consulta            = consulta,
    pagina              = nombre_sitio,
    hora_consulta       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    pagina_consulta     = pagina_consulta
  )
}

extraer_producto_offline <- function(archivo_html) {
  
  # Deduce sitio y consulta desde <Sitio>_<consulta>.html
  nombre <- sub("\\.html$", "", basename(archivo_html))
  nombre_sitio <- sub("_.*$", "", nombre)
  consulta <- sub("^[^_]+_", "", nombre)
  
  if (!nombre_sitio %in% names(sitios)) {
    stop("Sitio desconocido en el archivo: ", archivo_html)
  }
  if (!file.exists(archivo_html)) {
    stop("No existe el archivo: ", archivo_html)
  }
  
  cfg <- sitios[[nombre_sitio]]
  termino <- utils::URLencode(stringr::str_replace_all(consulta, "_", " "), reserved = TRUE)
  pagina_consulta <- cfg$url(termino)
  
  doc <- xml2::read_html(archivo_html, encoding = "UTF-8")
  hora <- file.info(archivo_html)$mtime
  nodo <- rvest::html_elements(doc, cfg$tarjeta)
  
  if (length(nodo) == 0) {
    warning("Sin nodos en ", nombre_sitio, " para la consulta '", consulta, "'.", call. = FALSE)
    return(tibble(
      titulo = character(),
      precio_texto = character(), precio = numeric(),
      precio_previo_texto = character(), precio_previo = numeric(),
      marca_producto = character(),
      tiene_descuento = logical(), descuento_pct = numeric(),
      consulta = character(), pagina = character(), hora_consulta = character(),
      pagina_consulta = character()
    ))
  }
  
  titulo <- texto_selector(nodo, cfg$titulo)
  
  # Precio final: el primer selector con valor.
  finales <- lapply(cfg$precio_final, function(sel) texto_selector(nodo, sel))
  precio_texto <- dplyr::coalesce(!!!finales)
  
  # Precio previo al descuento.
  precio_previo_texto <- texto_selector(nodo, cfg$precio_previo)
  if (cfg$previo_solo_con_primero) {
    precio_previo_texto[is.na(finales[[1]])] <- NA_character_
  }
  
  precio <- precio_a_numero(precio_texto)
  precio_previo <- precio_a_numero(precio_previo_texto)
  marca_producto <- texto_selector(nodo, cfg$marca_producto)
  
  tibble(
    titulo              = titulo,
    precio_texto        = precio_texto,
    precio              = precio,
    precio_previo_texto = precio_previo_texto,
    precio_previo       = precio_previo,
    marca_producto      = marca_producto,
    tiene_descuento     = !is.na(precio_previo),
    descuento_pct       = round((1 - precio / precio_previo) * 100, 1),
    consulta            = consulta,
    pagina              = nombre_sitio,
    hora_consulta       = format(hora, "%Y-%m-%dT%H:%M:%S%z"),
    pagina_consulta     = pagina_consulta
  )
}

crear_base <- function(consultas = NULL, pausa = 30, online = TRUE) {
  
  resultados <- list()
  k <- 0
  
  if (online) {
    verificar_chrome()
    
    for (q in consultas) {
      for (s in names(sitios)) {
        message("Consultando ", s, ": ", q)
        k <- k + 1
        
        fila <- NULL
        for (intento in 1:3) {
          fila <- tryCatch(
            extraer_producto(s, q, online = TRUE),
            error = function(e) {
              warning("Intento ", intento, " fallo en ", s, " / ", q, ": ",
                      conditionMessage(e), call. = FALSE)
              NULL
            }
          )
          if (!is.null(fila)) break
          Sys.sleep(5)
        }
        
        resultados[[k]] <- fila
        names(resultados)[k] <- paste(s, q, sep = "_")
        
        Sys.sleep(pausa)
      }
    }
    
  } else {
    if (is.null(consultas)) {
      return(cat("Error, no hay consultas"))
    }
    
    for (q in consultas) {
      for (s in names(sitios)) {
        message("Leyendo ", s, ": ", q)
        k <- k + 1
        
        fila <- tryCatch(
          extraer_producto_offline(file.path("raw", paste0(s, "_", q, ".html"))),
          error = function(e) {
            warning("Fallo en ", s, " / ", q, ": ", conditionMessage(e), call. = FALSE)
            NULL
          }
        )
        
        resultados[k] <- list(fila)
        names(resultados)[k] <- paste(s, q, sep = "_")
      }
    }
  }
  
  dplyr::bind_rows(resultados)
}
# ---- 5. Ejecucion ---------------------------------------------------------

# palabras a consultar
consultas <- c(
  "frutos_secos",
  "granola",
  "chocorramo",
  "barras_cereal",
  "gansito",
  "conserva",
  "cereales",
  "galletas"
)

# guardado de consultas
base_datos <- crear_base(consultas = consultas, pausa = 0, online = F)


# revision de resultados
print(
  dplyr::count(
    base_datos,
    pagina,
    consulta
  )
)

# Agregado de marcas para Exito y Carulla

# Extracción de marcas en mayúsculas.
extraer_marca <- function(x) {
  patron <- "(?<!\\p{L})\\p{Lu}{2,}(?:\\s+\\p{Lu}{2,})*(?!\\p{L})"
  sapply(x, function(t) {
    m <- regmatches(t, regexpr(patron, t, perl = TRUE))
    if (length(m) == 0) NA_character_ else m
  }, USE.NAMES = FALSE)
}

tiendas <- base_datos$pagina %in% c("Carulla", "Exito")
base_datos$marca_producto[tiendas] <- extraer_marca(base_datos$titulo[tiendas])

# Revisa el resultado
head(base_datos[tiendas, c("titulo", "marca_producto")], 100)

# Extraer gramaje de cada tienda.

extraer_gramos <- function(x) {
  patron <- "(?i)(?<![\\d.,])(\\d+(?:[.,]\\d+)?)\\s*(kg|grs?|gramos?|g)\\b"
  m <- regmatches(x, regexec(patron, x, perl = TRUE))
  sapply(m, function(v) {
    if (length(v) == 0) return(NA_real_)
    num <- as.numeric(sub(",", ".", v[2]))
    if (tolower(v[3]) == "kg") num * 1000 else num
  })
}

# Unidades por presentacion
extraer_unidades <- function(x) {
  patron <- "(?i)(?<![\\d.,])(\\d+)\\s*(?:unds?|unid(?:ad(?:es)?)?|uds?)\\b"
  m <- regmatches(x, regexec(patron, x, perl = TRUE))
  sapply(m, function(v) if (length(v) == 0) 1 else as.numeric(v[2]))
}

base_datos$gramos   <- extraer_gramos(base_datos$titulo)
base_datos$unidades <- extraer_unidades(base_datos$titulo)

# ---- 6. Exportar a SQLite -------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), "consultas_iniciales.db")

dbWriteTable(con, "productos", base_datos, overwrite = TRUE)

# Verifica la tabla
dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) AS filas FROM productos;")

dbDisconnect(con)

# Fin del script.