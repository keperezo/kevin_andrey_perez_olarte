# ============================================================================
# Ajustes de auditoría v1.1:
# - fechas ISO 8601 homogéneas (-05:00)
# - subcategoría estandarizada con fallback a categoría fuente
# - promoción Cruz Verde basada en evidencia de precio anterior
# - La Rebaja offline conserva fecha de la ejecución fuente
# - fecha_fin de ejecuciones antiguas no queda vacía
# UNIVERSIDAD NACIONAL DE COLOMBIA
# Minería de Datos (2016325) - Taller 1
# scraper.R - módulo unificado de extracción y carga
# Categoría 32: Medicamentos dermatológicos
# ============================================================================
# Este archivo contiene una función por comercio, modo online/offline,
# conservación de copias raw, homologación documentada, carga incremental
# en SQLite y una función general para ejecutar el proceso completo.
# ============================================================================

# ============================================================
# UNIVERSIDAD NACIONAL DE COLOMBIA
# Minería de Datos - Taller 1
# Prototipo de scraper para Droguerías Cafam
# Categoría académica: Medicamentos dermatológicos
# Versión: 0.3.3
# ============================================================

# Este módulo implementa SOLO la extracción cruda de Cafam.
# La clasificación según la categoría académica 32 y la normalización
# entre comercios se harán en una etapa posterior, para no alterar ni
# perder la información original recolectada.

# Dependencias esperadas:
# rvest, xml2, httr2, dplyr, purrr, stringr, tibble, readr, jsonlite

suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(httr2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readr)
  library(jsonlite)
})

CAFAM_BASE <- "https://www.drogueriascafam.com.co"
CAFAM_CATEGORIA <- paste0(CAFAM_BASE, "/salud-y-medicamentos/dermatologicos")

# ------------------------------------------------------------
# Utilidades generales
# ------------------------------------------------------------

iso_colombia <- function(x = Sys.time()) {
  txt <- format(x, tz = "America/Bogota", format = "%Y-%m-%dT%H:%M:%S%z")
  # Convierte -0500 -> -05:00
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", txt)
}

crear_ejecucion_id <- function(prefijo = "cafam") {
  paste0(prefijo, "_", format(Sys.time(), tz = "America/Bogota", "%Y%m%d_%H%M%S"))
}

asegurar_directorio <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

normalizar_url <- function(href, base = CAFAM_BASE) {
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  if (str_detect(href, "^https?://")) return(href)
  paste0(base, ifelse(str_starts(href, "/"), "", "/"), href)
}

# Descarga HTML con httr2, guarda la respuesta CRUDA y devuelve el DOM.
# La copia raw permite reproducir la extracción sin volver a consultar el sitio.
obtener_html_online <- function(url, raw_path, pausa = 0.6) {
  asegurar_directorio(dirname(raw_path))

  req <- request(url) |>
    req_user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) " %+%
        "AppleWebKit/537.36 Chrome/154 Safari/537.36"
    ) |>
    req_headers(
      `Accept-Language` = "es-CO,es;q=0.9,en;q=0.8",
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    ) |>
    req_timeout(30) |>
    req_retry(max_tries = 3)

  resp <- req_perform(req)
  resp_check_status(resp)

  raw <- resp_body_raw(resp)
  con <- file(raw_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(raw, con)

  Sys.sleep(pausa)
  resp_body_html(resp)
}

# Pequeño operador para construir el user-agent sin depender de glue.
`%+%` <- function(x, y) paste0(x, y)

obtener_html_offline <- function(raw_path) {
  if (!file.exists(raw_path)) {
    stop("No existe la copia raw requerida: ", raw_path)
  }
  read_html(raw_path)
}

# ------------------------------------------------------------
# Extracción de enlaces de la categoría
# ------------------------------------------------------------

extraer_total_reportado_cafam <- function(doc) {
  txt <- html_element(doc, "body") |> html_text2()
  m <- str_match(txt, "(?i)([0-9]+)\\s+productos")
  if (is.na(m[1, 2])) return(NA_integer_)
  as.integer(m[1, 2])
}

extraer_links_producto_cafam <- function(doc) {
  hrefs <- doc |>
    html_elements("a") |>
    html_attr("href")

  hrefs |>
    discard(is.na) |>
    keep(~ str_detect(.x, "/p(?:$|[?#])")) |>
    map_chr(normalizar_url) |>
    unique()
}

# Recorre ?page=1, ?page=2, ... hasta alcanzar el total publicado
# o hasta que una página deje de aportar productos nuevos.
recolectar_urls_cafam <- function(
  ejecucion_id,
  raw_root = "raw",
  modo = c("online", "offline"),
  pausa = 0.6,
  max_paginas = 20
) {
  modo <- match.arg(modo)
  carpeta <- file.path(raw_root, "cafam", ejecucion_id)
  asegurar_directorio(carpeta)

  urls <- character()
  total_reportado <- NA_integer_
  paginas_leidas <- 0L

  for (pagina in seq_len(max_paginas)) {
    url <- paste0(CAFAM_CATEGORIA, "?page=", pagina)
    raw_path <- file.path(carpeta, sprintf("categoria_page_%03d.html", pagina))

    doc <- if (modo == "online") {
      obtener_html_online(url, raw_path, pausa = pausa)
    } else {
      # En modo offline se detiene naturalmente cuando ya no haya más páginas raw.
      if (!file.exists(raw_path)) break
      obtener_html_offline(raw_path)
    }

    paginas_leidas <- pagina

    if (pagina == 1L) {
      total_reportado <- extraer_total_reportado_cafam(doc)
    }

    nuevos <- setdiff(extraer_links_producto_cafam(doc), urls)

    if (length(nuevos) == 0L) break
    urls <- c(urls, nuevos)

    if (!is.na(total_reportado) && length(urls) >= total_reportado) break
  }

  # Si el sitio repite enlaces o incluye recomendaciones, no truncamos silenciosamente:
  # devolvemos todo y la validación posterior señalará la diferencia.
  list(
    urls = unique(urls),
    total_reportado = total_reportado,
    total_urls = length(unique(urls)),
    paginas_leidas = paginas_leidas,
    carpeta_raw = carpeta
  )
}

# ------------------------------------------------------------
# Lectura de JSON-LD y metadatos
# ------------------------------------------------------------

buscar_productos_jsonld <- function(x) {
  encontrados <- list()

  recorrer <- function(obj) {
    if (!is.list(obj)) return(invisible(NULL))

    tipo <- obj[["@type"]]
    if (!is.null(tipo)) {
      tipo_chr <- unlist(tipo, use.names = FALSE)
      if (any(tipo_chr %in% c("Product", "IndividualProduct"))) {
        encontrados[[length(encontrados) + 1L]] <<- obj
      }
    }

    for (hijo in obj) {
      if (is.list(hijo)) recorrer(hijo)
    }
    invisible(NULL)
  }

  recorrer(x)
  encontrados
}

jsonld_productos <- function(doc) {
  scripts <- doc |>
    html_elements("script[type='application/ld+json']") |>
    html_text2()

  if (length(scripts) == 0L) return(list())

  parsed <- purrr::map(scripts, function(txt) {
    tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }) |>
    purrr::compact()

  # IMPORTANTE: usar explícitamente purrr::list_flatten().
  # jsonlite también exporta una función flatten() y, al cargarse después
  # de purrr, podía enmascararla. jsonlite::flatten() exige un data.frame
  # y producía: "is.data.frame(x) is not TRUE" al procesar JSON-LD.
  purrr::list_flatten(purrr::map(parsed, buscar_productos_jsonld))
}

primer_no_nulo <- function(...) {
  xs <- list(...)
  for (x in xs) {
    if (!is.null(x) && length(x) > 0L && !all(is.na(x))) return(x)
  }
  NULL
}

extraer_precio_jsonld <- function(productos) {
  if (length(productos) == 0L) return(NA_real_)

  for (p in productos) {
    offers <- p$offers
    if (is.null(offers)) next

    # Offers puede ser objeto o lista de objetos.
    candidatas <- if (is.list(offers) && !is.null(names(offers))) list(offers) else offers

    for (o in candidatas) {
      if (!is.list(o)) next
      val <- primer_no_nulo(o$price, o$lowPrice, o$priceSpecification$price)
      if (!is.null(val)) {
        num <- suppressWarnings(as.numeric(val))
        if (!is.na(num)) return(num)
      }
    }
  }

  NA_real_
}

extraer_sku_jsonld <- function(productos) {
  if (length(productos) == 0L) return(NA_character_)
  for (p in productos) {
    val <- primer_no_nulo(p$sku, p$productID, p$mpn)
    if (!is.null(val)) return(as.character(val[[1]]))
  }
  NA_character_
}

extraer_marca_jsonld <- function(productos) {
  if (length(productos) == 0L) return(NA_character_)
  for (p in productos) {
    b <- p$brand
    if (is.character(b) && length(b)) return(as.character(b[[1]]))
    if (is.list(b)) {
      val <- primer_no_nulo(b$name, b$brand)
      if (!is.null(val)) return(as.character(val[[1]]))
    }
  }
  NA_character_
}

# Fallback adicional: algunos sitios exponen precio en meta tags.
extraer_precio_meta <- function(doc) {
  selectores <- c(
    "meta[property='product:price:amount']",
    "meta[property='og:price:amount']",
    "meta[itemprop='price']"
  )

  for (sel in selectores) {
    nodo <- html_element(doc, sel)
    if (!inherits(nodo, "xml_missing")) {
      val <- primer_no_nulo(html_attr(nodo, "content"), html_attr(nodo, "value"))
      num <- suppressWarnings(as.numeric(val))
      if (!is.na(num)) return(num)
    }
  }
  NA_real_
}

# ------------------------------------------------------------
# Estado interno de VTEX
# ------------------------------------------------------------

# Cafam usa VTEX. La respuesta HTML contiene un objeto JavaScript
# __STATE__ con el producto, SKU, vendedor, precio y disponibilidad.
# Leer esta fuente es preferible a reconstruir el precio desde el PUM.
extraer_state_vtex <- function(doc) {
  scripts <- rvest::html_elements(doc, "script")

  if (length(scripts) == 0L) return(list())

  textos <- xml2::xml_text(scripts)
  candidatos <- textos[stringr::str_detect(textos, "__STATE__\\s*=")]

  if (length(candidatos) == 0L) return(list())

  for (txt in candidatos) {
    m <- stringr::str_match(
      txt,
      stringr::regex(
        "__STATE__\\s*=\\s*(\\{.*\\})\\s*$",
        dotall = TRUE
      )
    )

    if (!is.na(m[1, 2])) {
      estado <- tryCatch(
        jsonlite::fromJSON(m[1, 2], simplifyVector = FALSE),
        error = function(e) NULL
      )

      if (!is.null(estado)) return(estado)
    }
  }

  list()
}

# Busca primero entre los objetos de nivel superior del cache de Apollo.
# Si VTEX cambia ligeramente la estructura, hace una búsqueda recursiva.
buscar_nodos_vtex <- function(x, predicado) {
  encontrados <- list()

  agregar_si_cumple <- function(obj) {
    if (!is.list(obj)) return(invisible(NULL))

    cumple <- tryCatch(
      isTRUE(predicado(obj)),
      error = function(e) FALSE
    )

    if (cumple) {
      encontrados[[length(encontrados) + 1L]] <<- obj
    }

    for (hijo in obj) {
      if (is.list(hijo)) agregar_si_cumple(hijo)
    }

    invisible(NULL)
  }

  agregar_si_cumple(x)
  encontrados
}

extraer_oferta_vtex <- function(state) {
  vacio <- list(
    precio = NA_real_,
    precio_lista = NA_real_,
    precio_sin_descuento = NA_real_,
    spot_price = NA_real_,
    cantidad_disponible = NA_real_
  )

  if (!is.list(state) || length(state) == 0L) return(vacio)

  # En el HTML actual la oferta está guardada como un objeto de nivel
  # superior cuyo nombre termina en ".commertialOffer".
  candidatos <- purrr::keep(
    state,
    ~ is.list(.x) &&
      any(
        c(
          "Price", "ListPrice", "PriceWithoutDiscount",
          "spotPrice", "AvailableQuantity"
        ) %in% names(.x)
      )
  )

  if (length(candidatos) == 0L) {
    candidatos <- buscar_nodos_vtex(
      state,
      function(x) {
        any(
          c(
            "Price", "ListPrice", "PriceWithoutDiscount",
            "spotPrice", "AvailableQuantity"
          ) %in% names(x)
        )
      }
    )
  }

  if (length(candidatos) == 0L) return(vacio)

  oferta <- candidatos[[1]]

  num <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NA_real_)
    suppressWarnings(as.numeric(x[[1]]))
  }

  list(
    precio = num(oferta$Price),
    precio_lista = num(oferta$ListPrice),
    precio_sin_descuento = num(oferta$PriceWithoutDiscount),
    spot_price = num(oferta$spotPrice),
    cantidad_disponible = num(oferta$AvailableQuantity)
  )
}

vtex_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  val <- as.character(x[[1]])
  if (length(val) == 0L || is.na(val) || !nzchar(stringr::str_squish(val))) {
    return(NA_character_)
  }
  stringr::str_squish(val)
}

extraer_producto_vtex <- function(state, ref_id = NA_character_) {
  vacio <- list(
    producto_vtex_id = NA_character_,
    producto_referencia = NA_character_,
    producto_nombre = NA_character_,
    marca = NA_character_,
    descripcion = NA_character_
  )

  if (!is.list(state) || length(state) == 0L) return(vacio)

  candidatos <- purrr::keep(
    state,
    ~ is.list(.x) &&
      !is.null(.x$productId) &&
      (!is.null(.x$productName) || !is.null(.x$productReference))
  )

  if (length(candidatos) == 0L) return(vacio)

  # Prioriza el producto cuyo productReference coincide con el Ref Id
  # recuperado de la URL/ficha. Así evitamos tomar otro producto del estado.
  elegido <- NULL

  if (!is.na(ref_id) && nzchar(ref_id)) {
    idx_ref <- which(
      purrr::map_lgl(
        candidatos,
        ~ identical(vtex_chr(.x$productReference), as.character(ref_id))
      )
    )

    if (length(idx_ref) > 0L) {
      elegido <- candidatos[[idx_ref[[1]]]]
    }
  }

  if (is.null(elegido)) {
    elegido <- candidatos[[1]]
  }

  list(
    producto_vtex_id = vtex_chr(elegido$productId),
    producto_referencia = vtex_chr(elegido$productReference),
    producto_nombre = vtex_chr(elegido$productName),
    marca = vtex_chr(elegido$brand),
    descripcion = vtex_chr(elegido$description)
  )
}

extraer_ids_vtex <- function(state) {
  vacio <- list(
    sku_vtex = NA_character_,
    ean = NA_character_
  )

  if (!is.list(state) || length(state) == 0L) return(vacio)

  sku <- purrr::keep(
    state,
    ~ is.list(.x) && !is.null(.x$itemId)
  )

  sku_vtex <- if (length(sku)) {
    vtex_chr(sku[[1]]$itemId)
  } else {
    NA_character_
  }

  ean <- if (length(sku) && !is.null(sku[[1]]$ean)) {
    vtex_chr(sku[[1]]$ean)
  } else {
    NA_character_
  }

  list(
    sku_vtex = sku_vtex,
    ean = ean
  )
}

# ------------------------------------------------------------
# Ficha técnica Cafam
# ------------------------------------------------------------

extraer_tabla_especificaciones <- function(doc) {
  tablas <- doc |>
    html_elements("table") |>
    map(~ tryCatch(html_table(.x, fill = TRUE), error = function(e) NULL)) |>
    compact()

  if (length(tablas) == 0L) return(character())

  # Busca una tabla cuya primera columna contenga campos propios de la ficha.
  idx <- which(map_lgl(tablas, function(tb) {
    if (ncol(tb) < 2L || nrow(tb) == 0L) return(FALSE)
    any(str_detect(
      str_to_lower(as.character(tb[[1]])),
      "registro invima|principio activo|unidad de medida|contenido|presentaci"
    ))
  }))

  if (length(idx) == 0L) return(character())

  tb <- tablas[[idx[[1]]]]
  claves <- str_squish(as.character(tb[[1]]))
  valores <- str_squish(as.character(tb[[2]]))
  setNames(valores, str_to_lower(claves))
}

spec_get <- function(spec, patron) {
  if (length(spec) == 0L) return(NA_character_)
  idx <- which(str_detect(names(spec), regex(patron, ignore_case = TRUE)))
  if (length(idx) == 0L) return(NA_character_)
  unname(spec[[idx[[1]]]])
}

numero_desde_texto <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  x |>
    str_replace_all("\\.", "") |>
    str_replace(",", ".") |>
    str_extract("[0-9]+(?:\\.[0-9]+)?") |>
    as.numeric()
}

extraer_ref_id <- function(doc, url) {
  txt <- html_element(doc, "body") |> html_text2()
  m <- str_match(txt, "(?i)Ref\\s*Id:\\s*([0-9A-Za-z_-]+)")
  if (!is.na(m[1, 2])) return(m[1, 2])

  m2 <- str_match(url, "([0-9]+)(?=/p(?:$|[?#]))")
  if (!is.na(m2[1, 2])) return(m2[1, 2])
  NA_character_
}

texto_nodo_o_na <- function(doc, selector) {
  nodo <- rvest::html_element(doc, selector)
  if (inherits(nodo, "xml_missing")) return(NA_character_)
  txt <- rvest::html_text2(nodo)
  if (length(txt) == 0L || is.na(txt) || !nzchar(stringr::str_squish(txt))) {
    return(NA_character_)
  }
  stringr::str_squish(txt)
}

extraer_descripcion_producto <- function(doc) {
  # Intenta conservar el texto descriptivo sin normalizarlo.
  body <- html_element(doc, "body") |> html_text2()
  m <- str_match(
    body,
    regex("Descripción del producto\\s*(.*?)\\s*Ref\\s*Id:", ignore_case = TRUE, dotall = TRUE)
  )
  if (is.na(m[1, 2])) return(NA_character_)
  str_squish(m[1, 2])
}

extraer_producto_cafam_doc <- function(doc, url, fecha_hora, ejecucion_id) {
  titulo_html <- texto_nodo_o_na(doc, "h1")
  spec <- extraer_tabla_especificaciones(doc)
  productos_ld <- jsonld_productos(doc)

  # Ref Id primero, para poder identificar de forma inequívoca el producto
  # correcto dentro del estado normalizado de VTEX.
  ref_id <- extraer_ref_id(doc, url)

  # Fuente principal para precio y respaldo de metadatos: __STATE__ de VTEX.
  state_vtex <- extraer_state_vtex(doc)
  oferta_vtex <- extraer_oferta_vtex(state_vtex)
  producto_vtex <- extraer_producto_vtex(state_vtex, ref_id = ref_id)
  ids_vtex <- extraer_ids_vtex(state_vtex)

  # Si el h1 no fue renderizado en el HTML, conserva el nombre original
  # expuesto por VTEX. No se inventa ni normaliza el nombre.
  titulo <- dplyr::coalesce(
    titulo_html,
    producto_vtex$producto_nombre
  )

  # Fallbacks si en el futuro VTEX deja de exponer el precio en __STATE__.
  precio_ld <- extraer_precio_jsonld(productos_ld)
  precio_meta <- extraer_precio_meta(doc)

  precio <- dplyr::case_when(
    !is.na(oferta_vtex$precio) ~ oferta_vtex$precio,
    !is.na(precio_ld) ~ precio_ld,
    !is.na(precio_meta) ~ precio_meta,
    TRUE ~ NA_real_
  )

  fuente_precio <- dplyr::case_when(
    !is.na(oferta_vtex$precio) ~ "vtex_state_commertialOffer",
    !is.na(precio_ld) ~ "jsonld",
    !is.na(precio_meta) ~ "meta",
    TRUE ~ "no_encontrado"
  )

  sku_ld <- extraer_sku_jsonld(productos_ld)
  sku <- dplyr::coalesce(ids_vtex$sku_vtex, sku_ld)

  marca_ld <- extraer_marca_jsonld(productos_ld)
  marca_original <- dplyr::coalesce(
    marca_ld,
    producto_vtex$marca
  )

  unidad_original <- spec_get(spec, "^unidad de medida$")
  cantidad_original <- spec_get(spec, "^cantidad por unidad de medida$")
  contenido_original <- spec_get(spec, "^contenido$")
  presentacion_original <- spec_get(spec, "^presentaci")
  principio_activo <- spec_get(spec, "^principio activo$")
  laboratorio <- spec_get(spec, "laboratorio fabricante")
  via <- spec_get(spec, "vía de administración|via de administracion")
  unidades_empaque <- spec_get(spec, "unidades por empaque")
  invima <- spec_get(spec, "registro invima")
  pum_txt <- spec_get(spec, "^pum$")
  descripcion_html <- extraer_descripcion_producto(doc)
  descripcion <- dplyr::coalesce(
    descripcion_html,
    producto_vtex$descripcion
  )

  precio_antes <- dplyr::case_when(
    !is.na(oferta_vtex$precio_lista) &&
      !is.na(precio) &&
      oferta_vtex$precio_lista > precio ~ oferta_vtex$precio_lista,
    !is.na(oferta_vtex$precio_sin_descuento) &&
      !is.na(precio) &&
      oferta_vtex$precio_sin_descuento > precio ~ oferta_vtex$precio_sin_descuento,
    TRUE ~ NA_real_
  )

  promo_precio <- !is.na(precio_antes)

  promo_texto <- if (is.na(titulo)) {
    FALSE
  } else {
    stringr::str_detect(
      titulo,
      stringr::regex("\\boferta\\b|promoci[oó]n|descuento", ignore_case = TRUE)
    )
  }

  texto_ficha <- if (length(spec)) {
    paste(paste(names(spec), unname(spec), sep = ": "), collapse = " | ")
  } else {
    NA_character_
  }

  tibble(
    comercio = "Droguerías Cafam",
    categoria_origen = "Dermatológicos",
    url_categoria = CAFAM_CATEGORIA,
    producto_comercio_id = ref_id,
    producto_vtex_id = producto_vtex$producto_vtex_id,
    sku = sku,
    ean = ids_vtex$ean,
    producto_original = titulo,
    marca_original = marca_original,
    laboratorio_original = laboratorio,
    subcategoria_original = NA_character_,
    presentacion_original = presentacion_original,
    principio_activo_original = principio_activo,
    via_administracion_original = via,
    contenido_original = contenido_original,
    contenido_valor = numero_desde_texto(cantidad_original),
    unidad_original = unidad_original,
    unidades_empaque_original = unidades_empaque,
    invima = invima,
    precio_cop = precio,
    precio_fuente = fuente_precio,
    precio_lista_cop = oferta_vtex$precio_lista,
    precio_sin_descuento_cop = oferta_vtex$precio_sin_descuento,
    spot_price_cop = oferta_vtex$spot_price,
    precio_antes_cop = precio_antes,
    promocion = promo_precio | promo_texto,
    cantidad_disponible = oferta_vtex$cantidad_disponible,
    pum_cop = numero_desde_texto(pum_txt),
    url = url,
    descripcion_original = descripcion,
    ficha_tecnica_original = texto_ficha,
    texto_original = str_squish(
      paste(na.omit(c(titulo, descripcion, texto_ficha)), collapse = " | ")
    ),
    fecha_hora_consulta = fecha_hora,
    ejecucion_id = ejecucion_id,
    comparable = 0L,
    estado_extraccion = dplyr::case_when(
      is.na(titulo) | titulo == "" ~ "incompleto",
      TRUE ~ "ok"
    )
  )
}


# Diagnóstico rápido de una copia raw individual.
# Útil para verificar el parser sin hacer solicitudes al sitio.
diagnosticar_cafam_raw <- function(raw_file) {
  doc <- obtener_html_offline(raw_file)
  productos_ld <- jsonld_productos(doc)

  state_vtex <- extraer_state_vtex(doc)
  oferta_vtex <- extraer_oferta_vtex(state_vtex)
  ref_id_diag <- extraer_ref_id(doc, NA_character_)
  producto_vtex <- extraer_producto_vtex(state_vtex, ref_id = ref_id_diag)
  ids_vtex <- extraer_ids_vtex(state_vtex)

  tibble::tibble(
    archivo = raw_file,
    titulo = texto_nodo_o_na(doc, "h1"),
    titulo_vtex = producto_vtex$producto_nombre,
    jsonld_productos = length(productos_ld),
    ref_id = ref_id_diag,
    producto_vtex_id = producto_vtex$producto_vtex_id,
    sku_vtex = ids_vtex$sku_vtex,
    ean_vtex = ids_vtex$ean,
    precio_vtex = oferta_vtex$precio,
    precio_lista_vtex = oferta_vtex$precio_lista,
    precio_sin_descuento_vtex = oferta_vtex$precio_sin_descuento,
    spot_price_vtex = oferta_vtex$spot_price,
    cantidad_disponible_vtex = oferta_vtex$cantidad_disponible,
    fuente_vtex = ifelse(
      is.na(oferta_vtex$precio),
      NA_character_,
      "vtex_state_commertialOffer"
    ),
    precio_jsonld = extraer_precio_jsonld(productos_ld),
    precio_meta = extraer_precio_meta(doc),
    tablas_html = length(rvest::html_elements(doc, "table"))
  )
}

# ------------------------------------------------------------
# Scraper principal Cafam
# ------------------------------------------------------------

scrapear_cafam <- function(
  modo = c("online", "offline"),
  raw_root = "raw",
  ejecucion_id = NULL,
  pausa = 0.6,
  max_paginas = 20,
  verbose = TRUE
) {
  modo <- match.arg(modo)

  if (is.null(ejecucion_id)) {
    if (modo == "offline") {
      stop("En modo offline debe indicar el ejecucion_id de una corrida existente.")
    }
    ejecucion_id <- crear_ejecucion_id("cafam")
  }

  inicio <- iso_colombia()

  info_cat <- recolectar_urls_cafam(
    ejecucion_id = ejecucion_id,
    raw_root = raw_root,
    modo = modo,
    pausa = pausa,
    max_paginas = max_paginas
  )

  carpeta <- info_cat$carpeta_raw
  manifest_path <- file.path(carpeta, "manifest_productos.csv")

  if (modo == "online") {
    manifest <- tibble(
      orden = seq_along(info_cat$urls),
      url = info_cat$urls,
      raw_file = file.path(
        carpeta,
        sprintf("producto_%03d.html", seq_along(info_cat$urls))
      ),
      fecha_hora_consulta = NA_character_
    )
  } else {
    if (file.exists(manifest_path)) {
      manifest <- read_csv(manifest_path, show_col_types = FALSE)
    } else {
      # Recuperación robusta del modo offline:
      # 1) si existen páginas raw de categoría, usa sus URLs;
      # 2) si no existen, reconstruye las URLs desde los <link rel="canonical">
      #    de los HTML crudos producto_XXX.html.

      if (length(info_cat$urls) > 0L) {
        urls_manifest <- info_cat$urls

        raw_files <- file.path(
          carpeta,
          sprintf("producto_%03d.html", seq_along(urls_manifest))
        )

        faltantes_raw <- raw_files[!file.exists(raw_files)]
        if (length(faltantes_raw) > 0L) {
          stop(
            "No existe manifest_productos.csv y faltan ",
            length(faltantes_raw),
            " archivos HTML de producto. Primero revise la carpeta raw."
          )
        }
      } else {
        raw_files <- list.files(
          carpeta,
          pattern = "^producto_[0-9]+\\.html$",
          full.names = TRUE
        )

        if (length(raw_files) == 0L) {
          stop(
            "No existe manifest_productos.csv, no hay páginas raw de categoría ",
            "y tampoco se encontraron archivos producto_XXX.html para la ejecución ",
            ejecucion_id
          )
        }

        # Orden numérico estable: producto_001, producto_002, ...
        orden_raw <- suppressWarnings(
          as.integer(
            stringr::str_match(
              basename(raw_files),
              "^producto_([0-9]+)\\.html$"
            )[, 2]
          )
        )

        raw_files <- raw_files[order(orden_raw, na.last = TRUE)]

        extraer_canonical_raw <- function(path) {
          doc_raw <- obtener_html_offline(path)

          canonical <- doc_raw |>
            rvest::html_element("link[rel='canonical']") |>
            rvest::html_attr("href")

          if (
            length(canonical) == 0L ||
            is.na(canonical) ||
            !nzchar(stringr::str_squish(canonical))
          ) {
            canonical <- doc_raw |>
              rvest::html_element("meta[property='og:url']") |>
              rvest::html_attr("content")
          }

          if (
            length(canonical) == 0L ||
            is.na(canonical) ||
            !nzchar(stringr::str_squish(canonical))
          ) {
            return(NA_character_)
          }

          stringr::str_squish(canonical)
        }

        urls_manifest <- purrr::map_chr(
          raw_files,
          ~ tryCatch(
            extraer_canonical_raw(.x),
            error = function(e) NA_character_
          )
        )

        if (any(is.na(urls_manifest))) {
          stop(
            "Se encontraron ", length(raw_files),
            " HTML de producto, pero ",
            sum(is.na(urls_manifest)),
            " no contienen una URL canonical/og:url recuperable."
          )
        }

        # Actualiza el resumen de cobertura con lo recuperado directamente
        # de las copias raw de productos.
        info_cat$urls <- urls_manifest
        info_cat$total_urls <- length(unique(urls_manifest))

        # Si existe un resumen anterior de la corrida online, recupera el
        # número de productos que reportaba el sitio (por ejemplo, 41).
        resumen_previo_path <- file.path(carpeta, "resumen_ejecucion.csv")

        if (file.exists(resumen_previo_path)) {
          resumen_previo <- tryCatch(
            readr::read_csv(resumen_previo_path, show_col_types = FALSE),
            error = function(e) NULL
          )

          if (
            !is.null(resumen_previo) &&
            "productos_reportados_sitio" %in% names(resumen_previo) &&
            nrow(resumen_previo) >= 1L
          ) {
            valor_reportado <- suppressWarnings(
              as.integer(resumen_previo$productos_reportados_sitio[[1]])
            )

            if (!is.na(valor_reportado)) {
              info_cat$total_reportado <- valor_reportado
            }
          }
        }

        warning(
          "No se encontraron páginas raw de categoría. ",
          "Las URLs del manifest se reconstruyeron directamente desde ",
          length(raw_files),
          " archivos producto_XXX.html."
        )
      }

      # Intentamos recuperar la fecha/hora original desde una extracción
      # anterior si existe.
      extraido_previo <- file.path(carpeta, "cafam_extraido.csv")

      fecha_base <- NA_character_

      # Fallback: reconstruir el instante de la ejecución a partir del ID:
      # cafam_YYYYMMDD_HHMMSS -> YYYY-MM-DDTHH:MM:SS-05:00
      m_fecha <- stringr::str_match(
        ejecucion_id,
        "^cafam_([0-9]{8})_([0-9]{6})$"
      )

      if (!is.na(m_fecha[1, 2]) && !is.na(m_fecha[1, 3])) {
        f <- m_fecha[1, 2]
        h <- m_fecha[1, 3]

        fecha_base <- sprintf(
          "%s-%s-%sT%s:%s:%s-05:00",
          substr(f, 1, 4),
          substr(f, 5, 6),
          substr(f, 7, 8),
          substr(h, 1, 2),
          substr(h, 3, 4),
          substr(h, 5, 6)
        )
      }

      manifest <- tibble(
        orden = seq_along(urls_manifest),
        url = urls_manifest,
        raw_file = raw_files,
        fecha_hora_consulta = fecha_base
      )

      if (file.exists(extraido_previo)) {
        previo <- tryCatch(
          readr::read_csv(extraido_previo, show_col_types = FALSE),
          error = function(e) NULL
        )

        if (
          !is.null(previo) &&
          all(c("url", "fecha_hora_consulta") %in% names(previo))
        ) {
          fechas_previas <- previo |>
            dplyr::select(url, fecha_hora_consulta) |>
            dplyr::filter(
              !is.na(url),
              !is.na(fecha_hora_consulta)
            ) |>
            dplyr::distinct(url, .keep_all = TRUE)

          manifest <- manifest |>
            dplyr::left_join(
              fechas_previas,
              by = "url",
              suffix = c("_reconstruida", "_previa")
            ) |>
            dplyr::mutate(
              fecha_hora_consulta = dplyr::coalesce(
                fecha_hora_consulta_previa,
                fecha_hora_consulta_reconstruida
              )
            ) |>
            dplyr::select(
              orden,
              url,
              raw_file,
              fecha_hora_consulta
            )
        }
      }

      readr::write_csv(manifest, manifest_path)

      warning(
        "No se encontró manifest_productos.csv. Se reconstruyó automáticamente ",
        "a partir de los archivos raw y se guardó en: ",
        manifest_path
      )
    }
  }

  resultados <- vector("list", nrow(manifest))

  for (i in seq_len(nrow(manifest))) {
    if (verbose) {
      message(sprintf("Cafam: producto %d/%d", i, nrow(manifest)))
    }

    url <- manifest$url[[i]]
    raw_file <- manifest$raw_file[[i]]

    tryCatch({
      if (modo == "online") {
        doc <- obtener_html_online(url, raw_file, pausa = pausa)
        fecha_hora <- iso_colombia()
        manifest$fecha_hora_consulta[[i]] <- fecha_hora
        # Guardado incremental para no perder el manifiesto si la corrida se interrumpe.
        write_csv(manifest, manifest_path)
      } else {
        doc <- obtener_html_offline(raw_file)
        fecha_hora <- manifest$fecha_hora_consulta[[i]]
      }

      resultados[[i]] <- extraer_producto_cafam_doc(
        doc = doc,
        url = url,
        fecha_hora = fecha_hora,
        ejecucion_id = ejecucion_id
      )
    }, error = function(e) {
      resultados[[i]] <<- tibble(
        comercio = "Droguerías Cafam",
        categoria_origen = "Dermatológicos",
        url_categoria = CAFAM_CATEGORIA,
        producto_comercio_id = NA_character_,
        producto_vtex_id = NA_character_,
        sku = NA_character_,
        ean = NA_character_,
        producto_original = NA_character_,
        marca_original = NA_character_,
        laboratorio_original = NA_character_,
        subcategoria_original = NA_character_,
        presentacion_original = NA_character_,
        principio_activo_original = NA_character_,
        via_administracion_original = NA_character_,
        contenido_original = NA_character_,
        contenido_valor = NA_real_,
        unidad_original = NA_character_,
        unidades_empaque_original = NA_character_,
        invima = NA_character_,
        precio_cop = NA_real_,
        precio_fuente = "error",
        precio_lista_cop = NA_real_,
        precio_sin_descuento_cop = NA_real_,
        spot_price_cop = NA_real_,
        precio_antes_cop = NA_real_,
        promocion = NA,
        cantidad_disponible = NA_real_,
        pum_cop = NA_real_,
        url = url,
        descripcion_original = NA_character_,
        ficha_tecnica_original = NA_character_,
        texto_original = NA_character_,
        fecha_hora_consulta = ifelse(modo == "online", iso_colombia(), manifest$fecha_hora_consulta[[i]]),
        ejecucion_id = ejecucion_id,
        comparable = 0L,
        estado_extraccion = paste0("error: ", conditionMessage(e))
      )
    })
  }

  datos <- bind_rows(resultados)
  fin <- iso_colombia()

  resumen <- tibble(
    ejecucion_id = ejecucion_id,
    comercio = "Droguerías Cafam",
    fecha_hora_inicio = inicio,
    fecha_hora_fin = fin,
    modo = modo,
    productos_reportados_sitio = info_cat$total_reportado,
    urls_productos_detectadas = info_cat$total_urls,
    registros_obtenidos = nrow(datos),
    registros_ok = sum(datos$estado_extraccion == "ok", na.rm = TRUE),
    registros_error = sum(stringr::str_detect(datos$estado_extraccion, "^error"), na.rm = TRUE),
    registros_incompletos = sum(datos$estado_extraccion == "incompleto", na.rm = TRUE),
    precios_encontrados = sum(!is.na(datos$precio_cop)),
    precios_faltantes = sum(is.na(datos$precio_cop)),
    paginas_categoria_leidas = info_cat$paginas_leidas,
    estado = case_when(
      nrow(datos) == 0 ~ "fallida",
      (!is.na(info_cat$total_reportado) && nrow(datos) != info_cat$total_reportado) ~ "parcial",
      any(stringr::str_detect(datos$estado_extraccion, "^error"), na.rm = TRUE) ~ "parcial",
      any(datos$estado_extraccion == "incompleto", na.rm = TRUE) ~ "parcial",
      any(is.na(datos$precio_cop)) ~ "parcial",
      TRUE ~ "completa"
    )
  )

  write_csv(datos, file.path(carpeta, "cafam_extraido.csv"))
  write_csv(resumen, file.path(carpeta, "resumen_ejecucion.csv"))

  # Validaciones visibles: NO detienen la extracción, pero dejan claro qué revisar.
  if (verbose) {
    message("\n--- Validación Cafam ---")
    message("Productos reportados por el sitio: ", resumen$productos_reportados_sitio)
    message("URLs únicas detectadas: ", resumen$urls_productos_detectadas)
    message("Registros obtenidos: ", resumen$registros_obtenidos)
    message("Precios encontrados: ", resumen$precios_encontrados)

    if (!is.na(resumen$productos_reportados_sitio) &&
        resumen$urls_productos_detectadas != resumen$productos_reportados_sitio) {
      warning(
        "La cantidad de URLs detectadas no coincide con el total publicado por Cafam. ",
        "Revisar paginación/selectores antes de considerar la corrida completa."
      )
    }

    if (any(is.na(datos$precio_cop))) {
      warning(
        sum(is.na(datos$precio_cop)),
        " productos quedaron sin precio total. Revisar VTEX __STATE__, JSON-LD y meta tags."
      )
    }
  }

  list(
    datos = datos,
    ejecucion = resumen,
    manifest = manifest,
    raw_dir = carpeta
  )
}

# ============================================================
# Cruz Verde - Dermatológicos
# Versión: 0.2.1
#
# Requisitos cubiertos:
#   - categoría completa por Search-UpdateGrid
#   - raw HTML por página de categoría y por producto
#   - modo online y offline
#   - no fabrica precios faltantes
#   - conserva URL, ID interno, texto visible, timestamp y ejecución
#   - extrae precio, precio normal, promoción, PUM y ficha técnica
# ============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readr)
  library(jsonlite)
})

CV_BASE <- "https://beta1.cruzverde.com.co"

CV_CATEGORIA <- paste0(
  CV_BASE,
  "/medicamentos/dermatologicos/"
)

CV_GRID <- paste0(
  CV_BASE,
  "/on/demandware.store/Sites-Colombia-Site/es_CO/",
  "Search-UpdateGrid"
)

CV_CGID <- "medicamento-dermat"

CV_UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36",
  "Chrome/142 Safari/537.36"
)

# ------------------------------------------------------------
# Utilidades
# ------------------------------------------------------------

iso_colombia_cv <- function() {
  format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%S%z",
    tz = "America/Bogota"
  ) |>
    stringr::str_replace(
      "([+-][0-9]{2})([0-9]{2})$",
      "\\1:\\2"
    )
}

crear_id_ejecucion_cv <- function() {
  paste0(
    "cruzverde_",
    format(
      Sys.time(),
      "%Y%m%d_%H%M%S",
      tz = "America/Bogota"
    )
  )
}

request_cv <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(CV_UA) |>
    httr2::req_headers(
      `Accept-Language` =
        "es-CO,es;q=0.9,en;q=0.8",
      Accept =
        "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(
      max_tries = 3,
      backoff = ~ 1
    )
}

descargar_html_cv <- function(
  url,
  pausa = 0.8
) {
  resp <- request_cv(url) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)

  if (status != 200L) {
    stop(
      "Cruz Verde respondió HTTP ",
      status,
      " para ",
      url
    )
  }

  txt <- httr2::resp_body_string(resp)

  if (
    is.na(txt) ||
    !nzchar(txt)
  ) {
    stop(
      "Cruz Verde devolvió HTML vacío para ",
      url
    )
  }

  if (pausa > 0) {
    Sys.sleep(pausa)
  }

  txt
}

guardar_html_cv <- function(
  texto,
  archivo
) {
  writeLines(
    texto,
    archivo,
    useBytes = TRUE
  )

  invisible(archivo)
}

leer_html_cv <- function(path) {
  if (!file.exists(path)) {
    stop(
      "No existe el raw esperado: ",
      path
    )
  }

  rvest::read_html(path)
}

texto_body_cv <- function(doc) {
  nodo <- rvest::html_element(
    doc,
    "body"
  )

  if (
    inherits(
      nodo,
      "xml_missing"
    )
  ) {
    return(NA_character_)
  }

  rvest::html_text2(
    nodo
  )
}

primer_chr_cv <- function(...) {
  xs <- list(...)

  for (x in xs) {
    if (
      length(x) > 0L &&
      !is.null(x) &&
      !is.na(x[[1]])
    ) {
      y <- as.character(
        x[[1]]
      ) |>
        stringr::str_squish()

      if (nzchar(y)) {
        return(y)
      }
    }
  }

  NA_character_
}

texto_selector_cv <- function(
  doc,
  selectores
) {
  for (sel in selectores) {
    nodos <- tryCatch(
      rvest::html_elements(
        doc,
        sel
      ),
      error = function(e) {
        xml2::xml_missing()
      }
    )

    if (length(nodos) == 0L) {
      next
    }

    valores <- purrr::map_chr(
      nodos,
      ~ rvest::html_text2(.x) |>
        stringr::str_squish()
    )

    valores <- valores[
      !is.na(valores) &
        nzchar(valores)
    ]

    if (length(valores) > 0L) {
      return(
        valores[[1]]
      )
    }
  }

  NA_character_
}

atributo_selector_cv <- function(
  doc,
  selectores,
  atributo
) {
  for (sel in selectores) {
    nodos <- tryCatch(
      rvest::html_elements(
        doc,
        sel
      ),
      error = function(e) {
        xml2::xml_missing()
      }
    )

    if (length(nodos) == 0L) {
      next
    }

    valores <- rvest::html_attr(
      nodos,
      atributo
    )

    valores <- valores[
      !is.na(valores) &
        nzchar(valores)
    ]

    if (length(valores) > 0L) {
      return(
        valores[[1]]
      )
    }
  }

  NA_character_
}

# ------------------------------------------------------------
# URLs / IDs
# ------------------------------------------------------------

normalizar_url_producto_cv <- function(href) {
  if (
    length(href) == 0L ||
    is.na(href) ||
    !nzchar(
      stringr::str_trim(href)
    )
  ) {
    return(NA_character_)
  }

  href <- stringr::str_trim(
    href
  )

  if (
    stringr::str_detect(
      href,
      "^https?://"
    )
  ) {
    url <- href
  } else {
    if (
      !stringr::str_starts(
        href,
        "/"
      )
    ) {
      href <- paste0(
        "/",
        href
      )
    }

    url <- paste0(
      CV_BASE,
      href
    )
  }

  url |>
    stringr::str_remove(
      "[?#].*$"
    )
}

producto_id_cv <- function(url) {
  id <- stringr::str_match(
    url,
    "COCV_([0-9]+)\\.html"
  )[, 2]

  ifelse(
    is.na(id),
    NA_character_,
    id
  )
}

url_producto_cv_valida <- function(url) {
  !is.na(url) &&
    stringr::str_detect(
      url,
      "^https://beta1\\.cruzverde\\.com\\.co/.+/COCV_[0-9]+\\.html$"
    )
}

extraer_urls_productos_cv <- function(doc) {
  hrefs <- doc |>
    rvest::html_elements(
      "a[href*='COCV_']"
    ) |>
    rvest::html_attr(
      "href"
    )

  if (length(hrefs) == 0L) {
    return(character())
  }

  hrefs |>
    purrr::map_chr(
      normalizar_url_producto_cv
    ) |>
    (\(x) x[!is.na(x)])() |>
    (\(x) x[
      stringr::str_detect(
        x,
        "COCV_[0-9]+\\.html$"
      )
    ])() |>
    unique()
}

# ------------------------------------------------------------
# Paginación de categoría
# ------------------------------------------------------------

extraer_data_urls_paginacion_cv <- function(doc) {
  doc |>
    rvest::html_elements(
      "[data-url*='Search-UpdateGrid']"
    ) |>
    rvest::html_attr(
      "data-url"
    ) |>
    na.omit() |>
    unique()
}

extraer_parametro_num_cv <- function(
  url,
  parametro
) {
  patron <- paste0(
    "(?:\\?|&)",
    parametro,
    "=([0-9]+)"
  )

  x <- stringr::str_match(
    url,
    patron
  )[, 2]

  suppressWarnings(
    as.integer(x)
  )
}

diagnosticar_paginacion_inicial_cv <- function(doc) {
  data_urls <-
    extraer_data_urls_paginacion_cv(
      doc
    )

  starts <- extraer_parametro_num_cv(
    data_urls,
    "start"
  )

  szs <- extraer_parametro_num_cv(
    data_urls,
    "sz"
  )

  starts <- starts[
    !is.na(starts)
  ]

  szs <- szs[
    !is.na(szs)
  ]

  max_start <- if (
    length(starts) > 0L
  ) {
    max(starts)
  } else {
    NA_integer_
  }

  tam_pagina <- if (
    length(szs) > 0L
  ) {
    as.integer(
      names(
        sort(
          table(szs),
          decreasing = TRUE
        )
      )[[1]]
    )
  } else {
    12L
  }

  if (is.na(max_start)) {
    stop(
      paste(
        "No se pudo inferir la última página",
        "de Cruz Verde."
      )
    )
  }

  starts_completos <- seq.int(
    0L,
    max_start,
    by = tam_pagina
  )

  tibble::tibble(
    max_start_detectado =
      max_start,
    productos_por_pagina =
      tam_pagina,
    paginas_inferidas =
      length(
        starts_completos
      ),
    starts =
      list(
        starts_completos
      )
  )
}

construir_url_grid_cv <- function(
  start,
  sz = 12L
) {
  paste0(
    CV_GRID,
    "?cgid=",
    CV_CGID,
    "&start=",
    as.integer(start),
    "&sz=",
    as.integer(sz)
  )
}

# ------------------------------------------------------------
# Categoría completa
# ------------------------------------------------------------

scrapear_categoria_cruz_verde <- function(
  modo = c(
    "online",
    "offline"
  ),
  raw_root = "raw",
  ejecucion_id = NULL,
  pausa = 0.8
) {
  modo <- match.arg(
    modo
  )

  if (
    is.null(ejecucion_id)
  ) {
    if (
      modo == "offline"
    ) {
      stop(
        "En modo offline debes indicar ejecucion_id."
      )
    }

    ejecucion_id <-
      crear_id_ejecucion_cv()
  }

  inicio <- iso_colombia_cv()

  carpeta <- file.path(
    raw_root,
    "cruz_verde",
    ejecucion_id
  )

  if (
    modo == "online"
  ) {
    dir.create(
      carpeta,
      recursive = TRUE,
      showWarnings = FALSE
    )
  } else if (
    !dir.exists(carpeta)
  ) {
    stop(
      "No existe la carpeta raw: ",
      carpeta
    )
  }

  raw_inicial <- file.path(
    carpeta,
    "categoria_page_001.html"
  )

  if (
    modo == "online" &&
    !file.exists(raw_inicial)
  ) {
    message(
      "Cruz Verde: leyendo página inicial"
    )

    txt_inicial <- descargar_html_cv(
      CV_CATEGORIA,
      pausa = pausa
    )

    guardar_html_cv(
      txt_inicial,
      raw_inicial
    )
  }

  doc_inicial <- leer_html_cv(
    raw_inicial
  )

  diag_pag <-
    diagnosticar_paginacion_inicial_cv(
      doc_inicial
    )

  starts <- diag_pag$starts[[1]]
  sz <- diag_pag$productos_por_pagina[[1]]

  message(
    "Cruz Verde: ",
    length(starts),
    " páginas inferidas; tamaño = ",
    sz
  )

  urls_acumuladas <- character()
  diagnostico <- vector(
    "list",
    length(starts)
  )

  for (
    i in seq_along(starts)
  ) {
    start_i <- starts[[i]]

    archivo_i <- file.path(
      carpeta,
      sprintf(
        "categoria_page_%03d.html",
        i
      )
    )

    if (
      i > 1L &&
      modo == "online" &&
      !file.exists(archivo_i)
    ) {
      url_i <-
        construir_url_grid_cv(
          start = start_i,
          sz = sz
        )

      message(
        "Cruz Verde: página ",
        i,
        "/",
        length(starts),
        " | start=",
        start_i
      )

      txt_i <- descargar_html_cv(
        url_i,
        pausa = pausa
      )

      guardar_html_cv(
        txt_i,
        archivo_i
      )
    }

    doc_i <- leer_html_cv(
      archivo_i
    )

    urls_i <-
      extraer_urls_productos_cv(
        doc_i
      )

    nuevas_i <- setdiff(
      urls_i,
      urls_acumuladas
    )

    urls_acumuladas <- unique(
      c(
        urls_acumuladas,
        urls_i
      )
    )
    
    diagnostico[[i]] <-
      tibble::tibble(
        pagina = i,
        start = start_i,
        sz = sz,
        productos_pagina =
          length(urls_i),
        productos_nuevos =
          length(nuevas_i),
        acumulados =
          length(urls_acumuladas),
        archivo_raw =
          basename(
            archivo_i
          )
      )
  }
  
  diagnostico <-
    dplyr::bind_rows(
      diagnostico
    )
  
  ids <- purrr::map_chr(
    urls_acumuladas,
    producto_id_cv
  )
  
  productos_ultima <-
    diagnostico$productos_pagina[
      nrow(diagnostico)
    ]
  
  total_inferido <-
    max(starts) +
    productos_ultima
  
  manifest <- tibble::tibble(
    orden =
      seq_along(
        urls_acumuladas
      ),
    producto_comercio_id =
      ids,
    url_scraping =
      urls_acumuladas
  )
  
  readr::write_csv(
    diagnostico,
    file.path(
      carpeta,
      "diagnostico_categoria.csv"
    )
  )
  
  readr::write_csv(
    manifest,
    file.path(
      carpeta,
      "manifest_productos.csv"
    )
  )
  
  fin <- iso_colombia_cv()
  
  ejecucion <- tibble::tibble(
    ejecucion_id =
      ejecucion_id,
    comercio =
      "Cruz Verde",
    fecha_hora_inicio =
      inicio,
    fecha_hora_fin =
      fin,
    modo =
      modo,
    paginas_categoria =
      nrow(
        diagnostico
      ),
    productos_por_pagina =
      sz,
    total_inferido_paginacion =
      total_inferido,
    urls_productos_detectadas =
      length(
        urls_acumuladas
      ),
    ids_unicos =
      dplyr::n_distinct(
        ids
      ),
    ids_duplicados =
      sum(
        duplicated(ids)
      ),
    urls_invalidas =
      sum(
        !purrr::map_lgl(
          urls_acumuladas,
          url_producto_cv_valida
        )
      ),
    estado =
      if (
        total_inferido ==
        length(
          urls_acumuladas
        ) &&
        !any(
          duplicated(ids)
        )
      ) {
        "completa"
      } else {
        "revisar"
      }
  )
  
  list(
    ejecucion_id =
      ejecucion_id,
    carpeta_raw =
      carpeta,
    urls =
      urls_acumuladas,
    manifest =
      manifest,
    diagnostico =
      diagnostico,
    ejecucion =
      ejecucion
  )
}

# ------------------------------------------------------------
# Precio
# ------------------------------------------------------------

parsear_cop_cv <- function(x) {
  if (
    length(x) == 0L ||
    is.na(x) ||
    !nzchar(
      stringr::str_trim(x)
    )
  ) {
    return(NA_real_)
  }
  
  y <- as.character(x) |>
    stringr::str_squish() |>
    stringr::str_replace_all(
      "[^0-9,.]",
      ""
    )
  
  if (!nzchar(y)) {
    return(NA_real_)
  }
  
  # Precios colombianos del sitio:
  # 95.940 -> 95940
  # 1.599,00 -> 1599
  if (
    stringr::str_detect(
      y,
      ",[0-9]{2}$"
    )
  ) {
    y <- y |>
      stringr::str_replace_all(
        "\\.",
        ""
      ) |>
      stringr::str_replace(
        ",",
        "."
      )
  } else {
    y <- y |>
      stringr::str_replace_all(
        "[.,]",
        ""
      )
  }
  
  suppressWarnings(
    as.numeric(y)
  )
}

extraer_precio_cv <- function(
    doc,
    texto
) {
  # Fuente preferida: nodos de precio.
  candidatos <- c(
    texto_selector_cv(
      doc,
      c(
        ".sales .value",
        ".price-sales .value",
        ".price .sales .value",
        "[class*='sales'] [class*='value']",
        "[itemprop='price']"
      )
    ),
    atributo_selector_cv(
      doc,
      c(
        "meta[itemprop='price']",
        "[itemprop='price'][content]"
      ),
      "content"
    )
  )
  
  for (x in candidatos) {
    v <- parsear_cop_cv(x)
    
    if (
      !is.na(v) &&
      v > 0
    ) {
      return(
        list(
          precio_cop = v,
          fuente = "html_css"
        )
      )
    }
  }
  
  # Respaldo sobre texto visible.
  # En la PDP, el primer valor monetario corresponde al precio
  # del producto; el PUM aparece después.
  m <- stringr::str_match(
    texto,
    "\\$\\s*([0-9]{1,3}(?:\\.[0-9]{3})+(?:,[0-9]{2})?|[0-9]+(?:,[0-9]{2})?)"
  )
  
  v <- parsear_cop_cv(
    m[, 2]
  )
  
  list(
    precio_cop = v,
    fuente =
      ifelse(
        is.na(v),
        NA_character_,
        "texto_visible"
      )
  )
}

extraer_precio_antes_cv <- function(
    doc,
    texto
) {
  # Primero nodos de precio tachado/lista.
  candidatos <- c(
    texto_selector_cv(
      doc,
      c(
        ".strike-through",
        ".strike-through .value",
        ".list .value",
        ".price .list .value",
        "[class*='strike']",
        "[class*='list-price']"
      )
    )
  )
  
  for (x in candidatos) {
    v <- parsear_cop_cv(x)
    
    if (
      !is.na(v) &&
      v > 0
    ) {
      return(v)
    }
  }
  
  # Texto observado en Cruz Verde:
  # "Precio reducido de $133.250 (Normal)"
  m <- stringr::str_match(
    texto,
    regex(
      "Precio\\s+reducido\\s+de\\s*\\$\\s*([0-9\\.]+(?:,[0-9]{2})?)",
      ignore_case = TRUE
    )
  )
  
  parsear_cop_cv(
    m[, 2]
  )
}

extraer_pum_cv <- function(texto) {
  m <- stringr::str_match(
    texto,
    regex(
      "PUM:\\s*([^\\r\\n]+?)\\s+a\\s*\\$\\s*([0-9\\.]+(?:,[0-9]{2})?)",
      ignore_case = TRUE
    )
  )
  
  list(
    pum_unidad =
      ifelse(
        is.na(m[, 2]),
        NA_character_,
        stringr::str_squish(
          m[, 2]
        )
      ),
    pum_cop =
      parsear_cop_cv(
        m[, 3]
      )
  )
}

# ------------------------------------------------------------
# Ficha técnica
# ------------------------------------------------------------

normalizar_etiqueta_cv <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general(
      "Latin-ASCII"
    ) |>
    stringr::str_replace_all(
      "[^a-z0-9]+",
      " "
    ) |>
    stringr::str_squish()
}

lineas_texto_cv <- function(texto) {
  if (
    is.na(texto) ||
    !nzchar(texto)
  ) {
    return(character())
  }
  
  stringr::str_split(
    texto,
    "\\r?\\n"
  )[[1]] |>
    stringr::str_squish() |>
    (\(x) x[nzchar(x)])()
}

extraer_campo_lineas_cv <- function(
    lineas,
    patron
) {
  if (length(lineas) == 0L) {
    return(NA_character_)
  }
  
  normalizadas <-
    purrr::map_chr(
      lineas,
      normalizar_etiqueta_cv
    )
  
  idx <- which(
    stringr::str_detect(
      normalizadas,
      patron
    )
  )
  
  if (length(idx) == 0L) {
    return(NA_character_)
  }
  
  for (i in idx) {
    if (i >= length(lineas)) {
      next
    }
    
    candidato <-
      lineas[[i + 1L]] |>
      stringr::str_squish()
    
    candidato_norm <-
      normalizar_etiqueta_cv(
        candidato
      )
    
    # Evitar devolver la siguiente etiqueta como valor.
    if (
      nzchar(candidato) &&
      !stringr::str_detect(
        candidato_norm,
        "^(registro invima|descripcion articulo|principio activo|concentracion|presentacion|tecnica|uso)$"
      )
    ) {
      return(candidato)
    }
  }
  
  NA_character_
}

extraer_bloque_ficha_cv <- function(
    texto,
    etiqueta,
    siguientes
) {
  if (
    is.na(texto) ||
    !nzchar(texto)
  ) {
    return(NA_character_)
  }
  
  limite <- paste(
    siguientes,
    collapse = "|"
  )
  
  patron <- paste0(
    "(?is)",
    etiqueta,
    "\\s*:\\s*",
    "(.*?)",
    "\\s*(?=",
    limite,
    "|$)"
  )
  
  m <- stringr::str_match(
    texto,
    stringr::regex(
      patron,
      ignore_case = TRUE
    )
  )
  
  if (
    ncol(m) < 2L ||
    is.na(m[, 2])
  ) {
    return(NA_character_)
  }
  
  valor <- m[, 2] |>
    stringr::str_squish()
  
  if (
    !nzchar(valor)
  ) {
    NA_character_
  } else {
    valor
  }
}

extraer_ficha_cv <- function(
    doc,
    texto
) {
  # La ficha técnica de Cruz Verde se presenta en el texto visible como:
  #
  # Registro Invima:
  # <valor>
  # DESCRIPCION ARTÍCULO:
  # <valor>
  # PRINCIPIO ACTIVO:
  # <valor>
  # CONCENTRACIÓN:
  # <valor>
  # PRESENTACIÓN:
  # <valor>
  # TÉCNICA:
  # <valor>
  #
  # html_text2() no siempre conserva "etiqueta" y "valor" en líneas
  # separadas. Por eso la fuente principal es extracción por bloques entre
  # etiquetas y el parser por líneas queda como respaldo.
  
  et_invima <- "Registro\\s+Invima"
  et_desc <- "DESCRIPCION\\s+ART[ÍI]CULO"
  et_principio <- "PRINCIPIO\\s+ACTIVO"
  et_concentracion <- "CONCENTRACI[ÓO]N"
  et_presentacion <- "PRESENTACI[ÓO]N"
  et_tecnica <- "T[ÉE]CNICA"
  et_uso <- "USO"
  
  invima <- extraer_bloque_ficha_cv(
    texto,
    et_invima,
    c(
      et_desc,
      et_principio,
      et_concentracion,
      et_presentacion,
      et_tecnica,
      et_uso
    )
  )
  
  descripcion <- extraer_bloque_ficha_cv(
    texto,
    et_desc,
    c(
      et_principio,
      et_concentracion,
      et_presentacion,
      et_tecnica,
      et_uso
    )
  )
  
  principio <- extraer_bloque_ficha_cv(
    texto,
    et_principio,
    c(
      et_concentracion,
      et_presentacion,
      et_tecnica,
      et_uso
    )
  )
  
  concentracion <- extraer_bloque_ficha_cv(
    texto,
    et_concentracion,
    c(
      et_presentacion,
      et_tecnica,
      et_uso
    )
  )
  
  presentacion <- extraer_bloque_ficha_cv(
    texto,
    et_presentacion,
    c(
      et_tecnica,
      et_uso
    )
  )
  
  tecnica <- extraer_bloque_ficha_cv(
    texto,
    et_tecnica,
    c(
      et_uso,
      "Te\\s+Puede\\s+Interesar",
      "Aviso\\s+Legal"
    )
  )
  
  # Respaldo del parser por líneas.
  lineas <- lineas_texto_cv(
    texto
  )
  
  invima <- primer_chr_cv(
    invima,
    extraer_campo_lineas_cv(
      lineas,
      "^registro invima$"
    ),
    stringr::str_extract(
      texto,
      stringr::regex(
        "INVIMA\\s+[A-Z0-9-]+",
        ignore_case = TRUE
      )
    )
  )
  
  descripcion <- primer_chr_cv(
    descripcion,
    extraer_campo_lineas_cv(
      lineas,
      "^descripcion articulo$"
    )
  )
  
  principio <- primer_chr_cv(
    principio,
    extraer_campo_lineas_cv(
      lineas,
      "^principio activo$"
    )
  )
  
  concentracion <- primer_chr_cv(
    concentracion,
    extraer_campo_lineas_cv(
      lineas,
      "^concentracion$"
    )
  )
  
  presentacion <- primer_chr_cv(
    presentacion,
    extraer_campo_lineas_cv(
      lineas,
      "^presentacion$"
    )
  )
  
  tecnica <- primer_chr_cv(
    tecnica,
    extraer_campo_lineas_cv(
      lineas,
      "^tecnica$"
    )
  )
  
  list(
    invima = invima,
    descripcion_articulo = descripcion,
    principio_activo = principio,
    concentracion = concentracion,
    presentacion = presentacion,
    tecnica = tecnica
  )
}


# ------------------------------------------------------------
# JSON-LD
# ------------------------------------------------------------

extraer_jsonld_cv <- function(doc) {
  scripts <- doc |>
    rvest::html_elements(
      "script[type='application/ld+json']"
    ) |>
    rvest::html_text()
  
  if (length(scripts) == 0L) {
    return(list())
  }
  
  objetos <- purrr::map(
    scripts,
    function(x) {
      tryCatch(
        jsonlite::fromJSON(
          x,
          simplifyVector = FALSE
        ),
        error = function(e) {
          NULL
        }
      )
    }
  )
  
  objetos[
    !purrr::map_lgl(
      objetos,
      is.null
    )
  ]
}

buscar_producto_jsonld_cv <- function(objetos) {
  revisar <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    
    if (is.list(x)) {
      tipo <- x[["@type"]]
      
      if (
        !is.null(tipo) &&
        any(
          as.character(tipo) ==
          "Product"
        )
      ) {
        return(x)
      }
      
      if (
        !is.null(x[["@graph"]]) &&
        is.list(x[["@graph"]])
      ) {
        for (g in x[["@graph"]]) {
          z <- revisar(g)
          
          if (!is.null(z)) {
            return(z)
          }
        }
      }
      
      for (elem in x) {
        if (is.list(elem)) {
          z <- revisar(elem)
          
          if (!is.null(z)) {
            return(z)
          }
        }
      }
    }
    
    NULL
  }
  
  for (obj in objetos) {
    z <- revisar(obj)
    
    if (!is.null(z)) {
      return(z)
    }
  }
  
  NULL
}

jsonld_chr_cv <- function(
    producto,
    ruta
) {
  if (is.null(producto)) {
    return(NA_character_)
  }
  
  x <- producto
  
  for (campo in ruta) {
    if (
      is.null(x) ||
      !is.list(x) ||
      is.null(x[[campo]])
    ) {
      return(NA_character_)
    }
    
    x <- x[[campo]]
  }
  
  if (
    length(x) == 0L ||
    is.null(x)
  ) {
    return(NA_character_)
  }
  
  as.character(
    x[[1]]
  )
}

# ------------------------------------------------------------
# Contenido / unidad
# ------------------------------------------------------------

extraer_contenido_cv <- function(
    titulo,
    presentacion = NA_character_
) {
  fuente <- paste(
    na.omit(
      c(
        presentacion,
        titulo
      )
    ),
    collapse = " "
  )
  
  m <- stringr::str_match(
    fuente,
    regex(
      "(?:x\\s*)?([0-9]+(?:[\\.,][0-9]+)?)\\s*(ml|mL|gr|g|mg|kg|cc)\\b",
      ignore_case = TRUE
    )
  )
  
  if (is.na(m[, 2])) {
    return(
      list(
        contenido_original =
          NA_character_,
        contenido_valor =
          NA_real_,
        unidad_original =
          NA_character_
      )
    )
  }
  
  valor <- suppressWarnings(
    as.numeric(
      stringr::str_replace(
        m[, 2],
        ",",
        "."
      )
    )
  )
  
  unidad <- m[, 3]
  
  list(
    contenido_original =
      stringr::str_squish(
        paste(
          m[, 2],
          unidad
        )
      ),
    contenido_valor =
      valor,
    unidad_original =
      unidad
  )
}

# ------------------------------------------------------------
# Breadcrumb / subcategoría
# ------------------------------------------------------------

extraer_breadcrumb_cv <- function(doc) {
  textos <- doc |>
    rvest::html_elements(
      ".breadcrumb a, [class*='breadcrumb'] a"
    ) |>
    rvest::html_text2() |>
    stringr::str_squish()
  
  unique(
    textos[
      nzchar(textos)
    ]
  )
}

extraer_subcategoria_cv <- function(doc) {
  bc <- extraer_breadcrumb_cv(
    doc
  )
  
  if (length(bc) == 0L) {
    return("Dermatológicos")
  }
  
  excluir <- normalizar_etiqueta_cv(
    c(
      "Inicio",
      "Medicamentos",
      "Dermatológicos"
    )
  )
  
  bc_norm <- purrr::map_chr(
    bc,
    normalizar_etiqueta_cv
  )
  
  candidatos <- bc[
    !bc_norm %in% excluir
  ]
  
  if (length(candidatos) == 0L) {
    "Dermatológicos"
  } else {
    candidatos[[length(candidatos)]]
  }
}

# ------------------------------------------------------------
# Parser de una ficha de producto
# ------------------------------------------------------------

parsear_producto_cruz_verde <- function(
    raw_file,
    url,
    producto_id,
    ejecucion_id,
    fecha_hora
) {
  doc <- leer_html_cv(
    raw_file
  )
  
  texto <- texto_body_cv(
    doc
  )
  
  jsonld <- buscar_producto_jsonld_cv(
    extraer_jsonld_cv(
      doc
    )
  )
  
  titulo <- primer_chr_cv(
    texto_selector_cv(
      doc,
      c(
        "h1",
        ".product-name",
        "[itemprop='name']"
      )
    ),
    jsonld_chr_cv(
      jsonld,
      c("name")
    )
  )
  
  marca <- primer_chr_cv(
    jsonld_chr_cv(
      jsonld,
      c(
        "brand",
        "name"
      )
    ),
    texto_selector_cv(
      doc,
      c(
        ".product-brand a",
        ".product-brand",
        ".brand a",
        ".brand",
        "[class*='brand'] a",
        "[class*='manufacturer'] a"
      )
    )
  )
  
  laboratorio <- primer_chr_cv(
    texto_selector_cv(
      doc,
      c(
        ".product-brand a",
        ".product-brand",
        "[class*='manufacturer'] a"
      )
    ),
    marca
  )
  
  precio <-
    extraer_precio_cv(
      doc,
      texto
    )
  
  precio_antes <-
    extraer_precio_antes_cv(
      doc,
      texto
    )
  
  pum <- extraer_pum_cv(
    texto
  )
  
  ficha <- extraer_ficha_cv(
    doc,
    texto
  )
  
  presentacion <- ficha$presentacion
  
  contenido <- extraer_contenido_cv(
    titulo,
    presentacion
  )
  
  # La PDP de Cruz Verde no expone de forma confiable la
  # subcategoría del árbol; algunos enlaces del breadcrumb pueden ser
  # marca/producto. Evitamos guardar una marca (p. ej. "Benzac") como
  # subcategoría. La categoría fuente sí está verificada.
  subcategoria <-
    "Dermatológicos"
  
  promocion <- (
    !is.na(precio_antes) &&
      !is.na(precio$precio_cop) &&
      precio_antes >
      precio$precio_cop
  ) ||
    (
      !is.na(texto) &&
        stringr::str_detect(
          texto,
          regex(
            "\\bOferta\\b|Oferta Club",
            ignore_case = TRUE
          )
        )
    )
  
  ficha_tecnica_original <-
    stringr::str_squish(
      paste(
        na.omit(
          c(
            if (!is.na(ficha$invima))
              paste0(
                "Registro Invima: ",
                ficha$invima
              ),
            if (!is.na(ficha$descripcion_articulo))
              paste0(
                "DESCRIPCION ARTÍCULO: ",
                ficha$descripcion_articulo
              ),
            if (!is.na(ficha$principio_activo))
              paste0(
                "PRINCIPIO ACTIVO: ",
                ficha$principio_activo
              ),
            if (!is.na(ficha$concentracion))
              paste0(
                "CONCENTRACIÓN: ",
                ficha$concentracion
              ),
            if (!is.na(ficha$presentacion))
              paste0(
                "PRESENTACIÓN: ",
                ficha$presentacion
              ),
            if (!is.na(ficha$tecnica))
              paste0(
                "TÉCNICA: ",
                ficha$tecnica
              )
          )
        ),
        collapse = " | "
      )
    )
  
  tibble::tibble(
    comercio =
      "Cruz Verde",
    categoria_origen =
      "Dermatológicos",
    url_categoria =
      CV_CATEGORIA,
    
    producto_comercio_id =
      producto_id,
    producto_vtex_id =
      NA_character_,
    sku =
      producto_id,
    ean =
      NA_character_,
    
    producto_original =
      titulo,
    marca_original =
      marca,
    laboratorio_original =
      laboratorio,
    subcategoria_original =
      subcategoria,
    
    presentacion_original =
      presentacion,
    principio_activo_original =
      ficha$principio_activo,
    concentracion_original =
      ficha$concentracion,
    forma_farmaceutica_original =
      NA_character_,
    via_administracion_original =
      NA_character_,
    
    contenido_original =
      contenido$contenido_original,
    contenido_valor =
      contenido$contenido_valor,
    unidad_original =
      contenido$unidad_original,
    unidades_empaque_original =
      NA_character_,
    
    invima =
      ficha$invima,
    
    precio_cop =
      precio$precio_cop,
    precio_fuente =
      precio$fuente,
    precio_lista_cop =
      precio_antes,
    precio_sin_descuento_cop =
      precio_antes,
    spot_price_cop =
      precio$precio_cop,
    precio_antes_cop =
      precio_antes,
    promocion =
      as.integer(
        isTRUE(promocion)
      ),
    
    cantidad_disponible =
      NA_real_,
    
    pum_cop =
      pum$pum_cop,
    pum_unidad_original =
      pum$pum_unidad,
    
    url =
      url,
    
    descripcion_original =
      ficha$descripcion_articulo,
    
    ficha_tecnica_original =
      ficha_tecnica_original,
    
    texto_original =
      texto,
    
    fecha_hora_consulta =
      fecha_hora,
    
    ejecucion_id =
      ejecucion_id,
    
    comparable =
      0L,
    
    estado_extraccion =
      dplyr::case_when(
        is.na(titulo) |
          !nzchar(titulo) ~
          "incompleto_nombre",
        is.na(precio$precio_cop) ~
          "ok_sin_precio",
        TRUE ~
          "ok"
      )
  )
}

# ------------------------------------------------------------
# Filas de error
# ------------------------------------------------------------

fila_error_cv <- function(
    producto_id,
    url,
    ejecucion_id,
    fecha_hora,
    estado
) {
  tibble::tibble(
    comercio = "Cruz Verde",
    categoria_origen = "Dermatológicos",
    url_categoria = CV_CATEGORIA,
    
    producto_comercio_id = producto_id,
    producto_vtex_id = NA_character_,
    sku = producto_id,
    ean = NA_character_,
    
    producto_original = NA_character_,
    marca_original = NA_character_,
    laboratorio_original = NA_character_,
    subcategoria_original = NA_character_,
    
    presentacion_original = NA_character_,
    principio_activo_original = NA_character_,
    concentracion_original = NA_character_,
    forma_farmaceutica_original = NA_character_,
    via_administracion_original = NA_character_,
    
    contenido_original = NA_character_,
    contenido_valor = NA_real_,
    unidad_original = NA_character_,
    unidades_empaque_original = NA_character_,
    
    invima = NA_character_,
    
    precio_cop = NA_real_,
    precio_fuente = NA_character_,
    precio_lista_cop = NA_real_,
    precio_sin_descuento_cop = NA_real_,
    spot_price_cop = NA_real_,
    precio_antes_cop = NA_real_,
    promocion = NA_integer_,
    
    cantidad_disponible = NA_real_,
    
    pum_cop = NA_real_,
    pum_unidad_original = NA_character_,
    
    url = url,
    
    descripcion_original = NA_character_,
    ficha_tecnica_original = NA_character_,
    texto_original = NA_character_,
    
    fecha_hora_consulta = fecha_hora,
    ejecucion_id = ejecucion_id,
    
    comparable = 0L,
    estado_extraccion = estado
  )
}

# ------------------------------------------------------------
# Scraper completo
# ------------------------------------------------------------

scrapear_cruz_verde <- function(
    modo = c(
      "online",
      "offline"
    ),
    raw_root = "raw",
    ejecucion_id = NULL,
    pausa = 0.8
) {
  modo <- match.arg(
    modo
  )
  
  # Si es online y se pasa un ID existente, se reutilizan
  # los raws de categoría ya descargados en v0.1.
  if (
    is.null(ejecucion_id)
  ) {
    if (
      modo == "offline"
    ) {
      stop(
        "En modo offline debes indicar ejecucion_id."
      )
    }
    
    ejecucion_id <-
      crear_id_ejecucion_cv()
  }
  
  inicio <- iso_colombia_cv()
  
  categoria <-
    scrapear_categoria_cruz_verde(
      modo =
        if (
          dir.exists(
            file.path(
              raw_root,
              "cruz_verde",
              ejecucion_id
            )
          )
        ) {
          "offline"
        } else {
          modo
        },
      raw_root =
        raw_root,
      ejecucion_id =
        ejecucion_id,
      pausa =
        pausa
    )
  
  carpeta <- categoria$carpeta_raw
  
  manifest_base <-
    categoria$manifest
  
  manifest_detalle_path <- file.path(
    carpeta,
    "manifest_productos_detalle.csv"
  )
  
  if (
    modo == "online"
  ) {
    detalle <- vector(
      "list",
      nrow(
        manifest_base
      )
    )
    
    for (
      i in seq_len(
        nrow(
          manifest_base
        )
      )
    ) {
      id_i <-
        manifest_base$producto_comercio_id[[i]]
      
      url_i <-
        manifest_base$url_scraping[[i]]
      
      archivo_i <- file.path(
        carpeta,
        sprintf(
          "producto_%03d.html",
          i
        )
      )
      
      fecha_i <- iso_colombia_cv()
      
      message(
        "Cruz Verde: producto ",
        i,
        "/",
        nrow(manifest_base),
        " | ID ",
        id_i
      )
      
      estado_descarga <- "ok"
      mensaje_error <- NA_character_
      
      if (
        !file.exists(
          archivo_i
        )
      ) {
        tryCatch(
          {
            txt_i <- descargar_html_cv(
              url_i,
              pausa = pausa
            )
            
            guardar_html_cv(
              txt_i,
              archivo_i
            )
          },
          error = function(e) {
            estado_descarga <<- "error_descarga"
            mensaje_error <<-
              conditionMessage(e)
          }
        )
      }
      
      detalle[[i]] <-
        tibble::tibble(
          orden = i,
          producto_comercio_id =
            id_i,
          url_scraping =
            url_i,
          archivo_raw_producto =
            basename(
              archivo_i
            ),
          fecha_hora_descarga =
            fecha_i,
          estado_descarga =
            estado_descarga,
          mensaje_error =
            mensaje_error
        )
    }
    
    manifest_detalle <-
      dplyr::bind_rows(
        detalle
      )
    
    readr::write_csv(
      manifest_detalle,
      manifest_detalle_path
    )
  } else {
    if (
      !file.exists(
        manifest_detalle_path
      )
    ) {
      # Compatibilidad si los HTML existen pero aún no se creó
      # el manifiesto de detalle.
      manifest_detalle <-
        manifest_base |>
        dplyr::mutate(
          archivo_raw_producto =
            sprintf(
              "producto_%03d.html",
              orden
            ),
          fecha_hora_descarga =
            NA_character_,
          estado_descarga =
            ifelse(
              file.exists(
                file.path(
                  carpeta,
                  archivo_raw_producto
                )
              ),
              "ok",
              "raw_faltante"
            ),
          mensaje_error =
            NA_character_
        )
    } else {
      manifest_detalle <-
        readr::read_csv(
          manifest_detalle_path,
          show_col_types = FALSE,
          col_types = readr::cols(
            .default = readr::col_character(),
            orden = readr::col_integer()
          )
        )
    }
  }
  
  # ----------------------------------------------------------
  # Parseo de las fichas
  # ----------------------------------------------------------
  
  filas <- vector(
    "list",
    nrow(
      manifest_detalle
    )
  )
  
  for (
    i in seq_len(
      nrow(
        manifest_detalle
      )
    )
  ) {
    id_i <-
      as.character(
        manifest_detalle$producto_comercio_id[[i]]
      )
    
    url_i <-
      manifest_detalle$url_scraping[[i]]
    
    archivo_i <- file.path(
      carpeta,
      manifest_detalle$archivo_raw_producto[[i]]
    )
    
    fecha_i <- manifest_detalle$fecha_hora_descarga[[i]]
    
    if (
      is.na(fecha_i) ||
      !nzchar(fecha_i)
    ) {
      fecha_i <-
        inicio
    }
    
    if (
      !file.exists(
        archivo_i
      )
    ) {
      filas[[i]] <-
        fila_error_cv(
          producto_id = id_i,
          url = url_i,
          ejecucion_id = ejecucion_id,
          fecha_hora = fecha_i,
          estado = "error_raw_faltante"
        )
      
      next
    }
    
    filas[[i]] <- tryCatch(
      parsear_producto_cruz_verde(
        raw_file =
          archivo_i,
        url =
          url_i,
        producto_id =
          id_i,
        ejecucion_id =
          ejecucion_id,
        fecha_hora =
          fecha_i
      ),
      error = function(e) {
        fila_error_cv(
          producto_id =
            id_i,
          url =
            url_i,
          ejecucion_id =
            ejecucion_id,
          fecha_hora =
            fecha_i,
          estado =
            paste0(
              "error_parseo: ",
              conditionMessage(e)
            )
        )
      }
    )
  }
  
  datos <- dplyr::bind_rows(
    filas
  )
  
  fin <- iso_colombia_cv()
  
  ejecucion <- tibble::tibble(
    ejecucion_id =
      ejecucion_id,
    comercio =
      "Cruz Verde",
    fecha_hora_inicio =
      inicio,
    fecha_hora_fin =
      fin,
    modo =
      modo,
    productos_reportados_sitio =
      categoria$ejecucion$total_inferido_paginacion[[1]],
    urls_productos_detectadas =
      nrow(
        manifest_detalle
      ),
    registros_obtenidos =
      nrow(datos),
    registros_ok =
      sum(
        datos$estado_extraccion ==
          "ok"
      ),
    registros_sin_precio =
      sum(
        datos$estado_extraccion ==
          "ok_sin_precio"
      ),
    registros_error =
      sum(
        stringr::str_starts(
          datos$estado_extraccion,
          "error"
        )
      ),
    registros_incompletos =
      sum(
        datos$estado_extraccion ==
          "incompleto_nombre"
      ),
    precios_encontrados =
      sum(
        !is.na(
          datos$precio_cop
        )
      ),
    precios_faltantes =
      sum(
        is.na(
          datos$precio_cop
        )
      ),
    paginas_categoria_leidas =
      nrow(
        categoria$diagnostico
      ),
    estado =
      dplyr::case_when(
        nrow(datos) !=
          categoria$ejecucion$total_inferido_paginacion[[1]] ~
          "incompleta",
        sum(
          stringr::str_starts(
            datos$estado_extraccion,
            "error"
          )
        ) > 0 ~
          "con_errores",
        TRUE ~
          "completa"
      )
  )
  
  message(
    "\n--- Validación Cruz Verde ---"
  )
  message(
    "Productos reportados por el sitio: ",
    ejecucion$productos_reportados_sitio[[1]]
  )
  message(
    "URLs únicas detectadas: ",
    dplyr::n_distinct(
      datos$url
    )
  )
  message(
    "Registros obtenidos: ",
    nrow(datos)
  )
  message(
    "Precios encontrados: ",
    ejecucion$precios_encontrados[[1]]
  )
  
  list(
    ejecucion_id =
      ejecucion_id,
    carpeta_raw =
      carpeta,
    datos =
      datos,
    manifest =
      manifest_detalle,
    categoria =
      categoria,
    ejecucion =
      ejecucion
  )
}

# ------------------------------------------------------------
# Diagnósticos
# ------------------------------------------------------------

diagnosticar_ficha_cruz_verde <- function(
    raw_file
) {
  doc <- leer_html_cv(
    raw_file
  )
  
  texto <- texto_body_cv(
    doc
  )
  
  ficha <- extraer_ficha_cv(
    doc,
    texto
  )
  
  list(
    ficha = ficha,
    fragmento = stringr::str_extract(
      texto,
      stringr::regex(
        "Registro\\s+Invima\\s*:.*?(?=USO\\s*:|Te\\s+Puede\\s+Interesar|Aviso\\s+Legal|$)",
        ignore_case = TRUE,
        dotall = TRUE
      )
    )
  )
}

diagnosticar_enriquecimiento_cruz_verde <- function(datos) {
  datos |>
    dplyr::summarise(
      registros =
        dplyr::n(),
      con_marca =
        sum(
          !is.na(
            marca_original
          )
        ),
      con_laboratorio =
        sum(
          !is.na(
            laboratorio_original
          )
        ),
      con_subcategoria =
        sum(
          !is.na(
            subcategoria_original
          )
        ),
      con_presentacion =
        sum(
          !is.na(
            presentacion_original
          )
        ),
      con_principio_activo =
        sum(
          !is.na(
            principio_activo_original
          )
        ),
      con_concentracion =
        sum(
          !is.na(
            concentracion_original
          )
        ),
      con_invima =
        sum(
          !is.na(
            invima
          )
        ),
      con_contenido =
        sum(
          !is.na(
            contenido_original
          )
        ),
      con_pum =
        sum(
          !is.na(
            pum_cop
          )
        ),
      con_precio =
        sum(
          !is.na(
            precio_cop
          )
        ),
      con_precio_antes =
        sum(
          !is.na(
            precio_antes_cop
          )
        )
    )
}

# ============================================================
# SCRAPER LA REBAJA
# Taller 1 - Minería de Datos
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ------------------------------------------------------------
# Utilidades
# ------------------------------------------------------------

fecha_colombia <- function() {
  format(
    Sys.time(),
    tz = "America/Bogota",
    format = "%Y-%m-%dT%H:%M:%S%z"
  )
}

crear_id_ejecucion_lr <- function() {
  paste0(
    "larebaja_",
    format(
      Sys.time(),
      tz = "America/Bogota",
      format = "%Y%m%d_%H%M%S"
    )
  )
}

valor_campo_lr <- function(p, campo) {
  
  valor <- p[[campo]]
  
  if (is.null(valor)) {
    return(NA_character_)
  }
  
  valor <- unlist(
    valor,
    recursive = TRUE,
    use.names = FALSE
  )
  
  valor <- as.character(valor)
  
  valor <- valor[
    !is.na(valor) &
      nzchar(valor)
  ]
  
  if (length(valor) == 0) {
    return(NA_character_)
  }
  
  paste(
    unique(valor),
    collapse = " | "
  )
}

# ------------------------------------------------------------
# Extracción de un producto VTEX
# ------------------------------------------------------------

extraer_producto_lr <- function(
    p,
    ejecucion_id,
    rama,
    ruta_ids,
    orden,
    desde,
    fecha_extraccion
) {
  
  item <- NULL
  seller <- NULL
  oferta <- NULL
  
  if (
    !is.null(p$items) &&
    length(p$items) > 0
  ) {
    item <- p$items[[1]]
  }
  
  if (
    !is.null(item) &&
    !is.null(item$sellers) &&
    length(item$sellers) > 0
  ) {
    seller <- item$sellers[[1]]
  }
  
  if (
    !is.null(seller) &&
    !is.null(seller$commertialOffer)
  ) {
    oferta <- seller$commertialOffer
  }
  
  obtener_num <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_real_)
    }
    
    suppressWarnings(
      as.numeric(x[[1]])
    )
  }
  
  precio <- if (is.null(oferta)) {
    NA_real_
  } else {
    obtener_num(oferta$Price)
  }
  
  precio_lista <- if (is.null(oferta)) {
    NA_real_
  } else {
    obtener_num(oferta$ListPrice)
  }
  
  item_id <- if (is.null(item)) {
    NA_character_
  } else {
    as.character(
      item$itemId %||% NA_character_
    )
  }
  
  ean <- if (is.null(item)) {
    NA_character_
  } else {
    as.character(
      item$ean %||% NA_character_
    )
  }
  
  referencia <- NA_character_
  
  if (
    !is.null(item) &&
    !is.null(item$referenceId) &&
    length(item$referenceId) > 0
  ) {
    
    referencia <- as.character(
      item$referenceId[[1]]$Value %||%
        NA_character_
    )
  }
  
  categorias <- paste(
    unlist(
      p$categories %||% character(0)
    ),
    collapse = " | "
  )
  
  categorias_ids <- paste(
    unlist(
      p$categoriesIds %||% character(0)
    ),
    collapse = " | "
  )
  
  tibble(
    ejecucion_id = ejecucion_id,
    comercio = "La Rebaja",
    
    producto_comercio_id =
      as.character(
        p$productId %||% NA_character_
      ),
    
    sku_id = item_id,
    referencia = referencia,
    ean = ean,
    
    producto_original =
      as.character(
        p$productName %||% NA_character_
      ),
    
    marca_original =
      as.character(
        p$brand %||% NA_character_
      ),
    
    brand_id =
      as.character(
        p$brandId %||% NA_character_
      ),
    
    principio_activo_original =
      valor_campo_lr(
        p,
        "Principio activo"
      ),
    
    contenido_original =
      valor_campo_lr(
        p,
        "Contenido"
      ),
    
    presentacion_original =
      valor_campo_lr(
        p,
        "Presentacion"
      ),
    
    registro_sanitario_original =
      valor_campo_lr(
        p,
        "Registro Sanitario"
      ),
    
    unidad_medida_original =
      valor_campo_lr(
        p,
        "Unidadmedida"
      ),
    
    cantidad_unidad_medida =
      valor_campo_lr(
        p,
        "Cantidadunidadesmedida"
      ),
    
    presentacion_unidad_medida =
      valor_campo_lr(
        p,
        "Presentacionunidadmedida"
      ),
    
    fabricante_original =
      valor_campo_lr(
        p,
        "ML_Fabricante"
      ),
    
    laboratorio_original =
      valor_campo_lr(
        p,
        "ML_Laboratorio"
      ),
    
    precio_cop = precio,
    
    precio_antes_cop =
      precio_lista,
    
    tiene_promocion =
      !is.na(precio) &
      !is.na(precio_lista) &
      precio_lista > precio,
    
    categorias_originales =
      categorias,
    
    categorias_ids_originales =
      categorias_ids,
    
    url =
      as.character(
        p$link %||% NA_character_
      ),
    
    texto_original =
      as.character(
        p$productName %||% NA_character_
      ),
    
    rama_origen = rama,
    ruta_ids_origen = ruta_ids,
    orden_consulta = orden,
    offset_consulta = desde,
    
    fecha_extraccion =
      fecha_extraccion
  )
}

# ------------------------------------------------------------
# Descargar una página y guardar RAW
# ------------------------------------------------------------

descargar_pagina_lr <- function(
    ruta_ids,
    rama,
    desde,
    hasta,
    orden,
    raw_dir,
    ejecucion_id
) {
  
  resp <- httr2::request(
    paste0(
      "https://www.larebajavirtual.com",
      "/api/catalog_system/pub/products/search"
    )
  ) |>
    httr2::req_url_query(
      fq = paste0(
        "C:/",
        ruta_ids,
        "/"
      ),
      `_from` = desde,
      `_to` = hasta,
      O = orden
    ) |>
    httr2::req_user_agent(
      "Mozilla/5.0"
    ) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    ) |>
    httr2::req_perform()
  
  status <- httr2::resp_status(resp)
  
  nombre_archivo <- paste0(
    rama,
    "__",
    orden,
    "__",
    sprintf("%04d", desde),
    "_",
    sprintf("%04d", hasta),
    ".json"
  )
  
  ruta_archivo <- file.path(
    raw_dir,
    nombre_archivo
  )
  
  writeBin(
    httr2::resp_body_raw(resp),
    ruta_archivo
  )
  
  if (status >= 400) {
    
    return(
      list(
        datos = list(),
        status = status,
        resources = NA_character_
      )
    )
  }
  
  datos <- httr2::resp_body_json(
    resp,
    simplifyVector = FALSE
  )
  
  list(
    datos = datos,
    status = status,
    resources =
      httr2::resp_header(
        resp,
        "resources"
      )
  )
}

# ------------------------------------------------------------
# Obtener total declarado por VTEX
# ------------------------------------------------------------

extraer_total_resources <- function(x) {
  
  if (
    is.null(x) ||
    is.na(x) ||
    !nzchar(x)
  ) {
    return(NA_integer_)
  }
  
  as.integer(
    sub(
      ".*/",
      "",
      x
    )
  )
}

# ------------------------------------------------------------
# Scraping de una rama
# Usa varios ordenamientos para superar ventana VTEX
# ------------------------------------------------------------

scrapear_rama_lr <- function(
    ruta_ids,
    rama,
    raw_dir,
    ejecucion_id
) {
  
  ordenes <- c(
    "OrderByNameASC",
    "OrderByNameDESC",
    "OrderByPriceASC",
    "OrderByPriceDESC",
    "OrderByReleaseDateDESC"
  )
  
  fecha_extraccion <- fecha_colombia()
  
  registros <- list()
  
  total_esperado <- NA_integer_
  
  for (orden in ordenes) {
    
    message(
      "La Rebaja | ",
      rama,
      " | ",
      orden
    )
    
    desde <- 0L
    
    repeat {
      
      hasta <- desde + 49L
      
      resultado <- descargar_pagina_lr(
        ruta_ids = ruta_ids,
        rama = rama,
        desde = desde,
        hasta = hasta,
        orden = orden,
        raw_dir = raw_dir,
        ejecucion_id = ejecucion_id
      )
      
      if (
        is.na(total_esperado) &&
        !is.na(resultado$resources)
      ) {
        total_esperado <-
          extraer_total_resources(
            resultado$resources
          )
      }
      
      if (
        resultado$status >= 400 ||
        length(resultado$datos) == 0
      ) {
        break
      }
      
      pagina_df <- purrr::map_dfr(
        resultado$datos,
        extraer_producto_lr,
        ejecucion_id = ejecucion_id,
        rama = rama,
        ruta_ids = ruta_ids,
        orden = orden,
        desde = desde,
        fecha_extraccion =
          fecha_extraccion
      )
      
      registros[[
        length(registros) + 1L
      ]] <- pagina_df
      
      actuales <- dplyr::bind_rows(
        registros
      ) |>
        dplyr::distinct(
          producto_comercio_id,
          .keep_all = TRUE
        )
      
      message(
        "  IDs únicos: ",
        nrow(actuales),
        ifelse(
          is.na(total_esperado),
          "",
          paste0(
            " / ",
            total_esperado
          )
        )
      )
      
      # Si ya recuperamos todo, terminamos rama
      if (
        !is.na(total_esperado) &&
        nrow(actuales) >= total_esperado
      ) {
        return(actuales)
      }
      
      # Página incompleta = fin natural
      if (
        length(resultado$datos) < 50
      ) {
        break
      }
      
      desde <- desde + 50L
      
      # límite práctico VTEX
      if (desde > 2500L) {
        break
      }
    }
  }
  
  resultado_final <- bind_rows(
    registros
  ) |>
    distinct(
      producto_comercio_id,
      .keep_all = TRUE
    )
  
  resultado_final
}

# ------------------------------------------------------------
# Función general La Rebaja
# ------------------------------------------------------------

scraper_larebaja <- function(
    ejecucion_id = crear_id_ejecucion_lr(),
    raiz_raw = "raw"
) {
  
  raw_dir <- file.path(
    raiz_raw,
    "la_rebaja",
    ejecucion_id
  )
  
  dir.create(
    raw_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  message(
    "\nEjecución: ",
    ejecucion_id
  )
  
  # Categoría amplia donde La Rebaja ubica
  # medicamentos con fórmula médica
  formula_medica <- scrapear_rama_lr(
    ruta_ids = "9/627/628",
    rama = "medicamentos_formula_medica",
    raw_dir = raw_dir,
    ejecucion_id = ejecucion_id
  )
  
  # Categoría Piel:
  # contiene, entre otros, Caladryl
  piel <- scrapear_rama_lr(
    ruta_ids = "9/60",
    rama = "piel",
    raw_dir = raw_dir,
    ejecucion_id = ejecucion_id
  )
  
  todos <- bind_rows(
    formula_medica,
    piel
  )
  
  productos_unicos <- todos |>
    distinct(
      producto_comercio_id,
      .keep_all = TRUE
    )
  
  # Guardar salida estructurada
  saveRDS(
    todos,
    file.path(
      raw_dir,
      "larebaja_registros.rds"
    )
  )
  
  readr::write_csv(
    todos,
    file.path(
      raw_dir,
      "larebaja_registros.csv"
    )
  )
  
  message(
    "\n-----------------------------"
  )
  
  message(
    "Fórmula médica: ",
    nrow(formula_medica)
  )
  
  message(
    "Piel: ",
    nrow(piel)
  )
  
  message(
    "Registros totales: ",
    nrow(todos)
  )
  
  message(
    "Productos únicos: ",
    nrow(productos_unicos)
  )
  
  message(
    "RAW: ",
    raw_dir
  )
  
  message(
    "-----------------------------\n"
  )
  
  list(
    ejecucion_id = ejecucion_id,
    raw_dir = raw_dir,
    formula_medica = formula_medica,
    piel = piel,
    datos = todos,
    productos_unicos =
      productos_unicos
  )
}

# ------------------------------------------------------------
# REPROCESAMIENTO OFFLINE
# ------------------------------------------------------------

reprocesar_larebaja_raw <- function(
    raw_dir,
    ejecucion_id = basename(raw_dir)
) {
  
  archivos <- list.files(
    raw_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )
  
  if (length(archivos) == 0) {
    stop(
      "No hay archivos JSON en: ",
      raw_dir
    )
  }
  
  fecha_reproceso <- fecha_colombia()
  
  purrr::map_dfr(
    archivos,
    function(archivo) {
      
      nombre <- basename(archivo)
      
      partes <- str_split(
        str_remove(
          nombre,
          "\\.json$"
        ),
        "__",
        simplify = TRUE
      )
      
      rama <- partes[1]
      orden <- partes[2]
      
      rango <- partes[3]
      
      desde <- suppressWarnings(
        as.integer(
          str_extract(
            rango,
            "^\\d+"
          )
        )
      )
      
      datos <- tryCatch(
        jsonlite::fromJSON(
          archivo,
          simplifyVector = FALSE
        ),
        error = function(e) list()
      )
      
      if (
        !is.list(datos) ||
        length(datos) == 0
      ) {
        return(tibble())
      }
      
      purrr::map_dfr(
        datos,
        extraer_producto_lr,
        ejecucion_id = ejecucion_id,
        rama = rama,
        ruta_ids = NA_character_,
        orden = orden,
        desde = desde,
        fecha_extraccion =
          fecha_reproceso
      )
    }
  ) |>
    distinct(
      producto_comercio_id,
      rama_origen,
      .keep_all = TRUE
    )
}

# ============================================================================
# INTEGRACION FINAL DEL TALLER
# - interfaz común para los tres comercios
# - homologación documentada de los 8 productos
# - clasificación de la categoría académica 32
# - carga normalizada en SQLite
# - ejecución general online/offline
# ============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

# ---------------------------------------------------------------------------
# Configuración de la entrega
# ---------------------------------------------------------------------------

PRODUCTOS_COMUNES_T1 <- tibble::tribble(
  ~producto_comun_id, ~producto_comun, ~principio_activo, ~concentracion, ~forma, ~contenido, ~unidades,
  "P01", "Aciclovir La Santé crema 5% 20 g",       "ACICLOVIR",   "5%", "crema",    "20g",   1L,
  "P02", "Aciclovir Genfar ungüento 5% 15 g",      "ACICLOVIR",   "5%", "unguento", "15g",   1L,
  "P03", "Virex ungüento labial 5% 10 g",          "ACICLOVIR",   "5%", "unguento", "10g",   1L,
  "P04", "Virex ungüento 5% 15 g",                 "ACICLOVIR",   "5%", "unguento", "15g",   1L,
  "P05", "Virex ungüento 5% 30 g",                 "ACICLOVIR",   "5%", "unguento", "30g",   1L,
  "P06", "Clotrimazol AG/Lafrancol crema 1% 40 g", "CLOTRIMAZOL", "1%", "crema",    "40g",   1L,
  "P07", "Clotrimazol La Santé crema 1% 40 g",     "CLOTRIMAZOL", "1%", "crema",    "40g",   1L,
  "P08", "Caladryl loción 8% 100 ml",              "CALAMINA",    "8%", "locion",    "100ml", 1L
)

MAPA_COMPARABLES_T1 <- tibble::tribble(
  ~comercio,            ~producto_comercio_id, ~producto_comun_id,
  "Cruz Verde",         "140228",              "P01",
  "Droguerías Cafam",   "1257745",             "P01",
  "La Rebaja",          "1394",                "P01",
  "Cruz Verde",         "17747",               "P02",
  "Droguerías Cafam",   "275915",              "P02",
  "La Rebaja",          "3383",                "P02",
  "Cruz Verde",         "549015",              "P03",
  "Droguerías Cafam",   "74824",               "P03",
  "La Rebaja",          "984",                 "P03",
  "Cruz Verde",         "18714",               "P04",
  "Droguerías Cafam",   "8588",                "P04",
  "La Rebaja",          "986",                 "P04",
  "Cruz Verde",         "18716",               "P05",
  "Droguerías Cafam",   "8589",                "P05",
  "La Rebaja",          "1051",                "P05",
  "Cruz Verde",         "50184",               "P06",
  "Droguerías Cafam",   "822632",              "P06",
  "La Rebaja",          "4603",                "P06",
  "Cruz Verde",         "97936",               "P07",
  "Droguerías Cafam",   "613066",              "P07",
  "La Rebaja",          "4459",                "P07",
  "Cruz Verde",         "21247",               "P08",
  "Droguerías Cafam",   "650569",              "P08",
  "La Rebaja",          "5484",                "P08"
)

EJECUCIONES_OFFLINE_T1 <- c(
  cafam = "cafam_20261010_092736",
  cruz_verde = "cruzverde_20261010_123437",
  la_rebaja = "larebaja_20261010_214127"
)

# ---------------------------------------------------------------------------
# Utilidades comunes
# ---------------------------------------------------------------------------

iso_colombia_final <- function(x = Sys.time()) {
  out <- format(
    x,
    tz = "America/Bogota",
    format = "%Y-%m-%dT%H:%M:%S%z"
  )
  stringr::str_replace(
    out,
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2"
  )
}

normalizar_iso_final <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(stringr::str_trim(x))] <- NA_character_

  # Corrige formatos como:
  # 2026-10-10 14:27:56 -> 2026-10-10T14:27:56-05:00
  sin_offset <- !is.na(x) &
    stringr::str_detect(
      x,
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}$"
    )

  x[sin_offset] <- x[sin_offset] |>
    stringr::str_replace(
      "^([0-9]{4}-[0-9]{2}-[0-9]{2}) ",
      "\\1T"
    ) |>
    paste0("-05:00")

  # Corrige -0500 -> -05:00
  x <- stringr::str_replace(
    x,
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2"
  )

  x
}

fecha_desde_id_ejecucion <- function(id) {
  id <- as.character(id)
  m <- stringr::str_match(
    id,
    "([0-9]{8})_([0-9]{6})"
  )

  if (nrow(m) == 0L || is.na(m[1, 2]) || is.na(m[1, 3])) {
    return(NA_character_)
  }

  f <- m[1, 2]
  h <- m[1, 3]

  sprintf(
    "%s-%s-%sT%s:%s:%s-05:00",
    substr(f, 1, 4),
    substr(f, 5, 6),
    substr(f, 7, 8),
    substr(h, 1, 2),
    substr(h, 3, 4),
    substr(h, 5, 6)
  )
}

crear_id_ejecucion_general <- function(prefijo = "recoleccion") {
  paste0(
    prefijo,
    "_",
    format(
      Sys.time(),
      tz = "America/Bogota",
      format = "%Y%m%d_%H%M%S"
    )
  )
}

crear_id_offline_determinista <- function(ids) {
  limpio <- paste(
    stringr::str_extract(as.character(ids), "[0-9]{8}_[0-9]{6}"),
    collapse = "_"
  )

  paste0(
    "comparacion_offline_",
    stringr::str_replace_all(limpio, "[^0-9_]", "")
  )
}

asegurar_columnas_final <- function(df, especificacion) {
  for (nm in names(especificacion)) {
    if (!nm %in% names(df)) {
      df[[nm]] <- especificacion[[nm]]
    }
  }
  df
}

texto_ascii_final <- function(x) {
  x <- dplyr::coalesce(as.character(x), "")
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_squish()
}

normalizar_unidad_final <- function(x) {
  y <- texto_ascii_final(x)
  dplyr::case_when(
    stringr::str_detect(y, "^kg$|kilogram") ~ "g",
    stringr::str_detect(y, "^mg$|miligram") ~ "mg",
    stringr::str_detect(y, "^g$|^gr$|gram") ~ "g",
    stringr::str_detect(y, "^l$|litro") ~ "ml",
    stringr::str_detect(y, "^ml$|mililit|^cc$") ~ "ml",
    stringr::str_detect(y, "unidad|tableta|capsula|sobre|ampolla") ~ "unidad",
    TRUE ~ NA_character_
  )
}

convertir_contenido_final <- function(valor, unidad) {
  valor <- suppressWarnings(as.numeric(
    stringr::str_replace(as.character(valor), ",", ".")
  ))
  u0 <- texto_ascii_final(unidad)
  u <- normalizar_unidad_final(unidad)

  valor2 <- dplyr::case_when(
    !is.na(valor) & stringr::str_detect(u0, "^kg$|kilogram") ~ valor * 1000,
    !is.na(valor) & stringr::str_detect(u0, "^l$|^litro") ~ valor * 1000,
    TRUE ~ valor
  )

  list(valor = valor2, unidad = u)
}

extraer_contenido_texto_final <- function(texto) {
  x <- texto_ascii_final(texto)
  m <- stringr::str_match(
    x,
    "([0-9]+(?:[\\.,][0-9]+)?)\\s*(kg|mg|ml|cc|gr|g|litros?|l|gramos?|mililitros?)\\b"
  )

  if (is.na(m[1, 2])) {
    return(list(valor = NA_real_, unidad = NA_character_))
  }

  convertir_contenido_final(m[1, 2], m[1, 3])
}

extraer_forma_final <- function(...) {
  x <- texto_ascii_final(paste(..., collapse = " "))
  dplyr::case_when(
    stringr::str_detect(x, "\\bcrema\\b") ~ "crema",
    stringr::str_detect(x, "\\bunguento\\b") ~ "unguento",
    stringr::str_detect(x, "\\blocion\\b") ~ "locion",
    stringr::str_detect(x, "\\bgel\\b") ~ "gel",
    stringr::str_detect(x, "\\bsolucion\\b") ~ "solucion",
    stringr::str_detect(x, "\\bspray\\b") ~ "spray",
    stringr::str_detect(x, "\\bshampoo\\b|\\bchampu\\b") ~ "shampoo",
    stringr::str_detect(x, "\\blaca\\b") ~ "laca",
    stringr::str_detect(x, "\\bpolvo\\b") ~ "polvo",
    TRUE ~ NA_character_
  )
}

normalizar_invima_final <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""
  out <- y |>
    stringr::str_to_upper() |>
    stringr::str_remove("^INVIMA\\s*") |>
    stringr::str_replace_all("[^A-Z0-9]", "")
  out[out == ""] <- NA_character_
  out
}

normalizar_marca_final <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- NA_character_
  stringr::str_squish(y)
}

# ---------------------------------------------------------------------------
# Clasificación reproducible de la categoría académica 32
# ---------------------------------------------------------------------------

clasificar_categoria_32_final <- function(datos) {
  texto <- paste(
    dplyr::coalesce(datos$producto_original, ""),
    dplyr::coalesce(datos$principio_activo_original, ""),
    dplyr::coalesce(datos$subcategoria_original, ""),
    dplyr::coalesce(datos$categoria_origen, ""),
    dplyr::coalesce(datos$descripcion_original, "")
  ) |>
    texto_ascii_final()

  exclusion <- stringr::str_detect(
    texto,
    paste0(
      "caladryl clear|pramoxina|lidocaina|diclofenaco|ibuprofeno|capsaicina|",
      "sulfaplata|sulfadiazina de plata|fitostimoline|kelo[ -]?cote|",
      "procicar|cicatrimax|cicatriz|herida|quemadura|",
      "ovulo|vaginal|intravaginal|",
      "repelente|protector solar|bloqueador solar|maquillaje"
    )
  )

  activo_dermatologico <- stringr::str_detect(
    texto,
    paste0(
      "aciclovir|clotrimazol|ketoconazol|terbinafina|amorolfina|ciclopirox|",
      "miconazol|econazol|isoconazol|nistatina|fluconazol topico|",
      "adapaleno|tretinoina|isotretinoina|peroxido de benzoilo|",
      "acido azelaico|clindamicina|",
      "hidrocortisona|betametasona|clobetasol|desonida|mometasona|",
      "triamcinolona|tacrolimus|pimecrolimus|",
      "imiquimod|podofilotoxina|podofilina|acido salicilico|acido lactico|",
      "minoxidil|calamina|ivermectina|metronidazol|",
      "mupirocina|acido fusidico|permethrina|permetrina"
    )
  )

  termino_dermatologico <- stringr::str_detect(
    texto,
    paste0(
      "antimicot|antifung|dermat|cutane|topica|topico|",
      "acne|verruga|rosacea|alopecia|caspa|onicomicos|",
      "caladryl|virex|cutamycon"
    )
  )

  comercio_nativo <- datos$comercio %in% c(
    "Droguerías Cafam",
    "Cruz Verde"
  )

  incluido <- dplyr::case_when(
    exclusion ~ 0L,
    comercio_nativo ~ 1L,
    datos$comercio == "La Rebaja" & (activo_dermatologico | termino_dermatologico) ~ 1L,
    TRUE ~ 0L
  )

  regla <- dplyr::case_when(
    exclusion ~ "exclusion_explicita_del_alcance",
    comercio_nativo ~ "categoria_nativa_dermatologicos",
    datos$comercio == "La Rebaja" & activo_dermatologico ~ "regla_principio_activo_dermatologico",
    datos$comercio == "La Rebaja" & termino_dermatologico ~ "regla_texto_uso_dermatologico",
    TRUE ~ "fuera_del_alcance_academico"
  )

  datos |>
    dplyr::mutate(
      incluido_categoria = incluido,
      regla_categoria = regla
    )
}

# ---------------------------------------------------------------------------
# Homologación explícita de los 8 productos comparables
# ---------------------------------------------------------------------------

aplicar_comparables_final <- function(datos) {
  datos |>
    dplyr::mutate(
      producto_comercio_id = as.character(producto_comercio_id)
    ) |>
    dplyr::left_join(
      MAPA_COMPARABLES_T1,
      by = c("comercio", "producto_comercio_id")
    ) |>
    dplyr::mutate(
      comparable = as.integer(!is.na(producto_comun_id))
    )
}

# ---------------------------------------------------------------------------
# Estandarización de salida: los tres scrapers devuelven el mismo esquema
# ---------------------------------------------------------------------------

columnas_salida_final <- c(
  "ejecucion_id", "ejecucion_fuente", "comercio",
  "categoria_origen", "url_categoria",
  "producto_comercio_id", "sku", "ean",
  "producto_original", "marca_original", "marca_estandar",
  "laboratorio_original", "subcategoria_original", "subcategoria_estandar",
  "presentacion_original", "principio_activo_original",
  "concentracion_original", "forma_farmaceutica_original", "forma_estandar",
  "via_administracion_original", "contenido_original",
  "contenido_valor", "unidad_original", "contenido_valor_estandar",
  "unidad_estandar", "contenido_estandar", "unidades_empaque_original",
  "invima", "invima_norm",
  "precio_cop", "precio_fuente", "precio_lista_cop",
  "precio_antes_cop", "promocion", "pum_cop", "pum_unidad_original",
  "url", "descripcion_original", "ficha_tecnica_original", "texto_original",
  "fecha_hora_consulta", "comparable", "producto_comun_id",
  "incluido_categoria", "regla_categoria", "estado_extraccion"
)

completar_estandarizacion_final <- function(datos, ejecucion_global, ejecucion_fuente) {
  datos <- asegurar_columnas_final(
    datos,
    list(
      categoria_origen = NA_character_,
      url_categoria = NA_character_,
      sku = NA_character_,
      ean = NA_character_,
      marca_original = NA_character_,
      laboratorio_original = NA_character_,
      subcategoria_original = NA_character_,
      presentacion_original = NA_character_,
      principio_activo_original = NA_character_,
      concentracion_original = NA_character_,
      forma_farmaceutica_original = NA_character_,
      via_administracion_original = NA_character_,
      contenido_original = NA_character_,
      contenido_valor = NA_real_,
      unidad_original = NA_character_,
      unidades_empaque_original = NA_character_,
      invima = NA_character_,
      precio_cop = NA_real_,
      precio_fuente = NA_character_,
      precio_lista_cop = NA_real_,
      precio_antes_cop = NA_real_,
      promocion = NA_integer_,
      pum_cop = NA_real_,
      pum_unidad_original = NA_character_,
      url = NA_character_,
      descripcion_original = NA_character_,
      ficha_tecnica_original = NA_character_,
      texto_original = NA_character_,
      fecha_hora_consulta = NA_character_,
      estado_extraccion = NA_character_
    )
  )

  fallback_fecha <- fecha_desde_id_ejecucion(ejecucion_fuente)

  contenido_lista <- purrr::map2(
    datos$contenido_valor,
    datos$unidad_original,
    convertir_contenido_final
  )

  valor_est <- purrr::map_dbl(
    contenido_lista,
    ~ .x$valor %||% NA_real_
  )

  unidad_est <- purrr::map_chr(
    contenido_lista,
    ~ .x$unidad %||% NA_character_
  )

  faltan_contenido <- is.na(valor_est) | is.na(unidad_est)

  if (any(faltan_contenido)) {
    respaldo <- purrr::map(
      paste(
        dplyr::coalesce(datos$presentacion_original, ""),
        dplyr::coalesce(datos$contenido_original, ""),
        dplyr::coalesce(datos$producto_original, "")
      ),
      extraer_contenido_texto_final
    )

    valor_respaldo <- purrr::map_dbl(respaldo, ~ .x$valor %||% NA_real_)
    unidad_respaldo <- purrr::map_chr(respaldo, ~ .x$unidad %||% NA_character_)

    valor_est[faltan_contenido] <- valor_respaldo[faltan_contenido]
    unidad_est[faltan_contenido] <- unidad_respaldo[faltan_contenido]
  }

  fecha <- normalizar_iso_final(datos$fecha_hora_consulta)
  fecha[is.na(fecha)] <- fallback_fecha

  datos <- datos |>
    dplyr::mutate(
      ejecucion_id = ejecucion_global,
      ejecucion_fuente = ejecucion_fuente,
      producto_comercio_id = as.character(producto_comercio_id),
      marca_estandar = normalizar_marca_final(marca_original),
      subcategoria_estandar = dplyr::coalesce(
        dplyr::na_if(
          stringr::str_squish(as.character(subcategoria_original)),
          ""
        ),
        dplyr::na_if(
          stringr::str_squish(as.character(categoria_origen)),
          ""
        )
      ),
      forma_estandar = purrr::pmap_chr(
        list(
          forma_farmaceutica_original,
          presentacion_original,
          producto_original
        ),
        extraer_forma_final
      ),
      contenido_valor_estandar = valor_est,
      unidad_estandar = unidad_est,
      contenido_estandar = dplyr::if_else(
        !is.na(contenido_valor_estandar) & !is.na(unidad_estandar),
        paste0(
          format(
            contenido_valor_estandar,
            scientific = FALSE,
            trim = TRUE
          ),
          unidad_estandar
        ),
        NA_character_
      ),
      invima_norm = normalizar_invima_final(invima),
      promocion = dplyr::case_when(
        is.logical(promocion) ~ as.integer(promocion),
        TRUE ~ suppressWarnings(as.integer(promocion))
      ),
      fecha_hora_consulta = fecha
    )

  datos <- clasificar_categoria_32_final(datos)
  datos <- aplicar_comparables_final(datos)

  datos |>
    dplyr::select(dplyr::all_of(columnas_salida_final))
}

normalizar_resultado_cafam <- function(resultado, ejecucion_global, ejecucion_fuente) {
  d <- resultado$datos
  d <- asegurar_columnas_final(
    d,
    list(
      concentracion_original = NA_character_,
      forma_farmaceutica_original = NA_character_,
      pum_unidad_original = NA_character_
    )
  )

  completar_estandarizacion_final(
    d,
    ejecucion_global = ejecucion_global,
    ejecucion_fuente = ejecucion_fuente
  )
}

normalizar_resultado_cv <- function(resultado, ejecucion_global, ejecucion_fuente) {
  d <- resultado$datos |>
    dplyr::mutate(
      # Evita falsos positivos por la palabra "Oferta" presente en elementos
      # generales de la página. Se marca promoción únicamente cuando existe
      # evidencia numérica de un precio anterior mayor al precio observado.
      promocion = dplyr::case_when(
        !is.na(precio_antes_cop) &
          !is.na(precio_cop) &
          precio_antes_cop > precio_cop ~ 1L,
        TRUE ~ 0L
      )
    )

  completar_estandarizacion_final(
    d,
    ejecucion_global = ejecucion_global,
    ejecucion_fuente = ejecucion_fuente
  )
}

normalizar_resultado_lr <- function(resultado, ejecucion_global, ejecucion_fuente) {
  d <- resultado$datos

  d <- d |>
    dplyr::mutate(
      categoria_origen = dplyr::case_when(
        stringr::str_detect(
          dplyr::coalesce(categorias_ids_originales, ""),
          "/9/60/|/9/60$"
        ) ~ "Piel",
        stringr::str_detect(
          dplyr::coalesce(categorias_ids_originales, ""),
          "/9/627/628/|/9/627/628$"
        ) ~ "Medicamentos con fórmula médica",
        TRUE ~ rama_origen
      ),
      url_categoria = dplyr::case_when(
        categoria_origen == "Piel" ~
          "https://www.larebajavirtual.com/9/60?map=c,c",
        TRUE ~
          "https://www.larebajavirtual.com/9/627/628?map=c,c,c"
      ),
      sku = sku_id,
      laboratorio_original = dplyr::coalesce(
        laboratorio_original,
        fabricante_original
      ),
      subcategoria_original = categorias_originales,
      forma_farmaceutica_original = NA_character_,
      via_administracion_original = NA_character_,
      contenido_valor = suppressWarnings(
        as.numeric(
          stringr::str_replace(
            cantidad_unidad_medida,
            ",",
            "."
          )
        )
      ),
      unidad_original = unidad_medida_original,
      unidades_empaque_original = NA_character_,
      invima = registro_sanitario_original,
      precio_fuente = "vtex_search_api",
      precio_lista_cop = precio_antes_cop,
      promocion = as.integer(tiene_promocion),
      pum_cop = NA_real_,
      pum_unidad_original = NA_character_,
      descripcion_original = NA_character_,
      ficha_tecnica_original = stringr::str_squish(
        paste(
          dplyr::coalesce(principio_activo_original, ""),
          dplyr::coalesce(presentacion_original, ""),
          dplyr::coalesce(registro_sanitario_original, ""),
          dplyr::coalesce(presentacion_unidad_medida, ""),
          dplyr::coalesce(laboratorio_original, "")
        )
      ),
      texto_original = stringr::str_squish(
        paste(
          dplyr::coalesce(producto_original, ""),
          dplyr::coalesce(presentacion_original, ""),
          dplyr::coalesce(principio_activo_original, ""),
          dplyr::coalesce(registro_sanitario_original, ""),
          dplyr::coalesce(presentacion_unidad_medida, ""),
          dplyr::coalesce(categorias_originales, "")
        )
      ),
      # En modo offline `fecha_extraccion` puede corresponder al momento
      # del reprocesamiento. Para no confundirlo con la captura original se
      # conserva como fecha de consulta el instante de la ejecución fuente.
      fecha_hora_consulta = fecha_desde_id_ejecucion(
        ejecucion_fuente
      ),
      estado_extraccion = dplyr::if_else(
        is.na(precio_cop),
        "ok_sin_precio",
        "ok"
      )
    )

  completar_estandarizacion_final(
    d,
    ejecucion_global = ejecucion_global,
    ejecucion_fuente = ejecucion_fuente
  )
}

# ---------------------------------------------------------------------------
# La Rebaja: verificación de cobertura y recuperación por marca
# ---------------------------------------------------------------------------

lr_total_categoria_online <- function(ruta_ids, raw_dir) {
  url <- paste0(
    "https://www.larebajavirtual.com/api/catalog_system/pub/products/search",
    "?fq=C:/", ruta_ids, "/&_from=0&_to=0&O=OrderByNameASC"
  )

  resp <- httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  writeBin(
    httr2::resp_body_raw(resp),
    file.path(
      raw_dir,
      paste0(
        "total_probe__",
        stringr::str_replace_all(ruta_ids, "/", "_"),
        ".json"
      )
    )
  )

  resource <- httr2::resp_header(resp, "resources")

  if (is.null(resource) || is.na(resource)) {
    return(NA_integer_)
  }

  suppressWarnings(as.integer(sub(".*/", "", resource)))
}

lr_pagina_marca <- function(
    brand_id,
    desde,
    ruta_ids,
    raw_dir,
    ejecucion_id,
    fecha_hora) {

  hasta <- desde + 49L

  url <- paste0(
    "https://www.larebajavirtual.com/api/catalog_system/pub/products/search",
    "?fq=C:/", ruta_ids, "/",
    "&fq=B:", utils::URLencode(as.character(brand_id), reserved = TRUE),
    "&_from=", desde,
    "&_to=", hasta,
    "&O=OrderByNameASC"
  )

  resp <- httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)

  archivo <- file.path(
    raw_dir,
    sprintf(
      "medicamentos_formula_medica__brand_%s__%04d_%04d.json",
      brand_id,
      desde,
      hasta
    )
  )

  writeBin(httr2::resp_body_raw(resp), archivo)

  if (status >= 400L) {
    return(list(datos = tibble::tibble(), status = status, n = 0L))
  }

  objetos <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (length(objetos) == 0L) {
    return(list(datos = tibble::tibble(), status = status, n = 0L))
  }

  datos <- purrr::map_dfr(
    objetos,
    extraer_producto_lr,
    ejecucion_id = ejecucion_id,
    rama = "medicamentos_formula_medica",
    ruta_ids = ruta_ids,
    orden = paste0("brand_", brand_id),
    desde = desde,
    fecha_extraccion = fecha_hora
  )

  list(datos = datos, status = status, n = length(objetos))
}

lr_completar_por_marcas <- function(
    base,
    total_esperado,
    raw_dir,
    ejecucion_id,
    ruta_ids = "9/627/628") {

  actual <- base |>
    dplyr::filter(
      !is.na(producto_comercio_id),
      producto_comercio_id != ""
    ) |>
    dplyr::distinct(producto_comercio_id, .keep_all = TRUE)

  if (is.na(total_esperado) || nrow(actual) >= total_esperado) {
    return(actual)
  }

  brands <- actual |>
    dplyr::filter(!is.na(brand_id), brand_id != "") |>
    dplyr::distinct(brand_id) |>
    dplyr::pull(brand_id)

  message(
    "La Rebaja: cobertura directa ",
    nrow(actual),
    "/",
    total_esperado,
    ". Se activa recuperación por marca."
  )

  for (brand in brands) {
    desde <- 0L

    repeat {
      fecha_i <- iso_colombia_final()

      res <- lr_pagina_marca(
        brand_id = brand,
        desde = desde,
        ruta_ids = ruta_ids,
        raw_dir = raw_dir,
        ejecucion_id = ejecucion_id,
        fecha_hora = fecha_i
      )

      if (res$status >= 400L || res$n == 0L) {
        break
      }

      actual <- dplyr::bind_rows(actual, res$datos) |>
        dplyr::distinct(producto_comercio_id, .keep_all = TRUE)

      if (nrow(actual) >= total_esperado) {
        message("La Rebaja: cobertura completa = ", nrow(actual))
        return(actual)
      }

      if (res$n < 50L) {
        break
      }

      desde <- desde + 50L
      if (desde > 2500L) break
    }
  }

  actual
}

scrapear_la_rebaja <- function(
    modo = c("online", "offline"),
    raw_root = "raw",
    ejecucion_id = NULL) {

  modo <- match.arg(modo)

  if (is.null(ejecucion_id)) {
    if (modo == "offline") {
      stop("En modo offline debe indicar ejecucion_id para La Rebaja.")
    }
    ejecucion_id <- crear_id_ejecucion_lr()
  }

  raw_dir <- file.path(raw_root, "la_rebaja", ejecucion_id)

  if (modo == "online") {
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    inicio <- iso_colombia_final()

    total_formula <- lr_total_categoria_online(
      "9/627/628",
      raw_dir
    )

    resultado_base <- scraper_larebaja(
      ejecucion_id = ejecucion_id,
      raiz_raw = raw_root
    )

    formula <- resultado_base$formula_medica |>
      dplyr::distinct(producto_comercio_id, .keep_all = TRUE)

    formula <- lr_completar_por_marcas(
      base = formula,
      total_esperado = total_formula,
      raw_dir = raw_dir,
      ejecucion_id = ejecucion_id
    )

    piel <- resultado_base$piel |>
      dplyr::distinct(producto_comercio_id, .keep_all = TRUE)

    datos <- dplyr::bind_rows(formula, piel) |>
      dplyr::filter(
        !is.na(producto_comercio_id),
        producto_comercio_id != ""
      ) |>
      dplyr::distinct(producto_comercio_id, .keep_all = TRUE)

    fin <- iso_colombia_final()

    estado <- dplyr::case_when(
      !is.na(total_formula) && nrow(formula) < total_formula ~ "parcial",
      TRUE ~ "completa"
    )

    ejecucion <- tibble::tibble(
      ejecucion_id = ejecucion_id,
      comercio = "La Rebaja",
      fecha_hora_inicio = inicio,
      fecha_hora_fin = fin,
      modo = modo,
      productos_reportados_sitio = total_formula,
      registros_formula = nrow(formula),
      registros_piel = nrow(piel),
      registros_obtenidos = nrow(datos),
      precios_encontrados = sum(!is.na(datos$precio_cop)),
      precios_faltantes = sum(is.na(datos$precio_cop)),
      estado = estado
    )

    readr::write_csv(
      datos,
      file.path(raw_dir, "larebaja_extraido_final.csv")
    )

    readr::write_csv(
      ejecucion,
      file.path(raw_dir, "resumen_ejecucion.csv")
    )

    saveRDS(
      datos,
      file.path(raw_dir, "larebaja_extraido_final.rds")
    )
  } else {
    if (!dir.exists(raw_dir)) {
      stop("No existe la carpeta raw de La Rebaja: ", raw_dir)
    }

    inicio <- fecha_desde_id_ejecucion(ejecucion_id)

    rds_final <- file.path(raw_dir, "larebaja_extraido_final.rds")
    csv_final <- file.path(raw_dir, "larebaja_extraido_final.csv")

    if (file.exists(rds_final)) {
      datos <- readRDS(rds_final)
    } else if (file.exists(csv_final)) {
      datos <- readr::read_csv(csv_final, show_col_types = FALSE)
    } else {
      datos <- reprocesar_larebaja_raw(
        raw_dir = raw_dir,
        ejecucion_id = ejecucion_id
      ) |>
        dplyr::filter(
          !is.na(producto_comercio_id),
          producto_comercio_id != ""
        ) |>
        dplyr::distinct(producto_comercio_id, .keep_all = TRUE)
    }

    resumen_path <- file.path(raw_dir, "resumen_ejecucion.csv")

    if (file.exists(resumen_path)) {
      ejecucion <- readr::read_csv(
        resumen_path,
        show_col_types = FALSE
      )
    } else {
      ejecucion <- tibble::tibble(
        ejecucion_id = ejecucion_id,
        comercio = "La Rebaja",
        fecha_hora_inicio = inicio,
        fecha_hora_fin = NA_character_,
        modo = "offline",
        productos_reportados_sitio = NA_integer_,
        registros_formula = sum(
          stringr::str_detect(
            dplyr::coalesce(datos$categorias_ids_originales, ""),
            "/9/627/628/"
          )
        ),
        registros_piel = sum(
          stringr::str_detect(
            dplyr::coalesce(datos$categorias_ids_originales, ""),
            "/9/60/"
          )
        ),
        registros_obtenidos = nrow(datos),
        precios_encontrados = sum(!is.na(datos$precio_cop)),
        precios_faltantes = sum(is.na(datos$precio_cop)),
        estado = "reprocesada_offline"
      )
    }
  }

  list(
    ejecucion_id = ejecucion_id,
    raw_dir = raw_dir,
    datos = datos,
    ejecucion = ejecucion
  )
}

# ---------------------------------------------------------------------------
# SQLite
# ---------------------------------------------------------------------------

inicializar_base_sqlite <- function(
    db_path = "comparador_precios.sqlite",
    reiniciar = FALSE,
    crear_backup = TRUE) {

  if (reiniciar && file.exists(db_path)) {
    if (crear_backup) {
      backup <- paste0(
        db_path,
        ".bak_",
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
      file.copy(db_path, backup, overwrite = TRUE)
      message("Backup SQLite: ", backup)
    }
    unlink(db_path)
  }

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    db_path
  )

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS comercios (
      comercio_id INTEGER PRIMARY KEY,
      nombre TEXT NOT NULL UNIQUE
    );
    "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS productos_comunes (
      producto_comun_id TEXT PRIMARY KEY,
      producto_comun TEXT NOT NULL,
      principio_activo TEXT,
      concentracion TEXT,
      forma TEXT,
      contenido TEXT,
      unidades INTEGER
    );
    "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS ejecuciones (
      ejecucion_id TEXT NOT NULL,
      comercio_id INTEGER NOT NULL,
      ejecucion_fuente TEXT NOT NULL,
      fecha_hora_inicio TEXT,
      fecha_hora_fin TEXT,
      modo TEXT NOT NULL,
      productos_reportados_sitio INTEGER,
      registros_obtenidos INTEGER NOT NULL,
      productos_categoria INTEGER,
      comparables_con_precio INTEGER,
      precios_encontrados INTEGER,
      precios_faltantes INTEGER,
      estado TEXT NOT NULL,
      raw_dir TEXT,
      PRIMARY KEY (ejecucion_id, comercio_id),
      FOREIGN KEY (comercio_id) REFERENCES comercios(comercio_id)
    );
    "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS anuncios (
      ejecucion_id TEXT NOT NULL,
      comercio_id INTEGER NOT NULL,
      ejecucion_fuente TEXT NOT NULL,
      producto_comercio_id TEXT NOT NULL,
      producto_comun_id TEXT,
      comparable INTEGER NOT NULL DEFAULT 0,
      incluido_categoria INTEGER NOT NULL DEFAULT 0,
      regla_categoria TEXT,
      categoria_origen TEXT,
      url_categoria TEXT,
      sku TEXT,
      ean TEXT,
      producto_original TEXT,
      marca_original TEXT,
      marca_estandar TEXT,
      laboratorio_original TEXT,
      subcategoria_original TEXT,
      subcategoria_estandar TEXT,
      presentacion_original TEXT,
      principio_activo_original TEXT,
      concentracion_original TEXT,
      forma_farmaceutica_original TEXT,
      forma_estandar TEXT,
      via_administracion_original TEXT,
      contenido_original TEXT,
      contenido_valor REAL,
      unidad_original TEXT,
      contenido_valor_estandar REAL,
      unidad_estandar TEXT,
      contenido_estandar TEXT,
      unidades_empaque_original TEXT,
      invima TEXT,
      invima_norm TEXT,
      url TEXT,
      descripcion_original TEXT,
      ficha_tecnica_original TEXT,
      texto_original TEXT,
      estado_extraccion TEXT,
      PRIMARY KEY (ejecucion_id, comercio_id, producto_comercio_id),
      FOREIGN KEY (ejecucion_id, comercio_id)
        REFERENCES ejecuciones(ejecucion_id, comercio_id),
      FOREIGN KEY (producto_comun_id)
        REFERENCES productos_comunes(producto_comun_id)
    );
    "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS observaciones (
      observacion_id INTEGER PRIMARY KEY AUTOINCREMENT,
      ejecucion_id TEXT NOT NULL,
      comercio_id INTEGER NOT NULL,
      ejecucion_fuente TEXT NOT NULL,
      producto_comercio_id TEXT NOT NULL,
      fecha_hora_consulta TEXT NOT NULL,
      precio_cop REAL,
      precio_fuente TEXT,
      precio_lista_cop REAL,
      precio_antes_cop REAL,
      promocion INTEGER,
      pum_cop REAL,
      pum_unidad_original TEXT,
      UNIQUE (comercio_id, producto_comercio_id, fecha_hora_consulta),
      FOREIGN KEY (ejecucion_id, comercio_id, producto_comercio_id)
        REFERENCES anuncios(ejecucion_id, comercio_id, producto_comercio_id)
    );
    "
  )

  comercios <- tibble::tibble(
    nombre = c(
      "Droguerías Cafam",
      "Cruz Verde",
      "La Rebaja"
    )
  )

  DBI::dbWriteTable(
    con,
    "tmp_comercios_t1",
    comercios,
    temporary = TRUE,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO comercios(nombre)
    SELECT nombre FROM tmp_comercios_t1;
    "
  )

  DBI::dbWriteTable(
    con,
    "tmp_productos_comunes_t1",
    PRODUCTOS_COMUNES_T1,
    temporary = TRUE,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO productos_comunes(
      producto_comun_id, producto_comun, principio_activo,
      concentracion, forma, contenido, unidades
    )
    SELECT
      producto_comun_id, producto_comun, principio_activo,
      concentracion, forma, contenido, unidades
    FROM tmp_productos_comunes_t1;
    "
  )

  con
}

obtener_id_comercio <- function(con, comercio) {
  x <- DBI::dbGetQuery(
    con,
    "SELECT comercio_id FROM comercios WHERE nombre = ?;",
    params = list(comercio)
  )

  if (nrow(x) != 1L) {
    stop("No se encontró comercio en SQLite: ", comercio)
  }

  x$comercio_id[[1]]
}

construir_resumen_general <- function(
    datos,
    resultado,
    comercio,
    ejecucion_global,
    ejecucion_fuente,
    modo,
    raw_dir) {

  resumen_fuente <- resultado$ejecucion

  get1 <- function(nombre, default = NA) {
    if (!is.null(resumen_fuente) && nombre %in% names(resumen_fuente)) {
      return(resumen_fuente[[nombre]][[1]])
    }
    default
  }

  fecha_inicio <- normalizar_iso_final(
    as.character(get1("fecha_hora_inicio", NA_character_))
  )

  if (is.na(fecha_inicio)) {
    fecha_inicio <- fecha_desde_id_ejecucion(ejecucion_fuente)
  }

  fecha_fin <- normalizar_iso_final(
    as.character(get1("fecha_hora_fin", NA_character_))
  )

  # Corridas antiguas de La Rebaja no guardaron fecha de fin.
  # Para una reconstrucción offline usamos al menos el instante
  # documentado de la ejecución fuente en lugar de dejar el campo vacío.
  if (is.na(fecha_fin)) {
    fecha_fin <- fecha_inicio
  }

  precios_encontrados <- sum(!is.na(datos$precio_cop))
  precios_faltantes <- sum(is.na(datos$precio_cop))

  tibble::tibble(
    ejecucion_id = ejecucion_global,
    comercio = comercio,
    ejecucion_fuente = ejecucion_fuente,
    fecha_hora_inicio = fecha_inicio,
    fecha_hora_fin = fecha_fin,
    modo = modo,
    productos_reportados_sitio = suppressWarnings(as.integer(
      get1("productos_reportados_sitio", NA_integer_)
    )),
    registros_obtenidos = nrow(datos),
    productos_categoria = sum(datos$incluido_categoria == 1L, na.rm = TRUE),
    comparables_con_precio = sum(
      datos$comparable == 1L & !is.na(datos$precio_cop),
      na.rm = TRUE
    ),
    precios_encontrados = precios_encontrados,
    precios_faltantes = precios_faltantes,
    estado = as.character(get1("estado", "reprocesada")),
    raw_dir = raw_dir
  )
}

insertar_resultado_sqlite <- function(con, datos, resumen) {
  comercio <- unique(datos$comercio)
  if (length(comercio) != 1L) {
    stop("insertar_resultado_sqlite espera un solo comercio.")
  }

  comercio_id <- obtener_id_comercio(con, comercio)

  resumen_sql <- resumen |>
    dplyr::mutate(comercio_id = comercio_id) |>
    dplyr::select(
      ejecucion_id,
      comercio_id,
      ejecucion_fuente,
      fecha_hora_inicio,
      fecha_hora_fin,
      modo,
      productos_reportados_sitio,
      registros_obtenidos,
      productos_categoria,
      comparables_con_precio,
      precios_encontrados,
      precios_faltantes,
      estado,
      raw_dir
    )

  DBI::dbWriteTable(
    con,
    "tmp_ejecucion_t1",
    resumen_sql,
    temporary = TRUE,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO ejecuciones(
      ejecucion_id, comercio_id, ejecucion_fuente,
      fecha_hora_inicio, fecha_hora_fin, modo,
      productos_reportados_sitio, registros_obtenidos,
      productos_categoria, comparables_con_precio,
      precios_encontrados, precios_faltantes, estado, raw_dir
    )
    SELECT
      ejecucion_id, comercio_id, ejecucion_fuente,
      fecha_hora_inicio, fecha_hora_fin, modo,
      productos_reportados_sitio, registros_obtenidos,
      productos_categoria, comparables_con_precio,
      precios_encontrados, precios_faltantes, estado, raw_dir
    FROM tmp_ejecucion_t1;
    "
  )

  anuncios <- datos |>
    dplyr::filter(
      !is.na(producto_comercio_id),
      producto_comercio_id != ""
    ) |>
    dplyr::transmute(
      ejecucion_id,
      comercio_id = comercio_id,
      ejecucion_fuente,
      producto_comercio_id,
      producto_comun_id,
      comparable,
      incluido_categoria,
      regla_categoria,
      categoria_origen,
      url_categoria,
      sku,
      ean,
      producto_original,
      marca_original,
      marca_estandar,
      laboratorio_original,
      subcategoria_original,
      subcategoria_estandar,
      presentacion_original,
      principio_activo_original,
      concentracion_original,
      forma_farmaceutica_original,
      forma_estandar,
      via_administracion_original,
      contenido_original,
      contenido_valor,
      unidad_original,
      contenido_valor_estandar,
      unidad_estandar,
      contenido_estandar,
      unidades_empaque_original,
      invima,
      invima_norm,
      url,
      descripcion_original,
      ficha_tecnica_original,
      texto_original,
      estado_extraccion
    ) |>
    dplyr::distinct(
      ejecucion_id,
      comercio_id,
      producto_comercio_id,
      .keep_all = TRUE
    )

  DBI::dbWriteTable(
    con,
    "tmp_anuncios_t1",
    anuncios,
    temporary = TRUE,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO anuncios(
      ejecucion_id, comercio_id, ejecucion_fuente,
      producto_comercio_id, producto_comun_id, comparable,
      incluido_categoria, regla_categoria, categoria_origen,
      url_categoria, sku, ean, producto_original, marca_original,
      marca_estandar, laboratorio_original, subcategoria_original,
      subcategoria_estandar, presentacion_original,
      principio_activo_original, concentracion_original,
      forma_farmaceutica_original, forma_estandar,
      via_administracion_original, contenido_original,
      contenido_valor, unidad_original, contenido_valor_estandar,
      unidad_estandar, contenido_estandar, unidades_empaque_original,
      invima, invima_norm, url, descripcion_original,
      ficha_tecnica_original, texto_original, estado_extraccion
    )
    SELECT
      ejecucion_id, comercio_id, ejecucion_fuente,
      producto_comercio_id, producto_comun_id, comparable,
      incluido_categoria, regla_categoria, categoria_origen,
      url_categoria, sku, ean, producto_original, marca_original,
      marca_estandar, laboratorio_original, subcategoria_original,
      subcategoria_estandar, presentacion_original,
      principio_activo_original, concentracion_original,
      forma_farmaceutica_original, forma_estandar,
      via_administracion_original, contenido_original,
      contenido_valor, unidad_original, contenido_valor_estandar,
      unidad_estandar, contenido_estandar, unidades_empaque_original,
      invima, invima_norm, url, descripcion_original,
      ficha_tecnica_original, texto_original, estado_extraccion
    FROM tmp_anuncios_t1;
    "
  )

  observaciones <- datos |>
    dplyr::filter(
      !is.na(producto_comercio_id),
      producto_comercio_id != "",
      !is.na(fecha_hora_consulta),
      fecha_hora_consulta != ""
    ) |>
    dplyr::transmute(
      ejecucion_id,
      comercio_id = comercio_id,
      ejecucion_fuente,
      producto_comercio_id,
      fecha_hora_consulta,
      precio_cop,
      precio_fuente,
      precio_lista_cop,
      precio_antes_cop,
      promocion,
      pum_cop,
      pum_unidad_original
    ) |>
    dplyr::distinct(
      comercio_id,
      producto_comercio_id,
      fecha_hora_consulta,
      .keep_all = TRUE
    )

  DBI::dbWriteTable(
    con,
    "tmp_observaciones_t1",
    observaciones,
    temporary = TRUE,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO observaciones(
      ejecucion_id, comercio_id, ejecucion_fuente,
      producto_comercio_id, fecha_hora_consulta,
      precio_cop, precio_fuente, precio_lista_cop,
      precio_antes_cop, promocion, pum_cop, pum_unidad_original
    )
    SELECT
      ejecucion_id, comercio_id, ejecucion_fuente,
      producto_comercio_id, fecha_hora_consulta,
      precio_cop, precio_fuente, precio_lista_cop,
      precio_antes_cop, promocion, pum_cop, pum_unidad_original
    FROM tmp_observaciones_t1;
    "
  )

  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Validaciones finales
# ---------------------------------------------------------------------------

validar_comparables_ejecucion <- function(con, ejecucion_id) {
  cobertura <- DBI::dbGetQuery(
    con,
    "
    SELECT
      a.producto_comun_id,
      p.producto_comun,
      COUNT(DISTINCT a.comercio_id) AS n_tiendas,
      SUM(CASE WHEN o.precio_cop IS NOT NULL THEN 1 ELSE 0 END) AS n_precios
    FROM anuncios a
    INNER JOIN productos_comunes p
      ON a.producto_comun_id = p.producto_comun_id
    LEFT JOIN observaciones o
      ON a.ejecucion_id = o.ejecucion_id
     AND a.comercio_id = o.comercio_id
     AND a.producto_comercio_id = o.producto_comercio_id
    WHERE a.ejecucion_id = ?
      AND a.comparable = 1
    GROUP BY a.producto_comun_id, p.producto_comun
    ORDER BY a.producto_comun_id;
    ",
    params = list(ejecucion_id)
  )

  duplicados <- DBI::dbGetQuery(
    con,
    "
    SELECT
      c.nombre AS comercio,
      a.producto_comercio_id,
      COUNT(*) AS n
    FROM anuncios a
    INNER JOIN comercios c
      ON a.comercio_id = c.comercio_id
    WHERE a.ejecucion_id = ?
    GROUP BY c.nombre, a.producto_comercio_id
    HAVING COUNT(*) > 1;
    ",
    params = list(ejecucion_id)
  )

  total_24 <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM anuncios a
    INNER JOIN observaciones o
      ON a.ejecucion_id = o.ejecucion_id
     AND a.comercio_id = o.comercio_id
     AND a.producto_comercio_id = o.producto_comercio_id
    WHERE a.ejecucion_id = ?
      AND a.comparable = 1
      AND o.precio_cop IS NOT NULL;
    ",
    params = list(ejecucion_id)
  )$n[[1]]

  list(
    cobertura = cobertura,
    total_comparables_con_precio = total_24,
    duplicados = duplicados,
    cumple_24 = nrow(cobertura) == 8L &&
      all(cobertura$n_tiendas == 3L) &&
      all(cobertura$n_precios == 3L) &&
      total_24 == 24L &&
      nrow(duplicados) == 0L
  )
}

auditar_base_t1 <- function(
    db_path = "comparador_precios.sqlite",
    ejecucion_id = NULL) {

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (is.null(ejecucion_id)) {
    ejecucion_id <- DBI::dbGetQuery(
      con,
      "
      SELECT ejecucion_id
      FROM ejecuciones
      GROUP BY ejecucion_id
      ORDER BY MAX(fecha_hora_inicio) DESC
      LIMIT 1;
      "
    )$ejecucion_id[[1]]
  }

  resumen <- DBI::dbGetQuery(
    con,
    "
    SELECT
      e.ejecucion_id,
      c.nombre AS comercio,
      e.ejecucion_fuente,
      e.registros_obtenidos,
      e.productos_categoria,
      e.comparables_con_precio,
      e.productos_reportados_sitio,
      e.estado
    FROM ejecuciones e
    INNER JOIN comercios c
      ON e.comercio_id = c.comercio_id
    WHERE e.ejecucion_id = ?
    ORDER BY c.nombre;
    ",
    params = list(ejecucion_id)
  )

  cobertura <- validar_comparables_ejecucion(con, ejecucion_id)

  list(
    ejecucion_id = ejecucion_id,
    resumen = resumen,
    cobertura = cobertura$cobertura,
    total_comparables_con_precio = cobertura$total_comparables_con_precio,
    duplicados = cobertura$duplicados,
    cumple_24 = cobertura$cumple_24
  )
}

# ---------------------------------------------------------------------------
# FUNCIÓN GENERAL DEL TALLER
# ---------------------------------------------------------------------------

ejecutar_recoleccion <- function(
    modo = c("online", "offline"),
    db_path = "comparador_precios.sqlite",
    raw_root = "raw",
    ejecucion_id = NULL,
    ejecuciones_fuente = EJECUCIONES_OFFLINE_T1,
    reiniciar_db = FALSE,
    pausa_cafam = 0.6,
    pausa_cruz_verde = 0.8) {

  modo <- match.arg(modo)

  if (modo == "online") {
    if (is.null(ejecucion_id)) {
      ejecucion_id <- crear_id_ejecucion_general("recoleccion")
    }

    ids_fuente <- c(
      cafam = ejecucion_id,
      cruz_verde = ejecucion_id,
      la_rebaja = ejecucion_id
    )
  } else {
    requeridos <- c("cafam", "cruz_verde", "la_rebaja")

    if (!all(requeridos %in% names(ejecuciones_fuente))) {
      stop(
        "ejecuciones_fuente debe contener: cafam, cruz_verde y la_rebaja."
      )
    }

    ids_fuente <- ejecuciones_fuente[requeridos]

    if (is.null(ejecucion_id)) {
      ejecucion_id <- crear_id_offline_determinista(ids_fuente)
    }
  }

  message("\n========================================")
  message("Ejecución general: ", ejecucion_id)
  message("Modo: ", modo)
  message("========================================\n")

  resultado_cafam <- scrapear_cafam(
    modo = modo,
    raw_root = raw_root,
    ejecucion_id = ids_fuente[["cafam"]],
    pausa = pausa_cafam
  )

  resultado_cv <- scrapear_cruz_verde(
    modo = modo,
    raw_root = raw_root,
    ejecucion_id = ids_fuente[["cruz_verde"]],
    pausa = pausa_cruz_verde
  )

  resultado_lr <- scrapear_la_rebaja(
    modo = modo,
    raw_root = raw_root,
    ejecucion_id = ids_fuente[["la_rebaja"]]
  )

  cafam <- normalizar_resultado_cafam(
    resultado_cafam,
    ejecucion_global = ejecucion_id,
    ejecucion_fuente = ids_fuente[["cafam"]]
  )

  cv <- normalizar_resultado_cv(
    resultado_cv,
    ejecucion_global = ejecucion_id,
    ejecucion_fuente = ids_fuente[["cruz_verde"]]
  )

  lr <- normalizar_resultado_lr(
    resultado_lr,
    ejecucion_global = ejecucion_id,
    ejecucion_fuente = ids_fuente[["la_rebaja"]]
  )

  datos <- dplyr::bind_rows(cafam, cv, lr)

  resumen_cafam <- construir_resumen_general(
    cafam, resultado_cafam, "Droguerías Cafam",
    ejecucion_id, ids_fuente[["cafam"]], modo,
    resultado_cafam$raw_dir
  )

  resumen_cv <- construir_resumen_general(
    cv, resultado_cv, "Cruz Verde",
    ejecucion_id, ids_fuente[["cruz_verde"]], modo,
    resultado_cv$carpeta_raw
  )

  resumen_lr <- construir_resumen_general(
    lr, resultado_lr, "La Rebaja",
    ejecucion_id, ids_fuente[["la_rebaja"]], modo,
    resultado_lr$raw_dir
  )

  con <- inicializar_base_sqlite(
    db_path = db_path,
    reiniciar = reiniciar_db
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  insertar_resultado_sqlite(con, cafam, resumen_cafam)
  insertar_resultado_sqlite(con, cv, resumen_cv)
  insertar_resultado_sqlite(con, lr, resumen_lr)

  validacion <- validar_comparables_ejecucion(
    con,
    ejecucion_id
  )

  message("\n--- Validación final ---")
  message("Productos comparables: ", nrow(validacion$cobertura), "/8")
  message(
    "Observaciones comparables con precio: ",
    validacion$total_comparables_con_precio,
    "/24"
  )
  message("Duplicados comparables: ", nrow(validacion$duplicados))
  message("Cumple 8 × 3: ", validacion$cumple_24)

  list(
    ejecucion_id = ejecucion_id,
    datos = datos,
    datos_categoria = datos |>
      dplyr::filter(incluido_categoria == 1L),
    comparables = datos |>
      dplyr::filter(comparable == 1L),
    resumen = dplyr::bind_rows(
      resumen_cafam,
      resumen_cv,
      resumen_lr
    ),
    validacion = validacion,
    db_path = db_path
  )
}

# ---------------------------------------------------------------------------
# Uso recomendado
# ---------------------------------------------------------------------------
#
# 1) Reprocesar las copias raw existentes y reconstruir la base sin internet:
#
# resultado <- ejecutar_recoleccion(
#   modo = "offline",
#   db_path = "comparador_precios.sqlite",
#   raw_root = "raw",
#   ejecuciones_fuente = EJECUCIONES_OFFLINE_T1,
#   reiniciar_db = TRUE
# )
#
# resultado$validacion$cobertura
# resultado$validacion$cumple_24
#
# 2) Nueva corrida completa en línea. Los tres comercios comparten el mismo
#    identificador de ejecución general y cada respuesta se guarda en raw/:
#
# resultado_online <- ejecutar_recoleccion(
#   modo = "online",
#   db_path = "comparador_precios.sqlite",
#   raw_root = "raw",
#   reiniciar_db = FALSE
# )
#
# 3) Auditar una base ya construida:
#
# auditoria <- auditar_base_t1("comparador_precios.sqlite")
# auditoria$resumen
# auditoria$cobertura
# auditoria$cumple_24
