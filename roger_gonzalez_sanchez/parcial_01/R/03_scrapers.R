# 03_scrapers.R
# Funciones de extracción independientes por comercio:
#   scrape_farmatodo()  -> Algolia (paginación por número de página)
#   scrape_cruzverde()  -> Salesforce Commerce Cloud (sesión invitado + offset)
#   scrape_larebaja()   -> VTEX (_from/_to + cabecera "resources")
#   scrape_pasteur()    -> VTEX   (respaldo)
#   scrape_exito()      -> VTEX   (respaldo, prefijo /io)
#
# Todas comparten la misma firma y devuelven las 13 columnas estándar + aux_*:
#   scrape_x(test_mode = FALSE, offline = FALSE, fecha_offline = NULL,
#            id_ejecucion = NULL, max_paginas = Inf,
#            categorias = NULL, terminos = NULL, ctx = NULL)
#   - categorias / terminos = NULL -> valores por defecto de COMERCIOS / TERMINOS_BUSQUEDA
#   - test_mode = TRUE -> SOLO 1 página de la primera categoría, raw en raw/test/
#   - offline  = TRUE -> NO hace HTTP: re-procesa raw/{comercio}/{fecha}/


# Columnas comunes que se agregan a lo devuelto por cada parser.
completar_meta <- function(df, meta) {
  if (nrow(df) == 0) return(df)
  df |> mutate(fecha_hora_consulta = meta$fecha_hora_consulta,
               id_ejecucion = meta$id_ejecucion,
               url_consulta = as.character(meta$url %||% NA),
               archivo_raw = as.character(meta$archivo %||% NA),
               aux_origen = paste0(meta$origen, ":", meta$etiqueta))
}

# Une pares "atributo: valor" de una ficha técnica, omitiendo los vacíos.
#   unir_ficha(Proveedor = c("A", NA), Registro = c("X", "Y"))
#   -> c("Proveedor: A | Registro: X", "Registro: Y")
unir_ficha <- function(...) {
  campos <- list(...)
  n <- length(campos[[1]])
  map_chr(seq_len(n), function(i) {
    v <- map_chr(campos, ~ as.character(.x[i]))
    v <- v[!is.na(v) & str_trim(v) != ""]
    if (length(v) == 0) NA_character_ else paste0(names(v), ": ", v, collapse = " | ")
  })
}

# Resumen de cada petición (lo usa la Fase 1 y el log).
fila_resumen <- function(meta, df, status = NA) {
  tibble(origen = meta$origen %||% NA, etiqueta = meta$etiqueta %||% NA,
         pagina = meta$pagina %||% NA, http_status = meta$http_status %||% status,
         total_reportado = meta$total_reportado %||% NA_real_, filas = nrow(df))
}

# Cierra la extracción: estandariza, deduplica y adjunta el resumen.
finalizar_scrape <- function(partes, resumen, ctx) {
  crudo <- bind_rows(partes)
  df <- if (nrow(crudo) == 0) plantilla_estandar() else estandarizar_crudo(crudo, ctx)
  attr(df, "resumen_peticiones") <- bind_rows(resumen)
  attr(df, "id_ejecucion") <- ctx$id_ejecucion
  log_msg(sprintf("  %s: %d registros únicos (%d peticiones, modo %s)",
                  ctx$comercio, nrow(df), nrow(bind_rows(resumen)), ctx$modo))
  df
}

# Re-procesa respuestas guardadas en disco con el parser del comercio.
procesar_offline <- function(ctx, parser, id_ejecucion_offline = NULL) {
  respuestas <- cargar_raw_offline(ctx, id_ejecucion_offline)
  ctx$id_ejecucion <- respuestas[[1]]$meta$id_ejecucion
  partes <- list(); resumen <- list()
  for (r in respuestas) {
    df <- parser(r$texto, r$meta, ctx)
    partes[[length(partes) + 1]] <- df
    resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
  }
  df <- finalizar_scrape(partes, resumen, ctx)
  # Entorno y hora de la captura ORIGINAL (no la del reprocesamiento).
  ent <- unlist(map(respuestas, ~ .x$meta$entorno))
  attr(df, "entorno_captura") <- ent[!is.na(ent)][1] %||% NA_character_
  attr(df, "inicio_captura") <- min(map_chr(respuestas, ~ as.character(.x$meta$fecha_hora_consulta)))
  df
}

preparar_ctx <- function(clave, ctx, test_mode, offline, id_ejecucion, fecha_offline) {
  ctx %||% crear_contexto(clave, test_mode = test_mode, offline = offline,
                          id_ejecucion = id_ejecucion, fecha_offline = fecha_offline)
}
resolver_fuentes <- function(ctx, categorias, terminos, por_defecto) {
  cats <- if (is.null(categorias)) por_defecto else categorias
  terms <- if (is.null(terminos)) TERMINOS_BUSQUEDA else terminos
  if (ctx$test_mode) { cats <- head(cats, 1); terms <- character(0) }
  list(categorias = cats, terminos = terms)
}

# FARMATODO (Algolia)

parsear_farmatodo <- function(texto, meta, ctx) {
  j <- jsonlite::fromJSON(texto, simplifyVector = FALSE)
  hits <- j$hits %||% list()
  if (length(hits) == 0) return(tibble())
  g <- function(campo) map_chr(hits, ~ chr1(.x[[campo]]))
  n <- function(campo) map_dbl(hits, ~ num1(.x[[campo]]))
  full <- n("fullPrice"); oferta <- n("offerPrice")
  precio <- if_else(!is.na(oferta) & oferta > 0 & (is.na(full) | oferta < full), oferta, full)
  marca <- vacio_a_na(g("marca")); marca <- coalesce(marca, g("brand"))
  categoria <- paste(g("categorie"), g("subCategory"), g("rms_group"), g("rms_deparment"), g("rms_class"), sep = " > ")
  con_stock <- map_int(hits, ~ length(.x$stores_with_stock %||% list())) > 0
  tibble(
    sku_comercio = coalesce(g("objectID"), g("id")),
    producto_nombre_original = g("mediaDescription"),
    precio_cop = precio,
    precio_regular_cop = full,
    url_producto = paste0(ctx$cfg$url_base, "/producto/", coalesce(g("url"), g("objectID"))),
    aux_marca_comercio = marca,
    aux_proveedor = g("supplier"),
    # Farmatodo publica la presentación dentro del nombre (mediaDescription).
    presentacion_original = NA_character_,
    descripcion_original = vacio_a_na(g("largeDescription")),
    ficha_tecnica_original = unir_ficha(
      Proveedor = g("supplier"), `Código de barras` = g("barcode"),
      `Requiere fórmula médica` = g("requirePrescription"),
      `Grupo` = g("rms_group"), `Departamento` = g("rms_deparment"),
      `Clase` = g("rms_class"), `Subclase` = g("rms_subclass")),
    categoria_origen = categoria,
    aux_texto_contenido = g("mediaDescription"),
    aux_principio_activo = NA_character_,
    aux_es_salud = str_detect(normalizar_texto(categoria),
                              "salud|medicament|droguer|gripa|\\brx\\b|\\botc\\b|respirator|alerg"),
    # status "E" = retirado. Registros sin inventario NI departamento comercial
    # son fichas huérfanas del índice (no se ofrecen).
    aux_activo = g("status") %in% c("A", NA) & (con_stock | !is.na(g("rms_deparment"))),
    aux_disponible = con_stock
  ) |> completar_meta(meta)
}

# Lee del JavaScript principal del sitio la configuración pública del buscador
# (aplicación, índice y llave de solo búsqueda), tal como la usa el navegador.
.cache_algolia <- new.env()
obtener_config_algolia <- function(cfg) {
  if (!is.null(.cache_algolia$key)) return(.cache_algolia$key)
  llave <- Sys.getenv("FARMATODO_ALGOLIA_KEY", "")
  if (!nzchar(llave)) {
    leer <- function(url) request(url) |> req_user_agent(USER_AGENT) |>
      req_timeout(TIMEOUT_SEG) |> req_retry(max_tries = MAX_REINTENTOS) |>
      req_perform() |> resp_body_string()
    html <- leer(cfg$url_base)
    scripts <- str_match_all(html, 'src="(main[^"]*\\.js)"')[[1]][, 2]
    for (js in scripts) {
      codigo <- leer(paste0(cfg$url_base, "/", js))
      m <- str_match(codigo, '\\{prod:"([a-f0-9]{32})",dev:"[a-f0-9]{32}",sandbox:')
      if (!is.na(m[1, 2]) && str_detect(codigo, fixed(cfg$algolia_app))) { llave <- m[1, 2]; break }
    }
  }
  if (!nzchar(llave)) stop("No se encontró la configuración pública del buscador de Farmatodo ",
                           "(¿cambió el sitio?). Puede definir FARMATODO_ALGOLIA_KEY.")
  .cache_algolia$key <- llave
  llave
}

peticion_farmatodo <- function(ctx, query, filtro, pagina, tam, origen, etiqueta) {
  cfg <- ctx$cfg
  params <- paste0("query=", utils::URLencode(query, reserved = TRUE),
                   "&hitsPerPage=", tam, "&page=", pagina,
                   if (!is.null(filtro)) paste0("&filters=", utils::URLencode(filtro, reserved = TRUE)) else "")
  cuerpo <- as.character(jsonlite::toJSON(list(params = params), auto_unbox = TRUE))
  url <- sprintf("https://%s-dsn.algolia.net/1/indexes/%s/query", tolower(cfg$algolia_app), cfg$algolia_indice)
  realizar_peticion(ctx, url, cuerpo_json = cuerpo,
                    headers = list(`x-algolia-application-id` = cfg$algolia_app,
                                   `x-algolia-api-key` = obtener_config_algolia(cfg)),
                    origen = origen, etiqueta = etiqueta, pagina = pagina + 1L,
                    fn_total = function(txt, resp) num1(jsonlite::fromJSON(txt, simplifyVector = FALSE)$nbHits))
}

scrape_farmatodo <- function(test_mode = FALSE, offline = FALSE, fecha_offline = NULL,
                             id_ejecucion = NULL, max_paginas = Inf,
                             categorias = NULL, terminos = NULL, ctx = NULL,
                             id_ejecucion_offline = NULL) {
  ctx <- preparar_ctx("farmatodo", ctx, test_mode, offline, id_ejecucion, fecha_offline)
  log_msg("Farmatodo [", ctx$modo, "]")
  if (ctx$modo == "offline") return(procesar_offline(ctx, parsear_farmatodo, id_ejecucion_offline))
  cfg <- ctx$cfg
  f <- resolver_fuentes(ctx, categorias, terminos,
                        c("Sistema respiratorio OTC+RX" = paste(cfg$filtro_categorias, collapse = " OR ")))
  partes <- list(); resumen <- list()

  for (i in seq_along(f$categorias)) {
    etiqueta <- names(f$categorias)[i] %||% f$categorias[i]
    pagina <- 0L
    repeat {
      r <- peticion_farmatodo(ctx, "", f$categorias[[i]], pagina, cfg$tam_pagina, "categoria", etiqueta)
      if (!r$ok) { resumen[[length(resumen) + 1]] <- fila_resumen(list(origen = "categoria", etiqueta = etiqueta, pagina = pagina + 1), tibble(), r$status); break }
      df <- parsear_farmatodo(r$texto, r$meta, ctx)
      partes[[length(partes) + 1]] <- df
      resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
      n_pag <- ceiling(coalesce(r$meta$total_reportado, 0) / cfg$tam_pagina)
      pagina <- pagina + 1L
      if (ctx$test_mode || pagina >= n_pag || pagina >= max_paginas || nrow(df) == 0) break
    }
  }
  for (term in f$terminos) {
    r <- peticion_farmatodo(ctx, term, NULL, 0L, cfg$tam_pagina_busqueda, "busqueda", term)
    if (!r$ok) { resumen[[length(resumen) + 1]] <- fila_resumen(list(origen = "busqueda", etiqueta = term, pagina = 1L), tibble(), r$status); next }
    df <- parsear_farmatodo(r$texto, r$meta, ctx)
    partes[[length(partes) + 1]] <- df
    resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
  }
  finalizar_scrape(partes, resumen, ctx)
}

# CRUZ VERDE (Salesforce Commerce Cloud, sesión de invitado)
parsear_cruzverde <- function(texto, meta, ctx) {
  j <- jsonlite::fromJSON(texto, simplifyVector = FALSE)
  hits <- j$hits %||% list()
  if (length(hits) == 0) return(tibble())
  g <- function(campo) map_chr(hits, ~ chr1(.x[[campo]]))
  lista <- map_dbl(hits, ~ num1(.x$prices[["price-list-col"]]))
  venta <- map_dbl(hits, ~ num1(.x$prices[["price-sale-col"]]))
  precio <- case_when(!is.na(venta) & venta > 0 & (is.na(lista) | venta < lista) ~ venta,
                      TRUE ~ lista)
  tibble(
    sku_comercio = g("productId"),
    producto_nombre_original = g("productName"),
    precio_cop = precio,
    precio_regular_cop = coalesce(lista, venta),
    url_producto = paste0(ctx$cfg$url_base, "/", g("pageURL"), "/", g("productId"), ".html"),
    aux_marca_comercio = g("brand"),
    aux_proveedor = NA_character_,
    presentacion_original = NA_character_,
    descripcion_original = NA_character_,
    ficha_tecnica_original = NA_character_,
    categoria_origen = if (identical(meta$origen, "categoria")) meta$etiqueta else NA_character_,
    aux_texto_contenido = g("productName"),
    aux_principio_activo = NA_character_,
    aux_es_salud = TRUE,                     
    aux_activo = TRUE,
    aux_disponible = NA                       
  ) |> completar_meta(meta)
}

iniciar_sesion_cruzverde <- function(ctx) {
  r <- realizar_peticion(ctx, paste0(ctx$cfg$api, "/customer-service/login"),
                         cuerpo_json = "{}", guardar = FALSE, origen = "login")
  if (!isTRUE(r$ok)) stop("No fue posible abrir sesión de invitado en Cruz Verde (HTTP ", r$status, ")")
  invisible(TRUE)
}

peticion_cruzverde <- function(ctx, q, cgid, offset, tam, origen, etiqueta, pagina) {
  url <- paste0(ctx$cfg$api, "/product-service/products/search?limit=", tam,
                "&offset=", offset, "&sort=&q=", utils::URLencode(q %||% "", reserved = TRUE),
                if (!is.null(cgid)) paste0("&refine%5B%5D=", utils::URLencode(paste0("cgid=", cgid), reserved = TRUE)) else "")
  total_fn <- function(txt, resp) num1(jsonlite::fromJSON(txt, simplifyVector = FALSE)$total)
  r <- realizar_peticion(ctx, url, origen = origen, etiqueta = etiqueta, pagina = pagina, fn_total = total_fn)
  if (identical(r$status, 401L)) {           # sesión expirada -> renovar y reintentar
    iniciar_sesion_cruzverde(ctx)
    r <- realizar_peticion(ctx, url, origen = origen, etiqueta = etiqueta, pagina = pagina, fn_total = total_fn)
  }
  r
}

scrape_cruzverde <- function(test_mode = FALSE, offline = FALSE, fecha_offline = NULL,
                             id_ejecucion = NULL, max_paginas = Inf,
                             categorias = NULL, terminos = NULL, ctx = NULL,
                             id_ejecucion_offline = NULL) {
  ctx <- preparar_ctx("cruzverde", ctx, test_mode, offline, id_ejecucion, fecha_offline)
  log_msg("Cruz Verde [", ctx$modo, "]")
  if (ctx$modo == "offline") return(procesar_offline(ctx, parsear_cruzverde, id_ejecucion_offline))
  cfg <- ctx$cfg
  f <- resolver_fuentes(ctx, categorias, terminos, cfg$categorias)
  iniciar_sesion_cruzverde(ctx)
  partes <- list(); resumen <- list()

  for (cat in f$categorias) {
    offset <- 0; pagina <- 1L
    repeat {
      r <- peticion_cruzverde(ctx, "", cat, offset, cfg$tam_pagina, "categoria", cat, pagina)
      if (!r$ok) { resumen[[length(resumen) + 1]] <- fila_resumen(list(origen = "categoria", etiqueta = cat, pagina = pagina), tibble(), r$status); break }
      df <- parsear_cruzverde(r$texto, r$meta, ctx)
      partes[[length(partes) + 1]] <- df
      resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
      total <- r$meta$total_reportado %||% 0
      offset <- offset + cfg$tam_pagina; pagina <- pagina + 1L
      if (ctx$test_mode || offset >= total || pagina > max_paginas || nrow(df) == 0) break
    }
  }
  for (term in f$terminos) {
    r <- peticion_cruzverde(ctx, term, NULL, 0, cfg$tam_pagina, "busqueda", term, 1L)
    if (!r$ok) { resumen[[length(resumen) + 1]] <- fila_resumen(list(origen = "busqueda", etiqueta = term, pagina = 1L), tibble(), r$status); next }
    df <- parsear_cruzverde(r$texto, r$meta, ctx)
    partes[[length(partes) + 1]] <- df
    resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
  }
  finalizar_scrape(partes, resumen, ctx)
}


# VTEX genérico (La Rebaja, Farmacias Pasteur, Éxito)
quitar_html <- function(x) str_squish(str_replace_all(coalesce(x, ""), "<[^>]+>|&nbsp;", " "))

parsear_vtex <- function(texto, meta, ctx) {
  prods <- jsonlite::fromJSON(texto, simplifyVector = FALSE)
  if (length(prods) == 0) return(tibble())
  cfg <- ctx$cfg
  filas <- map(prods, function(p) {
    items <- p$items %||% list()
    if (length(items) == 0) return(NULL)
    campos_pa <- names(p)[str_detect(names(p), regex("principio|activo|composici", ignore_case = TRUE))]
    principio <- if (length(campos_pa)) paste(unlist(p[campos_pa]), collapse = ", ") else NA_character_
    campos_prov <- intersect(c("Fabricante/ Distribuidor", "Proveedor", "Laboratorio", "Fabricante"), names(p))
    proveedor <- if (length(campos_prov)) chr1(p[[campos_prov[1]]]) else NA_character_
    categorias <- paste(unlist(p$categories), collapse = " | ")
    pres <- if (length(items) == 1 && !is.null(p$Presentacionunidadmedida))
      paste(chr1(p$Presentacionunidadmedida), chr1(p$Unidadmedida)) else NA_character_
    pres_original <- chr1(p$Presentacionunidadmedida)
    if (is.na(pres_original)) {
      partes_pres <- c(chr1(p[["Presentaci\u00f3n"]]), chr1(p[["Tama\u00f1o"]]))
      partes_pres <- partes_pres[!is.na(partes_pres)]
      if (length(partes_pres)) pres_original <- paste(partes_pres, collapse = " ")
    }
    # Ficha técnica: atributos publicados (allSpecifications) sin transformar.
    specs <- unlist(p$allSpecifications) %||% character(0)
    specs <- specs[!str_detect(specs, REGEX_SPECS_EXCLUIDAS)]
    ficha <- if (length(specs)) {
      valores <- map_chr(specs, ~ paste(unlist(p[[.x]]), collapse = ", "))
      do.call(unir_ficha, as.list(set_names(valores, specs)))
    } else NA_character_
    # Descripción larga (La Rebaja la publica en DescripcionContenido).
    descripcion <- coalesce(vacio_a_na(chr1(p$DescripcionContenido)), vacio_a_na(chr1(p$description)))
    url <- chr1(p$link)
    if (is.na(url) || !str_starts(url, "http")) url <- paste0(cfg$url_base, "/", chr1(p$linkText), "/p")

    map(items, function(it) {
      sellers <- it$sellers %||% list()
      if (length(sellers) == 0) return(NULL)
      def <- which(map_lgl(sellers, ~ isTRUE(.x$sellerDefault)))
      co <- sellers[[if (length(def)) def[1] else 1]]$commertialOffer
      precio <- num1(co$Price)
      regular <- num1(co$ListPrice)
      if (is.na(regular) || regular <= 0) regular <- num1(co$PriceWithoutDiscount)
      nombre <- if (identical(cfg$campo_nombre, "productName")) chr1(p$productName) else chr1(it$nameComplete %||% p$productName)
      tibble(
        sku_comercio = chr1(it$itemId),
        producto_nombre_original = nombre,
        precio_cop = precio,
        precio_regular_cop = regular,
        url_producto = url,
        presentacion_original = if (length(items) == 1) pres_original else coalesce(chr1(it$name), pres_original),
        descripcion_original = descripcion,
        ficha_tecnica_original = ficha,
        categoria_origen = categorias,
        aux_marca_comercio = chr1(p$brand),
        aux_proveedor = proveedor,
        aux_texto_contenido = coalesce(pres, chr1(it$nameComplete), nombre),
        aux_principio_activo = principio,
        aux_es_salud = str_detect(normalizar_texto(categorias), "drogueria|salud|medicament|farmac") &
          !str_detect(normalizar_texto(categorias), "mascota"),
        aux_activo = TRUE,
        aux_disponible = coalesce(num1(co$AvailableQuantity), 0) > 0
      )
    }) |> list_rbind()
  })
  bind_rows(filas) |> completar_meta(meta)
}

total_vtex <- function(txt, resp) {
  h <- resp_header(resp, "resources")
  if (is.null(h)) return(NA_real_)
  as.numeric(str_extract(h, "\\d+$"))
}

scrape_vtex <- function(clave, test_mode, offline, fecha_offline, id_ejecucion, max_paginas,
                        categorias, terminos, ctx, id_ejecucion_offline) {
  ctx <- preparar_ctx(clave, ctx, test_mode, offline, id_ejecucion, fecha_offline)
  log_msg(ctx$comercio, " [", ctx$modo, "]")
  if (ctx$modo == "offline") return(procesar_offline(ctx, parsear_vtex, id_ejecucion_offline))
  cfg <- ctx$cfg
  f <- resolver_fuentes(ctx, categorias, terminos, cfg$categorias)
  tam <- cfg$tam_pagina
  partes <- list(); resumen <- list()

  for (cat in f$categorias) {
    desde <- 0; pagina <- 1L
    repeat {
      url <- sprintf("%s/%s?_from=%d&_to=%d", cfg$api, cat, desde, desde + tam - 1)
      r <- realizar_peticion(ctx, url, origen = "categoria", etiqueta = cat, pagina = pagina, fn_total = total_vtex)
      if (!r$ok) { resumen[[length(resumen) + 1]] <- fila_resumen(list(origen = "categoria", etiqueta = cat, pagina = pagina), tibble(), r$status); break }
      df <- parsear_vtex(r$texto, r$meta, ctx)
      partes[[length(partes) + 1]] <- df
      resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
      total <- coalesce(r$meta$total_reportado, 0)
      desde <- desde + tam; pagina <- pagina + 1L
      if (ctx$test_mode || desde >= total || desde >= 2500 || pagina > max_paginas || nrow(df) == 0) break
    }
  }
  for (term in f$terminos) {
    url <- sprintf("%s?ft=%s&_from=0&_to=%d", cfg$api, utils::URLencode(term, reserved = TRUE), tam - 1)
    r <- realizar_peticion(ctx, url, origen = "busqueda", etiqueta = term, pagina = 1L, fn_total = total_vtex)
    if (!r$ok) { resumen[[length(resumen) + 1]] <- fila_resumen(list(origen = "busqueda", etiqueta = term, pagina = 1L), tibble(), r$status); next }
    df <- parsear_vtex(r$texto, r$meta, ctx)
    partes[[length(partes) + 1]] <- df
    resumen[[length(resumen) + 1]] <- fila_resumen(r$meta, df)
  }
  finalizar_scrape(partes, resumen, ctx)
}

scrape_larebaja <- function(test_mode = FALSE, offline = FALSE, fecha_offline = NULL,
                            id_ejecucion = NULL, max_paginas = Inf,
                            categorias = NULL, terminos = NULL, ctx = NULL,
                            id_ejecucion_offline = NULL) {
  scrape_vtex("larebaja", test_mode, offline, fecha_offline, id_ejecucion, max_paginas,
              categorias, terminos, ctx, id_ejecucion_offline)
}

scrape_pasteur <- function(test_mode = FALSE, offline = FALSE, fecha_offline = NULL,
                           id_ejecucion = NULL, max_paginas = Inf,
                           categorias = NULL, terminos = NULL, ctx = NULL,
                           id_ejecucion_offline = NULL) {
  scrape_vtex("pasteur", test_mode, offline, fecha_offline, id_ejecucion, max_paginas,
              categorias, terminos, ctx, id_ejecucion_offline)
}

scrape_exito <- function(test_mode = FALSE, offline = FALSE, fecha_offline = NULL,
                         id_ejecucion = NULL, max_paginas = Inf,
                         categorias = NULL, terminos = NULL, ctx = NULL,
                         id_ejecucion_offline = NULL) {
  scrape_vtex("exito", test_mode, offline, fecha_offline, id_ejecucion, max_paginas,
              categorias, terminos, ctx, id_ejecucion_offline)
}

FUNCIONES_SCRAPE <- list(farmatodo = scrape_farmatodo, cruzverde = scrape_cruzverde,
                         larebaja = scrape_larebaja, pasteur = scrape_pasteur,
                         exito = scrape_exito)
