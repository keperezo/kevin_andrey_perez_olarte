# scraper.R -- Taller 1, Mineria de Datos
# Categoria: lagrimas artificiales y medicamentos de aplicacion ocular u otica
# (excluye soluciones para limpiar lentes de contacto)
#
# Uso en linea:     source("scraper.R"); recolectar_todo()
# Uso sin conexion: recolectar_todo(offline = TRUE, fecha_dir = "2026-10-10")

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(DBI); library(RSQLite)
  library(dplyr); library(purrr); library(stringr); library(tibble)
})

TZ_CO   <- "America/Bogota"
RAW_DIR <- "raw"
DB_PATH <- "comparador_precios.sqlite"
UA      <- "Proyecto academico Universidad Nacional (contacto: mbarretoz.edu.co)"

# ---------------------------------------------------------------- utilidades
`%||%` <- function(a, b) if (is.null(a)) b else a
iso_ahora <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = TZ_CO)
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", x)          # -0500 -> -05:00
}

# ------------------------------------------------- configuracion de comercios
# COMPLETAR tras revisar robots.txt, terminos de uso y DevTools > Network.
# cat_ids: IDs de categoria VTEX (ver https://<host>/api/catalog_system/pub/category/tree/3)
# Si un sitio NO es VTEX, reemplaza su funcion scrape_* por una que use rvest
# o el endpoint JSON que encuentres, devolviendo las mismas columnas.
COMERCIOS <- list(
  farmatodo = list(nombre = "Farmatodo", host = "www.farmatodo.com.co",
                   categorias_doc = paste0("Salud y medicamentos > Cuidado de la vista > ",
                     "subcategoria 'Gotas, lubricantes y lagrimas artificiales' (incluida). ",
                     "'Solucion para lentes de contacto' (extraida y excluida).")),
  colsubsidio = list(nombre = "Colsubsidio", host = "www.drogueriascolsubsidio.com",
                     categorias_doc = paste0("Salud Visual > Soluciones y gotas ",
                       "(ruta salud-visual/soluciones-y-gotas). Se extraen tambien las soluciones ",
                       "para lentes de contacto y otros productos no oculares para marcarlos como excluidos.")),
  pasteur = list(nombre = "Farmacias Pasteur", host = "www.farmaciaspasteur.com.co",
                 categorias_doc = paste0("Salud y medicamentos > Visuales > Gotas y lubricantes ",
                   "(ruta salud-y-medicamentos/visuales/gotas-y-lubricantes, 70 productos segun el sitio).")))

COLUMNAS <- c("sku", "nombre_original", "marca_original", "descripcion_original",
              "url", "categoria_sitio", "precio", "precio_lista", "promocion",
              "disponible",
              # datos que algunos sitios publican aparte (NA si el sitio no los da)
              "marca_comercial", "principio_activo", "concentracion",
              "presentacion", "registro_invima")

# ----------------------------------------------------- extraccion FARMATODO
# Farmatodo no usa VTEX: su pagina consulta un indice Algolia ("products-colombia").
# La clave de busqueda NO se guarda en el codigo. Ponla en .Renviron:
#   FARMATODO_ALGOLIA_KEY=<valor del encabezado x-algolia-api-key que ves en Network>
FARMATODO_APP_ID <- "VCOJEYD2PO"
FARMATODO_URL    <- "https://api-search.farmatodo.com/1/indexes/*/queries"

# Mismos filtros que aplica el sitio al listar la categoria "Cuidado de la vista"
# (classification.id 13): oculta agotados, productos con formula medica y muestras.
FARMATODO_BASE <- paste("outofstore:false", "classification.id:13",
                        "NOT requirePrescription:true", "NOT rms_class:SAMPLING",
                        sep = " AND ")

# Grupos de consulta de Farmatodo:
#  - gotas: subcategoria equivalente a la categoria del taller (incluida)
#  - lentes_sol: se extrae solo para documentar la exclusion (estandarizar() la excluye)
#  - oticas: Farmatodo no tiene seccion otica propia; se usa la busqueda interna del
#    sitio ("gotas oticas") y estandarizar() solo conserva lo que dice otico en el nombre.
#    Sin filtro de receta, porque asi aparecen en la busqueda del sitio.
FARMATODO_GRUPOS <- list(
  gotas = list(query = "", busqueda = FALSE,
    filtro = paste(FARMATODO_BASE, 'subCategory:"Gotas, lubricantes y lágrimas artificiales"',
                   sep = " AND ")),
  lentes_sol = list(query = "", busqueda = FALSE,
    filtro = paste(FARMATODO_BASE, 'subCategory:"Solución para lentes de contacto"',
                   sep = " AND ")),
  oticas = list(query = "gotas oticas", busqueda = TRUE,
    filtro = "outofstore:false AND NOT rms_class:SAMPLING")
)

# Convierte los "hits" de Algolia en las columnas estandar.
parsear_farmatodo <- function(hits) {
  # precios por ciudad: se usa Bogota (BOG)
  por_ciudad <- function(lista, campo) {
    x <- keep(lista %||% list(), ~ identical(.x$cityCode, "BOG"))
    if (length(x)) x[[1]][[campo]] else NULL
  }
  map_dfr(hits, function(h) {
    lista  <- por_ciudad(h$fullPriceByCity, "fullPrice") %||% h$fullPrice
    oferta <- por_ciudad(h$offerPriceByCity, "offerPrice") %||% h$offerPrice %||% 0
    lista  <- if (is.null(lista) || lista <= 0) NA_real_ else as.numeric(lista)
    promo  <- !is.na(lista) && oferta > 0 && oferta < lista
    desc   <- str_squish(h$largeDescription %||% "")
    tibble(
      sku  = as.character(h$id %||% h$objectID),
      nombre_original      = h$mediaDescription %||% NA_character_,
      marca_original       = h$marca %||% NA_character_,   # 'brand' NO es la marca (ver registro_invima)
      descripcion_original = if (nzchar(desc)) desc else NA_character_,
      url  = if (is.null(h$url)) NA_character_
             else paste0("https://www.farmatodo.com.co/producto/", h$url),
      categoria_sitio = paste(h$categorie %||% "", h$subCategory %||% "", sep = " > "),
      precio       = if (promo) as.numeric(oferta) else lista,
      precio_lista = if (promo) lista else NA_real_,
      promocion    = as.integer(promo),
      disponible   = as.integer(isTRUE(h$hasStock)),
      marca_comercial  = h$marca %||% NA_character_,
      principio_activo = NA_character_,   # Farmatodo no lo publica como campo
      concentracion    = NA_character_,
      presentacion     = NA_character_,
      # OJO: 'brand' NO es el registro Invima; es un codigo fijo de Farmatodo (igual en
      # todos los productos). Farmatodo no publica el registro, asi que queda NA.
      registro_invima  = NA_character_
    )
  })
}

scrape_farmatodo <- function(fecha_dir, offline = FALSE, por_pagina = 100, pausa = 1) {
  dir_raw <- file.path(RAW_DIR, "farmatodo", fecha_dir)
  meta_f  <- file.path(dir_raw, "meta.json")
  hits_g  <- list()                      # productos crudos, por grupo

  if (offline) {
    archivos <- sort(list.files(dir_raw, pattern = "\\.json\\.gz$", full.names = TRUE))
    if (length(archivos) == 0) stop("No hay copias crudas en ", dir_raw)
    for (f in archivos) {
      g  <- sub("_p\\d+\\.json\\.gz$", "", basename(f))   # el nombre del archivo trae el grupo
      hs <- fromJSON(gzfile(f), simplifyVector = FALSE)$results[[1]]$hits
      hits_g[[g]] <- c(hits_g[[g]], hs)
    }
    meta <- fromJSON(meta_f)
  } else {
    clave <- Sys.getenv("FARMATODO_ALGOLIA_KEY")
    if (!nzchar(clave)) stop("Falta FARMATODO_ALGOLIA_KEY en .Renviron (ver README)")
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
    fecha_consulta <- iso_ahora()
    total <- 0L

    for (g in names(FARMATODO_GRUPOS)) {
      cfg <- FARMATODO_GRUPOS[[g]]
      pag <- 0L                                   # en Algolia la pagina inicial es 0
      repeat {
        cuerpo <- list(requests = list(list(
          indexName = "products-colombia", query = cfg$query,
          params = sprintf("hitsPerPage=%d&page=%d&filters=%s", por_pagina, pag,
                           URLencode(enc2utf8(cfg$filtro), reserved = TRUE)))))
        resp <- request(FARMATODO_URL) |>
          req_headers(`x-algolia-application-id` = FARMATODO_APP_ID,
                      `x-algolia-api-key` = clave,
                      Origin = "https://www.farmatodo.com.co") |>
          req_user_agent(UA) |> req_body_json(cuerpo) |>
          req_retry(max_tries = 3) |> req_perform()
        txt <- resp_body_string(resp)
        r   <- fromJSON(txt, simplifyVector = FALSE)$results[[1]]

        gz <- gzfile(file.path(dir_raw, sprintf("%s_p%03d.json.gz", g, pag)), "w")
        writeLines(txt, gz); close(gz)

        if (pag == 0) total <- total + r$nbHits
        hits_g[[g]] <- c(hits_g[[g]], r$hits)
        pag <- pag + 1L
        if (pag >= r$nbPages) break
        Sys.sleep(pausa)
      }
    }
    meta <- list(fecha_consulta = fecha_consulta, total_sitio = total)
    write_json(meta, meta_f, auto_unbox = TRUE)
  }

  if (length(hits_g) == 0)
    stop("Farmatodo: la consulta no devolvio productos (revisa clave y filtros)")
  df <- bind_rows(imap(hits_g, function(hs, g) {
    d <- parsear_farmatodo(hs)
    if (isTRUE(FARMATODO_GRUPOS[[g]]$busqueda))     # marca que viene de una busqueda
      d$categoria_sitio <- paste0("Busqueda: ", FARMATODO_GRUPOS[[g]]$query,
                                  " | ", d$categoria_sitio)
    d
  })) |> distinct(sku, .keep_all = TRUE)
  attr(df, "fecha_consulta") <- meta$fecha_consulta
  attr(df, "total_sitio")    <- meta$total_sitio
  df[, COLUMNAS]
}

# ------------------------------------- extraccion VTEX IO (Colsubsidio y Pasteur)
# Ambas tiendas piden a /_v/segment/graphql/v1 la "persisted query" productSearchV3
# (se identifico en DevTools > Network). La categoria y la pagina (from/to) viajan
# como JSON en base64 dentro de 'extensions'. El hash es el mismo en ambos sitios.
# Paginas de 12 productos, igual que los sitios.
VTEXIO_HASH <- "a96c5ed03ef513568c34b63a4bf8da055ea6296293ee02eadb56e2e895eaffbc"
VTEXIO <- list(
  colsubsidio = list(
    base    = "https://www.drogueriascolsubsidio.com",
    binding = "f6e98a9f-25bc-476b-b0df-62ddc6b57b48",
    # agregar aqui la ruta de gotas oticas cuando se ubique
    rutas   = c("salud-visual/soluciones-y-gotas"),
    orden   = "OrderByScoreDESC",
    extra   = list()),
  pasteur = list(
    base    = "https://www.farmaciaspasteur.com.co",
    binding = "0f700f00-7599-43f3-9dad-514c68824d95",
    rutas   = c("salud-y-medicamentos/visuales/gotas-y-lubricantes"),
    # orden por nombre: estable entre paginas (el orden por descuento tiene empates)
    orden   = "OrderByNameASC",
    extra   = list(operator = "and", fuzzy = "0", hideUnavailableItems = FALSE))
)

vtexio_url <- function(cfg, ruta, desde, hasta) {
  partes <- strsplit(ruta, "/")[[1]]
  vars <- c(list(
    skusFilter = "ALL", simulationBehavior = "default",
    installmentCriteria = "MAX_WITHOUT_INTEREST", productOriginVtex = FALSE,
    map = paste(rep("c", length(partes)), collapse = ","), query = ruta,
    orderBy = cfg$orden, from = desde, to = hasta,
    selectedFacets = lapply(partes, function(x) list(key = "c", value = x)),
    facetsBehavior = "Static", categoryTreeBehavior = "default",
    withFacets = FALSE), cfg$extra)
  vars_b64 <- base64_enc(charToRaw(toJSON(vars, auto_unbox = TRUE)))
  ext <- list(
    persistedQuery = list(version = 1, sha256Hash = VTEXIO_HASH,
                          sender = "vtex.store-resources@0.x",
                          provider = "vtex.search-graphql@0.x"),
    variables = vars_b64)
  sprintf(paste0("%s/_v/segment/graphql/v1?workspace=master&maxAge=short&appsEtag=remove",
                 "&domain=store&locale=es-CO&__bindingId=%s&operationName=productSearchV3",
                 "&variables=%%7B%%7D&extensions=%s"),
          cfg$base, cfg$binding,
          URLencode(toJSON(ext, auto_unbox = TRUE), reserved = TRUE))
}

parsear_vtexio <- function(productos, base) {
  # busca una propiedad del producto por parte de su nombre (regex, sin mayusculas)
  prop <- function(p, patron) {
    for (x in p$properties %||% list()) {
      if (str_detect(str_to_lower(x$name %||% ""), patron))
        return(paste(unlist(x$values), collapse = ", "))
    }
    NA_character_
  }
  map_dfr(productos, function(p) {
    item <- (p$items %||% list(list()))[[1]]
    of   <- ((item$sellers %||% list(list()))[[1]])$commertialOffer %||% list()
    pr <- of$Price;     pr <- if (is.null(pr) || pr <= 0) NA_real_ else as.numeric(pr)
    pl <- of$ListPrice; pl <- if (is.null(pl) || pl <= 0) NA_real_ else as.numeric(pl)
    promo <- !is.na(pr) && !is.na(pl) && pl > pr
    lnk   <- p$link %||% NA_character_
    desc  <- prop(p, "descripci")             # HTML original, sin limpiar
    tibble(
      sku  = as.character(p$productId),
      nombre_original      = p$productName %||% NA_character_,
      marca_original       = p$brand %||% NA_character_,   # suele ser el laboratorio
      descripcion_original = if (is.na(desc)) (p$description %||% NA_character_) else desc,
      url  = if (is.na(lnk)) NA_character_
             else if (str_starts(lnk, "http")) lnk else paste0(base, lnk),
      categoria_sitio = paste(unlist(p$categories), collapse = " | "),
      precio       = pr,
      precio_lista = if (promo) pl else NA_real_,
      promocion    = as.integer(promo),
      disponible   = as.integer((of$AvailableQuantity %||% 0) > 0),
      marca_comercial  = {m <- prop(p, "marca comercial"); if (is.na(m)) prop(p, "^marca$") else m},
      principio_activo = prop(p, "principio activo"),
      concentracion    = prop(p, "concentraci"),
      presentacion     = prop(p, "presentaci"),
      registro_invima  = prop(p, "invima|registro sanitario")
    )
  })
}

scrape_vtexio <- function(clave, fecha_dir, offline = FALSE, paso = 12, pausa = 1.5) {
  cfg     <- VTEXIO[[clave]]
  dir_raw <- file.path(RAW_DIR, clave, fecha_dir)
  meta_f  <- file.path(dir_raw, "meta.json")

  if (offline) {
    archivos <- sort(list.files(dir_raw, pattern = "\\.json\\.gz$", full.names = TRUE))
    if (length(archivos) == 0) stop("No hay copias crudas en ", dir_raw)
    productos <- unlist(
      map(archivos, ~ fromJSON(gzfile(.x), simplifyVector = FALSE)$data$productSearch$products),
      recursive = FALSE)
    meta <- fromJSON(meta_f)
  } else {
    dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
    fecha_consulta <- iso_ahora()
    total <- 0L; productos <- list()

    for (i in seq_along(cfg$rutas)) {
      desde <- 0L
      repeat {
        resp <- request(vtexio_url(cfg, cfg$rutas[i], desde, desde + paso - 1L)) |>
          req_headers(accept = "*/*", `content-type` = "application/json",
                      Referer = paste0(cfg$base, "/", cfg$rutas[i])) |>
          req_user_agent(UA) |> req_retry(max_tries = 3) |> req_perform()
        txt <- resp_body_string(resp)
        r   <- fromJSON(txt, simplifyVector = FALSE)
        # si el sitio se actualiza, el hash puede cambiar (PersistedQueryNotFound)
        if (!is.null(r$errors))
          stop(clave, ": ", r$errors[[1]]$message, " (revisar VTEXIO_HASH en Network)")
        ps <- r$data$productSearch

        gz <- gzfile(file.path(dir_raw, sprintf("ruta%d_%04d.json.gz", i, desde)), "w")
        writeLines(txt, gz); close(gz)

        if (desde == 0L) total <- total + ps$recordsFiltered
        productos <- c(productos, ps$products)
        desde <- desde + as.integer(paso)
        if (length(ps$products) == 0 || desde >= ps$recordsFiltered) break
        Sys.sleep(pausa)
      }
    }
    meta <- list(fecha_consulta = fecha_consulta, total_sitio = total)
    write_json(meta, meta_f, auto_unbox = TRUE)
  }

  if (length(productos) == 0) stop(clave, ": la consulta no devolvio productos")
  df <- parsear_vtexio(productos, cfg$base) |> distinct(sku, .keep_all = TRUE)
  attr(df, "fecha_consulta") <- meta$fecha_consulta
  attr(df, "total_sitio")    <- meta$total_sitio
  df[, COLUMNAS]
}

scrape_colsubsidio <- function(fecha_dir, offline = FALSE) scrape_vtexio("colsubsidio", fecha_dir, offline)
scrape_pasteur     <- function(fecha_dir, offline = FALSE) scrape_vtexio("pasteur", fecha_dir, offline)

# ------------------------------------- una funcion por comercio (mismas columnas)
FUNCIONES <- list(farmatodo   = scrape_farmatodo,
                  colsubsidio = scrape_colsubsidio,
                  pasteur     = scrape_pasteur)

# ------------------------------------------- estandarizacion (campos propios)
# Reglas de clasificacion. Todo lo que aparece en la seccion ocular u otica del
# sitio se INCLUYE por defecto; solo se excluye lo que la delimitacion deja fuera.
rx_lentes  <- regex(paste0("multiprop|multi ?solu|multisolu|opti-?free|biotrue|aosept|",
                           "clear ?care|simplus|multi 20/20|desinfectante|per[oó]xido|",
                           "estuche|portalentes"), ignore_case = TRUE)
rx_cat_lentes <- regex("soluci[oó]n(es)? para lentes", ignore_case = TRUE)  # categoria del sitio
rx_higiene <- regex(paste0("limpiador|limpieza|espum|clean-?t|toallita|wipes|lidclean|blefadex|",
                           "aplicadores|pesta[nñ]as|p[aá]rpados|anteojos|gafas|blephagel|higiene"),
                    ignore_case = TRUE)
rx_oral    <- regex("c[aá]psula|tableta|\\boral\\b|jarabe|suplemento", ignore_case = TRUE)
rx_otico   <- regex("\\b[oó]tic[oa]s?\\b|o[ií]do|cerumen|otitis", ignore_case = TRUE)
rx_lagrima <- regex(paste0("l[aá]grima|tears|lubricante|humectante|hialuron|carboximetil|",
                           "hipromelosa|hidroxipropil|polivin|propilenglicol|polietilenglicol|",
                           "ojo seco|systane|refresh|hylo|freegen|hialtears|lagricel|hialsoft|carmelub"),
                    ignore_case = TRUE)
rx_revisar <- regex("renu|contacto|rehumect|lentes", ignore_case = TRUE)
# Marcas conocidas: se toman del NOMBRE del producto porque el campo "Marca Comercial"
# de algunos sitios es inconsistente (p. ej. Hialsoft figura como "Chocobreak").
# Ampliar esta lista al ver productos nuevos de Pasteur.
rx_marcas  <- regex(paste0("stoptear|toptear|systane|lagricel|hialsoft|freegen|hialtears|hylo|",
                           "humylub|tearsoft|carmelub|lagrifresh|lagrigen|refresh|optive|",
                           "vital ?vision|comfort ?vision|tears naturale|vercyt|aluvit|tonimer|gatidex"),
                    ignore_case = TRUE)

estandarizar <- function(df) {
  nombre_pa <- paste(df$nombre_original, coalesce(df$principio_activo, ""))
  texto     <- paste(nombre_pa, df$categoria_sitio)
  # contenido del envase: primero en 'presentacion', luego en el nombre.
  # Solo mL y g (los mg suelen ser concentracion, no contenido del envase).
  # (en Pasteur 'presentacion' solo dice "Caja"/"Frasco": sin numeros, se usa el nombre)
  fuente <- ifelse(!is.na(df$presentacion) & str_detect(df$presentacion, "\\d"),
                   df$presentacion, df$nombre_original)
  cont  <- str_match(str_to_lower(fuente),
                     "(\\d+(?:[.,]\\d+)?)\\s*(ml|g|gr|l)\\b(?!\\s*/)")
  valor <- as.numeric(str_replace(cont[, 2], ",", "."))
  unid  <- cont[, 3]
  factor <- case_when(unid == "l" ~ 1000, TRUE ~ 1)
  unid_b <- case_when(unid %in% c("ml", "l") ~ "mL", unid %in% c("g", "gr") ~ "g",
                      TRUE ~ NA_character_)
  vol_ml <- ifelse(unid_b == "mL", valor * factor, NA_real_)
  # promociones "15+10", "gratis", "pague 1 lleve 2": el contenido total es ambiguo
  # -> contenido queda NA y se revisa a mano (no se inventa un valor)
  ambiguo <- str_detect(str_to_lower(paste(df$nombre_original, df$presentacion)),
                        "^\\s*oferta|gratis|\\d+\\s*\\+\\s*\\d+|prepack|pague|lleve|duo ?pack|combo|\\bkit\\b|\\bx ?([2-9]|\\d{2,}) ?und")

  es_lentes  <- str_detect(texto, rx_lentes) | str_detect(df$categoria_sitio, rx_cat_lentes)
  es_higiene <- str_detect(texto, rx_higiene)
  es_oral    <- str_detect(texto, rx_oral)
  es_vol     <- !is.na(vol_ml) & vol_ml > 30     # un colirio rara vez pasa de 30 mL
  # resultados de busqueda (p. ej. "gotas oticas") solo valen si el nombre dice otico
  es_busq    <- str_detect(df$categoria_sitio, "^B[uú]squeda") & !str_detect(nombre_pa, rx_otico)
  excl <- es_lentes | es_higiene | es_oral | es_vol | es_busq

  tipo <- case_when(
    str_detect(texto, rx_otico)       ~ "otico",
    str_detect(nombre_pa, rx_lagrima) ~ "lagrima_artificial",
    TRUE                              ~ "oftalmico")

  df |> mutate(
    nombre_estandar = str_squish(str_to_lower(paste(nombre_original, coalesce(presentacion, "")))),
    marca_estandar  = str_squish(str_to_title(coalesce(
      str_extract(nombre_original, rx_marcas), marca_comercial, marca_original))),
    subcategoria    = tipo,
    # llave de cruce: registro Invima sin guiones ni sufijo de renovacion (-R1, R2)
    registro_invima_norm = str_replace(
      str_extract(str_remove_all(str_to_upper(registro_invima), "[-\\s]"),
                  "^\\d{4}[A-Z]{1,2}\\d+"),
      "^(\\d{4}[A-Z]{1,2})0+(\\d+)$", "\\1\\2"),
    contenido_valor  = ifelse(unid_b %in% c("mL", "g") & !ambiguo, valor * factor, NA_real_),
    contenido_unidad = ifelse(unid_b %in% c("mL", "g") & !ambiguo, unid_b, NA_character_),
    incluido = as.integer(!excl),
    motivo_exclusion = case_when(
      es_lentes  ~ "Solucion de lentes de contacto (excluida por delimitacion)",
      es_higiene ~ "Higiene de parpados/limpiador, no medicamento",
      es_oral    ~ "Via oral (no ocular ni otica)",
      es_vol     ~ "Volumen > 30 mL: probable solucion de lentes (revisar)",
      es_busq    ~ "Resultado de busqueda que no es otico",
      TRUE ~ NA_character_),
    # casos borde incluidos que conviene mirar a mano
    revisar_manual = as.integer(!excl & (str_detect(texto, rx_revisar) | ambiguo)))
}

# ------------------------------------------------------------------ SQLite
crear_esquema <- function(con) {
  dbExecute(con, "PRAGMA foreign_keys = ON")
  sentencias <- c(
    "CREATE TABLE IF NOT EXISTS comercios (
       id INTEGER PRIMARY KEY, nombre TEXT UNIQUE NOT NULL, url_base TEXT,
       categorias_equivalentes TEXT)",
    "CREATE TABLE IF NOT EXISTS ejecuciones (
       id TEXT PRIMARY KEY, inicio TEXT NOT NULL, comercio_id INTEGER NOT NULL,
       n_registros INTEGER, total_sitio INTEGER,
       estado TEXT CHECK (estado IN ('completa','parcial','fallida')),
       detalle TEXT, FOREIGN KEY (comercio_id) REFERENCES comercios(id))",
    # identificador comun de producto/presentacion comparable entre comercios
    "CREATE TABLE IF NOT EXISTS productos (
       id INTEGER PRIMARY KEY, nombre TEXT NOT NULL, marca TEXT,
       presentacion TEXT, contenido_valor REAL, contenido_unidad TEXT,
       num_unidades INTEGER DEFAULT 1, regla_correspondencia TEXT)",
    # cada producto tal como aparece en un comercio (texto original sin tocar)
    "CREATE TABLE IF NOT EXISTS anuncios (
       id INTEGER PRIMARY KEY, comercio_id INTEGER NOT NULL, sku TEXT NOT NULL,
       url TEXT, nombre_original TEXT, marca_original TEXT, descripcion_original TEXT,
       categoria_sitio TEXT, nombre_estandar TEXT, marca_estandar TEXT,
       subcategoria TEXT, marca_comercial TEXT, principio_activo TEXT,
       concentracion TEXT, presentacion TEXT, registro_invima TEXT,
       registro_invima_norm TEXT, contenido_valor REAL, contenido_unidad TEXT,
       incluido INTEGER NOT NULL DEFAULT 1, motivo_exclusion TEXT,
       revisar_manual INTEGER NOT NULL DEFAULT 0,
       producto_id INTEGER, comparable INTEGER NOT NULL DEFAULT 0,
       UNIQUE (comercio_id, sku),
       FOREIGN KEY (comercio_id) REFERENCES comercios(id),
       FOREIGN KEY (producto_id) REFERENCES productos(id))",
    # precio en un momento dado
    "CREATE TABLE IF NOT EXISTS observaciones (
       id INTEGER PRIMARY KEY, ejecucion_id TEXT NOT NULL, anuncio_id INTEGER NOT NULL,
       precio REAL, precio_lista REAL, promocion INTEGER, disponible INTEGER,
       fecha_consulta TEXT NOT NULL,
       UNIQUE (anuncio_id, fecha_consulta),
       FOREIGN KEY (ejecucion_id) REFERENCES ejecuciones(id),
       FOREIGN KEY (anuncio_id) REFERENCES anuncios(id))")
  walk(sentencias, ~ dbExecute(con, .x))
}

insertar_resultados <- function(con, df, comercio_id, ejecucion_id, fecha_consulta) {
  # anuncios: upsert (no toca producto_id/comparable ya asignados)
  dbExecute(con, "INSERT INTO anuncios
      (comercio_id, sku, url, nombre_original, marca_original, descripcion_original,
       categoria_sitio, nombre_estandar, marca_estandar, subcategoria,
       marca_comercial, principio_activo, concentracion, presentacion,
       registro_invima, registro_invima_norm,
       contenido_valor, contenido_unidad, incluido, motivo_exclusion, revisar_manual)
     VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
     ON CONFLICT(comercio_id, sku) DO UPDATE SET
       url = excluded.url, nombre_original = excluded.nombre_original,
       marca_original = excluded.marca_original,
       descripcion_original = excluded.descripcion_original,
       categoria_sitio = excluded.categoria_sitio,
       nombre_estandar = excluded.nombre_estandar,
       marca_estandar = excluded.marca_estandar, subcategoria = excluded.subcategoria,
       marca_comercial = excluded.marca_comercial,
       principio_activo = excluded.principio_activo,
       concentracion = excluded.concentracion, presentacion = excluded.presentacion,
       registro_invima = excluded.registro_invima,
       registro_invima_norm = excluded.registro_invima_norm,
       contenido_valor = excluded.contenido_valor,
       contenido_unidad = excluded.contenido_unidad,
       incluido = excluded.incluido, motivo_exclusion = excluded.motivo_exclusion,
       revisar_manual = excluded.revisar_manual",
    params = unname(as.list(tibble(
      comercio_id = comercio_id, df$sku, df$url, df$nombre_original,
      df$marca_original, df$descripcion_original, df$categoria_sitio,
      df$nombre_estandar, df$marca_estandar, df$subcategoria,
      df$marca_comercial, df$principio_activo, df$concentracion, df$presentacion,
      df$registro_invima, df$registro_invima_norm,
      df$contenido_valor, df$contenido_unidad, df$incluido, df$motivo_exclusion, df$revisar_manual))))

  ids <- dbGetQuery(con, "SELECT id, sku FROM anuncios WHERE comercio_id = ?",
                    params = list(comercio_id))
  df  <- left_join(df, ids, by = "sku", suffix = c("", "_db"))
  # INSERT OR IGNORE: respeta UNIQUE(anuncio_id, fecha_consulta), nunca sobrescribe
  dbExecute(con, "INSERT OR IGNORE INTO observaciones
      (ejecucion_id, anuncio_id, precio, precio_lista, promocion, disponible, fecha_consulta)
     VALUES (?,?,?,?,?,?,?)",
    params = unname(as.list(tibble(
      ejecucion_id, df$id, df$precio, df$precio_lista, df$promocion,
      df$disponible, fecha_consulta))))
}

# --------------------------------------------------- funcion general
recolectar_todo <- function(db = DB_PATH, offline = FALSE,
                            fecha_dir = format(Sys.time(), "%Y-%m-%d", tz = TZ_CO)) {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  crear_esquema(con)
  resumen <- list()
  # identificador de corrida: el mismo para los tres comercios de una llamada.
  # En modo sin conexion se deriva de la fecha de las copias crudas.
  corrida <- if (offline) paste0(gsub("-", "", fecha_dir), "T000000")
             else format(Sys.time(), "%Y%m%dT%H%M%S", tz = TZ_CO)

  for (clave in names(FUNCIONES)) {
    cfg <- COMERCIOS[[clave]]
    dbExecute(con, "INSERT OR IGNORE INTO comercios (nombre, url_base, categorias_equivalentes)
                    VALUES (?,?,?)",
              params = list(cfg$nombre, paste0("https://", cfg$host), cfg$categorias_doc))
    com_id <- dbGetQuery(con, "SELECT id FROM comercios WHERE nombre = ?",
                         params = list(cfg$nombre))$id
    ejec_id <- paste0(corrida, "_", clave)
    inicio  <- iso_ahora()

    res <- tryCatch({
      df <- FUNCIONES[[clave]](fecha_dir, offline) |> estandarizar()
      fc    <- attr(df, "fecha_consulta") %||% inicio
      total <- attr(df, "total_sitio")
      total <- if (is.null(total) || is.na(total)) NA_integer_ else as.integer(total)
      estado <- if (nrow(df) == 0) "fallida"
                else if (!is.na(total) && nrow(df) < total) "parcial"
                else "completa"
      dbExecute(con, "INSERT INTO ejecuciones
          (id, inicio, comercio_id, n_registros, total_sitio, estado, detalle)
          VALUES (?,?,?,?,?,?,?)",
          params = list(ejec_id, inicio, com_id, nrow(df), total, estado,
                        if (offline) "reprocesado desde raw/" else "en linea"))
      insertar_resultados(con, df, com_id, ejec_id, fc)
      tibble(comercio = cfg$nombre, extraidos = nrow(df), sitio = total, estado = estado)
    }, error = function(e) {
      dbExecute(con, "INSERT INTO ejecuciones
          (id, inicio, comercio_id, n_registros, estado, detalle) VALUES (?,?,?,?,?,?)",
          params = list(ejec_id, inicio, com_id, 0L, "fallida", conditionMessage(e)))
      tibble(comercio = cfg$nombre, extraidos = 0L, sitio = NA_integer_, estado = "fallida")
    })
    resumen[[clave]] <- res
  }
  bind_rows(resumen)
}

# --------------------------- correspondencia de productos comparables (manual)
# Tras la primera extraccion, revisa los anuncios y registra la correspondencia:
#   registrar_comparable("Systane Ultra 10 mL", "Alcon", 10, "mL",
#                        skus = c(farmatodo = "123", colsubsidio = "SKU-9", pasteur = "777"),
#                        regla = "Misma marca, variante y contenido; 1 unidad")
registrar_comparable <- function(nombre, marca, contenido, unidad, skus, regla,
                                 num_unidades = 1L, db = DB_PATH) {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  dbExecute(con, "INSERT INTO productos
      (nombre, marca, presentacion, contenido_valor, contenido_unidad, num_unidades, regla_correspondencia)
      VALUES (?,?,?,?,?,?,?)",
      params = list(nombre, marca, paste(contenido, unidad), contenido, unidad,
                    num_unidades, regla))
  pid <- dbGetQuery(con, "SELECT last_insert_rowid() id")$id
  for (clave in names(skus)) {
    dbExecute(con, "UPDATE anuncios SET producto_id = ?, comparable = 1
                    WHERE sku = ? AND comercio_id =
                    (SELECT id FROM comercios WHERE nombre = ?)",
              params = list(pid, skus[[clave]], COMERCIOS[[clave]]$nombre))
  }
  invisible(pid)
}

# ---------------- correspondencia por reglas documentadas (productos comparables)
# Farmatodo no publica el registro Invima (su campo 'brand' es un codigo fijo), asi que
# la correspondencia entre los tres comercios se hace por reglas sobre el nombre:
#   mismo producto y variante (patron sobre nombre_estandar), mismo contenido en mL,
#   una sola unidad (las ofertas/combos tienen contenido NA y no entran) y
#   exactamente UN anuncio incluido por comercio. Si una regla no cumple, se informa
#   y ese producto NO se marca como comparable.
# 'patron' y 'excluir' son expresiones regulares, sin distinguir mayusculas.
PRODUCTOS_COMUNES <- tibble::tribble(
  ~nombre,                       ~marca,       ~contenido, ~patron,             ~excluir,
  "Systane Ultra 10 mL",         "Systane",    10, "systane ultra",             NA_character_,
  "Systane Balance 10 mL",       "Systane",    10, "systane balance",           NA_character_,
  "Systane Complete 10 mL",      "Systane",    10, "systane complete",          NA_character_,
  "Systane HA 10 mL",            "Systane",    10, "systane ha\\b",             "\\bsp\\b",
  "Hylo Dual 10 mL",             "Hylo",       10, "hylo[- ]dual",              NA_character_,
  "Hylo Gel 10 mL",              "Hylo",       10, "hylo[- ]gel",               NA_character_,
  "Hylo Comod 10 mL",            "Hylo",       10, "hylo[- ]comod",             NA_character_,
  "Hialsoft 0.15% 10 mL",        "Hialsoft",   10, "hialsoft",                  NA_character_,
  "Lagricel Ofteno 0.4% 10 mL",  "Lagricel",   10, "lagricel",                  NA_character_,
  "Freegen PF 0.5% 10 mL",       "Freegen",    10, "freegen pf",                NA_character_,
  "Freegen PF 0.5% 15 mL",       "Freegen",    15, "freegen pf",                NA_character_,
  "Cristaltears Plus 10 mL",     "Cristaltears", 10, "cristaltears plus",       NA_character_,
  "Glicolub Ultra 10 mL",        "Glicolub",   10, "glicolub ultra",            NA_character_,
  "Optive Fusion 10 mL",         "Optive",     10, "optive fusion",             NA_character_,
  "Optive Fusion 15 mL",         "Optive",     15, "optive fusion",             NA_character_
)

# Aplica las reglas sobre la base y marca comparable = 1 y producto_id en cada anuncio.
# Devuelve un informe: una fila por producto con cuantos comercios y anuncios encontro.
asignar_comparables <- function(db = DB_PATH, definiciones = PRODUCTOS_COMUNES) {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE anuncios SET producto_id = NULL, comparable = 0")
  dbExecute(con, "DELETE FROM productos")
  n_com <- dbGetQuery(con, "SELECT COUNT(*) n FROM comercios")$n
  an <- dbGetQuery(con, "
    SELECT a.id, a.comercio_id, a.nombre_estandar, a.contenido_valor, a.contenido_unidad
    FROM anuncios a
    WHERE a.incluido = 1 AND a.contenido_valor IS NOT NULL AND a.contenido_unidad = 'mL'")

  informe <- vector("list", nrow(definiciones))
  for (i in seq_len(nrow(definiciones))) {
    d <- definiciones[i, ]
    cand <- an[abs(an$contenido_valor - d$contenido) < 1e-9 &
               str_detect(an$nombre_estandar, regex(d$patron, ignore_case = TRUE)), ]
    if (!is.na(d$excluir))
      cand <- cand[!str_detect(cand$nombre_estandar, regex(d$excluir, ignore_case = TRUE)), ]
    por_com <- table(cand$comercio_id)
    ok <- length(por_com) == n_com && all(por_com == 1)
    informe[[i]] <- tibble(producto = d$nombre, comercios = length(por_com),
                           anuncios = nrow(cand), ok = ok)
    if (!ok) next
    dbExecute(con, "INSERT INTO productos
        (nombre, marca, presentacion, contenido_valor, contenido_unidad, num_unidades,
         regla_correspondencia) VALUES (?,?,?,?,?,1,?)",
      params = list(d$nombre, d$marca, paste(d$contenido, "mL"), d$contenido, "mL",
                    sprintf("nombre ~ /%s/ excluyendo /%s/; contenido %s mL; 1 unidad; 1 anuncio por comercio",
                            d$patron, d$excluir, d$contenido)))
    pid <- dbGetQuery(con, "SELECT last_insert_rowid() id")$id
    for (aid in cand$id)
      dbExecute(con, "UPDATE anuncios SET producto_id = ?, comparable = 1 WHERE id = ?",
                params = list(pid, aid))
  }
  bind_rows(informe)
}
