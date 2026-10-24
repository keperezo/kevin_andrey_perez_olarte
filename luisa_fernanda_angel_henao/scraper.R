# =============================================================================
# scraper.R - Módulo de extracción (Taller 1, Minería de Datos, UNAL)
# Categoría: Maquillaje y Cosméticos
# Comercios: Farmatodo (API Algolia), Éxito (API VTEX), Cromantic (API VTEX)
#
# Uso:
#   source("scraper.R")
#   ejecutar_scraping()                                   # en línea (consulta los sitios)
#   ejecutar_scraping("farmatodo", ejecucion_id = "20261009T235308")  # repite un comercio en una ejecución existente
#   ejecutar_scraping(modo = "offline",                   # sin conexión: reprocesa raw/
#                     ejecucion_id = "20261008T101500",   # (si se omite, usa la más reciente)
#                     ruta_db = "reprocesada.sqlite")     # conviene usar otra base
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
})

CFG <- list(
  ruta_db    = "comparador_precios.sqlite",
  dir_raw    = "raw",
  pausa      = 2,      # segundos entre peticiones
  timeout    = 60,
  zona       = "America/Bogota",
  por_pagina_algolia = 100L,
  tope_algolia = 80L,          # el índice de Farmatodo corta cada consulta en 80 resultados
  pausa_algolia = 0.3,         # pausa corta entre consultas a Algolia
  max_consultas_algolia = 1500L,
  lote_vtex  = 50L,    # VTEX permite como máximo 50 productos por petición
  max_vtex   = 2500L,  # y no entrega más de 2500 resultados por búsqueda
  # Clave de solo búsqueda que el propio sitio de Farmatodo envía desde el navegador
  algolia_key = "5e0e7a2b90bfae7e6008bf50438b4d05",
  algolia_app = "7S224P4F90",
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

# ---- Funciones auxiliares ----------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- x[[1]]
  if (is.null(v) || (length(v) == 1 && is.na(v)) || !nzchar(as.character(v))) return(NA_character_)
  as.character(v)
}

num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(v)) NA_real_ else v
}

# Un precio de 0 o negativo no es un precio: se deja como ausente (NA -> NULL en SQLite)
pos <- function(x) if (is.na(x) || x <= 0) NA_real_ else x

ahora_iso <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = CFG$zona)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", x)
}

COLS <- c("comercio", "producto_id", "sku_comercio", "ean", "referencia",
          "nombre_original", "marca_original", "descripcion_original",
          "categoria_origen", "categoria_consulta", "url",
          "precio", "precio_lista", "en_promocion", "disponible", "moneda",
          "texto_original", "fecha_hora")

# Petición con reintentos (errores de red o respuestas 5xx)
con_reintentos <- function(f, intentos = 3) {
  for (i in seq_len(intentos)) {
    res <- tryCatch(f(), error = function(e) e)
    if (!inherits(res, "error") && status_code(res) < 500) return(res)
    Sys.sleep(CFG$pausa * i)
  }
  if (inherits(res, "error")) stop(conditionMessage(res))
  res
}

# ---- Copias crudas (raw/) ----------------------------------------------------
dir_raw_exec <- function(comercio, ctx) file.path(CFG$dir_raw, comercio, ctx$ejecucion_id)

guardar_raw <- function(texto, archivo) {
  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
  con <- gzfile(archivo, "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(texto)), con)
}

leer_gz <- function(archivo) {
  con <- gzfile(archivo, "rb")
  on.exit(close(con))
  paste(readLines(con, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# meta.json guarda lo que no está en las páginas: hora de consulta, total del sitio y si se llegó al final
guardar_meta <- function(comercio, ctx, fecha_hora, total_sitio, fin_natural) {
  meta <- list(fecha_hora = fecha_hora,
               total_sitio = if (is.na(total_sitio)) NULL else total_sitio,
               fin_natural = fin_natural)
  dir.create(dir_raw_exec(comercio, ctx), recursive = TRUE, showWarnings = FALSE)
  write_json(meta, file.path(dir_raw_exec(comercio, ctx), "meta.json"), auto_unbox = TRUE, null = "null")
}

leer_raw <- function(comercio, ctx) {
  dir <- dir_raw_exec(comercio, ctx)
  archivos <- sort(list.files(dir, pattern = "^pag_.*\\.json\\.gz$", full.names = TRUE))
  if (length(archivos) == 0) stop("No hay copias crudas en ", dir)
  meta <- fromJSON(file.path(dir, "meta.json"))
  list(paginas     = vapply(archivos, leer_gz, character(1), USE.NAMES = FALSE),
       fecha_hora  = meta$fecha_hora,
       total_sitio = suppressWarnings(as.integer(meta$total_sitio %||% NA)),
       fin_natural = isTRUE(meta$fin_natural))
}

# =============================================================================
# 1. FARMATODO (API Algolia)
#    El índice corta cada consulta en 80 resultados, así que se consultan varias palabras
#    de la planilla y las consultas que llegan al tope se parten por marca/categoría/precio.
#    Filtros usados por el propio sitio: solo con stock y sin muestras.
# =============================================================================
# Palabras de la categoría de la planilla (bases, correctores, polvos, rubores, sombras,
# pestañinas y labiales). Cada una se consulta por separado y los resultados se unen.
FARMATODO_TERMINOS <- c("labial", "tinta de labios", "base", "corrector", "polvo",
                        "rubor", "colorete", "sombra", "pestanina", "mascara de pestanas",
                        "maquillaje")

# Una consulta a Algolia. 'ff' son filtros de faceta tipo "marca:Maybelline" (se combinan con Y).
algolia_pedir <- function(q, ff = character(0), pagina = 0L) {
  url <- "https://api-search.farmatodo.com/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20JavaScript%20(4.26.0)%3B%20Browser"
  # Los filtros por marca/categoría solo se aplican si van dentro de 'filters'
  # (facetFilters y numericFilters son ignorados por este índice).
  filtro <- "outofstore:false  AND NOT rms_class:SAMPLING"
  for (x in ff) {
    dim <- sub(":.*$", "", x)
    val <- sub("^[^:]*:", "", x)
    val_q <- if (grepl("^[0-9.]+$", val)) val else paste0('"', gsub('"', '', val), '"')
    filtro <- paste0(filtro, " AND ", dim, ":", val_q)
  }
  params <- paste0("hitsPerPage=", CFG$por_pagina_algolia,
                   "&filters=", utils::URLencode(filtro, reserved = TRUE),
                   "&facets=marca%2Ccategorie%2CsubCategory%2CfullPrice&page=", pagina)
  payload <- as.character(toJSON(
    list(requests = list(list(query = q, indexName = "products-colombia", params = params))),
    auto_unbox = TRUE))

  resp <- con_reintentos(function()
    POST(url, body = payload, encode = "raw",
         add_headers(`Content-Type` = "application/x-www-form-urlencoded",
                     `x-algolia-api-key` = CFG$algolia_key,
                     `x-algolia-application-id` = CFG$algolia_app,
                     `User-Agent` = CFG$user_agent),
         timeout(CFG$timeout)))
  if (status_code(resp) >= 400) return(NULL)
  txt <- content(resp, "text", encoding = "UTF-8")
  r <- tryCatch(fromJSON(txt, simplifyVector = FALSE)$results[[1]], error = function(e) NULL)
  if (is.null(r)) return(NULL)
  list(txt = txt, res = r)
}

# Si una consulta devuelve el tope (80), se parte por marca, luego categoría, subcategoría y
# precio, hasta que cada trozo quede por debajo del tope. Si no es posible, se marca como truncado.
barrer_farmatodo <- function(q, ff, dims, st, ctx) {
  if (st$consultas >= CFG$max_consultas_algolia) { st$truncado <- TRUE; return(invisible()) }
  r <- algolia_pedir(q, ff)
  st$consultas <- st$consultas + 1L
  Sys.sleep(CFG$pausa_algolia)
  if (is.null(r)) { st$error <- TRUE; return(invisible()) }
  if (st$consultas %% 25L == 0L) message("      ... ", st$consultas, " consultas, ", st$n, " respuestas guardadas")

  nb <- r$res$nbHits
  if (is.null(nb) || nb == 0) return(invisible())

  st$n <- st$n + 1L
  guardar_raw(r$txt, file.path(dir_raw_exec("farmatodo", ctx), sprintf("pag_%04d.json.gz", st$n)))
  st$paginas <- c(st$paginas, r$txt)

  if (nb < CFG$tope_algolia) return(invisible())       # trozo completo

  while (length(dims) > 0) {
    dim <- dims[1]
    dims <- dims[-1]
    vals <- names(r$res$facets[[dim]])
    if (length(vals) > 0) {
      for (v in vals) barrer_farmatodo(q, c(ff, paste0(dim, ":", v)), dims, st, ctx)
      return(invisible())
    }
  }
  st$truncado <- TRUE                                   # no se pudo partir más
}

capturar_farmatodo <- function(ctx) {
  fecha_hora <- ahora_iso()
  st <- new.env()
  st$paginas <- character(0); st$n <- 0L; st$consultas <- 0L
  st$truncado <- FALSE; st$error <- FALSE

  for (q in FARMATODO_TERMINOS) {
    message("   Farmatodo: '", q, "' ...")
    barrer_farmatodo(q, character(0), c("marca", "categorie", "subCategory", "fullPrice"), st, ctx)
  }
  if (st$n == 0L) stop("Algolia no devolvió resultados")

  fin_natural <- !st$truncado && !st$error
  guardar_meta("farmatodo", ctx, fecha_hora, NA_integer_, fin_natural)
  list(paginas = st$paginas, fecha_hora = fecha_hora, total_sitio = NA_integer_,
       fin_natural = fin_natural)
}

parse_farmatodo <- function(cap) {
  filas <- list()
  for (txt in cap$paginas) {
    hits <- fromJSON(txt, simplifyVector = FALSE)$results[[1]]$hits
    for (h in hits) {
      pr <- pos(num(h$fullPrice))
      po <- pos(num(h$offerPrice))
      if (is.na(po)) po <- pr

      nom <- chr(h$mediaDescription)
      if (is.na(nom)) nom <- chr(h$largeDescription)
      if (is.na(nom)) nom <- "Sin nombre"

      mar <- chr(h$marca)
      if (is.na(mar)) mar <- chr(h$brand)
      if (is.na(mar)) mar <- "Sin marca"

      filas[[length(filas) + 1]] <- data.frame(
        comercio = "farmatodo",
        producto_id = chr(h$objectID),
        sku_comercio = chr(h$objectID),
        ean = chr(h$barcode),
        referencia = chr(h$item),
        nombre_original = nom,
        marca_original = mar,
        descripcion_original = nom,
        categoria_origen = chr(h$categorie),
        categoria_consulta = "maquillaje",
        url = paste0("https://www.farmatodo.com.co/producto/", chr(h$objectID)),
        precio = po,
        precio_lista = pr,
        en_promocion = as.integer(!is.na(po) && !is.na(pr) && po < pr),
        disponible = 1L,
        moneda = "COP",
        texto_original = nom,
        fecha_hora = cap$fecha_hora,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(filas) == 0) {
    return(list(datos = NULL, total_sitio = cap$total_sitio, completo = FALSE,
                notas = "Algolia no devolvió productos"))
  }
  df <- do.call(rbind, filas)
  df <- df[!duplicated(df$sku_comercio), ]

  # El sitio no informa un total para la categoría; se considera completa si todos los trozos
  # de la búsqueda quedaron por debajo del tope de 80 resultados y no hubo errores.
  completo <- cap$fin_natural
  list(datos = df, total_sitio = cap$total_sitio, completo = completo,
       notas = sprintf("Algolia: %d productos únicos en %d consultas (términos: %s); el sitio no reporta total de la categoría%s",
                       nrow(df), length(cap$paginas), paste(FARMATODO_TERMINOS, collapse = ", "),
                       if (!completo) ". Recolección parcial (trozos que alcanzaron el tope o error)" else ""))
}

scrape_farmatodo <- function(ctx) {
  cap <- if (ctx$modo == "offline") leer_raw("farmatodo", ctx) else capturar_farmatodo(ctx)
  parse_farmatodo(cap)
}

# =============================================================================
# 2 y 3. ÉXITO y CROMANTIC (API de catálogo VTEX)
#    Búsqueda de texto "maquillaje", paginada de 50 en 50 con _from/_to.
#    El encabezado "resources" (p. ej. "0-49/1234") informa el total del sitio.
#    Se genera una fila por SKU, porque cada tono o tamaño es una presentación.
# =============================================================================
capturar_vtex <- function(ctx, comercio, base_url, consultas = "ft=maquillaje") {
  fecha_hora <- ahora_iso()
  paginas <- character(0)
  total <- 0L
  total_conocido <- TRUE
  fin_natural <- TRUE

  # Cada elemento de 'consultas' es un filtro VTEX (p. ej. "ft=maquillaje" o "fq=C:/1/2/").
  # Se piden páginas hasta alcanzar el total que reporta el sitio para ESA consulta;
  # una página corta no se toma como final, porque VTEX a veces devuelve menos de 50.
  for (consulta in consultas) {
    desde <- 0L
    total_q <- NA_integer_
    repeat {
      url <- sprintf("%s/api/catalog_system/pub/products/search?%s&_from=%d&_to=%d",
                     base_url, consulta, desde, desde + CFG$lote_vtex - 1L)
      res <- con_reintentos(function()
        GET(url, user_agent(CFG$user_agent),
            add_headers(`Accept` = "application/json", `Accept-Language` = "es-CO,es;q=0.9"),
            timeout(CFG$timeout)))

      if (status_code(res) >= 400) {
        if (length(paginas) == 0) stop("VTEX respondió HTTP ", status_code(res), " en ", base_url)
        fin_natural <- FALSE
        break
      }

      if (is.na(total_q)) {
        rs <- headers(res)[["resources"]]
        if (!is.null(rs)) total_q <- suppressWarnings(as.integer(sub(".*/", "", rs)))
      }

      txt <- content(res, "text", encoding = "UTF-8")
      lote <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(lote) || length(lote) == 0) {
        # Respuesta vacía: fin de la consulta; es anómalo si faltaba por traer
        if (!is.na(total_q) && desde < total_q) fin_natural <- FALSE
        break
      }

      guardar_raw(txt, file.path(dir_raw_exec(comercio, ctx),
                                 sprintf("pag_%03d.json.gz", length(paginas) + 1L)))
      paginas <- c(paginas, txt)

      desde <- desde + CFG$lote_vtex
      if (!is.na(total_q) && desde >= total_q) break                         # ya se pidió todo
      if (is.na(total_q) && length(lote) < CFG$lote_vtex) break              # sin total: página corta
      if (desde >= CFG$max_vtex) { fin_natural <- FALSE; break }             # tope de VTEX
      Sys.sleep(CFG$pausa)
    }
    if (is.na(total_q)) total_conocido <- FALSE else total <- total + total_q
    Sys.sleep(CFG$pausa)
  }

  total <- if (total_conocido) total else NA_integer_
  guardar_meta(comercio, ctx, fecha_hora, total, fin_natural)
  list(paginas = paginas, fecha_hora = fecha_hora, total_sitio = total, fin_natural = fin_natural)
}

parse_vtex <- function(cap, comercio) {
  filas <- list()
  ids_prod <- character(0)

  for (txt in cap$paginas) {
    for (p in fromJSON(txt, simplifyVector = FALSE)) {
      ids_prod <- c(ids_prod, chr(p$productId))
      for (it in p$items) {
        # Se toma el primer vendedor con stock; si ninguno tiene, el primero
        vend <- NULL
        for (s in it$sellers) {
          q <- num(s$commertialOffer$AvailableQuantity)
          if (!is.na(q) && q > 0) { vend <- s; break }
        }
        if (is.null(vend) && length(it$sellers) > 0) vend <- it$sellers[[1]]
        of <- if (is.null(vend)) list() else vend$commertialOffer

        pr <- pos(num(of$Price))
        pl <- pos(num(of$ListPrice))
        if (is.na(pl)) pl <- pr
        q  <- num(of$AvailableQuantity)

        sku <- chr(it$itemId)
        if (is.na(sku)) sku <- chr(p$productId)

        nombre <- chr(it$nameComplete)
        if (is.na(nombre)) nombre <- chr(p$productName)
        desc <- chr(p$description)

        filas[[length(filas) + 1]] <- data.frame(
          comercio = comercio,
          producto_id = chr(p$productId),
          sku_comercio = sku,
          ean = chr(it$ean),
          referencia = chr(p$productReference),
          nombre_original = nombre,
          marca_original = chr(p$brand),
          descripcion_original = desc,
          categoria_origen = chr(paste(unlist(p$categories), collapse = " || ")),
          categoria_consulta = "maquillaje",
          url = chr(p$link),
          precio = pr,
          precio_lista = pl,
          en_promocion = as.integer(!is.na(pl) && !is.na(pr) && pl > pr),
          disponible = if (is.na(q)) NA_integer_ else as.integer(q > 0),
          moneda = "COP",
          texto_original = paste(c(nombre, desc), collapse = " | "),
          fecha_hora = cap$fecha_hora,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(filas) == 0) {
    return(list(datos = NULL, total_sitio = cap$total_sitio, completo = FALSE,
                notas = "VTEX no devolvió productos"))
  }
  df <- do.call(rbind, filas)
  df <- df[!duplicated(df$sku_comercio), ]
  n_prod <- length(unique(ids_prod))

  # Completa = se pidieron todas las páginas y se recuperó al menos el 90 % de los productos
  # que reporta el sitio. El total de VTEX no siempre coincide exacto con los productos
  # devueltos (p. ej. productos sin ficha completa), por eso se reporta la proporción.
  prop <- if (is.na(cap$total_sitio) || cap$total_sitio == 0) NA_real_ else n_prod / cap$total_sitio
  completo <- cap$fin_natural && (is.na(prop) || prop >= 0.9)
  list(datos = df, total_sitio = cap$total_sitio, completo = completo,
       notas = sprintf("VTEX: %d productos (%d SKU) en %d páginas; total que reporta el sitio: %s (%s)%s",
                       n_prod, nrow(df), length(cap$paginas), format(cap$total_sitio),
                       if (is.na(prop)) "sin proporción" else sprintf("%.0f%% recuperado", 100 * prop),
                       if (!completo) ". Recolección parcial (tope de 2500 de VTEX, error o faltantes)" else ""))
}

# Éxito: la búsqueda de texto "maquillaje" mezcla disfraces, juguetes y hogar, así que se consultan
# categorías del árbol del sitio (/api/catalog_system/pub/category/tree/4). La ruta de ids
# 346098589 (Belleza) / 346098591 (Maquillaje y cosméticos) es la que acepta el filtro fq=C:.
# Cada consulta devuelve como máximo 2500 productos, por eso se pide por subcategoría.
# Se incluyen solo las subcategorías de la planilla (bases, correctores, polvos, rubores,
# sombras, pestañinas y labiales). Quedan fuera: Uñas, Delineador y lápices de ojos y
# los accesorios de maquillaje, que la planilla no menciona.
#   Labios (346098598): 2338 | Pestañina: 803 | Sombras: 1329
#   Base y correctores: 1612 | Polvo: 1051 | Rubor: 601   (totales reportados por el sitio)
EXITO_CONSULTAS <- c(
  "fq=C:/346098589/346098591/346098598/",
  "fq=C:/346098589/346098591/346098597/346098614/",
  "fq=C:/346098589/346098591/346098597/346098615/",
  "fq=C:/346098589/346098591/346098599/346098619/",
  "fq=C:/346098589/346098591/346098599/346098620/",
  "fq=C:/346098589/346098591/346098599/346098621/"
)

scrape_exito <- function(ctx) {
  cap <- if (ctx$modo == "offline") leer_raw("exito", ctx)
         else capturar_vtex(ctx, "exito", "https://www.exito.com", consultas = EXITO_CONSULTAS)
  parse_vtex(cap, "exito")
}

scrape_cromantic <- function(ctx) {
  cap <- if (ctx$modo == "offline") leer_raw("cromantic", ctx)
         else capturar_vtex(ctx, "cromantic", "https://www.cromantic.com")
  parse_vtex(cap, "cromantic")
}

SCRAPERS <- list(farmatodo = scrape_farmatodo, exito = scrape_exito, cromantic = scrape_cromantic)

COMERCIOS <- data.frame(
  comercio = c("farmatodo", "exito", "cromantic"),
  nombre   = c("Farmatodo Colombia", "Almacenes Éxito", "Cromantic"),
  base_url = c("https://www.farmatodo.com.co", "https://www.exito.com", "https://www.cromantic.com"),
  stringsAsFactors = FALSE
)

# ---- BASE DE DATOS -----------------------------------------------------------
crear_esquema <- function(con) {
  dbExecute(con, "PRAGMA foreign_keys = ON")
  sentencias <- c(
    "CREATE TABLE IF NOT EXISTS comercios (comercio TEXT PRIMARY KEY, nombre TEXT, base_url TEXT)",
    "CREATE TABLE IF NOT EXISTS ejecuciones (ejecucion_id TEXT NOT NULL, comercio TEXT NOT NULL REFERENCES comercios(comercio), inicio TEXT NOT NULL, fin TEXT, modo TEXT, n_registros INTEGER, n_productos INTEGER, total_sitio INTEGER, estado TEXT, notas TEXT, PRIMARY KEY (ejecucion_id, comercio))",
    "CREATE TABLE IF NOT EXISTS anuncios (anuncio_id INTEGER PRIMARY KEY AUTOINCREMENT, comercio TEXT NOT NULL REFERENCES comercios(comercio), producto_id TEXT, sku_comercio TEXT NOT NULL, ean TEXT, referencia TEXT, nombre_original TEXT, marca_original TEXT, descripcion_original TEXT, categoria_origen TEXT, categoria_consulta TEXT, url TEXT, texto_original TEXT, primera_ejecucion TEXT, UNIQUE (comercio, sku_comercio))",
    "CREATE TABLE IF NOT EXISTS observaciones (obs_id INTEGER PRIMARY KEY AUTOINCREMENT, ejecucion_id TEXT NOT NULL, comercio TEXT NOT NULL, anuncio_id INTEGER NOT NULL REFERENCES anuncios(anuncio_id), fecha_hora TEXT NOT NULL, precio REAL, precio_lista REAL, en_promocion INTEGER, disponible INTEGER, moneda TEXT DEFAULT 'COP', url TEXT, UNIQUE (comercio, anuncio_id, fecha_hora), FOREIGN KEY (ejecucion_id, comercio) REFERENCES ejecuciones(ejecucion_id, comercio))"
  )
  for (s in sentencias) dbExecute(con, s)
  dbExecute(con, "INSERT OR IGNORE INTO comercios (comercio, nombre, base_url) VALUES (?,?,?)",
            params = unname(as.list(COMERCIOS)))
}

# Inserta anuncios y observaciones. La fila de la ejecución ya debe existir (ver ejecutar_scraping).
# INSERT OR IGNORE + restricciones UNIQUE: nunca se sobrescriben observaciones anteriores.
guardar_resultados <- function(con, datos, ctx, comercio) {
  datos <- datos[, COLS]
  dbWithTransaction(con, {
    a <- unique(datos[, c("comercio", "producto_id", "sku_comercio", "ean", "referencia",
                          "nombre_original", "marca_original", "descripcion_original",
                          "categoria_origen", "categoria_consulta", "url", "texto_original")])
    a$primera_ejecucion <- ctx$ejecucion_id
    dbExecute(con, "INSERT OR IGNORE INTO anuncios (comercio, producto_id, sku_comercio, ean, referencia, nombre_original, marca_original, descripcion_original, categoria_origen, categoria_consulta, url, texto_original, primera_ejecucion) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
              params = unname(as.list(a)))

    ids <- dbGetQuery(con, "SELECT anuncio_id, comercio, sku_comercio FROM anuncios WHERE comercio = ?",
                      params = list(comercio))
    o <- merge(datos, ids, by = c("comercio", "sku_comercio"))
    o$ejecucion_id <- ctx$ejecucion_id

    dbExecute(con, "INSERT OR IGNORE INTO observaciones (ejecucion_id, comercio, anuncio_id, fecha_hora, precio, precio_lista, en_promocion, disponible, moneda, url) VALUES (?,?,?,?,?,?,?,?,?,?)",
              params = unname(as.list(o[, c("ejecucion_id", "comercio", "anuncio_id", "fecha_hora",
                                            "precio", "precio_lista", "en_promocion", "disponible",
                                            "moneda", "url")])))
  })
}

registrar_ejecucion <- function(con, ctx, comercio, inicio, fin, n, n_prod, total, estado, notas) {
  dbExecute(con, "INSERT INTO ejecuciones (ejecucion_id, comercio, inicio, fin, modo, n_registros, n_productos, total_sitio, estado, notas) VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT(ejecucion_id, comercio) DO UPDATE SET fin = excluded.fin, modo = excluded.modo, n_registros = excluded.n_registros, n_productos = excluded.n_productos, total_sitio = excluded.total_sitio, estado = excluded.estado, notas = excluded.notas",
            params = list(ctx$ejecucion_id, comercio, inicio, fin, ctx$modo,
                          as.integer(n), as.integer(n_prod), as.integer(total), estado, notas))
}

ultima_ejecucion_raw <- function(comercios) {
  d <- sort(list.dirs(file.path(CFG$dir_raw, comercios[1]), full.names = FALSE, recursive = FALSE))
  if (length(d) == 0) stop("No hay copias crudas en ", CFG$dir_raw)
  tail(d, 1)
}

# ---- FUNCIÓN GENERAL ---------------------------------------------------------
# modo = "online"  : consulta los sitios, guarda raw/ y carga SQLite.
# modo = "offline" : no consulta nada; reprocesa raw/<comercio>/<ejecucion_id>/.
ejecutar_scraping <- function(comercios = names(SCRAPERS), ruta_db = CFG$ruta_db,
                              modo = c("online", "offline"), ejecucion_id = NULL) {
  modo <- match.arg(modo)
  reutilizar <- FALSE
  if (modo == "online") {
    if (is.null(ejecucion_id)) {
      ejecucion_id <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = CFG$zona)
    } else {
      reutilizar <- TRUE   # repetir comercios dentro de una ejecución ya existente
    }
  } else if (is.null(ejecucion_id)) {
    ejecucion_id <- ultima_ejecucion_raw(comercios)
  }
  ctx <- list(ejecucion_id = ejecucion_id, modo = modo)

  con <- dbConnect(SQLite(), ruta_db)
  on.exit(dbDisconnect(con), add = TRUE)
  crear_esquema(con)

  resumen <- list()
  for (nm in comercios) {
    message(sprintf("[%s | %s] Procesando %s ...", ejecucion_id, modo, nm))
    inicio <- ahora_iso()
    if (reutilizar) {
      # Se descarta lo anterior de ese comercio en esa ejecución, para no duplicar observaciones
      dbExecute(con, "DELETE FROM observaciones WHERE ejecucion_id = ? AND comercio = ?",
                params = list(ejecucion_id, nm))
      unlink(dir_raw_exec(nm, ctx), recursive = TRUE)
    }
    # La ejecución se registra de entrada como 'parcial'; se actualiza al terminar
    registrar_ejecucion(con, ctx, nm, inicio, NA, 0L, 0L, NA, "parcial", "en curso")

    res <- tryCatch(SCRAPERS[[nm]](ctx), error = function(e) {
      list(datos = NULL, total_sitio = NA, completo = FALSE,
           notas = paste("ERROR:", conditionMessage(e)))
    })

    datos <- res$datos
    n <- if (is.null(datos)) 0L else nrow(datos)

    if (n > 0) {
      ok <- tryCatch({ guardar_resultados(con, datos, ctx, nm); TRUE },
                     error = function(e) { res$notas <<- paste("ERROR al guardar:", conditionMessage(e)); FALSE })
      if (!ok) n <- 0L
    }
    estado <- if (n == 0) "fallida" else if (isTRUE(res$completo)) "completa" else "parcial"

    registrar_ejecucion(con, ctx, nm, inicio, ahora_iso(), n,
                        if (n > 0) length(unique(datos$sku_comercio)) else 0L,
                        res$total_sitio, estado, res$notas)
    resumen[[nm]] <- data.frame(ejecucion = ejecucion_id, comercio = nm, registros = n,
                                total_sitio = as.integer(res$total_sitio %||% NA),
                                estado = estado, stringsAsFactors = FALSE)
    if (modo == "online" && nm != tail(comercios, 1)) Sys.sleep(CFG$pausa)
  }
  do.call(rbind, resumen)
}
