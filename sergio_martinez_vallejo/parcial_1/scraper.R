# =============================================================================
# scraper.R — Taller 1, Minería de Datos (UNAL)
# Categoría: papel higiénico, pañuelos, servilletas, toallas de cocina,
#            vasos, platos y cubiertos desechables.
# Comercios: Éxito, Carulla y Olímpica (los tres sobre la plataforma VTEX).
#
# Uso desde taller_1.qmd:
#   source("scraper.R", encoding = "UTF-8")
#   ejecutar_recoleccion()                                   # en línea
#   ejecutar_recoleccion(offline = TRUE,
#                        id_ejecucion = "20261011T101500")   # desde raw/
# =============================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(DBI); library(RSQLite)
})

UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)
TZ_CO   <- "America/Bogota"
DIR_RAW <- "raw"

# Rutas de categoría equivalentes a la delimitación de la planilla.
# Para revisarlas o agregar otras: listar_categorias_vtex("exito", "papel|servillet|toalla|pañuel|desechab")
TIENDAS <- list(
  exito = list(
    comercio = "\u00c9xito",
    base     = "https://www.exito.com",
    rutas    = c(
      "mercado/aseo-del-hogar/papel-higienico",
      "mercado/aseo-del-hogar/servilletas-y-toallas-de-cocina",
      "mercado/aseo-del-hogar/desechables-velas-y-carbon",
      "cuidado-personal/panitos-y-panuelos/panuelos-desechables",
      "id:346100590"                  # Desechables para fiesta (ruta resuelta desde el árbol)
    )
  ),
  carulla = list(
    comercio = "Carulla",
    base     = "https://www.carulla.com",
    rutas    = c(
      "aseo-del-hogar/papel-higienico",
      "aseo-del-hogar/servilletas-papeles-y-toallas-de-cocina/servilletas",
      "aseo-del-hogar/servilletas-papeles-y-toallas-de-cocina/toallas-de-cocina",
      "aseo-del-hogar/desechables-velas-y-carbon/desechables",
      "fiesta/desechables"            # vasos, platos y cubiertos desechables
    )
  ),
  olimpica = list(
    comercio = "Ol\u00edmpica",
    base     = "https://www.olimpica.com",
    rutas    = c(
      "supermercado/aseo-del-hogar/papeles-desechables",
      "supermercado/aseo-del-hogar/desechables"
    )
  )
)

# ---- Utilidades -------------------------------------------------------------

# Fecha y hora ISO 8601 en hora de Colombia: 2026-10-11T10:15:00-05:00
ahora_iso <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = TZ_CO)
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", x)
}

# Petición HTTP con user agent, reintentos y como máximo 1 petición por segundo
pedir <- function(url) {
  request(url) |>
    req_user_agent(UA) |>
    req_headers(`accept-language` = "es-CO,es;q=0.9") |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_throttle(rate = 1) |>
    req_perform()
}

# Cada página cruda se guarda como lista: url, ruta, fecha_hora_consulta, cuerpo, total
guardar_raw <- function(pagina, carpeta, id_ejecucion, nombre) {
  dir <- file.path(DIR_RAW, carpeta, id_ejecucion)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  con <- gzfile(file.path(dir, paste0(nombre, ".json.gz")), "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(toJSON(pagina, auto_unbox = TRUE, null = "null", na = "null"), con)
}

leer_raw <- function(carpeta, id_ejecucion) {
  archivos <- sort(list.files(file.path(DIR_RAW, carpeta, id_ejecucion),
                              pattern = "\\.json\\.gz$", full.names = TRUE))
  if (length(archivos) == 0) {
    stop("No hay copias crudas de ", carpeta, " para la ejecución ", id_ejecucion)
  }
  map(archivos, function(a) {
    con <- gzfile(a, "r", encoding = "UTF-8"); on.exit(close(con))
    fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  })
}

# Precio numérico; 0, negativo o ausente -> NA (nunca inventar ceros)
num_o_na <- function(x) {
  x <- suppressWarnings(as.numeric(x %||% NA))
  if (length(x) == 0 || is.na(x) || x <= 0) NA_real_ else x
}

# Columnas comunes que devuelve TODA función de comercio
COLUMNAS <- c("comercio", "sku", "ean", "nombre_original", "marca", "subcategoria",
              "descripcion_original", "precio", "precio_antes", "promocion",
              "disponible", "url", "fecha_hora_consulta")

uniformar <- function(df) {
  plantilla <- tibble(
    comercio = character(), sku = character(), ean = character(),
    nombre_original = character(), marca = character(), subcategoria = character(),
    descripcion_original = character(), precio = numeric(), precio_antes = numeric(),
    promocion = integer(), disponible = integer(), url = character(),
    fecha_hora_consulta = character()
  )
  bind_rows(plantilla, df) |> select(all_of(COLUMNAS))
}

# ---- VTEX: categorías ---------------------------------------------------------

# Árbol de categorías de una tienda, para encontrar las rutas equivalentes
listar_categorias_vtex <- function(id_tienda, patron = "papel|servillet|toalla|pañuel|desechab") {
  cfg  <- TIENDAS[[id_tienda]]
  arbol <- pedir(paste0(cfg$base, "/api/catalog_system/pub/category/tree/3")) |> resp_body_json()
  aplanar <- function(nodos) {
    map_dfr(nodos, function(n) bind_rows(tibble(id = n$id, nombre = n$name, url = n$url),
                                         aplanar(n$children)))
  }
  aplanar(arbol) |>
    filter(str_detect(str_to_lower(nombre), patron)) |>
    mutate(ruta = str_remove(url, "^https?://[^/]+/"))
}

# ---- VTEX: productos --------------------------------------------------------

# Rutas escritas como "id:123" se buscan en el árbol de categorías de la tienda
resolver_rutas <- function(id_tienda) {
  rutas <- TIENDAS[[id_tienda]]$rutas
  if (!any(startsWith(rutas, "id:"))) return(rutas)
  arbol <- listar_categorias_vtex(id_tienda, patron = ".")
  vapply(rutas, function(r) {
    if (!startsWith(r, "id:")) return(r)
    fila <- arbol[arbol$id == as.integer(sub("id:", "", r)), ]
    if (nrow(fila) == 0) stop("No se encontró la categoría ", r, " en ", id_tienda)
    fila$ruta[1]
  }, character(1), USE.NAMES = FALSE)
}

capturar_vtex <- function(cfg, carpeta, id_ejecucion) {
  paginas <- list(); fallos <- character()
  cfg$rutas <- resolver_rutas(carpeta)
  message("   rutas: ", paste(cfg$rutas, collapse = " | "))
  for (i in seq_along(cfg$rutas)) {
    ruta  <- cfg$rutas[i]
    mapa  <- paste(rep("c", length(strsplit(ruta, "/")[[1]])), collapse = ",")
    desde <- 0; paso <- 50                       # VTEX: máximo 50 por página
    repeat {
      url  <- sprintf("%s/api/catalog_system/pub/products/search/%s?map=%s&_from=%d&_to=%d",
                      cfg$base, ruta, mapa, desde, desde + paso - 1)
      t    <- ahora_iso()
      resp <- tryCatch(pedir(url), error = function(e) NULL)
      if (is.null(resp)) {                       # algunas tiendas rechazan el parámetro map
        url  <- sprintf("%s/api/catalog_system/pub/products/search/%s?_from=%d&_to=%d",
                        cfg$base, ruta, desde, desde + paso - 1)
        resp <- tryCatch(pedir(url), error = function(e) {
          message("   ERROR en ", url, ": ", conditionMessage(e)); NULL })
      }
      if (is.null(resp)) {                       # se registra la ruta fallida y se sigue con la siguiente
        fallos <- c(fallos, url)
        break
      }
      pg   <- list(url = url, ruta = ruta, fecha_hora_consulta = t,
                   cuerpo = resp_body_string(resp),
                   total  = resp_header(resp, "resources") %||% NA)   # "0-49/312"
      guardar_raw(pg, carpeta, id_ejecucion, sprintf("r%02d_pag_%05d", i, desde))
      paginas[[length(paginas) + 1]] <- pg

      n_pag <- length(fromJSON(pg$cuerpo, simplifyVector = FALSE))
      total <- as.integer(str_extract(pg$total %||% "", "\\d+$"))
      desde <- desde + paso
      if (n_pag == 0 || (!is.na(total) && desde >= total) || desde >= 2500) break
    }
  }
  attr(paginas, "fallos") <- fallos
  paginas
}

parsear_vtex <- function(pg, comercio) {
  prods <- fromJSON(pg$cuerpo, simplifyVector = FALSE)
  map_dfr(prods, function(p) map_dfr(p$items, function(it) {
    of     <- if (length(it$sellers)) it$sellers[[1]]$commertialOffer else list()
    precio <- num_o_na(of$Price)
    antes  <- num_o_na(of$ListPrice)
    promo  <- !is.na(precio) && !is.na(antes) && antes > precio
    tibble(
      comercio             = comercio,
      sku                  = as.character(it$itemId),
      ean                  = as.character(it$ean %||% NA_character_),
      nombre_original      = it$nameComplete %||% p$productName %||% NA_character_,
      marca                = p$brand %||% NA_character_,
      subcategoria         = if (length(p$categories)) p$categories[[1]] else NA_character_,
      descripcion_original = p$description %||% NA_character_,
      precio               = precio,
      precio_antes         = if (promo) antes else NA_real_,
      promocion            = as.integer(promo),
      disponible           = as.integer((of$AvailableQuantity %||% 0) > 0),
      url                  = p$link %||% pg$url,
      fecha_hora_consulta  = pg$fecha_hora_consulta
    )
  }))
}

scrape_vtex <- function(id_tienda, id_ejecucion, offline = FALSE) {
  cfg     <- TIENDAS[[id_tienda]]
  paginas <- if (offline) leer_raw(id_tienda, id_ejecucion) else capturar_vtex(cfg, id_tienda, id_ejecucion)

  obs <- map_dfr(paginas, parsear_vtex, comercio = cfg$comercio) |>
    uniformar() |>
    filter(!is.na(nombre_original)) |>
    distinct(sku, .keep_all = TRUE)        # un producto puede estar en dos rutas

  # Completitud: productos únicos obtenidos vs. suma de totales que reporta el sitio por ruta
  totales <- map_dfr(paginas, ~ tibble(ruta = .x$ruta %||% NA_character_,
                                       total = as.integer(str_extract(.x$total %||% "", "\\d+$")))) |>
    distinct(ruta, .keep_all = TRUE)
  ids_producto <- unique(unlist(map(paginas, function(pg)
    map_chr(fromJSON(pg$cuerpo, simplifyVector = FALSE), ~ as.character(.x$productId)))))
  esperado <- sum(totales$total, na.rm = TRUE)

  fallos <- attr(paginas, "fallos")
  attr(obs, "n_esperado") <- esperado
  attr(obs, "estado") <- if (length(ids_producto) == 0) "fallida" else
                         if (length(fallos) > 0 ||
                             (esperado > 0 && length(ids_producto) < esperado * 0.95)) "parcial" else "completa"
  if (length(fallos) > 0) message("   ", length(fallos), " petición(es) fallida(s) en ", cfg$comercio)
  obs
}

scrape_exito    <- function(id_ejecucion, offline = FALSE) scrape_vtex("exito",    id_ejecucion, offline)
scrape_carulla  <- function(id_ejecucion, offline = FALSE) scrape_vtex("carulla",  id_ejecucion, offline)
scrape_olimpica <- function(id_ejecucion, offline = FALSE) scrape_vtex("olimpica", id_ejecucion, offline)

# ---- Base SQLite ------------------------------------------------------------

crear_esquema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ejecuciones (
      id_ejecucion TEXT NOT NULL,
      comercio     TEXT NOT NULL,
      inicio       TEXT NOT NULL,
      fin          TEXT,
      modo         TEXT NOT NULL,
      n_registros  INTEGER NOT NULL,
      n_esperado   INTEGER,
      estado       TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
      detalle      TEXT,
      PRIMARY KEY (id_ejecucion, comercio)
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS observaciones (
      id_obs               INTEGER PRIMARY KEY AUTOINCREMENT,
      id_ejecucion         TEXT NOT NULL,
      comercio             TEXT NOT NULL,
      sku                  TEXT NOT NULL,
      ean                  TEXT,
      nombre_original      TEXT,
      marca                TEXT,
      subcategoria         TEXT,
      descripcion_original TEXT,
      precio               REAL,
      precio_antes         REAL,
      promocion            INTEGER,
      disponible           INTEGER,
      url                  TEXT NOT NULL,
      fecha_hora_consulta  TEXT NOT NULL,
      UNIQUE (comercio, sku, fecha_hora_consulta),
      FOREIGN KEY (id_ejecucion, comercio) REFERENCES ejecuciones (id_ejecucion, comercio)
    )")
  invisible(TRUE)
}

# ---- Función general --------------------------------------------------------

COMERCIOS <- list(
  "Éxito"     = scrape_exito,
  "Carulla"        = scrape_carulla,
  "Olímpica"  = scrape_olimpica
)

ejecutar_recoleccion <- function(db = "comparador_precios.sqlite", offline = FALSE,
                                 id_ejecucion = NULL, comercios = names(COMERCIOS)) {
  if (offline && is.null(id_ejecucion)) {
    stop("En modo sin conexión indique id_ejecucion (nombre de la carpeta en raw/<comercio>/)")
  }
  id_ejecucion <- id_ejecucion %||% format(Sys.time(), "%Y%m%dT%H%M%S", tz = TZ_CO)

  con <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(con))
  crear_esquema(con)

  map_dfr(comercios, function(nombre) {
    inicio <- ahora_iso()
    err    <- NULL
    res    <- tryCatch(COMERCIOS[[nombre]](id_ejecucion, offline = offline),
                       error = function(e) { err <<- conditionMessage(e); NULL })

    n        <- if (is.null(res)) 0L else nrow(res)
    estado   <- if (n == 0) "fallida" else attr(res, "estado") %||% "completa"
    esperado <- if (is.null(res)) NA_integer_ else attr(res, "n_esperado") %||% NA_integer_

    # INSERT OR IGNORE: reprocesar sin conexión no altera el registro original
    dbExecute(con, "INSERT OR IGNORE INTO ejecuciones VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(id_ejecucion, nombre, inicio, ahora_iso(),
                            if (offline) "sin_conexion" else "en_linea",
                            n, esperado, estado, err %||% NA_character_))

    nuevos <- 0L
    if (n > 0) {
      res$id_ejecucion <- id_ejecucion
      cols <- c("id_ejecucion", COLUMNAS)
      sql  <- sprintf("INSERT OR IGNORE INTO observaciones (%s) VALUES (%s)",
                      paste(cols, collapse = ", "),
                      paste(rep("?", length(cols)), collapse = ", "))
      nuevos <- dbExecute(con, sql, params = unname(as.list(res[cols])))
    }

    message(sprintf("%-12s %-9s %5d registros, %5d nuevos%s", nombre, estado, n, nuevos,
                    if (is.null(err)) "" else paste0("  [", err, "]")))
    tibble(id_ejecucion, comercio = nombre, estado,
           n_registros = n, n_esperado = esperado, nuevos = nuevos)
  })
}
