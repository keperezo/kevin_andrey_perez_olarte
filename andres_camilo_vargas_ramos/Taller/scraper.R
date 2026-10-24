# =============================================================================
# scraper.R — Taller 1, Minería de Datos (UNAL)
# Requiere Google Chrome instalado (Farmatodo y Cruz Verde usan read_html_live).
# =============================================================================

suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(DBI); library(RSQLite)
  library(rvest); library(xml2); library(chromote)
})

UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)
TZ_CO   <- "America/Bogota"
DIR_RAW <- "raw"


# Para Colsubsidio, obtenerla con listar_categorias_colsubsidio("digest").
RUTAS <- list(
  colsubsidio = "medicamentos/venta-libre/bienestar-digestivo"
)


# Fecha y hora ISO 8601 en hora de Colombia: 2026-10-03T10:15:00-05:00
ahora_iso <- function() {
  x <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = TZ_CO)
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", x)
}

# Petición HTTP con user agent, reintentos y pausa entre peticiones
pedir <- function(url, ..., cuerpo = NULL) {
  req <- request(url) |>
    req_user_agent(UA) |>
    req_headers(`accept-language` = "es-CO,es;q=0.9", ...) |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_throttle(rate = 1)            # máximo 1 petición por segundo
  if (!is.null(cuerpo)) req <- req_body_json(req, cuerpo)
  req_perform(req)
}

# Cada página cruda se guarda como lista: url, fecha_hora_consulta, cuerpo, total
guardar_raw <- function(pagina, comercio, id_ejecucion, nombre) {
  dir <- file.path(DIR_RAW, comercio, id_ejecucion)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  con <- gzfile(file.path(dir, paste0(nombre, ".json.gz")), "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(toJSON(pagina, auto_unbox = TRUE, null = "null", na = "null"), con)
}

leer_raw <- function(comercio, id_ejecucion) {
  archivos <- sort(list.files(file.path(DIR_RAW, comercio, id_ejecucion),
                              pattern = "\\.json\\.gz$", full.names = TRUE))
  if (length(archivos) == 0) {
    stop("No hay copias crudas de ", comercio, " para la ejecución ", id_ejecucion)
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
COLUMNAS <- c("comercio", "sku", "nombre_original", "marca", "subcategoria",
              "descripcion_original", "precio", "precio_antes", "promocion",
              "disponible", "url", "fecha_hora_consulta")

uniformar <- function(df) {
  plantilla <- tibble(
    comercio = character(), sku = character(), nombre_original = character(),
    marca = character(), subcategoria = character(), descripcion_original = character(),
    precio = numeric(), precio_antes = numeric(), promocion = integer(),
    disponible = integer(), url = character(), fecha_hora_consulta = character()
  )
  bind_rows(plantilla, df) |> select(all_of(COLUMNAS))
}

# ---- Colsubsidio (VTEX) -----------------------------------------------------

# Árbol de categorías del sitio, para identificar la ruta equivalente
listar_categorias_colsubsidio <- function(patron = "digest") {
  arbol <- pedir("https://www.drogueriascolsubsidio.com/api/catalog_system/pub/category/tree/3") |>
    resp_body_json()
  aplanar <- function(nodos) {
    map_dfr(nodos, function(n) {
      bind_rows(tibble(id = n$id, nombre = n$name, url = n$url),
                aplanar(n$children))
    })
  }
  aplanar(arbol) |> filter(str_detect(str_to_lower(nombre), patron))
}

scrape_colsubsidio <- function(id_ejecucion, offline = FALSE,
                               ruta_categoria = RUTAS$colsubsidio) {
  carpeta <- "colsubsidio"
  base    <- "https://www.drogueriascolsubsidio.com"

  if (offline) {
    paginas <- leer_raw(carpeta, id_ejecucion)
  } else {
    if (identical(ruta_categoria, "TODO")) stop("Defina RUTAS$colsubsidio")
    paginas <- list(); desde <- 0; paso <- 50     # VTEX: máx. 50 por página
    repeat {
      url  <- sprintf("%s/api/catalog_system/pub/products/search/%s?_from=%d&_to=%d",
                      base, ruta_categoria, desde, desde + paso - 1)
      t    <- ahora_iso()
      resp <- pedir(url)
      pg   <- list(url = url, fecha_hora_consulta = t,
                   cuerpo = resp_body_string(resp),
                   total  = resp_header(resp, "resources") %||% NA)  # "0-49/312"
      guardar_raw(pg, carpeta, id_ejecucion, sprintf("pag_%05d", desde))
      paginas[[length(paginas) + 1]] <- pg

      n_pag <- length(fromJSON(pg$cuerpo, simplifyVector = FALSE))
      total <- as.integer(str_extract(pg$total %||% "", "\\d+$"))
      desde <- desde + paso
      if (n_pag == 0 || (!is.na(total) && desde >= total) || desde >= 2500) break
    }
  }

  # Cada producto VTEX puede tener varios items (SKU = presentación)
  obs <- map_dfr(paginas, function(pg) {
    prods <- fromJSON(pg$cuerpo, simplifyVector = FALSE)
    map_dfr(prods, function(p) map_dfr(p$items, function(it) {
      of     <- if (length(it$sellers)) it$sellers[[1]]$commertialOffer else list()
      precio <- num_o_na(of$Price)
      antes  <- num_o_na(of$ListPrice)
      promo  <- !is.na(precio) && !is.na(antes) && antes > precio
      tibble(
        comercio             = "Colsubsidio",
        sku                  = as.character(it$itemId),
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
  })

  # Verificación de completitud: productos obtenidos vs. total que reporta el sitio
  n_productos <- sum(map_int(paginas, ~ length(fromJSON(.x$cuerpo, simplifyVector = FALSE))))
  total       <- as.integer(str_extract(paginas[[1]]$total %||% "", "\\d+$"))

  obs <- uniformar(obs)
  attr(obs, "n_esperado") <- total
  attr(obs, "estado") <- if (!is.na(total) && n_productos < total) "parcial" else "completa"
  obs
}

# ---- Tiendas dinámicas con navegador (patrón visto en clase) ----------------
# read_html_live() + Sys.sleep() + on.exit(), adaptado al taller:
#   - recorre TODA la categoría (páginas, scroll o botón "Ver más")
#   - guarda el HTML renderizado en raw/ y permite reprocesarlo sin conexión
#   - conserva texto original, URL del producto, precio antes y hora ISO 8601
#
# Selectores: en Chrome, clic derecho sobre un producto > Inspeccionar.
# Usar coincidencia parcial [class*='...'] como en clase, porque los sufijos
# con hash cambian entre despliegues.

CONFIG_LIVE <- list(
  farmatodo = list(
    comercio        = "Farmatodo",
    carpeta         = "farmatodo",
    base            = "https://www.farmatodo.com.co",
    
    construir_url   = function(k) "https://www.farmatodo.com.co/categorias/salud-y-medicamentos/salud-digestiva",
    modo            = "boton",         
    texto_boton     = "Cargar más",   
    selector_boton  = NA_character_,   
    selector_ficha  = "div.product-card-handler",            
    selector_nombre = "a.product-card-handler__detail-description",
    selector_marca  = "p.product-card-handler__brand-value",
    
    selector_precio = c("span.product-all-price__offer", "span.product-all-price__normal"),
    selector_precio_antes = "span.product-all-price__normal--line-through",  
    selector_enlace = "a[href*='/producto/']",  
    patron_sku      = "/producto/(\\d+)",     
    atributo_sku    = NA_character_,   
    selector_total  = NA_character_,  
    max_pasos       = 60
  ),
  cruzverde = list(
    comercio        = "Cruz Verde",
    carpeta         = "cruzverde",
    base            = "https://www.cruzverde.com.co",
    construir_url   = function(k) "https://www.cruzverde.com.co/medicamentos/salud-digestiva/",
    modo            = "boton",
    texto_boton     = "Ver más productos",
    selector_boton  = NA_character_,
    selector_ficha  = "div.relative.h-full.bg-white.rounded-sm.border.border-gray",
    selector_nombre = "at-link a",
    selector_marca  = "div.italic.uppercase",
    selector_precio = "ml-price-tag span.font-bold",           # "$ 2.592"
    selector_precio_antes = "ml-price-tag div.line-through",   # "$ 3.050 (Normal)"
    selector_enlace = NA_character_,
    sku_desde       = list(selector = "at-image img", atributo = c("data-src", "src"),
                           patron = "/images/large/(\\d+)"),
    recorrer_pagina = TRUE,   
    atributo_sku    = NA_character_,
    selector_total  = NA_character_,
    max_pasos       = 60
  )
)


configurar_chromote <- function(timeout = 60) {
  ch <- chromote::default_chromote_object()
  ch$default_timeout <- timeout
  chromote::set_default_chromote_object(ch)
  invisible(ch)
}


precio_texto_a_num <- function(x) {
  monto <- str_extract(x, "\\$\\s?[\\d\\.,]+")
  monto <- ifelse(is.na(monto), x, monto)
  monto <- str_remove(monto, ",\\d{1,2}$")             # quita centavos si los hay
  v <- suppressWarnings(as.numeric(str_remove_all(monto, "[^0-9]")))
  ifelse(is.na(v) | v <= 0, NA_real_, v)
}

texto_o_na <- function(fichas, selector) {
  out <- rep(NA_character_, length(fichas))
  for (sel in selector[!is.na(selector)]) {
    v   <- html_text2(html_element(fichas, sel))
    out <- ifelse(is.na(out) | out == "", v, out)
  }
  out
}

clic_por_texto <- function(sesion, texto) {
  js <- sprintf(
    "(() => { const t = %s.toLowerCase();
       const els = [...document.querySelectorAll('*')]
         .filter(e => e.innerText && e.innerText.trim().toLowerCase() === t);
       let b = els[els.length - 1];
       if (!b) b = [...document.querySelectorAll('button, a, [role=button]')]
         .find(e => e.innerText && e.innerText.toLowerCase().includes(t));
       if (!b) return false;
       b.scrollIntoView(); b.click(); return true; })()",
    jsonlite::toJSON(texto, auto_unbox = TRUE))
  res <- sesion$session$Runtime$evaluate(js)
  isTRUE(res$result$value)
}

recorrer_pagina <- function(sesion, paso = 700, pausa = 0.4) {
  alto <- sesion$session$Runtime$evaluate("document.body.scrollHeight")$result$value
  for (y in seq(0, alto, by = paso)) {
    sesion$session$Runtime$evaluate(sprintf("window.scrollTo(0, %d)", as.integer(y)))
    Sys.sleep(pausa)
  }
  Sys.sleep(2)
  invisible(TRUE)
}

capturar_live <- function(cfg, id_ejecucion, pausa = 5) {
  configurar_chromote()
  guardar <- function(sesion, url, k) {
    pg <- list(
      url = url,
      fecha_hora_consulta = ahora_iso(),
      # HTML completo tal como lo pintó el navegador
      cuerpo = sesion$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
      total  = if (is.na(cfg$selector_total)) NA else
                 html_text2(html_element(sesion, cfg$selector_total))
    )
    guardar_raw(pg, cfg$carpeta, id_ejecucion, sprintf("pag_%05d", k))
    pg
  }
  paginas <- list()

  if (cfg$modo == "paginas") {
    firma_anterior <- ""
    for (k in seq_len(cfg$max_pasos)) {
      url <- cfg$construir_url(k)
      message(sprintf("-> %s | página %d", cfg$comercio, k))
      pg <- local({
        sesion <- read_html_live(url)
        on.exit(sesion$session$close(), add = TRUE)
        Sys.sleep(pausa)
        fichas <- html_elements(sesion, cfg$selector_ficha)
        if (length(fichas) == 0) return(NULL)
        firma <- html_text2(fichas[[1]])
        if (identical(firma, firma_anterior)) return(NULL)   # el sitio repite la última página
        firma_anterior <<- firma
        guardar(sesion, url, k)
      })
      if (is.null(pg)) break
      paginas[[length(paginas) + 1]] <- pg
      Sys.sleep(runif(1, 2, 4))
    }
  } else {
    url <- cfg$construir_url(1)
    message(sprintf("-> %s | %s", cfg$comercio, cfg$modo))
    sesion <- read_html_live(url)
    on.exit(sesion$session$close(), add = TRUE)
    Sys.sleep(pausa)
    n_prev <- -1
    for (paso in seq_len(cfg$max_pasos)) {
      n <- length(html_elements(sesion, cfg$selector_ficha))
      if (n == n_prev) {                                      # quizá tardó más: esperar y recontar
        Sys.sleep(pausa * 2)
        n <- length(html_elements(sesion, cfg$selector_ficha))
      }
      message(sprintf("   %d productos cargados", n))
      if (n == n_prev) break                                  # ya no carga más
      n_prev <- n
      if (cfg$modo == "scroll") {
        sesion$scroll_to(top = 1e7)
      } else if (!is.null(cfg$texto_boton)) {
        if (!clic_por_texto(sesion, cfg$texto_boton)) break
      } else {
        if (length(html_elements(sesion, cfg$selector_boton)) == 0) break
        sesion$click(cfg$selector_boton)
      }
      Sys.sleep(pausa)
    }
    if (isTRUE(cfg$recorrer_pagina)) {
      message("   recorriendo la página para cargar imágenes...")
      recorrer_pagina(sesion)
    }
    if (n_prev > 0) paginas[[1]] <- guardar(sesion, url, 1)
  }
  paginas
}

parsear_live <- function(pg, cfg) {
  doc    <- read_html(pg$cuerpo)
  fichas <- html_elements(doc, cfg$selector_ficha)
  if (length(fichas) == 0) return(tibble())

  nombre <- texto_o_na(fichas, cfg$selector_nombre)

  
  url_abs <- if (is.na(cfg$selector_enlace)) rep(pg$url, length(fichas)) else {
    href <- html_attr(html_element(fichas, cfg$selector_enlace), "href")
    ifelse(is.na(href), pg$url, xml2::url_absolute(href, cfg$base))
  }

 
  sku <- if (!is.null(cfg$sku_desde)) {
           img <- html_element(fichas, cfg$sku_desde$selector)
           v   <- rep(NA_character_, length(fichas))
           for (a in cfg$sku_desde$atributo) {
             v <- ifelse(is.na(v), str_match(html_attr(img, a), cfg$sku_desde$patron)[, 2], v)
           }
           v
         } else if (!is.null(cfg$patron_sku)) {
           str_match(url_abs, cfg$patron_sku)[, 2]
         } else if (!is.na(cfg$atributo_sku)) {
           html_attr(fichas, cfg$atributo_sku)
         } else {
           str_remove(str_extract(url_abs, "[^/?#]+/?(?=([?#]|$))"), "/$")
         }
  sku <- ifelse(is.na(sku) | sku == "", paste0("sin_sku:", nombre), sku)

  precio <- precio_texto_a_num(texto_o_na(fichas, cfg$selector_precio))
  antes  <- precio_texto_a_num(texto_o_na(fichas, cfg$selector_precio_antes))
  promo  <- !is.na(precio) & !is.na(antes) & antes > precio

  tibble(
    comercio             = cfg$comercio,
    sku                  = sku,
    nombre_original      = nombre,
    marca                = texto_o_na(fichas, cfg$selector_marca),
    subcategoria         = NA_character_,
    descripcion_original = html_text2(fichas),        # texto completo de la ficha, sin limpiar
    precio               = precio,
    precio_antes         = ifelse(promo, antes, NA_real_),
    promocion            = as.integer(promo),
    disponible           = NA_integer_,
    url                  = url_abs,
    fecha_hora_consulta  = pg$fecha_hora_consulta
  )
}

scrape_live <- function(id_tienda, id_ejecucion, offline = FALSE) {
  cfg <- CONFIG_LIVE[[id_tienda]]
  if (!offline && any(c(cfg$construir_url(1), cfg$selector_ficha, cfg$selector_precio) %in% "TODO")) {
    stop("Defina la URL y los selectores de ", cfg$comercio, " en CONFIG_LIVE")
  }
  paginas <- if (offline) leer_raw(cfg$carpeta, id_ejecucion) else capturar_live(cfg, id_ejecucion)
  if (length(paginas) == 0) {
    stop("No se encontraron fichas en ", cfg$comercio, ": ¿selector roto o bloqueo?")
  }
  obs <- map_dfr(paginas, parsear_live, cfg = cfg) |>
    uniformar() |>
    filter(!is.na(nombre_original)) |>     # descarta tarjetas vacías (sin nombre)
    distinct(sku, .keep_all = TRUE)
  if (nrow(obs) == 0) {
    stop("El HTML guardado de ", cfg$comercio, " no contiene fichas con el selector '",
         cfg$selector_ficha, "'")
  }
  total <- as.integer(str_extract(str_remove_all(paginas[[1]]$total %||% "", "[\\.,]"), "\\d+"))
  attr(obs, "n_esperado") <- total
  attr(obs, "estado") <- if (!is.na(total) && nrow(obs) < total) "parcial" else "completa"
  obs
}

scrape_farmatodo <- function(id_ejecucion, offline = FALSE) scrape_live("farmatodo", id_ejecucion, offline)
scrape_cruzverde <- function(id_ejecucion, offline = FALSE) scrape_live("cruzverde", id_ejecucion, offline)

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
  "Colsubsidio" = scrape_colsubsidio,
  "Farmatodo"   = scrape_farmatodo,
  "Cruz Verde"  = scrape_cruzverde
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

