# Taller 1 - Comparador de precios
# Categoria 42: Repelentes y control de insectos
# Juan Felipe Rojas Casallas
# Mineria de Datos 2016325
#
# Comercios: La Rebaja Virtual, Exito, Jumbo Colombia
# Los 3 usan VTEX asi que se puede usar la misma API para todos
#
# Para correr: source("taller_1.R") en RStudio
# Si ya existe la base de datos no vuelve a scrapear

library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Config de las tiendas ----

VTEX_BATCH <- 50

tiendas <- list(
  la_rebaja = list(
    id = "la_rebaja",
    nombre = "La Rebaja Virtual",
    base_url = "https://www.larebajavirtual.com",
    paths = c("cuidado-personal/cuidado-corporal/repelentes"),
    search_terms = c("repelente", "insecticida")
  ),
  exito = list(
    id = "exito",
    nombre = "Exito",
    base_url = "https://www.exito.com",
    paths = c(
      "cuidado-personal/proteccion-solar-y-repelentes/repelentes",
      "mercado/aseo-del-hogar/ambientadores-e-insecticidas"
    ),
    search_terms = NULL
  ),
  jumbo = list(
    id = "jumbo",
    nombre = "Jumbo Colombia",
    base_url = "https://www.jumbocolombia.com",
    paths = c(
      "supermercado/belleza/repelentes",
      "supermercado/aseo-de-hogar/insecticidas"
    ),
    search_terms = NULL
  )
)

# ---- Funciones auxiliares ----

colombia_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00", tz = "America/Bogota")

gen_run_id <- function() format(Sys.time(), "%Y%m%d_%H%M%S", tz = "America/Bogota")

# guardar y cargar los json crudos
guardar_raw <- function(texto, store_id, run_id, cat_path, pag) {
  dir_raw <- file.path("raw", store_id, run_id)
  if (!dir.exists(dir_raw)) dir.create(dir_raw, recursive = TRUE)
  slug <- gsub("[/?=&%]", "_", cat_path)
  slug <- gsub("_+", "_", gsub("^_|_$", "", slug))
  fp <- file.path(dir_raw, sprintf("%s_p%02d.json", slug, pag))
  writeLines(texto, fp, useBytes = TRUE)
  fp
}

cargar_raw <- function(store_id, run_id, cat_path, pag) {
  slug <- gsub("[/?=&%]", "_", cat_path)
  slug <- gsub("_+", "_", gsub("^_|_$", "", slug))
  fp <- file.path("raw", store_id, run_id, sprintf("%s_p%02d.json", slug, pag))
  if (file.exists(fp)) paste(readLines(fp, warn = FALSE), collapse = "\n") else NULL
}

ultimo_run <- function(store_id) {
  d <- file.path("raw", store_id)
  if (!dir.exists(d)) return(NULL)
  runs <- sort(list.dirs(d, full.names = FALSE, recursive = FALSE), decreasing = TRUE)
  if (length(runs) > 0) runs[1] else NULL
}

# ---- Consultar la API de VTEX ----
# VTEX devuelve 206 cuando hay paginacion, no solo 200

fetch_vtex <- function(base_url, path, from, to) {
  if (startsWith(path, "?ft=")) {
    url <- sprintf("%s/api/catalog_system/pub/products/search/%s&_from=%d&_to=%d",
                   base_url, path, from, to)
  } else {
    url <- sprintf("%s/api/catalog_system/pub/products/search/%s?_from=%d&_to=%d",
                   base_url, path, from, to)
  }

  resp <- NULL
  for (intento in 1:3) {
    resp <- tryCatch(
      GET(url, add_headers(Accept = "application/json",
                           `Content-Type` = "application/json"),
          timeout(60)),
      error = function(e) { message("  Error conexion: ", e$message); NULL }
    )
    if (!is.null(resp) && status_code(resp) %in% c(200L, 206L)) break
    if (intento < 3) { message("  Reintentando..."); Sys.sleep(3) }
  }

  if (is.null(resp) || !(status_code(resp) %in% c(200L, 206L))) {
    return(list(text = NULL, total = 0))
  }

  # el total viene en el header "resources" como "0-49/123"
  total <- 0
  rh <- headers(resp)[["resources"]]
  if (!is.null(rh)) {
    partes <- strsplit(rh, "/")[[1]]
    if (length(partes) == 2) total <- as.integer(partes[2])
  }

  list(text = content(resp, as = "text", encoding = "UTF-8"), total = total)
}

# paginar todas las paginas de una categoria
fetch_todos <- function(base_url, path, store_id, run_id, offline = FALSE) {
  message("  Cat: ", path)
  productos <- list()
  pag <- 1
  desde <- 0

  repeat {
    hasta <- desde + VTEX_BATCH - 1
    if (offline) {
      json_txt <- cargar_raw(store_id, run_id, path, pag)
      if (is.null(json_txt)) break
    } else {
      res <- fetch_vtex(base_url, path, desde, hasta)
      json_txt <- res$text
      if (is.null(json_txt)) break
      if (res$total > 0 && pag == 1) message("    Total: ", res$total)
      guardar_raw(json_txt, store_id, run_id, path, pag)
      Sys.sleep(runif(1, 1, 2.5))
    }

    prods <- tryCatch(fromJSON(json_txt, simplifyDataFrame = FALSE),
                      error = function(e) list())
    if (length(prods) == 0) break
    productos <- c(productos, prods)
    message("    Pag ", pag, ": ", length(prods), " (total: ", length(productos), ")")
    if (length(prods) < VTEX_BATCH) break
    desde <- desde + VTEX_BATCH
    pag <- pag + 1
  }
  productos
}

# ---- Extraer contenido (ml, g) del nombre ----

extraer_contenido <- function(txt) {
  if (is.na(txt) || is.null(txt) || nchar(txt) == 0)
    return(list(valor = NA_real_, unidad = NA_character_))

  m <- regexpr("(\\d+\\.?\\d*)\\s*(ml|mL|ML|g|G|gr|grs|GR|GRS|kg|KG|mg|MG|lt|LT|l|L|cc|CC|oz|OZ)",
               txt, perl = TRUE)
  if (m[1] > 0) {
    match <- regmatches(txt, m)
    p <- regmatches(match, regexec("(\\d+\\.?\\d*)\\s*(\\S+)", match, perl = TRUE))[[1]]
    u <- tolower(p[3])
    u <- switch(u, "grs" = "g", "gr" = "g", "lt" = "l", "cc" = "ml", u)
    return(list(valor = as.numeric(p[2]), unidad = u))
  }
  list(valor = NA_real_, unidad = NA_character_)
}

# ---- Filtro para la categoria "ambientadores e insecticidas" de Exito ----
# esa categoria tiene ambientadores mezclados, hay que filtrarlos

es_repelente <- function(nombre, marca) {
  n <- tolower(nombre %||% "")
  m <- tolower(marca %||% "")

  # esto NO es repelente/insecticida
  excluir <- c("ambientador", "aromatiz", "desodorante.*(ambiental|hogar|bano)",
    "difusor", "vela.*(arom|perfum)", "incienso", "fragancia",
    "shampoo", "collar.*(pulga|garrapata)", "raticida", "veneno.*(rata|raton)",
    "limpia", "jabon", "detergente", "suavizante", "blanqueador",
    "desinfectan", "cloro", "lava(platos|dora|rropas)", "cera",
    "quitamanch", "quitagrasa", "escoba", "trapero", "guante",
    "esponja", "bolsa.*(basura|residuo)", "papel.*(higien|cocina)",
    "servilleta", "toalla.*(cocina|papel)", "recogedor", "balde")

  # esto SI es
  incluir <- c("repelente.*(insect|mosqu|zancud|plagas)",
    "insecticida", "mata.?insect", "mata.?mosqu", "mata.?cucarach",
    "mata.?hormig", "antimosquit", "anti.?mosquit",
    "espiral.*(mosqu|insect)", "tableta.*(mosquit|insect)",
    "citronela", "fumig", "cucarachicida",
    "aerosol.*(insect|cucarach|mosqu|hormig|volad)",
    "control.*(plaga|insect)")

  marcas_ok <- c("\\braid\\b", "\\bbaygon\\b", "stay.?off", "\\bnopikex\\b",
    "\\bautan\\b", "\\bbacterion\\b", "\\boff!?\\b")

  if (grepl(paste0(excluir, collapse = "|"), n, perl = TRUE)) return(FALSE)
  if (grepl(paste0(incluir, collapse = "|"), n, perl = TRUE)) return(TRUE)
  if (grepl(paste0(marcas_ok, collapse = "|"), m, perl = TRUE)) return(TRUE)
  if (grepl(paste0(marcas_ok, collapse = "|"), n, perl = TRUE)) return(TRUE)
  FALSE
}

# ---- Parsear los productos del JSON de VTEX ----

parsear_productos <- function(lista, comercio_id, comercio_nombre, fecha, filtrar = FALSE) {
  filas <- lapply(lista, function(p) {
    tryCatch({
      precio <- NA_real_; precio_reg <- NA_real_; sku <- NA_character_

      if (length(p$items) > 0) {
        item <- p$items[[1]]
        sku <- as.character(item$itemId %||% NA)
        if (length(item$sellers) > 0) {
          oferta <- item$sellers[[1]]$commertialOffer
          if (!is.null(oferta)) {
            precio <- as.numeric(oferta$Price %||% NA)
            precio_reg <- as.numeric(oferta$ListPrice %||% NA)
          }
        }
      }

      cont_val <- NA_real_; cont_ud <- NA_character_; pres <- NA_character_

      # intentar sacar contenido de los campos VTEX
      cv <- p[["Cantidadunidadesmedida"]]
      if (!is.null(cv) && length(cv) > 0) cont_val <- as.numeric(cv[[1]])
      um <- p[["Unidadmedida"]]
      if (!is.null(um) && length(um) > 0) {
        cont_ud <- tolower(um[[1]])
        cont_ud <- switch(cont_ud,
          "mililitro"="ml", "mililitros"="ml", "gramo"="g", "gramos"="g",
          "kilogramo"="kg", "litro"="l", "litros"="l", cont_ud)
      }
      pum <- p[["Presentacionunidadmedida"]]
      if (!is.null(pum) && length(pum) > 0) pres <- pum[[1]]

      # si no hay datos de contenido, intentar sacarlo del nombre
      if (is.na(cont_val) || is.na(cont_ud)) {
        ext <- extraer_contenido(pres %||% (p$productName %||% ""))
        if (is.na(cont_val)) cont_val <- ext$valor
        if (is.na(cont_ud)) cont_ud <- ext$unidad
        if (is.na(cont_val) || is.na(cont_ud)) {
          ext2 <- extraer_contenido(p$productName %||% "")
          if (is.na(cont_val)) cont_val <- ext2$valor
          if (is.na(cont_ud)) cont_ud <- ext2$unidad
        }
      }
      if (is.na(pres)) pres <- p$productName %||% NA_character_

      subcat <- NA_character_
      if (length(p$categories) > 0) subcat <- p$categories[[1]]

      nom <- p$productName %||% NA_character_
      mar <- p$brand %||% NA_character_

      if (filtrar && !es_repelente(nom, mar)) return(NULL)

      desc <- p$description %||% ""
      txt_orig <- if (nchar(desc) > 0 && desc != nom) paste(nom, desc, sep=" | ") else nom

      data.frame(
        comercio_id = comercio_id, producto_id_comercio = as.character(p$productId %||% NA),
        sku_comercio = sku, nombre_producto = nom,
        nombre_estandarizado = NA_character_, marca = mar,
        marca_estandarizada = NA_character_, subcategoria = subcat,
        presentacion = pres, contenido_valor = cont_val,
        contenido_unidad = cont_ud, precio = precio,
        precio_regular = precio_reg,
        es_promocion = as.integer(!is.na(precio) & !is.na(precio_reg) & precio < precio_reg),
        url = p$link %||% NA_character_, texto_original = txt_orig,
        fecha_hora_consulta = fecha, comparable = 0L,
        producto_comun_id = NA_character_, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  bind_rows(Filter(Negate(is.null), filas))
}

# ---- Scrapear una tienda completa ----

scrapear_tienda <- function(tienda, offline = FALSE, run_id = NULL) {
  message("\n=== ", tienda$nombre, " ===")
  if (is.null(run_id)) run_id <- gen_run_id()
  if (offline) {
    run_id <- ultimo_run(tienda$id)
    if (is.null(run_id)) { message("  No hay datos guardados"); return(NULL) }
  }

  fecha <- colombia_now()
  datos <- list()

  for (path in tienda$paths) {
    hay_que_filtrar <- grepl("ambientador", path)
    raw <- fetch_todos(tienda$base_url, path, tienda$id, run_id, offline)
    if (length(raw) == 0) next
    df <- parsear_productos(raw, tienda$id, tienda$nombre, fecha, filtrar = hay_que_filtrar)
    if (nrow(df) > 0) datos[[length(datos) + 1]] <- df
  }

  # busqueda por texto (para La Rebaja que tiene pocos en la categoria)
  if (!is.null(tienda$search_terms)) {
    for (term in tienda$search_terms) {
      sp <- sprintf("?ft=%s", URLencode(term, reserved = TRUE))
      raw <- fetch_todos(tienda$base_url, sp, tienda$id, run_id, offline)
      if (length(raw) == 0) next
      df <- parsear_productos(raw, tienda$id, tienda$nombre, fecha, filtrar = TRUE)
      if (nrow(df) > 0) datos[[length(datos) + 1]] <- df
    }
  }

  if (length(datos) == 0) return(NULL)
  resultado <- bind_rows(datos)
  resultado <- resultado[!duplicated(resultado$producto_id_comercio), ]
  message("  Unicos: ", nrow(resultado))
  list(data = resultado, run_id = run_id)
}

# ---- Crear la base SQLite ----

crear_db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)

  dbExecute(con, "CREATE TABLE IF NOT EXISTS comercios (
    comercio_id TEXT PRIMARY KEY, nombre TEXT NOT NULL, url_base TEXT NOT NULL)")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS ejecuciones (
    ejecucion_id TEXT PRIMARY KEY, fecha_hora_inicio TEXT NOT NULL,
    comercio_id TEXT NOT NULL, n_registros INTEGER DEFAULT 0,
    estado TEXT DEFAULT 'en_curso',
    FOREIGN KEY (comercio_id) REFERENCES comercios(comercio_id))")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS observaciones (
    observacion_id INTEGER PRIMARY KEY AUTOINCREMENT,
    ejecucion_id TEXT NOT NULL, comercio_id TEXT NOT NULL,
    producto_id_comercio TEXT, sku_comercio TEXT,
    nombre_producto TEXT NOT NULL, nombre_estandarizado TEXT,
    marca TEXT, marca_estandarizada TEXT, subcategoria TEXT,
    presentacion TEXT, contenido_valor REAL, contenido_unidad TEXT,
    precio REAL, precio_regular REAL, es_promocion INTEGER DEFAULT 0,
    url TEXT, texto_original TEXT, fecha_hora_consulta TEXT NOT NULL,
    comparable INTEGER DEFAULT 0, producto_comun_id TEXT,
    FOREIGN KEY (ejecucion_id) REFERENCES ejecuciones(ejecucion_id),
    FOREIGN KEY (comercio_id) REFERENCES comercios(comercio_id),
    UNIQUE(comercio_id, producto_id_comercio, fecha_hora_consulta))")

  for (t in tiendas) {
    dbExecute(con, "INSERT OR IGNORE INTO comercios VALUES (?, ?, ?)",
              params = list(t$id, t$nombre, t$base_url))
  }
  con
}

insertar_obs <- function(con, df, ejec_id) {
  if (is.null(df) || nrow(df) == 0) return(0)
  df$ejecucion_id <- ejec_id
  cols <- c("ejecucion_id","comercio_id","producto_id_comercio","sku_comercio",
            "nombre_producto","nombre_estandarizado","marca","marca_estandarizada",
            "subcategoria","presentacion","contenido_valor","contenido_unidad",
            "precio","precio_regular","es_promocion","url","texto_original",
            "fecha_hora_consulta","comparable","producto_comun_id")
  n0 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n
  dbWriteTable(con, "observaciones", df[, cols, drop = FALSE], append = TRUE, row.names = FALSE)
  dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n - n0
}

# ---- Correr todo el scraping ----

correr_scraper <- function(db_path, offline = FALSE) {
  cat("Iniciando scraper -", colombia_now(), "\n")
  con <- crear_db(db_path)
  on.exit(dbDisconnect(con))

  for (key in names(tiendas)) {
    cfg <- tiendas[[key]]
    rid <- gen_run_id()
    eid <- paste0(key, "_", rid)
    dbExecute(con, "INSERT OR IGNORE INTO ejecuciones VALUES (?,?,?,0,'en_curso')",
              params = list(eid, colombia_now(), cfg$id))

    res <- tryCatch(
      scrapear_tienda(cfg, offline, run_id = if(offline) NULL else rid),
      error = function(e) { message("ERROR: ", e$message); NULL })

    if (!is.null(res) && nrow(res$data) > 0) {
      n <- insertar_obs(con, res$data, eid)
      message("  Guardados: ", n)
      dbExecute(con, "UPDATE ejecuciones SET n_registros=?, estado='completa' WHERE ejecucion_id=?",
                params = list(n, eid))
    } else {
      dbExecute(con, "UPDATE ejecuciones SET estado='fallida' WHERE ejecucion_id=?",
                params = list(eid))
    }
  }
  cat("Scraping listo -", colombia_now(), "\n")
}

# =========================================================
# EJECUCION
# =========================================================

db_path <- "comparador_precios.sqlite"

if (!file.exists(db_path)) {
  correr_scraper(db_path)
} else {
  cat("Ya existe la base de datos, no se vuelve a scrapear\n")
}

con <- dbConnect(SQLite(), db_path)

# ---- Ver que se obtuvo ----

cat("\n-- Ejecuciones --\n")
print(dbGetQuery(con, "
  SELECT c.nombre, e.n_registros, e.estado
  FROM ejecuciones e JOIN comercios c ON e.comercio_id = c.comercio_id"))

cat("\n-- Productos por comercio --\n")
print(dbGetQuery(con, "
  SELECT c.nombre AS comercio, COUNT(*) AS total,
    SUM(CASE WHEN precio > 0 THEN 1 ELSE 0 END) AS con_precio
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  GROUP BY c.nombre"))

# =========================================================
# LIMPIEZA Y ESTANDARIZACION
# =========================================================

obs <- dbGetQuery(con, "SELECT * FROM observaciones")

# estandarizar unidades
obs$contenido_unidad <- tolower(obs$contenido_unidad)
obs$contenido_unidad[obs$contenido_unidad %in% c("grs","gr","gramo","gramos")] <- "g"
obs$contenido_unidad[obs$contenido_unidad %in% c("mililitro","mililitros")] <- "ml"
obs$contenido_unidad[obs$contenido_unidad == "cc"] <- "ml"

# estandarizar marcas (cada tienda las escribe diferente)
ml <- tolower(obs$marca)
obs$marca_std <- obs$marca
obs$marca_std[grepl("stay.?off", ml)] <- "Stay Off"
obs$marca_std[grepl("^autan$", ml)]   <- "Autan"
obs$marca_std[grepl("^nopikex$", ml)] <- "Nopikex"
obs$marca_std[grepl("^bacterion$", ml)] <- "Bacterion"
obs$marca_std[grepl("^raid$", ml)]    <- "Raid"
obs$marca_std[grepl("^baygon$", ml)]  <- "Baygon"
obs$marca_std[grepl("^off!?$", ml)]   <- "Off!"
obs$marca_std[grepl("^repeblanc$", ml)] <- "Repeblanc"

cat("\nTotal productos:", nrow(obs), "\n")
cat("Con precio:", sum(obs$precio > 0, na.rm = TRUE), "\n")
cat("Sin precio:", sum(is.na(obs$precio) | obs$precio == 0), "\n")

# =========================================================
# IDENTIFICAR PRODUCTOS COMPARABLES
# =========================================================
# Revise manualmente los productos de cada tienda y encontre 9 que estan en las 3

cat("\n-- Productos comparables --\n")

comparables <- data.frame(
  id = paste0("P0", 1:9),
  nombre = c("Stay Off Spray Adulto 120ml", "Stay Off Amazonic Spray 120ml",
    "Autan Crema Family 200g", "Autan Aerosol 177ml",
    "Nopikex Liquido 120ml", "Nopikex Barra 50g",
    "Bacterion Family Spray 150ml", "Stay Off Ninos Spray 120ml",
    "Bacterion Kids Crema 120g"),
  stringsAsFactors = FALSE)
print(comparables)

# funcion para clasificar cada producto
# basicamente reviso marca + palabras clave + contenido
clasificar <- function(row) {
  n  <- tolower(row["nombre_producto"])
  m  <- tolower(row["marca"])
  cv <- as.numeric(row["contenido_valor"])
  cu <- tolower(row["contenido_unidad"])

  # Stay Off
  if (grepl("stay.?off", m)) {
    if (grepl("adulto", n) && grepl("spray|ato", n) &&
        !grepl("roll|amazonic|crema", n) &&
        !is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") return("P01")
    if (grepl("amazonic", n) && grepl("spray|ato", n) && !grepl("roll|crema", n) &&
        !is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") return("P02")
    # el de ninos en La Rebaja aparece como "FSPRAY" con unidad "unidad"
    if ((grepl("ni.o|nino|kids", n) || grepl("fspray", n)) &&
        grepl("spray|fspray|ato", n) && !grepl("amazonic|roll|bebe|baby", n)) {
      if ((!is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") ||
          (!is.na(cu) && cu == "unidad")) return("P08")
    }
  }
  # Autan
  if (grepl("autan", m)) {
    if (grepl("crema|cream", n) && !grepl("kids|ni.o", n) &&
        !is.na(cv) && cv >= 190 && cv <= 210 && cu %in% c("g","ml")) return("P03")
    # el aerosol en Exito dice "spray", en La Rebaja dice "aerosol"
    if (grepl("aerosol|spray|liquido|liquid|íquido", n) && !grepl("crema", n) &&
        !is.na(cv) && cv >= 170 && cv <= 180 && cu == "ml") return("P04")
  }
  # Nopikex
  if (grepl("nopikex", m) || grepl("nopikex|nopikes", n)) {
    if (!grepl("barra|solid", n) && !grepl("ni.o|nino|kids", n) &&
        !is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") return("P05")
    if (!is.na(cv) && cv >= 45 && cv <= 55 &&
        (grepl("barra|solid", n) || cu == "g")) return("P06")
  }
  # Bacterion
  if (grepl("bacterion", m)) {
    if (grepl("family|familiar", n) && !grepl("kids|baby|botanik|selvatic", n) &&
        !is.na(cv) && cv >= 140 && cv <= 160 && cu == "ml") return("P07")
    if (grepl("kids|ni.o|nino", n) && grepl("crema", n) &&
        !is.na(cv) && cv >= 110 && cv <= 160 && cu %in% c("g","ml")) return("P09")
  }
  NA_character_
}

obs$prod_id <- apply(obs, 1, clasificar)
obs$comparable <- as.integer(!is.na(obs$prod_id))
obs$nombre_std <- comparables$nombre[match(obs$prod_id, comparables$id)]

n_comp <- sum(obs$comparable == 1 & obs$precio > 0, na.rm = TRUE)
cat("Observaciones comparables con precio:", n_comp, "\n")

# verificar que cada producto este en los 3 comercios
check <- obs %>% filter(comparable == 1, precio > 0) %>%
  group_by(prod_id, nombre_std) %>%
  summarise(n = n_distinct(comercio_id), .groups = "drop")
print(check)

# guardar en la base
dbExecute(con, "UPDATE observaciones SET comparable=0, producto_comun_id=NULL, nombre_estandarizado=NULL")
for (i in seq_len(nrow(obs))) {
  if (!is.na(obs$prod_id[i])) {
    dbExecute(con, "UPDATE observaciones SET comparable=1, producto_comun_id=?,
      nombre_estandarizado=?, marca_estandarizada=?, contenido_unidad=?
      WHERE observacion_id=?",
      params = list(obs$prod_id[i], obs$nombre_std[i],
                    obs$marca_std[i], obs$contenido_unidad[i], obs$observacion_id[i]))
  } else {
    dbExecute(con, "UPDATE observaciones SET marca_estandarizada=?, contenido_unidad=?
      WHERE observacion_id=?",
      params = list(obs$marca_std[i], obs$contenido_unidad[i], obs$observacion_id[i]))
  }
}

# =========================================================
# ESTRUCTURA DE LA BASE
# =========================================================

cat("\n-- Tablas en la base --\n")
for (tbl in dbListTables(con)) {
  cat(tbl, ":\n")
  info <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tbl))
  print(info[, c("name","type")])
  cat("\n")
}

# =========================================================
# CONSULTAS SQL
# =========================================================

# 1. Cuantos comercios, productos y observaciones hay?
cat("\n=== Pregunta 1: Contenido de la base ===\n")
print(dbGetQuery(con, "
  SELECT COUNT(DISTINCT comercio_id) AS comercios,
         COUNT(DISTINCT producto_id_comercio || comercio_id) AS productos,
         COUNT(*) AS observaciones
  FROM observaciones"))

print(dbGetQuery(con, "
  SELECT c.nombre, COUNT(*) AS total,
    SUM(CASE WHEN comparable=1 THEN 1 ELSE 0 END) AS comparables
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  GROUP BY c.nombre"))

cat("La Rebaja tiene menos productos porque es drogueria, Exito tiene muchos\n")
cat("porque es marketplace.\n")

# 2. Se cumplen las 24 observaciones comparables?
cat("\n=== Pregunta 2: Observaciones comparables ===\n")
q2 <- dbGetQuery(con, "
  SELECT c.nombre, COUNT(*) AS comparables
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  WHERE comparable=1 AND precio > 0
  GROUP BY c.nombre")
print(q2)
cat("Total:", sum(q2$comparables), "(se necesitan minimo 24)\n")

# 3. Precio min, max, promedio por producto
cat("\n=== Pregunta 3: Precios por producto ===\n")
q3 <- dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto,
    MIN(precio) AS minimo, MAX(precio) AS maximo,
    ROUND(AVG(precio)) AS promedio
  FROM observaciones WHERE comparable=1 AND precio > 0
  GROUP BY nombre_estandarizado")
print(q3)

# cual es el mas barato en cada producto
cat("\nComercio mas barato por producto:\n")
print(dbGetQuery(con, "
  SELECT o.nombre_estandarizado AS producto, c.nombre AS comercio, o.precio
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  WHERE comparable=1 AND precio > 0
    AND precio = (SELECT MIN(precio) FROM observaciones o2
                  WHERE o2.nombre_estandarizado = o.nombre_estandarizado AND precio > 0)
  ORDER BY producto"))

cat("En general Exito y Jumbo tienen mejores precios que La Rebaja.\n")

# 4. Ahorro potencial
cat("\n=== Pregunta 4: Ahorro al comprar en el mas barato ===\n")
q4 <- dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto,
    MAX(precio) - MIN(precio) AS ahorro_pesos,
    ROUND(100.0 * (MAX(precio) - MIN(precio)) / MAX(precio), 1) AS ahorro_pct
  FROM observaciones WHERE comparable=1 AND precio > 0
  GROUP BY nombre_estandarizado
  ORDER BY ahorro_pct DESC")
print(q4)
cat("El Stay Off Adulto y el Autan Crema son donde mas se ahorra (>35%).\n")

# 5. Precio por unidad de contenido
cat("\n=== Pregunta 5: Precio por mL o g ===\n")
print(dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto, c.nombre AS comercio,
    precio, contenido_valor, contenido_unidad,
    ROUND(precio / NULLIF(contenido_valor, 0), 1) AS precio_x_unidad
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  WHERE comparable=1 AND precio > 0 AND contenido_valor > 0
    AND contenido_unidad != 'unidad'
  ORDER BY contenido_unidad, producto, precio_x_unidad"))

# 6. Mayores diferencias de precio
cat("\n=== Pregunta 6: Donde hay mas diferencia de precio ===\n")
print(dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto,
    MAX(precio) - MIN(precio) AS diferencia,
    ROUND(100.0 * (MAX(precio)-MIN(precio)) / MAX(precio), 1) AS pct
  FROM observaciones WHERE comparable=1 AND precio > 0
  GROUP BY nombre_estandarizado ORDER BY pct DESC"))

# 7. Datos faltantes
cat("\n=== Pregunta 7: Datos faltantes ===\n")
print(dbGetQuery(con, "
  SELECT
    SUM(CASE WHEN precio IS NULL OR precio=0 THEN 1 ELSE 0 END) AS sin_precio,
    SUM(CASE WHEN marca IS NULL OR marca='' THEN 1 ELSE 0 END) AS sin_marca,
    SUM(CASE WHEN contenido_valor IS NULL THEN 1 ELSE 0 END) AS sin_contenido,
    COUNT(*) AS total
  FROM observaciones"))
cat("Los de precio 0 son de Exito, productos que no estaban disponibles.\n")
cat("Se excluyen de las comparaciones.\n")

# 8. Tamano de la oferta
cat("\n=== Pregunta 8: Oferta por comercio ===\n")
print(dbGetQuery(con, "
  SELECT c.nombre, COUNT(*) AS total,
    SUM(CASE WHEN precio > 0 THEN 1 ELSE 0 END) AS disponibles,
    ROUND(100.0*SUM(CASE WHEN contenido_valor IS NOT NULL THEN 1 ELSE 0 END)/COUNT(*),1) AS pct_con_contenido
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  GROUP BY c.nombre ORDER BY total DESC"))

# =========================================================
# GRAFICOS
# =========================================================

colores <- c("Exito" = "#FDDA24", "Jumbo Colombia" = "#E31937", "La Rebaja Virtual" = "#00A651")

# grafico 1: comparacion de precios
viz <- dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto, c.nombre AS comercio, precio
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  WHERE comparable=1 AND precio > 0")

g1 <- ggplot(viz, aes(x = reorder(producto, precio), y = precio, fill = comercio)) +
  geom_col(position = "dodge", width = 0.7) + coord_flip() +
  scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = ","))) +
  scale_fill_manual(values = colores) +
  labs(title = "Precios por producto y comercio", x = NULL, y = "Precio (COP)", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
print(g1)

# grafico 2: ahorro porcentual
g2 <- ggplot(q4, aes(x = reorder(producto, ahorro_pct), y = ahorro_pct)) +
  geom_col(fill = "#2E86AB", width = 0.7) +
  geom_text(aes(label = paste0(ahorro_pct, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Ahorro si se elige el comercio mas barato", x = NULL, y = "Ahorro (%)") +
  theme_minimal() + expand_limits(y = max(q4$ahorro_pct) * 1.15)
print(g2)

# grafico 3: precio por mL
ppu <- dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto, c.nombre AS comercio,
    ROUND(precio / NULLIF(contenido_valor,0), 1) AS cop_ml, contenido_unidad
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  WHERE comparable=1 AND precio > 0 AND contenido_valor > 0 AND contenido_unidad='ml'")

g3 <- ggplot(ppu, aes(x = reorder(producto, cop_ml), y = cop_ml, fill = comercio)) +
  geom_col(position = "dodge", width = 0.7) + coord_flip() +
  scale_fill_manual(values = colores) +
  labs(title = "Precio por mililitro", x = NULL, y = "COP/mL", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
print(g3)

# grafico 4: oferta por marca
marcas <- dbGetQuery(con, "
  SELECT c.nombre AS comercio,
    CASE WHEN marca_estandarizada IN ('Stay Off','Autan','Nopikex','Bacterion','Raid','Baygon','Off!')
         THEN marca_estandarizada ELSE 'Otras' END AS marca,
    COUNT(*) AS n
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  GROUP BY comercio, marca")

g4 <- ggplot(marcas, aes(x = comercio, y = n, fill = marca)) +
  geom_col(position = "stack") +
  labs(title = "Marcas por comercio", x = NULL, y = "Productos", fill = "Marca") +
  theme_minimal()
print(g4)

# =========================================================
# CONCLUSIONES
# =========================================================

cat("\n=== CONCLUSIONES ===\n\n")

cat("Mejor precio por producto:\n")
print(dbGetQuery(con, "
  SELECT nombre_estandarizado AS producto, c.nombre AS donde, precio
  FROM observaciones o JOIN comercios c ON o.comercio_id = c.comercio_id
  WHERE comparable=1 AND precio > 0
    AND precio = (SELECT MIN(precio) FROM observaciones o2
                  WHERE o2.nombre_estandarizado = o.nombre_estandarizado AND precio > 0)
  ORDER BY producto"))

cat("\n")
cat("- Exito y Jumbo tienen mejores precios, con ahorros de hasta 37%\n")
cat("  comparado con La Rebaja (que es drogueria y cobra mas caro).\n")
cat("- Solo en Nopikex Liquido La Rebaja es la mas barata.\n")
cat("- Vale la pena comparar sobre todo en Stay Off y Autan\n")
cat("  que son los que mas varian entre tiendas.\n")
cat("- Ojo: los precios son del momento de la consulta, pueden cambiar.\n")
cat("  Algunos productos de Exito tenian precio $0 (no disponibles).\n")

dbDisconnect(con)
