# limpieza_sqlite.R -- Taller 1, Minería de Datos
# Filtro de la categoría, limpieza, correspondencia de productos y carga a SQLite.
#
# Uso:
#   source("scraper.R"); source("limpieza_sqlite.R")
#   res   <- readRDS("brutos_run_YYYYMMDD_HHMMSS.rds")   # o: res <- recolectar_todo()
#   datos <- limpiar_datos(res$datos)
#   cargar_sqlite(datos, res$resumen)                    # crea/actualiza comparador_precios.sqlite

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

# ======================================================================
# 1. Correspondencia manual de productos comparables
#    Regla: mismo EAN en Éxito, Jumbo y Olímpica + verificación manual de
#    marca, variante y contenido en los tres anuncios.
# ======================================================================
CORRESPONDENCIA <- data.frame(
  producto_id = sprintf("P%02d", 1:10),
  ean_n = c("7702215130109", "7702215130208", "7707194538632", "7707194530872",
            "7509552787009", "7702044290517", "7707109924550", "7702277082545",
            "7702277080343", "7702277078821"),
  producto_estandar = c("Removedor de esmalte Lander tradicional 55 ml",
                        "Removedor de esmalte Lander tradicional 75 ml",
                        "Removedor de esmalte Masglo aloe vera 60 ml",
                        "Esmalte Masglo Gel Evolution Linda 13.5 ml",
                        "Esmalte Vogue efecto gel Guayaquil 14 ml",
                        "Esmalte Vitú Gel tono 10 Blanco 8 ml",
                        "Base Masglo Clinical Total Care Violeta 13.5 ml",
                        "Cortaúñas grande Freestyle",
                        "Lima curva manicure E-09 Freestyle",
                        "Cortacutícula básico Freestyle"),
  marca_estandar = c("Lander", "Lander", "Masglo", "Masglo", "Vogue", "Vitú",
                     "Masglo", "Freestyle", "Freestyle", "Freestyle"),
  subcategoria = c("Removedores", "Removedores", "Removedores", "Esmaltes",
                   "Esmaltes", "Esmaltes", "Bases, tratamientos y brillos",
                   "Cortaúñas", "Limas", "Alicates y cortacutículas"),
  contenido = c(55, 75, 60, 13.5, 14, 8, 13.5, 1, 1, 1),
  unidad = c(rep("ml", 7), rep("und", 3)),
  principal = c(rep(1L, 8), 0L, 0L),   # P09 y P10: comparables extra (reserva)
  stringsAsFactors = FALSE)

# ======================================================================
# 2. Filtro de la categoría (reglas documentadas, con motivo de exclusión)
# ======================================================================
clasificar_categoria <- function(d, excluir_drills = TRUE) {
  nombre <- d$nombre_original
  marca  <- tolower(d$marca_original); marca[is.na(marca)] <- ""
  es_exito <- d$comercio == "exito"

  pat_unas     <- "(?<![[:alpha:]])[uú][ñn#]as?(?![[:alpha:]])|esmalt|removedor|quitaesmalt|endurec|cut[ií]cula|manicur|pedicur|alicate|cort[aá][uú][ñn#]|(^| )limas?( |$)"
  pat_lamparas <- "l[aá]mpara|fototerapia|\\bleds?\\b|\\blcd\\b|\\bnail dryer\\b|infrarrojo|colector (de )?polvo|guantes especiales"
  pat_pies     <- "callo|p[oó]mez|archivo de pie|cuidado de pies|juanete|talones|footlogix|separador de dedos"
  pat_aero     <- "aer[oó]grafo"
  pat_drill    <- "drill|\\brpm\\b|pulidora|pulidor.*(inal[aá]mbrico|el[eé]ctric|mercedes)|grabado pluma|repuesto l[aá]piz"
  marcas_ajenas <- c("moringa", "saloon in", "framesi", "e+n", "scunci", "shock", "mundo bebe",
                     "cerave", "uriage", "la roche posay", "arches & halos", "nobelsound",
                     "capibell", "smart")
  pat_ajenos <- "antibacterial|shampoo|cabello|cuero cabelludo|capilar|anticaida|crema.*manos|base l[ií]quida|serum facial|brochas|calentad|olla |difusor|dental|tijera|ceja|peine"
  excl_manual <- c("Brillo labial Vitú tono 4 miel lima x6.5ml",
                   "Brillo Vitu Aceite Almendras Tn.2 Lima x10ml")

  g <- function(p, x = nombre) grepl(p, x, ignore.case = TRUE, perl = TRUE)
  marcar <- function(actual, cond, motivo) ifelse(is.na(actual) & cond, motivo, actual)
  masglo_unas <- marca == "masglo" & g("13[.,]5 ?ml|dilusor|secante")

  m <- rep(NA_character_, nrow(d))
  m <- marcar(m, g("Mundo Beb", d$categoria_consulta), "cortaúñas de bebé")
  m <- marcar(m, g("baby ?& ?kids") | nombre %in% excl_manual |
                 g("^kit para decoraci[oó]n de u[ñn]as$") | marca == "h la cosedora",
              "exclusión manual")
  m <- marcar(m, g(pat_lamparas), "lámparas/equipos de secado")
  m <- marcar(m, g(pat_pies),     "cuidado de pies")
  m <- marcar(m, g(pat_aero),     "aerógrafo")
  m <- marcar(m, excluir_drills & g(pat_drill), "drills/pulidores eléctricos")
  m <- marcar(m, es_exito & (marca %in% marcas_ajenas | g(pat_ajenos)), "producto ajeno a uñas")
  pasa <- g(pat_unas, paste(nombre, d$categoria_api)) | masglo_unas
  m <- marcar(m, !es_exito & !pasa, "no es de uñas (filtro por palabras)")

  d$motivo_exclusion <- m
  d$incluido <- is.na(m)
  d
}

# ======================================================================
# 3. Estandarización de marca, contenido y subcategoría
# ======================================================================
estandarizar_marca <- function(x) {
  k <- trimws(chartr("áéíóúüñ", "aeiouun", tolower(x)))
  out <- rep(NA_character_, length(k))
  ok <- !is.na(k) & k != ""
  out[ok] <- tools::toTitleCase(k[ok])
  out[ok & k == "opi"]      <- "OPI"
  out[ok & k == "vitu"]     <- "Vitú"
  out[ok & k == "generico"] <- "Genérico"
  out
}

# Primer "número + unidad" del texto; ml, g y und como unidades comunes.
# 1 L = 1000 mL; 1 fl oz = 29.5735 mL.
extraer_contenido <- function(texto) {
  texto[is.na(texto)] <- ""
  pat <- "(\\d+(?:[.,]\\d+)?)\\s*(ml|cc|gr|gms?|g|oz|lts?|l|unds?|uds?|pcs?|pzas?|piezas|unidades)(?![[:alpha:]])"
  m <- regmatches(texto, regexec(pat, texto, ignore.case = TRUE, perl = TRUE))
  val <- vapply(m, function(z) if (length(z)) as.numeric(sub(",", ".", z[2])) else NA_real_, 0)
  uni <- vapply(m, function(z) if (length(z)) tolower(z[3]) else NA_character_, "")
  mult <- rep(1, length(uni)); un <- rep(NA_character_, length(uni))
  esml <- uni %in% c("ml", "cc");             un[esml] <- "ml"
  esl  <- uni %in% c("l", "lt", "lts");       un[esl]  <- "ml"; mult[esl] <- 1000
  eso  <- uni %in% "oz";                      un[eso]  <- "ml"; mult[eso] <- 29.5735
  esg  <- uni %in% c("g", "gr", "gm", "gms"); un[esg]  <- "g"
  esu  <- uni %in% c("und", "unds", "ud", "uds", "pc", "pcs", "pza", "pzas", "piezas", "unidades")
  un[esu] <- "und"
  data.frame(contenido_valor = round(val * mult, 2), contenido_unidad = un,
             stringsAsFactors = FALSE)
}

asignar_subcategoria <- function(nombre) {
  g <- function(p) grepl(p, nombre, ignore.case = TRUE, perl = TRUE)
  s <- rep("Otros", length(nombre))
  # se asigna de la regla más débil a la más fuerte (la última que aplica gana)
  s[g("esmalt|esmate|laca|gelcolor|nail polish|enamel|\\bpolish\\b")] <- "Esmaltes"
  s[g("endurec|fortalec|recuperador|regenerador|antiescam|top coat|\\bbase\\b|tratamiento|primer|secante|matific|\\bbrillo\\b") &
      !g("^esmalt")] <- "Bases, tratamientos y brillos"
  s[g("cut[ií]cula|cuticula")]                              <- "Cuidado de cutículas"
  s[g("postiz|press on|u[ñn]as falsas")]                    <- "Uñas postizas"
  s[g("removedor|remover|quitaesmalt|\\bremov\\b|dilusor")] <- "Removedores"
  s[g("(^| )(lima|limas|nail file)( |$)|lima curva")]       <- "Limas"
  s[g("alicate|cortacut|corta cut")]                        <- "Alicates y cortacutículas"
  s[g("cort[aá][uú][ñn#]|corta u[ñn]a|cortau")]             <- "Cortaúñas"
  s
}

limpiar_datos <- function(d, excluir_drills = TRUE) {
  d <- clasificar_categoria(d, excluir_drills)

  d$ean_n <- sub("^0+", "", gsub("\\D", "", d$ean))
  d$ean_n[!is.na(d$ean_n) & nchar(d$ean_n) < 8] <- NA

  d$marca_estandar <- estandarizar_marca(d$marca_original)
  ct <- extraer_contenido(ifelse(is.na(d$nombre_original), d$nombre_sku_original, d$nombre_original))
  d$contenido_valor  <- ct$contenido_valor
  d$contenido_unidad <- ct$contenido_unidad
  d$subcategoria <- asignar_subcategoria(d$nombre_original)

  # correspondencia manual: pisa los valores automáticos de los productos comparables
  i  <- match(d$ean_n, CORRESPONDENCIA$ean_n)
  ok <- !is.na(i)
  d$producto_id <- CORRESPONDENCIA$producto_id[i]
  d$marca_estandar[ok]   <- CORRESPONDENCIA$marca_estandar[i[ok]]
  d$subcategoria[ok]     <- CORRESPONDENCIA$subcategoria[i[ok]]
  d$contenido_valor[ok]  <- CORRESPONDENCIA$contenido[i[ok]]
  d$contenido_unidad[ok] <- CORRESPONDENCIA$unidad[i[ok]]

  d$comparable <- as.integer(d$incluido & !is.na(d$producto_id) & !is.na(d$precio_cop))
  d
}

# ======================================================================
# 4. Esquema SQLite
# ======================================================================
ESQUEMA_SQL <- "
CREATE TABLE IF NOT EXISTS comercios (
  comercio_id INTEGER PRIMARY KEY,
  nombre      TEXT NOT NULL UNIQUE
);
CREATE TABLE IF NOT EXISTS ejecuciones (
  ejecucion_id      TEXT NOT NULL,
  comercio_id       INTEGER NOT NULL REFERENCES comercios(comercio_id),
  fecha_hora_inicio TEXT NOT NULL,
  n_registros       INTEGER,
  estado            TEXT NOT NULL CHECK (estado IN ('completa','parcial','fallida')),
  PRIMARY KEY (ejecucion_id, comercio_id)
);
CREATE TABLE IF NOT EXISTS productos (
  producto_id       TEXT PRIMARY KEY,
  ean               TEXT NOT NULL,
  producto_estandar TEXT NOT NULL,
  marca_estandar    TEXT,
  subcategoria      TEXT,
  contenido         REAL,
  unidad            TEXT,
  principal         INTEGER NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS anuncios (
  anuncio_id              INTEGER PRIMARY KEY AUTOINCREMENT,
  comercio_id             INTEGER NOT NULL REFERENCES comercios(comercio_id),
  product_id_comercio     TEXT NOT NULL,
  sku                     TEXT NOT NULL,
  ean                     TEXT,
  nombre_original         TEXT,
  nombre_sku_original     TEXT,
  marca_original          TEXT,
  descripcion_original    TEXT,
  ficha_tecnica_original  TEXT,
  categoria_api           TEXT,
  categoria_consulta      TEXT,
  vendedor                TEXT,
  url                     TEXT,
  unidad_medida           TEXT,
  multiplicador_unidad    REAL,
  marca_estandar          TEXT,
  subcategoria            TEXT,
  contenido_valor         REAL,
  contenido_unidad        TEXT,
  producto_id             TEXT REFERENCES productos(producto_id),
  incluido                INTEGER NOT NULL,
  motivo_exclusion        TEXT,
  UNIQUE (comercio_id, product_id_comercio, sku)
);
CREATE TABLE IF NOT EXISTS observaciones (
  observacion_id      INTEGER PRIMARY KEY AUTOINCREMENT,
  anuncio_id          INTEGER NOT NULL REFERENCES anuncios(anuncio_id),
  ejecucion_id        TEXT NOT NULL,
  fecha_hora_consulta TEXT NOT NULL,
  precio_cop          REAL,
  precio_lista_cop    REAL,
  en_promocion        INTEGER,
  disponible          INTEGER,
  comparable          INTEGER NOT NULL DEFAULT 0,
  UNIQUE (anuncio_id, fecha_hora_consulta)
);
CREATE INDEX IF NOT EXISTS idx_obs_ejecucion ON observaciones(ejecucion_id);
CREATE INDEX IF NOT EXISTS idx_anuncios_producto ON anuncios(producto_id);
CREATE VIEW IF NOT EXISTS v_comparables AS
SELECT o.ejecucion_id, o.fecha_hora_consulta, c.nombre AS comercio,
       p.producto_id, p.producto_estandar, p.marca_estandar, p.subcategoria,
       p.contenido, p.unidad, o.precio_cop, o.precio_lista_cop, o.en_promocion,
       a.vendedor, a.url, a.nombre_original
FROM observaciones o
JOIN anuncios  a ON a.anuncio_id  = o.anuncio_id
JOIN comercios c ON c.comercio_id = a.comercio_id
JOIN productos p ON p.producto_id = a.producto_id
WHERE o.comparable = 1
"

crear_esquema <- function(con) {
  dbExecute(con, "PRAGMA foreign_keys = ON")
  sentencias <- trimws(strsplit(ESQUEMA_SQL, ";")[[1]])
  for (s in sentencias[nzchar(sentencias)]) dbExecute(con, s)
  invisible(TRUE)
}

# ======================================================================
# 5. Carga (sin sobrescribir observaciones anteriores)
# ======================================================================
inicio_iso <- function(ejecucion_id) {
  t <- as.POSIXct(sub("^run_", "", ejecucion_id), format = "%Y%m%d_%H%M%S", tz = "America/Bogota")
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(t, "%Y-%m-%dT%H:%M:%S%z"))
}

cargar_sqlite <- function(datos, resumen, ruta_db = "comparador_precios.sqlite") {
  con <- dbConnect(SQLite(), ruta_db)
  on.exit(dbDisconnect(con))
  crear_esquema(con)

  dbWithTransaction(con, {
    # comercios
    dbExecute(con, "INSERT OR IGNORE INTO comercios (nombre) VALUES (?)",
              params = list(unique(datos$comercio)))
    cm <- dbGetQuery(con, "SELECT comercio_id, nombre FROM comercios")
    datos$comercio_id <- cm$comercio_id[match(datos$comercio, cm$nombre)]

    # productos comparables (correspondencia)
    p <- CORRESPONDENCIA
    dbExecute(con,
      "INSERT INTO productos (producto_id, ean, producto_estandar, marca_estandar,
                              subcategoria, contenido, unidad, principal)
       VALUES (?,?,?,?,?,?,?,?)
       ON CONFLICT(producto_id) DO UPDATE SET
         ean = excluded.ean, producto_estandar = excluded.producto_estandar,
         marca_estandar = excluded.marca_estandar, subcategoria = excluded.subcategoria,
         contenido = excluded.contenido, unidad = excluded.unidad, principal = excluded.principal",
      params = list(p$producto_id, p$ean_n, p$producto_estandar, p$marca_estandar,
                    p$subcategoria, p$contenido, p$unidad, p$principal))

    # anuncios: una fila por comercio + producto + sku; el texto original no se modifica
    a <- datos[!duplicated(datos[, c("comercio_id", "product_id", "sku")]), ]
    dbExecute(con,
      "INSERT INTO anuncios (comercio_id, product_id_comercio, sku, ean, nombre_original,
         nombre_sku_original, marca_original, descripcion_original, ficha_tecnica_original,
         categoria_api, categoria_consulta, vendedor, url, unidad_medida, multiplicador_unidad,
         marca_estandar, subcategoria, contenido_valor, contenido_unidad, producto_id,
         incluido, motivo_exclusion)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
       ON CONFLICT(comercio_id, product_id_comercio, sku) DO UPDATE SET
         marca_estandar = excluded.marca_estandar, subcategoria = excluded.subcategoria,
         contenido_valor = excluded.contenido_valor, contenido_unidad = excluded.contenido_unidad,
         producto_id = excluded.producto_id, incluido = excluded.incluido,
         motivo_exclusion = excluded.motivo_exclusion",
      params = list(a$comercio_id, a$product_id, a$sku, a$ean, a$nombre_original,
                    a$nombre_sku_original, a$marca_original, a$descripcion_original,
                    a$ficha_tecnica_original, a$categoria_api, a$categoria_consulta,
                    a$vendedor, a$url, a$unidad_medida, a$multiplicador_unidad,
                    a$marca_estandar, a$subcategoria, a$contenido_valor, a$contenido_unidad,
                    a$producto_id, as.integer(a$incluido), a$motivo_exclusion))

    # observaciones: INSERT OR IGNORE respeta la unicidad (anuncio, fecha y hora)
    ids <- dbGetQuery(con, "SELECT anuncio_id, comercio_id, product_id_comercio, sku FROM anuncios")
    llave_ids <- paste(ids$comercio_id, ids$product_id_comercio, ids$sku)
    datos$anuncio_id <- ids$anuncio_id[match(paste(datos$comercio_id, datos$product_id, datos$sku), llave_ids)]
    n_nuevas <- dbExecute(con,
      "INSERT OR IGNORE INTO observaciones (anuncio_id, ejecucion_id, fecha_hora_consulta,
         precio_cop, precio_lista_cop, en_promocion, disponible, comparable)
       VALUES (?,?,?,?,?,?,?,?)",
      params = list(datos$anuncio_id, datos$ejecucion_id, datos$fecha_hora_consulta,
                    datos$precio_cop, datos$precio_lista_cop, as.integer(datos$en_promocion),
                    as.integer(datos$disponible), datos$comparable))

    # ejecuciones: una fila por corrida y comercio
    est <- aggregate(cbind(total_sitio, n_productos, n_omitidos) ~ comercio + ejecucion_id,
                     data = resumen, FUN = sum)
    est$estado <- ifelse(est$n_productos == 0, "fallida",
                  ifelse(est$n_omitidos > 0 | est$n_productos < est$total_sitio, "parcial", "completa"))
    est$n_registros <- as.integer(table(datos$comercio, datos$ejecucion_id)[cbind(est$comercio, est$ejecucion_id)])
    est$comercio_id <- cm$comercio_id[match(est$comercio, cm$nombre)]
    dbExecute(con,
      "INSERT OR IGNORE INTO ejecuciones (ejecucion_id, comercio_id, fecha_hora_inicio, n_registros, estado)
       VALUES (?,?,?,?,?)",
      params = list(est$ejecucion_id, est$comercio_id, inicio_iso(est$ejecucion_id),
                    est$n_registros, est$estado))
    message("Observaciones nuevas insertadas: ", n_nuevas)
  })
  invisible(ruta_db)
}
