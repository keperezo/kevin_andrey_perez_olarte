# =============================================================================
# limpieza.R  -  Limpieza, clasificación y correspondencia de productos
# (Taller 1, Minería de Datos, UNAL)  Categoría 29: Medicamentos para dolor e inflamación
#
# Tema: "Analgésicos, antipiréticos y antiinflamatorios, incluidas presentaciones tópicas.
#        Excluye combinaciones específicas para síntomas gripales".
#
# Este módulo NO modifica el texto original: todas las versiones estandarizadas se guardan en
# columnas/tablas propias. Lo llama taller_1.Rmd:
#     source("limpieza.R")
#     d <- ejecutar_limpieza(con, ejecucion_id)      # lee SQLite, limpia, clasifica, empareja
#     guardar_limpieza(con, d)                       # escribe clasificacion/productos/correspondencia
#
# REGLAS (resumen; el detalle está en cada función):
#   R-cat   Pertenencia a la categoría por PRINCIPIO ACTIVO, no por la sección del sitio:
#             1 = incluido (hay un analgésico/antipirético/antiinflamatorio),
#             0 = excluido (con motivo: combinación gripal, otro principio activo, ...),
#             NA = revisar (no se puede comprobar; no se clasifica arbitrariamente).
#   R1      Misma presentación en distintos comercios = mismo EAN Y mismo contenido
#           (un mismo EAN puede venderse como blíster x10 y como caja x100: son presentaciones
#           distintas).
#   R2      Sin EAN: se asigna a un producto ya identificado por EAN solo si coinciden marca,
#           principios activos, concentración y contenido y la coincidencia es única.
#   Compar. Un producto es comparable si, en la MISMA ejecución, está en todos los comercios,
#           pertenece a la categoría, no es paquete promocional, tiene precio y contenido, y no
#           tiene anuncios duplicados con precios contradictorios.
#
# Paquetes: DBI, RSQLite (solo base R para el resto)
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

# ---- Texto -------------------------------------------------------------------
# Los caracteres acentuados se generan por código Unicode (no como literales del archivo) para que
# el resultado no dependa de la configuración regional del equipo (Windows, Linux, contenedores).
.TILDES_DE <- vapply(c(225, 233, 237, 243, 250, 252, 241, 193, 201, 205, 211, 218, 220, 209),
                     function(cp) intToUtf8(cp), character(1))   # á é í ó ú ü ñ Á É Í Ó Ú Ü Ñ
.TILDES_A  <- c("a", "e", "i", "o", "u", "u", "n", "A", "E", "I", "O", "U", "U", "N")
sin_tildes <- function(x) {
  x <- as.character(x)
  Encoding(x) <- "UTF-8"          # RSQLite/jsonlite entregan UTF-8; se marca de forma explícita
  for (i in seq_along(.TILDES_DE)) x <- gsub(.TILDES_DE[i], .TILDES_A[i], x, fixed = TRUE)
  x
}
norm_txt <- function(x) gsub("\\s+", " ", trimws(toupper(sin_tildes(x))))

# Valor de un campo de la ficha técnica dentro de texto_original ("Campo: valor | Campo2: ...")
get_spec <- function(tn, campo) {
  patron <- paste0("(?:^|\\| )", campo, ": ([^|]*)")
  m <- regmatches(tn, regexec(patron, tn, perl = TRUE))
  vapply(m, function(z) if (length(z) >= 2) trimws(z[2]) else NA_character_, character(1))
}

num_txt <- function(x) format(x, trim = TRUE, scientific = FALSE, digits = 12)
a_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))

# ---- Diccionarios de principios activos (texto normalizado: mayúsculas, sin tildes) -----
# A = analgésicos / antipiréticos / antiinflamatorios (incluye tópicos): determinan la inclusión
ACT_A <- c(
  ACETAMINOFEN      = "ACETAMINOF|ACETAMONIFEN|PARACETAMOL",
  IBUPROFENO        = "IBUPROFEN|\\bBUPROFEN",
  NAPROXENO         = "NAPROXEN",
  DICLOFENACO       = "DICLOFENAC",
  DEXKETOPROFENO    = "DEXKETOPROFEN",
  KETOPROFENO       = "(?<!DEX)KETOPROFEN",
  KETOROLACO        = "KETOROLAC",
  PIROXICAM         = "PIROXICAM",
  MELOXICAM         = "MELOXICAM",
  ETORICOXIB        = "ETORICOXIB",
  CELECOXIB         = "CELECOXIB",
  INDOMETACINA      = "INDOMETACIN",
  NIMESULIDA        = "NIMESULID",
  NABUMETONA        = "NABUMETON",
  FLURBIPROFENO     = "FLURBIPROFEN",
  ACIDO_MEFENAMICO  = "ACIDO MEFENAMICO",
  CLONIXINATO       = "CLONIXIN",
  ACIDO_ACETILSALICILICO = "ACETILSALICILIC|ASPIRINA|\\bASA\\b",
  DIPIRONA          = "DIPIRONA|METAMIZOL|NORAMIDOPIRINA",
  TRAMADOL          = "TRAMADOL",
  CODEINA           = "CODEINA",
  HIDROCODONA       = "HIDROCODONA",
  SALICILATO_METILO = "SALICILATO DE METILO|METIL SALICILATO|SALICILATO METILO",
  MENTOL            = "MENTOL",
  ALCANFOR          = "ALCANFOR|CANFOR",
  CAPSAICINA        = "CAPSAICIN",
  BENCIDAMINA       = "BENCIDAMINA|BENZIDAMINA"
)
# Adyuvantes / otros reconocidos (no determinan la inclusión por sí solos)
ACT_ADY <- c(
  CAFEINA = "CAFEIN", ISOMETEPTENO = "ISOMETEPTEN",
  HIOSCINA = "HIOSCIN|BUTILBROMURO|BUTILESCOPOLAMIN",
  RELAJANTE_MUSCULAR = "ORFENADRIN|TIZANIDIN|CICLOBENZAPRIN|METOCARBAMOL|CARISOPRODOL|BACLOFEN"
)
# Combinaciones gripales: antigripales/descongestionantes/antitusivos asociados a un analgésico
ACT_GRIPAL <- c(
  FENILEFRINA = "FENILEFRIN", PSEUDOEFEDRINA = "PSEUDOEFEDRIN", CLORFENIRAMINA = "CLORFENIRAMIN|CLORFENAMIN",
  DEXTROMETORFANO = "DEXTROMETORFAN", GUAIFENESINA = "GUAIFENESIN", AMBROXOL = "AMBROXOL",
  BROMHEXINA = "BROMHEXIN", LORATADINA = "LORATADIN", DIFENHIDRAMINA = "DIFENHIDRAMIN",
  FENILPROPANOLAMINA = "FENILPROPANOLAMIN", OXIMETAZOLINA = "OXIMETAZOLIN"
)
PAL_GRIPAL <- "\\bGRIPA|GRIPAL|ANTIGRIP|RESFRIAD|DESCONGEST|CONGESTION NASAL"
# Ambiguos: podrían ser analgésicos según el uso; se marcan "revisar" (no se clasifican a la fuerza)
ACT_REVISAR <- c(
  LIDOCAINA = "LIDOCAIN", SUMATRIPTAN = "SUMATRIPTAN", RIZATRIPTAN = "RIZATRIPTAN",
  ZOLMITRIPTAN = "ZOLMITRIPTAN", ERGOTAMINA = "ERGOTAMIN", PREGABALINA = "PREGABALIN",
  GABAPENTINA = "GABAPENTIN"
)

detectar <- function(txt, dic) {
  vapply(txt, function(t) {
    if (is.na(t)) return(NA_character_)
    h <- names(dic)[vapply(dic, function(p) grepl(p, t, perl = TRUE), logical(1))]
    if (length(h)) paste(h, collapse = "+") else ""
  }, character(1), USE.NAMES = FALSE)
}

# ---- Presentación: contenido y unidad ----------------------------------------------------
# Unidades contables en nombres de Cafam/Locatel ("Caja con 10 Comprimidos", "X 30Und")
RE_CONTEO <- "(\\d+)\\s*(?:TABLETAS?|TABS?|COMPRIMIDOS?|COMP|CAPSULAS?|CAPS?|CAPLETAS?|GRAGEAS?|SOBRES?|SACHETS?|PARCHES?|AMPOLLAS?|UNIDADES?|UNDS?|SOFTGELS?|GELCAPS?|PASTILLAS?|SUPOSITORIOS?)\\b"
# ml / g como contenido (se excluye "5 ml" de "100 mg en 5 ml" y "mg/ml")
RE_VOLPES <- "(?<![/\\d.,])(?<!\\bEN )(\\d+(?:[.,]\\d+)?)\\s*(ML|CC|GRS|GR|G|GRAMOS)\\b"

contenido_desde_nombre <- function(nn) {
  n <- length(nn); valor <- rep(NA_real_, n); unidad <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    if (is.na(nn[i])) next
    m <- regmatches(nn[i], regexec(RE_CONTEO, nn[i], perl = TRUE))[[1]]
    if (length(m) >= 2) { valor[i] <- as.numeric(m[2]); unidad[i] <- "un"; next }
    m <- regmatches(nn[i], regexec(RE_VOLPES, nn[i], perl = TRUE))[[1]]
    if (length(m) >= 3) {
      valor[i] <- a_num(m[2]); unidad[i] <- if (m[3] %in% c("ML", "CC")) "ml" else "g"
    }
  }
  data.frame(contenido_valor = valor, contenido_unidad = unidad, stringsAsFactors = FALSE)
}

# La Rebaja trae la presentación estructurada: "Presentacionunidadmedida: BLISTER X 10",
# "CAJA X 100 TAB", "FRASCO X 120 ML", "TUBO X 30 GR" y "Unidadmedida: TABLETA/GRAMO/MILILITRO/..."
contenido_larebaja <- function(tn) {
  pu  <- get_spec(tn, "PRESENTACIONUNIDADMEDIDA")
  um  <- get_spec(tn, "UNIDADMEDIDA")
  ct  <- a_num(get_spec(tn, "CANTIDADUNIDADESMEDIDA"))
  n <- length(tn); valor <- rep(NA_real_, n); unidad <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    v <- NA_real_
    if (!is.na(pu[i])) {
      m <- regmatches(pu[i], regexec("\\bX\\s*(\\d+(?:[.,]\\d+)?)", pu[i], perl = TRUE))[[1]]
      if (length(m) >= 2) v <- a_num(m[2])
    }
    if (is.na(v)) v <- ct[i]
    if (is.na(v) || is.na(um[i])) next
    valor[i] <- v
    unidad[i] <- if (um[i] %in% c("GRAMO", "G", "GR", "GRS")) "g" else if (um[i] %in% c("MILILITRO", "ML")) "ml" else "un"
  }
  data.frame(contenido_valor = valor, contenido_unidad = unidad, stringsAsFactors = FALSE)
}

# Concentración: "500 mg", "1 %", "160 mg/5 ml"; se estandariza sin modificar el texto original
extraer_concentracion <- function(txt) {
  vapply(txt, function(t) {
    if (is.na(t)) return(NA_character_)
    m <- gregexpr("(\\d+(?:[.,]\\d+)?)\\s*(MG|MCG|UI|%)(?:\\s*/\\s*(\\d+(?:[.,]\\d+)?)\\s*(ML|G))?", t, perl = TRUE)
    h <- regmatches(t, m)[[1]]
    if (!length(h)) return(NA_character_)
    h <- gsub("\\s+", "", h); h <- gsub(",", ".", h)
    h <- sub("^(\\d+(?:\\.\\d+)?)(MG|MCG|UI|%)", "\\1 \\2", h, perl = TRUE)
    paste(unique(tolower(h)), collapse = " / ")
  }, character(1), USE.NAMES = FALSE)
}

# ---- Carga y enriquecimiento -------------------------------------------------------------
cargar_datos <- function(con, ejecucion_id) {
  dbGetQuery(con, "
    SELECT o.obs_id, a.anuncio_id, a.comercio, a.sku_comercio, a.ean, a.nombre_original, a.marca_original,
           a.descripcion_original, a.categoria_origen, a.categoria_consulta, a.texto_original,
           o.ejecucion_id, o.fecha_hora, o.precio, o.precio_lista, o.en_promocion, o.disponible,
           o.url AS url_obs
    FROM observaciones o JOIN anuncios a USING (anuncio_id)
    WHERE o.ejecucion_id = ?", params = list(ejecucion_id))
}

enriquecer <- function(d) {
  d$nombre_norm <- norm_txt(d$nombre_original)
  tn <- norm_txt(d$texto_original)
  # EAN: solo dígitos, sin ceros a la izquierda, mínimo 8 dígitos
  e <- gsub("[^0-9]", "", ifelse(is.na(d$ean), "", d$ean)); e <- sub("^0+", "", e)
  d$ean_norm <- ifelse(nchar(e) >= 8, e, NA_character_)

  d$pa_campo <- get_spec(tn, "PRINCIPIO ACTIVO")
  desc_cafam <- ifelse(d$comercio == "cafam", norm_txt(d$descripcion_original), NA_character_)
  # Texto en el que se buscan principios activos: nombre + campo de principio activo (+ descripción
  # corta de Cafam, p. ej. "NAPROXENO 250mg TAB ORAL"). No se usa la ficha completa (ruido).
  d$texto_clave <- trimws(paste(d$nombre_norm,
                                ifelse(is.na(d$pa_campo), "", d$pa_campo),
                                ifelse(is.na(desc_cafam), "", desc_cafam), sep = " | "))

  # Contenido y unidad
  cn <- contenido_desde_nombre(d$nombre_norm)
  lr <- contenido_larebaja(tn)
  es_lr <- d$comercio == "larebaja"
  d$contenido_valor  <- ifelse(es_lr, lr$contenido_valor,  cn$contenido_valor)
  d$contenido_unidad <- ifelse(es_lr, lr$contenido_unidad, cn$contenido_unidad)
  # Un "contenido" de 1 unidad en cremas/geles/jarabes no es un contenido real (falta g/ml)
  topico_pal <- grepl("\\bGEL\\b|CREMA|UNGUENTO|POMADA|SPRAY|AEROSOL|ROLL ?ON|LOCION|LINIMENTO|EMULGEL|JARABE|SUSPENSION|GOTAS", d$nombre_norm)
  sin_cont <- topico_pal & d$contenido_unidad %in% "un" & d$contenido_valor %in% 1
  d$contenido_valor[sin_cont] <- NA_real_; d$contenido_unidad[sin_cont] <- NA_character_

  d$concentracion <- extraer_concentracion(paste(d$nombre_norm, ifelse(is.na(desc_cafam), "", desc_cafam)))

  # Marca estandarizada
  mk <- norm_txt(d$marca_original)
  mk[mk %in% c("AMERICAN GENERICS", "AMERICAN GENERIC")] <- "AG"
  d$marca_std <- ifelse(is.na(mk) | mk == "", NA_character_, mk)

  # Principios activos detectados
  d$act_a   <- detectar(d$texto_clave, ACT_A)
  d$act_ady <- detectar(d$texto_clave, ACT_ADY)
  d$act_grip <- detectar(d$texto_clave, ACT_GRIPAL)
  d$act_rev <- detectar(d$texto_clave, ACT_REVISAR)
  d$kw_gripal <- grepl(PAL_GRIPAL, d$nombre_norm, perl = TRUE)

  # Banderas
  d$es_paquete_promo <- as.integer(grepl("OFERTA|PAGUE|LLEVE|PRECIO ESPECIAL|\\b2 ?X ?1\\b|\\bKIT\\b|COMBO|GRATIS|DISPLAY",
                                         d$nombre_norm, perl = TRUE))
  d$pediatrico <- as.integer(grepl("NINO|PEDIATR|KIDS|CHILDREN|INFANTIL|JUNIOR|BEBE", d$nombre_norm))
  rx <- get_spec(tn, "PRODUCTO RX"); otc <- get_spec(tn, "PRODUCTO OTC")
  d$venta <- ifelse(!is.na(otc) & otc == "SI", "OTC",
             ifelse(!is.na(rx) & rx == "SI", "Con formula medica",
             ifelse(grepl("OTC", norm_txt(d$categoria_origen)), "OTC", "No informado")))
  d$forma <- ifelse(grepl("PARCHE|EMPLASTO", d$nombre_norm), "parche",
             ifelse(topico_pal & !grepl("JARABE|SUSPENSION|GOTAS", d$nombre_norm), "topico (gel/crema/spray)",
             ifelse(d$contenido_unidad %in% "ml", "liquido oral",
             ifelse(d$contenido_unidad %in% "un", "solido oral", NA_character_))))
  d
}

# ---- Pertenencia a la categoría ------------------------------------------------------------
# Activos de acción tópica "débil": solos no bastan (p. ej. mentol en pastillas para la garganta);
# cuentan si hay otro analgésico o si el anuncio habla de dolor/músculo.
ACT_DEBIL <- c("MENTOL", "ALCANFOR", "CAPSAICINA")
PAL_DOLOR <- "DOLOR|MUSCULAR|ARTICULAR|ANALGES|CONTRAIRRITANTE|ALIVIA|ALIVIO|SPORT|DEPORT|LINIMENTO|TRAUMA|CONTUSION"

# ¿El campo "Principio activo" del comercio trae información útil? ("NA", "N/A", "NO CODIFICADO"... no)
pa_util <- function(pa) !is.na(pa) & nzchar(pa) &
  !grepl("NO CODIFICADO|^(N/?A|NO APLICA|NINGUNO|NINGUNA|SIN INFORMACION|-+|0)$", pa)

clasificar_categoria <- function(d) {
  n <- nrow(d); cat <- rep(NA_integer_, n); motivo <- rep(NA_character_, n)
  confianza <- rep("declarado", n)
  pa_conocido <- pa_util(d$pa_campo)
  for (i in seq_len(n)) {
    activos <- if (nzchar(d$act_a[i])) strsplit(d$act_a[i], "+", fixed = TRUE)[[1]] else character()
    fuertes <- setdiff(activos, ACT_DEBIL)
    solo_debil <- length(activos) > 0 && length(fuertes) == 0
    pal_dolor <- grepl(PAL_DOLOR, d$texto_clave[i], perl = TRUE)
    tiene_a <- length(fuertes) > 0 || (solo_debil && pal_dolor)
    grip <- nzchar(d$act_grip[i]) || d$kw_gripal[i]
    if (tiene_a && grip) {
      cat[i] <- 0L; motivo[i] <- "combinacion para sintomas gripales (excluida por el tema)"
    } else if (!tiene_a && d$kw_gripal[i]) {
      cat[i] <- 0L; motivo[i] <- "producto para sintomas gripales (excluido por el tema)"
    } else if (tiene_a) {
      cat[i] <- 1L; motivo[i] <- paste("principio activo analgesico/antipiretico/antiinflamatorio:", d$act_a[i])
    } else if (solo_debil) {
      cat[i] <- NA_integer_
      motivo[i] <- paste("solo activos topicos debiles sin indicacion de dolor, revisar:", d$act_a[i])
    } else if (nzchar(d$act_rev[i])) {
      cat[i] <- NA_integer_; motivo[i] <- paste("principio activo ambiguo, revisar:", d$act_rev[i])
    } else if (pa_conocido[i] && grepl("ANALGES|ANTIINFLAM", d$nombre_norm[i])) {
      cat[i] <- NA_integer_
      motivo[i] <- "el nombre sugiere analgesico pero el principio activo declarado no es farmacologico, revisar"
    } else if (pa_conocido[i] || nzchar(d$act_ady[i]) || nzchar(d$act_grip[i])) {
      cat[i] <- 0L
      motivo[i] <- paste("principio activo fuera de la categoria:",
                         if (pa_conocido[i]) d$pa_campo[i] else paste(c(d$act_ady[i], d$act_grip[i])[nzchar(c(d$act_ady[i], d$act_grip[i]))], collapse = "+"))
    } else {
      cat[i] <- NA_integer_; motivo[i] <- "principio activo no identificable, revisar"
    }
  }
  # Herencia por EAN: el mismo código de barras es el mismo producto. Un anuncio sin clasificar
  # hereda la clasificación de otros anuncios con ese EAN solo si todos los clasificados coinciden.
  for (e in unique(d$ean_norm[!is.na(d$ean_norm) & is.na(cat)])) {
    ix <- which(d$ean_norm %in% e); cl <- unique(cat[ix][!is.na(cat[ix])])
    if (length(cl) == 1) {
      pend <- ix[is.na(cat[ix])]; fuente <- ix[!is.na(cat[ix])][1]
      cat[pend] <- cl
      d$act_a[pend] <- d$act_a[fuente]          # hereda también el principio activo detectado
      motivo[pend] <- sprintf("heredada por EAN (%s, clasificado %d en %s): %s", e, cl, d$comercio[fuente], motivo[fuente])
      confianza[pend] <- "heredado por EAN"
    }
  }
  d$en_categoria <- cat; d$motivo_categoria <- motivo; d$confianza_categoria <- confianza
  d$principio_activo_std <- ifelse(nzchar(d$act_a), paste(d$act_a, ifelse(nzchar(d$act_ady), paste0("+", d$act_ady), ""), sep = ""),
                            ifelse(pa_util(d$pa_campo), d$pa_campo, NA_character_))
  d$principio_activo_std <- gsub("\\+\\+", "+", d$principio_activo_std)
  # Subcategoría estandarizada (por uso y vía; sin tomar la sección del sitio)
  solo_apap <- grepl("^ACETAMINOFEN(\\+CAFEINA)?$", d$principio_activo_std)
  d$subcategoria_std <- ifelse(is.na(d$en_categoria) | d$en_categoria == 0, NA_character_,
    ifelse(d$forma %in% c("topico (gel/crema/spray)", "parche") | grepl("SALICILATO|MENTOL|ALCANFOR|CAPSAICINA", d$act_a), "Analgesico/antiinflamatorio topico",
    ifelse(grepl("\\+", d$act_a) & !grepl("^ACETAMINOFEN$", d$act_a), "Combinacion analgesica oral",
    ifelse(solo_apap, "Acetaminofen oral",
    ifelse(d$act_a == "ACIDO_ACETILSALICILICO", "Acido acetilsalicilico oral",
    ifelse(d$act_a %in% c("TRAMADOL", "CODEINA", "HIDROCODONA"), "Opioide oral (con formula)", "AINE oral u otro analgesico oral"))))))
  d
}

# ---- Correspondencia entre comercios ----------------------------------------------------------
asignar_producto <- function(d) {
  d$producto_id <- NA_character_; d$regla <- NA_character_
  cont_txt <- ifelse(is.na(d$contenido_valor), NA_character_, paste0(num_txt(d$contenido_valor), d$contenido_unidad))
  ok1 <- !is.na(d$ean_norm) & !is.na(cont_txt)
  d$producto_id[ok1] <- paste0("EAN", d$ean_norm[ok1], "-", cont_txt[ok1])
  d$regla[ok1] <- "R1: mismo EAN y mismo contenido"
  # Clave de respaldo (R2): marca + principios activos + concentración + contenido
  kok <- !is.na(d$marca_std) & nzchar(d$act_a) & !is.na(d$concentracion) & !is.na(cont_txt)
  d$k_key <- ifelse(kok, paste(d$marca_std, d$act_a, d$concentracion, cont_txt, sep = "|"), NA_character_)
  # k_key -> producto EAN, solo si la clave apunta a un único producto identificado por EAN
  base <- unique(d[ok1 & kok, c("k_key", "producto_id")])
  unica <- names(which(table(base$k_key) == 1)); base <- base[base$k_key %in% unica, ]
  mapa <- setNames(base$producto_id, base$k_key)
  sin <- is.na(d$producto_id) & kok
  hit <- sin & d$k_key %in% names(mapa)
  d$producto_id[hit] <- unname(mapa[d$k_key[hit]])
  d$regla[hit] <- "R2: sin EAN; coincide marca+activos+concentracion+contenido con un producto con EAN"
  d$k_key <- NULL
  d
}

# Duplicados dentro de un comercio y comparabilidad (en UNA misma ejecución)
marcar_comparables <- function(d, comercios = sort(unique(d$comercio))) {
  d$elegible <- d$en_categoria %in% 1L & d$es_paquete_promo == 0L & !is.na(d$precio) & !is.na(d$producto_id)
  clave <- paste(d$comercio, d$producto_id)
  d$n_dup <- ave(seq_len(nrow(d)), clave, FUN = length)
  d$n_dup[is.na(d$producto_id)] <- 1L
  # entre duplicados: si todos tienen el mismo precio se colapsan; si no, hay conflicto
  d$dup_conflicto <- 0L
  for (k in unique(clave[d$n_dup > 1 & !is.na(d$producto_id)])) {
    ix <- which(clave == k)
    if (length(unique(d$precio[ix])) > 1) d$dup_conflicto[ix] <- 1L
  }
  d$usable <- d$elegible & d$dup_conflicto == 0L
  cobertura <- tapply(d$comercio[d$usable], d$producto_id[d$usable], function(x) length(unique(x)))
  ids_ok <- names(cobertura)[cobertura == length(comercios)]
  d$comparable <- as.integer(d$usable & d$producto_id %in% ids_ok)
  d$precio_unidad <- ifelse(!is.na(d$precio) & !is.na(d$contenido_valor) & d$contenido_valor > 0,
                            d$precio / d$contenido_valor, NA_real_)
  d
}

# ---- Función general -------------------------------------------------------------------------
ejecutar_limpieza <- function(con, ejecucion_id, comercios = NULL) {
  d <- cargar_datos(con, ejecucion_id)
  if (!nrow(d)) stop("No hay observaciones para la ejecucion ", ejecucion_id)
  d <- enriquecer(d)
  d <- clasificar_categoria(d)
  d <- asignar_producto(d)
  d <- marcar_comparables(d, comercios %||% sort(unique(d$comercio)))
  d
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Consenso a nivel de producto y familias de presentaciones ---------------------------------
# Un mismo producto (mismo EAN y contenido) puede traer datos distintos según el comercio. A nivel de
# producto se usa: la marca más frecuente y la UNION de principios activos que declare algún comercio.
moda <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  t <- table(x); names(t)[order(-as.integer(t), names(t))][1]
}
pa_consenso <- function(x) {
  a <- sort(unique(unlist(strsplit(x[!is.na(x) & nzchar(x)], "+", fixed = TRUE))))
  if (length(a)) paste(a, collapse = "+") else NA_character_
}

# Familia = mismo producto en distintos tamaños. Regla automática: misma marca + mismos principios
# activos + misma concentración (valor del activo) + misma unidad de contenido (ml, g, un) + misma
# "línea". La línea no se puede leer de forma fiable del texto, así que se declara aquí a mano, por EAN,
# SOLO cuando dos líneas comparten todo lo demás (validado revisando los nombres originales):
LINEA_MANUAL <- c("7703363006094" = "ACTIVGEL", "7703363006100" = "ACTIVGEL",   # Dolex Activgel: capsulas liquidas
                  "7707172688939" = "AVANZADO", "7707397791490" = "AVANZADO")   # Dolex Avanzado: tabletas

construir_familias <- function(con, ejecucion_id) {
  x <- dbGetQuery(con, "SELECT DISTINCT producto_id FROM v_observaciones
                        WHERE comparable = 1 AND ejecucion_id = ?", params = list(ejecucion_id))
  if (!nrow(x)) return(data.frame(producto_id = character(), familia = character()))
  p <- dbGetQuery(con, sprintf("SELECT producto_id, marca_std, principio_activo_std, concentracion,
                                contenido_valor, contenido_unidad FROM productos WHERE producto_id IN (%s)",
                               paste0("'", x$producto_id, "'", collapse = ",")))
  ean <- sub("^EAN([0-9]+)-.*$", "\\1", p$producto_id)
  # concentración = primer valor numérico + su unidad ("250 mg / 65 mg" -> "250mg"; "1 %" -> "1%")
  conc <- tolower(ifelse(is.na(p$concentracion), "", p$concentracion))
  conc <- ifelse(grepl("^[ ]*[0-9.]+ *(mg|mcg|g|%)", conc),
                 gsub(" ", "", sub("^[ ]*([0-9.]+ *(mg|mcg|g|%)).*$", "\\1", conc)), "")
  linea <- ifelse(ean %in% names(LINEA_MANUAL), unname(LINEA_MANUAL[ean]), "")
  p$familia <- trimws(paste(p$marca_std, p$principio_activo_std, conc, linea, paste0("[", p$contenido_unidad, "]")))
  p$n_tamanos <- ave(p$contenido_valor, p$familia, FUN = function(v) length(unique(v)))
  p[order(p$familia, p$contenido_valor), c("producto_id", "familia", "n_tamanos")]
}

# Guarda la tabla familias (producto comparable -> familia de presentaciones del mismo producto)
guardar_familias <- function(con, fam) {
  dbExecute(con, "CREATE TABLE IF NOT EXISTS familias (
    producto_id TEXT PRIMARY KEY REFERENCES productos(producto_id), familia TEXT NOT NULL, n_tamanos INTEGER)")
  if (nrow(fam))
    dbExecute(con, "INSERT OR REPLACE INTO familias VALUES (?,?,?)", params = unname(as.list(fam)))
  invisible(fam)
}

# ---- Persistencia en SQLite ------------------------------------------------------------------
# Se puede llamar una vez por ejecución (otra fecha = otra llamada): clasificacion, productos y
# correspondencia se actualizan (upsert) sin borrar lo ya calculado; obs_comparable guarda, por
# observación, si esa observación fue comparable DENTRO de su ejecución.
guardar_limpieza <- function(con, d) {
  ejecucion_id <- unique(d$ejecucion_id); stopifnot(length(ejecucion_id) == 1)
  dbWithTransaction(con, {
    dbExecute(con, "DROP VIEW IF EXISTS v_observaciones")
    dbExecute(con, "CREATE TABLE IF NOT EXISTS clasificacion (
      anuncio_id INTEGER PRIMARY KEY REFERENCES anuncios(anuncio_id),
      en_categoria INTEGER, motivo_categoria TEXT, confianza_categoria TEXT, principio_activo_std TEXT, forma TEXT,
      venta TEXT, es_paquete_promo INTEGER, pediatrico INTEGER, ean_norm TEXT,
      marca_std TEXT, subcategoria_std TEXT, concentracion TEXT,
      contenido_valor REAL, contenido_unidad TEXT, n_dup_en_comercio INTEGER, dup_conflicto INTEGER)")
    dbExecute(con, "CREATE TABLE IF NOT EXISTS obs_comparable (
      obs_id INTEGER PRIMARY KEY REFERENCES observaciones(obs_id),
      ejecucion_id TEXT NOT NULL, comparable INTEGER NOT NULL)")

    cl <- d[, c("anuncio_id", "en_categoria", "motivo_categoria", "confianza_categoria", "principio_activo_std",
                "forma", "venta", "es_paquete_promo", "pediatrico", "ean_norm", "marca_std", "subcategoria_std",
                "concentracion", "contenido_valor", "contenido_unidad", "n_dup", "dup_conflicto")]
    dbExecute(con, "INSERT OR REPLACE INTO clasificacion VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
              params = unname(as.list(cl)))

    u <- d[!is.na(d$producto_id), ]
    pr <- do.call(rbind, lapply(split(u, u$producto_id), function(g) {
      data.frame(producto_id = g$producto_id[1],
                 descripcion = g$nombre_original[order(nchar(g$nombre_original), decreasing = TRUE)[1]],
                 marca_std = moda(g$marca_std),
                 principio_activo_std = pa_consenso(g$principio_activo_std),
                 subcategoria_std = g$subcategoria_std[!is.na(g$subcategoria_std)][1] %||% NA_character_,
                 contenido_valor = g$contenido_valor[1], contenido_unidad = g$contenido_unidad[1],
                 unidades_empaque = if (g$contenido_unidad[1] %in% "un") as.integer(g$contenido_valor[1]) else 1L,
                 concentracion = g$concentracion[!is.na(g$concentracion)][1] %||% NA_character_,
                 comparable = max(g$comparable), stringsAsFactors = FALSE)
    }))
    if (!"principio_activo_std" %in% dbListFields(con, "productos"))
      dbExecute(con, "ALTER TABLE productos ADD COLUMN principio_activo_std TEXT")
    dbExecute(con, "INSERT INTO productos (producto_id, descripcion, marca_std, principio_activo_std, subcategoria_std,
                    contenido_valor, contenido_unidad, unidades_empaque, concentracion, comparable)
                    VALUES (?,?,?,?,?,?,?,?,?,?)
                    ON CONFLICT(producto_id) DO UPDATE SET descripcion = excluded.descripcion,
                    marca_std = excluded.marca_std, principio_activo_std = excluded.principio_activo_std,
                    subcategoria_std = excluded.subcategoria_std,
                    contenido_valor = excluded.contenido_valor, contenido_unidad = excluded.contenido_unidad,
                    unidades_empaque = excluded.unidades_empaque, concentracion = excluded.concentracion,
                    comparable = excluded.comparable",
              params = unname(as.list(pr[, c("producto_id", "descripcion", "marca_std", "principio_activo_std",
                                             "subcategoria_std", "contenido_valor", "contenido_unidad", "unidades_empaque",
                                             "concentracion", "comparable")])))
    co <- d[!is.na(d$producto_id), c("anuncio_id", "producto_id", "comparable", "regla")]
    dbExecute(con, "INSERT INTO correspondencia (anuncio_id, producto_id, comparable, regla) VALUES (?,?,?,?)
                    ON CONFLICT(anuncio_id) DO UPDATE SET producto_id = excluded.producto_id,
                    comparable = excluded.comparable, regla = excluded.regla",
              params = unname(as.list(co)))
    # anuncios de esta ejecución que ya no tienen producto (cambió la regla): se quita la fila vieja
    viejos <- setdiff(dbGetQuery(con, "SELECT DISTINCT anuncio_id FROM correspondencia")$anuncio_id,
                      co$anuncio_id)
    viejos <- intersect(viejos, d$anuncio_id)
    if (length(viejos)) dbExecute(con, "DELETE FROM correspondencia WHERE anuncio_id = ?", params = list(viejos))

    # comparabilidad por observación, dentro de su ejecución
    oc <- d[, c("obs_id", "ejecucion_id", "comparable")]
    dbExecute(con, "INSERT OR REPLACE INTO obs_comparable VALUES (?,?,?)", params = unname(as.list(oc)))

    # Vista de trabajo para las consultas SQL: una fila por observación con todo lo estandarizado
    dbExecute(con, "CREATE VIEW v_observaciones AS
      SELECT o.obs_id, o.ejecucion_id, o.fecha_hora, c.nombre AS comercio_nombre, a.comercio, a.anuncio_id,
             a.sku_comercio, a.nombre_original, a.marca_original, a.descripcion_original, a.texto_original,
             a.url AS url_anuncio, o.url AS url_extraccion,
             o.precio, o.precio_lista, o.en_promocion, o.disponible, o.moneda,
             cl.en_categoria, cl.motivo_categoria, cl.confianza_categoria, cl.principio_activo_std, cl.subcategoria_std,
             cl.marca_std, cl.concentracion, cl.contenido_valor, cl.contenido_unidad, cl.es_paquete_promo,
             cl.n_dup_en_comercio, cl.dup_conflicto,
             co.producto_id, co.regla, COALESCE(oc.comparable, 0) AS comparable,
             CASE WHEN o.precio IS NOT NULL AND cl.contenido_valor > 0 THEN o.precio / cl.contenido_valor END AS precio_por_unidad
      FROM observaciones o
      JOIN anuncios a ON a.anuncio_id = o.anuncio_id
      JOIN comercios c ON c.comercio = a.comercio
      LEFT JOIN clasificacion cl ON cl.anuncio_id = a.anuncio_id
      LEFT JOIN correspondencia co ON co.anuncio_id = a.anuncio_id
      LEFT JOIN obs_comparable oc ON oc.obs_id = o.obs_id")
  })
  invisible(TRUE)
}
