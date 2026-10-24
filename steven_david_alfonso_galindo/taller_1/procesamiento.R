# =============================================================================
# procesamiento.R · Limpieza, exclusiones y correspondencia de productos
# Taller 1 · Minería de Datos (2016325)
# =============================================================================
#
# Trabaja sobre la base que llena scraper.R. No toca los textos originales:
# escribe la versión estandarizada en columnas propias de la tabla anuncios
# (marca_std, concentracion_std, contenido_ml, ...), marca las exclusiones con
# su motivo y asigna el identificador común (producto_id) a las presentaciones
# del catálogo datos/catalogo_comparables.csv.
#
# Uso:
#   source("scraper.R"); source("procesamiento.R")
#   con <- conectar_bd()
#   procesar_base(con)
#
# Se puede ejecutar cuantas veces se quiera: recalcula todo desde los textos
# originales y deja el mismo resultado.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(DBI)
})

RUTA_CATALOGO <- file.path("datos", "catalogo_comparables.csv")

# -----------------------------------------------------------------------------
# 1. Normalización de texto
# -----------------------------------------------------------------------------
# Mayúsculas, sin tildes, "&" como " Y " y cualquier signo como espacio:
#   "Perfume Ahmed O Clock Ext. Parfum" -> "PERFUME AHMED O CLOCK EXT PARFUM"
#   "ÉCLAIR AFFAIR EDP, E284-010,100ML" -> "ECLAIR AFFAIR EDP E284 010 100ML"
normalizar <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_replace_all("&", " Y ") |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

# -----------------------------------------------------------------------------
# 2. Marca
# -----------------------------------------------------------------------------
# Variantes del mismo nombre de marca (ya normalizadas) -> forma canónica.
ALIAS_MARCAS <- c(
  "GIANNI VERSACE"               = "VERSACE",
  "DOLCE Y GABBANA"              = "DOLCE & GABBANA",
  "D Y G DOLCE"                  = "DOLCE & GABBANA",
  "D Y G"                        = "DOLCE & GABBANA",
  "AHMED"                        = "AHMED AL MAGHRIBI",
  "REYANE"                       = "REYANE TRADITION",
  "PRIVATE COLLECTION"           = "LOMANI",
  "PRIVATE COLLECTION BY LOMANI" = "LOMANI",
  "BANDERAS"                     = "ANTONIO BANDERAS",
  "RASASI ALIZA"                 = "RASASI",
  "BILLIE ELISH"                 = "BILLIE EILISH",
  "D SQUARED"                    = "DSQUARED2",
  "DSQUARED"                     = "DSQUARED2",
  "GANSO"                        = "EL GANSO",
  "100 100"                      = "100%100",
  "AJMA"                         = "AJMAL",
  "LOEW"                         = "LOEWE",
  "AL HARAM"                     = "AL HARAMAIN",
  "AL HARAMAI"                   = "AL HARAMAIN",
  "SALVATORE FERRAGAMO"          = "FERRAGAMO",
  "POLO RALPH LAUREN"            = "RALPH LAUREN",
  "ROGER Y GALLET"               = "ROGER & GALLET",
  "ABERCROMBIE Y FITCH"          = "ABERCROMBIE & FITCH",
  "VIKTOR Y ROLF"                = "VIKTOR & ROLF",
  "LAVANTE PARIS"                = "LAVANTE",
  "AGATHA RUIZ"                  = "AGATHA RUIZ DE LA PRADA",
  "PACO RABANNE"                 = "RABANNE"
)

canonica <- function(marca_norm) {
  out <- unname(ALIAS_MARCAS[marca_norm])
  coalesce(out, marca_norm)
}

TOKENS_PRESENTACION <- "\\b(EDP|EDT|EDC|EDPI|EAU|PARFUM|PARF|EXTRAIT|SP|SPR|SPRAY|VAPO|NATURAL|NAT|INTENSE|SET)\\b"

# La Riviera no trae la marca en un campo aparte. El nombre tiene la forma
# "MARCA, LÍNEA, CÓDIGO, TAMAÑO", pero a veces la marca va en segundo lugar
# ("TOUS L'EAU EDP SP, TOUS, ..."). Se elige entre las dos primeras partes:
#   + mucho si coincide con una marca que Falabella o Farmatodo publican,
#   + la cantidad de veces que esa parte se repite en el catálogo de La Riviera
#     (una marca se repite; el nombre de una línea casi nunca),
#   - si la parte contiene palabras de presentación (EDP, SPRAY...).
marca_la_riviera <- function(nombres, diccionario) {
  partes <- str_split(nombres, ",") |>
    map(\(p) {
      p <- normalizar(p)
      p <- p[p != "" & !str_detect(p, "^[A-Z]{1,4}[0-9]{0,3} [0-9]{3}$") & !str_detect(p, "^[0-9]+ ?(ML|M)\\b")]
      head(p, 2)
    })
  frecuencia <- table(unlist(partes))
  map_chr(partes, \(p) {
    if (length(p) == 0) return(NA_character_)
    puntaje <- map_dbl(seq_along(p), \(i) {
      x <- p[i]
      otra <- p[-i]
      # Si una parte es el comienzo de la otra ("JEAN PASCAL" frente a
      # "JEAN PASCAL ESTUCHE CUERO"), la más corta es la marca.
      prefijo <- length(otra) > 0 && str_starts(otra[1], paste0(x, " "))
      1000 * (canonica(x) %in% diccionario) +
        300 * prefijo +
        as.numeric(frecuencia[[x]]) -
        500 * str_detect(x, TOKENS_PRESENTACION)
    })
    canonica(p[which.max(puntaje)])
  })
}

# -----------------------------------------------------------------------------
# 3. Concentración, contenido, unidades y género
# -----------------------------------------------------------------------------

# Sobre el texto normalizado. El orden importa: "EAU DE PARFUM" contiene
# "PARFUM", así que primero se busca Extrait, luego EDP, EDT, EDC y al final
# Parfum. "ADP" (La Riviera) no se interpreta: queda sin concentración.
concentracion_de <- function(n) {
  case_when(
    str_detect(n, "\\bEXTRAIT\\b|\\bEXTRACTO\\b|\\bEXT PARF") ~ "Extrait",
    str_detect(n, "\\bEAU DE PARFUM\\b|\\bEAU DE PERFUM\\b|\\bEDPI?\\b|\\bEDP[0-9]") ~ "EDP",
    str_detect(n, "\\bEAU DE TOILETTE\\b|\\bTOILETTE\\b|\\bEDT\\b") ~ "EDT",
    str_detect(n, "\\bEAU DE COLOGNE\\b|\\bEDC\\b|\\bCOLONIA\\b|\\bCOLOGNE\\b") ~ "EDC",
    str_detect(n, "\\bPARFUM\\b|\\bPARF\\b") ~ "Parfum",
    .default = NA_character_
  )
}

TAMANOS_ESTANDAR <- c(5, 7.5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 88,
                      90, 95, 100, 105, 110, 115, 120, 125, 150, 180, 200, 250, 300)

# Contenido en mL a partir del nombre original (no normalizado, para no perder
# el punto decimal de "3.4 oz"). Las onzas se convierten con 1 fl oz = 29.5735 mL
# y se ajustan al tamaño comercial más cercano si la diferencia es menor al 2 %
# (3.4 oz = 100.5 mL -> 100 mL). Devuelve también cuántas cantidades distintas
# trae el nombre (más de una suele indicar un estuche) y las unidades.
contenido_de <- function(nombre) {
  patron <- regex("(\\d+(?:[.,]\\d{1,2})?)\\s*(ML|MLS|M|FL\\.?\\s*OZ|OZ)\\b", ignore_case = TRUE)
  todas <- str_match_all(nombre, patron)
  map(seq_along(nombre), \(i) {
    m <- todas[[i]]
    nm <- normalizar(nombre[i])
    # Unidades: solo cuando el nombre las dice de forma explícita, como
    # "4 x 25 ml" o "Unidad x 2" (Farmatodo). "Botella x 1" o "Botella x 100"
    # no son unidades: Farmatodo escribe así el envase o el contenido.
    multi <- str_match(nm, "\\b([0-9]{1,2}) X ([0-9]+) ?(ML|M)\\b")
    explicitas <- str_match(nm, "\\b(UNIDAD|UNIDADES|UND|UNDS|UDS) X ([0-9]{1,2})\\b")
    unidades <- if (!is.na(multi[1, 2])) as.integer(multi[1, 2]) else
      if (!is.na(explicitas[1, 3])) as.integer(explicitas[1, 3]) else 1L
    if (is.na(unidades) || unidades < 1 || unidades > 12) unidades <- 1L
    if (nrow(m) == 0) return(tibble(contenido_ml = NA_real_, unidades = unidades, n_cantidades = 0L))
    valores <- as.numeric(str_replace(m[, 2], ",", "."))
    es_oz <- str_detect(str_to_lower(m[, 3]), "oz")
    ml <- ifelse(es_oz, valores * 29.5735, valores)
    cercano <- map_dbl(ml, \(v) TAMANOS_ESTANDAR[which.min(abs(TAMANOS_ESTANDAR - v))])
    ml <- ifelse(es_oz & abs(cercano - ml) / ml < 0.02, cercano, round(ml, 1))
    contenido <- if (!is.na(multi[1, 2])) unidades * as.numeric(multi[1, 3]) else ml[1]
    tibble(contenido_ml = contenido, unidades = unidades, n_cantidades = length(unique(ml)))
  }) |> list_rbind()
}

# Género según palabras del nombre; si el nombre no dice nada se usa la
# subcategoría del sitio (Farmatodo publica Hombre / Mujer).
genero_de <- function(n, subcategoria_original) {
  h <- str_detect(n, "\\b(HOMBRE|HOMBRES|MEN|MAN|HOMME|HIM|POUR LUI|MASCULINO|CABALLERO|UOMO|BOY)\\b")
  m <- str_detect(n, "\\b(MUJER|MUJERES|WOMEN|WOMAN|FEMME|HER|POUR ELLE|FEMENINO|DAMA|DAMAS|DONNA|GIRL|GIRLS|LADY)\\b")
  u <- str_detect(n, "\\bUNISEX\\b|\\bF M\\b|\\bF W\\b")
  sub <- normalizar(coalesce(subcategoria_original, ""))
  case_when(
    u | (h & m) ~ "Unisex",
    h ~ "Hombre",
    m ~ "Mujer",
    sub == "HOMBRE" ~ "Hombre",
    sub == "MUJER" ~ "Mujer",
    .default = NA_character_
  )
}

# Línea aproximada: el nombre sin marca, tamaños, códigos ni palabras de
# presentación. Solo es descriptiva; la correspondencia usa el catálogo.
linea_de <- function(n, marca) {
  quitar <- paste0(
    "\\b(PERFUME|PERFME|PARA|DE|HOMBRE|MUJER|UNISEX|EDP|EDT|EDC|EAU|PARFUM|TOILETTE|",
    "EXTRAIT|EXT|SP|SPR|SPRAY|VAPO|NATURAL|NAT|BOTELLA|BOTELL|FRASCO|CAJA|UNIDAD|X|ML|M|",
    "MEN|WOMEN|HOMME|FEMME|[A-Z]{1,4}[0-9]{0,3} [0-9]{3}|[0-9]+ ?ML|[0-9]+)\\b"
  )
  marca_n <- normalizar(coalesce(marca, ""))
  map2_chr(n, marca_n, \(x, mk) {
    if (mk != "") x <- str_remove_all(x, paste0("\\b", mk, "\\b"))
    str_squish(str_remove_all(x, quitar))
  })
}

# -----------------------------------------------------------------------------
# 4. Exclusiones (delimitación de la categoría)
# -----------------------------------------------------------------------------
# Categoría: solo perfumes (Parfum/Extrait, EDP, EDT, EDC) de hombre, mujer y
# unisex. Se excluyen, en este orden (el primer motivo que aplique):
REGLAS_EXCLUSION <- tribble(
  ~motivo,                         ~patron, ~palabras,
  "Registro de prueba del sitio",  "\\b(PRODUCTO PRUEBA|PRUEBA|PRUEBA AUTO|TEST|TESTING)\\b",
                                   "producto prueba, prueba, test, testing",
  "Estuche, set, kit o combo",     "\\b(SET|ESTUCHE|KIT|COMBO|COFRE|PACK|GIFT)\\b",
                                   "set, estuche, kit, combo, cofre, pack, gift; también un \"+\" entre productos, dos contenidos distintos en el nombre (100 ml y 150 ml) o varias unidades",
  "Body mist, splash o bruma",     "\\b(BODY MIST|MIST|SPLASH|BRUMA|BRUMAS|BODY SPRAY|SPRAY CORPORAL)\\b",
                                   "body mist, mist, splash, bruma, body spray, spray corporal",
  "Desodorante",                   "\\b(DESODORANTE|DEO|DEODORANT|ANTITRANSPIRANTE|ROLL ON)\\b",
                                   "desodorante, deo, deodorant, antitranspirante, roll on",
  "Fragancia infantil",            "\\b(KID|KIDS|KIDSBEAR|NINOS|NINAS|INFANTIL|BABY|BEBES|JUNIOR|DISNEY|FROZEN|SPIDERMAN|MARVEL|CRY BABIES|KIDSHOP)\\b",
                                   "kid(s), niños, niñas, infantil, baby, bebés, junior, licencias infantiles (Disney, Frozen, Spiderman, Marvel, Cry Babies, Kidshop); también \"perfume o colonia (de/para) niño, niña o bebé\", salvo en las marcas Nina Ricci, Nino Cerruti y bebe",
  "Otro formato (no es Parfum, EDP, EDT ni EDC)",
                                   "\\b(CREMA|LOCION CORPORAL|LOCION HIDRATANTE|LOTION|SHOWER|GEL|JABON|SHAMPOO|ACONDICIONADOR|ACEITE|OIL|CAPILAR|HAIR|CABELLO|VELA|DIFUSOR|AMBIENTADOR|TALCO|ANTIBACTERIAL|SANITIZANTE|AFTER SHAVE|AFTERSHAVE)\\b",
                                   "crema, loción corporal o hidratante, lotion, shower gel, gel, jabón, shampoo, aceite o perfume oil, capilar, hair, cabello, vela, difusor, ambientador, talco, antibacterial, after shave"
)

# "Perfume niña", "Colonia para bebé"... son fragancias infantiles. "NINA",
# "NINO" y "BEBE" solos no bastan: Nina Ricci, Nino Cerruti y bebe son marcas
# de adultos ("Perfume Nina Ricci Nina EDT", "Bebe Love EDP").
FRASE_INFANTIL <- "\\b(PERFUME|COLONIA|AGUA|FRAGANCIA) (DE |PARA )?(NIN[OA]S?|BEBES?)\\b"
MARCAS_ADULTAS <- c("NINA RICCI", "NINO CERRUTI", "CERRUTI", "BEBE")

motivo_exclusion_de <- function(n, nombre_original, unidades, n_cantidades, marca) {
  motivo <- rep(NA_character_, length(n))
  for (i in seq_len(nrow(REGLAS_EXCLUSION))) {
    regla <- REGLAS_EXCLUSION$motivo[i]
    aplica <- str_detect(n, REGLAS_EXCLUSION$patron[i])
    if (regla == "Estuche, set, kit o combo") {
      # Un "+" entre productos ("Perfume 100 ml + Body Spray"), dos contenidos
      # distintos ("... 100 ml, incluye desodorante 150 ml") o varias unidades
      # también son un estuche o combo, aunque el nombre no diga "set".
      aplica <- aplica | str_detect(nombre_original, "\\+") | unidades > 1 | n_cantidades > 1
    }
    if (regla == "Fragancia infantil") {
      aplica <- aplica | (str_detect(n, FRASE_INFANTIL) & !(coalesce(marca, "") %in% MARCAS_ADULTAS))
    }
    motivo[is.na(motivo) & coalesce(aplica, FALSE)] <- regla
  }
  motivo
}

# Presentaciones que siguen siendo perfume pero no se comparan con una
# presentación normal (probadores, recargas, miniaturas).
ES_ESPECIAL <- "\\b(TESTER|PROBADOR|REFILL|RECARGA|MINI|MINIATURA|SAMPLE|MUESTRA|TRAVEL)\\b"

# -----------------------------------------------------------------------------
# 5. Estandarización de todos los anuncios
# -----------------------------------------------------------------------------

# Toma el texto más reciente de cada anuncio y calcula los campos estándar.
estandarizar_anuncios <- function(con) {
  base <- dbGetQuery(con, "
    SELECT a.anuncio_id, c.nombre AS comercio, o.nombre_original, o.marca_original,
           o.subcategoria_original
      FROM anuncios a
      JOIN comercios c ON c.comercio_id = a.comercio_id
      JOIN observaciones o ON o.observacion_id = (
           SELECT o2.observacion_id FROM observaciones o2
            WHERE o2.anuncio_id = a.anuncio_id
            ORDER BY o2.fecha_hora_consulta DESC LIMIT 1)") |>
    as_tibble()
  if (nrow(base) == 0) return(base)

  base <- base |> mutate(nombre_norm = normalizar(nombre_original))

  # Diccionario de marcas: las que publican Falabella y Farmatodo en su campo marca.
  diccionario <- base |>
    filter(!is.na(marca_original)) |>
    mutate(m = canonica(normalizar(marca_original))) |>
    distinct(m) |>
    pull(m)

  es_lr <- base$comercio == "La Riviera"
  marca <- rep(NA_character_, nrow(base))
  marca[!es_lr] <- canonica(normalizar(base$marca_original[!es_lr]))
  marca[es_lr]  <- marca_la_riviera(base$nombre_original[es_lr], diccionario)
  base$marca_std <- marca

  contenido <- contenido_de(base$nombre_original)
  base |>
    bind_cols(contenido) |>
    mutate(
      concentracion_std = concentracion_de(nombre_norm),
      genero_std = genero_de(nombre_norm, subcategoria_original),
      genero_nombre = genero_de(nombre_norm, NA_character_),   # solo lo que dice el nombre
      subcategoria_std = case_when(
        genero_std == "Hombre" ~ "Perfume de hombre",
        genero_std == "Mujer"  ~ "Perfume de mujer",
        genero_std == "Unisex" ~ "Perfume unisex",
        .default = NA_character_
      ),
      linea_std = linea_de(nombre_norm, marca_std),
      motivo_exclusion = motivo_exclusion_de(nombre_norm, nombre_original, unidades, n_cantidades, marca_std),
      excluido = as.integer(!is.na(motivo_exclusion)),
      especial = str_detect(nombre_norm, ES_ESPECIAL)
    )
}

# -----------------------------------------------------------------------------
# 6. Correspondencia con el catálogo de presentaciones comparables
# -----------------------------------------------------------------------------
# Regla estricta: un anuncio corresponde a una presentación del catálogo solo
# si cumple TODO lo siguiente:
#   1. no está excluido y no es probador, recarga ni miniatura;
#   2. su marca estandarizada es la del catálogo;
#   3. su nombre contiene la línea (patron_linea) y no contiene ninguna de las
#      variantes que la confunden: las propias de esa línea (patron_excluir:
#      Flame, Energy...) ni las generales de GUARDA_VARIANTES (Intense, Elixir,
#      Absolu, Extreme, Privé, inspirado, réplica...);
#   4. su concentración, leída del nombre, es la del catálogo (si el nombre no
#      la dice, no se empareja);
#   5. su contenido en mL y su número de unidades son los del catálogo;
#   6. el género que dice su nombre no contradice el de la línea (un anuncio
#      "Mujer" no se empareja con una línea de hombre; las líneas unisex
#      aceptan cualquier rótulo, porque los comercios las rotulan distinto).
# Si un anuncio cumpliera dos presentaciones a la vez, no se le asigna ninguna.

# Palabras que indican otra versión del perfume. Se aplican a todas las líneas,
# salvo que la palabra forme parte del nombre de la línea en el catálogo.
GUARDA_VARIANTES <- c("INTENSE", "INTENS", "INTENSO", "ELIXIR", "ABSOLU", "ABSOLUTE",
                      "EXTREME", "PRIVE", "INSPIRADO", "INSPIRADA", "INSPIRACION",
                      "REPLICA", "IMITACION")

patron_guarda <- function(patron_linea) {
  palabras <- GUARDA_VARIANTES[!str_detect(patron_linea, GUARDA_VARIANTES)]
  if (length(palabras) == 0) return("(?!)")      # patrón que nunca coincide
  paste0("\\b(", paste(palabras, collapse = "|"), ")\\b")
}

# ¿El género del nombre es compatible con el de la línea del catálogo?
genero_compatible <- function(genero_nombre, genero_linea) {
  genero_linea == "Unisex" | is.na(genero_nombre) | genero_nombre == "Unisex" |
    genero_nombre == genero_linea
}

leer_catalogo <- function(ruta = RUTA_CATALOGO) {
  utils::read.csv(ruta, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "") |>
    as_tibble()
}

texto_regla <- function(cat) {
  paste0(
    "marca = ", cat$marca, "; nombre contiene /", cat$patron_linea, "/",
    ifelse(is.na(cat$patron_excluir), "", paste0(" y no /", cat$patron_excluir, "/")),
    "; concentración = ", cat$concentracion, "; ", cat$contenido_ml, " ml; ",
    cat$unidades, " unidad(es)"
  )
}

asignar_productos <- function(anuncios, catalogo) {
  coincidencias <- map(seq_len(nrow(catalogo)), \(i) {
    cat <- catalogo[i, ]
    linea_ok <- str_detect(anuncios$nombre_norm, cat$patron_linea)
    excl_ok <- if (is.na(cat$patron_excluir)) TRUE else !str_detect(anuncios$nombre_norm, cat$patron_excluir)
    guarda_ok <- !str_detect(anuncios$nombre_norm, patron_guarda(cat$patron_linea))
    cumple <- anuncios$excluido == 0 & !anuncios$especial &
      coalesce(anuncios$marca_std == cat$marca, FALSE) &
      linea_ok & excl_ok & guarda_ok &
      coalesce(anuncios$concentracion_std == cat$concentracion, FALSE) &
      coalesce(abs(anuncios$contenido_ml - cat$contenido_ml) < 0.5, FALSE) &
      coalesce(anuncios$unidades == cat$unidades, FALSE) &
      genero_compatible(anuncios$genero_nombre, cat$genero)
    tibble(anuncio_id = anuncios$anuncio_id[cumple], producto_id = cat$producto_id,
           regla_correspondencia = texto_regla(cat))
  }) |> list_rbind()

  # Anuncios que cumplen más de una presentación: conflicto, no se asignan.
  conflictos <- coincidencias |> count(anuncio_id) |> filter(n > 1) |> pull(anuncio_id)
  if (length(conflictos) > 0) {
    message("Aviso: ", length(conflictos), " anuncios cumplen dos presentaciones y quedan sin asignar")
  }
  coincidencias |> filter(!anuncio_id %in% conflictos)
}

# Casi coincidencias: misma marca y línea, pero falla otro criterio. Sirve para
# documentar los casos dudosos (p. ej., 100 ml frente a 105 ml).
diagnostico_correspondencias <- function(con, catalogo = leer_catalogo()) {
  anuncios <- estandarizar_anuncios(con)
  map(seq_len(nrow(catalogo)), \(i) {
    cat <- catalogo[i, ]
    linea_ok <- str_detect(anuncios$nombre_norm, cat$patron_linea)
    excl_ok <- if (is.na(cat$patron_excluir)) TRUE else !str_detect(anuncios$nombre_norm, cat$patron_excluir)
    marca_ok <- coalesce(anuncios$marca_std == cat$marca, FALSE)
    candidatos <- anuncios[marca_ok & linea_ok & excl_ok, ]
    if (nrow(candidatos) == 0) return(NULL)
    candidatos |>
      mutate(
        producto_id = cat$producto_id,
        variante = str_extract(nombre_norm, patron_guarda(cat$patron_linea)),
        motivo = case_when(
          excluido == 1 ~ paste("Excluido:", motivo_exclusion),
          especial ~ "Probador, recarga o miniatura",
          !is.na(variante) ~ paste0("Otra versión de la línea (", str_to_title(variante), ")"),
          !genero_compatible(genero_nombre, cat$genero) ~ paste0("Género distinto (", genero_nombre, ")"),
          is.na(concentracion_std) ~ "El nombre no indica la concentración",
          concentracion_std != cat$concentracion ~ paste0("Concentración distinta (", concentracion_std, ")"),
          is.na(contenido_ml) ~ "El nombre no indica el contenido",
          abs(contenido_ml - cat$contenido_ml) >= 0.5 ~ paste0("Contenido distinto (", contenido_ml, " ml)"),
          unidades != cat$unidades ~ paste0("Unidades distintas (", unidades, ")"),
          .default = "Coincide"
        )
      ) |>
      select(producto_id, comercio, anuncio_id, nombre_original, motivo)
  }) |> list_rbind()
}

# -----------------------------------------------------------------------------
# 7. Procesar la base completa
# -----------------------------------------------------------------------------

# Comercios elegidos para la comparación: los definidos en scraper.R. Si en el
# futuro se reemplaza un comercio, la tabla comercios conserva el anterior,
# pero la cobertura se exige solo sobre los elegidos.
comercios_configurados <- function(con) {
  if (exists("COMERCIOS")) COMERCIOS$comercio_id else dbGetQuery(con, "SELECT comercio_id FROM comercios")$comercio_id
}

procesar_base <- function(con, catalogo = leer_catalogo()) {
  anuncios <- estandarizar_anuncios(con)
  if (nrow(anuncios) == 0) {
    message("La base no tiene anuncios todavía.")
    return(invisible(NULL))
  }
  asignados <- asignar_productos(anuncios, catalogo)
  anuncios <- anuncios |>
    left_join(asignados, by = "anuncio_id")

  dbWithTransaction(con, {
    # Catálogo de presentaciones (identificador común).
    productos <- catalogo |>
      transmute(producto_id, familia, marca, linea, concentracion, contenido = contenido_ml,
                unidad = "ml", unidades, genero,
                subcategoria = paste("Perfume", ifelse(genero == "Unisex", "unisex", paste("de", tolower(genero)))),
                descripcion)
    dbExecute(con, "UPDATE anuncios SET producto_id = NULL, regla_correspondencia = NULL")
    dbWriteTable(con, "tmp_productos", productos, temporary = TRUE, overwrite = TRUE)
    dbExecute(con, "
      INSERT INTO productos (producto_id, familia, marca, linea, concentracion, contenido, unidad,
                             unidades, genero, subcategoria, descripcion)
      SELECT producto_id, familia, marca, linea, concentracion, contenido, unidad,
             unidades, genero, subcategoria, descripcion FROM tmp_productos WHERE true
      ON CONFLICT (producto_id) DO UPDATE SET
        familia = excluded.familia, marca = excluded.marca, linea = excluded.linea,
        concentracion = excluded.concentracion, contenido = excluded.contenido,
        unidades = excluded.unidades, genero = excluded.genero,
        subcategoria = excluded.subcategoria, descripcion = excluded.descripcion")
    dbExecute(con, "DELETE FROM productos WHERE producto_id NOT IN (SELECT producto_id FROM tmp_productos)")
    dbExecute(con, "DROP TABLE tmp_productos")

    # Campos estandarizados de cada anuncio.
    std <- anuncios |>
      select(anuncio_id, marca_std, linea_std, concentracion_std, contenido_ml, unidades,
             genero_std, subcategoria_std, excluido, motivo_exclusion, producto_id,
             regla_correspondencia)
    dbWriteTable(con, "tmp_std", std, temporary = TRUE, overwrite = TRUE)
    dbExecute(con, "
      UPDATE anuncios
         SET marca_std = t.marca_std, linea_std = t.linea_std,
             concentracion_std = t.concentracion_std, contenido_ml = t.contenido_ml,
             unidades = t.unidades, genero_std = t.genero_std,
             subcategoria_std = t.subcategoria_std, excluido = t.excluido,
             motivo_exclusion = t.motivo_exclusion, producto_id = t.producto_id,
             regla_correspondencia = t.regla_correspondencia
        FROM tmp_std t
       WHERE anuncios.anuncio_id = t.anuncio_id")
    dbExecute(con, "DROP TABLE tmp_std")

    # Indicador comparable: la observación pertenece a una presentación del
    # catálogo que, en esa misma ejecución, tiene precio en los tres comercios
    # elegidos.
    ids <- comercios_configurados(con)
    lista_ids <- paste(as.integer(ids), collapse = ", ")
    dbExecute(con, sprintf("
      UPDATE observaciones
         SET comparable = CASE WHEN observaciones.comercio_id IN (%s) AND EXISTS (
               SELECT 1
                 FROM anuncios a
                 JOIN (SELECT o2.id_ejecucion, a2.producto_id
                         FROM observaciones o2
                         JOIN anuncios a2 ON a2.anuncio_id = o2.anuncio_id
                        WHERE a2.producto_id IS NOT NULL AND o2.precio IS NOT NULL
                          AND o2.comercio_id IN (%s)
                        GROUP BY o2.id_ejecucion, a2.producto_id
                       HAVING COUNT(DISTINCT o2.comercio_id) = %d
                      ) cob
                   ON cob.producto_id = a.producto_id
                WHERE a.anuncio_id = observaciones.anuncio_id
                  AND cob.id_ejecucion = observaciones.id_ejecucion
                  AND observaciones.precio IS NOT NULL)
             THEN 1 ELSE 0 END", lista_ids, lista_ids, length(ids)))
  })

  resumen <- dbGetQuery(con, "
    SELECT c.nombre AS comercio,
           COUNT(*) AS anuncios,
           SUM(a.excluido) AS excluidos,
           SUM(a.producto_id IS NOT NULL) AS con_producto_comun
      FROM anuncios a JOIN comercios c ON c.comercio_id = a.comercio_id
     GROUP BY c.nombre ORDER BY c.nombre")
  message("Procesamiento terminado:")
  print(resumen)
  invisible(anuncios)
}

# Ejecución que se usa en el análisis: entre las que tienen al menos 8
# presentaciones comparables en los tres comercios, se prefieren las que
# quedaron "completa" en los tres; entre ellas, la más reciente. Si ninguna
# está completa en los tres, se usa la más reciente con 8 o más (el documento
# muestra el estado de cada comercio).
ejecucion_analisis <- function(con) {
  ids <- comercios_configurados(con)
  candidatas <- dbGetQuery(con, sprintf("
    WITH cobertura AS (
           SELECT o.id_ejecucion, COUNT(DISTINCT a.producto_id) AS presentaciones
             FROM observaciones o
             JOIN anuncios a ON a.anuncio_id = o.anuncio_id
            WHERE o.comparable = 1
            GROUP BY o.id_ejecucion
           HAVING COUNT(DISTINCT a.producto_id) >= 8),
         estados AS (
           SELECT id_ejecucion, SUM(estado = 'completa') AS completas
             FROM ejecuciones
            WHERE comercio_id IN (%s)
            GROUP BY id_ejecucion)
    SELECT c.id_ejecucion, c.presentaciones, COALESCE(e.completas, 0) AS completas
      FROM cobertura c
      LEFT JOIN estados e ON e.id_ejecucion = c.id_ejecucion
     ORDER BY (COALESCE(e.completas, 0) = %d) DESC, c.id_ejecucion DESC",
    paste(as.integer(ids), collapse = ", "), length(ids)))
  if (nrow(candidatas) == 0) return(NA_character_)
  candidatas$id_ejecucion[1]
}
