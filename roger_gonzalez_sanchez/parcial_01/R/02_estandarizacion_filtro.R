# 02_estandarizacion_filtro.R
# Normalización de texto, extracción de contenido/unidad, marca estandarizada,
# subcategoría y filtro estricto de la categoría (listas negra y blanca).


# Texto auxiliar para reglas: minúsculas, sin tildes, sin signos raros.
normalizar_texto <- function(x) {
  x <- coalesce(as.character(x), "")
  x <- str_replace_all(x, "[\u00b5\u03bc]g", "mcg")   # µg / μg -> mcg (no es "g")
  x <- str_replace_all(x, "[\u00b5\u03bc]l", "ul")
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9.,%/+ ]", " ")
  str_squish(x)
}


detectar_primero <- function(texto, diccionario) {
  if (length(texto) == 0) return(character(0))
  pos <- vapply(diccionario, function(p) {
    s <- str_locate(texto, regex(p))[, "start"]
    ifelse(is.na(s), Inf, s)
  }, numeric(length(texto)))
  if (is.null(dim(pos))) pos <- matrix(pos, nrow = length(texto))
  idx <- apply(pos, 1, which.min)
  ok <- apply(pos, 1, function(r) any(is.finite(r)))
  out <- names(diccionario)[idx]
  out[!ok] <- NA_character_
  out
}


# Contenido del envase -> (contenido_num, unidad_medida)
RX_NUM         <- "(\\d+(?:[.,]\\d+)?)"
UNID_DOSIS     <- "dosis|puff|pulsaciones|atomizaciones|inhalaciones"
UNID_SOBRES    <- "sobres?|sob|sachets?"
UNID_CONTEO    <- paste0("tabletas?|tabs?|comprimidos?|comp|capsulas?|caps?|cap|grageas?|",
                         "perlas?|softgels?|unidades|unidad|unds?|und|u|pastillas?|chicles?|",
                         "ovulos?|ampollas?|viales?|parches?")
TOKENS_GENERICOS <- c("u", "un", "und", "unds", "unidad", "unidades")

buscar_cantidades <- function(t, unidades) {
  patron <- paste0("(?<![a-z0-9])(x|por)?\\s*", RX_NUM, "\\s*(", unidades, ")(?![a-z])")
  m <- str_match_all(t, patron)[[1]]
  if (nrow(m) == 0) return(NULL)
  data.frame(con_x = !is.na(m[, 2]) & m[, 2] != "",
             valor = as.numeric(str_replace(m[, 3], ",", ".")),
             token = m[, 4], stringsAsFactors = FALSE)
}

elegir_valor <- function(m, criterio = c("max", "ultimo")) {
  criterio <- match.arg(criterio)
  base <- if (any(m$con_x)) m[m$con_x, , drop = FALSE] else m
  if (criterio == "max") base[which.max(base$valor), , drop = FALSE] else base[nrow(base), , drop = FALSE]
}

extraer_contenido_1 <- function(texto) {
  t <- normalizar_texto(texto)
  if (t == "") return(list(NA_real_, NA_character_))

  # Multiplicador de empaques: "pack x 5" de una presentación "x 2 tabletas"
  mult <- 1
  pm <- str_match(t, "(?<![a-z])(pack|paquete|kit|display)\\s*(x|por)\\s*(\\d+)")
  if (!is.na(pm[1, 1])) {
    mult <- as.numeric(pm[1, 4])
    t <- str_replace(t, fixed(pm[1, 1]), " ")
  }

  # Volumen / peso (se usan como respaldo o cuando la cuenta es trivial = 1).
  # Conversión a la unidad base ANTES de comparar: 1 L = 1000 mL; 1 kg = 1000 g.
  vol <- convertir_unidad(buscar_cantidades(t, UNID_VOLUMEN), "^(l|lt|lts|litros?)$", 1000)
  pes <- convertir_unidad(buscar_cantidades(t, UNID_PESO), "^(kg|kilos?|kilogramos?)$", 1000)
  # Paquetes múltiples: "2 frascos de 200 ml" o "200 ml x 2" = 400 mL en total.
  mp_vol <- contenido_multiple(t, UNID_VOLUMEN, "^(l|lt|lts|litros?)$")
  mp_pes <- contenido_multiple(t, UNID_PESO, "^(kg|kilos?|kilogramos?)$")

  dos <- buscar_cantidades(t, UNID_DOSIS)
  if (!is.null(dos)) {
    d <- elegir_valor(dos, "max")
    if (d$valor > 1) return(list(d$valor, "dosis"))
  }
  sob <- buscar_cantidades(t, UNID_SOBRES)
  if (!is.null(sob)) {
    s <- elegir_valor(sob, "max")
    return(list(s$valor * mult, "sobres"))
  }
  con <- buscar_cantidades(t, UNID_CONTEO)
  if (!is.null(con)) {
    k <- elegir_valor(con, "max")
    trivial <- k$valor == 1 && mult == 1 && (!is.null(vol) || !is.null(pes))
    if (!trivial) {
      unidad <- if (k$token %in% TOKENS_GENERICOS &&
                    str_detect(t, "\\bsobres?\\b|\\bsob\\b|granulad|granulos")) "sobres" else "unidad"
      return(list(k$valor * mult, unidad))
    }
  }
  if (str_detect(t, "\\bsobre\\b|sobre individual|\\bsachet\\b") && !str_detect(t, "\\bsobres\\b")) {
    return(list(1 * mult, "sobres"))
  }
  if (!is.na(mp_vol)) return(list(mp_vol * mult, "mL"))
  if (!is.null(vol)) return(list(elegir_valor(vol, "ultimo")$valor * mult, "mL"))
  if (!is.na(mp_pes)) return(list(mp_pes * mult, "g"))
  if (!is.null(pes)) return(list(elegir_valor(pes, "ultimo")$valor * mult, "g"))

  caja <- str_match(t, paste0("(?<![a-z])(caja|cja|caj|blister|estuche|empaque)\\s*(x|por)?\\s*(\\d+)",
                              "(?!\\s*(mg|mcg|ml|g\\b|gr|%|[.,]\\d))"))
  if (!is.na(caja[1, 1])) return(list(as.numeric(caja[1, 4]) * mult, "unidad"))
  fin <- str_match(t, "(?<![a-z])x\\s*(\\d+)\\s*$")
  if (!is.na(fin[1, 1])) return(list(as.numeric(fin[1, 2]) * mult, "unidad"))
  if (mult > 1) return(list(mult, "unidad"))


  list(NA_real_, NA_character_)
}

UNID_VOLUMEN <- "ml|mililitros?|cc|l|lt|lts|litros?"
UNID_PESO    <- "g|gr|grs|gramos?|kg|kilos?|kilogramos?"

# Multiplica por `factor` los valores cuya unidad coincide con `regex_token`.
convertir_unidad <- function(m, regex_token, factor) {
  if (is.null(m)) return(NULL)
  grande <- str_detect(m$token, regex_token)
  m$valor[grande] <- m$valor[grande] * factor
  m
}

# Contenido total de paquetes múltiples ("2 frascos de 200 ml", "200 ml x 2").
# Devuelve el total en la unidad base (mL o g) o NA si no hay paquete múltiple.
contenido_multiple <- function(t, unidades, regex_grande) {
  envases <- "(?:frascos?|envases?|tubos?|botellas?|unidades|und|u)?"
  a <- str_match(t, paste0("(?<![a-z0-9.,])(\\d{1,2})\\s*", envases, "\\s*(?:x|por|de)\\s*",
                           "(\\d+(?:[.,]\\d+)?)\\s*(", unidades, ")(?![a-z])"))
  b <- str_match(t, paste0("(?<![a-z0-9.,])(\\d+(?:[.,]\\d+)?)\\s*(", unidades, ")\\s*(?:x|por)\\s*",
                           "(\\d{1,2})(?![\\d.,]|\\s*(?:mg|mcg|ml|g|gr|%|", unidades, ")\\b)"))
  total <- function(n, valor, token) {
    n <- as.numeric(n); v <- as.numeric(str_replace(valor, ",", "."))
    if (is.na(n) || n < 2) return(NA_real_)
    n * v * if (str_detect(token, regex_grande)) 1000 else 1
  }
  if (!is.na(a[1, 1])) return(total(a[1, 2], a[1, 3], a[1, 4]))
  if (!is.na(b[1, 1])) return(total(b[1, 4], b[1, 2], b[1, 3]))
  NA_real_
}

extraer_contenido <- function(texto) {
  r <- map(texto, extraer_contenido_1)
  tibble(contenido_num = map_dbl(r, 1), unidad_medida = map_chr(r, 2))
}


# Marca estandarizada: 1) marca comercial en el nombre, 2) laboratorio en el
# nombre, 3) marca/proveedor reportado por el comercio (si no es un código).

estandarizar_marca <- function(nombre, marca_comercio = NA, proveedor = NA) {
  n  <- normalizar_texto(nombre)
  mc <- normalizar_texto(marca_comercio)
  pv <- normalizar_texto(proveedor)
  marca_valida <- mc != "" & !str_detect(mc, "^[0-9]|\\d{4}m|\\d{5,}|sin marca|generico|^n/?a$")
  titulo <- ifelse(marca_valida, str_to_title(str_squish(as.character(marca_comercio))), NA_character_)
  coalesce(
    detectar_primero(n, MARCAS_COMERCIALES),
    detectar_primero(n, LABORATORIOS),
    ifelse(marca_valida, detectar_primero(mc, MARCAS_COMERCIALES), NA_character_),
    ifelse(marca_valida, detectar_primero(mc, LABORATORIOS), NA_character_),
    detectar_primero(pv, LABORATORIOS),
    titulo,
    marca_primer_token(nombre)
  )
}

# Último recurso: nombres del tipo "Rupafin Rupatadina 10 mg ..." -> "Rupafin",
# siempre que la primera palabra no sea un principio activo ni un descriptor.
PALABRAS_NO_MARCA <- c(
  "acetaminofen", "paracetamol", "ibuprofeno", "furoato", "propionato", "dipropionato",
  "bromuro", "clorhidrato", "maleato", "sulfato", "jarabe", "tabletas", "tableta",
  "capsulas", "solucion", "suspension", "spray", "inhalador", "gotas", "pastillas",
  "obsequio", "oferta", "promo", "combo", "pack", "caja", "frasco", "sobre", "sobres",
  "granulado", "unguento", "crema", "gel", "vitamina", "ninos", "adultos", "noche",
  "dia", "fraccion", "kit", "nasal", "expectorante", "antigripal", "descongestionante")
marca_primer_token <- function(nombre) {
  tok <- str_match(coalesce(nombre, ""), "^\\s*([[:alpha:]][[:alpha:]0-9-]{2,})")[, 2]
  tok_n <- normalizar_texto(tok)
  regex_dci <- paste(c(LISTA_BLANCA_TALLER, LISTA_BLANCA_COMPLEMENTARIA, REGEX_SUBCATEGORIA), collapse = "|")
  valido <- !is.na(tok) & !tok_n %in% PALABRAS_NO_MARCA & !str_detect(tok_n, regex_dci)
  ifelse(valido, str_to_title(tok), NA_character_)
}

asignar_subcategoria <- function(texto_normalizado) {
  detectar_primero_orden <- function(t) {
    for (nm in names(REGEX_SUBCATEGORIA)) if (str_detect(t, REGEX_SUBCATEGORIA[[nm]])) return(nm)
    NA_character_
  }
  vapply(texto_normalizado, detectar_primero_orden, character(1), USE.NAMES = FALSE)
}


# Estandarización de las filas crudas de cualquier comercio: 13 columnas
# estándar + 6 de trazabilidad (texto original) + auxiliares aux_* que usan el
# filtro y la Fase 1.

estandarizar_crudo <- function(df, ctx) {
  if (nrow(df) == 0) return(plantilla_estandar())
  cont <- extraer_contenido(df$aux_texto_contenido)
  falta <- is.na(cont$contenido_num)
  if (any(falta)) cont[falta, ] <- extraer_contenido(df$producto_nombre_original[falta])

  df |>
    mutate(
      comercio = ctx$comercio,
      sku_comercio = as.character(sku_comercio),
      marca_estandarizada = estandarizar_marca(producto_nombre_original, aux_marca_comercio, aux_proveedor),
      subcategoria_estandar = NA_character_,
      contenido_num = cont$contenido_num,
      unidad_medida = cont$unidad_medida,
      precio_regular_cop = if_else(is.na(precio_regular_cop) | precio_regular_cop <= 0,
                                   precio_cop, precio_regular_cop),
      precio_regular_cop = pmax(precio_regular_cop, precio_cop),
      es_promocion = as.integer(!is.na(precio_cop) & precio_cop < precio_regular_cop),
      disponible = as.integer(aux_disponible),
      aux_extraible = !is.na(sku_comercio) & !is.na(producto_nombre_original) &
        str_trim(producto_nombre_original) != "" & !is.na(precio_cop) & precio_cop > 0 &
        coalesce(aux_activo, TRUE) &
        (!EXIGIR_DISPONIBILIDAD | coalesce(aux_disponible, TRUE))
    ) |>
    select(all_of(COLUMNAS_ESTANDAR), any_of(COLUMNAS_EXTRA), starts_with("aux_")) |>
    arrange(desc(aux_extraible)) |>
    distinct(comercio, sku_comercio, .keep_all = TRUE)
}

plantilla_estandar <- function() {
  tibble(comercio = character(), sku_comercio = character(),
         producto_nombre_original = character(), marca_estandarizada = character(),
         subcategoria_estandar = character(), contenido_num = numeric(),
         unidad_medida = character(), precio_cop = numeric(),
         precio_regular_cop = numeric(), es_promocion = integer(),
         url_producto = character(), fecha_hora_consulta = character(),
         id_ejecucion = character(),
         presentacion_original = character(), descripcion_original = character(),
         ficha_tecnica_original = character(), categoria_origen = character(),
         url_consulta = character(), archivo_raw = character(), disponible = integer())
}

# FILTRO ESTRICTO DE LA CATEGORÍA
# Orden de decisión (la primera regla que aplica decide):
#   1. Sin precio/SKU válido                         -> excluido
#   2. Categoría del comercio ajena a salud          -> excluido
#   3. Lista NEGRA del taller (descarte obligatorio) -> excluido
#   4. Ibuprofeno sin componente respiratorio        -> excluido
#   5. Exclusiones complementarias (dispositivos...) -> excluido
#   6. Sin coincidencia con la lista BLANCA          -> excluido
#   7. Sin subcategoría estándar asignable           -> excluido
#   8. Resto                                         -> INCLUIDO + subcategoría
# Devuelve las 13 columnas estándar + las 6 de trazabilidad; la traza completa
# de la decisión queda en attr(resultado, "auditoria_filtro").

filtrar_categoria_respiratorios <- function(df, verbose = TRUE) {
  if (nrow(df) == 0) return(plantilla_estandar())
  col_o_na <- function(nm, defecto = NA) if (nm %in% names(df)) df[[nm]] else rep(defecto, nrow(df))

  texto  <- normalizar_texto(paste(df$producto_nombre_original, coalesce(col_o_na("aux_principio_activo"), "")))
  texto_m <- str_squish(paste(texto, normalizar_texto(df$marca_estandarizada)))
  extraible <- coalesce(as.logical(col_o_na("aux_extraible", TRUE)), TRUE)
  es_salud  <- coalesce(as.logical(col_o_na("aux_es_salud", TRUE)), TRUE)

  negra      <- detectar_primero(texto, LISTA_NEGRA_TALLER)
  ibu_solo   <- str_detect(texto, REGEX_IBUPROFENO) & !str_detect(texto, REGEX_COMPONENTE_RESPIRATORIO)
  negra_comp <- detectar_primero(texto, LISTA_NEGRA_COMPLEMENTARIA)
  blanca     <- detectar_primero(texto_m, LISTA_BLANCA_TALLER)
  blanca_c   <- detectar_primero(texto_m, LISTA_BLANCA_COMPLEMENTARIA)
  subcat     <- asignar_subcategoria(texto_m)

  motivo <- case_when(
    !extraible            ~ "Excluido: sin precio/SKU válido (no extraíble)",
    !es_salud             ~ "Excluido: categoría del comercio ajena a salud",
    !is.na(negra)         ~ paste0("Excluido lista negra: ", negra),
    ibu_solo              ~ "Excluido lista negra: Ibuprofeno (solo)",
    !is.na(negra_comp)    ~ paste0("Excluido (complementaria): ", negra_comp),
    is.na(blanca) & is.na(blanca_c) ~ "Excluido: sin coincidencia con lista blanca",
    is.na(subcat)         ~ "Excluido: sin subcategoría estándar",
    !is.na(blanca)        ~ paste0("Incluido lista blanca: ", blanca),
    TRUE                  ~ paste0("Incluido (complementaria): ", blanca_c)
  )
  incluido <- str_starts(motivo, "Incluido")

  auditoria <- df |>
    mutate(subcategoria_estandar = if_else(incluido, subcat, NA_character_),
           incluido = incluido, motivo_filtro = motivo)

  resultado <- auditoria |>
    filter(incluido) |>
    select(all_of(COLUMNAS_ESTANDAR), any_of(COLUMNAS_EXTRA))
  attr(resultado, "auditoria_filtro") <- auditoria

  if (verbose) {
    com <- paste(unique(df$comercio), collapse = ", ")
    log_msg(sprintf("  Filtro %s: %d de %d registros incluidos (%d excluidos)",
                    com, sum(incluido), nrow(df), sum(!incluido)))
  }
  resultado
}

resumen_filtro <- function(df_filtrado) {
  aud <- attr(df_filtrado, "auditoria_filtro")
  if (is.null(aud)) return(NULL)
  aud |> mutate(motivo = str_remove(motivo_filtro, ":.*$")) |>
    count(comercio, incluido, motivo_filtro, sort = TRUE)
}
