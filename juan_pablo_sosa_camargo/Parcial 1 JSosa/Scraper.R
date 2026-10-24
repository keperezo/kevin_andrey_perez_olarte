# =============================================================
# scraper.R (v4) - Higiene y cuidado del bebé (Éxito, Carulla, Jumbo)
# SCRIPT 1 de 3: recolección, limpieza y carga a SQLite.
# (Los comparables viven en comparables.R; el orquestador es correr_todo.R)
#
# Categoría: jabones, champús, lociones, aceites, cremas hidratantes
# y colonias infantiles. Excluye cambio de pañal, protección solar,
# cuidado bucal, pomadas, kits y peinado.
#
# Uso rápido:
#   source("scraper.R")
#   datos <- ejecutar_recoleccion(modo = "offline")   # reprocesa raw/ sin internet
#   datos <- ejecutar_recoleccion(modo = "online")    # consulta los sitios
# =============================================================

suppressPackageStartupMessages({
  library(rvest); library(xml2); library(chromote)
  library(dplyr); library(purrr); library(stringr); library(tidyr)
  library(readr); library(tibble); library(DBI); library(RSQLite)
})

# -------------------------------------------------------------
# 0) CONFIGURACIÓN GENERAL
# -------------------------------------------------------------
TERMINOS <- c("jabon bebe", "shampoo bebe", "locion bebe",
              "aceite bebe", "crema bebe", "colonia bebe")

INCLUIR_KIDS <- TRUE

ahora_iso <- function(t = Sys.time()) {
  s <- format(t, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Bogota")
  sub("(\\d{2})(\\d{2})$", "\\1:\\2", s)
}

configurar_chromote <- function(timeout = 60) {
  obj <- chromote::default_chromote_object()
  obj$default_timeout <- timeout
  chromote::set_default_chromote_object(obj)
}

COMERCIOS <- list(
  exito = list(
    nombre = "Éxito", base = "https://www.exito.com", pagina_inicial = 0,
    construir_url = function(termino, pagina) {
      paste0("https://www.exito.com/s?q=", URLencode(termino, reserved = TRUE),
             "&sort=score_desc&page=", pagina)
    },
    tipo = "css",
    selector_ficha  = "article[class*='productCard_productCard']",
    selector_nombre = "h3[class*='styles_name']",
    selector_precio = "p[data-fs-container-price-otros='true']",
    selector_precio_antes = "p[data-fs-container-price-listing='true']"
  ),
  carulla = list(
    nombre = "Carulla", base = "https://www.carulla.com", pagina_inicial = 0,
    construir_url = function(termino, pagina) {
      paste0("https://www.carulla.com/s?q=", URLencode(termino, reserved = TRUE),
             "&sort=score_desc&page=", pagina)
    },
    tipo = "css",
    selector_ficha  = "article[class*='productCard_productCard']",
    selector_nombre = "h3[class*='styles_name']",
    selector_precio = "p[data-fs-container-price-otros='true']",
    selector_precio_antes = "p[data-fs-container-price-listing='true']"
  ),
  jumbo = list(
    nombre = "Jumbo", base = "https://www.jumbocolombia.com", pagina_inicial = 1,
    construir_url = function(termino, pagina) {
      paste0("https://www.jumbocolombia.com/search?query=",
             URLencode(termino, reserved = TRUE), "&type=term&page=", pagina)
    },
    tipo = "jumbo_ds",
    selector_ficha = "h3[class*='ProductCardstyles']"
  )
)

tabla_vacia <- function() {
  tibble(
    comercio = character(), termino_busqueda = character(),
    nombre_original = character(), texto_anuncio = character(),
    marca_sitio = character(),
    precio_txt = character(), precio_antes_txt = character(),
    precio_tarjeta_txt = character(),
    sku = character(), url_producto = character(), url_busqueda = character(),
    pagina = integer(), fecha_hora_consulta = character(), ejecucion_id = character()
  )
}

# -------------------------------------------------------------
# 1) DESCARGA Y PARSEO (siempre sobre HTML estático)
# -------------------------------------------------------------
obtener_html_vivo <- function(url, selector_ficha, espera_max = 12, intentos = 2) {
  js_contar <- sprintf("document.querySelectorAll(%s).length",
                       jsonlite::toJSON(selector_ficha, auto_unbox = TRUE))
  ultimo_error <- NULL
  for (k in seq_len(intentos)) {
    html <- tryCatch({
      sesion <- NULL
      sesion <- read_html_live(url)
      t0 <- Sys.time(); previo <- -1; estable <- 0
      repeat {
        n <- tryCatch(sesion$session$Runtime$evaluate(js_contar)$result$value,
                      error = function(e) 0)
        if (is.null(n)) n <- 0
        try(sesion$session$Runtime$evaluate("window.scrollBy(0, 2500)"), silent = TRUE)
        if (n > 0 && n == previo) estable <- estable + 1 else estable <- 0
        if (estable >= 2) break
        if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > espera_max) break
        previo <- n
        Sys.sleep(0.7)
      }
      sesion$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    }, error = function(e) { ultimo_error <<- e; NULL },
    finally = if (!is.null(sesion)) try(sesion$session$close(), silent = TRUE))
    
    if (is.character(html) && length(html) == 1 && nzchar(html)) return(html)
    message(sprintf("   intento %d/%d falló: %s", k, intentos,
                    if (is.null(ultimo_error)) "HTML vacío" else conditionMessage(ultimo_error)))
    Sys.sleep(2)
  }
  stop(if (is.null(ultimo_error)) "HTML vacío" else conditionMessage(ultimo_error))
}

# --- Jumbo (diseño "jumbo-ds") -------------------------------
#   Paga $ X -> precio con tarjeta Jumbo Más -> precio_tarjeta
#   1.er $   -> precio público actual         -> precio
#   2.º $    -> precio antes (tachado)        -> precio_antes
parsear_jumbo <- function(doc, cfg) {
  h3 <- html_elements(doc, cfg$selector_ficha)
  if (length(h3) == 0) return(NULL)
  nodos   <- xml_parent(h3)
  nombres <- html_text2(h3)
  
  marcas <- map_chr(seq_along(nodos), function(i) {
    m <- html_text2(html_element(nodos[[i]], "p"))
    if (is.na(m) || str_detect(m, "^\\$")) NA_character_ else str_squish(m)
  })
  
  txt <- html_text2(nodos) %>% str_replace_all("\u00a0", " ") %>% str_squish()
  tarjeta  <- str_match(txt, "Paga\\s*\\$\\s*(\\d[\\d\\.]*)")[, 2]
  sin_paga <- str_remove(txt, "Paga\\s*\\$\\s*\\d[\\d\\.]*")
  pr <- str_extract_all(sin_paga, "\\$\\s*\\d[\\d\\.]*")
  actual <- map_chr(pr, ~ if (length(.x) >= 1) .x[1] else NA_character_)
  antes  <- map_chr(pr, ~ if (length(.x) >= 2) .x[2] else NA_character_)
  actual <- ifelse(is.na(actual) & !is.na(tarjeta), paste0("$", tarjeta), actual)
  
  href <- map_chr(seq_along(nodos), function(i) {
    a <- html_element(nodos[[i]], "a")
    if (inherits(a, "xml_missing")) a <- html_element(nodos[[i]], xpath = "ancestor::a[1]")
    html_attr(a, "href")
  })
  url_producto <- ifelse(is.na(href), NA_character_, url_absolute(href, cfg$base))
  
  tibble(
    nombre_original    = nombres,
    texto_anuncio      = html_text2(nodos),
    marca_sitio        = marcas,
    precio_txt         = actual,
    precio_antes_txt   = antes,
    precio_tarjeta_txt = ifelse(is.na(tarjeta), NA_character_, paste0("$", tarjeta)),
    sku                = str_match(url_producto, "-(\\d+)/p")[, 2],
    url_producto       = url_producto
  )
}

parsear_pagina <- function(doc, cfg) {
  if (cfg$tipo == "jumbo_ds") return(parsear_jumbo(doc, cfg))
  
  nodos <- html_elements(doc, cfg$selector_ficha)
  if (length(nodos) == 0) return(NULL)
  
  nombres <- nodos %>% html_element(cfg$selector_nombre) %>% html_text2()
  precios <- nodos %>% html_element(cfg$selector_precio) %>% html_text2()
  antes   <- nodos %>% html_element(cfg$selector_precio_antes) %>% html_text2()
  
  href <- nodos %>% html_element("a") %>% html_attr("href")
  url_producto <- ifelse(is.na(href), NA_character_, url_absolute(href, cfg$base))
  skus <- str_match(url_producto, "-(\\d+)/p")[, 2]
  
  tibble(
    nombre_original    = nombres,
    texto_anuncio      = html_text2(nodos),
    marca_sitio        = NA_character_,
    precio_txt         = precios,
    precio_antes_txt   = antes,
    precio_tarjeta_txt = NA_character_,
    sku                = skus,
    url_producto       = url_producto
  )
}

# -------------------------------------------------------------
# 2) COPIAS CRUDAS
# -------------------------------------------------------------
guardar_raw <- function(html, carpeta, run_id, termino, pagina) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  slug <- str_replace_all(termino, "[^a-z0-9]+", "-")
  archivo <- sprintf("%s_p%02d.html.gz", slug, pagina)
  con <- gzfile(file.path(carpeta, archivo), "w", encoding = "UTF-8")
  writeLines(html, con); close(con)
  archivo
}

# -------------------------------------------------------------
# 3) SCRAPING DE UN COMERCIO (online u offline)
# -------------------------------------------------------------
scrape_comercio <- function(id, terminos, run_id, modo = "online",
                            dir_raw = "raw", max_paginas = 30) {
  cfg <- COMERCIOS[[id]]
  carpeta <- file.path(dir_raw, id, run_id)
  filas <- list(); indice <- list(); errores <- 0
  vistos <- character()
  
  if (modo == "online") {
    for (t in terminos) {
      n_prev <- Inf
      for (p in seq(cfg$pagina_inicial, length.out = max_paginas)) {
        url <- cfg$construir_url(t, p)
        message(sprintf("-> %s | '%s' | página %d", cfg$nombre, t, p))
        html <- tryCatch(obtener_html_vivo(url, cfg$selector_ficha),
                         error = function(e) { message("   error: ", conditionMessage(e)); NULL })
        if (is.null(html)) { errores <- errores + 1; break }
        
        pag <- parsear_pagina(read_html(html), cfg)
        if (is.null(pag)) break
        clave <- coalesce(pag$url_producto, pag$nombre_original)
        nuevos <- !(clave %in% vistos)
        vistos <- union(vistos, clave)
        if (!any(nuevos)) break
        
        archivo <- guardar_raw(html, carpeta, run_id, t, p)
        fh <- ahora_iso()
        indice[[length(indice) + 1]] <- tibble(archivo = archivo, termino = t, pagina = p,
                                               url = url, fecha_hora = fh)
        filas[[length(filas) + 1]] <- pag %>% mutate(
          comercio = cfg$nombre, termino_busqueda = t, url_busqueda = url,
          pagina = as.integer(p), fecha_hora_consulta = fh, ejecucion_id = run_id)
        if (nrow(pag) < n_prev * 0.8 && p > cfg$pagina_inicial) break
        n_prev <- nrow(pag)
        Sys.sleep(runif(1, 0.5, 1.5))
      }
    }
    if (length(indice) > 0) write_csv(bind_rows(indice), file.path(carpeta, "indice.csv"))
    
  } else {  # offline
    if (!file.exists(file.path(carpeta, "indice.csv"))) {
      message("   Sin copias crudas para ", cfg$nombre, " en ", carpeta)
      return(list(datos = tabla_vacia(), estado = "fallida"))
    }
    idx <- read_csv(file.path(carpeta, "indice.csv"), show_col_types = FALSE)
    for (i in seq_len(nrow(idx))) {
      html <- paste(readLines(gzfile(file.path(carpeta, idx$archivo[i]), encoding = "UTF-8"),
                              warn = FALSE), collapse = "\n")
      pag <- parsear_pagina(read_html(html), cfg)
      if (is.null(pag)) next
      filas[[length(filas) + 1]] <- pag %>% mutate(
        comercio = cfg$nombre, termino_busqueda = idx$termino[i], url_busqueda = idx$url[i],
        pagina = as.integer(idx$pagina[i]), fecha_hora_consulta = idx$fecha_hora[i],
        ejecucion_id = run_id)
    }
  }
  
  datos <- if (length(filas) > 0) bind_rows(filas) else tabla_vacia()
  estado <- if (nrow(datos) == 0) "fallida" else if (errores > 0) "parcial" else "completa"
  list(datos = datos, estado = estado)
}

scrape_exito   <- function(...) scrape_comercio("exito",   ...)
scrape_carulla <- function(...) scrape_comercio("carulla", ...)
scrape_jumbo   <- function(...) scrape_comercio("jumbo",   ...)

# -------------------------------------------------------------
# 4) LIMPIEZA Y ESTANDARIZACIÓN
# -------------------------------------------------------------

# 4.1 Marca ---------------------------------------------------
MARCAS <- c(
  "Johnson's" = "johnson", "Pequeñín" = "peque[ñn][ií]n", "Arrurru" = "arrurru",
  "Bebesit" = "bebesit", "Mustela" = "mustela", "Cerave" = "cerave",
  "Cetaphil Baby" = "cetaphil", "Aveeno Baby" = "aveeno", "Nivea Baby" = "\\bnivea\\b",
  "Baby Dove" = "\\bdove\\b", "Eucerin" = "eucerin", "Sebamed" = "sebamed",
  "Bepanthol" = "bepanth", "Chicco" = "chicco", "Huggies" = "huggies",
  "Pampers" = "pampers", "Babysec" = "babysec", "Mennen Baby" = "mennen",
  "Natura" = "\\bnatura\\b", "Weleda" = "weleda", "Avent" = "\\bavent\\b",
  "Nuk" = "\\bnuk\\b", "Prenatal" = "prenatal", "Ponds" = "\\bponds?\\b",
  "Vaseline" = "vaseline", "Burt's Bees" = "burt", "Almipro" = "almipro",
  "Muss" = "\\bmuss\\b", "AMPM Plus" = "\\bampm\\b", "Personal Choice" = "personal choice",
  "Acid Mantle" = "acid mantle", "Bioderma" = "bioderma", "La Roche-Posay" = "roche",
  "Avène" = "av[eè]ne"
)

MARCAS_BEBE <- c("Johnson's", "Pequeñín", "Arrurru", "Mustela", "Bebesit", "Chicco")

GENERICAS <- c("DE","DEL","LA","EL","LOS","LAS","PARA","CON","SIN","Y","E","EN","POR","UN","UNA",
               "BEBE","BEBES","BABY","NINOS","NINAS","KIDS","INFANTIL","SHAMPOO","CHAMPU",
               "JABON","LIQUIDO","LOCION","ACEITE","CREMA","COLONIA","BANO","GEL","WASH",
               "ML","CC","GR","G","KG","L","LT","X","SPF","FPS","PH","UND","UNDS","UN","PACK",
               "HIDRATANTE","SUAVE","ORIGINAL","CABELLO","PIEL","SENSIBLE","CORPORAL","AGUA",
               "ALMENDRAS","MANZANILLA","AVENA","LAGRIMAS","ESPUMA","FRASCO","NUEVO","NEW","S")

marca_mayusculas <- function(nombre) {
  map_chr(nombre, function(s) {
    if (is.na(s)) return(NA_character_)
    runs <- str_extract_all(s, "(?<![\\p{L}'’])\\p{Lu}{2,}(?:\\s+\\p{Lu}{2,})*(?![\\p{L}])")[[1]]
    for (r in runs) {
      tk <- str_split(r, "\\s+")[[1]]
      asc <- stringi::stri_trans_general(tk, "Latin-ASCII")
      keep <- tk[!(asc %in% GENERICAS)]
      if (length(keep)) return(paste(keep, collapse = " "))
    }
    NA_character_
  })
}

normalizar_marca <- function(x) {
  y <- str_to_title(str_squish(str_to_lower(x)))
  fix <- c("Ampm Plus" = "AMPM Plus", "Ampm" = "AMPM", "Johnson S" = "Johnson's",
           "Johnsons" = "Johnson's", "Cerave" = "Cerave")
  y <- ifelse(y %in% names(fix), unname(fix[y]), y)
  y[is.na(x) | !nzchar(str_squish(x))] <- NA_character_
  y
}

extraer_marca <- function(nombre, marca_sitio = NA_character_) {
  n <- length(nombre); marca_sitio <- rep_len(marca_sitio, n)
  x <- str_to_lower(paste(coalesce(marca_sitio, ""), coalesce(nombre, "")))
  dic <- map_chr(x, function(s) {
    hit <- names(MARCAS)[str_detect(s, MARCAS)]
    if (length(hit)) hit[1] else NA_character_
  })
  coalesce(dic, normalizar_marca(marca_sitio), normalizar_marca(marca_mayusculas(nombre)))
}

# 4.2 Público, subcategoría y exclusiones ----------------------
RE_BEBE <- "beb[eé]|baby|reci[eé]n nacido|lactante"
RE_KIDS <- "ni[ñn]os?\\b|ni[ñn]as?\\b|infantil|kids|junior"

clasificar_publico <- function(texto, marca) {
  x <- str_to_lower(texto)
  case_when(
    str_detect(x, RE_BEBE) | marca %in% MARCAS_BEBE ~ "bebé",
    str_detect(x, RE_KIDS) ~ "niños",
    TRUE ~ NA_character_
  )
}

SUBCATS <- c(
  "Champú"  = "champ[uú]|shampoo|sampoo",
  "Colonia" = "colonia|fragancia|perfume|cologne",
  "Aceite"  = "aceite",
  "Loción"  = "loci[oó]n|lotion",
  "Jabón"   = "jab[oó]n|syndet|gel de ba[ñn]o|gel ba[ñn]o|ba[ñn]o l[ií]quido|body wash|baby wash|\\bwash\\b",
  "Crema hidratante" = "crema|humectante|hidratante|emulsi"
)

clasificar_subcategoria <- function(x) {
  x <- str_to_lower(x)
  map_chr(x, function(s) {
    if (is.na(s)) return(NA_character_)
    pos <- map_int(SUBCATS, function(p) {
      m <- str_locate(s, p)[1, 1]
      if (is.na(m)) NA_integer_ else as.integer(m)
    })
    if (all(is.na(pos))) NA_character_ else names(SUBCATS)[which.min(pos)]
  })
}

motivo_exclusion <- function(x, publico, subcat, incluir_kids = INCLUIR_KIDS) {
  x <- str_to_lower(x)
  case_when(
    str_detect(x, "\\bkit\\b|\\bcombo\\b|canastilla|\\bset (de )?(regalo|cuidado|ba[ñn]o)|\\bregalo\\b") ~ "kit/combo",
    str_detect(x, "pa[ñn]al|toallit|pa[ñn]alitis|rash|calzoncito") ~ "cambio de pañal",
    str_detect(x, "protector solar|bloqueador|\\bfps\\b|\\bspf\\b|solar") ~ "protección solar",
    str_detect(x, "crema dental|pasta dental|dent[ií]frico|cepillo|enjuague bucal|gel dental") ~ "cuidado bucal",
    str_detect(x, "pomada|ung[uü]ento|ointment|b[aá]lsamo|vaselina|petrolato") ~ "pomada/ungüento",
    str_detect(x, "peinar|peinado|styling|fijador|mousse|desenredante|gel (para )?(el )?(cabello|pelo)") ~ "peinado/styling",
    str_detect(x, "detergente|suavizante|limpia ?pisos|lavaloza|lavavajillas|desinfectante|biber[oó]n|\\bropa\\b") ~ "no es higiene personal",
    is.na(publico) ~ "no es para bebé",
    publico == "niños" & !incluir_kids ~ "niños (excluido por decisión)",
    is.na(subcat) ~ "fuera de subcategorías de la categoría",
    TRUE ~ NA_character_
  )
}

# 4.3 Precio ---------------------------------------------------
limpiar_precio <- function(x) {
  x %>%
    str_replace_all("\u00a0", " ") %>%
    str_extract("\\$?\\s?[\\d\\.,]+") %>%
    str_remove("[,]\\d{1,2}$") %>%
    str_remove_all("[^0-9]") %>%
    as.numeric()
}

# 4.4 Contenido y pack ------------------------------------------
a_numero <- function(s) {
  s <- as.character(s)
  ifelse(str_detect(s, "^\\d{1,3}\\.\\d{3}$"),
         as.numeric(str_remove(s, "\\.")),
         as.numeric(str_replace(s, ",", ".")))
}

CONTENIDO_MAX <- 5000   # mL o g; por encima se considera lectura errónea (p. ej. un precio)

extraer_contenido <- function(texto) {
  t <- str_to_lower(texto)
  # quita fragmentos de precio ("$ 108.900", "$22.800") antes de buscar contenido
  t <- str_replace_all(t, "\\$\\s*\\d[\\d\\.,]*", " ")
  u <- "(?:ml|cc|lts?|l|litros?|gr?s?|gramos?|kg|kilos?)"
  m <- str_match(t, paste0("(?<![\\d\\.,])(\\d+(?:[\\.,]\\d+)?)\\s*(", u, ")\\b"))
  
  pre  <- str_match(t, paste0("(\\d+)\\s*x\\s*\\d+(?:[\\.,]\\d+)?\\s*", u, "\\b"))[, 2]
  post <- str_match(t, paste0(u, "\\s*x\\s*(\\d+)\\b"))[, 2]
  pack <- str_match(t, "(?:pack|paquete|set|combo)\\s*(?:de)?\\s*(\\d+)")[, 2]
  xn   <- str_match(t, paste0("\\bx\\s?(\\d+)\\b(?!\\s*", u, "\\b)"))[, 2]
  und  <- str_match(t, "\\b(\\d+)\\s*(?:unidades|unidad|unids?|unds?|uds?|piezas?)\\b")[, 2]
  
  valor <- a_numero(m[, 2]); un <- m[, 3]
  factor <- case_when(
    un %in% c("ml", "cc") ~ 1,
    str_detect(un, "^(l|lt|lts|litro|litros)$") ~ 1000,
    str_detect(un, "^(g|gr|grs|gramo|gramos)$") ~ 1,
    str_detect(un, "^(kg|kilo|kilos)$") ~ 1000,
    TRUE ~ NA_real_
  )
  unidad <- case_when(
    un %in% c("ml", "cc") | str_detect(un, "^(l|lt|lts|litro|litros)$") ~ "mL",
    str_detect(un, "^(g|gr|grs|gramo|gramos|kg|kilo|kilos)$") ~ "g",
    TRUE ~ NA_character_
  )
  unidades <- coalesce(as.numeric(pre), as.numeric(post), as.numeric(pack),
                       as.numeric(xn), as.numeric(und), 1)
  unitario <- valor * factor
  
  tibble(contenido_unitario = unitario, unidad = unidad,
         unidades_pack = unidades, contenido_total = unitario * unidades)
}

contenido_robusto <- function(nombre, texto) {
  c1 <- extraer_contenido(nombre); c2 <- extraer_contenido(texto)
  usa2 <- is.na(c1$contenido_unitario) & !is.na(c2$contenido_unitario)
  unit <- ifelse(usa2, c2$contenido_unitario, c1$contenido_unitario)
  und  <- ifelse(usa2, c2$unidad, c1$unidad)
  pack <- ifelse(c1$unidades_pack > 1, c1$unidades_pack, c2$unidades_pack)
  # contenidos imposibles (p. ej. un precio leído como gramos) quedan como NA
  unit <- ifelse(!is.na(unit) & unit > CONTENIDO_MAX, NA_real_, unit)
  tibble(contenido_unitario = unit, unidad = und,
         unidades_pack = pack, contenido_total = unit * pack)
}

# 4.5 Variante (texto distintivo del nombre; lo usa comparables.R) --------
STOP <- c("de","del","la","el","los","las","para","con","sin","y","e","en","por","un","una",
          "bebe","bebes","baby","ninos","nino","kids","infantil","shampoo","champu","jabon",
          "liquido","liquida","locion","aceite","crema","colonia","bano","gel","wash","und",
          "unds","unidad","unidades","pack","ml","gr","frasco","botella","tarro","tubo",
          "dosificador","pote","recien","nacido","nacidos")

variante_std <- function(nombre, marca) {
  x <- stringi::stri_trans_general(str_to_lower(coalesce(nombre, "")), "Latin-ASCII")
  m <- stringi::stri_trans_general(str_to_lower(coalesce(marca, "")), "Latin-ASCII")
  x <- str_remove_all(x, "\\d+(?:[\\.,]\\d+)?\\s*(?:ml|cc|lts?|l|litros?|gr?s?|gramos?|kg|kilos?)\\b")
  x <- str_remove_all(x, "\\bx\\s?\\d+\\b")
  x <- str_replace_all(x, "[^a-z ]", " ")
  map2_chr(x, m, function(s, mm) {
    tk <- str_split(str_squish(s), " ")[[1]]
    mt <- str_split(str_squish(str_replace_all(mm, "[^a-z ]", " ")), " ")[[1]]
    tk <- setdiff(unique(tk), c(STOP, mt))
    tk <- tk[nchar(tk) > 1]
    paste(sort(tk), collapse = " ")
  })
}

# 4.6 Vendedor externo y precios atípicos -------------------------
# Las fichas de Éxito/Carulla traen "Vendido por: Exito" (con dos puntos).
# Es tercero solo si ese texto existe y NO es Éxito / Carulla / Jumbo.
# Jumbo no muestra vendedor en la ficha: se considera directo (0).
detectar_tercero <- function(texto) {
  x <- stringi::stri_trans_general(str_to_lower(coalesce(texto, "")), "Latin-ASCII")
  vendido <- str_match(x, "vendido por:?\\s*([^\\n]+)")[, 2]
  as.integer(!is.na(vendido) &
               !str_detect(vendido, "^\\s*(el\\s+)?(exito|carulla|jumbo)\\b"))
}

marcar_atipicos <- function(df) {
  df %>%
    mutate(.p = ifelse(incluido == 1, precio_100, NA_real_)) %>%
    group_by(clave_relajada) %>%
    mutate(.n = sum(!is.na(.p)), .med = median(.p, na.rm = TRUE),
           precio_atipico = as.integer(!is.na(clave_relajada) & .n >= 3 & !is.na(.p) &
                                         (.p > 1.6 * .med | .p < 0.6 * .med))) %>%
    ungroup() %>% select(-.p, -.n, -.med)
}

# 4.7 Función principal de estandarización -----------------------
estandarizar <- function(df) {
  if (nrow(df) == 0) return(df)
  nombre <- coalesce(df$nombre_original, df$texto_anuncio)
  texto  <- str_squish(paste(coalesce(df$marca_sitio, ""), nombre))
  cont   <- contenido_robusto(nombre, df$texto_anuncio)
  
  out <- df %>%
    mutate(
      precio_cop         = limpiar_precio(precio_txt),
      precio_antes_cop   = limpiar_precio(precio_antes_txt),
      precio_tarjeta_cop = limpiar_precio(precio_tarjeta_txt),
      es_promocion       = as.integer(!is.na(precio_antes_cop) & !is.na(precio_cop) &
                                        precio_antes_cop > precio_cop),
      marca_std          = extraer_marca(nombre, marca_sitio),
      publico            = clasificar_publico(texto, marca_std),
      subcategoria_std   = clasificar_subcategoria(nombre),
      motivo_exclusion   = motivo_exclusion(nombre, publico, subcategoria_std),
      incluido           = as.integer(is.na(motivo_exclusion)),
      clave_producto     = coalesce(sku, url_producto, nombre_original),
      vendedor_externo   = detectar_tercero(texto_anuncio),
      variante_std       = variante_std(nombre, marca_std)
    ) %>%
    bind_cols(cont) %>%
    mutate(
      precio_100 = ifelse(!is.na(precio_cop) & !is.na(contenido_total) & contenido_total > 0,
                          precio_cop / contenido_total * 100, NA_real_),
      clave_relajada = ifelse(is.na(marca_std) | is.na(subcategoria_std) | is.na(contenido_total),
                              NA_character_,
                              paste(marca_std, subcategoria_std, round(contenido_total, 1),
                                    unidad, unidades_pack, sep = "|"))
    ) %>%
    distinct(comercio, clave_producto, ejecucion_id, .keep_all = TRUE)
  out
}

# -------------------------------------------------------------
# 5) SQLITE
# -------------------------------------------------------------
crear_bd <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ejecuciones (
      ejecucion_id TEXT NOT NULL, comercio TEXT NOT NULL,
      fecha_hora_inicio TEXT NOT NULL, n_registros INTEGER,
      estado TEXT CHECK (estado IN ('completa','parcial','fallida')), modo TEXT,
      PRIMARY KEY (ejecucion_id, comercio))")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS productos (
      producto_id TEXT PRIMARY KEY, nombre_comun TEXT, marca TEXT, subcategoria TEXT,
      contenido REAL, unidad TEXT, unidades_pack REAL)")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS observaciones (
      obs_id INTEGER PRIMARY KEY AUTOINCREMENT,
      ejecucion_id TEXT NOT NULL REFERENCES ejecuciones(ejecucion_id),
      comercio TEXT NOT NULL, clave_producto TEXT NOT NULL,
      sku TEXT, url_producto TEXT, url_busqueda TEXT, termino_busqueda TEXT,
      nombre_original TEXT, texto_anuncio TEXT,
      marca_std TEXT, subcategoria_std TEXT,
      precio_cop REAL, precio_antes_cop REAL, es_promocion INTEGER,
      contenido_unitario REAL, unidad TEXT, unidades_pack REAL, contenido_total REAL,
      incluido INTEGER, motivo_exclusion TEXT,
      producto_id TEXT REFERENCES productos(producto_id),
      comparable INTEGER DEFAULT 0,
      fecha_hora_consulta TEXT NOT NULL,
      UNIQUE (comercio, clave_producto, fecha_hora_consulta))")
}

migrar_bd <- function(con) {
  nuevas <- c(publico = "TEXT", marca_sitio = "TEXT", precio_tarjeta_cop = "REAL",
              precio_100 = "REAL", variante_std = "TEXT", clave_relajada = "TEXT",
              vendedor_externo = "INTEGER", precio_atipico = "INTEGER")
  existentes <- dbListFields(con, "observaciones")
  for (nm in setdiff(names(nuevas), existentes))
    dbExecute(con, sprintf("ALTER TABLE observaciones ADD COLUMN %s %s", nm, nuevas[[nm]]))
}

COLS_OBS <- c("ejecucion_id", "comercio", "clave_producto", "sku", "url_producto",
              "url_busqueda", "termino_busqueda", "nombre_original", "texto_anuncio",
              "marca_std", "subcategoria_std", "publico", "marca_sitio",
              "precio_cop", "precio_antes_cop", "precio_tarjeta_cop", "es_promocion",
              "contenido_unitario", "unidad", "unidades_pack", "contenido_total", "precio_100",
              "variante_std", "clave_relajada", "vendedor_externo", "precio_atipico",
              "incluido", "motivo_exclusion", "fecha_hora_consulta")

insertar_observaciones <- function(con, df) {
  if (nrow(df) == 0) return(0L)
  d <- df[, COLS_OBS]
  sql <- sprintf("INSERT OR IGNORE INTO observaciones (%s) VALUES (%s)",
                 paste(COLS_OBS, collapse = ","),
                 paste(rep("?", length(COLS_OBS)), collapse = ","))
  dbExecute(con, sql, params = unname(as.list(d)))
}

# -------------------------------------------------------------
# 6) FUNCIÓN GENERAL
# -------------------------------------------------------------
ejecutar_recoleccion <- function(comercios = names(COMERCIOS), terminos = TERMINOS,
                                 modo = c("online", "offline"),
                                 db = "comparador_precios.sqlite", dir_raw = "raw",
                                 run_id = NULL, max_paginas = 30) {
  modo <- match.arg(modo)
  if (modo == "online") {
    configurar_chromote()
    run_id <- format(Sys.time(), "%Y-%m-%dT%H%M%S", tz = "America/Bogota")
  } else if (is.null(run_id)) {
    run_id <- sort(unlist(map(comercios, ~ list.files(file.path(dir_raw, .x)))), decreasing = TRUE)[1]
    message("Modo offline, reprocesando ejecución: ", run_id)
  }
  
  con <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(con), add = TRUE)
  crear_bd(con); migrar_bd(con)
  
  funciones <- list(exito = scrape_exito, carulla = scrape_carulla, jumbo = scrape_jumbo)
  res_all <- list()
  
  for (id in comercios) {
    inicio <- ahora_iso()
    res <- funciones[[id]](terminos = terminos, run_id = run_id, modo = modo,
                           dir_raw = dir_raw, max_paginas = max_paginas)
    res_all[[id]] <- list(datos = estandarizar(res$datos), estado = res$estado, inicio = inicio)
  }
  
  todo <- bind_rows(map(res_all, "datos"))
  if (nrow(todo) > 0 && "clave_relajada" %in% names(todo)) todo <- marcar_atipicos(todo)
  
  for (id in comercios) {
    nm <- COMERCIOS[[id]]$nombre
    d  <- if (nrow(todo) > 0) filter(todo, comercio == nm) else todo
    if (modo == "offline")
      dbExecute(con, "DELETE FROM observaciones WHERE ejecucion_id = ? AND comercio = ?",
                params = list(run_id, nm))
    insertar_observaciones(con, d)
    dbExecute(con, "INSERT OR REPLACE INTO ejecuciones VALUES (?,?,?,?,?,?)",
              params = list(run_id, nm, res_all[[id]]$inicio, nrow(d), res_all[[id]]$estado, modo))
    message(sprintf("%s: %d productos únicos | %d incluidos | %d incluidos con precio (%s)",
                    nm, nrow(d),
                    if (nrow(d)) sum(d$incluido == 1, na.rm = TRUE) else 0L,
                    if (nrow(d)) sum(d$incluido == 1 & !is.na(d$precio_cop), na.rm = TRUE) else 0L,
                    res_all[[id]]$estado))
  }
  invisible(todo)
}

cargar_datos <- function(db = "comparador_precios.sqlite") {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  ult <- dbGetQuery(con, "SELECT MAX(ejecucion_id) AS m FROM ejecuciones")$m
  as_tibble(dbGetQuery(con, "SELECT * FROM observaciones WHERE ejecucion_id = ?",
                       params = list(ult)))
}

# Cobertura por comercio y término: productos obtenidos vs. total que muestra el
# sitio (si se logra leer del HTML crudo de la primera página; si no, queda NA).
cobertura_por_termino <- function(db = "comparador_precios.sqlite", dir_raw = "raw") {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  ult <- dbGetQuery(con, "SELECT MAX(ejecucion_id) AS m FROM ejecuciones")$m
  obt <- as_tibble(dbGetQuery(con,
                              "SELECT comercio, termino_busqueda AS termino, COUNT(*) AS obtenidos,
            SUM(incluido = 1) AS incluidos
     FROM observaciones WHERE ejecucion_id = ?
     GROUP BY comercio, termino_busqueda", params = list(ult)))
  
  total_sitio <- function(id, termino) {
    tryCatch({
      carpeta <- file.path(dir_raw, id, ult)
      idx <- read_csv(file.path(carpeta, "indice.csv"), show_col_types = FALSE)
      f <- idx$archivo[idx$termino == termino][1]
      html <- paste(readLines(gzfile(file.path(carpeta, f), encoding = "UTF-8"), warn = FALSE),
                    collapse = "\n")
      txt <- html_text2(read_html(html))
      n <- str_match(txt, "(\\d[\\d\\.]*)\\s+(?:resultados|productos|art[ií]culos)")[, 2]
      as.numeric(str_remove_all(n, "\\."))
    }, error = function(e) NA_real_)
  }
  ids <- setNames(names(COMERCIOS), map_chr(COMERCIOS, "nombre"))
  obt %>%
    mutate(total_sitio = map2_dbl(ids[comercio], termino, total_sitio),
           cobertura = ifelse(!is.na(total_sitio) & total_sitio > 0,
                              round(obtenidos / total_sitio, 2), NA_real_)) %>%
    arrange(comercio, desc(obtenidos))
}

# -------------------------------------------------------------
# 7) REVISIÓN DE EXCLUSIONES Y EXPORTACIÓN
# -------------------------------------------------------------
exportar_revision <- function(datos, dir = "revision") {
  dir.create(dir, showWarnings = FALSE)
  cols <- c("comercio", "marca_std", "publico", "subcategoria_std", "nombre_original",
            "precio_cop", "motivo_exclusion", "url_producto")
  datos %>% filter(motivo_exclusion == "no es para bebé") %>%
    arrange(comercio, marca_std, nombre_original) %>% select(any_of(cols)) %>%
    write_csv(file.path(dir, "excluidos_no_bebe.csv"))
  datos %>% filter(incluido == 1) %>%
    arrange(comercio, subcategoria_std, marca_std) %>% select(any_of(cols)) %>%
    write_csv(file.path(dir, "incluidos.csv"))
  resumen <- datos %>% count(comercio, motivo_exclusion, name = "n") %>%
    arrange(comercio, desc(n))
  write_csv(resumen, file.path(dir, "resumen_exclusiones.csv"))
  print(resumen, n = 50)
  invisible(resumen)
}

exportar_database_csv <- function(db = "comparador_precios.sqlite",
                                  archivo = "database_export.csv") {
  con <- dbConnect(SQLite(), db)
  on.exit(dbDisconnect(con), add = TRUE)
  datos <- as_tibble(dbGetQuery(con, "SELECT * FROM observaciones"))
  write_csv(datos, archivo)
  message("CSV guardado: ", normalizePath(archivo, mustWork = FALSE),
          " | ", nrow(datos), " observaciones")
  invisible(datos)
}


# Empareja productos de los 3 comercios aunque vengan en distinto tamaño y los
# compara por precio por 100 mL / 100 g. Niveles de confianza:
#   A. Misma presentación 
#   B. Misma línea, distinto tamaño 
#   C. Misma línea, nombre poco parecido
#   E. (opcional) sin exigir tipo de producto, solo por nombre parecido
#
# Uso:
#   source("scraper.R"); source("comparables.R")
#   res <- ejecutar_comparables()          # candidatos, tablas, registro y verificación
#   res$ancha ; res$larga ; res$final
# =============================================================

ascii <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

# Marca normalizada para comparar entre comercios ("Baby Dove" = "Dove", "Johnson S" = "Johnson's")
clave_marca <- function(x) {
  y <- ascii(str_to_lower(coalesce(x, "")))
  y <- str_remove_all(y, "[^a-z ]")
  y <- str_remove_all(y, "\\b(baby|bebe|kids)\\b")
  y <- str_remove_all(y, " ")
  ifelse(nzchar(y), y, NA_character_)
}

tokens_v <- function(s) if (is.na(s) || !nzchar(s)) character() else str_split(s, " ")[[1]]
jaccard <- function(a, b) {
  ta <- tokens_v(a); tb <- tokens_v(b); u <- union(ta, tb)
  if (!length(u)) 1 else length(intersect(ta, tb)) / length(u)
}

# -------------------------------------------------------------
# 1) EMPAREJAMIENTO
# -------------------------------------------------------------
una_pasada <- function(base, ancla, sim_min, max_ratio, exigir_subcat, pasada, min_comercios = 3) {
  A <- filter(base, com == ancla)
  if (!nrow(A)) return(tibble())
  pares <- map_dfr(seq_len(nrow(A)), function(i) {
    a <- A[i, ]
    cand <- base %>% filter(com != ancla, marca_clave == a$marca_clave, unidad == a$unidad)
    if (exigir_subcat) cand <- filter(cand, subcategoria_std == a$subcategoria_std)
    if (!nrow(cand)) return(NULL)
    cand$sim   <- map_dbl(cand$variante_std, jaccard, b = a$variante_std)
    cand$ratio <- pmax(cand$contenido_total / a$contenido_total,
                       a$contenido_total / cand$contenido_total)
    best <- cand %>% filter(sim >= sim_min, ratio <= max_ratio) %>%
      group_by(com) %>% arrange(desc(round(sim, 2)), ratio, .by_group = TRUE) %>%
      slice(1) %>% ungroup()
    if (n_distinct(best$com) + 1 < min_comercios) return(NULL)
    bind_rows(mutate(a, sim = 1, ratio = 1), best) %>%
      mutate(grupo = i, marca_g = a$marca_std, subcat_g = a$subcategoria_std, unidad_g = a$unidad)
  })
  if (nrow(pares) == 0) return(tibble())
  
  ancho <- pares %>%
    pivot_wider(id_cols = c(grupo, marca_g, subcat_g, unidad_g),
                names_from = com,
                values_from = c(precio_cop, precio_100, contenido_total, nombre_original,
                                clave_producto, variante_std, sim)) %>%
    rename(marca_std = marca_g, subcategoria_std = subcat_g, unidad = unidad_g)
  
  ccols <- grep("^contenido_total_", names(ancho), value = TRUE)
  pcols <- grep("^precio_100_",      names(ancho), value = TRUE)
  scols <- setdiff(grep("^sim_", names(ancho), value = TRUE), paste0("sim_", ancla))
  mc <- as.matrix(ancho[ccols]); mp <- as.matrix(ancho[pcols])
  ancho$n_comercios        <- rowSums(!is.na(mp))
  ancho$ratio_tam          <- apply(mc, 1, function(r) max(r, na.rm = TRUE) / min(r, na.rm = TRUE))
  ancho$misma_presentacion <- ancho$ratio_tam <= 1.03
  ancho$similitud_min      <- apply(as.matrix(ancho[scols]), 1, min, na.rm = TRUE)
  ancho$spread_precio_100  <- apply(mp, 1, function(r) max(r, na.rm = TRUE) / min(r, na.rm = TRUE))
  ancho$pasada             <- pasada
  
  ancho <- filter(ancho, n_comercios >= min_comercios) %>%
    arrange(desc(misma_presentacion), desc(similitud_min), ratio_tam)
  
  # un producto de un comercio no se reutiliza en dos filas de esta pasada
  kcols <- grep("^clave_producto_", names(ancho), value = TRUE)
  usados <- character(); keep <- logical(nrow(ancho))
  for (i in seq_len(nrow(ancho))) {
    ks <- paste(kcols, as.character(unlist(ancho[i, kcols])))
    if (!any(ks %in% usados)) { keep[i] <- TRUE; usados <- c(usados, ks) }
  }
  ancho[keep, ]
}

proponer_por_niveles <- function(datos, sim_b = 0.34, sim_c = 0.15, max_ratio_b = 4,
                                 usar_nivel_e = FALSE, solo_directos = TRUE) {
  base <- datos %>%
    filter(incluido == 1, !is.na(precio_cop), !is.na(precio_100), !is.na(marca_std),
           !is.na(contenido_total), !is.na(unidad), !is.na(subcategoria_std))
  if (solo_directos) base <- filter(base, vendedor_externo == 0)
  base <- base %>% mutate(com = ascii(comercio), marca_clave = clave_marca(marca_std)) %>%
    filter(!is.na(marca_clave))
  
  coms <- sort(unique(base$com))
  message("Comercios con datos utilizables: ", paste(coms, collapse = ", "))
  if (length(coms) < 3) return(tibble())
  ancla <- names(sort(table(base$com)))[1]
  
  config <- list(
    list(sim = sim_b, ratio = max_ratio_b, subcat = TRUE),
    list(sim = sim_c, ratio = Inf,         subcat = TRUE),
    list(sim = 0,     ratio = Inf,         subcat = TRUE))
  if (usar_nivel_e) config[[4]] <- list(sim = sim_b, ratio = max_ratio_b, subcat = FALSE)
  
  usados <- character(); res <- list()
  for (p in seq_along(config)) {
    cfg <- config[[p]]
    b <- base[!(paste(base$com, base$clave_producto) %in% usados), ]
    r <- una_pasada(b, ancla, cfg$sim, cfg$ratio, cfg$subcat, p)
    message("Pasada ", p, ": ", nrow(r), " candidatos")
    if (nrow(r) == 0) next
    for (cc in grep("^clave_producto_", names(r), value = TRUE))
      usados <- c(usados, paste(sub("^clave_producto_", "", cc), r[[cc]]))
    res[[p]] <- r
  }
  out <- bind_rows(res)
  if (!nrow(out)) return(out)
  out %>% mutate(
    nivel = case_when(
      pasada == 1 & misma_presentacion ~ "A. Misma presentación",
      pasada == 1 ~ "B. Misma línea, distinto tamaño",
      pasada == 2 ~ "C. Misma línea, nombre poco parecido (revisar)",
      pasada == 3 ~ "D. Misma marca y tipo, otra variante (familia)",
      TRUE        ~ "E. Sin exigir tipo (revisar)")) %>%
    arrange(pasada, desc(misma_presentacion), desc(similitud_min))
}

# -------------------------------------------------------------
# 2) TECHO TEÓRICO (¿cuántos comparables pueden existir?)
# -------------------------------------------------------------
techo_comparables <- function(datos) {
  b0 <- datos %>%
    filter(incluido == 1, !is.na(precio_cop), !is.na(precio_100), !is.na(marca_std),
           !is.na(contenido_total), !is.na(subcategoria_std)) %>%
    mutate(com = ascii(comercio), marca_clave = clave_marca(marca_std)) %>%
    filter(!is.na(marca_clave))
  techo <- b0 %>% count(marca_clave, subcategoria_std, com) %>%
    pivot_wider(names_from = com, values_from = n, values_fill = 0)
  for (cn in c("Exito", "Carulla", "Jumbo")) if (!cn %in% names(techo)) techo[[cn]] <- 0L
  lk <- b0 %>% group_by(marca_clave) %>% summarise(marca = first(marca_std), .groups = "drop")
  techo <- techo %>% left_join(lk, by = "marca_clave") %>%
    mutate(max_posible = pmin(Exito, Carulla, Jumbo),
           falta = case_when(Exito == 0 ~ "Éxito", Carulla == 0 ~ "Carulla",
                             Jumbo == 0 ~ "Jumbo", TRUE ~ "")) %>%
    select(marca, subcategoria_std, Exito, Carulla, Jumbo, max_posible, falta) %>%
    arrange(desc(max_posible), marca)
  list(tabla = techo, total = sum(techo$max_posible))
}

# -------------------------------------------------------------
# 3) TABLAS: ANCHA (3 artículos en una fila) y LARGA (un artículo por fila)
# -------------------------------------------------------------
armar_tablas <- function(cand) {
  tabla <- cand %>%
    transmute(
      n = row_number(), nivel, producto = str_squish(paste(marca_std, subcategoria_std)), unidad,
      misma_presentacion, ratio_tam = round(ratio_tam, 2), similitud = round(similitud_min, 2),
      Exito_nombre = nombre_original_Exito,     Exito_cont = contenido_total_Exito,
      Exito_precio = precio_cop_Exito,          Exito_x100 = round(precio_100_Exito),
      Carulla_nombre = nombre_original_Carulla, Carulla_cont = contenido_total_Carulla,
      Carulla_precio = precio_cop_Carulla,      Carulla_x100 = round(precio_100_Carulla),
      Jumbo_nombre = nombre_original_Jumbo,     Jumbo_cont = contenido_total_Jumbo,
      Jumbo_precio = precio_cop_Jumbo,          Jumbo_x100 = round(precio_100_Jumbo))
  m <- as.matrix(tabla[c("Exito_x100", "Carulla_x100", "Jumbo_x100")])
  tabla$mas_barato_x100 <- sub("_x100", "", colnames(m)[apply(m, 1, which.min)])
  tabla$ahorro_pct_x100 <- round(100 * (apply(m, 1, max) - apply(m, 1, min)) / apply(m, 1, max), 1)
  
  # TABLA ANCHA: una fila por comparable, los 3 comercios juntos
  ancha <- tabla %>%
    select(n, nivel, producto, unidad, misma_presentacion, ratio_tam, similitud,
           Exito_nombre,   Exito_cont,   Exito_precio,   Exito_x100,
           Carulla_nombre, Carulla_cont, Carulla_precio, Carulla_x100,
           Jumbo_nombre,   Jumbo_cont,   Jumbo_precio,   Jumbo_x100,
           mas_barato_x100, ahorro_pct_x100)
  
  # TABLA LARGA: una fila por artículo y comercio (con su clave_producto)
  claves <- cand %>%
    mutate(comparable_id = row_number()) %>%
    select(comparable_id, starts_with("clave_producto_")) %>%
    pivot_longer(-comparable_id, names_to = "comercio", names_prefix = "clave_producto_",
                 values_to = "clave_producto") %>%
    mutate(comercio = recode(comercio, Exito = "Éxito"))
  
  larga <- tabla %>%
    select(n, nivel, producto, unidad, misma_presentacion, matches("^(Exito|Carulla|Jumbo)_")) %>%
    pivot_longer(matches("^(Exito|Carulla|Jumbo)_"),
                 names_to = c("comercio", ".value"), names_sep = "_") %>%
    rename(comparable_id = n, nombre_original = nombre, contenido = cont,
           precio_cop = precio, precio_por_100 = x100) %>%
    mutate(comercio = recode(comercio, Exito = "Éxito")) %>%
    left_join(claves, by = c("comparable_id", "comercio")) %>%
    group_by(comparable_id) %>%
    mutate(es_el_mas_barato_x100 = precio_por_100 == min(precio_por_100, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(comparable_id, comercio)
  
  list(tabla = tabla, ancha = ancha, larga = larga)
}

# -------------------------------------------------------------
# 4) REGISTRO EN SQLITE
# -------------------------------------------------------------
# producto_id = línea de producto; el contenido de cada comercio queda en
# observaciones (contenido_total, unidad, precio_100). Reinicia los flags anteriores.
fijar_comparables_linea <- function(elegidos, db = "comparador_precios.sqlite") {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  mapa <- setNames(map_chr(COMERCIOS, "nombre"), ascii(map_chr(COMERCIOS, "nombre")))
  ccols <- grep("^clave_producto_", names(elegidos), value = TRUE)
  vcols <- grep("^variante_std_",   names(elegidos), value = TRUE)
  dbExecute(con, "UPDATE observaciones SET comparable = 0, producto_id = NULL")
  dbExecute(con, "DELETE FROM productos")
  for (i in seq_len(nrow(elegidos))) {
    e <- elegidos[i, ]
    pid <- sprintf("P%02d", i)
    variante <- na.omit(as.character(unlist(e[vcols])))[1]
    nombre_comun <- str_squish(paste(e$marca_std, e$subcategoria_std, variante))
    dbExecute(con, "INSERT OR REPLACE INTO productos VALUES (?,?,?,?,?,?,?)",
              params = list(pid, nombre_comun, e$marca_std, e$subcategoria_std,
                            NA_real_, e$unidad, NA_real_))
    for (cc in ccols) {
      k <- e[[cc]]
      if (is.na(k)) next
      comercio <- mapa[[sub("^clave_producto_", "", cc)]]
      dbExecute(con, paste("UPDATE observaciones SET producto_id = ?, comparable = 1",
                           "WHERE comercio = ? AND clave_producto = ?"),
                params = list(pid, comercio, k))
    }
  }
  message(nrow(elegidos), " productos comparables registrados.")
  invisible(dbGetQuery(con, "SELECT * FROM productos"))
}

# -------------------------------------------------------------
# 5) FUNCIÓN GENERAL
# -------------------------------------------------------------
# registrar = TRUE  -> marca como comparables las filas `idx` de los candidatos
#                      (NULL = las primeras n_obj, ya ordenadas por nivel A, B, C...)
ejecutar_comparables <- function(db = "comparador_precios.sqlite", n_obj = 12,
                                 registrar = TRUE, idx = NULL,
                                 sim_b = 0.34, sim_c = 0.15, max_ratio_b = 4,
                                 usar_nivel_e = FALSE, dir_salida = ".") {
  datos <- cargar_datos(db)
  cat("\n== Datos de la última ejecución ==\n")
  print(count(datos, ejecucion_id, comercio))
  
  cat("\n== Techo teórico de comparables (por marca y tipo) ==\n")
  tc <- techo_comparables(datos)
  print(tc$tabla, n = 60, width = Inf)
  cat("\nTECHO teórico (suma del mínimo por marca x tipo):", tc$total, "\n")
  
  cat("\n== Candidatos por niveles ==\n")
  cand <- proponer_por_niveles(datos, sim_b, sim_c, max_ratio_b, usar_nivel_e)
  if (nrow(cand) == 0) {
    message("Sin candidatos. Revise el techo: falta oferta en algún comercio. ",
            "Amplíe TERMINOS y recolecte en modo online.")
    return(invisible(list(datos = datos, techo = tc$tabla, cand = cand)))
  }
  tb <- armar_tablas(cand)
  cat("\nResumen por nivel:\n"); print(count(tb$tabla, nivel))
  for (nv in sort(unique(tb$tabla$nivel))) {
    cat("\n---", nv, "---\n")
    print(filter(tb$ancha, nivel == nv) %>% select(-nivel), n = Inf, width = Inf)
  }
  
  write_csv(tb$ancha, file.path(dir_salida, "comparables_ancha.csv"))
  write_csv(tb$larga, file.path(dir_salida, "comparables_larga.csv"))
  cat("\nGuardados: comparables_ancha.csv y comparables_larga.csv (",
      nrow(tb$ancha), "candidatos )\n")
  
  final <- NULL
  if (registrar) {
    sel <- if (is.null(idx)) cand else cand[idx, ]
    sel <- sel[seq_len(min(nrow(sel), n_obj)), ]
    if (nrow(sel) < 8) cat("\nOJO: solo hay", nrow(sel), "comparables; la guía exige 8.\n")
    cat("\n== Registrando", nrow(sel), "comparables en SQLite ==\n")
    print(fijar_comparables_linea(sel, db))
    
    con <- dbConnect(SQLite(), db)
    cat("\n== Verificación: observaciones comparables por ejecución ==\n")
    print(dbGetQuery(con, "SELECT ejecucion_id, COUNT(*) obs, COUNT(DISTINCT producto_id) productos,
                            COUNT(DISTINCT comercio) comercios
                            FROM observaciones WHERE comparable=1 AND precio_cop IS NOT NULL
                            GROUP BY ejecucion_id"))
    final <- dbGetQuery(con, "
      SELECT p.producto_id, p.nombre_comun, o.comercio, o.contenido_total, o.unidad,
             o.precio_cop, o.precio_100, o.nombre_original, o.url_producto, o.fecha_hora_consulta
      FROM observaciones o JOIN productos p ON p.producto_id = o.producto_id
      WHERE o.comparable = 1 AND o.ejecucion_id = (SELECT MAX(ejecucion_id) FROM observaciones)
      ORDER BY p.producto_id, o.comercio")
    dbDisconnect(con)
    print(final, n = Inf, width = Inf)
  }
  invisible(list(datos = datos, techo = tc$tabla, cand = cand, tabla = tb$tabla,
                 ancha = tb$ancha, larga = tb$larga, final = final))
}