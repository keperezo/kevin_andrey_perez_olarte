# =====================================================================
# Taller de Web Scraping con R — Parte 2
# Ejercicio 7.2: precios de cerveza en Éxito, Carulla y Jumbo
# ---------------------------------------------------------------------
# Marcas:        Michelob, Stella Artois, Club Colombia
# Supermercados: exito.com, carulla.com, tiendasjumbo.co
# Herramienta:   rvest::read_html_live() (Chrome vía chromote)
# =====================================================================


# ---------------------------------------------------------------------
# 0. Paquetes (mismos del taller)
# ---------------------------------------------------------------------
paquetes <- c("rvest", "xml2", "dplyr", "stringr", "purrr",
              "tibble", "readr", "chromote")

pendientes <- setdiff(paquetes, rownames(installed.packages()))
if (length(pendientes) > 0) install.packages(pendientes)

invisible(lapply(paquetes, library, character.only = TRUE))

if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior (read_html_live).")
}


# ---------------------------------------------------------------------
# 1. Parámetros del ejercicio
# ---------------------------------------------------------------------
cervezas <- tibble(
  marca        = c("Michelob", "Stella Artois", "Club Colombia"),
  termino      = c("michelob", "stella artois", "club colombia"),
  # patrón (sin tildes, en minúscula) que DEBE aparecer en la presentación
  patron_marca = c("michelob", "stella\\s*artois", "club\\s*colombia")
)

# Éxito y Carulla (Grupo Éxito) usan VTEX FastStore: búsqueda en /s?q=
# Jumbo usa VTEX IO: búsqueda en /<termino>?_q=<termino>&map=ft
# selector_tarjeta es un primer intento; si no devuelve productos se usa
# una estrategia genérica basada en los enlaces de producto de VTEX (/p).
supermercados <- tibble(
  supermercado     = c("Éxito", "Carulla", "Jumbo"),
  url_base         = c("https://www.exito.com",
                       "https://www.carulla.com",
                       "https://www.tiendasjumbo.co"),
  plataforma       = c("faststore", "faststore", "vtex_io"),
  selector_tarjeta = c("article",
                       "article",
                       "section.vtex-product-summary-2-x-container, article.vtex-product-summary-2-x-element")
)

pausa_min <- 3   # segundos de pausa entre solicitudes (aleatoria entre min y max)
pausa_max <- 6

user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Precio en pesos colombianos: "$ 2.930", "$16.500", "$ 1.234.567"
PATRON_PRECIO <- "\\$\\s?[0-9]{1,3}(?:\\.[0-9]{3})+"

# En VTEX la URL de un producto termina en /p (o /p?skuId=...)
XPATH_LINK_PRODUCTO <- ".//a[contains(@href, '/p?') or substring(@href, string-length(@href) - 1) = '/p']"


# ---------------------------------------------------------------------
# 2. Funciones auxiliares
# ---------------------------------------------------------------------

construir_url_super <- function(url_base, plataforma, termino) {
  q <- URLencode(termino)
  if (plataforma == "vtex_io") {
    paste0(url_base, "/", q, "?_q=", q, "&map=ft")
  } else {
    paste0(url_base, "/s?q=", q)
  }
}

# Estructura vacía para cuando una consulta no devuelve productos
tabla_super_vacia <- function() {
  tibble(
    presentacion    = character(),
    precio          = numeric(),
    precio_ml_sitio = numeric(),
    enlace          = character()
  )
}

# Toma una "foto" del HTML ya renderizado por JavaScript.
# Así podemos usar CSS y XPath sobre un documento estático.
capturar_html <- function(sesion) {
  html_txt <- sesion$session$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  read_html(html_txt)
}

# Una tarjeta válida tiene exactamente un producto enlazado y un precio
es_tarjeta_producto <- function(nodo) {
  hrefs <- html_elements(nodo, xpath = XPATH_LINK_PRODUCTO) %>% html_attr("href")
  n_distinct(hrefs) == 1 && str_detect(html_text2(nodo), PATRON_PRECIO)
}

# Localiza las tarjetas de producto:
#  1) con el selector CSS propio del sitio;
#  2) si falla, sube desde cada enlace /p hasta el ancestro más cercano
#     que contenga un precio.
extraer_tarjetas <- function(doc, selector_tarjeta) {
  tarjetas <- if (!is.na(selector_tarjeta)) html_elements(doc, selector_tarjeta) else list()
  if (length(tarjetas) > 0) {
    tarjetas <- tarjetas[map_lgl(tarjetas, es_tarjeta_producto)]
  }
  if (length(tarjetas) == 0) {
    xpath_generico <- paste0(sub("^\\.", "", XPATH_LINK_PRODUCTO),
                             "/ancestor::*[contains(., '$')][1]")
    tarjetas <- html_elements(doc, xpath = xpath_generico)
    if (length(tarjetas) > 0) {
      tarjetas <- tarjetas[map_lgl(tarjetas, es_tarjeta_producto)]
    }
  }
  tarjetas
}

# Convierte "2.930" o "10,89" a número
a_numero_cop <- function(x) {
  x <- str_remove_all(x, "[$\\s]")
  x <- str_remove_all(x, "\\.(?=[0-9]{3}(\\D|$))")  # separador de miles
  suppressWarnings(as.numeric(str_replace(x, ",", ".")))
}

# Extrae presentación, precio y enlace de UNA tarjeta
extraer_producto_super <- function(tarjeta) {
  
  # --- Presentación: candidatos de texto dentro de la tarjeta ---
  candidatos <- c(
    tarjeta %>%
      html_elements(xpath = paste(
        ".//h1 | .//h2 | .//h3",
        "| .//*[contains(@class, 'productName') or contains(@class, 'productBrand')",
        "or contains(@class, 'name') or contains(@class, 'Name') or contains(@class, 'title')]",
        "|", XPATH_LINK_PRODUCTO
      )) %>%
      html_text2(),
    tarjeta %>% html_elements("img") %>% html_attr("alt")
  ) %>%
    str_squish()
  
  candidatos <- candidatos[!is.na(candidatos) & candidatos != "" &
                             !str_detect(candidatos, "\\$")]
  # Preferimos textos que parezcan nombre de cerveza (con volumen o la palabra cerveza)
  con_volumen <- candidatos[str_detect(str_to_lower(candidatos), "cerveza|[0-9]\\s*(ml|cc)")]
  if (length(con_volumen) > 0) candidatos <- con_volumen
  presentacion <- if (length(candidatos) == 0) NA_character_ else
    candidatos[which.max(nchar(candidatos))]
  
  # --- Precio: elementos "hoja" cuyo texto es solo un precio ---
  hojas <- tarjeta %>%
    html_elements(xpath = ".//*[not(*)]") %>%
    html_text2() %>%
    str_squish()
  precios_txt <- hojas[str_detect(hojas, paste0("^", PATRON_PRECIO, "$"))]
  if (length(precios_txt) == 0) {
    precios_txt <- str_extract_all(html_text2(tarjeta), PATRON_PRECIO)[[1]]
  }
  precios <- a_numero_cop(precios_txt)
  # Si hay precio antes/después de descuento, el menor es el precio vigente
  precio <- if (length(precios) == 0) NA_real_ else min(precios, na.rm = TRUE)
  
  # Éxito y Carulla muestran "(Ml a $ 10,89)": lo guardamos para validar
  precio_ml_sitio <- html_text2(tarjeta) %>%
    str_extract("(?i)ml\\s+a\\s+\\$\\s?[0-9.,]+") %>%
    str_extract("[0-9.,]+$") %>%
    a_numero_cop()
  
  enlace <- tarjeta %>%
    html_element(xpath = XPATH_LINK_PRODUCTO) %>%
    html_attr("href")
  
  tibble(
    presentacion    = presentacion,
    precio          = precio,
    precio_ml_sitio = precio_ml_sitio,
    enlace          = enlace
  )
}

# Espera a que JavaScript reemplace los skeletons por productos
esperar_productos <- function(sesion, selector_tarjeta,
                              max_intentos = 12, espera = 1.5) {
  for (i in seq_len(max_intentos)) {
    Sys.sleep(espera)
    doc <- capturar_html(sesion)
    if (length(extraer_tarjetas(doc, selector_tarjeta)) > 0) return(TRUE)
  }
  FALSE
}

# Scrapea UN supermercado para UN término de búsqueda
scrapear_super <- function(supermercado, url_base, plataforma, selector_tarjeta,
                           termino, marca) {
  url <- construir_url_super(url_base, plataforma, termino)
  message("→ ", supermercado, " | ", termino, " | ", url)
  
  sesion <- read_html_live(url)
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  # user-agent de navegador normal (Chrome headless se anuncia como "HeadlessChrome")
  try({
    sesion$session$Network$setUserAgentOverride(userAgent = user_agent_navegador)
    sesion$session$Page$reload()
  }, silent = TRUE)
  
  hay_productos <- esperar_productos(sesion, selector_tarjeta)
  
  # Scroll para activar productos que cargan de forma perezosa
  if (hay_productos) {
    for (i in 1:4) {
      sesion$scroll_by(top = 1500)
      Sys.sleep(1.5)
    }
  }
  
  doc      <- capturar_html(sesion)
  titulo   <- html_text2(html_element(doc, "title"))
  tarjetas <- extraer_tarjetas(doc, selector_tarjeta)
  
  datos <- if (length(tarjetas) == 0) {
    tabla_super_vacia()
  } else {
    map_dfr(tarjetas, extraer_producto_super)
  }
  
  list(
    datos = datos %>%
      mutate(supermercado   = supermercado,
             marca_buscada  = marca,
             termino        = termino,
             url_busqueda   = url,
             enlace         = xml2::url_absolute(enlace, url_base),
             fecha_consulta = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             .before = 1),
    estado = if (nrow(datos) == 0) paste0("sin productos (título: ", titulo, ")") else "ok"
  )
}

# Versión segura: un error en un sitio NO detiene el resto del proceso
scrapear_super_seguro <- function(...) {
  args <- list(...)
  tryCatch(
    scrapear_super(...),
    error = function(e) {
      message("   ✗ Falló ", args$supermercado, " (", args$termino, "): ",
              conditionMessage(e))
      list(
        datos = tabla_super_vacia() %>%
          mutate(supermercado = character(), marca_buscada = character(),
                 termino = character(), url_busqueda = character(),
                 fecha_consulta = character(), .before = 1),
        estado = paste("error:", conditionMessage(e))
      )
    }
  )
}


# ---------------------------------------------------------------------
# 3. Scraping: 3 supermercados × 3 cervezas = 9 consultas
# ---------------------------------------------------------------------
chrome_disponible <- tryCatch({ chromote::chromote_info(); TRUE },
                              error = function(e) FALSE)
if (!chrome_disponible) stop("Chrome no fue detectado.")

consultas <- cross_join(supermercados, cervezas)

salidas <- pmap(consultas, function(supermercado, url_base, plataforma,
                                    selector_tarjeta, marca, termino, patron_marca) {
  res <- scrapear_super_seguro(
    supermercado = supermercado, url_base = url_base, plataforma = plataforma,
    selector_tarjeta = selector_tarjeta, termino = termino, marca = marca
  )
  Sys.sleep(runif(1, pausa_min, pausa_max))   # pausa entre solicitudes
  res
})

bitacora <- consultas %>%
  select(supermercado, marca, termino) %>%
  mutate(
    estado      = map_chr(salidas, "estado"),
    n_productos = map_int(salidas, ~ nrow(.x$datos))
  )
print(bitacora)

cervezas_crudo <- map_dfr(salidas, "datos")

dir.create("resultados", showWarnings = FALSE)
write_csv(bitacora,       file.path("resultados", "bitacora_scraping.csv"))
# Tabla consolidada pedida: presentación, precio y supermercado (+ contexto)
# (write_excel_csv() en vez de write_csv() si va a abrirlo en Excel con tildes)
write_csv(cervezas_crudo, file.path("resultados", "cervezas_supermercados.csv"))


# ---------------------------------------------------------------------
# 4. Limpieza: volumen en ml, unidades y filtro de productos irrelevantes
# ---------------------------------------------------------------------
# Si ya scrapeó antes, puede empezar desde aquí:
# cervezas_crudo <- read_csv("resultados/cervezas_supermercados.csv")

normalizar <- function(x) {
  x %>% str_to_lower() %>% stringi::stri_trans_general("Latin-ASCII") %>% str_squish()
}

palabras_excluidas <- paste0(
  "llanta|\\brin\\b|neumatic|michelin|vaso|copa|combo|kit|ancheta|",
  "camiseta|gorra|destapador|nevera|hielera|cooler|papas|pasaboca|",
  "chocolate|bombon|galleta|sal\\b|salsa"
)

cervezas_limpias_todo <- cervezas_crudo %>%
  left_join(cervezas %>% select(marca, patron_marca),
            by = c("marca_buscada" = "marca")) %>%
  mutate(
    presentacion = str_squish(presentacion),
    p            = normalizar(presentacion),
    
    # Volumen: primero lo que esté entre paréntesis "(2130 ml)", luego cualquier "269ml"/"330 cc"
    vol_paren = str_match(p, "\\(\\s*([0-9]+(?:[.,][0-9]+)?)\\s*(?:ml|cc)\\s*\\)")[, 2],
    vol_ml    = str_match(p, "([0-9]+(?:[.,][0-9]+)?)\\s*(?:ml|cc|mililitros?)\\b")[, 2],
    vol_l     = str_match(p, "([0-9]+(?:[.,][0-9]+)?)\\s*(?:l|lt|lts|litros?)\\b")[, 2],
    volumen   = coalesce(a_numero_cop(vol_paren),
                         a_numero_cop(vol_ml),
                         a_numero_cop(vol_l) * 1000),
    
    # Unidades: "x6und", "x 12 unidades", "six pack", "4 pack", "x6"
    unidades = coalesce(
      as.numeric(str_match(p, "([0-9]+)\\s*(?:und|unds|unid|unidades|u)\\b")[, 2]),
      if_else(str_detect(p, "six\\s*-?\\s*pack|sixpack"), 6, NA_real_),
      as.numeric(str_match(p, "([0-9]+)\\s*-?\\s*pack\\b")[, 2]),
      as.numeric(str_match(p, "\\bpack\\s*x?\\s*([0-9]+)\\b")[, 2]),
      as.numeric(str_match(p, "\\bx\\s*([0-9]+)\\b(?!\\s*(?:ml|cc|l\\b|lt|g\\b|gr))")[, 2]),
      1
    ),
    
    # ¿El volumen es por unidad o total del paquete?
    # Éxito escribe el total "(2130 ml)"; Jumbo el unitario "x269ml c-u".
    # Si el número es menor que 150 ml × unidades no puede ser el total.
    ml_total  = if_else(unidades > 1 & volumen < 150 * unidades,
                        volumen * unidades, volumen),
    ml_unidad = ml_total / unidades,
    envase    = case_when(
      str_detect(p, "\\blata") ~ "lata",
      str_detect(p, "botella|\\bbot\\b|\\bnrb\\b|\\brb\\b") ~ "botella",
      str_detect(p, "barril") ~ "barril",
      TRUE ~ "sin dato"
    ),
    precio_ml = precio / ml_total,
    
    # Filtro de relevancia (pregunta 5)
    motivo_descarte = case_when(
      is.na(presentacion)                    ~ "sin nombre",
      is.na(precio)                          ~ "sin precio",
      !str_detect(p, patron_marca)           ~ "no es la marca buscada",
      str_detect(p, palabras_excluidas)      ~ "no es cerveza / combo",
      is.na(ml_total)                        ~ "sin volumen en ml",
      TRUE                                   ~ NA_character_
    )
  ) %>%
  rename(marca = marca_buscada) %>%
  distinct(supermercado, marca, presentacion, precio, .keep_all = TRUE)

descartados <- cervezas_limpias_todo %>% filter(!is.na(motivo_descarte))

cervezas_limpias <- cervezas_limpias_todo %>%
  filter(is.na(motivo_descarte)) %>%
  select(supermercado, marca, presentacion, envase, unidades, ml_unidad,
         ml_total, precio, precio_ml, precio_ml_sitio, enlace, fecha_consulta)

write_csv(cervezas_limpias, file.path("resultados", "cervezas_limpias.csv"))
write_csv(descartados %>% select(supermercado, marca, presentacion, precio, motivo_descarte),
          file.path("resultados", "cervezas_descartadas.csv"))

# Validación: nuestro precio/ml vs. el que publica el sitio (Éxito/Carulla)
validacion_ml <- cervezas_limpias %>%
  filter(!is.na(precio_ml_sitio)) %>%
  mutate(dif_relativa = abs(precio_ml - precio_ml_sitio) / precio_ml_sitio) %>%
  arrange(desc(dif_relativa))
print(validacion_ml %>% select(supermercado, presentacion, precio_ml, precio_ml_sitio, dif_relativa))


# ---------------------------------------------------------------------
# 5. Preguntas
# ---------------------------------------------------------------------
pesos <- function(x, dec = 0) {
  paste0("$", format(round(x, dec), big.mark = ".", decimal.mark = ",", nsmall = dec))
}

# --- P1. Precio total, sin tener en cuenta la presentación -------------
p1 <- cervezas_limpias %>%
  group_by(marca, supermercado) %>%
  summarise(n_productos     = n(),
            precio_min      = min(precio),
            precio_promedio = mean(precio),
            .groups = "drop") %>%
  arrange(marca, precio_min)
print(p1)

p1_ganador <- p1 %>% group_by(marca) %>% slice_min(precio_min, n = 1, with_ties = TRUE)

# --- P2. Precio promedio por mililitro -------------------------------
p2 <- cervezas_limpias %>%
  group_by(supermercado) %>%
  summarise(n_productos        = n(),
            precio_ml_promedio = mean(precio_ml),
            precio_ml_mediana  = median(precio_ml),
            .groups = "drop") %>%
  arrange(precio_ml_promedio)
print(p2)

# Promedio "balanceado": cada marca pesa lo mismo, así un supermercado
# con muchas referencias de una sola marca no sesga el promedio
p2_balanceado <- cervezas_limpias %>%
  group_by(supermercado, marca) %>%
  summarise(precio_ml_marca = mean(precio_ml), .groups = "drop") %>%
  group_by(supermercado) %>%
  summarise(n_marcas = n(),
            precio_ml_promedio_balanceado = mean(precio_ml_marca),
            .groups = "drop") %>%
  arrange(desc(n_marcas), precio_ml_promedio_balanceado)
print(p2_balanceado)

# --- P3. ¿Cambia el ganador según la marca? ---------------------------
p3 <- cervezas_limpias %>%
  group_by(marca, supermercado) %>%
  summarise(precio_ml_promedio = mean(precio_ml), .groups = "drop") %>%
  group_by(marca) %>%
  mutate(puesto = min_rank(precio_ml_promedio)) %>%
  arrange(marca, puesto) %>%
  ungroup()
print(p3)

p3_ganador <- p3 %>% filter(puesto == 1)
consistente <- n_distinct(p3_ganador$supermercado) == 1 && nrow(p3_ganador) == 3

# --- P4. Presentaciones equivalentes ---------------------------------
# Unidad individual, mismo envase y mismo volumen, presente en el mayor
# número de supermercados; de esas, la más pequeña de cada marca.
individuales <- cervezas_limpias %>% filter(unidades == 1)

presentacion_comun <- individuales %>%
  group_by(marca, envase, ml_unidad) %>%
  summarise(n_super = n_distinct(supermercado), .groups = "drop") %>%
  group_by(marca) %>%
  filter(n_super == max(n_super)) %>%
  slice_min(ml_unidad, n = 1, with_ties = FALSE) %>%
  ungroup()
print(presentacion_comun)

p4 <- individuales %>%
  semi_join(presentacion_comun, by = c("marca", "envase", "ml_unidad")) %>%
  group_by(marca, envase, ml_unidad, supermercado) %>%
  summarise(precio = min(precio), .groups = "drop") %>%
  group_by(marca) %>%
  mutate(mas_barato = precio == min(precio)) %>%
  arrange(marca, precio) %>%
  ungroup()
print(p4)

p4_ganador <- p4 %>% filter(mas_barato)


# ---------------------------------------------------------------------
# 6. Respuestas redactadas a partir de los datos
# ---------------------------------------------------------------------
cat("\n================ RESPUESTAS ================\n")

cat("\nP1. Más barata por precio total (producto más económico disponible):\n")
p1_ganador %>%
  group_by(marca) %>%
  summarise(txt = paste0("  - ", first(marca), ": ", paste(supermercado, collapse = " / "),
                         " (", pesos(first(precio_min)), ")"), .groups = "drop") %>%
  pull(txt) %>% cat(sep = "\n")

cat("\nP2. Precio promedio por ml por supermercado:\n")
cat(paste0("  - ", p2$supermercado, ": ", pesos(p2$precio_ml_promedio, 2), " por ml (",
           p2$n_productos, " productos)"), sep = "\n")
cat("  → Más conveniente en general:", p2$supermercado[1], "\n")
cat("  → Con promedio balanceado por marca:", p2_balanceado$supermercado[1], "\n")

cat("\nP3. Supermercado más barato por ml en cada marca:\n")
cat(paste0("  - ", p3_ganador$marca, ": ", p3_ganador$supermercado), sep = "\n")
cat(if (consistente) {
  paste0("  → ", p3_ganador$supermercado[1], " es consistentemente el más económico.\n")
} else {
  "  → El ganador cambia según la marca.\n"
})

cat("\nP4. Presentación equivalente (unidad individual más pequeña común):\n")
p4_ganador %>%
  mutate(txt = paste0("  - ", marca, " ", envase, " ", ml_unidad, " ml: ",
                      supermercado, " (", pesos(precio), ")")) %>%
  pull(txt) %>% cat(sep = "\n")

cat("\nP5. Registros descartados antes de calcular promedios:\n")
print(count(descartados, supermercado, motivo_descarte))