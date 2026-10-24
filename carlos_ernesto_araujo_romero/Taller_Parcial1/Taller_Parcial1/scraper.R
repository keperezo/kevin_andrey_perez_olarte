library(rvest)
library(chromote)
library(dplyr)
library(stringr)
library(lubridate)
library(DBI)
library(RSQLite)

Sys.setenv(CHROMOTE_CHROME = "/usr/bin/brave-browser")

fecha_hoy <- format(Sys.Date(), "%Y-%m-%d")
dir.create(file.path("raw", "exito", fecha_hoy), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("raw", "carulla", fecha_hoy), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("raw", "jumbo", fecha_hoy), recursive = TRUE, showWarnings = FALSE)

# 1. Extracción de Éxito y Carulla (FastStore)
extraer_faststore <- function(nombre_tienda, url, slug) {
  message(sprintf("\n[Brave] Descargando catálogo de %s...", nombre_tienda))
  sesion <- tryCatch(read_html_live(url), error = function(e) NULL)
  if (is.null(sesion)) return(tibble())
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  Sys.sleep(5)
  
  # Aceptar cookies
  try({
    sesion$session$Runtime$evaluate("
      const btn = Array.from(document.querySelectorAll('button')).find(b => b.innerText.includes('Acepto'));
      if (btn) btn.click();
    ")
    Sys.sleep(2)
  }, silent = TRUE)
  
  # Scroll
  for (i in 1:4) { sesion$scroll_by(top = 1000); Sys.sleep(1.2) }
  
  # Guardar copia cruda en raw/
  ruta_raw <- file.path("raw", slug, fecha_hoy, "catalogo.html")
  html_txt <- sesion$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  writeLines(html_txt, ruta_raw)
  message("  -> Copia cruda guardada en: ", ruta_raw)
  
  tarjetas <- sesion %>% html_elements("article, [data-fs-product-card='true']")
  if (length(tarjetas) == 0) return(tibble())
  
  purrr::map_dfr(tarjetas, function(t) {
    nombre <- t %>% html_element("h3, [data-fs-product-card-title='true']") %>% html_text2()
    precio <- t %>% html_element("[data-fs-price='true'], [class*='Price']") %>% html_text2() %>%
      str_remove_all("[^0-9]") %>% as.numeric()
    url_p  <- t %>% html_element("a") %>% html_attr("href")
    
    if (is.na(nombre) || is.na(precio)) return(NULL)
    
    tibble(
      comercio       = nombre_tienda,
      sku_comercio   = str_extract(url_p, "[0-9]+(?=/p)"),
      producto       = nombre,
      precio         = precio,
      url_producto   = ifelse(str_starts(url_p, "http"), url_p, paste0("https://www.", slug, ".com", url_p))
    )
  }) %>% distinct(producto, .keep_all = TRUE)
}

# 2. Extracción de Jumbo (VTEX IO)
extraer_jumbo <- function(url) {
  message("\n[Brave] Descargando catálogo de Jumbo...")
  sesion <- tryCatch(read_html_live(url), error = function(e) NULL)
  if (is.null(sesion)) return(tibble())
  on.exit(try(sesion$session$close(), silent = TRUE), add = TRUE)
  
  Sys.sleep(5)
  for (i in 1:4) { sesion$scroll_by(top = 1000); Sys.sleep(1.2) }
  
  # Guardar copia cruda en raw/
  ruta_raw <- file.path("raw", "jumbo", fecha_hoy, "catalogo.html")
  html_txt <- sesion$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  writeLines(html_txt, ruta_raw)
  message("  -> Copia cruda guardada en: ", ruta_raw)
  
  tarjetas <- sesion %>% html_elements("a[href*='/p'], [class*='vtex-product-summary']")
  if (length(tarjetas) == 0) return(tibble())
  
  purrr::map_dfr(tarjetas, function(t) {
    txt <- html_text2(t)
    lineas <- str_split(txt, "\n")[[1]]
    lineas <- lineas[nchar(str_trim(lineas)) > 3]
    
    nombre <- lineas[1]
    nombre <- str_remove_all(nombre, "^[\\$\\s0-9\\.\\,xgrdtoadicionalPaga%]+")
    nombre <- str_trim(str_remove_all(nombre, "(Agregar.*$|[★\\*]+)"))
    
    precio_match <- str_extract(txt, "\\$[\\s]*[0-9\\.\\,]+")
    precio <- as.numeric(str_remove_all(precio_match, "[^0-9]"))
    enlace <- html_attr(t, "href")
    
    if (is.na(nombre) || is.na(precio) || precio < 500) return(NULL)
    
    tibble(
      comercio       = "Jumbo",
      sku_comercio   = str_extract(enlace, "[0-9]+(?=/p)"),
      producto       = nombre,
      precio         = precio,
      url_producto   = ifelse(str_starts(enlace, "http"), enlace, paste0("https://www.tiendasjumbo.co", enlace))
    )
  }) %>% distinct(producto, .keep_all = TRUE)
}

# 3. Ejecutar las 3 extracciones y consolidar
df_exito   <- extraer_faststore("Éxito", "https://www.exito.com/s?q=dulces%20y%20chocolates", "exito")
df_carulla <- extraer_faststore("Carulla", "https://www.carulla.com/s?q=dulces%20y%20chocolates", "carulla")
df_jumbo   <- extraer_jumbo("https://www.tiendasjumbo.co/chocolates?_q=chocolates&map=ft")

datos_crudos <- bind_rows(df_exito, df_carulla, df_jumbo)
message(sprintf("\n-> Total productos recolectados en vivo: %d", nrow(datos_crudos)))

# 4. Procesamiento, normalización de unidades y cálculo de $/g
id_ejecucion <- paste0("EJEC_", format(now(tzone = "America/Bogota"), "%Y%m%d_%H%M%S"))
fecha_ahora  <- format(now(tzone = "America/Bogota"), "%Y-%m-%dT%H:%M:%S-05:00")

t_min <- str_to_lower(datos_crudos$producto)

# Extraer contenido numérico (gramos o ml)
cont_raw <- str_extract(t_min, "[0-9]+(?:\\.[0-9]+)?\\s*(g|gr|gramos|ml)")
cont_val <- as.numeric(str_extract(cont_raw, "[0-9]+(?:\\.[0-9]+)?"))
cont_val[is.na(cont_val)] <- 100 # default razonable para confitería

datos_finales <- tibble(
  id_ejecucion      = id_ejecucion,
  comercio          = datos_crudos$comercio,
  id_producto_comun = case_when(
    str_detect(t_min, "kinder")                               ~ "CONF_01_KINDER_LECHE_100G",
    str_detect(t_min, "m&m|m y m") & str_detect(t_min, "leche") ~ "CONF_02_MYM_LECHE_48G",
    str_detect(t_min, "m&m|m y m") & str_detect(t_min, "man[ií]")~ "CONF_03_MYM_MANI_49G",
    str_detect(t_min, "chocobreak") & str_detect(t_min, "150")  ~ "CONF_04_CHOCOBREAK_150G",
    str_detect(t_min, "chocobreak") & str_detect(t_min, "250")  ~ "CONF_05_CHOCOBREAK_250G",
    str_detect(t_min, "corona") & str_detect(t_min, "tradicional")~ "CONF_06_CHOCOLATE_CORONA_TRAD",
    str_detect(t_min, "jumbo") & str_detect(t_min, "man[ií]")   ~ "CONF_07_JUMBO_MANI",
    str_detect(t_min, "nucita")                               ~ "CONF_08_NUCITA_CREMA",
    str_detect(t_min, "ferrero")                              ~ "CONF_09_FERRERO_ROCHER",
    str_detect(t_min, "jet")                                  ~ "CONF_10_JET_TRADICIONAL",
    TRUE ~ NA_character_
  ),
  es_comparable     = as.integer(!is.na(id_producto_comun)),
  sku_comercio      = coalesce(datos_crudos$sku_comercio, as.character(row_number(datos_crudos$producto))),
  texto_original    = datos_crudos$producto,
  precio_cop        = as.numeric(datos_crudos$precio),
  contenido_valor   = cont_val,
  contenido_unidad  = ifelse(str_detect(t_min, "ml"), "ml", "g"),
  precio_por_unidad = round(datos_crudos$precio / cont_val, 2),
  url_producto      = coalesce(datos_crudos$url_producto, paste0("https://www.", str_to_lower(datos_crudos$comercio), ".com")),
  fecha_consulta    = fecha_ahora
)


# 5. Inicializar e Insertar en SQLite (comparador_precios.sqlite)
con <- dbConnect(SQLite(), "comparador_precios.sqlite")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS ejecuciones (
    id_ejecucion TEXT PRIMARY KEY,
    fecha_inicio TEXT,
    fecha_fin TEXT,
    total_registros INTEGER,
    estado TEXT
  );
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS observaciones (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ejecucion TEXT,
    comercio TEXT,
    id_producto_comun TEXT,
    es_comparable INTEGER,
    sku_comercio TEXT,
    texto_original TEXT,
    precio_cop REAL,
    contenido_valor REAL,
    contenido_unidad TEXT,
    precio_por_unidad REAL,
    url_producto TEXT,
    fecha_consulta TEXT,
    UNIQUE(comercio, sku_comercio, fecha_consulta),
    FOREIGN KEY(id_ejecucion) REFERENCES ejecuciones(id_ejecucion)
  );
")

# Registrar ejecución
dbExecute(con, "
  INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, fecha_fin, total_registros, estado)
  VALUES (?, ?, ?, ?, ?);
", params = list(id_ejecucion, fecha_ahora, fecha_ahora, nrow(datos_finales), "completa"))

# Insertar observaciones
dbWriteTable(con, "observaciones", datos_finales, append = TRUE)
dbDisconnect(con)

cat("   ¡BASE SQLITE CREADA        \n")
cat("Base de datos: comparador_precios.sqlite\n")
cat(sprintf("Total productos insertados: %d\n", nrow(datos_finales)))
cat(sprintf("Productos comparables identificados: %d\n", sum(datos_finales$es_comparable == 1)))
cat("\nDesglose por comercio:\n")
print(table(datos_finales$comercio))
