library(tidyverse)   
library(rvest)       
library(httr)        

location_key <- "107487"

idioma <- "es"
unidad <- "c"

anios <- c(2024, 2025)

nombres_mes <- c(
  "january", "february", "march", "april",
  "may", "june", "july", "august",
  "september", "october", "november", "december"
)

pausa_segundos <- 2

archivo_csv <- "bogota_temperaturas_2024_2025.csv"

construir_url_mes <- function(anio, mes_en) {
  paste0(
    "https://www.accuweather.com/",
    idioma, "/co/bogota/", location_key, "/",
    mes_en, "-weather/", location_key,
    "?year=", anio,
    "&unit=", unidad
  )
}

calendario_urls <- tidyr::expand_grid(
  anio    = anios,
  mes_num = 1:12
) %>%
  mutate(
    mes_en     = nombres_mes[mes_num],
    url_origen = construir_url_mes(anio, mes_en)
  )

print(calendario_urls)

extraer_mes <- function(url, anio, mes_num) {
  
  message("Leyendo: ", url)
  
  respuesta <- httr::GET(
    url,
    httr::user_agent(
      "TallerMineriaDatos-Ejercicio13/1.0 (uso academico; rvest)"
    ),
    httr::timeout(30)
  )
  
  codigo <- httr::status_code(respuesta)
  if (codigo != 200) {
    warning("HTTP ", codigo, " en ", url)
    return(tibble(
      fecha      = as.Date(character()),
      high       = numeric(),
      low        = numeric(),
      url_origen = character()
    ))
  }
  
  pagina <- httr::content(respuesta, as = "parsed", encoding = "UTF-8")
  
  nodos <- pagina %>% html_elements("a.monthly-daypanel")
  
  if (length(nodos) == 0) {
    nodos <- pagina %>% html_elements(".monthly-calendar a")
  }
  
  if (length(nodos) == 0) {
    warning(
      "No encontré el calendario en el HTML de: ", url
    )
    return(tibble(
      fecha      = as.Date(character()),
      high       = numeric(),
      low        = numeric(),
      url_origen = character()
    ))
  }
  
  filas <- list()
  
  for (i in seq_along(nodos)) {
    nodo <- nodos[[i]]
    
    dia_texto <- nodo %>%
      html_element(".date") %>%
      html_text2()
    
    high_texto <- nodo %>%
      html_element(".high") %>%
      html_text2()
    
    low_texto <- nodo %>%
      html_element(".low") %>%
      html_text2()
    
    if (any(is.na(c(dia_texto, high_texto, low_texto)))) {
      next
    }
    
    dia_num <- as.integer(str_extract(dia_texto, "[0-9]+"))
    
    if (is.na(dia_num)) {
      next
    }
    
    fecha_candidata <- suppressWarnings(
      as.Date(sprintf("%04d-%02d-%02d", anio, mes_num, dia_num))
    )
    
    if (is.na(fecha_candidata)) {
      next
    }
    
    if (as.integer(format(fecha_candidata, "%m")) != mes_num) {
      next
    }
    
    filas[[length(filas) + 1]] <- tibble(
      fecha      = fecha_candidata,
      high       = as.numeric(str_replace_all(high_texto, "[^0-9.-]", "")),
      low        = as.numeric(str_replace_all(low_texto,  "[^0-9.-]", "")),
      url_origen = url
    )
  }
  
  if (length(filas) == 0) {
    warning("La página respondió, pero no pude armar filas: ", url)
    return(tibble(
      fecha      = as.Date(character()),
      high       = numeric(),
      low        = numeric(),
      url_origen = character()
    ))
  }
  
  bind_rows(filas) %>%
    distinct(fecha, .keep_all = TRUE) %>%
    arrange(fecha)
}

extraer_mes_seguro <- purrr::safely(extraer_mes)

resultados <- list()
errores    <- list()

for (i in seq_len(nrow(calendario_urls))) {
  
  fila <- calendario_urls[i, ]
  Sys.sleep(pausa_segundos)
  
  salida <- extraer_mes_seguro(
    url     = fila$url_origen,
    anio    = fila$anio,
    mes_num = fila$mes_num
  )
  
  if (!is.null(salida$error)) {
    errores[[i]] <- tibble(
      url   = fila$url_origen,
      error = conditionMessage(salida$error)
    )
  } else {
    resultados[[i]] <- salida$result
  }
}

bogota <- resultados %>%
  compact() %>%
  bind_rows() %>%
  arrange(fecha) %>%
  select(fecha, high, low, url_origen)

errores_tbl <- errores %>%
  compact() %>%
  bind_rows()


fechas_esperadas <- tibble(
  fecha = seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
)

validacion <- tibble(
  filas              = nrow(bogota),
  fechas_unicas      = n_distinct(bogota$fecha),
  duplicados_fecha   = sum(duplicated(bogota$fecha)),
  faltantes_high     = sum(is.na(bogota$high)),
  faltantes_low      = sum(is.na(bogota$low)),
  fecha_min          = if (nrow(bogota) == 0) as.Date(NA) else min(bogota$fecha, na.rm = TRUE),
  fecha_max          = if (nrow(bogota) == 0) as.Date(NA) else max(bogota$fecha, na.rm = TRUE),
  dias_esperados     = nrow(fechas_esperadas),
  dias_faltantes     = nrow(anti_join(fechas_esperadas, bogota, by = "fecha")),
  paginas_con_error  = nrow(errores_tbl)
)

print(validacion)

if (nrow(errores_tbl) > 0) {
  message("Páginas que fallaron:")
  print(errores_tbl)
}

dias_huecos <- anti_join(fechas_esperadas, bogota, by = "fecha")
if (nrow(dias_huecos) > 0 && nrow(dias_huecos) <= 40) {
  message("Fechas sin dato:")
  print(dias_huecos)
}

print(head(bogota, 10))

