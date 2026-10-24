paquetes <- c("httr2", "rvest", "dplyr", "stringr", "purrr", "readr", "lubridate", "janitor")
invisible(lapply(paquetes, library, character.only = TRUE))

USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"

session_base <- request("https://www.accuweather.com") %>%
  req_headers(
    `User-Agent`      = USER_AGENT,
    `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
    `Connection`      = "keep-alive"
  ) %>%
  req_retry(max_tries = 2) %>%
  req_throttle(rate = 15 / 60) 

tibble_vacio <- tibble(
  fecha      = as.Date(character()),
  high       = numeric(),
  low        = numeric(),
  url_origen = character()
)

generar_urls_accuweather <- function(anios = 2024:2025) {
  base_url <- "https://www.accuweather.com/es/co/bogota/107487"
  meses_slugs <- c(
    "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "december"
  )
  
  expand.grid(anio = anios, mes_num = 1:12, stringsAsFactors = FALSE) %>%
    arrange(anio, mes_num) %>%
    mutate(
      mes_slug = meses_slugs[mes_num],
      url_origen = paste0(base_url, "/", mes_slug, "-weather/107487?year=", anio, "&month=", mes_num)
    )
}


# función de extracción de un mes
extraer_mes_accuweather <- function(url) {
  # Pausa ética entre peticiones
  Sys.sleep(runif(1, min = 2, max = 4))
  
  # petición HTTP
  resp <- session_base %>%
    req_url(url) %>%
    req_perform()
  
  if (resp_status(resp) != 200) {
    stop(paste("Respuesta HTTP no exitosa:", resp_status(resp)))
  }
  
  html_raw <- resp_body_string(resp)
  
  # control de retos Cloudflare / Botwall
  if (str_detect(html_raw, regex("just a moment|cloudflare|attention required|checking your browser", ignore_case = TRUE))) {
    stop("Bloqueo Cloudflare / Reto de verificación detectado.")
  }
  
  doc <- read_html(html_raw)
  
  # selectores reales del calendario mensual de AccuWeather
  nodos_dias <- doc %>% html_elements("a.monthly-daypanel")
  
  if (length(nodos_dias) == 0) {
    stop("No se encontraron elementos 'a.monthly-daypanel'. El DOM cambió o requiere renderizado JS.")
  }
  
  # obtener mes y año desde los parámetros de la URL
  anio_url <- as.integer(str_extract(url, "(?<=year=)[0-9]{4}"))
  mes_url  <- as.integer(str_extract(url, "(?<=month=)[0-9]+"))
  
  # parseo de cada día
  registros <- map_dfr(nodos_dias, function(nodo) {
    
    dia_texto <- nodo %>% html_element(".date") %>% html_text2()
    dia_num   <- as.integer(str_extract(dia_texto, "[0-9]+"))
    
    
    high_texto <- nodo %>% html_element(".high") %>% html_text2()
    high_num   <- as.numeric(str_extract(high_texto, "-?[0-9]+"))
    
    
    low_texto  <- nodo %>% html_element(".low") %>% html_text2()
    low_num    <- as.numeric(str_extract(low_texto, "-?[0-9]+"))
    
    if (is.na(high_num) || is.na(low_num) || is.na(dia_num)) {
      return(NULL)
    }
    
    # construcción de fecha
    fecha_construida <- make_date(anio_url, mes_url, dia_num)
    
    tibble(
      fecha      = fecha_construida,
      high       = high_num,
      low        = low_num,
      url_origen = url
    )
  })
  
  if (nrow(registros) == 0) {
    return(tibble_vacio)
  }
  
  return(registros)
}

extraer_seguro <- purrr::safely(extraer_mes_accuweather, otherwise = tibble_vacio)

urls_df <- generar_urls_accuweather(anios = 2024:2025)
message("Iniciando scraping de ", nrow(urls_df), " meses de datos históricos...")

resultados <- list()
errores    <- list()

for (i in seq_len(nrow(urls_df))) {
  url_i <- urls_df$url_origen[i]
  message(sprintf("[%02d/%02d] Consultando: %s %d", i, nrow(urls_df), urls_df$mes_slug[i], urls_df$anio[i]))
  
  res <- extraer_seguro(url_i)
  
  resultados[[i]] <- res$result
  
  if (!is.null(res$error)) {
    errores[[i]] <- tibble(url = url_i, mensaje = res$error$message)
    message("   -> Advertencia/Error: ", res$error$message)
  }
}

# consolidar extracciones
df_crudo <- bind_rows(resultados)
df_errores <- bind_rows(errores)

cat("                ESTADO DE LA RECOLECCIÓN               \n")

if (nrow(df_crudo) == 0) {
  cat("RESULTADO: No se pudieron extraer datos vía HTML estático (rvest/httr2).\n\n")
  cat("DIAGNÓSTICO TÉCNICO (Requerido para el ejercicio de clase):\n")
  cat("1. AccuWeather utiliza protección de capa perimetral (Cloudflare) y/o\n")
  cat("   renderizado del lado del cliente (Client-Side Rendering mediante React/Next.js).\n")
  cat("2. Las peticiones estáticas reciben un bloqueo 403 o una plantilla esqueleto vacía.\n")
  cat("3. Conforme a las instrucciones del taller, se documenta la limitación:\n")
  cat("   Para scrapear este sitio en producción se requiere automatización de navegador\n")
  cat("   real mediante paquetes como 'chromote', 'selenider' o el consumo de una API oficial.\n\n")
  cat("Detalle de los errores capturados:\n")
  print(head(df_errores, 5))
  
} else {
  df_limpio <- df_crudo %>%
    filter(!is.na(fecha)) %>%
    filter(fecha >= as.Date("2024-01-01") & fecha <= as.Date("2025-12-31")) %>%
    distinct(fecha, .keep_all = TRUE) %>%
    arrange(fecha) %>%
    select(fecha, high, low, url_origen)
  
  # resumen de validación
  cat("Conteo total de filas extraídas:", nrow(df_limpio), "\n")
  cat("Filas duplicadas por fecha:    ", sum(duplicated(df_limpio$fecha)), "\n")
  cat("Faltantes (NA) en 'high':       ", sum(is.na(df_limpio$high)), "\n")
  cat("Faltantes (NA) en 'low':        ", sum(is.na(df_limpio$low)), "\n")
  cat("Rango de fechas cubierto:       ", as.character(min(df_limpio$fecha, na.rm = TRUE)), 
      " a ", as.character(max(df_limpio$fecha, na.rm = TRUE)), "\n")
  
  # guardar archivo CSV
  archivo_salida <- "temperaturas_bogota_2024_2025.csv"
  write_csv(df_limpio, archivo_salida)
  cat("\nArchivo guardado con éxito:", archivo_salida, "\n")
}