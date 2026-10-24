# =============================================================================
# Temperaturas diarias (max/min, °C) - Bogota 2024-2025



setwd("C:/Users/Estudiante/Downloads/david_esteban_marquez_bayona/david_esteban_marquez_bayona/Taller01")




librerias <- c("httr", "jsonlite", "dplyr", "purrr", "readr", "tibble")
invisible(lapply(librerias, library, character.only = TRUE))

lat <- 4.7110; lon <- -74.0721  # Bogota, Colombia

# 1) Construir una URL por mes (2024-2025)
construir_url_mes <- function(anio, mes) {
  desde <- sprintf("%d-%02d-01", anio, mes)
  hasta <- as.character(seq(as.Date(desde), by = "month", length.out = 2)[2] - 1)
  sprintf(paste0(
    "https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s",
    "&start_date=%s&end_date=%s",
    "&daily=temperature_2m_max,temperature_2m_min",
    "&timezone=America/Bogota&temperature_unit=celsius"
  ), lat, lon, desde, hasta)
}

urls_meses <- purrr::pmap_chr(expand.grid(anio = 2024:2025, mes = 1:12),
                              ~ construir_url_mes(..1, ..2))

# 2) Descargar cada mes con pausa entre solicitudes, extraer fecha/high/low
#    y conservar la URL de origen
descargar_mes <- function(url) {
  Sys.sleep(1)  # pausa entre solicitudes
  resp <- httr::GET(url, httr::timeout(30))
  httr::stop_for_status(resp)
  d <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))$daily
  tibble::tibble(
    fecha      = as.Date(d$time),                       # 3) tipo Date
    high       = as.numeric(d$temperature_2m_max),       # 3) numerico
    low        = as.numeric(d$temperature_2m_min),
    url_origen = url                                     # trazabilidad
  )
}

clima_bogota <- purrr::map_dfr(urls_meses, descargar_mes)

# 4) Verificaciones: duplicados, faltantes y cobertura de fechas
stopifnot(anyDuplicated(clima_bogota$fecha) == 0)
faltantes <- sum(is.na(clima_bogota$high) | is.na(clima_bogota$low))
rango_esperado <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
dias_faltantes <- setdiff(rango_esperado, clima_bogota$fecha)
message(sprintf("Filas: %d | Valores faltantes: %d | Fechas sin cubrir: %d",
                nrow(clima_bogota), faltantes, length(dias_faltantes)))

# 5) Guardar CSV con la estructura minima pedida (fecha, high, low)
#    write_csv2() usa ";" como separador y "," como decimal (formato que
#    Excel en configuracion regional es-CO espera por defecto). Si vas a
#    abrir el archivo en un Excel en ingles, usa write_csv() en su lugar.

readr::write_csv2(dplyr::select(clima_bogota, fecha, high, low),
                  "clima_bogota_2024_2025.csv")
