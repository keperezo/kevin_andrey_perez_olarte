# INSTALACIÓN Y CARGA DE PAQUETES
install.packages("pacman")
library(pacman)

p_load(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr",
  "knitr", "httr2", "lubridate"
)


# MESES Y AÑOS A CONSULTAR
meses <- c(
  "january", "february", "march", "april",
  "may", "june", "july", "august",
  "september", "october", "november", "december"
)

anios <- c(2024, 2025)


# CREACIÓN DE LAS 24 URL
urls <- expand.grid(
  anio = anios,
  mes = 1:12
) %>%
  arrange(anio, mes) %>%
  mutate(
    nombre_mes = meses[mes],
    url = paste0(
      "https://www.accuweather.com/es/co/bogota/107487/",
      nombre_mes,
      "-weather/107487?year=",
      anio
    )
  )


# FUNCIÓN PARA EXTRAER LOS DATOS DE UN MES
extraer_mes <- function(url, anio, mes) {
  
  cat("Procesando:", anio, "-", meses[mes], "\n")
  
  pagina <- read_html_live(url)
  
  dias <- pagina %>%
    html_elements("a.monthly-daypanel")
  
  if (length(dias) == 0) {
    warning("No se encontraron días en: ", url)
    return(tibble())
  }
  
  datos <- tibble(
    posicion = seq_along(dias),
    
    dia = map_chr(dias, ~ {
      .x %>%
        html_element(".date") %>%
        html_text2()
    }),
    
    high = map_chr(dias, ~ {
      .x %>%
        html_element(".high") %>%
        html_text2()
    }),
    
    low = map_chr(dias, ~ {
      .x %>%
        html_element(".low") %>%
        html_text2()
    })
  ) %>%
    mutate(
      dia = as.integer(str_extract(dia, "\\d+")),
      high = as.numeric(str_extract(high, "-?\\d+")),
      low = as.numeric(str_extract(low, "-?\\d+"))
    )
  
  
  # PRIMER DÍA DEL MES
  primer_dia <- as.Date(
    sprintf("%d-%02d-01", anio, mes)
  )
  
  
  # NÚMERO DE DÍAS DEL MES
  siguiente_mes <- seq(
    primer_dia,
    length = 2,
    by = "month"
  )[2]
  
  dias_mes <- as.integer(
    format(siguiente_mes - 1, "%d")
  )
  
  
  # POSICIÓN DEL DÍA 1
  posicion_inicio <- which(datos$dia == 1)[1]
  
  
  # SELECCIÓN DE LOS DÍAS DEL MES
  datos %>%
    filter(
      posicion >= posicion_inicio,
      posicion < posicion_inicio + dias_mes
    ) %>%
    mutate(
      fecha = as.Date(
        sprintf("%d-%02d-%02d", anio, mes, dia)
      ),
      url = url
    ) %>%
    select(fecha, high, low, url)
}


# EXTRACCIÓN DE LOS 24 MESES
datos_2024_2025 <- map_dfr(
  seq_len(nrow(urls)),
  function(i) {
    
    tryCatch(
      {
        extraer_mes(
          url = urls$url[i],
          anio = urls$anio[i],
          mes = urls$mes[i]
        )
      },
      error = function(e) {
        warning(
          "Error en ",
          urls$anio[i], " - ",
          meses[urls$mes[i]], ": ",
          conditionMessage(e)
        )
        
        tibble()
      }
    )
    
  }
)


# VALIDACIÓN DEL NÚMERO DE REGISTROS
nrow(datos_2024_2025)


# REGISTROS POR AÑO
datos_2024_2025 %>%
  count(format(fecha, "%Y"))


# REVISIÓN DE DUPLICADOS
sum(duplicated(datos_2024_2025$fecha))


# REVISIÓN DE VALORES FALTANTES
sum(is.na(datos_2024_2025$fecha))
sum(is.na(datos_2024_2025$high))
sum(is.na(datos_2024_2025$low))


# COBERTURA DE FECHAS
min(datos_2024_2025$fecha)
max(datos_2024_2025$fecha)


# GUARDAR LOS DATOS EN CSV
write.csv(
  datos_2024_2025,
  "johan_steven_avilan_penaloza/taller_01/clima_bogota_2024_2025.csv",
  row.names = FALSE
)


