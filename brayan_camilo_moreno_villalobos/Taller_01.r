paquetes <- c(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr", "knitr", "httr"
)
# Verificamos qué paquetes faltan. La instalación se hace por fuera de la
# compilación para evitar cambios inesperados en el entorno del estudiante.
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  stop(
    "Faltan paquetes: ", paste(pendientes, collapse = ", "),
    ". Instálelos con install.packages(c(",
    paste(sprintf('"%s"', pendientes), collapse = ", "), "))"
  )
}
invisible(lapply(paquetes, library, character.only = TRUE))

###################################################################
meses <- c("January", "February", "March", "April", "May", "June", 
           "July", "August", "September", "October", "November", "December")
anios <- 2025:2026
combinaciones <- expand.grid(mes = meses, anio = anios)

fun_mes <- function(mes){
  case_when(mes == "enero" ~ 01,
            mes == "febrero" ~ 02,
            mes == "marzo" ~ 03,
            mes == "abril" ~ 04,
            mes == "mayo" ~ 05,
            mes == "junio" ~ 06,
            mes == "julio" ~ 07,
            mes == "agosto" ~ 08,
            mes == "septiembre" ~ 09,
            mes == "octubre" ~ 10,
            mes == "noviembre" ~ 11,
            mes == "diciembre" ~ 12
  )
}


urls_weather <- paste0(
  "https://www.accuweather.com/es/co/bogota/107487/",
  combinaciones$mes,
  "-weather/107487?year=",
  combinaciones$anio
)
urls_weather

scrapear_weather <- function(url) {
    Sys.sleep(9)
    print(paste0("conectado a:",url))
    respuesta <- GET(
      url,
      add_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
        `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
        `Referer` = "https://www.google.com/"
      )
    )
    print(status_code(respuesta))
    if(status_code(respuesta) == 200) {
      pagina <- read_html(content(respuesta, "text", encoding = "UTF-8"))
      print("Conexión exitosa")
    } else {
      print("no conexion")
    }
    nodos <- pagina %>% html_elements("div.page-content.content-module")
    
    
    map_dfr(nodos, function(nodo,tabla_mes) {
      año <- nodo %>% html_elements("div.map-dropdown-toggle h2") %>% html_text2() 
      dia <- nodo %>% 
        html_elements("div.monthly-component a div.monthly-panel-top div") %>%
        html_text2()
      
      min <- nodo %>% 
        html_elements("div.monthly-component a div.temp div.low") %>%
        html_text2()
      max <- nodo %>% 
        html_elements("div.monthly-component a div.temp div.high") %>%
        html_text2()
      
      tabla_mes <- tibble(
        fecha = as.Date(paste0(año[2],"-",fun_mes(año[1]),"-",dia[cumsum(dia == "1") == 1]), format = "%Y-%m-%d"),
        high =  as.numeric(max[cumsum(dia == "1") == 1] %>% str_replace_all("[°]", "")) ,
        low = as.numeric(min[cumsum(dia == "1") == 1] %>% str_replace_all("[°]", "")) 
      )%>%
      filter(fecha <= Sys.Date()) 
      write_csv(tabla_mes, "backup_clima_bogota.csv", append = TRUE)
      return(tabla_mes)
     
    })
    
}

wather_final <- map_dfr(urls_weather, scrapear_weather)

