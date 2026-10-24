
# Carga de paquetes -------------------------------------------------------

library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(readr)
library(knitr)
library(httr)

# Creacion lista de URLS --------------------------------------------------



URL_base_1 <- "https://www.accuweather.com/es/co/bogota/107487/"
URL_base_2 <- "-weather/107487?year=2025"
meses <- c("january", "february", "march",
           "april", "may", "june",
           "july", "august", "september",
           "october", "November", "december")

URLS <- vector()
for (i in 1:12){
  URLS[i] <- paste0(URL_base_1, meses[i], URL_base_2)
}





# Agente ------------------------------------------------------------------

agente_p <- user_agent(
  paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/120.0.0.0 Safari/537.36"
  )
)


# lectura html ------------------------------------------------------------
# En el ciclo for se lee el html, extrae los datos, se pasa a formato date
# se quitan fechas que no pertenecen al mes
# guardamos en la variable respuesta
# y esperamos unos segundos antes de continuar

respuesta <- data_frame()
for(i in 1:12){
clima <- GET(URLS[i],
             agente_p,
             add_headers(`Accept-Language` = "en-US,en;q=0.9"))
clima_html <- read_html(content(clima, "text"))
status_code(clima)

mes <- html_elements(clima_html, css = ".monthly-daypanel") 
tabla <- purrr::map_dfr(mes, function(mes){
  tibble::tibble(
    dia = html_element(mes, css = ".date") |> html_text(trim = TRUE),
    alta = html_element(mes, css = ".high") |> html_text(trim = TRUE),
    baja = html_element(mes, css = ".low")  |> html_text(trim = TRUE)
  )
})

while(tabla[1,1]!=1) tabla <- tabla[-1,]
while(!(last(tabla[,1])%in%c(28, 29, 30, 31))) {
  tabla <-tabla[1:(length(tabla$dia)-1),]
}
tabla <- tabla %>% mutate(
  dia = as.Date(paste(2025, i, dia, sep = "-"), format = "%Y-%m-%d")
)
respuesta <- rbind(respuesta, tabla)
# para saber avance print mes actual
print(i)
Sys.sleep(abs(rnorm(5,1)))
}

respuesta <- respuesta %>% mutate(
  alta = as.integer(sub("°", "",  alta)),
  baja = as.integer(sub("°", "",  baja))
)

# escritura csv -----------------------------------------------------------

write.csv(respuesta, "./clase_02/clima2025.csv",
          row.names = F)

