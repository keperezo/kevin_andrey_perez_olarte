# Ejercicio 1.

#Para cada fuente de datos, indique: 
#(a) si es estructurada, semi-estructurada o no estructurada; 
#(b) la razón; 
#(c) el paso de preprocesamiento mínimo para convertirla en un tibble analizable.


#1. Microdatos GEIH del DANE (archivo .csv de 300.000 filas)
# Es una fuente de datos estructurada, pues es un dataframe, con columnas de
# nombre y tipo fijo.

#2. Respuesta JSON de la API pública datos.gov.co con indicadores de calidad del aire
# Su formato JSON lo hace una fuente de datos semi estructurada.

#3. Grabaciones de audio de audiencias judiciales de la Rama Judicial
# Son archivos de audio, lo que lo hace una fuente de datos no estructurados.

#4. Factura electrónica emitida por la DIAN (formato XML)
# Por su formato XML, es una fuente de datos semi-estructurada.

#5. Tabla HTML de Wikipedia scrapeada con rvest en las clases 2–3
# Por su formato HTML, es una fuente de datos semi-estructurada.

#------------

#Ejercicio 2

install.packages("pacman")

library(pacman)

p_load(jsonlite, dplyr, tibble)

url  <- "https://jsonplaceholder.typicode.com/users"
raw  <- fromJSON(url)

str(raw)

#Las columnas anidadas son:
# - address
#   - geo
# - company


tabla <- tibble(raw)

#Esta fuente es un JSON, específicamente un JSON con una estructura anidada.

#fromJSON() no produce directamente un tibble plano porque la información
#no está organizada como una tabla simple de filas y columnas.

rm(ls=raw)
