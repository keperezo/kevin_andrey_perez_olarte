############Ejercicio 1: clasificación de tipos de datos###############

#Para cada fuente de datos, indique: (a) si es estructurada, semi-estructurada 
#o no estructurada; (b) la razón; (c) el paso de preprocesamiento mínimo para 
#convertirla en un tibble analizable.

#1.Microdatos GEIH del DANE (archivo .csv de 300.000 filas)

# a) Son datos estructurados
# b) Porque esta en un formato tabla y puedo hacer consultas directamente de eso
# c) En los datos estructurados el paso basico es la limpieza, es decir que tengamos
# verificar espacios, mayusculas, tildes, datos nulos, etc.

#2.Respuesta JSON de la API pública datos.gov.co con indicadores de calidad del aire

# a) Son datos semi-estructurados
# b) No tiene una estructura tabular como tal, tiene jerarquia en los indicadores
# c) El principal paso es hacer aplanado de los datos hasta llegar a tibble, 
# pero puede no ser un paso directo

#3.Grabaciones de audio de audiencias judiciales de la Rama Judicial

# a) Son datos no estructurados
# b) Sin formato predefinido. 
# c) Se requiere hacer feature engineering para convertirse en representación 
# numérica que los algoritmos puedan consumir y hacer algoritmo DM

#4.Factura electrónica emitida por la DIAN (formato XML)

# a) Son datos semi-estructurados
# b) No tiene una estructura tabular como tal, y estructuras no tan lineales
# c) El principal paso es hacer aplanado de los datos hasta llegar a tibble, 
# pero puede no ser un paso directo

#5.Tabla HTML de Wikipedia scrapeada con rvest en las clases 2–3

# a) Son datos semi-estructurados
# b) No tiene una estructura tabular como tal, y estructuras no tan lineales
# c) El principal paso es hacer aplanado de los datos hasta llegar a tibble, 
# pero puede no ser un paso directo


#########Ejercicio 2: JSON anidado a DataFrame

# Descargue el siguiente JSON público con datos de usuarios (incluye 
# campos anidados address y company)

library(jsonlite); library(dplyr); library(tibble)
url  <- "https://jsonplaceholder.typicode.com/users"
raw  <- fromJSON(url)

str(raw)
