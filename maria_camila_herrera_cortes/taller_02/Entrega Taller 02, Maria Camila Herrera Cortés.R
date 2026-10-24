# ------------------------------------------------------------
## Taller práctico Web Scraping
## Mineria de datos
## Maria Camila Herrera Cortés
# ------------------------------------------------------------

# ---- Base de datos ----
url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"

ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  
  cat("Base de datos descargada en:", ruta_db, "\n")
  
} else {
  
  cat("La base de datos ya existe:", ruta_db, "\n")
  
}

# ---- Instalamos los paquetes necesarios ----
library(DBI)
library(RSQLite)
library(dplyr)
library(knitr)

# ---- Abrimos la conexión ----
con <- dbConnect(RSQLite::SQLite(), "chinook.db")

cat("Conexión establecida.\n")

# ---- Tablas disponibles ----
dbListTables(con)

# ---- Columnas de la tabla Track ----
dbListFields(con, "Track")

# ---- Tabla ----
resultado <- dbGetQuery(con, "

  SELECT *

  FROM   Track

  LIMIT  5;

")

print(resultado, caption = "Primeras 5 filas de Track")


# ------------------------------------------------------------------- #
# ---- Ejercicio 1: Análisis descriptivo de la colección musical ---- #
# ------------------------------------------------------------------- #

# ---- 1. ¿Cuántas pistas hay en total en la base de datos? ----
resultado <- dbGetQuery(con, "
  SELECT COUNT(*)                    AS total_pistas
  FROM   Track;

")

print(resultado, caption = "Número total de pistas")

# ---- 2. ¿Cuántos géneros distintos existen? ----
resultado <- dbGetQuery(con, "
  SELECT  COUNT(DISTINCT GenreId)     AS generos_distintos
  FROM   Track;

")

print(resultado, caption = "Número total de géneros distintos")

# ----   3. Obtén el nombre del género, el número de pistas y la  ----
# ---- duración promedio (en minutos) para cada género. Ordena de ----
# ----              mayor a menor número de pistas.               ----
resultado <- dbGetQuery(con, "

  SELECT   g.Name        AS genero,

           COUNT(t.TrackId) AS n_pistas,
          
           ROUND(AVG(t.Milliseconds)/60000.0, 2) AS duracion_media_min

  FROM     Track  t

  JOIN     Genre  g ON t.GenreId = g.GenreId

  GROUP BY g.Name

  ORDER BY n_pistas DESC;

")

print(resultado, caption = "Tabla: Género, número de pistas y duración promedio")


dbListFields(con, "Track")
dbListFields(con, "Album")

# ---- 4. ¿Cuáles son las 5 pistas más largas? Muestra ----
# ----   el nombre, el álbum y la duración en minutos  ----
resultado <- dbGetQuery(con, "

  SELECT Name                              AS Pista,

         ROUND(Milliseconds / 60000.0, 2)  AS Duracion_min,
         
         Album.Title                       AS Album

  FROM   Track
  
  JOIN Album ON Track.AlbumId = Album.AlbumId 

  ORDER  BY Milliseconds DESC

  LIMIT  5;

")

print(resultado, caption = "Las 5 pistas más largas")

# ---- ¿Cuántas pistas tienen un UnitPrice superior a ----
# ----  $0.99? ¿Qué porcentaje representan del total? ----
resultado <- dbGetQuery(con, "
  SELECT COUNT(CASE WHEN UnitPrice > 0.99 THEN 1 END) AS Pistas_caras,
      
         ROUND(COUNT(CASE WHEN UnitPrice > 0.99 THEN 1 END) * 
         100.0 / COUNT(*), 2) AS Porcentaje_total
  
  FROM Track;
")
print(resultado)

# ---- Calcula la media, mínimo, máximo y varianza (en SQL) ----
# ----  de la duración en milisegundos de todas las pistas  ----
resultado <- dbGetQuery(con, "
  SELECT  ROUND(AVG(Milliseconds), 2) AS Media_mls,
      
          MIN(Milliseconds) AS Minimo_mls,
      
          MAX(Milliseconds) AS Maximo_mls,
       
          ROUND(AVG(Milliseconds * Milliseconds) - AVG(Milliseconds) * 
          AVG(Milliseconds), 2) AS Varianza_mls
  FROM Track;
")
print(resultado)


# ----------------------------------------------------- #
# ---- Ejercicio 2: Análisis descriptivo de ventas ---- #
# ----------------------------------------------------- #


# ---- Cerrar conexión ----
dbDisconnect(con)

cat("Conexión cerrada.\n")
