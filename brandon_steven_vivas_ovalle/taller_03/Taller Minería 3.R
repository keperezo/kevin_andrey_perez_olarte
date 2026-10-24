url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}
# Instalamos los paquetes necesarios
paquetes  <- c("DBI", "RSQLite", "dplyr", "knitr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)
library(dplyr)

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")
dbListTables(con)

dbListFields(con, "Track")
resultado <- dbGetQuery(con, "
  SELECT *
  FROM   Track
  LIMIT  5;
")
print(resultado, caption = "Primeras 5 filas de Track")

###PUNTO 1
# Número de pistas por género
resultado <- dbGetQuery(con, "
  SELECT   g.Name        AS genero,
           COUNT(t.TrackId) AS n_pistas
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")
print(resultado, caption = "Número de pistas por género")
 
Suma_pistas <- dbGetQuery(con, "
                          select count(*) AS n_pistas
                          FROM track;
                          ")
Suma_pistas

###PUNTO 2
resultado <- dbGetQuery(con, "
 SELECT g.name AS genero, 
 COUNT(t.TrackId) AS generolist
 FROM Track t
 JOIN Genre g ON t.GenreId = g.GenreId
  GROUP BY g.name
 ORDER BY genero DESC;
 ")
print(resultado, caption = "Total de Géneros")

totalg <- dbGetQuery(con, "
 SELECT COUNT (*) AS totalg
 FROM Genre;
 ")
 totalg ###TOTAL DE GÉNEROS
 


resultado <- dbGetQuery(con, "
  SELECT *
  FROM   Track
  LIMIT  50;
")
print(resultado, caption = "BASE DE DATOS")

###PUNTO 3

resultado <- dbGetQuery(con, "
  SELECT g.name AS genero,
 COUNT(t.TrackId) AS n_pistas,
 ROUND(AVG(T.Milliseconds)/60000.0, 2) AS DURACION
 FROM Track t
 JOIN Genre g ON t.GenreId= g.GenreId
 GROUP BY g.name
 ORDER BY n_pistas DESC
 ")
print(resultado, caption = "Estadísticas del total de facturas")


###PUNTO 4
resultado <- dbGetQuery(con, "
 SELECT g.name AS genero, 
 COUNT(t.TrackId) AS n_pistas,
 ROUND(AVG(T.Milliseconds)/60000.0, 2) AS DURACION
 FROM Track t
 JOIN Genre g ON t.GenreId= g.Genre
 LIMIT 5;
 ")
print(resultado, caption = "Estadísticas del total de facturas")
