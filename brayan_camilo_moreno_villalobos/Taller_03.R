url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}
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
dbListFields(con,"Track")


##########################################################
###### ejercicio 1 ######################################
#########################################################
resultado1 <- dbGetQuery(con, "
  SELECT   COUNT(t.TrackId) AS n_pistas
  FROM     Track  t
")
print(resultado1, caption = "Número de pistas")

resultado2 <- dbGetQuery(con, "
  SELECT   COUNT(g.GenreId) AS n_generos
  FROM     Genre g
")
print(resultado2, caption = "Número de pistas")

resultado3 <- dbGetQuery(con, "
  SELECT   g.Name        AS genero,
           COUNT(t.TrackId) AS n_pistas
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")
print(resultado3, caption = "Número de pistas por género")

resultado4 <- dbGetQuery(con, "
  SELECT   g.Name        AS genero,
           ROUND(Milliseconds / 60000.0, 2)/COUNT(t.TrackId) AS Prom_duracion_min,
           COUNT(t.TrackId) AS n_pistas
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")
print(resultado4, caption = "Número de pistas por género")

resultado5 <- dbGetQuery(con, "
  SELECT Name                              AS pista,
         ROUND(Milliseconds / 60000.0, 2) AS duracion_min
  FROM   Track
  ORDER  BY Milliseconds DESC
  LIMIT  5;
")
print(resultado5, caption = "Las 5 pistas más largas")

resultado6 <- dbGetQuery(con, "
  SELECT COUNT(t.TrackId)/ AS n_pistas
  FROM   Track t
  WHERE  UnitPrice > 0.99
")
print(round(resultado6*100/resultado1,2), caption = "Primeras 5 filas de Track")

resultado7 <- dbGetQuery(con, "
  SELECT 
    COUNT(t.TrackId) AS n_pistas,
    ROUND(AVG(t.Milliseconds/n_pistas),2) AS media,
    ROUND(AVG(t.Milliseconds * t.Milliseconds) - AVG(t.Milliseconds/n_pistas)*AVG(t.Milliseconds/n_pistas), 2) AS varianza_pob,
    ROUND(MIN(t.Milliseconds),2) AS minimo,
    ROUND(MAX(t.Milliseconds),2) AS maximo
  FROM Track t;
")
print(resultado7, caption = "Primeras 5 filas de Track")
