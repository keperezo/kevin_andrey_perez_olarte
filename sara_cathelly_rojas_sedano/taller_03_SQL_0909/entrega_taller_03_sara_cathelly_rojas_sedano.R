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


# Listamos las tablas disponibles
dbListTables(con)

#####¿Cuántas pistas hay en total en la base de datos?
# Columnas de la tabla Track
dbListFields(con, "Track")


# Consultar columnas
resultado <- dbGetQuery(con, "
  SELECT *
  
  FROM   Track;
  ")

#consultar tracks unicos
tracksunicos <- dbGetQuery(con, "
  SELECT DISTINCT Name, COUNT(TrackId) As num_pistas
  FROM   Track
  Group  BY Name;
")

#####¿Cuántos géneros distintos existen?
# Columnas de la tabla genero
dbListFields(con, "Genre")


# Consultar columnas
generosunicos <- dbGetQuery(con, "
  SELECT *
  
  FROM   Genre;
  ")


# Consultar columnas
resultado <- dbGetQuery(con, "
  SELECT *
  
  FROM   Genre;
  ")

#consultar generos unicos
generosunicos <- dbGetQuery(con, "
  SELECT DISTINCT Name, COUNT(GenreId) As num_pistas
  FROM   genre
  Group  BY ;
")

####Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. Ordena de mayor a menor número de pistas.

# Número de pistas por género
generoxpista <- dbGetQuery(con, "
  SELECT   g.Name        AS genero,
  COUNT(t.TrackId) AS n_pistas,
  AVG(t.Milliseconds) As promedio
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")


####¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.
#toppistas
toppistas <- dbGetQuery(con, "
  SELECT name, Milliseconds, AlbumId
  FROM   Track
  ORDER BY Milliseconds
  LIMIT  5;
")
