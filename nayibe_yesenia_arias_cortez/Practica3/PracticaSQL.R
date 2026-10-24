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

# Columnas de la tabla Track
dbListFields(con, "PlaylistTrack")



# Mi primera consulta

paises <- dbGetQuery(con, "
  SELECT DISTINCT BillingCountry AS pais
  FROM   Invoice
  ORDER  BY pais;
")

#¿Cuántas pistas hay en total en la base de datos?
Pistas <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_filas 
  FROM PlaylistTrack;
")
Pistas

# ¿Cuántos géneros distintos existen?
generos <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT Name) AS Total_Generos
  FROM Genre;
")
generos

# Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. Ordena de mayor a menor número de pistas.
## usamos Join 
respuesta <- dbGetQuery(con, "
  SELECT 
    g.Name AS Genero,
    COUNT(t.TrackId) AS Numero_Pistas,
    ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS Duracion_Promedio_Min
FROM Genre g
JOIN Track t ON g.GenreId = t.GenreId
GROUP BY g.GenreId, g.Name
ORDER BY Numero_Pistas DESC;
")

respuesta

# ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.

Pista_L <- dbGetQuery(con, "
  SELECT 
    t.Name AS Cancion,
    al.Title AS Album,
    ROUND(t.Milliseconds / 60000.0, 2) AS Duracion_Min
  FROM Track t
  JOIN Album al ON t.AlbumId = al.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT 5;
")
Pista_L

# ¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del total?

UnitPrice <- dbGetQuery(con, "
  SELECT 
    COUNT(*) AS Total_Pistas,
    SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) AS Pistas_Caras,
    ROUND(
        (SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) * 100.0) / COUNT(*), 
        2
    ) AS Porcentaje
  FROM Track;
")
UnitPrice
