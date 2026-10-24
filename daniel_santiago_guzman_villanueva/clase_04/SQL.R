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
mostrar_tabla(resultado, caption = "Primeras 5 filas de Track")






# pregunta 1: numero de pistas --------------------------------------------
n_pistas <- dbGetQuery(con, "
  SELECT COUNT(Name) as n_pistas
  FROM Track
")

# pregunta 2: numero de generos -------------------------------------------
n_generos <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT GenreId) as n_generos
  FROM Track
")

# Pregunta 3: estadisticas genero -----------------------------------------
ests_gen <- dbGetQuery(con, "
  SELECT g.Name AS Genero,
    COUNT(t.Name) AS Conteo,
    AVG(T.Milliseconds)/60000 AS Tiempo
  FROM Track t
  LEFT JOIN Genre g ON t.GenreId = g.GenreId
  GROUP BY t.GenreId
  ORDER BY COUNT(t.Name) DESC
")

# Pregunta 4: Canciones largas --------------------------------------------
loooong <- dbGetQuery(con,"
  SELECT t.Name as Cancion,
    a.Title as Album,
    t.Milliseconds / 60000 AS Tiempo
  FROM Track t
  JOIN Album a ON t.AlbumId = a.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT 5
")
dbListFields(con, "Track")

# Ventas mayores a 0.99 ---------------------------------------------------
ven_99 <- dbGetQuery(con, "
  SELECT COUNT(*) AS Numero_pistas,
    COUNT(*) * 100/(SELECT COUNT(*) FROM Track) AS Porcentaje
  FROM Track
  WHERE UnitPrice > 0.99
")

# Pregunta 6 estas disp ---------------------------------------------------

stats_milli <- dbGetQuery(con, "
  SELECT AVG(Milliseconds) AS Promedio,
    MIN(Milliseconds) AS Min,
    MAX(Milliseconds) AS Max,
    AVG(Milliseconds * Milliseconds) -
    AVG(Milliseconds) * AVG(Milliseconds) AS Var
  FROM Track
")
