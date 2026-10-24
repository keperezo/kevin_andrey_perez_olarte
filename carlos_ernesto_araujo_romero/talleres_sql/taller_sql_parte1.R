library(DBI)
library(RSQLite)
library(dplyr)

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
}

con <- dbConnect(RSQLite::SQLite(), ruta_db)

# Ejercicio 1: Analisis descriptivo de la coleccion musical

# 1. Total de pistas en la base de datos
dbGetQuery(con, "
  SELECT COUNT(*) AS total_pistas 
  FROM Track;
")

# 2. Cantidad de generos distintos existentes
dbGetQuery(con, "
  SELECT COUNT(DISTINCT GenreId) AS generos_en_tracks,
         (SELECT COUNT(*) FROM Genre) AS total_generos_catalogo
  FROM Track;
")

# 3. Genero, numero de pistas y duracion promedio en minutos
dbGetQuery(con, "
  SELECT 
    g.Name AS genero,
    COUNT(t.TrackId) AS n_pistas,
    ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS duracion_promedio_min
  FROM Track t
  JOIN Genre g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")

# 4. Top 5 pistas mas largas con nombre, album y duracion en minutos
dbGetQuery(con, "
  SELECT 
    t.Name AS pista,
    a.Title AS album,
    ROUND(t.Milliseconds / 60000.0, 2) AS duracion_min
  FROM Track t
  JOIN Album a ON t.AlbumId = a.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT 5;
")

# 5. Pistas con UnitPrice > 0.99 y porcentaje que representan
dbGetQuery(con, "
  SELECT 
    SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) AS n_pistas_caras,
    COUNT(*) AS total_pistas,
    ROUND(100.0 * SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) / COUNT(*), 2) AS porcentaje
  FROM Track;
")

# 6. Estadisticas de duracion en milisegundos (media, min, max, varianza poblacional)
dbGetQuery(con, "
  SELECT 
    ROUND(AVG(Milliseconds), 2) AS media_ms,
    MIN(Milliseconds) AS min_ms,
    MAX(Milliseconds) AS max_ms,
    ROUND(AVG(Milliseconds * Milliseconds) - AVG(Milliseconds) * AVG(Milliseconds), 2) AS varianza_ms
  FROM Track;
")

# Ejercicio 2: Analisis descriptivo de ventas

# 1. Total de ingresos generados y promedio por factura
dbGetQuery(con, "
  SELECT 
    ROUND(SUM(Total), 2) AS total_ingresos,
    ROUND(AVG(Total), 2) AS promedio_factura
  FROM Invoice;
")

# 2. Facturas emitidas por ano (orden cronologico)
dbGetQuery(con, "
  SELECT 
    SUBSTR(InvoiceDate, 1, 4) AS anio,
    COUNT(*) AS n_facturas,
    ROUND(SUM(Total), 2) AS ingresos_anio
  FROM Invoice
  GROUP BY anio
  ORDER BY anio ASC;
")

# 3. Top 5 paises con mas ingresos
dbGetQuery(con, "
  SELECT 
    BillingCountry AS pais,
    COUNT(*) AS n_facturas,
    ROUND(SUM(Total), 2) AS ingreso_total,
    ROUND(AVG(Total), 2) AS promedio_factura
  FROM Invoice
  GROUP BY BillingCountry
  ORDER BY ingreso_total DESC
  LIMIT 5;
")

# 4. Desviacion estandar del total de facturas (Varianza en SQL, raiz en R)
var_df <- dbGetQuery(con, "
  SELECT 
    AVG(Total * Total) - AVG(Total) * AVG(Total) AS varianza_total
  FROM Invoice;
")

desv_estandar <- sqrt(var_df$varianza_total)
cat("Varianza (SQL):", round(var_df$varianza_total, 4), "\n")
cat("Desviacion Estandar (R):", round(desv_estandar, 4), "\n\n")

# 5. Meses con mayor y menor ingreso promedio (analizado por mes calendario 01-12)
ingresos_mensuales <- dbGetQuery(con, "
  SELECT 
    SUBSTR(InvoiceDate, 6, 2) AS mes,
    COUNT(*) AS n_facturas,
    ROUND(SUM(Total), 2) AS total_ingresos,
    ROUND(AVG(Total), 2) AS ingreso_promedio
  FROM Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio DESC;
")

cat("Mes con MAYOR ingreso promedio:\n")
print(head(ingresos_mensuales, 1))

cat("\nMes con MENOR ingreso promedio:\n")
print(tail(ingresos_mensuales, 1))

# Cerrar conexion al terminar
dbDisconnect(con)
