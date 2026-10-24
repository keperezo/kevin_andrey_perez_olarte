paquetes <- c("DBI","RSQLite","dplyr","knitr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes,instalados)
if(length(pendientes)>0) install.packages(pendientes)
library(DBI)
library(RSQLite)
library(dplyr)
url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

dbListTables(con)

resultado <- dbGetQuery(con,"
SELECT *
FROM Track
LIMIT 5;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name, Milliseconds, UnitPrice
FROM Track
LIMIT 10;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name AS pista,
ROUND(Milliseconds/60000.0,2) AS duracion_min,
UnitPrice AS precio_usd
FROM Track
LIMIT 10;")
resultado

paises <- dbGetQuery(con,"
SELECT DISTINCT BillingCountry AS pais
FROM Invoice
ORDER BY pais;")
head(paises,15)

resultado <- dbGetQuery(con,"
SELECT Name, UnitPrice
FROM Track
WHERE UnitPrice > 0.99
LIMIT 10;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name,
ROUND(Milliseconds/60000.0,2) AS duracion_min,
UnitPrice
FROM Track
WHERE UnitPrice > 0.99
AND Milliseconds < 180000
LIMIT 10;")
resultado

resultado_in <- dbGetQuery(con,"
SELECT InvoiceId, BillingCountry, Total
FROM Invoice
WHERE BillingCountry IN ('Brazil','Canada','France')
ORDER BY Total DESC
LIMIT 10;")
resultado_in

resultado_between <- dbGetQuery(con,"
SELECT InvoiceId, BillingCountry, Total
FROM Invoice
WHERE Total BETWEEN 5 AND 10
ORDER BY Total DESC
LIMIT 10;")
resultado_between

resultado <- dbGetQuery(con,"
SELECT Name AS artista
FROM Artist
WHERE Name LIKE 'The%'
ORDER BY Name;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name AS genero
FROM Genre
WHERE Name LIKE '%Blues'
ORDER BY Name;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name AS genero
FROM Genre
WHERE Name LIKE '%Rock%'
ORDER BY Name;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name AS artista
FROM Artist
WHERE Name LIKE '__'
ORDER BY Name;")
resultado

resultado <- dbGetQuery(con,"
SELECT Name AS pista,
ROUND(Milliseconds/60000.0,2) AS duracion_min
FROM Track
ORDER BY Milliseconds DESC
LIMIT 10;")
resultado

resultado <- dbGetQuery(con,"
SELECT BillingCountry AS pais,
InvoiceDate AS fecha,
Total
FROM Invoice
ORDER BY BillingCountry ASC, Total DESC
LIMIT 12;")
resultado

resultado <- dbGetQuery(con,"
SELECT g.Name AS genero,
COUNT(t.TrackId) AS n_pistas
FROM Track t
JOIN Genre g ON t.GenreId = g.GenreId
GROUP BY g.Name
ORDER BY n_pistas DESC;")
resultado

resultado <- dbGetQuery(con,"
SELECT BillingCountry AS pais,
SUBSTR(InvoiceDate,1,4) AS anho,
COUNT(*) AS n_facturas,
ROUND(SUM(Total),2) AS total_ventas
FROM Invoice
GROUP BY BillingCountry, anho
ORDER BY total_ventas DESC
LIMIT 15;")
resultado

resultado <- dbGetQuery(con,"
SELECT COUNT(*) AS total_pistas,
COUNT(DISTINCT GenreId) AS generos_distintos,
COUNT(DISTINCT AlbumId) AS albums_distintos,
COUNT(DISTINCT Composer) AS compositores_distintos
FROM Track;")
resultado

resultado <- dbGetQuery(con,"
SELECT g.Name AS genero,
ROUND(AVG(t.Milliseconds)/60000.0,2) AS duracion_media_min
FROM Track t
JOIN Genre g ON t.GenreId = g.GenreId
GROUP BY g.Name
ORDER BY duracion_media_min DESC;")
resultado

resultado <- dbGetQuery(con,"
SELECT ROUND(MIN(Total),2) AS factura_minima,
ROUND(MAX(Total),2) AS factura_maxima,
ROUND(AVG(Total),2) AS factura_media,
ROUND(MAX(Total)-MIN(Total),2) AS rango
FROM Invoice;")
resultado

resultado <- dbGetQuery(con,"
SELECT BillingCountry AS pais,
COUNT(*) AS n_facturas,
ROUND(SUM(Total),2) AS ventas_totales
FROM Invoice
GROUP BY BillingCountry
ORDER BY ventas_totales DESC
LIMIT 10;")
resultado

resultado <- dbGetQuery(con,"
SELECT ROUND(AVG(Total),2) AS media,
ROUND(AVG(Total*Total)-AVG(Total)*AVG(Total),2) AS varianza_pob,
ROUND(MIN(Total),2) AS minimo,
ROUND(MAX(Total),2) AS maximo
FROM Invoice;")
resultado

resultado <- dbGetQuery(con,"
SELECT g.Name AS genero,
COUNT(*) AS frecuencia
FROM Track t
JOIN Genre g ON t.GenreId = g.GenreId
GROUP BY g.Name
ORDER BY frecuencia DESC
LIMIT 5;")
resultado

resultado <- dbGetQuery(con,"
SELECT g.Name AS genero,
COUNT(t.TrackId) AS n_pistas,
ROUND(AVG(t.Milliseconds)/60000.0,2) AS duracion_media_min,
ROUND(MIN(t.Milliseconds)/60000.0,2) AS duracion_min_min,
ROUND(MAX(t.Milliseconds)/60000.0,2) AS duracion_max_min,
ROUND(AVG(t.UnitPrice),2) AS precio_medio,
ROUND(AVG(t.Milliseconds*t.Milliseconds)-AVG(t.Milliseconds)*AVG(t.Milliseconds),0) AS varianza_ms
FROM Track t
JOIN Genre g ON t.GenreId = g.GenreId
GROUP BY g.Name
ORDER BY n_pistas DESC;")
head(resultado,15)

resultado <- dbGetQuery(con,"
SELECT BillingCountry AS pais,
COUNT(*) AS n_facturas,
ROUND(SUM(Total),2) AS total_ventas,
ROUND(AVG(Total),2) AS media_factura,
ROUND(MIN(Total),2) AS min_factura,
ROUND(MAX(Total),2) AS max_factura,
ROUND(AVG(Total*Total)-AVG(Total)*AVG(Total),2) AS varianza_total
FROM Invoice
GROUP BY BillingCountry
ORDER BY total_ventas DESC
LIMIT 15;")
resultado

dbDisconnect(con)
cat("Conexión cerrada.\n")