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

### 1. ¿Cuántas pistas hay en total en la base de datos? ### 

pistas_totales <- dbGetQuery(con, "
  SELECT count(*) AS total_pistas  
  FROM TRACK;
")

### 2.  ¿Cuántos géneros distintos existen? ### 

generos_distintos <- dbGetQuery(con, "
  SELECT count(*) AS total_generos
  FROM Genre;
")

### 3. Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. Ordena de mayor a menor número de pistas.

Nombre_pistas_duracion <- dbGetQuery(con, "
  SELECT Genre.Name, count(Track.TrackId) AS total_pistas, AVG(Track.Milliseconds / 60000) AS duracion_min
  FROM Genre INNER JOIN Track ON Genre.GenreId = Track.GenreId 
  GROUP BY Genre.GenreID, Genre.Name
  ORDER BY total_pistas DESC;
")

### 4. ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos. ###

five_more <-  dbGetQuery(con, "
  SELECT Track.Name, Album.Title, Track.Milliseconds/60000 AS Minutos
  FROM Track INNER JOIN Album ON Track.AlbumId = Album.AlbumId
  ORDER BY Minutos DESC
  LIMIT 5;
")

### 5. ¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del total?

pistas_precio <- dbGetQuery(con, "
  SELECT 
    COUNT(*) AS pistas_mayor_099,
    COUNT(*) * 100.0 / (SELECT COUNT(*) FROM Track) AS porcentaje
  FROM Track
  WHERE UnitPrice > 0.99;
")


### 6. ¿Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos de todas las pistas.?

duracion <- dbGetQuery(con, "
  SELECT 
    AVG(Milliseconds) AS media,
    MIN(Milliseconds) AS minimo,
    MAX(Milliseconds) AS maximo,
    VARIANCE(Milliseconds) AS varianza
  FROM Track;
")

##### Punto 2 

ingresos <- dbGetQuery(con, "
  SELECT 
    SUM(UnitPrice * Quantity) AS ingreso_total,
    SUM(UnitPrice * Quantity) / COUNT(DISTINCT InvoiceId) AS promedio_por_factura
  FROM InvoiceLine;
")

facturas_anio <- dbGetQuery(con, "
  SELECT 
    strftime('%Y', InvoiceDate) AS año,
    COUNT(*) AS total_facturas
  FROM Invoice
  GROUP BY año
  ORDER BY año;
")

paises_ingresos <- dbGetQuery(con, "
  SELECT 
    Invoice.BillingCountry AS pais,
    COUNT(DISTINCT Invoice.InvoiceId) AS numero_facturas,
    SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) AS ingreso_total,
    SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) / COUNT(DISTINCT Invoice.InvoiceId) AS promedio_por_factura
  FROM Invoice 
  INNER JOIN InvoiceLine 
    ON Invoice.InvoiceId = InvoiceLine.InvoiceId
  GROUP BY Invoice.BillingCountry
  ORDER BY ingreso_total DESC
  LIMIT 5;
")

varianza_facturas <- dbGetQuery(con, "
  SELECT 
    AVG(total * total) - AVG(total) * AVG(total) AS varianza
  FROM (
    SELECT 
      InvoiceId,
      SUM(UnitPrice * Quantity) AS total
    FROM InvoiceLine
    GROUP BY InvoiceId
  );
")

ingreso_meses <- dbGetQuery(con, "
  SELECT 
    strftime('%m', InvoiceDate) AS mes,
    AVG(total) AS ingreso_promedio
  FROM (
    SELECT 
      Invoice.InvoiceId,
      Invoice.InvoiceDate,
      SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) AS total
    FROM Invoice
    INNER JOIN InvoiceLine 
      ON Invoice.InvoiceId = InvoiceLine.InvoiceId
    GROUP BY Invoice.InvoiceId, Invoice.InvoiceDate
  )
  GROUP BY mes
  ORDER BY ingreso_promedio DESC;
")









