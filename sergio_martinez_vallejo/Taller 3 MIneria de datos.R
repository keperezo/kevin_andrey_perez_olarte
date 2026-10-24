url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

library(DBI)
library(RSQLite)
library(dplyr)

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")


# Listamos las tablas disponibles
dbListTables(con)


# Columnas de la tabla Track
dbListFields(con, "Track")




# Listamos las tablas disponibles
dbListTables(con)


# Columnas de la tabla Track
dbListFields(con, "Track")


### EJERCICIO NUMERO 1


resultado <- dbGetQuery(con, "
  SELECT *
  FROM   Track
  LIMIT  5;
")
resultado


resultado1 <- dbGetQuery(con, "
  SELECT Name,
         Milliseconds,
         UnitPrice
  FROM   Track
  LIMIT  10;
")
resultado1


pistas <- dbGetQuery(con, "
  SELECT COUNT(*) AS pistas
  FROM Track;
")
pistas

generos <- dbGetQuery(con, "
  SELECT COUNT(*) AS generos
  FROM Genre;
")
generos


dbListFields(con, "Track")

## Genero por total de pistas y media de tiempo en canciones
gen_pist_time <- dbGetQuery(con, "
  SELECT Genre.Name,
  COUNT (Track.TrackId) AS numeropistas,
  AVG(Track.Milliseconds)/60000 AS tiempopromedio
  FROM Genre INNER JOIN Track ON GENRE.GenreId = Track.GenreId
  GROUP BY Genre.GenreId, Genre.Name
  ORDER BY numeropistas DESC;
  
")
gen_pist_time

### Nombre de las 5 pistas mas largas 
cincopistas <-  dbGetQuery(con, "
  SELECT Track.Name, 
  Album.Title, 
  Track.Milliseconds/60000 AS minutos
  FROM Track INNER JOIN Album ON Track.AlbumId = Album.AlbumId
  ORDER BY minutos DESC
  LIMIT 5;
")
cincopistas


### Pistas con UnitPrice superior a 0.99
UnitPrice <- dbGetQuery(con, "
  SELECT
    COUNT(*) AS mayor_099,
    COUNT(*) * 100.0 / (SELECT COUNT(*) FROM Track) AS porcentaje
  FROM Track
  WHERE UnitPrice > 0.99;
")
UnitPrice


### Media, mínimo, máximo y varianza de la duración en milisegundos

duracion <- dbGetQuery(con, "
  SELECT
    AVG(Milliseconds) AS media,
    MIN(Milliseconds) AS minimo,
    MAX(Milliseconds) AS maximo,
    VARIANCE(Milliseconds) AS varianza
  FROM Track;
")
duracion



### EJERCICIO NUMERO 2


# total de ingresos y promedio por factura
total_ingresos <- dbGetQuery(con, "
  SELECT
    SUM(UnitPrice * Quantity) AS ingreso_tienda,
    SUM(UnitPrice * Quantity) / COUNT(DISTINCT InvoiceId) AS promedio_factura
  FROM InvoiceLine;
")
total_ingresos


# facturas que se hicieron durante el año (cronologiacamente)
facturas_anuales <- dbGetQuery(con, "
  SELECT
    strftime('%Y', InvoiceDate) AS Año,
    COUNT(*) AS facturas
  FROM Invoice
  GROUP BY Año
  ORDER BY Año;
")
facturas_anuales 


# 5 paises con mas ingresos 

paises_ingresos <- dbGetQuery(con, "
  SELECT
    Invoice.BillingCountry AS pais,
    COUNT(DISTINCT Invoice.InvoiceId) AS facturas,
    SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) AS Ingresos,
    SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) / COUNT(DISTINCT Invoice.InvoiceId) AS promedio_factura
  FROM Invoice
  INNER JOIN InvoiceLine
    ON Invoice.InvoiceId = InvoiceLine.InvoiceId
  GROUP BY Invoice.BillingCountry
  ORDER BY Ingresos DESC
  LIMIT 5;
")
paises_ingresos


# desviacion estandar

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
varianza_facturas

## sacamos la raiz de la varianza
desviacion_facturas <- sqrt(varianza_facturas$varianza)
desviacion_facturas



# meses con mayor y menor ingreso promedio
ingreso_promedio <- dbGetQuery(con, "
  SELECT
    strftime('%m', InvoiceDate) AS Mes,
    AVG(total) AS Ingreso_promedio
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
  GROUP BY Mes
  ORDER BY Ingreso_promedio DESC
  limit 5;
")
ingreso_promedio
