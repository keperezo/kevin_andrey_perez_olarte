url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

install.packages("pacman")

library(pacman)

p_load("DBI", "RSQLite", "dplyr", "knitr")

con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

dbListTables(con)

dbListFields(con, "Track")

#Ejercicio 1
 
# 1. Cuántas pistas hay en total en la bd?
p1.1 <- dbGetQuery(con, "
  SELECT COUNT(*)                    AS total_pistas
                   FROM Track;
                   ")

p1.1


#2. Cuántos géneros distintos existen?
p1.2 <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT GenreId)     AS generos_distintos
                   FROM Track;
                   ")

p1.2

#3. Obtén el nombre del género, el número de pistas y la duración promedio 
#(en minutos) para cada género. Ordena de mayor a menor número de pistas.

p1.3 <- dbGetQuery(con, "
  SELECT 
    Genre.Name                                      AS Genero,
    COUNT(Track.TrackId)                            AS Numero_Pistas,
    ROUND(AVG(Track.Milliseconds) / 60000.0, 2)     AS Duracion_Promedio_Minutos
  FROM Track
  INNER JOIN Genre 
    ON Track.GenreId = Genre.GenreId
  GROUP BY Genre.GenreId, Genre.Name
  ORDER BY Numero_Pistas DESC;
")

p1.3


#4. Cuáles son las 5 pistas más largas? 
#Muestra el nombre, el álbum y la duración en minutos


p1.4 <- dbGetQuery(con, "
  SELECT 
    COUNT(Track.UnitPrice ),
    Album.Title AS Album,
    Track.Milliseconds / 60000.0 AS Duracion_Minutos
  FROM Track
  INNER JOIN Album
    ON Track.AlbumId = Album.AlbumId
  ORDER BY Track.Milliseconds DESC
  LIMIT 5;
")

p1.4


#5.Cuántas pistas tienen un UnitPrice superior a $0.99? 
#¿Qué porcentaje representan del total?

p1.5 <- dbGetQuery(con, "
                   SELECT 
                   COUNT(*) AS Numero_Pistas,
                   COUNT(*) * 100.0 / (SELECT COUNT(*) FROM Track) AS Porcentaje_Total
                   FROM Track
                   WHERE UnitPrice > 0.99;
                   ")

p1.5


#Calcula la media, mínimo, máximo y varianza (en SQL) 
#de la duración en milisegundos de todas las pistas.

p1.6 <- dbGetQuery(con, "
  SELECT 
    AVG(Milliseconds) AS Media,
    MIN(Milliseconds) AS Minimo,
    MAX(Milliseconds) AS Maximo,
    VARIANCE(Milliseconds) AS Varianza
  FROM Track;
")

p1.6


# Ejercicio 2

#1. ¿Cuál es el total de ingresos generados por la tienda? 
#¿Y el promedio por factura?

p2.1 <- dbGetQuery(con, "
  SELECT 
    SUM(Total) AS Ingreso_Total,
    AVG(Total) AS Promedio_Por_Factura
  FROM Invoice;
")

p2.1


#2. ¿Cuántas facturas se emitieron por año? Ordena cronológicamente.

p2.2 <- dbGetQuery(con, "
  SELECT 
    strftime('%Y', InvoiceDate) AS Año,
    COUNT(InvoiceId) AS Numero_Facturas
  FROM Invoice
  GROUP BY Año
  ORDER BY Año ASC;
")

p2.2



#3. Identifica los 5 países con más ingresos. Muestra: país, número de facturas, 
#ingreso total y promedio por factura.

p2.3 <- dbGetQuery(con, "
SELECT 
i.BillingCountry AS Pais,
COUNT(DISTINCT i.InvoiceId) AS Numero_Facturas,
SUM(il.UnitPrice * il.Quantity) AS Ingreso_Total,
SUM(il.UnitPrice * il.Quantity) / COUNT(DISTINCT i.InvoiceId) AS Promedio_Por_Factura
FROM Invoice i
INNER JOIN InvoiceLine il
ON i.InvoiceId = il.InvoiceId
GROUP BY i.BillingCountry
ORDER BY Ingreso_Total DESC
LIMIT 5;
")

p2.3


#4. Cuál es la desviación estándar del total de facturas? 
#Calcula primero la varianza en SQL y luego obtén la raíz en R o Python.

p2.4 <- dbGetQuery(con, "
  SELECT 
    AVG(Total * Total) - AVG(Total) * AVG(Total) AS Varianza
  FROM Invoice;
")

p2.4

Desviacion_Estandar <- sqrt(p3.4$Varianza)

Desviacion_Estandar


#5. Encuentra los meses con mayor y menor ingreso promedio.

p2.5_mayor <- dbGetQuery(con, "
  SELECT 
    strftime('%m', InvoiceDate) AS Mes,
    AVG(Total) AS Ingreso_Promedio
  FROM Invoice
  GROUP BY Mes
  ORDER BY Ingreso_Promedio DESC
  LIMIT 1;
")

p2.5_menor <- dbGetQuery(con, "
  SELECT 
    strftime('%m', InvoiceDate) AS Mes,
    AVG(Total) AS Ingreso_Promedio
  FROM Invoice
  GROUP BY Mes
  ORDER BY Ingreso_Promedio ASC
  LIMIT 1;
")

p2.5_mayor 
p2.5_menor

