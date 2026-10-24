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



# ¿Cuántas pistas hay en total en la base de datos?
resultado <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_pistas
  FROM Track;
")
resultado

# ¿Cuántos géneros distintos existen?
resultado <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT GenreId) AS total_generos
  FROM Track;
")
resultado
resultado <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_generos
  FROM Genre;
")
resultado

# Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. 
# Ordena de mayor a menor número de pistas.
resultado <- dbGetQuery(con, "
SELECT
    g.Name AS genero,
    COUNT(t.TrackId) AS numero_pistas,
    ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS duracion_promedio_minutos
FROM Genre g
JOIN Track t
    ON g.GenreId = t.GenreId
GROUP BY g.Name
ORDER BY numero_pistas DESC;
")
resultado

# ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.
resultado <- dbGetQuery(con, "
SELECT
t.Name AS pista,
a.Title AS album,
ROUND(t.Milliseconds / 60000.0, 2) AS duracion_minutos
FROM Track t
JOIN Album a
ON t.AlbumId = a.AlbumId
ORDER BY t.Milliseconds DESC
LIMIT 5;
")
resultado

# ¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del total?
resultado <- dbGetQuery(con, "
  SELECT
    COUNT(*) AS pistas_superiores,
    ROUND(COUNT() * 100.0 / (SELECT COUNT() FROM Track), 2) AS porcentaje
FROM Track
WHERE UnitPrice > 0.99;
")
resultado

# Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos de todas las pistas.

resultado <- dbGetQuery(con, "
SELECT
    AVG(Milliseconds) AS media,
    MIN(Milliseconds) AS minimo,
    MAX(Milliseconds) AS maximo,
    VARIANCE(Milliseconds) AS varianza
FROM Track;
")
resultado




# ¿Cuál es el total de ingresos generados por la tienda? ¿Y el promedio por factura?
resultado <- dbGetQuery(con, "
SELECT
SUM(Total) AS total_ingresos,
AVG(Total) AS promedio_por_factura
FROM Invoice;
")
resultado

# ¿Cuántas facturas se emitieron por año? Ordena cronológicamente.
resultado <- dbGetQuery(con, "
SELECT
STRFTIME('%Y', InvoiceDate) AS anio,
COUNT(*) AS total_facturas
FROM Invoice
GROUP BY STRFTIME('%Y', InvoiceDate)
ORDER BY anio;
")
resultado

# Identifica los 5 países con más ingresos. Muestra: país, número de facturas, 
# ingreso total y promedio por factura.
resultado <- dbGetQuery(con, "
SELECT
BillingCountry AS pais,
COUNT(InvoiceId) AS numero_facturas,
SUM(Total) AS ingreso_total,
AVG(Total) AS promedio_factura
FROM Invoice
GROUP BY BillingCountry
ORDER BY ingreso_total DESC
LIMIT 5;
")
resultado

# ¿Cuál es la desviación estándar del total de facturas? Calcula primero la varianza en SQL y luego obtén la raíz en R o Python.
resultado <- dbGetQuery(con, "
SELECT
VARIANCE(Total) AS varianza
FROM Invoice;
")
resultado
sqrt(resultado[1,1])

# Encuentra los meses con mayor y menor ingreso promedio.
resultado <- dbGetQuery(con, "
SELECT
STRFTIME('%m', InvoiceDate) AS mes,
AVG(Total) AS ingreso_promedio
FROM Invoice
GROUP BY STRFTIME('%m', InvoiceDate)
ORDER BY ingreso_promedio DESC
LIMIT 1;
")
resultado
resultado <- dbGetQuery(con, "
SELECT
STRFTIME('%m', InvoiceDate) AS mes,
AVG(Total) AS ingreso_promedio
FROM Invoice
GROUP BY STRFTIME('%m', InvoiceDate)
ORDER BY ingreso_promedio ASC
LIMIT 1;
")
resultado









