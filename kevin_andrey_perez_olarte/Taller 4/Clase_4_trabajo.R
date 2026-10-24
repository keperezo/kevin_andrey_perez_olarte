# EJERCICIO 1: ANÁLISIS DESCRIPTIVO DE LA COLECCIÓN MUSICAL

# 1. ¿Cuántas pistas hay?
dbGetQuery(con,"
SELECT COUNT(*) AS total_pistas
FROM Track;")

# 2. ¿Cuántos géneros distintos existen?
dbGetQuery(con,"
SELECT COUNT(DISTINCT GenreId) AS generos_distintos
FROM Track;")

# 3. Género, número de pistas y duración promedio
dbGetQuery(con,"
SELECT g.Name AS genero,
COUNT(t.TrackId) AS n_pistas,
ROUND(AVG(t.Milliseconds)/60000.0,2) AS duracion_promedio_min
FROM Track t
JOIN Genre g ON t.GenreId=g.GenreId
GROUP BY g.Name
ORDER BY n_pistas DESC;")

# 4. Las 5 pistas más largas
dbGetQuery(con,"
SELECT t.Name AS pista,
a.Title AS album,
ROUND(t.Milliseconds/60000.0,2) AS duracion_min
FROM Track t
JOIN Album a ON t.AlbumId=a.AlbumId
ORDER BY t.Milliseconds DESC
LIMIT 5;")

# 5. Pistas con UnitPrice > 0.99 y porcentaje
dbGetQuery(con,"
SELECT COUNT(*) AS pistas_mayor_099,
ROUND(100.0*COUNT(*)/(SELECT COUNT(*) FROM Track),2) AS porcentaje
FROM Track
WHERE UnitPrice>0.99;")

# 6. Media, mínimo, máximo y varianza de duración
dbGetQuery(con,"
SELECT
ROUND(AVG(Milliseconds),2) AS media,
MIN(Milliseconds) AS minimo,
MAX(Milliseconds) AS maximo,
ROUND(AVG(Milliseconds*Milliseconds)-AVG(Milliseconds)*AVG(Milliseconds),2) AS varianza
FROM Track;")


# EJERCICIO 2: ANÁLISIS DESCRIPTIVO DE VENTAS

# 1. Total de ingresos y promedio por factura
dbGetQuery(con,"
SELECT
ROUND(SUM(il.UnitPrice*il.Quantity),2) AS ingresos_totales,
ROUND(SUM(il.UnitPrice*il.Quantity)/COUNT(DISTINCT i.InvoiceId),2) AS promedio_factura
FROM Invoice i
JOIN InvoiceLine il ON i.InvoiceId=il.InvoiceId;")

# 2. Número de facturas por año
dbGetQuery(con,"
SELECT SUBSTR(InvoiceDate,1,4) AS anho,
COUNT(*) AS n_facturas
FROM Invoice
GROUP BY anho
ORDER BY anho;")

# 3. Los 5 países con más ingresos
dbGetQuery(con,"
SELECT
i.BillingCountry AS pais,
COUNT(DISTINCT i.InvoiceId) AS n_facturas,
ROUND(SUM(il.UnitPrice*il.Quantity),2) AS ingreso_total,
ROUND(SUM(il.UnitPrice*il.Quantity)/COUNT(DISTINCT i.InvoiceId),2) AS promedio_factura
FROM Invoice i
JOIN InvoiceLine il ON i.InvoiceId=il.InvoiceId
GROUP BY i.BillingCountry
ORDER BY ingreso_total DESC
LIMIT 5;")

# 4. Varianza y desviación estándar del total de facturas
varianza <- dbGetQuery(con,"
SELECT AVG(Total*Total)-AVG(Total)*AVG(Total) AS varianza
FROM Invoice;")
varianza
sqrt(varianza$varianza)

# 5. Meses con mayor y menor ingreso promedio
dbGetQuery(con,"
SELECT
SUBSTR(InvoiceDate,6,2) AS mes,
ROUND(AVG(Total),2) AS ingreso_promedio
FROM Invoice
GROUP BY mes
ORDER BY ingreso_promedio DESC;")