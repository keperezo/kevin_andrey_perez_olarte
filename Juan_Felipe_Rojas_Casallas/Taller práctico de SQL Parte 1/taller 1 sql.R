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

# 1. ¿Cuántas pistas hay en total en la base de datos?

Total_pistas <- "
SELECT COUNT(*) AS total_pistas
FROM Track;
"

dbGetQuery(con, Total_pistas)


# 2. ¿Cuántos géneros distintos existen?

Total_generos <- "
SELECT COUNT(DISTINCT GenreId) AS total_generos
FROM Track;
"

dbGetQuery(con, Total_generos)


# 3. Obtener el nombre del género, número de pistas y duración promedio en minutos,
#    ordenado de mayor a menor número de pistas.

Pistas_por_genero <- "
SELECT 
    g.Name AS genero,
    COUNT(t.TrackId) AS numero_pistas,
    ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS duracion_promedio_minutos
FROM Genre AS g
LEFT JOIN Track AS t
    ON g.GenreId = t.GenreId
GROUP BY g.GenreId, g.Name
ORDER BY numero_pistas DESC;
"

dbGetQuery(con, Pistas_por_genero)


# 4. ¿Cuáles son las 5 pistas más largas?
#    Mostrar nombre de la pista, álbum y duración en minutos.

Cinco_pistas_mas_largas <- "
SELECT 
    t.Name AS pista,
    a.Title AS album,
    ROUND(t.Milliseconds / 60000.0, 2) AS duracion_minutos
FROM Track AS t
INNER JOIN Album AS a
    ON t.AlbumId = a.AlbumId
ORDER BY t.Milliseconds DESC
LIMIT 5;
"

dbGetQuery(con, Cinco_pistas_mas_largas)


# 5. ¿Cuántas pistas tienen un UnitPrice superior a $0.99
#    y qué porcentaje representan del total?

Pistas_mayor_099 <- "
SELECT
    SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END) AS pistas_mayor_099,
    ROUND(
        100.0 * SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END)
        / COUNT(*),
        2
    ) AS porcentaje_total
FROM Track;
"

dbGetQuery(con, Pistas_mayor_099)


# 6. Calcular la media, mínimo, máximo y varianza de la duración
#    en milisegundos de todas las pistas.

Estadisticas_duracion <- "
SELECT
    AVG(Milliseconds) AS media_milisegundos,
    MIN(Milliseconds) AS minimo_milisegundos,
    MAX(Milliseconds) AS maximo_milisegundos,
    AVG(Milliseconds * 1.0 * Milliseconds)
        - AVG(Milliseconds) * AVG(Milliseconds) AS varianza_milisegundos
FROM Track;
"

dbGetQuery(con, Estadisticas_duracion)



############################## PARTE 2 ##################################


# 1. ¿Cuál es el total de ingresos generados por la tienda?
#    ¿Y cuál es el promedio de ingresos por factura?

Ingresos_tienda <- "
SELECT
    SUM(Total) AS ingresos_totales,
    AVG(Total) AS promedio_por_factura
FROM Invoice;
"

dbGetQuery(con, Ingresos_tienda)


# 2. ¿Cuántas facturas se emitieron por año?
#    Ordenadas cronológicamente.

Facturas_por_anio <- "
SELECT
    strftime('%Y', InvoiceDate) AS anio,
    COUNT(*) AS numero_facturas
FROM Invoice
GROUP BY anio
ORDER BY anio ASC;
"

dbGetQuery(con, Facturas_por_anio)


# 3. Identificar los 5 países con más ingresos.
#    Mostrar país, número de facturas, ingreso total
#    y promedio por factura.

Cinco_paises_mas_ingresos <- "
SELECT
    BillingCountry AS pais,
    COUNT(*) AS numero_facturas,
    SUM(Total) AS ingreso_total,
    AVG(Total) AS promedio_por_factura
FROM Invoice
GROUP BY BillingCountry
ORDER BY ingreso_total DESC
LIMIT 5;
"

dbGetQuery(con, Cinco_paises_mas_ingresos)


# 4. Calcular la varianza del total de las facturas en SQL.
#    Posteriormente, obtener la desviación estándar en R.

Varianza_facturas <- "
SELECT
    AVG(Total * 1.0 * Total)
        - AVG(Total) * AVG(Total) AS varianza_total_facturas
FROM Invoice;
"

resultado_varianza <- dbGetQuery(con, Varianza_facturas)

resultado_varianza

# Raíz cuadrada de la varianza = desviación estándar
desviacion_estandar <- sqrt(resultado_varianza$varianza_total_facturas)

desviacion_estandar


# 5. Encontrar los meses con mayor y menor ingreso promedio.

Ingresos_promedio_mes <- "
SELECT
    strftime('%m', InvoiceDate) AS mes,
    AVG(Total) AS ingreso_promedio
FROM Invoice
GROUP BY mes
ORDER BY ingreso_promedio DESC;
"

dbGetQuery(con, Ingresos_promedio_mes)















