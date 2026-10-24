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

# Columnas de la tabla Track
dbListFields(con, "Track")
###########1. TOTAL DE PISTAS###########
total_pistas <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_pistas
  FROM Track;
")


###########2. ¿Cuántos géneros distintos existen?###########
Generos_distintos <- dbGetQuery(con,"
                                SELECT COUNT(*) AS total_generos
                                FROM genre;
                                ")
###########3. Obtén el nombre del género, el número de pistas y la duración promedio (en minutos) para cada género. Ordena de mayor a menor número de pistas.            
punto13 <-dbGetQuery(con, "
  SELECT 
    g.Name AS genero,
    COUNT(t.TrackId) AS numero_pistas,
    AVG(t.Milliseconds) / 60000.0 AS duracion_promedio_minutos
  FROM Genre AS g
  INNER JOIN Track AS t 
    ON g.GenreId = t.GenreId
  GROUP BY g.GenreId, g.Name
  ORDER BY numero_pistas DESC;
")
########### 4. ¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración en minutos.###########

punto14 <- dbGetQuery(con, "
  SELECT 
    t.Name AS nombre,
    a.Title AS album,
    t.Milliseconds / 60000.0 AS duracion_en_minutos
  FROM Track AS t
  INNER JOIN Album AS a
    ON t.AlbumId = a.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT 5;
")

punto14


#####Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje representan del total?#######

punto15 <- dbGetQuery(con, "
  SELECT 
    COUNT(*) AS cantidad_pistas,
    100.0 * COUNT(*) / (SELECT COUNT(*) FROM Track) AS porcentaje
  FROM Track
  WHERE UnitPrice > 0.99;
")

#### Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en milisegundos de todas las pistas.###

punto16 <- dbGetQuery(con, "
  SELECT 
    AVG(Milliseconds) AS media,
    MIN(Milliseconds) AS minimo,
    MAX(Milliseconds) AS maximo,
    AVG(Milliseconds * Milliseconds) - AVG(Milliseconds) * AVG(Milliseconds) AS varianza
  FROM Track;
")

punto16



####################EJERCICIO 2 #####################
####¿Cuál es el total de ingresos generados por la tienda? ¿Y el promedio por factura?###

punto21 <- dbGetQuery(con, "
  SELECT 
    SUM(Total) AS ingresos_totales,
    AVG(Total) AS promedio_por_factura
  FROM Invoice;
")

punto21

############# ¿Cuántas facturas se emitieron por año? Ordena cronológicamente.##

punto22 <- dbGetQuery(con, "
  SELECT 
    strftime('%Y', InvoiceDate) AS anio,
    COUNT(InvoiceId) AS numero_facturas
  FROM Invoice
  GROUP BY anio
  ORDER BY anio ASC;
")

punto22



#######3. Identifica los 5 países con más ingresos. Muestra: país, número de facturas, ingreso total y promedio por factura.#########
punto23 <- dbGetQuery(con, "
  SELECT 
    BillingCountry AS pais,
    COUNT(InvoiceId) AS numero_facturas,
    SUM(Total) AS ingreso_total,
    AVG(Total) AS promedio_por_factura
  FROM Invoice
  GROUP BY BillingCountry
  ORDER BY ingreso_total DESC
  LIMIT 5;
")

punto23
#####4. ¿Cuál es la desviación estándar del total de facturas? Calcula primero la varianza en SQL y luego obtén la raíz en R o Python.####3

varianza_invoice <- dbGetQuery(con, "
  SELECT 
    AVG(Total * Total) - AVG(Total) * AVG(Total) AS varianza
  FROM Invoice;
")

varianza_invoice


desviacion_estandar <- sqrt(varianza_invoice$varianza)
desviacion_estandar
####5. Encuentra los meses con mayor y menor ingreso promedio. ######

ingresos_por_mes <- dbGetQuery(con, "
  SELECT 
    strftime('%m', InvoiceDate) AS mes,
    AVG(Total) AS ingreso_promedio
  FROM Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio DESC;
")

ingresos_por_mes

ingresos_por_mes[which.max(ingresos_por_mes$ingreso_promedio), ]

ingresos_por_mes[which.min(ingresos_por_mes$ingreso_promedio), ]




