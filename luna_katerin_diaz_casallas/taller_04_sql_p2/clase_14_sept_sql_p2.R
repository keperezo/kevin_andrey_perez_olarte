################ PREVIO ###############
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

################ EJERCICIO 1 ###############
#
tabla_E <- dbGetQuery(con, "
  SELECT *
  FROM   Employee;
")

tabla_C <- dbGetQuery(con, "
  SELECT *
  FROM   Customer;
")

tabla_I <- dbGetQuery(con, "
  SELECT *
  FROM   Invoice;
")

resultado_1 <- dbGetQuery(con, "
  SELECT c.FirstName || ' ' || c.LastName   AS cliente,
         e.FirstName || ' ' || e.LastName   AS empleado,
         e.Title                             AS cargo,
         m.FirstName || ' ' || m.LastName   AS supervisor
  FROM  Customer c
  INNER JOIN Employee e ON c.SupportRepId = e.EmployeeId
  LEFT JOIN Employee m ON e.ReportsTo = m.EmployeeId;
")

resultado_2 <- dbGetQuery(con, "
  SELECT c.FirstName || ' ' || c.LastName AS cliente,
         c.SupportRepId                         AS soporte,
         ROUND(SUM(i.Total), 2)            AS gasto_total
  FROM   Customer c
  JOIN   Invoice  i ON c.CustomerId = i.CustomerId
  GROUP BY c.CustomerId
  HAVING SUM(i.Total) > (
    -- promedio de gasto de los clientes del mismo soporte
    SELECT AVG(sub_total)
    FROM (
      SELECT SUM(i2.Total) AS sub_total
      FROM   Customer c2
      JOIN   Invoice  i2 ON c2.CustomerId = i2.CustomerId
      WHERE  c2.SupportRepId = c.SupportRepId
      GROUP BY c2.CustomerId
    )
  )
  ORDER BY soporte, gasto_total DESC;
")

resultado_3 <- dbGetQuery(con, "
  -- Todos los artistas con al menos un álbum
  SELECT DISTINCT al.EmployeeId, al.FirstName || ' ' || al.LastName   AS empleado
  FROM Employee al

  EXCEPT

  -- Artistas cuyas pistas aparecen en alguna factura
  SELECT DISTINCT al.EmployeeId, al.FirstName || ' ' || al.LastName   AS empleado
  FROM   Employee      al
  JOIN   Customer      c  ON c.SupportRepId = al.EmployeeId;
")

resultado_4 <- dbGetQuery(con, "
  WITH ventas_por_genero AS (
    SELECT   g.Name             AS genero,
             SUM(il.UnitPrice * il.Quantity) AS ingresos
    FROM     InvoiceLine il
    JOIN     Track  t  ON il.TrackId  = t.TrackId
    JOIN     Genre  g  ON t.GenreId   = g.GenreId
    GROUP BY g.Name
  ),
  promedio_global AS (
    SELECT AVG(ingresos) AS media_ingresos
    FROM   ventas_por_genero
  )
  SELECT v.genero,
         ROUND(v.ingresos, 2)      AS ingresos,
         ROUND(p.media_ingresos, 2) AS media_global,
         CASE
           WHEN v.ingresos > p.media_ingresos THEN 'sobre promedio'
           ELSE 'bajo promedio'
         END AS desempeno
  FROM ventas_por_genero v, promedio_global p
  ORDER BY ingresos DESC;
")