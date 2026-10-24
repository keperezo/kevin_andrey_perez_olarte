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


# Ejercicio 1

p1.1 <-  dbGetQuery(con, "
  SELECT 
      c.FirstName AS NombreCliente,
      c.LastName AS ApellidoCliente,
      e.FirstName AS NombreEmpleado,
      e.LastName AS ApellidoEmpleado,
      COALESCE(jefe.FirstName, 'Sin jefe') AS NombreJefe,
      COALESCE(jefe.LastName, 'Sin jefe') AS ApellidoJefe
  FROM Customer c
  INNER JOIN Employee e 
      ON c.SupportRepId = e.EmployeeId
  LEFT JOIN Employee jefe 
      ON e.ReportsTo = jefe.EmployeeId;
                   ")

p1.1


p1.2 <-  dbGetQuery(con, "
                    SELECT 
    c.CustomerId,
    c.FirstName,
    c.LastName,
    c.SupportRepId,
    ROUND(SUM(i.Total), 2) AS GastoTotal
FROM Customer c
JOIN Invoice i 
    ON c.CustomerId = i.CustomerId
GROUP BY c.CustomerId, c.FirstName, c.LastName, c.SupportRepId
HAVING SUM(i.Total) > (
    SELECT AVG(sub_total)
    FROM (
        SELECT SUM(i2.Total) AS sub_total
        FROM Customer c2
        JOIN Invoice i2 
            ON c2.CustomerId = i2.CustomerId
        WHERE c2.SupportRepId = c.SupportRepId
          AND c2.CustomerId <> c.CustomerId
        GROUP BY c2.CustomerId
    )
);
                    ")

p1.2


p1.3 <-  dbGetQuery(con, "
                    SELECT 
    EmployeeId,
    FirstName,
    LastName
FROM Employee

EXCEPT

SELECT DISTINCT 
    e.EmployeeId,
    e.FirstName,
    e.LastName
FROM Employee e
JOIN Customer c 
    ON e.EmployeeId = c.SupportRepId;
 
                    ")

p1.3


p1.4 <-dbGetQuery(con, "
                  WITH ResumenSoporte AS (
    SELECT 
        e.EmployeeId,
        e.FirstName,
        e.LastName,
        COUNT(DISTINCT c.CustomerId) AS CantidadClientes,
        SUM(i.Total) AS IngresoTotal
    FROM Employee e
    JOIN Customer c 
        ON e.EmployeeId = c.SupportRepId
    JOIN Invoice i 
        ON c.CustomerId = i.CustomerId
    GROUP BY e.EmployeeId, e.FirstName, e.LastName
)
SELECT 
    EmployeeId,
    FirstName,
    LastName,
    CantidadClientes,
    ROUND(IngresoTotal, 2) AS IngresoTotal
FROM ResumenSoporte
ORDER BY IngresoTotal DESC;
                  ") 

p1.4


# Ejercicio 2

p2.1 <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
      ROUND(SUM(Total), 2) AS ventas
    FROM Invoice
    GROUP BY anio_mes
  )
  SELECT 
    anio_mes,
    ventas,
    LAG(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_anterior,
    LEAD(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_siguiente,
    ROUND(
      100.0 * (ventas - LAG(ventas, 1) OVER (ORDER BY anio_mes)) / 
      LAG(ventas, 1) OVER (ORDER BY anio_mes), 
      2
    ) AS variacion_porcentual
  FROM ventas_mes
  ORDER BY anio_mes;
")

p2.1



p2.2 <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 4) AS anio,
      SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
      ROUND(SUM(Total), 2) AS ventas
    FROM Invoice
    GROUP BY anio_mes
  ),
  rankings AS (
    SELECT 
      anio,
      anio_mes,
      ventas,
      RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS rk_top,
      RANK() OVER (PARTITION BY anio ORDER BY ventas ASC) AS rk_low
    FROM ventas_mes
  )
  SELECT 
    anio,
    MAX(CASE WHEN rk_top = 1 THEN anio_mes END) AS mes_top,
    MAX(CASE WHEN rk_top = 1 THEN ventas END) AS ventas_top,
    MAX(CASE WHEN rk_low = 1 THEN anio_mes END) AS mes_bajo,
    MAX(CASE WHEN rk_low = 1 THEN ventas END) AS ventas_bajo
  FROM rankings
  GROUP BY anio
  ORDER BY anio;
")

p2.2


p2.3 <- dbGetQuery(con, "
  WITH ingresos_genero_anio AS (
    SELECT 
      SUBSTR(i.InvoiceDate, 1, 4) AS anio,
      g.Name AS genero,
      ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS ingresos
    FROM Invoice AS i
    INNER JOIN InvoiceLine AS il ON i.InvoiceId = il.InvoiceId
    INNER JOIN Track AS t ON il.TrackId = t.TrackId
    INNER JOIN Genre AS g ON t.GenreId = g.GenreId
    GROUP BY anio, genero
  ),
  ranking_generos AS (
    SELECT 
      anio,
      genero,
      ingresos,
      RANK() OVER (PARTITION BY anio ORDER BY ingresos DESC) AS rk
    FROM ingresos_genero_anio
  )
  SELECT 
    anio,
    genero,
    ingresos
  FROM ranking_generos
  WHERE rk = 1
  ORDER BY anio;
")

p2.3


p2.4 <- dbGetQuery(con, "
  WITH gasto_cliente AS (
    SELECT 
      c.CustomerId,
      ROUND(SUM(i.Total), 2) AS gasto_total
    FROM Customer AS c
    INNER JOIN Invoice AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  cuartiles AS (
    SELECT 
      CustomerId,
      gasto_total,
      NTILE(4) OVER (ORDER BY gasto_total ASC) AS cuartil
    FROM gasto_cliente
  ),
  resumen_cuartil AS (
    SELECT 
      cuartil,
      SUM(gasto_total) AS total_cuartil
    FROM cuartiles
    GROUP BY cuartil
  ),
  comparativa AS (
    SELECT 
      SUM(CASE WHEN cuartil = 4 THEN total_cuartil ELSE 0 END) AS ingreso_q4,
      SUM(CASE WHEN cuartil < 4 THEN total_cuartil ELSE 0 END) AS ingreso_q1_q3,
      SUM(total_cuartil) AS ingreso_total_tienda
    FROM resumen_cuartil
  )
  SELECT 
    ROUND(ingreso_q4, 2) AS ingreso_q4_superior,
    ROUND(ingreso_q1_q3, 2) AS ingreso_q1_q3_resto,
    ROUND(100.0 * ingreso_q4 / ingreso_total_tienda, 2) AS pct_q4_superior,
    ROUND(100.0 * ingreso_q1_q3 / ingreso_total_tienda, 2) AS pct_q1_q3_resto
  FROM comparativa;
")

p2.4


