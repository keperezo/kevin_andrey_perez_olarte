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

# Ejercicio 1

## Punto a.

resultado <- dbGetQuery(con, "
SELECT
    c.FirstName || ' ' || c.LastName                        AS cliente,
    e.FirstName || ' ' || e.LastName                        AS empleado_soporte,
    COALESCE(m.FirstName || ' ' || m.LastName, 'Sin jefe')  AS jefe_del_empleado
FROM   Customer  c
INNER JOIN Employee e ON c.SupportRepId = e.EmployeeId
LEFT JOIN  Employee m ON e.ReportsTo    = m.EmployeeId
ORDER BY empleado_soporte, cliente;
")
resultado

## Punto b.
resultado <- dbGetQuery(con, "
SELECT
    c.FirstName || ' ' || c.LastName  AS cliente,
    e.FirstName || ' ' || e.LastName  AS empleado_soporte,
    ROUND(SUM(i.Total), 2)            AS gasto_total
FROM   Customer c
JOIN   Invoice  i ON c.CustomerId   = i.CustomerId
JOIN   Employee e ON c.SupportRepId = e.EmployeeId
GROUP BY c.CustomerId
HAVING SUM(i.Total) > (
    -- promedio de gasto de los OTROS clientes del mismo representante
    SELECT AVG(sub.sub_total)
    FROM (
        SELECT SUM(i2.Total) AS sub_total
        FROM   Customer c2
        JOIN   Invoice  i2 ON c2.CustomerId = i2.CustomerId
        WHERE  c2.SupportRepId = c.SupportRepId   -- correlación con la fila externa
          AND  c2.CustomerId  <> c.CustomerId     -- excluye al propio cliente
        GROUP BY c2.CustomerId
    ) sub
)
ORDER BY empleado_soporte, gasto_total DESC;
  
")
resultado

## Punto c.
resultado <- dbGetQuery(con, "
  SELECT EmployeeId, FirstName, LastName, Title
FROM   Employee

EXCEPT

SELECT EmployeeId, FirstName, LastName, Title
FROM   Employee
WHERE  EmployeeId IN (SELECT SupportRepId FROM Customer WHERE SupportRepId IS NOT NULL);
")
resultado

## Punto d.
resultado <- dbGetQuery(con, "
  WITH carga_empleados AS (
    SELECT
        e.EmployeeId,
        e.FirstName || ' ' || e.LastName  AS empleado,
        COUNT(DISTINCT c.CustomerId)      AS n_clientes,
        ROUND(SUM(i.Total), 2)            AS ingreso_total
    FROM   Employee e
    JOIN   Customer c ON c.SupportRepId = e.EmployeeId
    JOIN   Invoice  i ON i.CustomerId   = c.CustomerId
    GROUP BY e.EmployeeId
)
SELECT *
FROM   carga_empleados
ORDER BY ingreso_total DESC;
")
resultado

# Ejercicio 2
## Punto a
resultado <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
           ROUND(SUM(Total), 2)      AS ventas
    FROM   Invoice
    GROUP BY anio_mes
)
SELECT
    anio_mes,
    ventas,
    LAG(ventas, 1)  OVER (ORDER BY anio_mes) AS ventas_mes_anterior,
    LEAD(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_siguiente,
    ROUND(
      100.0 * (ventas - LAG(ventas, 1) OVER (ORDER BY anio_mes))
            / LAG(ventas, 1) OVER (ORDER BY anio_mes)
    , 2) AS variacion_pct_vs_mes_anterior
FROM   ventas_mes
ORDER BY anio_mes;
")
resultado

## Punto b
resultado <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 4)  AS anio,
           SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
           ROUND(SUM(Total), 2)      AS ventas
    FROM   Invoice
    GROUP BY anio_mes
),
rankeado AS (
    SELECT
        anio, anio_mes, ventas,
        RANK() OVER (PARTITION BY anio ORDER BY ventas DESC, anio_mes) AS rank_max,
        RANK() OVER (PARTITION BY anio ORDER BY ventas ASC,  anio_mes) AS rank_min
    FROM ventas_mes
)
SELECT
    top.anio,
    top.anio_mes AS mes_top,          top.ventas AS ventas_mes_top,
    low.anio_mes AS mes_mas_bajo,     low.ventas AS ventas_mes_mas_bajo
FROM   (SELECT * FROM rankeado WHERE rank_max = 1) top
JOIN   (SELECT * FROM rankeado WHERE rank_min = 1) low ON top.anio = low.anio
ORDER BY top.anio;
")
resultado

## Punto c
resultado <- dbGetQuery(con, "
  WITH ingresos_genero_anio AS (
    SELECT
        SUBSTR(i.InvoiceDate, 1, 4)               AS anio,
        g.Name                                    AS genero,
        ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS ingresos
    FROM   InvoiceLine il
    JOIN   Invoice i ON il.InvoiceId = i.InvoiceId
    JOIN   Track   t ON il.TrackId   = t.TrackId
    JOIN   Genre   g ON t.GenreId    = g.GenreId
    GROUP BY anio, g.GenreId
),
ranking_genero AS (
    SELECT
        anio, genero, ingresos,
        RANK() OVER (PARTITION BY anio ORDER BY ingresos DESC) AS posicion
    FROM ingresos_genero_anio
)
SELECT anio, genero, ingresos
FROM   ranking_genero
WHERE  posicion = 1
ORDER BY anio;
")
resultado

## Punto d
resultado <- dbGetQuery(con, "
  WITH gasto_cliente AS (
    SELECT c.CustomerId, ROUND(SUM(i.Total), 2) AS gasto_total
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
cuartiles AS (
    SELECT
        CustomerId, gasto_total,
        NTILE(4) OVER (ORDER BY gasto_total) AS cuartil
    FROM gasto_cliente
),
resumen_cuartil AS (
    SELECT
        cuartil,
        COUNT(*)                  AS n_clientes,
        ROUND(SUM(gasto_total),2) AS ingreso_cuartil
    FROM cuartiles
    GROUP BY cuartil
)
SELECT
    CASE WHEN cuartil = 4 THEN 'Q4 (superior)' ELSE 'Q1-Q3 (resto)' END AS grupo,
    SUM(n_clientes)                                                    AS n_clientes,
    ROUND(SUM(ingreso_cuartil), 2)                                     AS ingreso,
    ROUND(100.0 * SUM(ingreso_cuartil)
          / (SELECT SUM(ingreso_cuartil) FROM resumen_cuartil), 2)     AS pct_del_total
FROM   resumen_cuartil
GROUP BY grupo;
")
resultado

# Ejercicio 3
## Punto a.
resultado <- dbGetQuery(con, "
  SELECT MAX(InvoiceDate) AS fecha_maxima
FROM   Invoice;
")
resultado

## Punto b y c.
resultado <- dbGetQuery(con, "
WITH fecha_referencia AS (
    SELECT MAX(InvoiceDate) AS fecha_maxima
    FROM   Invoice
),
ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        MAX(i.InvoiceDate)               AS ultima_compra,
        ROUND(SUM(i.Total), 2)           AS gasto_total,
        (JULIANDAY((SELECT fecha_maxima FROM fecha_referencia))
           - JULIANDAY(MAX(i.InvoiceDate))) / 30.0 AS meses_sin_comprar
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
clientes_segmentados AS (
    SELECT
        *,
        CASE
            WHEN meses_sin_comprar <= 6  THEN 'activo'
            WHEN meses_sin_comprar <= 12 THEN 'en riesgo'
            ELSE 'inactivo'
        END AS segmento
    FROM ultima_compra_cliente
)
SELECT cliente, ultima_compra, ROUND(meses_sin_comprar, 1) AS meses_sin_comprar,
       gasto_total, segmento
FROM   clientes_segmentados
ORDER BY meses_sin_comprar;
  
")
resultado

## Punto d.
### Parte A
resultado <- dbGetQuery(con, "
  WITH fecha_referencia AS (
    SELECT MAX(InvoiceDate) AS fecha_maxima FROM Invoice
),
ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        ROUND(SUM(i.Total), 2)           AS gasto_total,
        (JULIANDAY((SELECT fecha_maxima FROM fecha_referencia))
           - JULIANDAY(MAX(i.InvoiceDate))) / 30.0 AS meses_sin_comprar
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
clientes_segmentados AS (
    SELECT *,
        CASE
            WHEN meses_sin_comprar <= 6  THEN 'activo'
            WHEN meses_sin_comprar <= 12 THEN 'en riesgo'
            ELSE 'inactivo'
        END AS segmento
    FROM ultima_compra_cliente
)
SELECT
    segmento,
    COUNT(*)                   AS n_clientes,
    ROUND(AVG(gasto_total), 2) AS gasto_promedio,
    ROUND(SUM(gasto_total), 2) AS gasto_total_segmento
FROM   clientes_segmentados
GROUP BY segmento
ORDER BY gasto_total_segmento DESC;
")
resultado

### Parte B
resultado <- dbGetQuery(con, "
WITH fecha_referencia AS (
    SELECT MAX(InvoiceDate) AS fecha_maxima FROM Invoice
),
ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        ROUND(SUM(i.Total), 2)           AS gasto_total,
        (JULIANDAY((SELECT fecha_maxima FROM fecha_referencia))
           - JULIANDAY(MAX(i.InvoiceDate))) / 30.0 AS meses_sin_comprar
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
clientes_segmentados AS (
    SELECT *,
        CASE
            WHEN meses_sin_comprar <= 6  THEN 'activo'
            WHEN meses_sin_comprar <= 12 THEN 'en riesgo'
            ELSE 'inactivo'
        END AS segmento
    FROM ultima_compra_cliente
),
top_clientes_segmento AS (
    SELECT
        segmento, cliente, gasto_total,
        RANK() OVER (PARTITION BY segmento ORDER BY gasto_total DESC) AS puesto
    FROM clientes_segmentados
)
SELECT *
FROM   top_clientes_segmento
WHERE  puesto <= 3
ORDER BY segmento, puesto;
  
")
resultado

