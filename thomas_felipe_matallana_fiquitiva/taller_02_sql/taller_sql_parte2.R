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

con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

dbListTables(con)

# 1. INNER JOIN y SELF JOIN: Cliente, Empleado de soporte y Jefe (usando COALESCE)
ej1_1 <- dbGetQuery(con, "
  SELECT 
    c.FirstName || ' ' || c.LastName AS cliente,
    e.FirstName || ' ' || e.LastName AS soporte,
    COALESCE(j.FirstName || ' ' || j.LastName, 'Sin jefe') AS jefe
  FROM Customer c
  INNER JOIN Employee e ON c.SupportRepId = e.EmployeeId
  LEFT JOIN Employee j ON e.ReportsTo = j.EmployeeId;
")
print(head(ej1_1))

# 2. Subconsulta correlacionada: Clientes que gastan más que el promedio de su representante
ej1_2 <- dbGetQuery(con, "
  WITH GastoCliente AS (
    SELECT c.CustomerId, c.FirstName || ' ' || c.LastName AS cliente, c.SupportRepId, SUM(i.Total) AS gasto_total
    FROM Customer c
    JOIN Invoice i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  )
  SELECT g1.cliente, g1.gasto_total
  FROM GastoCliente g1
  WHERE g1.gasto_total > (
    SELECT AVG(g2.gasto_total)
    FROM GastoCliente g2
    WHERE g2.SupportRepId = g1.SupportRepId
      AND g2.CustomerId != g1.CustomerId
  );
")
print(head(ej1_2))

# 3. Operador EXCEPT: Empleados que nunca han sido soporte de ningún cliente
ej1_3 <- dbGetQuery(con, "
  SELECT EmployeeId, FirstName, LastName 
  FROM Employee
  EXCEPT
  SELECT e.EmployeeId, e.FirstName, e.LastName
  FROM Employee e
  INNER JOIN Customer c ON e.EmployeeId = c.SupportRepId;
")
print(ej1_3)

# 4. CTE: Clientes atendidos por empleado e ingresos generados, ordenados de mayor a menor
ej1_4 <- dbGetQuery(con, "
  WITH StatsEmpleado AS (
    SELECT 
      e.FirstName || ' ' || e.LastName AS empleado, 
      COUNT(DISTINCT c.CustomerId) AS num_clientes, 
      ROUND(SUM(i.Total), 2) AS ingresos_generados
    FROM Employee e
    JOIN Customer c ON e.EmployeeId = c.SupportRepId
    JOIN Invoice i ON c.CustomerId = i.CustomerId
    GROUP BY e.EmployeeId
  )
  SELECT * FROM StatsEmpleado ORDER BY ingresos_generados DESC;
")
print(ej1_4)

### Jane Peacock es la empleada que atiende a la mayor cantidad de clientes (21) a su misma
### es la empleada que más genera ingresos en total (833.04)


# 1. Total de ventas por año-mes con LAG, LEAD y variación porcentual
ej2_1 <- dbGetQuery(con, "
  WITH VentasMes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes, SUM(Total) AS ventas
    FROM Invoice
    GROUP BY anio_mes
  )
  SELECT 
    anio_mes, 
    ROUND(ventas, 2) AS ventas,
    ROUND(LAG(ventas) OVER (ORDER BY anio_mes), 2) AS mes_anterior,
    ROUND(LEAD(ventas) OVER (ORDER BY anio_mes), 2) AS mes_siguiente,
    ROUND((ventas - LAG(ventas) OVER (ORDER BY anio_mes)) / LAG(ventas) OVER (ORDER BY anio_mes) * 100, 2) AS variacion_pct
  FROM VentasMes;
")
print(head(ej2_1))

# 2. Ranking de ventas: Mes top y mes más bajo por año
ej2_2 <- dbGetQuery(con, "
  WITH VentasMes AS (
    SELECT SUBSTR(InvoiceDate, 1, 4) AS anio, SUBSTR(InvoiceDate, 1, 7) AS mes, SUM(Total) AS ventas
    FROM Invoice
    GROUP BY mes
  ),
  Rankings AS (
    SELECT anio, mes, ventas,
      RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS rank_mayor,
      RANK() OVER (PARTITION BY anio ORDER BY ventas ASC) AS rank_menor
    FROM VentasMes
  )
  SELECT r_mayor.anio, r_mayor.mes AS mes_top, r_menor.mes AS mes_bajo
  FROM Rankings r_mayor
  JOIN Rankings r_menor ON r_mayor.anio = r_menor.anio
  WHERE r_mayor.rank_mayor = 1 AND r_menor.rank_menor = 1;
")
print(ej2_2)

# 3. Género musical con mayores ingresos por año (encadenando CTEs y RANK)
ej2_3 <- dbGetQuery(con, "
  WITH IngresosGenero AS (
    SELECT SUBSTR(i.InvoiceDate, 1, 4) AS anio, g.Name AS genero, SUM(il.UnitPrice * il.Quantity) AS ingresos
    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Genre g ON t.GenreId = g.GenreId
    GROUP BY anio, genero
  ),
  RankingGenero AS (
    SELECT anio, genero, ROUND(ingresos, 2) AS ingresos,
      RANK() OVER(PARTITION BY anio ORDER BY ingresos DESC) as ranking
    FROM IngresosGenero
  )
  SELECT anio, genero, ingresos
  FROM RankingGenero
  WHERE ranking = 1;
")
print(ej2_3)

# 4. Análisis de cuartiles (NTILE): Ingresos de Q4 vs resto de cuartiles
ej2_4 <- dbGetQuery(con, "
  WITH GastoCliente AS (
    SELECT CustomerId, SUM(Total) AS gasto_total
    FROM Invoice
    GROUP BY CustomerId
  ),
  Cuartiles AS (
    SELECT gasto_total, NTILE(4) OVER (ORDER BY gasto_total ASC) AS cuartil
    FROM GastoCliente
  ),
  Agrupacion AS (
    SELECT 
      CASE WHEN cuartil = 4 THEN 'Q4 (Superior)' ELSE 'Q1-Q3 (Combinados)' END AS grupo,
      SUM(gasto_total) AS ingresos_grupo
    FROM Cuartiles
    GROUP BY grupo
  )
  SELECT grupo, ROUND(ingresos_grupo, 2) AS ingresos_grupo, 
         ROUND(ingresos_grupo * 100.0 / (SELECT SUM(gasto_total) FROM GastoCliente), 2) AS pct_ingreso_total
  FROM Agrupacion;
")
print(ej2_4)

# Este es el bloque CTE base que usaremos para ambas respuestas del reto
cte_segmentacion <- "
  WITH FechaReferencia AS (
    SELECT MAX(InvoiceDate) AS max_fecha FROM Invoice
  ),
  UltimaCompra AS (
    SELECT c.CustomerId, c.FirstName || ' ' || c.LastName AS cliente, MAX(i.InvoiceDate) AS fecha_ultima, SUM(i.Total) AS gasto_historico
    FROM Customer c
    JOIN Invoice i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  DiferenciaMeses AS (
    SELECT u.*, (JULIANDAY(f.max_fecha) - JULIANDAY(u.fecha_ultima)) / 30.0 AS meses_transcurridos
    FROM UltimaCompra u, FechaReferencia f
  ),
  Segmentacion AS (
    SELECT *,
      CASE
        WHEN meses_transcurridos <= 6 THEN 'activo'
        WHEN meses_transcurridos <= 12 THEN 'en riesgo'
        ELSE 'inactivo'
      END AS segmento
    FROM DiferenciaMeses
  )
"

# A. Resumen por segmento (clientes, gasto promedio y gasto total)
ej3_resumen <- dbGetQuery(con, paste0(cte_segmentacion, "
  SELECT segmento, 
         COUNT(*) AS num_clientes, 
         ROUND(AVG(gasto_historico), 2) AS gasto_promedio, 
         ROUND(SUM(gasto_historico), 2) AS gasto_total
  FROM Segmentacion 
  GROUP BY segmento
  ORDER BY gasto_total DESC;
"))
cat("\n--- Resumen por Segmento ---\n")
print(ej3_resumen)

# B. Los 3 clientes de mayor gasto histórico por segmento
ej3_ranking <- dbGetQuery(con, paste0(cte_segmentacion, "
  , RankingSegmento AS (
    SELECT segmento, cliente, ROUND(gasto_historico, 2) AS gasto_historico,
           RANK() OVER (PARTITION BY segmento ORDER BY gasto_historico DESC) AS ranking
    FROM Segmentacion
  )
  SELECT * FROM RankingSegmento 
  WHERE ranking <= 3;
"))
cat("\n--- Top 3 Clientes por Segmento ---\n")
print(ej3_ranking)

dbDisconnect(con)
cat("\nConexión cerrada.\n")


