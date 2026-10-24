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
dbListTables(con)
dbListFields(con,"Track")

####################################################
######## parte 1 #################################
###################################################

# --- 2. Ejecución de Consultas ---

# Consulta 1: JOIN y SELF JOIN con COALESCE
query_1 <- "
SELECT 
    c.FirstName || ' ' || c.LastName AS CustomerName,
    e.FirstName || ' ' || e.LastName AS SupportRepName,
    COALESCE(m.FirstName || ' ' || m.LastName, 'Sin jefe') AS ManagerName
FROM Customer c
INNER JOIN Employee e ON c.SupportRepId = e.EmployeeId
LEFT JOIN Employee m ON e.ReportsTo = m.EmployeeId;
"
resultado_1 <- dbGetQuery(con, query_1)
cat("\n--- Resultados Consulta 1: Relación Cliente-Empleado-Jefe ---\n")
print(head(resultado_1)) # Usamos head() para no saturar tu consola


# Consulta 2: Subconsulta Correlacionada
query_2 <- "
SELECT 
    c.CustomerId, 
    c.FirstName, 
    c.LastName, 
    SUM(i.Total) AS TotalGastado
FROM Customer c
JOIN Invoice i ON c.CustomerId = i.CustomerId
GROUP BY c.CustomerId, c.FirstName, c.LastName, c.SupportRepId
HAVING SUM(i.Total) > (
    SELECT AVG(GastoCliente)
    FROM (
        SELECT c2.CustomerId, SUM(i2.Total) AS GastoCliente
        FROM Customer c2
        JOIN Invoice i2 ON c2.CustomerId = i2.CustomerId
        WHERE c2.SupportRepId = c.SupportRepId 
          AND c2.CustomerId <> c.CustomerId
        GROUP BY c2.CustomerId
    ) AS Subconsulta
);
"
resultado_2 <- dbGetQuery(con, query_2)
cat("\n--- Resultados Consulta 2: Clientes con gasto superior al promedio de su representante ---\n")
print(resultado_2)


# Consulta 3: Operador EXCEPT
query_3 <- "
SELECT EmployeeId, FirstName, LastName
FROM Employee
EXCEPT
SELECT e.EmployeeId, e.FirstName, e.LastName
FROM Employee e
INNER JOIN Customer c ON e.EmployeeId = c.SupportRepId;
"
resultado_3 <- dbGetQuery(con, query_3)
cat("\n--- Resultados Consulta 3: Empleados sin clientes asignados ---\n")
print(resultado_3)


# Consulta 4: CTE (Common Table Expression)
query_4 <- "
WITH EmployeeStats AS (
    SELECT 
        e.EmployeeId,
        e.FirstName || ' ' || e.LastName AS EmployeeName,
        COUNT(DISTINCT c.CustomerId) AS TotalCustomers,
        SUM(i.Total) AS TotalRevenue
    FROM Employee e
    INNER JOIN Customer c ON e.EmployeeId = c.SupportRepId
    INNER JOIN Invoice i ON c.CustomerId = i.CustomerId
    GROUP BY e.EmployeeId, e.FirstName, e.LastName
)
SELECT 
    EmployeeName, 
    TotalCustomers, 
    TotalRevenue
FROM EmployeeStats
ORDER BY TotalRevenue DESC;
"
resultado_4 <- dbGetQuery(con, query_4)
cat("\n--- Resultados Consulta 4: Rendimiento por empleado ---\n")
print(resultado_4)

####################################################
######## parte 2 #################################
###################################################


# Consulta 1: Ventas por mes con LAG, LEAD y Variación Porcentual
query_1 <- "
WITH VentasMensuales AS (
    SELECT 
        SUBSTR(InvoiceDate, 1, 7) AS MesAnio,
        SUM(Total) AS VentasActuales
    FROM Invoice
    GROUP BY SUBSTR(InvoiceDate, 1, 7)
)
SELECT 
    MesAnio,
    VentasActuales,
    LAG(VentasActuales) OVER (ORDER BY MesAnio) AS VentasAnteriores,
    LEAD(VentasActuales) OVER (ORDER BY MesAnio) AS VentasSiguientes,
    ((VentasActuales - LAG(VentasActuales) OVER (ORDER BY MesAnio)) / 
      LAG(VentasActuales) OVER (ORDER BY MesAnio)) * 100.0 AS VariacionPorcentual
FROM VentasMensuales;
"
resultado_1 <- dbGetQuery(con, query_1)
cat("\n--- Resultados Consulta 1: Variación Porcentual Mensual ---\n")
print(head(resultado_1, 10)) # Mostramos los primeros 10 registros


# Consulta 2: Mes de mayor y menor venta por año usando RANK()
query_2 <- "
WITH VentasMensuales AS (
    SELECT 
        SUBSTR(InvoiceDate, 1, 4) AS Anio,
        SUBSTR(InvoiceDate, 6, 2) AS Mes,
        SUM(Total) AS Ventas
    FROM Invoice
    GROUP BY SUBSTR(InvoiceDate, 1, 4), SUBSTR(InvoiceDate, 6, 2)
),
Rankings AS (
    SELECT 
        Anio,
        Mes,
        Ventas,
        RANK() OVER (PARTITION BY Anio ORDER BY Ventas DESC) AS RankTop,
        RANK() OVER (PARTITION BY Anio ORDER BY Ventas ASC) AS RankBottom
    FROM VentasMensuales
)
SELECT 
    t.Anio,
    t.Mes AS MesMayorVenta,
    b.Mes AS MesMenorVenta
FROM Rankings t
JOIN Rankings b ON t.Anio = b.Anio
WHERE t.RankTop = 1 AND b.RankBottom = 1;
"
resultado_2 <- dbGetQuery(con, query_2)
cat("\n--- Resultados Consulta 2: Mes de mayor y menor venta por año ---\n")
print(resultado_2)


# Consulta 3: Género musical con mayores ingresos por año (CTEs encadenadas)
query_3 <- "
WITH VentasGeneroAnio AS (
    SELECT 
        SUBSTR(i.InvoiceDate, 1, 4) AS Anio,
        g.Name AS Genero,
        SUM(il.UnitPrice * il.Quantity) AS Ingresos
    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Genre g ON t.GenreId = g.GenreId
    GROUP BY SUBSTR(i.InvoiceDate, 1, 4), g.Name
),
RankingGeneros AS (
    SELECT 
        Anio,
        Genero,
        Ingresos,
        RANK() OVER (PARTITION BY Anio ORDER BY Ingresos DESC) AS RankIngresos
    FROM VentasGeneroAnio
)
SELECT Anio, Genero AS GeneroTop, Ingresos
FROM RankingGeneros
WHERE RankIngresos = 1;
"
resultado_3 <- dbGetQuery(con, query_3)
cat("\n--- Resultados Consulta 3: Género top por año ---\n")
print(resultado_3)


# Consulta 4: NTILE(4) para porcentaje de ingresos del Cuartil 4 vs Resto
query_4 <- "
WITH GastoCliente AS (
    SELECT 
        CustomerId,
        SUM(Total) AS GastoTotal
    FROM Invoice
    GROUP BY CustomerId
),
Cuartiles AS (
    SELECT 
        CustomerId,
        GastoTotal,
        NTILE(4) OVER (ORDER BY GastoTotal ASC) AS Cuartil
    FROM GastoCliente
),
AgregadoCuartil AS (
    SELECT 
        CASE 
            WHEN Cuartil = 4 THEN 'Q4 (Cuartil Superior)' 
            ELSE 'Q1 + Q2 + Q3 (Resto)' 
        END AS Grupo,
        SUM(GastoTotal) AS IngresoGrupo
    FROM Cuartiles
    GROUP BY CASE WHEN Cuartil = 4 THEN 'Q4 (Cuartil Superior)' ELSE 'Q1 + Q2 + Q3 (Resto)' END
)
SELECT 
    Grupo,
    IngresoGrupo,
    (IngresoGrupo / (SELECT SUM(IngresoGrupo) FROM AgregadoCuartil)) * 100.0 AS PorcentajeDelTotal
FROM AgregadoCuartil;
"
resultado_4 <- dbGetQuery(con, query_4)
cat("\n--- Resultados Consulta 4: Ingresos Q4 vs Resto ---\n")
print(resultado_4)

####################################################
######## parte opcional #################################
###################################################