library(DBI)
library(RSQLite)
library(dplyr)

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

# Listamos las tablas disponibles
dbListTables(con)
dbListFields(con, "Track") # Columnas de la tabla Track

# Punto 1
# 
resultado1 <- dbGetQuery(con, "
SELECT 
    c.FirstName || ' ' || c.LastName AS Cliente,
    emp.FirstName || ' ' || emp.LastName AS EmpleadoSoporte,
    COALESCE(jefe.FirstName || ' ' || jefe.LastName, 'Sin jefe') AS Jefe
FROM Customer c
INNER JOIN Employee emp 
    ON c.SupportRepId = emp.EmployeeId
LEFT JOIN Employee jefe 
    ON emp.ReportsTo = jefe.EmployeeId
ORDER BY Cliente;
")

# Punto 2
resultado2 <- dbGetQuery(con, "
SELECT 
    c.CustomerId,
    c.FirstName || ' ' || c.LastName AS Cliente,
    c.SupportRepId,
    (
        SELECT SUM(i.Total) 
        FROM Invoice i 
        WHERE i.CustomerId = c.CustomerId
    ) AS GastoTotalCliente
FROM Customer c
WHERE (
        SELECT SUM(i.Total) 
        FROM Invoice i 
        WHERE i.CustomerId = c.CustomerId
      )
      >
      (
        SELECT AVG(GastoOtroCliente)
        FROM (
            SELECT SUM(i2.Total) AS GastoOtroCliente
            FROM Customer c2
            INNER JOIN Invoice i2 ON i2.CustomerId = c2.CustomerId
            WHERE c2.SupportRepId = c.SupportRepId   -- correlación: mismo representante
              AND c2.CustomerId <> c.CustomerId       -- excluye al propio cliente
            GROUP BY c2.CustomerId
        ) AS GastosPorOtroCliente
      )
ORDER BY c.SupportRepId, GastoTotalCliente DESC;
")

# Punto 3
resultado3 <- dbGetQuery(con, "
SELECT EmployeeId, FirstName || ' ' || LastName AS Empleado
FROM Employee

EXCEPT

SELECT SupportRepId, ''
FROM Customer
WHERE SupportRepId IS NOT NULL
ORDER BY EmployeeId;
")

# Punto 4
resultado4 <- dbGetQuery(con, "
WITH IngresosPorEmpleado AS (
    SELECT 
        e.EmployeeId,
        e.FirstName || ' ' || e.LastName AS Empleado,
        COUNT(DISTINCT c.CustomerId) AS NumClientes,
        SUM(i.Total) AS IngresoTotal
    FROM Employee e
    INNER JOIN Customer c ON c.SupportRepId = e.EmployeeId
    INNER JOIN Invoice i ON i.CustomerId = c.CustomerId
    GROUP BY e.EmployeeId, e.FirstName, e.LastName
)
SELECT *
FROM IngresosPorEmpleado
ORDER BY IngresoTotal DESC;
")
# Ejercicio 2

#Punto 1

p1 <- dbGetQuery(con, "
WITH VentasMensuales AS (
  SELECT 
  SUBSTR(i.InvoiceDate, 1, 7) AS AnioMes,
  SUM(il.UnitPrice * il.Quantity) AS TotalVentas
  FROM Invoice i
  INNER JOIN InvoiceLine il ON il.InvoiceId = i.InvoiceId
  GROUP BY SUBSTR(i.InvoiceDate, 1, 7)
)
SELECT 
AnioMes,
TotalVentas,
LAG(TotalVentas) OVER (ORDER BY AnioMes)  AS VentasMesAnterior,
LEAD(TotalVentas) OVER (ORDER BY AnioMes) AS VentasMesSiguiente,
ROUND(
  (TotalVentas - LAG(TotalVentas) OVER (ORDER BY AnioMes)) 
  * 100.0 
  / LAG(TotalVentas) OVER (ORDER BY AnioMes)
  , 2) AS VariacionPorcentual
FROM VentasMensuales
ORDER BY AnioMes;

")

#Punto 2

p2 <- dbGetQuery(con, "
WITH VentasMensuales AS (
    SELECT 
        SUBSTR(i.InvoiceDate, 1, 4) AS Anio,
        SUBSTR(i.InvoiceDate, 1, 7) AS AnioMes,
        SUM(il.UnitPrice * il.Quantity) AS TotalVentas
    FROM Invoice i
    INNER JOIN InvoiceLine il ON il.InvoiceId = i.InvoiceId
    GROUP BY SUBSTR(i.InvoiceDate, 1, 4), SUBSTR(i.InvoiceDate, 1, 7)
),
VentasConRanking AS (
    SELECT 
        Anio,
        AnioMes,
        TotalVentas,
        RANK() OVER (PARTITION BY Anio ORDER BY TotalVentas DESC) AS RankTop,
        RANK() OVER (PARTITION BY Anio ORDER BY TotalVentas ASC)  AS RankBajo
    FROM VentasMensuales
)
SELECT 
    top.Anio,
    top.AnioMes  AS MesTop,
    top.TotalVentas AS VentasMesTop,
    bajo.AnioMes AS MesMasBajo,
    bajo.TotalVentas AS VentasMesMasBajo
FROM VentasConRanking top
INNER JOIN VentasConRanking bajo 
    ON top.Anio = bajo.Anio
    AND bajo.RankBajo = 1
WHERE top.RankTop = 1
ORDER BY top.Anio;
")

#Punto 3

p3 <- dbGetQuery(con, "
WITH IngresosPorGeneroAnio AS (
    SELECT 
        SUBSTR(i.InvoiceDate, 1, 4) AS Anio,
        g.Name AS Genero,
        SUM(il.UnitPrice * il.Quantity) AS IngresoTotal
    FROM Invoice i
    INNER JOIN InvoiceLine il ON il.InvoiceId = i.InvoiceId
    INNER JOIN Track t ON t.TrackId = il.TrackId
    INNER JOIN Genre g ON g.GenreId = t.GenreId
    GROUP BY SUBSTR(i.InvoiceDate, 1, 4), g.Name
),
RankingGeneros AS (
    SELECT 
        Anio,
        Genero,
        IngresoTotal,
        RANK() OVER (PARTITION BY Anio ORDER BY IngresoTotal DESC) AS Posicion
    FROM IngresosPorGeneroAnio
)
SELECT 
    Anio,
    Genero AS GeneroTop,
    IngresoTotal AS IngresoGeneroTop
FROM RankingGeneros
WHERE Posicion = 1
ORDER BY Anio;
")

#Punto 4

p4 <- dbGetQuery(con, "
WITH GastoPorCliente AS (
    SELECT 
        c.CustomerId,
        SUM(i.Total) AS GastoTotal
    FROM Customer c
    INNER JOIN Invoice i ON i.CustomerId = c.CustomerId
    GROUP BY c.CustomerId
),
ClientesConCuartil AS (
    SELECT 
        CustomerId,
        GastoTotal,
        NTILE(4) OVER (ORDER BY GastoTotal ASC) AS Cuartil
    FROM GastoPorCliente
),
IngresoPorCuartil AS (
    SELECT 
        Cuartil,
        SUM(GastoTotal) AS IngresoCuartil
    FROM ClientesConCuartil
    GROUP BY Cuartil
)
SELECT 
    SUM(CASE WHEN Cuartil = 4 THEN IngresoCuartil ELSE 0 END) AS IngresoQ4,
    SUM(CASE WHEN Cuartil < 4 THEN IngresoCuartil ELSE 0 END) AS IngresoQ1aQ3,
    ROUND(
        SUM(CASE WHEN Cuartil = 4 THEN IngresoCuartil ELSE 0 END) * 100.0 
        / SUM(IngresoCuartil)
    , 2) AS PorcentajeQ4,
    ROUND(
        SUM(CASE WHEN Cuartil < 4 THEN IngresoCuartil ELSE 0 END) * 100.0 
        / SUM(IngresoCuartil)
    , 2) AS PorcentajeQ1aQ3
FROM IngresoPorCuartil;
")

