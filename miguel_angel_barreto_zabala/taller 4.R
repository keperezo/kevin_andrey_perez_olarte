library(DBI)
library(RSQLite)
library(dplyr)
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
url_chinook <- "https://raw.githubusercontent.com/lerocha/chino
ok-database/master/ChinookDatabase/DataSources/Chinook_
Sqlite.sqlite"
ruta_db <- "chinook.db"
if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

dbListTables(con)

resultado <- dbGetQuery(con, "
SELECT
    c.FirstName AS NombreCliente,
    c.LastName AS ApellidoCliente,
    e.FirstName AS NombreEmpleado,
    e.LastName AS ApellidoEmpleado,
    COALESCE(j.FirstName, 'Sin jefe') AS NombreJefe,
    COALESCE(j.LastName, '') AS ApellidoJefe
FROM Customer AS c
INNER JOIN Employee AS e
    ON c.SupportRepId = e.EmployeeId
LEFT JOIN Employee AS j
    ON e.ReportsTo = j.EmployeeId
")

resultado

resultado2 <- dbGetQuery(con, "
SELECT
    c.FirstName AS Nombre,
    c.LastName AS Apellido,
    SUM(i.Total) AS GastoTotal
FROM Customer AS c
INNER JOIN Invoice AS i
    ON c.CustomerId = i.CustomerId
GROUP BY c.CustomerId
")
resultado2

resultado3 <- dbGetQuery(con, "
SELECT EmployeeId, FirstName, LastName
FROM Employee

EXCEPT

SELECT e.EmployeeId, e.FirstName, e.LastName
FROM Employee AS e
INNER JOIN Customer AS c
    ON e.EmployeeId = c.SupportRepId
")

resultado3

resultado4 <- dbGetQuery(con, "
WITH DatosEmpleado AS (
    SELECT
        e.EmployeeId,
        e.FirstName,
        e.LastName,
        COUNT(DISTINCT c.CustomerId) AS CantidadClientes,
        SUM(i.Total) AS IngresoTotal
    FROM Employee AS e
    INNER JOIN Customer AS c
        ON e.EmployeeId = c.SupportRepId
    INNER JOIN Invoice AS i
        ON c.CustomerId = i.CustomerId
    GROUP BY e.EmployeeId
)
SELECT
    EmployeeId,
    FirstName,
    LastName,
    CantidadClientes,
    IngresoTotal
FROM DatosEmpleado
ORDER BY IngresoTotal DESC
")

resultado4


resultado5 <- dbGetQuery(con, "
WITH VentasMensuales AS (
    SELECT
        SUBSTR(InvoiceDate, 1, 7) AS AnioMes,
        SUM(Total) AS Ventas
    FROM Invoice
    GROUP BY SUBSTR(InvoiceDate, 1, 7)
),
VentasConVentana AS (
    SELECT
        AnioMes,
        Ventas,
        LAG(Ventas) OVER (ORDER BY AnioMes) AS VentasMesAnterior,
        LEAD(Ventas) OVER (ORDER BY AnioMes) AS VentasMesSiguiente
    FROM VentasMensuales
)
SELECT
    AnioMes,
    Ventas,
    VentasMesAnterior,
    VentasMesSiguiente,
    CASE
        WHEN VentasMesAnterior IS NULL
             OR VentasMesAnterior = 0
        THEN NULL
        ELSE ((Ventas - VentasMesAnterior)
              / VentasMesAnterior) * 100
    END AS VariacionPorcentual
FROM VentasConVentana
ORDER BY AnioMes
")

resultado5

resultado6 <- dbGetQuery(con, "
WITH VentasMensuales AS (
    SELECT
        SUBSTR(InvoiceDate, 1, 4) AS Anio,
        SUBSTR(InvoiceDate, 6, 2) AS Mes,
        SUM(Total) AS Ventas
    FROM Invoice
    GROUP BY
        SUBSTR(InvoiceDate, 1, 4),
        SUBSTR(InvoiceDate, 6, 2)
),
Rankings AS (
    SELECT
        Anio,
        Mes,
        Ventas,
        RANK() OVER (
            PARTITION BY Anio
            ORDER BY Ventas DESC
        ) AS RankingMayor,
        RANK() OVER (
            PARTITION BY Anio
            ORDER BY Ventas ASC
        ) AS RankingMenor
    FROM VentasMensuales
)
SELECT
    Anio,
    MAX(CASE WHEN RankingMayor = 1 THEN Mes END) AS MesTop,
    MAX(CASE WHEN RankingMayor = 1 THEN Ventas END) AS VentaTop,
    MAX(CASE WHEN RankingMenor = 1 THEN Mes END) AS MesBajo,
    MAX(CASE WHEN RankingMenor = 1 THEN Ventas END) AS VentaBajo
FROM Rankings
GROUP BY Anio
ORDER BY Anio
")

resultado6

resultado7 <- dbGetQuery(con, "
WITH IngresosGenero AS (
    SELECT
        SUBSTR(i.InvoiceDate, 1, 4) AS Anio,
        g.Name AS Genero,
        SUM(il.UnitPrice * il.Quantity) AS Ingresos
    FROM Invoice AS i
    INNER JOIN InvoiceLine AS il
        ON i.InvoiceId = il.InvoiceId
    INNER JOIN Track AS t
        ON il.TrackId = t.TrackId
    INNER JOIN Genre AS g
        ON t.GenreId = g.GenreId
    GROUP BY
        SUBSTR(i.InvoiceDate, 1, 4),
        g.Name
),
RankingGenero AS (
    SELECT
        Anio,
        Genero,
        Ingresos,
        RANK() OVER (
            PARTITION BY Anio
            ORDER BY Ingresos DESC
        ) AS Posicion
    FROM IngresosGenero
)
SELECT
    Anio,
    Genero,
    Ingresos
FROM RankingGenero
WHERE Posicion = 1
ORDER BY Anio
")

resultado7

resultado8 <- dbGetQuery(con, "
WITH GastoCliente AS (
    SELECT
        c.CustomerId,
        SUM(i.Total) AS GastoTotal
    FROM Customer AS c
    INNER JOIN Invoice AS i
        ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
Cuartiles AS (
    SELECT
        CustomerId,
        GastoTotal,
        NTILE(4) OVER (
            ORDER BY GastoTotal
        ) AS Cuartil
    FROM GastoCliente
),
ResumenCuartil AS (
    SELECT
        Cuartil,
        SUM(GastoTotal) AS IngresosCuartil
    FROM Cuartiles
    GROUP BY Cuartil
)
SELECT
    SUM(CASE WHEN Cuartil = 4 THEN IngresosCuartil ELSE 0 END) AS IngresoQ4,
    SUM(CASE WHEN Cuartil < 4 THEN IngresosCuartil ELSE 0 END) AS IngresoOtros,
    ROUND(
        SUM(CASE WHEN Cuartil = 4 THEN IngresosCuartil ELSE 0 END) * 100.0
        / SUM(IngresosCuartil),
        2
    ) AS PorcentajeQ4,
    ROUND(
        SUM(CASE WHEN Cuartil < 4 THEN IngresosCuartil ELSE 0 END) * 100.0
        / SUM(IngresosCuartil),
        2
    ) AS PorcentajeOtros
FROM ResumenCuartil
")

resultado8
