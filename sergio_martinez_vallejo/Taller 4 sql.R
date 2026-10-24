install.packages("DBI")
install.packages("RSQLite")
install.packages("dplyr")

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

library(DBI)
library(RSQLite)
library(dplyr)

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")
# Listamos las tablas disponibles
dbListTables(con)


# 1.) Comprador, trabajador y jefe de trabajador 


resultado1 <- dbGetQuery(con, "
  SELECT Customer.FirstName || ' ' || Customer.LastName   AS Comprador,
         Employee.FirstName || ' ' || Employee.LastName   AS Empleado,
         COALESCE(jefe.FirstName|| ' ' || jefe.LastName, 'sin jefe') AS Jefe
  FROM   Customer 
  INNER JOIN Employee
  ON Customer.SupportRepId = Employee.EmployeeId
  LEFT JOIN Employee Jefe
  ON Employee.ReportsTo = Jefe.EmployeeId
  limit 10;
")
resultado1


# 2.) Subconsulta correlacionada 

resultado2 <- dbGetQuery(con, "
  SELECT Customer.CustomerId,
         Customer.FirstName AS Comprador,
         SUM(Invoice.Total) AS total
  FROM Customer 
  JOIN Invoice
    ON Customer.CustomerId = Invoice.CustomerId
  GROUP BY 
    Customer.CustomerId, 
    Customer.FirstName, 
    Customer.SupportRepId
  HAVING SUM(Invoice.Total) > (
    SELECT AVG(Promedio)
    FROM (
      SELECT SUM(FacturaPromedio.Total) AS Promedio
      FROM Customer AS CompradoresPromedio
      JOIN Invoice AS FacturaPromedio
        ON CompradoresPromedio.CustomerId = FacturaPromedio.CustomerId
      WHERE CompradoresPromedio.SupportRepId = Customer.SupportRepId
        AND CompradoresPromedio.CustomerId <> Customer.CustomerId
      GROUP BY CompradoresPromedio.CustomerId
    )
  )
  limit 10;
")
resultado2


# 3.) Empleados sin compradores


resultado3 <- dbGetQuery(con, "
  SELECT Employee.EmployeeId,
         Employee.FirstName AS Empleado,
         Employee.Title AS Ocupacion
  FROM Employee
  WHERE Employee.EmployeeId IN (
   SELECT EmployeeId FROM Employee
  EXCEPT
  SELECT SupportRepId FROM Customer WHERE SupportRepId IS NOT NULL
  )
  limit 5;
  "
)
resultado3


# 4.) CTE compradores que atiende cada empleado

resultado4 <- dbGetQuery(con, "
  WITH Resumen AS (
  SELECT e.EmployeeId,
         e.FirstName AS Empleado,
  COUNT(DISTINCT c.CustomerId) AS NumCompradores,
  COALESCE(SUM(i.Total), 0) AS Ingreso
  FROM Employee e
  LEFT JOIN Customer c
  ON e.EmployeeId = c.SupportRepId
  LEFT JOIN Invoice i 
  ON c.CustomerId = i.CustomerId
  GROUP BY e.EmployeeId, e.FirstName
  )
  SELECT Empleado,
         NumCompradores,
         Ingreso
  FROM Resumen
  ORDER BY Ingreso DESC            
  limit 5;
")
resultado4



### Punto 2

## Total de ventas por año y mes (mes anterior-siguiente) y variacion porcentual

resultado21 <- dbGetQuery(con, "
  WITH MetricasMensuales AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 7) AS Periodo,
      SUM(Total) AS Facturacion
    FROM Invoice
    GROUP BY SUBSTR(InvoiceDate, 1, 7)
  )
  SELECT 
    Periodo AS Mes,
    Facturacion,
    LAG(Facturacion, 1) OVER (ORDER BY Periodo) AS Mes_anterior,
    LEAD(Facturacion, 1) OVER (ORDER BY Periodo) AS Mes_siguiente,
    ROUND(
      ((Facturacion - LAG(Facturacion, 1) OVER (ORDER BY Periodo)) / LAG(Facturacion, 1) OVER (ORDER BY Periodo)) * 100.0, 
      2
    ) || '%' AS Variacion_Porcentual
  FROM MetricasMensuales
  limit 10;
")

resultado21


## CTE de ventas por año y mes (ranking con año de mayor-menor venta)

resultado22 <- dbGetQuery(con, "
  WITH TotalesMensuales AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 4) AS Año,
      SUBSTR(InvoiceDate, 1, 7) AS Mes,
      SUM(Total) AS Ventas
    FROM Invoice
    GROUP BY SUBSTR(InvoiceDate, 1, 7)
  ),
  MesesExtremos AS (
    SELECT 
      Año,
      Mes,
      Ventas,
      ROW_NUMBER() OVER (PARTITION BY Año ORDER BY Ventas DESC) AS Maximo,
      ROW_NUMBER() OVER (PARTITION BY Año ORDER BY Ventas ASC) AS Minimo
    FROM TotalesMensuales
  )
  SELECT 
    t_max.Año,
    t_max.Mes AS Mes_Mayor_Venta,
    t_max.Ventas AS Venta_Max,
    t_min.Mes AS Mes_Menor_Venta,
    t_min.Ventas AS Venta_Min
  FROM MesesExtremos t_max
  JOIN MesesExtremos t_min 
    ON t_max.Año = t_min.Año
  WHERE t_max.Maximo = 1 
    AND t_min.Minimo = 1;
")

resultado22


## Calculo de año con genero musical con mayor ingreso

resultado23 <- dbGetQuery(con, "
  WITH Ventas_Genero AS (
    SELECT 
      SUBSTR(i.InvoiceDate, 1, 4) AS Año,
      g.Name AS Genero_Musical,
      SUM(linea.UnitPrice * linea.Quantity) AS Ingreso
    FROM Invoice i
    INNER JOIN InvoiceLine linea ON i.InvoiceId = linea.InvoiceId
    INNER JOIN Track t ON linea.TrackId = t.TrackId
    INNER JOIN Genre g ON t.GenreId = g.GenreId
    GROUP BY SUBSTR(i.InvoiceDate, 1, 4), g.Name
  ),
  RankingGeneros AS (
    SELECT 
      Año,
      Genero_Musical,
      Ingreso,
      RANK() OVER (PARTITION BY Año ORDER BY Ingreso DESC) AS Posicion
    FROM Ventas_Genero
  )
  SELECT 
    Año,
    Genero_Musical,
    Ingreso
  FROM RankingGeneros
  WHERE Posicion = 1;
")

resultado23
