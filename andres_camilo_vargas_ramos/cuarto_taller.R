library(DBI)
library(RSQLite)
library(dplyr)

ruta_db <- "chinook.db"
if (file.exists(ruta_db)) file.remove(ruta_db)


url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
download.file(url_chinook, destfile = ruta_db, mode = "wb")


con <- dbConnect(RSQLite::SQLite(), ruta_db)
dbListTables(con) 



### 1 pregunta

Punto1a <- dbGetQuery(con," 
           SELECT 
           Customer.FirstName || ' ' || Customer.LastName AS Cliente,
           Employee.FirstName || ' ' || Employee.LastName AS Empleado_Soporte,
           COALESCE(jefe.FirstName || ' ' || jefe.LastName, 'Sin jefe') AS Jefe_Empleado
           FROM Customer 
           INNER JOIN Employee  
           ON Customer.SupportRepId = Employee.EmployeeId
           LEFT JOIN Employee jefe 
           ON Employee.ReportsTo = jefe.EmployeeId;
           ")

Punto1a

### 2 pregunta

Punto1b <- dbGetQuery(con, "
  SELECT 
    Customer.CustomerId,
    Customer.FirstName || ' ' || Customer.LastName AS Cliente,
    SUM(Invoice.Total) AS total_gastado
    FROM Customer 
    JOIN Invoice
    ON Customer.CustomerId = Invoice.CustomerId
    GROUP BY 
    Customer.CustomerId, 
    Customer.FirstName, 
    Customer.LastName, 
    Customer.SupportRepId
    HAVING SUM(Invoice.Total) > (
    SELECT AVG(GastoOtros)
    FROM (
      SELECT SUM(FacturaOtros.Total) AS GastoOtros
      FROM Customer AS ClienteOtros
      JOIN Invoice AS FacturaOtros
        ON ClienteOtros.CustomerId = FacturaOtros.CustomerId
      WHERE ClienteOtros.SupportRepId = Customer.SupportRepId
        AND ClienteOtros.CustomerId <> Customer.CustomerId
      GROUP BY ClienteOtros.CustomerId
    )
  );
")
Punto1b

##3 pregunta

punto1c <- dbGetQuery(con, "
                      SELECT
                      Employee.EmployeeId,
                      Employee.FirstName || ' ' || Employee.LastName AS Empleado,
                      Employee.Title AS Cargo
                      FROM Employee
                      WHERE Employee.EmployeeId IN (
                      SELECT EmployeeId FROM Employee
                      EXCEPT
                      SELECT SupportRepId FROM Customer WHERE SupportRepId IS NOT NULL
                      );
                      "
                      )
punto1c

## 4 pregunta

punto1d <- dbGetQuery(con, "
                      WITH ResumenSoporte AS (
                      SELECT 
                      e.EmployeeId,
                      e.FirstName || ' ' || e.LastName AS Empleado,
                      COUNT(DISTINCT c.CustomerId) AS Cantidad_Clientes,
                      COALESCE(SUM(i.Total), 0) AS Ingreso_Total
                      FROM Employee e
                      LEFT JOIN Customer c
                      ON e.EmployeeId = c.SupportRepId
                      LEFT JOIN Invoice i 
                      ON c.CustomerId = i.CustomerId
                      GROUP BY e.EmployeeId, e.FirstName, e.LastName
                      )
                      SELECT 
                      Empleado,
                      Cantidad_Clientes,
                      Ingreso_Total
                      FROM ResumenSoporte
                      ORDER BY Ingreso_Total DESC;            
                      ;
                      ")

punto1d



########################################################


## Punto 2.a

punto2a <- dbGetQuery(con, "
                      WITH VentasMensuales AS (
                      SELECT 
                      SUBSTR(InvoiceDate, 1, 7) AS AnioMes,
                      SUM(Total) AS Ventas
                      FROM Invoice
                      GROUP BY SUBSTR(InvoiceDate, 1, 7)
                      )
                      SELECT 
                      AnioMes,
                      Ventas,
                      LAG(Ventas) OVER (ORDER BY AnioMes) AS VentasMesAnterior,
                      LEAD(Ventas) OVER (ORDER BY AnioMes) AS VentasMesSiguiente,
                      ROUND(
                      ((Ventas - LAG(Ventas) OVER (ORDER BY AnioMes)) / LAG(Ventas) OVER (ORDER BY AnioMes)) * 100, 
                      2
                      ) || '%' AS VariacionPorcentual
                      FROM VentasMensuales;
                      ")

punto2a


## Punto 2.b
punto2b <- dbGetQuery(con, "
                      WITH VentasMensuales AS (
                      SELECT 
                      SUBSTR(InvoiceDate, 1, 4) AS Anio,
                      SUBSTR(InvoiceDate, 1, 7) AS AnioMes,
                      SUM(Total) AS Ventas
                      FROM Invoice
                      GROUP BY SUBSTR(InvoiceDate, 1, 7)
                      ),
                      Rankings AS (
                      SELECT 
                      Anio,
                      AnioMes,
                      Ventas,
                      RANK() OVER (PARTITION BY Anio ORDER BY Ventas DESC) AS RankTop,
                      RANK() OVER (PARTITION BY Anio ORDER BY Ventas ASC) AS RankBajo
                      FROM VentasMensuales
                      )
                      SELECT 
                        r1.Anio,
                        r1.AnioMes AS MesTop,
                        r1.Ventas AS VentasTop,
                        r2.AnioMes AS MesBajo,
                        r2.Ventas AS VentasBajo
                      FROM Rankings r1
                      JOIN Rankings r2 
                        ON r1.Anio = r2.Anio
                      WHERE r1.RankTop = 1 
                        AND r2.RankBajo = 1;
                      ")

punto2b


## Punto 2.3
punto2c <- dbGetQuery(con, "
                      WITH GeneroPorAnio AS (
                      SELECT 
                      SUBSTR(Invoice.InvoiceDate, 1, 4) AS Anio,
                      Genre.Name AS Genero,
                      SUM(InvoiceLine.UnitPrice * InvoiceLine.Quantity) AS Ingresos
                      FROM Invoice
                      JOIN InvoiceLine ON Invoice.InvoiceId = InvoiceLine.InvoiceId
                      JOIN Track ON InvoiceLine.TrackId = Track.TrackId
                      JOIN Genre ON Track.GenreId = Genre.GenreId
                      GROUP BY SUBSTR(Invoice.InvoiceDate, 1, 4), Genre.Name
                      ),
                      RankingGenero AS (
                      SELECT 
                      Anio,
                      Genero,
                      Ingresos,
                      RANK() OVER (PARTITION BY Anio ORDER BY Ingresos DESC) AS Posicion
                      FROM GeneroPorAnio
                      )
                      SELECT 
                        Anio,
                        Genero,
                        Ingresos
                      FROM RankingGenero
                      WHERE Posicion = 1;
                      ")

punto2c


## Punto 2.4

punto2d <- dbGetQuery(con, "
                      WITH GastoCliente AS (
                      SELECT 
                      CustomerId,
                      SUM(Total) AS GastoTotal,
                      NTILE(4) OVER (ORDER BY SUM(Total) ASC) AS Cuartil
                      FROM Invoice
                      GROUP BY CustomerId
                      ),
                      VentasPorCuartil AS (
                        SELECT 
                        Cuartil,
                        SUM(GastoTotal) AS TotalCuartil
                        FROM GastoCliente
                        GROUP BY Cuartil
                      ),
                      Comparacion AS (
                        SELECT 
                        SUM(CASE WHEN Cuartil = 4 THEN TotalCuartil ELSE 0 END) AS Ingreso_Q4,
                        SUM(CASE WHEN Cuartil < 4 THEN TotalCuartil ELSE 0 END) AS Ingreso_Otros,
                        SUM(TotalCuartil) AS Ingreso_Total
                        FROM VentasPorCuartil
                      )
                      SELECT 
                        Ingreso_Q4,
                        Ingreso_Otros,
                        Ingreso_Total,
                        ROUND((Ingreso_Q4 / Ingreso_Total) * 100, 2) || '%' AS Porcentaje_Q4,
                        ROUND((Ingreso_Otros / Ingreso_Total) * 100, 2) || '%' AS Porcentaje_Otros
                      FROM Comparacion;
                      ")

punto2d

