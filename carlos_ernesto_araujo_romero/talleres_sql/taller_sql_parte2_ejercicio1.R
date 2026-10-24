library(DBI)
library(RSQLite)
library(dplyr)

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

# Validación: Si el archivo existe pero está vacío/corrupto, lo borramos
if (file.exists(ruta_db)) {
  con_temp <- dbConnect(RSQLite::SQLite(), ruta_db)
  tablas_existentes <- dbListTables(con_temp)
  dbDisconnect(con_temp)
  
  if (length(tablas_existentes) == 0) {
    cat("El archivo 'chinook.db' estaba vacío o corrupto. Eliminándolo para re-descargar...\n")
    file.remove(ruta_db)
  }
}

# Descargar si no existe (o si fue borrado por estar vacío)
if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada correctamente en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe y es válida:", ruta_db, "\n")
}

# Abrimos la conexión oficial
con <- dbConnect(RSQLite::SQLite(), ruta_db)
cat("Conexión establecida.\n\n")

# Comprobación de seguridad (ahora sí debe mostrar las tablas)
cat("Tablas disponibles en la base de datos:\n")
print(dbListTables(con))
cat("\n")


# EJERCICIO 1 - PARTE 1: JOINs avanzados, COALESCE y SELF JOIN
cat("--- 1. Clientes, empleados de soporte y jefes ---\n")

query_1 <- "
  SELECT 
    c.FirstName || ' ' || c.LastName AS cliente,
    e.FirstName || ' ' || e.LastName AS empleado_soporte,
    COALESCE(m.FirstName || ' ' || m.LastName, 'Sin jefe') AS jefe_supervisor
  FROM Customer c
  INNER JOIN Employee e ON c.SupportRepId = e.EmployeeId
  LEFT JOIN Employee m ON e.ReportsTo = m.EmployeeId
  LIMIT 15;
"

res_1 <- dbGetQuery(con, query_1)
print(res_1)
cat("\n")


# EJERCICIO 1 - PARTE 2: Subconsulta correlacionada
cat("--- 2. Clientes que gastan más que el promedio de su representante ---\n")

query_2 <- "
  SELECT 
    c.CustomerId,
    c.FirstName || ' ' || c.LastName AS cliente,
    c.SupportRepId,
    ROUND(SUM(i.Total), 2) AS gasto_total_cliente
  FROM Customer c
  JOIN Invoice i ON c.CustomerId = i.CustomerId
  GROUP BY c.CustomerId
  HAVING SUM(i.Total) > (
    SELECT AVG(gasto_cliente_sub)
    FROM (
      SELECT 
        c_sub.CustomerId,
        SUM(i_sub.Total) AS gasto_cliente_sub
      FROM Customer c_sub
      JOIN Invoice i_sub ON c_sub.CustomerId = i_sub.CustomerId
      WHERE c_sub.SupportRepId = c.SupportRepId
      GROUP BY c_sub.CustomerId
    )
  )
  ORDER BY c.SupportRepId, gasto_total_cliente DESC;
"

res_2 <- dbGetQuery(con, query_2)
print(res_2)
cat("\n")

# EJERCICIO 1 - PARTE 3: Operador EXCEPT
cat("--- 3. Empleados que nunca han sido asignados como soporte ---\n")

query_3 <- "
  SELECT EmployeeId, FirstName || ' ' || LastName AS empleado, Title
  FROM Employee
  
  EXCEPT
  
  SELECT e.EmployeeId, e.FirstName || ' ' || e.LastName AS empleado, e.Title
  FROM Employee e
  JOIN Customer c ON e.EmployeeId = c.SupportRepId;
"

res_3 <- dbGetQuery(con, query_3)
print(res_3)
cat("\n")


# EJERCICIO 1 - PARTE 4: CTE y rendimiento de empleados
cat("--- 4. Rendimiento de empleados (clientes e ingresos) ---\n")

query_4 <- "
  WITH rendimiento_empleados AS (
    SELECT 
      e.EmployeeId,
      e.FirstName || ' ' || e.LastName AS empleado,
      e.Title AS cargo,
      COUNT(DISTINCT c.CustomerId) AS n_clientes_atendidos,
      ROUND(SUM(i.Total), 2) AS ingresos_totales
    FROM Employee e
    LEFT JOIN Customer c ON e.EmployeeId = c.SupportRepId
    LEFT JOIN Invoice i ON c.CustomerId = i.CustomerId
    WHERE e.Title LIKE '%Sales Support Agent%'
    GROUP BY e.EmployeeId, empleado, cargo
  )
  SELECT 
    empleado,
    cargo,
    n_clientes_atendidos,
    ingresos_totales
  FROM rendimiento_empleados
  ORDER BY ingresos_totales DESC;
"

res_4 <- dbGetQuery(con, query_4)
print(res_4)

cat("\n--- Interpretación Analítica del Ejercicio 1.4 ---\n")
cat("¿El empleado que atiende más clientes es también el que genera más ingresos?\n")
cat("Sí. Basado en los resultados obtenidos:\n")
cat("1. Jane Peacock lidera ambas métricas con 21 clientes y $833.04 en ingresos.\n")
cat("2. Margaret Park ocupa el segundo lugar con 20 clientes y $775.40.\n")
cat("3. Steve Johnson se ubica en el tercer lugar con 18 clientes y $720.16.\n")
cat("Conclusión: Existe una relación directamente proporcional y positiva entre el \n")
cat("número de clientes asignados y el valor monetario total aportado a la tienda.\n\n")


# Cerrar la conexión
dbDisconnect(con)
cat("Conexión cerrada.\n")

