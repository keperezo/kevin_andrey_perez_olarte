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


# EJERCICIO 2 - PARTE 1: Ventas mensuales, LAG, LEAD y Variación Porcentual
cat("--- EJERCICIO 2.1: Ventas mensuales con LAG, LEAD y Variación % ---\n")

query_2_1 <- "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
           ROUND(SUM(Total), 2)      AS ventas
    FROM Invoice
    GROUP BY anio_mes
  )
  SELECT 
    anio_mes,
    ventas,
    LAG(ventas, 1) OVER (ORDER BY anio_mes)  AS ventas_mes_anterior,
    LEAD(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_siguiente,
    ROUND(
      ((ventas - LAG(ventas, 1) OVER (ORDER BY anio_mes)) * 100.0) / 
      LAG(ventas, 1) OVER (ORDER BY anio_mes), 
    2)                                       AS variacion_porcentual
  FROM ventas_mes
  ORDER BY anio_mes
  LIMIT 15;
"

res_2_1 <- dbGetQuery(con, query_2_1)
print(res_2_1)
cat("\n")


# EJERCICIO 2 - PARTE 2: Mes de mayor y menor venta por año usando RANK()
cat("--- EJERCICIO 2.2: Mes top y mes más bajo de cada año (con RANK) ---\n")

query_2_2 <- "
  WITH ventas_anho_mes AS (
    SELECT 
      SUBSTR(InvoiceDate, 1, 4) AS anho,
      SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
      ROUND(SUM(Total), 2)      AS ventas
    FROM Invoice
    GROUP BY anio_mes
  ),
  ranking_ventas AS (
    SELECT 
      anho,
      anio_mes,
      ventas,
      RANK() OVER (PARTITION BY anho ORDER BY ventas DESC) AS rank_top,
      RANK() OVER (PARTITION BY anho ORDER BY ventas ASC)  AS rank_bottom
    FROM ventas_anho_mes
  )
  SELECT 
    anho,
    MAX(CASE WHEN rank_top = 1 THEN anio_mes END) AS mes_mas_alto,
    MAX(CASE WHEN rank_top = 1 THEN ventas   END) AS ventas_max,
    MAX(CASE WHEN rank_bottom = 1 THEN anio_mes END) AS mes_mas_bajo,
    MAX(CASE WHEN rank_bottom = 1 THEN ventas   END) AS ventas_min
  FROM ranking_ventas
  GROUP BY anho
  ORDER BY anho;
"

res_2_2 <- dbGetQuery(con, query_2_2)
print(res_2_2)
cat("\n")


# EJERCICIO 2 - PARTE 3: Género musical con mayores ingresos por año (CTEs encadenadas)
cat("--- EJERCICIO 2.3: Género musical líder en ingresos por año ---\n")

query_2_3 <- "
  WITH ingresos_genero_anho AS (
    SELECT 
      SUBSTR(i.InvoiceDate, 1, 4)             AS anho,
      g.Name                                  AS genero,
      ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS ingresos
    FROM Invoice     i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track       t  ON il.TrackId  = t.TrackId
    JOIN Genre       g  ON t.GenreId   = g.GenreId
    GROUP BY anho, g.Name
  ),
  ranking_generos AS (
    SELECT 
      anho,
      genero,
      ingresos,
      RANK() OVER (PARTITION BY anho ORDER BY ingresos DESC) AS rk
    FROM ingresos_genero_anho
  )
  SELECT 
    anho,
    genero AS genero_top_ingresos,
    ingresos
  FROM ranking_generos
  WHERE rk = 1
  ORDER BY anho;
"

res_2_3 <- dbGetQuery(con, query_2_3)
print(res_2_3)
cat("\n")


# EJERCICIO 2 - PARTE 4: Cuartiles de gasto (NTILE) y comparación Q4 vs Q1-Q3
cat("--- EJERCICIO 2.4: Análisis de cuartiles (Q4 vs Cuartiles 1, 2 y 3) ---\n")

# Tabla detallada por cuartil
query_2_4_detalles <- "
  WITH gasto_clientes AS (
    SELECT 
      c.CustomerId,
      ROUND(SUM(i.Total), 2) AS gasto_total
    FROM Customer c
    JOIN Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  clientes_cuartiles AS (
    SELECT 
      gasto_total,
      NTILE(4) OVER (ORDER BY gasto_total ASC) AS cuartil
    FROM gasto_clientes
  ),
  agregado_cuartiles AS (
    SELECT 
      cuartil,
      COUNT(*)               AS n_clientes,
      ROUND(SUM(gasto_total), 2) AS ingresos_cuartil
    FROM clientes_cuartiles
    GROUP BY cuartil
  ),
  ingreso_total_tienda AS (
    SELECT SUM(ingresos_cuartil) AS gran_total FROM agregado_cuartiles
  )
  SELECT 
    ac.cuartil,
    ac.n_clientes,
    ac.ingresos_cuartil,
    ROUND(100.0 * ac.ingresos_cuartil / it.gran_total, 2) AS pct_ingreso_total
  FROM agregado_cuartiles ac, ingreso_total_tienda it
  ORDER BY ac.cuartil;
"

cat("Distribución por Cuartiles:\n")
res_2_4_detalles <- dbGetQuery(con, query_2_4_detalles)
print(res_2_4_detalles)
cat("\n")

# Comparativa directa: Cuartil Superior (Q4) frente a los otros tres combinados (Q1-Q3)
query_2_4_comparacion <- "
  WITH gasto_clientes AS (
    SELECT 
      c.CustomerId,
      ROUND(SUM(i.Total), 2) AS gasto_total
    FROM Customer c
    JOIN Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  clientes_cuartiles AS (
    SELECT 
      gasto_total,
      NTILE(4) OVER (ORDER BY gasto_total ASC) AS cuartil
    FROM gasto_clientes
  ),
  resumen_q AS (
    SELECT 
      CASE 
        WHEN cuartil = 4 THEN 'Cuartil Superior (Q4)' 
        ELSE 'Cuartiles Inferiores y Medios (Q1-Q3)' 
      END AS grupo,
      ROUND(SUM(gasto_total), 2) AS ingresos_grupo
    FROM clientes_cuartiles
    GROUP BY grupo
  ),
  total_tienda AS (
    SELECT SUM(ingresos_grupo) AS gran_total FROM resumen_q
  )
  SELECT 
    grupo,
    ingresos_grupo,
    ROUND(100.0 * ingresos_grupo / gran_total, 2) AS porcentaje_ingreso_tienda
  FROM resumen_q, total_tienda
  ORDER BY ingresos_grupo DESC;
"

cat("Comparativa Q4 vs Resto de Cuartiles (Q1-Q3):\n")
res_2_4_comparacion <- dbGetQuery(con, query_2_4_comparacion)
print(res_2_4_comparacion)
cat("\n")


# Cerrar la conexión
dbDisconnect(con)
cat("Conexión cerrada exitosamente.\n")
