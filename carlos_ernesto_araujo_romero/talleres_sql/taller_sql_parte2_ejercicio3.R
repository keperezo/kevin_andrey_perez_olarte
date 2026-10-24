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


# EJERCICIO 3 (RETO INTEGRADOR): Análisis de Clientes en Riesgo de Abandono (Churn)

# PARTE A: Resumen por segmento (Número de clientes, gasto promedio y total)
cat("--- 3.A: Resumen de métricas por segmento de cliente ---\n")

query_3_resumen <- "
  WITH max_fecha_db AS (
    -- 1. Fecha de la última factura registrada en toda la base de datos (el presente)
    SELECT MAX(InvoiceDate) AS fecha_maxima FROM Invoice
  ),
  cliente_ultima_compra AS (
    -- 2. Última compra por cliente y cálculo de meses de inactividad
    SELECT 
      c.CustomerId,
      MAX(i.InvoiceDate) AS ultima_compra,
      SUM(i.Total)       AS gasto_historico_total,
      (JULIANDAY((SELECT fecha_maxima FROM max_fecha_db)) - JULIANDAY(MAX(i.InvoiceDate))) / 30.0 AS meses_inactivo
    FROM Customer c
    JOIN Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  clientes_segmentados AS (
    -- 3. Clasificación por segmento según los meses de inactividad
    SELECT 
      CustomerId,
      gasto_historico_total,
      CASE 
        WHEN meses_inactivo <= 6 THEN 'activo'
        WHEN meses_inactivo > 6 AND meses_inactivo <= 12 THEN 'en riesgo'
        ELSE 'inactivo'
      END AS segmento
    FROM cliente_ultima_compra
  )
  -- 4. Métricas agregadas por cada segmento
  SELECT 
    segmento,
    COUNT(CustomerId)                    AS n_clientes,
    ROUND(AVG(gasto_historico_total), 2) AS gasto_historico_promedio,
    ROUND(SUM(gasto_historico_total), 2) AS gasto_historico_total_segmento
  FROM clientes_segmentados
  GROUP BY segmento
  ORDER BY gasto_historico_total_segmento DESC;
"

res_3_resumen <- dbGetQuery(con, query_3_resumen)
print(res_3_resumen)
cat("\n")


# PARTE B: Top 3 clientes de mayor valor histórico dentro de cada segmento (RANK)
cat("--- 3.B: Top 3 clientes de mayor valor (gasto histórico) por segmento ---\n")

query_3_top3 <- "
  WITH max_fecha_db AS (
    SELECT MAX(InvoiceDate) AS fecha_maxima FROM Invoice
  ),
  cliente_ultima_compra AS (
    SELECT 
      c.CustomerId,
      c.FirstName || ' ' || c.LastName AS cliente,
      c.Country                        AS pais,
      SUM(i.Total)                     AS gasto_historico_total,
      (JULIANDAY((SELECT fecha_maxima FROM max_fecha_db)) - JULIANDAY(MAX(i.InvoiceDate))) / 30.0 AS meses_inactivo
    FROM Customer c
    JOIN Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  clientes_segmentados AS (
    SELECT 
      cliente,
      pais,
      gasto_historico_total,
      ROUND(meses_inactivo, 1) AS meses_inactivo,
      CASE 
        WHEN meses_inactivo <= 6 THEN 'activo'
        WHEN meses_inactivo > 6 AND meses_inactivo <= 12 THEN 'en riesgo'
        ELSE 'inactivo'
      END AS segmento
    FROM cliente_ultima_compra
  ),
  ranking_segmento AS (
    SELECT 
      segmento,
      cliente,
      pais,
      ROUND(gasto_historico_total, 2) AS gasto_historico_total,
      meses_inactivo,
      RANK() OVER (PARTITION BY segmento ORDER BY gasto_historico_total DESC) AS rk
    FROM clientes_segmentados
  )
  SELECT 
    segmento,
    rk AS ranking,
    cliente,
    pais,
    gasto_historico_total,
    meses_inactivo
  FROM ranking_segmento
  WHERE rk <= 3
  ORDER BY segmento, rk;
"

res_3_top3 <- dbGetQuery(con, query_3_top3)
print(res_3_top3)
cat("\n")


# INTERPRETACIÓN 
cat("--- Interpretación Analítica del Reto de Retención (Churn) ---\n")
cat("1. Segmento Activo:\n")
cat("   Representa la base saludable actual de la tienda. Son quienes generan ingresos constantes.\n")
cat("2. Segmento en Riesgo:\n")
cat("   ¡Es el segmento prioritario para campañas de retención (marketing directo/incentivos)!\n")
cat("   Aunque llevan entre 6 y 12 meses sin comprar, suelen acumular un valor histórico importante\n")
cat("   y rescatarlos es mucho más económico que adquirir nuevos clientes.\n")
cat("3. Segmento Inactivo:\n")
cat("   Clientes con más de 12 meses de inactividad. El Retorno de Inversión (ROI) para rescatarlos\n")
cat("   suele ser bajo, por lo que las campañas aquí deben ser automatizadas y de bajo costo\n")
cat("   (ej. correos masivos de reactivación), enfocándose únicamente en aquellos que se detecten\n")
cat("   en el Top 3 histórico con mayor valor previo.\n\n")


# Cerrar la conexión
dbDisconnect(con)
cat("Conexión cerrada exitosamente.\n")
