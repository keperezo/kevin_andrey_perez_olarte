

library(DBI)
library(RSQLite)

# Descargar la base de datos Chinook desde un repositorio activo
download.file("https://github.com/lerocha/chinook-database/raw/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite", 
              destfile = "chinook.db", 
              mode = "wb")

# Conectar al archivo recién descargado
con <- dbConnect(RSQLite::SQLite(), "chinook.db")

# Verificar las tablas disponibles
dbListTables(con)

# ==========================================
# EJERCICIO 1
# ==========================================
print(res1_1)
# 1.1 SELF JOIN con COALESCE
q1_1 <- "
SELECT 
    c.FirstName || ' ' || c.LastName AS cliente,
    e.FirstName || ' ' || e.LastName AS soporte,
    COALESCE(jefe.FirstName || ' ' || jefe.LastName, 'Sin jefe') AS jefe_soporte
FROM Customer c
INNER JOIN Employee e ON c.SupportRepId = e.EmployeeId
LEFT JOIN Employee jefe ON e.ReportsTo = jefe.EmployeeId
LIMIT 15;
"
res1_1 <- dbGetQuery(con, q1_1)
print(res1_1)

# 1.2 Subconsulta correlacionada
q1_2 <- "
SELECT 
    c.CustomerId,
    c.FirstName || ' ' || c.LastName AS cliente,
    c.SupportRepId,
    ROUND(SUM(i.Total), 2) AS gasto_total_cliente
FROM Customer c
JOIN Invoice i ON c.CustomerId = i.CustomerId
GROUP BY c.CustomerId, cliente, c.SupportRepId
HAVING SUM(i.Total) > (
    SELECT AVG(total_otro_cliente)
    FROM (
        SELECT SUM(i2.Total) AS total_otro_cliente
        FROM Customer c2
        JOIN Invoice i2 ON c2.CustomerId = i2.CustomerId
        WHERE c2.SupportRepId = c.SupportRepId
          AND c2.CustomerId <> c.CustomerId
        GROUP BY c2.CustomerId
    )
);
"
res1_2 <- dbGetQuery(con, q1_2)
print(res1_2)

# 1.3 Operador EXCEPT
q1_3 <- "
SELECT EmployeeId, FirstName || ' ' || LastName AS empleado
FROM Employee
EXCEPT
SELECT DISTINCT e.EmployeeId, e.FirstName || ' ' || e.LastName
FROM Employee e
JOIN Customer c ON e.EmployeeId = c.SupportRepId;
"
res1_3 <- dbGetQuery(con, q1_3)
print(res1_3)

# 1.4 CTE Métricas de soporte
q1_4 <- "
WITH metricas_soporte AS (
    SELECT 
        e.EmployeeId,
        e.FirstName || ' ' || e.LastName AS empleado,
        COUNT(DISTINCT c.CustomerId) AS n_clientes,
        ROUND(SUM(i.Total), 2) AS ingreso_total
    FROM Employee e
    LEFT JOIN Customer c ON e.EmployeeId = c.SupportRepId
    LEFT JOIN Invoice i ON c.CustomerId = i.CustomerId
    WHERE e.Title LIKE '%Sales%' OR e.Title LIKE '%Support%'
    GROUP BY e.EmployeeId, empleado
)
SELECT empleado, n_clientes, ingreso_total
FROM metricas_soporte
ORDER BY ingreso_total DESC;
"
res1_4 <- dbGetQuery(con, q1_4)
print(res1_4)


# ==========================================
# EJERCICIO 2
# ==========================================

# 2.1 Ventas mensuales con LAG y LEAD
q2_1 <- "
WITH ventas_mensuales AS (
    SELECT 
        SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
        ROUND(SUM(Total), 2) AS ventas
    FROM Invoice
    GROUP BY anio_mes
)
SELECT 
    anio_mes,
    ventas,
    LAG(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_anterior,
    LEAD(ventas, 1) OVER (ORDER BY anio_mes) AS ventas_mes_siguiente,
    ROUND(
        (ventas - LAG(ventas, 1) OVER (ORDER BY anio_mes)) * 100.0 / 
        LAG(ventas, 1) OVER (ORDER BY anio_mes), 2
    ) AS var_pct_mes_anterior
FROM ventas_mensuales
ORDER BY anio_mes
LIMIT 15;
"
res2_1 <- dbGetQuery(con, q2_1)
print(res2_1)

# 2.2 Meses extremo por año
q2_2 <- "
WITH ventas_mes AS (
    SELECT 
        SUBSTR(InvoiceDate, 1, 4) AS anio,
        SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
        ROUND(SUM(Total), 2) AS ventas
    FROM Invoice
    GROUP BY anio_mes
),
rankings AS (
    SELECT 
        anio,
        anio_mes,
        ventas,
        RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS rk_max,
        RANK() OVER (PARTITION BY anio ORDER BY ventas ASC) AS rk_min
    FROM ventas_mes
)
SELECT 
    anio,
    MAX(CASE WHEN rk_max = 1 THEN anio_mes || ' ($' || ventas || ')' END) AS mes_top_ventas,
    MAX(CASE WHEN rk_min = 1 THEN anio_mes || ' ($' || ventas || ')' END) AS mes_bajo_ventas
FROM rankings
GROUP BY anio
ORDER BY anio;
"
res2_2 <- dbGetQuery(con, q2_2)
print(res2_2)

# 2.3 Género más vendido por año
q2_3 <- "
WITH ventas_genero_anio AS (
    SELECT 
        SUBSTR(i.InvoiceDate, 1, 4) AS anio,
        g.Name AS genero,
        ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS ingresos
    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Genre g ON t.GenreId = g.GenreId
    GROUP BY anio, genero
),
ranking_genero AS (
    SELECT 
        anio,
        genero,
        ingresos,
        RANK() OVER (PARTITION BY anio ORDER BY ingresos DESC) AS rk
    FROM ventas_genero_anio
)
SELECT anio, genero AS genero_top, ingresos
FROM ranking_genero
WHERE rk = 1
ORDER BY anio;
"
res2_3 <- dbGetQuery(con, q2_3)
print(res2_3)

# 2.4 Cuartiles NTILE
q2_4 <- "
WITH gasto_por_cliente AS (
    SELECT 
        c.CustomerId,
        ROUND(SUM(i.Total), 2) AS gasto_total
    FROM Customer c
    JOIN Invoice i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
cuartiles AS (
    SELECT 
        CustomerId,
        gasto_total,
        NTILE(4) OVER (ORDER BY gasto_total ASC) AS cuartil
    FROM gasto_por_cliente
),
resumen_cuartil AS (
    SELECT 
        CASE WHEN cuartil = 4 THEN 'Q4 (Cuartil Superior)' ELSE 'Q1 - Q3 (Resto de clientes)' END AS grupo,
        SUM(gasto_total) AS gasto_grupo
    FROM cuartiles
    GROUP BY CASE WHEN cuartil = 4 THEN 'Q4 (Cuartil Superior)' ELSE 'Q1 - Q3 (Resto de clientes)' END
),
total_tienda AS (
    SELECT SUM(gasto_grupo) AS gran_total FROM resumen_cuartil
)
SELECT 
    r.grupo,
    ROUND(r.gasto_grupo, 2) AS ingreso_total,
    ROUND((r.gasto_grupo * 100.0) / t.gran_total, 2) AS pct_del_total
FROM resumen_cuartil r, total_tienda t
ORDER BY ingreso_total DESC;
"
res2_4 <- dbGetQuery(con, q2_4)
print(res2_4)


# ==========================================
# EJERCICIO 3 (Reto Integrador)
# ==========================================

q3 <- "
WITH 
fecha_referencia AS (
    SELECT MAX(InvoiceDate) AS max_fecha
    FROM Invoice
),
metricas_cliente AS (
    SELECT 
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        MAX(i.InvoiceDate) AS ultima_compra,
        ROUND((JULIANDAY(f.max_fecha) - JULIANDAY(MAX(i.InvoiceDate))) / 30.0, 1) AS meses_inactivo,
        ROUND(SUM(i.Total), 2) AS gasto_historico
    FROM Customer c
    JOIN Invoice i ON c.CustomerId = i.CustomerId
    CROSS JOIN fecha_referencia f
    GROUP BY c.CustomerId, cliente
),
segmentacion AS (
    SELECT 
        CustomerId,
        cliente,
        gasto_historico,
        meses_inactivo,
        CASE 
            WHEN meses_inactivo <= 6 THEN 'Activo'
            WHEN meses_inactivo > 6 AND meses_inactivo <= 12 THEN 'En Riesgo'
            ELSE 'Inactivo'
        END AS segmento,
        RANK() OVER (
            PARTITION BY 
                CASE 
                    WHEN meses_inactivo <= 6 THEN 'Activo'
                    WHEN meses_inactivo > 6 AND meses_inactivo <= 12 THEN 'En Riesgo'
                    ELSE 'Inactivo'
                END 
            ORDER BY gasto_historico DESC
        ) AS rank_segmento
    FROM metricas_cliente
)
SELECT 
    segmento,
    COUNT(CustomerId) AS n_clientes,
    ROUND(SUM(gasto_historico), 2) AS gasto_total_segmento,
    ROUND(AVG(gasto_historico), 2) AS gasto_promedio_cliente,
    GROUP_CONCAT(
        CASE WHEN rank_segmento <= 3 THEN cliente || ' ($' || gasto_historico || ')' END, 
        '; '
    ) AS top_3_clientes
FROM segmentacion
GROUP BY segmento
ORDER BY gasto_total_segmento DESC;
"
res3 <- dbGetQuery(con, q3)
print(res3)

# 3. Cerrar conexión
dbDisconnect(con)
3e

