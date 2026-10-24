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

cat("Tablas disponibles:\n")
dbListTables(con)


# ==========================================================================
# EJERCICIO 1: JOINs avanzados, subconsultas correlacionadas
#              y operadores de conjunto
# ==========================================================================


# 1. Cada cliente tiene un empleado de soporte asignado (Customer.SupportRepId
#    -> Employee.EmployeeId) y cada empleado puede tener un jefe
#    (Employee.ReportsTo -> Employee.EmployeeId). Combinamos Customer con
#    Employee mediante INNER JOIN y luego una segunda vez la tabla Employee
#    (alias distinto, es decir un SELF JOIN) para obtener el jefe del
#    empleado de soporte. Esa segunda unión se hace con LEFT JOIN porque no
#    todos los empleados tienen jefe (ReportsTo puede ser NULL); COALESCE
#    se encarga de mostrar 'Sin jefe' en esos casos.

Cliente_empleado_jefe <- "
SELECT
    c.FirstName || ' ' || c.LastName AS cliente,
    e.FirstName || ' ' || e.LastName AS empleado_soporte,
    COALESCE(m.FirstName || ' ' || m.LastName, 'Sin jefe') AS jefe_del_empleado
FROM Customer AS c
INNER JOIN Employee AS e
    ON c.SupportRepId = e.EmployeeId
LEFT JOIN Employee AS m
    ON e.ReportsTo = m.EmployeeId
ORDER BY cliente;
"

dbGetQuery(con, Cliente_empleado_jefe)


# 2. Clientes cuyo gasto total supera el gasto promedio de los DEMÁS
#    clientes atendidos por el mismo empleado de soporte (no el promedio
#    general ni el del país). La subconsulta es correlacionada porque
#    depende de c.SupportRepId y c.CustomerId de la consulta externa, y
#    excluye al propio cliente con c2.CustomerId <> c.CustomerId.

Clientes_sobre_promedio_representante <- "
SELECT
    c.FirstName || ' ' || c.LastName AS cliente,
    c.SupportRepId          AS id_empleado,
    ROUND(SUM(i.Total), 2)  AS gasto_total
FROM Customer AS c
JOIN Invoice AS i
    ON c.CustomerId = i.CustomerId
GROUP BY c.CustomerId
HAVING SUM(i.Total) > (
    -- promedio de gasto de los demás clientes del mismo representante
    SELECT AVG(gasto_otro.total_otro)
    FROM (
        SELECT c2.CustomerId, SUM(i2.Total) AS total_otro
        FROM Customer AS c2
        JOIN Invoice AS i2
            ON c2.CustomerId = i2.CustomerId
        WHERE c2.SupportRepId = c.SupportRepId
          AND c2.CustomerId  <> c.CustomerId
        GROUP BY c2.CustomerId
    ) AS gasto_otro
)
ORDER BY id_empleado, gasto_total DESC;
"

dbGetQuery(con, Clientes_sobre_promedio_representante)


# 3. Empleados que NUNCA han sido asignados como soporte de ningún cliente
#    (aparecen en Employee pero jamás en Customer.SupportRepId). Se resuelve
#    con EXCEPT: todos los empleados, menos los que sí tienen al menos un
#    cliente asignado.

Empleados_sin_clientes_asignados <- "
SELECT EmployeeId, FirstName || ' ' || LastName AS empleado, Title AS cargo
FROM Employee

EXCEPT

SELECT e.EmployeeId, e.FirstName || ' ' || e.LastName, e.Title
FROM Employee AS e
JOIN Customer AS c
    ON e.EmployeeId = c.SupportRepId;
"

dbGetQuery(con, Empleados_sin_clientes_asignados)


# 4. Con una CTE, calculamos cuántos clientes atiende cada empleado y el
#    ingreso total generado por esos clientes. Se encadenan dos CTEs:
#    una para el conteo de clientes y otra para el ingreso, y se unen
#    al final.

Clientes_ingresos_por_empleado <- "
WITH clientes_por_empleado AS (
    SELECT
        e.EmployeeId,
        e.FirstName || ' ' || e.LastName AS empleado,
        COUNT(DISTINCT c.CustomerId)     AS numero_clientes
    FROM Employee AS e
    JOIN Customer AS c
        ON c.SupportRepId = e.EmployeeId
    GROUP BY e.EmployeeId
),
ingresos_por_empleado AS (
    SELECT
        c.SupportRepId          AS EmployeeId,
        ROUND(SUM(i.Total), 2)  AS ingreso_total
    FROM Customer AS c
    JOIN Invoice AS i
        ON c.CustomerId = i.CustomerId
    GROUP BY c.SupportRepId
)
SELECT
    cpe.empleado,
    cpe.numero_clientes,
    ipe.ingreso_total
FROM clientes_por_empleado AS cpe
JOIN ingresos_por_empleado AS ipe
    ON cpe.EmployeeId = ipe.EmployeeId
ORDER BY ipe.ingreso_total DESC;
"

resultado_clientes_ingresos <- dbGetQuery(con, Clientes_ingresos_por_empleado)
resultado_clientes_ingresos

# Interpretación: en los resultados, el orden por número de clientes
# coincide exactamente con el orden por ingreso total (el empleado con más
# clientes asignados es también el de mayor ingreso, y así sucesivamente).
# Es decir, en este caso sí, el empleado que atiende más clientes es
# también el que genera más ingresos. Sin embargo, esto no tiene por qué
# cumplirse siempre: un empleado podría tener muchos clientes de bajo gasto
# y otro pocos clientes de alto gasto, rompiendo esa relación.


# ==========================================================================
# EJERCICIO 2: Funciones de ventana y CTEs encadenadas de varios niveles
# ==========================================================================


# 1. Total de ventas por año y mes (SUBSTR(InvoiceDate, 1, 7)). Con LAG y
#    LEAD (en la misma fila) agregamos las ventas del mes anterior y del
#    mes siguiente, y calculamos la variación PORCENTUAL (no absoluta)
#    respecto al mes anterior: (mes_actual - mes_anterior) / mes_anterior.

Ventas_mes_variacion <- "
WITH ventas_mes AS (
    SELECT
        SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
        ROUND(SUM(Total), 2)      AS ventas
    FROM Invoice
    GROUP BY anio_mes
)
SELECT
    anio_mes,
    ventas,
    LAG(ventas)  OVER (ORDER BY anio_mes) AS ventas_mes_anterior,
    LEAD(ventas) OVER (ORDER BY anio_mes) AS ventas_mes_siguiente,
    ROUND(
        100.0 * (ventas - LAG(ventas) OVER (ORDER BY anio_mes))
        / LAG(ventas) OVER (ORDER BY anio_mes),
        2
    ) AS variacion_pct_mes_anterior
FROM ventas_mes
ORDER BY anio_mes;
"

dbGetQuery(con, Ventas_mes_variacion)


# 2. Mes de mayor y menor venta de cada año, en una sola fila por año.
#    Se construye la CTE 'ventas_mes' y sobre ella una segunda CTE
#    'ranking_mensual' con DOS RANK(): uno ordenando de mayor a menor
#    (rank_top) y otro de menor a mayor (rank_bottom), ambos particionados
#    por año. Luego se agrupa por año usando CASE + MIN/MAX para "pivotear"
#    los dos rankings en una sola fila. Se usa GROUP BY (y no un JOIN entre
#    los dos subconjuntos con ranking = 1) porque, si un año tiene VARIOS
#    meses empatados en el primer o último lugar, un JOIN multiplicaría
#    las filas (probado: sin este ajuste, 2021 salía con 11 filas en vez
#    de 1, por 11 meses empatados). Con MIN(anio_mes) nos quedamos con el
#    primero de los empatados, de forma determinística.

Mes_top_y_mes_bajo_por_anio <- "
WITH ventas_mes AS (
    SELECT
        SUBSTR(InvoiceDate, 1, 4) AS anio,
        SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
        ROUND(SUM(Total), 2)      AS ventas
    FROM Invoice
    GROUP BY anio_mes
),
ranking_mensual AS (
    SELECT
        anio,
        anio_mes,
        ventas,
        RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS rank_top,
        RANK() OVER (PARTITION BY anio ORDER BY ventas ASC)  AS rank_bottom
    FROM ventas_mes
)
SELECT
    anio,
    MIN(CASE WHEN rank_top    = 1 THEN anio_mes END) AS mes_mayor_venta,
    MAX(CASE WHEN rank_top    = 1 THEN ventas   END) AS venta_mes_mayor,
    MIN(CASE WHEN rank_bottom = 1 THEN anio_mes END) AS mes_menor_venta,
    MAX(CASE WHEN rank_bottom = 1 THEN ventas   END) AS venta_mes_menor
FROM ranking_mensual
GROUP BY anio
ORDER BY anio;
"

dbGetQuery(con, Mes_top_y_mes_bajo_por_anio)


# 3. Género musical con mayores ingresos (UnitPrice * Quantity) por año,
#    recorriendo InvoiceLine -> Track -> Genre y usando Invoice solo para
#    obtener el año de cada línea de factura. Se encadena una segunda CTE
#    ('ranking_genero') que aplica RANK() OVER (PARTITION BY anio ORDER BY
#    ingresos DESC) y luego se filtra la posición 1 con un WHERE sobre el
#    resultado de esa CTE.

Genero_top_ingresos_por_anio <- "
WITH ingresos_genero_anio AS (
    SELECT
        SUBSTR(i.InvoiceDate, 1, 4)               AS anio,
        g.Name                                     AS genero,
        ROUND(SUM(il.UnitPrice * il.Quantity), 2) AS ingresos
    FROM InvoiceLine AS il
    JOIN Invoice AS i ON il.InvoiceId = i.InvoiceId
    JOIN Track   AS t ON il.TrackId   = t.TrackId
    JOIN Genre   AS g ON t.GenreId    = g.GenreId
    GROUP BY anio, g.Name
),
ranking_genero AS (
    SELECT
        anio,
        genero,
        ingresos,
        RANK() OVER (PARTITION BY anio ORDER BY ingresos DESC) AS ranking
    FROM ingresos_genero_anio
)
SELECT anio, genero, ingresos
FROM ranking_genero
WHERE ranking = 1
ORDER BY anio;
"

dbGetQuery(con, Genero_top_ingresos_por_anio)


# 4. Gasto total por cliente dividido en cuartiles con NTILE(4), y luego
#    (con una CTE adicional 'resumen_cuartil' que agrega por cuartil) el
#    porcentaje del ingreso total de la tienda que representa el cuartil
#    superior (Q4) frente a los otros tres cuartiles combinados.

# 4a. Desglose de contexto: número de clientes e ingreso de cada cuartil.
Cuartiles_desglose <- "
WITH gasto_cliente AS (
    SELECT c.CustomerId, ROUND(SUM(i.Total), 2) AS gasto_total
    FROM Customer AS c
    JOIN Invoice  AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
cuartiles_cliente AS (
    SELECT
        CustomerId,
        gasto_total,
        NTILE(4) OVER (ORDER BY gasto_total) AS cuartil
    FROM gasto_cliente
),
resumen_cuartil AS (
    SELECT
        cuartil,
        COUNT(*)                  AS numero_clientes,
        ROUND(SUM(gasto_total),2) AS ingreso_cuartil
    FROM cuartiles_cliente
    GROUP BY cuartil
)
SELECT cuartil, numero_clientes, ingreso_cuartil
FROM resumen_cuartil
ORDER BY cuartil;
"

dbGetQuery(con, Cuartiles_desglose)

# 4b. Respuesta a la pregunta: Q4 (25% que más gasta) vs Q1-Q3 (75% restante).
Cuartiles_q4_vs_resto <- "
WITH gasto_cliente AS (
    SELECT c.CustomerId, ROUND(SUM(i.Total), 2) AS gasto_total
    FROM Customer AS c
    JOIN Invoice  AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
cuartiles_cliente AS (
    SELECT
        CustomerId,
        gasto_total,
        NTILE(4) OVER (ORDER BY gasto_total) AS cuartil
    FROM gasto_cliente
),
resumen_cuartil AS (
    SELECT
        cuartil,
        COUNT(*)                  AS numero_clientes,
        ROUND(SUM(gasto_total),2) AS ingreso_cuartil
    FROM cuartiles_cliente
    GROUP BY cuartil
)
SELECT
    CASE WHEN cuartil = 4 THEN 'Q4 (25% que mas gasta)' ELSE 'Q1-Q3 (75% restante)' END AS grupo,
    SUM(numero_clientes)                                                              AS numero_clientes,
    ROUND(SUM(ingreso_cuartil), 2)                                                    AS ingreso_total_grupo,
    ROUND(
        100.0 * SUM(ingreso_cuartil)
        / (SELECT SUM(ingreso_cuartil) FROM resumen_cuartil),
        2
    ) AS pct_ingreso_total
FROM resumen_cuartil
GROUP BY grupo
ORDER BY grupo DESC;
"

resultado_cuartiles <- dbGetQuery(con, Cuartiles_q4_vs_resto)
resultado_cuartiles

# Interpretación: el 25% de clientes que más gasta (Q4) concentra
# aproximadamente el 26% del ingreso total de la tienda, mientras que el
# 75% restante (Q1-Q3) genera cerca del 74%. La concentración existe pero
# es moderada: no estamos ante un caso extremo tipo "80/20" donde unos
# pocos clientes sostienen casi todo el negocio; el ingreso está bastante
# repartido entre toda la base de clientes.


# ==========================================================================
# EJERCICIO 3 (reto integrador, opcional): clientes en riesgo de abandono
# ==========================================================================


# 1. Fecha de la última factura registrada en toda la base de datos: es
#    el "presente" del análisis, ya que los datos de Chinook son históricos
#    (no hay ventas "de hoy" reales con las que comparar).

Fecha_maxima_factura <- "
SELECT MAX(InvoiceDate) AS fecha_maxima
FROM Invoice;
"

dbGetQuery(con, Fecha_maxima_factura)


# 2. Con una CTE, calculamos para cada cliente la fecha de su última compra
#    y los meses transcurridos entre esa fecha y la fecha máxima del punto
#    anterior. JULIANDAY() convierte una fecha a un número (día juliano);
#    restando dos días julianos obtenemos la diferencia en DÍAS, y
#    dividiendo entre 30 la aproximamos a MESES. La fecha máxima se obtiene
#    con una subconsulta escalar (SELECT MAX(InvoiceDate) FROM Invoice),
#    igual que en el punto 1, pero embebida aquí para que quede fija y
#    disponible dentro de la CTE.

Ultima_compra_meses_sin_comprar <- "
WITH ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        MAX(i.InvoiceDate)     AS ultima_compra,
        ROUND(SUM(i.Total), 2) AS gasto_total,
        ROUND(
            (JULIANDAY((SELECT MAX(InvoiceDate) FROM Invoice))
             - JULIANDAY(MAX(i.InvoiceDate))) / 30,
            1
        ) AS meses_sin_comprar
    FROM Customer AS c
    JOIN Invoice  AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
)
SELECT CustomerId, cliente, ultima_compra, gasto_total, meses_sin_comprar
FROM ultima_compra_cliente
ORDER BY meses_sin_comprar DESC;
"

dbGetQuery(con, Ultima_compra_meses_sin_comprar)


# 3. Clasificamos a cada cliente en 'activo' / 'en riesgo' / 'inactivo' con
#    CASE, encadenando una segunda CTE ('clientes_segmentados') sobre la
#    CTE del punto anterior. gasto_total también se incluye aquí porque lo
#    necesitaremos en el punto 4.

Clientes_segmentados_sql <- "
WITH ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        MAX(i.InvoiceDate)     AS ultima_compra,
        ROUND(SUM(i.Total), 2) AS gasto_total,
        ROUND(
            (JULIANDAY((SELECT MAX(InvoiceDate) FROM Invoice))
             - JULIANDAY(MAX(i.InvoiceDate))) / 30,
            1
        ) AS meses_sin_comprar
    FROM Customer AS c
    JOIN Invoice  AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
clientes_segmentados AS (
    SELECT
        cliente,
        ultima_compra,
        gasto_total,
        meses_sin_comprar,
        CASE
            WHEN meses_sin_comprar <= 6  THEN 'activo'
            WHEN meses_sin_comprar <= 12 THEN 'en riesgo'
            ELSE 'inactivo'
        END AS segmento
    FROM ultima_compra_cliente
)
SELECT cliente, ultima_compra, gasto_total, meses_sin_comprar, segmento
FROM clientes_segmentados
ORDER BY segmento, meses_sin_comprar DESC;
"

dbGetQuery(con, Clientes_segmentados_sql)


# 4. Para cada segmento: número de clientes, gasto histórico promedio y
#    gasto histórico total (4a). Adicionalmente, dentro de cada segmento,
#    usamos RANK() OVER (PARTITION BY segmento ORDER BY gasto_total DESC)
#    para identificar a los 3 clientes de mayor gasto histórico (4b): los
#    que más valor perdería la tienda si se van. Ambas consultas repiten
#    la misma cadena de CTEs (ultima_compra_cliente -> clientes_segmentados)
#    porque un WITH solo vive dentro de la sentencia SELECT en la que se
#    declara; no se puede "guardar" para reutilizarlo en otra consulta
#    separada sin volver a escribirlo.

# 4a. Resumen agregado por segmento.
Resumen_por_segmento <- "
WITH ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        MAX(i.InvoiceDate)     AS ultima_compra,
        ROUND(SUM(i.Total), 2) AS gasto_total,
        ROUND(
            (JULIANDAY((SELECT MAX(InvoiceDate) FROM Invoice))
             - JULIANDAY(MAX(i.InvoiceDate))) / 30,
            1
        ) AS meses_sin_comprar
    FROM Customer AS c
    JOIN Invoice  AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
clientes_segmentados AS (
    SELECT
        cliente,
        gasto_total,
        CASE
            WHEN meses_sin_comprar <= 6  THEN 'activo'
            WHEN meses_sin_comprar <= 12 THEN 'en riesgo'
            ELSE 'inactivo'
        END AS segmento
    FROM ultima_compra_cliente
)
SELECT
    segmento,
    COUNT(*)                   AS numero_clientes,
    ROUND(AVG(gasto_total), 2) AS gasto_promedio,
    ROUND(SUM(gasto_total), 2) AS gasto_total_segmento
FROM clientes_segmentados
GROUP BY segmento
ORDER BY gasto_total_segmento DESC;
"

resultado_resumen_segmento <- dbGetQuery(con, Resumen_por_segmento)
resultado_resumen_segmento

# 4b. Top 3 clientes de mayor gasto histórico dentro de cada segmento.
Top3_gasto_por_segmento <- "
WITH ultima_compra_cliente AS (
    SELECT
        c.CustomerId,
        c.FirstName || ' ' || c.LastName AS cliente,
        MAX(i.InvoiceDate)     AS ultima_compra,
        ROUND(SUM(i.Total), 2) AS gasto_total,
        ROUND(
            (JULIANDAY((SELECT MAX(InvoiceDate) FROM Invoice))
             - JULIANDAY(MAX(i.InvoiceDate))) / 30,
            1
        ) AS meses_sin_comprar
    FROM Customer AS c
    JOIN Invoice  AS i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
),
clientes_segmentados AS (
    SELECT
        cliente,
        gasto_total,
        CASE
            WHEN meses_sin_comprar <= 6  THEN 'activo'
            WHEN meses_sin_comprar <= 12 THEN 'en riesgo'
            ELSE 'inactivo'
        END AS segmento
    FROM ultima_compra_cliente
),
ranking_gasto_segmento AS (
    SELECT
        segmento,
        cliente,
        gasto_total,
        RANK() OVER (PARTITION BY segmento ORDER BY gasto_total DESC) AS ranking_gasto
    FROM clientes_segmentados
)
SELECT segmento, cliente, gasto_total, ranking_gasto
FROM ranking_gasto_segmento
WHERE ranking_gasto <= 3
ORDER BY segmento, ranking_gasto;
"

dbGetQuery(con, Top3_gasto_por_segmento)

# Interpretación: el gasto histórico PROMEDIO por cliente es prácticamente
# igual en los tres segmentos (activo ≈ $39.56, en riesgo ≈ $39.82,
# inactivo ≈ $38.85); es decir, dejar de comprar no está relacionado con
# haber sido un cliente "barato". El segmento 'en riesgo' es el que debería
# priorizarse en una campaña de retención: son clientes con un gasto
# histórico tan alto como el de los activos (incluso ligeramente mayor en
# promedio), que todavía no se han ido del todo (llevan entre 6 y 12 meses
# sin comprar, no más de un año), por lo que aún son recuperables. Atender
# primero al segmento 'inactivo' sería menos eficiente: ya llevan más de un
# año sin comprar, por lo que es más probable que ya se hayan ido
# definitivamente y el costo de recuperarlos sea mayor que el de retener a
# quienes apenas empiezan a alejarse.


# ==========================================================================
# Cerramos la conexión
# ==========================================================================

dbDisconnect(con)
cat("Conexión cerrada.\n")

