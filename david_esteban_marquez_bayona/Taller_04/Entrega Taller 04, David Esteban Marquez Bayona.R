# ==============================================================================
# Taller práctico de SQL Parte 2: solución de los ejercicios de clase
# ==============================================================================

# ============================================================================
# 0. Preparación: paquetes, descarga de la base y conexión
# ============================================================================

# ------------------------------------------------------------------------------
# 0.1 Paquetes: se instalan automáticamente los que falten
# ------------------------------------------------------------------------------

paquetes   <- c("DBI", "RSQLite")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)

# ------------------------------------------------------------------------------
# 0.2 Base de datos: se descarga si no existe o si el archivo está vacío
# ------------------------------------------------------------------------------
# Igual que en la Parte 1. Además se valida que el archivo realmente tenga
# las tablas: si una conexión anterior falló, dbConnect() pudo haber creado
# un chinook.db vacío, y en ese caso se vuelve a descargar.

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

# Si quedó abierta una conexión de una ejecución anterior, se cierra
# (en Windows un archivo abierto no se puede sobrescribir)
if (exists("con") && inherits(con, "DBIConnection")) {
  try(dbDisconnect(con), silent = TRUE)
}

base_valida <- function(ruta) {
  if (!file.exists(ruta) || file.size(ruta) == 0) return(FALSE)
  tryCatch({
    con_tmp <- suppressWarnings(dbConnect(RSQLite::SQLite(), ruta, flags = RSQLite::SQLITE_RO))
    on.exit(dbDisconnect(con_tmp))
    all(c("Customer", "Employee", "Invoice", "InvoiceLine", "Track", "Genre") %in%
          dbListTables(con_tmp))
  }, error = function(e) FALSE)
}

if (!base_valida(ruta_db)) {
  options(timeout = max(300, getOption("timeout")))
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  if (!base_valida(ruta_db)) stop("La descarga de Chinook falló. Revisa tu conexión a internet.")
  cat("Base de datos descargada en:", normalizePath(ruta_db), "\n")
} else {
  cat("La base de datos ya existe:", normalizePath(ruta_db), "\n")
}

# ------------------------------------------------------------------------------
# 0.3 Conexión (solo lectura, para no modificar ni crear archivos por error)
# ------------------------------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), ruta_db, flags = RSQLite::SQLITE_RO)
cat("Conexión establecida.\n")
print(dbListTables(con))

# Función auxiliar para mostrar las tablas en consola
mostrar_tabla <- function(df, caption = NULL) {
  if (!is.null(caption)) cat("\n---", caption, "---\n")
  print(df, row.names = FALSE)
  invisible(df)
}

# ============================================================================
# EJERCICIO 1: JOINs avanzados, subconsultas correlacionadas y operadores
# de conjunto
# ============================================================================

# ----------------------------------------------------------------------------
# 1.1 Cliente, empleado de soporte y jefe (INNER JOIN + SELF JOIN)
# ----------------------------------------------------------------------------

# Customer se une con Employee por SupportRepId con INNER JOIN, porque todo
# cliente tiene representante. La segunda copia de Employee (alias j) se une
# con LEFT JOIN: con INNER JOIN, un empleado sin jefe (ReportsTo IS NULL)
# desaparecería del resultado y el COALESCE nunca tendría un NULL que
# reemplazar.

ej_1_1 <- dbGetQuery(con, "
  SELECT c.FirstName || ' ' || c.LastName                        AS cliente,
         e.FirstName || ' ' || e.LastName                        AS empleado_soporte,
         COALESCE(j.FirstName || ' ' || j.LastName, 'Sin jefe')  AS jefe
  FROM        Customer c
  INNER JOIN  Employee e ON c.SupportRepId = e.EmployeeId
  LEFT  JOIN  Employee j ON e.ReportsTo    = j.EmployeeId   -- SELF JOIN
  ORDER BY empleado_soporte, cliente;
")
cat(nrow(ej_1_1), "clientes\n")
mostrar_tabla(ej_1_1, caption = "Cliente, empleado de soporte y jefe")

# Los tres representantes de soporte (Jane Peacock, Margaret Park y Steve
# Johnson) reportan a Nancy Edwards, por eso en este resultado nunca aparece
# 'Sin jefe'. Para comprobar que el COALESCE funciona, el mismo SELF JOIN
# sobre toda la tabla Employee muestra que Andrew Adams (General Manager) no
# tiene jefe:

resultado <- dbGetQuery(con, "
  SELECT e.FirstName || ' ' || e.LastName                        AS empleado,
         e.Title                                                 AS cargo,
         COALESCE(j.FirstName || ' ' || j.LastName, 'Sin jefe')  AS jefe
  FROM   Employee e
  LEFT JOIN Employee j ON e.ReportsTo = j.EmployeeId;
")
mostrar_tabla(resultado, caption = "SELF JOIN: empleados y su jefe")

# ----------------------------------------------------------------------------
# 1.2 Clientes que gastan más que el promedio de los demás clientes de su
#     representante
# ----------------------------------------------------------------------------

# La subconsulta correlacionada se reevalúa para cada cliente c: calcula el
# gasto de los clientes con el mismo SupportRepId y distinto CustomerId (los
# "demás") y luego lo promedia. La comparación va en HAVING porque el gasto
# del cliente es un agregado.

ej_1_2 <- dbGetQuery(con, "
  SELECT c.FirstName || ' ' || c.LastName  AS cliente,
         c.SupportRepId                    AS rep_id,
         ROUND(SUM(i.Total), 2)            AS gasto_total,
         ROUND((
           SELECT AVG(t.gasto)
           FROM  (SELECT SUM(i2.Total) AS gasto
                  FROM   Customer c2
                  JOIN   Invoice  i2 ON c2.CustomerId = i2.CustomerId
                  WHERE  c2.SupportRepId = c.SupportRepId   -- mismo representante
                    AND  c2.CustomerId  <> c.CustomerId     -- excluye al propio cliente
                  GROUP BY c2.CustomerId) t
         ), 2)                             AS promedio_demas_mismo_rep
  FROM   Customer c
  JOIN   Invoice  i ON c.CustomerId = i.CustomerId
  GROUP BY c.CustomerId
  HAVING SUM(i.Total) > (
           SELECT AVG(t.gasto)
           FROM  (SELECT SUM(i2.Total) AS gasto
                  FROM   Customer c2
                  JOIN   Invoice  i2 ON c2.CustomerId = i2.CustomerId
                  WHERE  c2.SupportRepId = c.SupportRepId
                    AND  c2.CustomerId  <> c.CustomerId
                  GROUP BY c2.CustomerId) t
         )
  ORDER BY rep_id, gasto_total DESC;
")
cat(nrow(ej_1_2), "clientes superan el promedio de los demás clientes de su representante\n")
mostrar_tabla(ej_1_2, caption = "Subconsulta correlacionada: gasto vs. promedio de su representante")

# 18 de los 59 clientes cumplen la condición (6 por cada representante).
# Como el promedio excluye al propio cliente, cambia ligeramente de un
# cliente a otro dentro del mismo representante (por ejemplo, 39.37 vs.
# 39.47 para el representante 3).

# ----------------------------------------------------------------------------
# 1.3 Empleados que nunca han sido asignados como soporte (EXCEPT)
# ----------------------------------------------------------------------------

resultado <- dbGetQuery(con, "
  SELECT EmployeeId,
         FirstName || ' ' || LastName AS empleado,
         Title                        AS cargo
  FROM   Employee
  WHERE  EmployeeId IN (
           SELECT EmployeeId   FROM Employee
           EXCEPT
           SELECT SupportRepId FROM Customer
         )
  ORDER BY EmployeeId;
")
mostrar_tabla(resultado, caption = "EXCEPT: empleados sin clientes asignados")

# El EXCEPT devuelve los EmployeeId 1, 2, 6, 7 y 8; el WHERE ... IN solo
# sirve para recuperar sus nombres. Son los cargos gerenciales y de TI:
# únicamente los Sales Support Agent atienden clientes.

# ----------------------------------------------------------------------------
# 1.4 Clientes e ingresos por empleado (CTE)
# ----------------------------------------------------------------------------

resultado <- dbGetQuery(con, "
  WITH clientes_rep AS (
    SELECT SupportRepId, COUNT(*) AS n_clientes
    FROM   Customer
    GROUP BY SupportRepId
  ),
  ingresos_rep AS (
    SELECT c.SupportRepId, SUM(i.Total) AS ingreso_total
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.SupportRepId
  )
  SELECT e.FirstName || ' ' || e.LastName                                    AS empleado,
         cr.n_clientes,
         ROUND(ir.ingreso_total, 2)                                          AS ingreso_total,
         ROUND(ir.ingreso_total / cr.n_clientes, 2)                          AS ingreso_por_cliente,
         ROUND(100.0 * ir.ingreso_total / SUM(ir.ingreso_total) OVER (), 2)  AS pct_ingreso
  FROM   clientes_rep cr
  JOIN   ingresos_rep ir ON cr.SupportRepId = ir.SupportRepId
  JOIN   Employee     e  ON e.EmployeeId    = cr.SupportRepId
  ORDER BY ingreso_total DESC;
")
mostrar_tabla(resultado, caption = "CTE: clientes e ingresos por empleado de soporte")

# Interpretación. Sí: Jane Peacock atiende más clientes (21) y también
# genera más ingresos (833.04, 35.8 % del total), seguida de Margaret Park
# (20 clientes) y Steve Johnson (18). El orden por número de clientes y por
# ingreso coincide, pero no porque Jane tenga mejores clientes: el ingreso
# por cliente es casi idéntico entre los tres (≈ 39) y de hecho Steve
# Johnson tiene el mayor (40.01). La diferencia de ingresos se explica por
# el volumen de cartera, no por el valor de cada cliente. Con solo tres
# representantes, esto es una descripción, no evidencia de una relación
# general.

# ============================================================================
# EJERCICIO 2: Funciones de ventana y CTEs encadenadas
# ============================================================================

# ----------------------------------------------------------------------------
# 2.1 Ventas mensuales con LAG, LEAD y variación porcentual
# ----------------------------------------------------------------------------

# Variación porcentual respecto al mes anterior:
#   100 × (ventas_t - ventas_{t-1}) / ventas_{t-1}
# Se redondea solo al final para no arrastrar errores de redondeo.

ej_2_1 <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
           SUM(Total)                AS ventas
    FROM   Invoice
    GROUP BY anio_mes
  )
  SELECT anio_mes,
         ROUND(ventas, 2)                                    AS ventas,
         ROUND(LAG(ventas, 1)  OVER (ORDER BY anio_mes), 2)  AS ventas_mes_anterior,
         ROUND(LEAD(ventas, 1) OVER (ORDER BY anio_mes), 2)  AS ventas_mes_siguiente,
         ROUND(100.0 * (ventas - LAG(ventas, 1) OVER (ORDER BY anio_mes))
                     / LAG(ventas, 1) OVER (ORDER BY anio_mes), 2) AS variacion_pct
  FROM   ventas_mes
  ORDER BY anio_mes;
")
mostrar_tabla(ej_2_1, caption = "LAG y LEAD: ventas mensuales y variación porcentual")

# Meses con mayor caída y mayor subida porcentual
extremos <- ej_2_1[!is.na(ej_2_1$variacion_pct), ]
extremos <- extremos[extremos$variacion_pct %in% range(extremos$variacion_pct), ]
mostrar_tabla(extremos, caption = "Mayor caída y mayor subida mensual")

# El primer mes (2021-01) no tiene ventas_mes_anterior y el último (2025-12)
# no tiene ventas_mes_siguiente, como es de esperar con LAG/LEAD. La mayoría
# de meses venden exactamente 37.62 (variación 0 %), reflejo de que Chinook
# es una base sintética con un patrón de facturación muy regular. La mayor
# caída fue en 2023-11 (−36.84 %) y la mayor subida justo después, en
# 2023-12 (+58.33 %), al volver al nivel habitual.

# ----------------------------------------------------------------------------
# 2.2 Mes de mayor y menor venta por año (RANK con PARTITION BY)
# ----------------------------------------------------------------------------

# Se generan los dos rankings sobre la misma CTE y se pivota con agregación
# condicional. Como RANK() asigna la misma posición a los empates, se usa
# GROUP_CONCAT para no perder ningún mes empatado en el primer lugar.

resultado <- dbGetQuery(con, "
  WITH ventas_mes AS (
    SELECT SUBSTR(InvoiceDate, 1, 4) AS anio,
           SUBSTR(InvoiceDate, 1, 7) AS anio_mes,
           ROUND(SUM(Total), 2)      AS ventas
    FROM   Invoice
    GROUP BY anio, anio_mes
  ),
  rankings AS (
    SELECT anio, anio_mes, ventas,
           RANK() OVER (PARTITION BY anio ORDER BY ventas DESC) AS rank_top,
           RANK() OVER (PARTITION BY anio ORDER BY ventas ASC)  AS rank_bajo
    FROM   ventas_mes
  )
  SELECT anio,
         GROUP_CONCAT(CASE WHEN rank_top  = 1 THEN SUBSTR(anio_mes, 6, 2) END, ', ') AS mes_top,
         MAX(CASE WHEN rank_top  = 1 THEN ventas END)                                AS ventas_top,
         GROUP_CONCAT(CASE WHEN rank_bajo = 1 THEN SUBSTR(anio_mes, 6, 2) END, ', ') AS mes_bajo,
         MAX(CASE WHEN rank_bajo = 1 THEN ventas END)                                AS ventas_bajo
  FROM   rankings
  GROUP BY anio
  ORDER BY anio;
")
mostrar_tabla(resultado, caption = "RANK: mes de mayor y menor venta por año")

# En 2022, 2023 y 2025 hay un mes top y un mes bajo únicos (por ejemplo,
# 2023: abril con 51.62 y noviembre con 23.76). En 2021 once meses empatan
# en el máximo (37.62) y en 2024 ocho meses empatan en el mínimo; con
# ROW_NUMBER() en lugar de RANK() se habría elegido uno de ellos de forma
# arbitraria.

# ----------------------------------------------------------------------------
# 2.3 Género con mayores ingresos por año (CTEs encadenadas)
# ----------------------------------------------------------------------------

# Primera CTE: ingresos por año y género (UnitPrice × Quantity vía
# InvoiceLine → Track → Genre). Segunda CTE: ranking dentro de cada año. El
# filtro posicion = 1 va en un WHERE sobre la CTE, porque una función de
# ventana no se puede filtrar en el mismo nivel donde se calcula. Se agrega
# la participación del género en el ingreso del año como contexto.

resultado <- dbGetQuery(con, "
  WITH ingresos_genero AS (
    SELECT SUBSTR(i.InvoiceDate, 1, 4)     AS anio,
           g.Name                          AS genero,
           SUM(il.UnitPrice * il.Quantity) AS ingresos
    FROM   Invoice     i
    JOIN   InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN   Track       t  ON il.TrackId  = t.TrackId
    JOIN   Genre       g  ON t.GenreId   = g.GenreId
    GROUP BY anio, g.GenreId
  ),
  ranking_genero AS (
    SELECT anio, genero, ingresos,
           100.0 * ingresos / SUM(ingresos) OVER (PARTITION BY anio) AS pct_anio,
           RANK() OVER (PARTITION BY anio ORDER BY ingresos DESC)    AS posicion
    FROM   ingresos_genero
  )
  SELECT anio, genero,
         ROUND(ingresos, 2) AS ingresos,
         ROUND(pct_anio, 1) AS pct_ingreso_anio,
         posicion
  FROM   ranking_genero
  WHERE  posicion = 1
  ORDER BY anio;
")
mostrar_tabla(resultado, caption = "CTEs encadenadas: género con mayores ingresos por año")

# Rock es el género de mayores ingresos en los cinco años, con una
# participación estable de entre el 32 % (2022) y el 40 % (2021) del ingreso
# anual, aproximadamente el doble que el segundo género de cada año.

# ----------------------------------------------------------------------------
# 2.4 Concentración del ingreso por cuartiles de gasto (NTILE)
# ----------------------------------------------------------------------------

resultado <- dbGetQuery(con, "
  WITH gasto_cliente AS (
    SELECT c.CustomerId, SUM(i.Total) AS gasto_total
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    GROUP BY c.CustomerId
  ),
  cuartiles AS (
    SELECT CustomerId, gasto_total,
           NTILE(4) OVER (ORDER BY gasto_total) AS cuartil
    FROM   gasto_cliente
  ),
  por_cuartil AS (
    SELECT cuartil,
           COUNT(*)                   AS n_clientes,
           ROUND(MIN(gasto_total), 2) AS gasto_min,
           ROUND(MAX(gasto_total), 2) AS gasto_max,
           SUM(gasto_total)           AS ingreso
    FROM   cuartiles
    GROUP BY cuartil
  )
  SELECT cuartil, n_clientes, gasto_min, gasto_max,
         ROUND(ingreso, 2)                                                   AS ingreso,
         ROUND(100.0 * ingreso / (SELECT SUM(ingreso) FROM por_cuartil), 2) AS pct_ingreso
  FROM   por_cuartil
  ORDER BY cuartil;
")
mostrar_tabla(resultado, caption = "NTILE(4): ingreso por cuartil de gasto")

resultado <- dbGetQuery(con, "
  WITH gasto_cliente AS (
    SELECT CustomerId, SUM(Total) AS gasto_total
    FROM   Invoice
    GROUP BY CustomerId
  ),
  cuartiles AS (
    SELECT gasto_total, NTILE(4) OVER (ORDER BY gasto_total) AS cuartil
    FROM   gasto_cliente
  ),
  por_grupo AS (
    SELECT CASE WHEN cuartil = 4 THEN 'Q4 (superior)' ELSE 'Q1 a Q3 combinados' END AS grupo,
           COUNT(*)         AS n_clientes,
           SUM(gasto_total) AS ingreso
    FROM   cuartiles
    GROUP BY grupo
  )
  SELECT grupo, n_clientes,
         ROUND(ingreso, 2)                                                 AS ingreso,
         ROUND(100.0 * ingreso / (SELECT SUM(ingreso) FROM por_grupo), 2) AS pct_ingreso_total
  FROM   por_grupo
  ORDER BY pct_ingreso_total DESC;
")
mostrar_tabla(resultado, caption = "Q4 frente a Q1 a Q3 combinados")

# Interpretación. El cuartil superior (14 clientes, 23.7 % de la base)
# aporta el 26.40 % del ingreso, frente al 73.60 % de Q1 a Q3 combinados (45
# clientes). La concentración es mínima: con igualdad perfecta Q4 aportaría
# ≈ 23.7 %, así que no hay nada parecido a un patrón de Pareto. Esto es
# coherente con que 58 de los 59 clientes tienen exactamente 7 facturas y el
# gasto varía solo entre 36.64 y 49.62.

# Nota técnica: con 59 clientes, NTILE(4) forma grupos de 15, 15, 15 y 14.
# Además, muchos clientes empatan en 37.62 y quedan repartidos entre Q1, Q2
# y Q3 de forma arbitraria, así que los cuartiles bajos no deben leerse como
# segmentos con diferencias reales de gasto.

# ============================================================================
# EJERCICIO 3 (reto integrador): clientes en riesgo de abandono
# ============================================================================

# ----------------------------------------------------------------------------
# 3.1 Fecha de referencia ("presente" del análisis)
# ----------------------------------------------------------------------------

resultado <- dbGetQuery(con, "SELECT MAX(InvoiceDate) AS fecha_maxima FROM Invoice;")
mostrar_tabla(resultado, caption = "Fecha de la última factura registrada")

# ----------------------------------------------------------------------------
# 3.2 y 3.3 Recencia por cliente y segmentación con CASE
# ----------------------------------------------------------------------------

# Los meses se aproximan como días / 30, según el enunciado. Se clasifica
# con el valor sin redondear (el redondeo es solo para mostrar) y los
# límites quedan así: activo si lleva ≤ 6 meses, en riesgo si está en
# (6, 12] y inactivo si lleva > 12.

# Las CTEs se guardan en una variable de R para reutilizarlas en el punto
# 3.4 con paste0().

cte_segmentos <- "
  WITH fecha_ref AS (
    SELECT MAX(InvoiceDate) AS fecha_maxima FROM Invoice
  ),
  recencia AS (
    SELECT c.CustomerId,
           c.FirstName || ' ' || c.LastName  AS cliente,
           c.Country                         AS pais,
           DATE(MAX(i.InvoiceDate))          AS ultima_compra,
           SUM(i.Total)                      AS gasto_historico,
           (JULIANDAY(f.fecha_maxima) - JULIANDAY(MAX(i.InvoiceDate))) / 30 AS meses_sin_compra
    FROM   Customer c
    JOIN   Invoice  i ON c.CustomerId = i.CustomerId
    CROSS JOIN fecha_ref f
    GROUP BY c.CustomerId
  ),
  segmentos AS (
    SELECT *,
           CASE WHEN meses_sin_compra <= 6  THEN 'activo'
                WHEN meses_sin_compra <= 12 THEN 'en riesgo'
                ELSE 'inactivo'
           END AS segmento
    FROM   recencia
  )
"

ej_3_3 <- dbGetQuery(con, paste0(cte_segmentos, "
  SELECT cliente, pais, ultima_compra,
         ROUND(meses_sin_compra, 1) AS meses_sin_compra,
         ROUND(gasto_historico, 2)  AS gasto_historico,
         segmento
  FROM   segmentos
  ORDER BY meses_sin_compra DESC;
"))
mostrar_tabla(ej_3_3, caption = "Recencia y segmento de cada cliente")

# ----------------------------------------------------------------------------
# 3.4 Resumen por segmento y top 3 de gasto dentro de cada segmento
# ----------------------------------------------------------------------------

resultado <- dbGetQuery(con, paste0(cte_segmentos, "
  SELECT segmento,
         COUNT(*)                                                                    AS n_clientes,
         ROUND(AVG(gasto_historico), 2)                                              AS gasto_promedio,
         ROUND(SUM(gasto_historico), 2)                                              AS gasto_total,
         ROUND(100.0 * SUM(gasto_historico) / SUM(SUM(gasto_historico)) OVER (), 1) AS pct_gasto
  FROM   segmentos
  GROUP BY segmento
  ORDER BY CASE segmento WHEN 'activo' THEN 1 WHEN 'en riesgo' THEN 2 ELSE 3 END;
"))
mostrar_tabla(resultado, caption = "Resumen por segmento de recencia")

resultado <- dbGetQuery(con, paste0(cte_segmentos, "
  , ranking AS (
    SELECT segmento, cliente, pais, ultima_compra, meses_sin_compra, gasto_historico,
           RANK() OVER (PARTITION BY segmento ORDER BY gasto_historico DESC) AS rank_gasto
    FROM   segmentos
  )
  SELECT segmento, rank_gasto, cliente, pais, ultima_compra,
         ROUND(meses_sin_compra, 1) AS meses_sin_compra,
         ROUND(gasto_historico, 2)  AS gasto_historico
  FROM   ranking
  WHERE  rank_gasto <= 3
  ORDER BY CASE segmento WHEN 'activo' THEN 1 WHEN 'en riesgo' THEN 2 ELSE 3 END, rank_gasto;
"))
mostrar_tabla(resultado, caption = "RANK: top 3 de gasto histórico por segmento")

# Interpretación. Con fecha de referencia 2025-12-22 hay 31 clientes
# activos, 15 en riesgo y 13 inactivos. El gasto histórico promedio es
# prácticamente igual en los tres segmentos (≈ 39), así que la recencia no
# está asociada a clientes de menor valor.

# La campaña de retención debería priorizar el segmento en riesgo:

# - Representa el 25.7 % del gasto histórico de la tienda y todavía tiene
#   una relación reciente (entre 6 y 12 meses), por lo que es más barato
#   recuperarlo que a un cliente inactivo.
# - Concentra clientes de alto valor: Richard Cunningham (47.62) es el
#   segundo cliente de mayor gasto de toda la base, y Julia Barnett y Fynn
#   Zimmermann (43.62) también están por encima del promedio.
# - Varios están cerca de pasar a inactivos (Edward Francis y Phil Hughes
#   llevan más de 11 meses sin comprar).

# Los activos no requieren retención urgente, y para los inactivos conviene
# una campaña de reactivación puntual y más selectiva, empezando por Luis
# Rojas (46.62), el tercer cliente de mayor gasto de la base.

# Nota técnica: RANK() deja huecos tras un empate. En cada segmento hay dos
# clientes empatados en el puesto 2, por eso no aparece un puesto 3 pero sí
# tres clientes. Con DENSE_RANK() se incluiría también al siguiente nivel de
# gasto.

dbDisconnect(con)
cat("Conexión cerrada.\n")