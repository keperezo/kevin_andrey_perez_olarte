# =============================================================================
# consultas_sql.R — Preguntas de la sección 6 del taller
# (Cada consulta irá en taller_1.qmd con su resultado e interpretación)
# Requiere haber corrido scraper.R y correspondencia.R
# =============================================================================

library(DBI); library(RSQLite)

ID_PRINCIPAL <- "20261011T101202"
con <- dbConnect(SQLite(), "comparador_precios.sqlite")
q   <- function(sql) dbGetQuery(con, sql, params = list(ID_PRINCIPAL))
q0  <- function(sql) dbGetQuery(con, sql)          # consultas sin parámetro

# -----------------------------------------------------------------------------
# P1. ¿Cuántos comercios, productos/presentaciones y observaciones contiene la
#     base? ¿Cuántos productos aporta cada comercio?
# -----------------------------------------------------------------------------
sql_1a <- "
SELECT
  (SELECT COUNT(DISTINCT comercio)            FROM observaciones) AS n_comercios,
  (SELECT COUNT(DISTINCT comercio || '|' || sku) FROM observaciones) AS n_productos_categoria,
  (SELECT COUNT(*)                            FROM productos)     AS n_presentaciones_comparables,
  (SELECT COUNT(*)                            FROM observaciones) AS n_observaciones,
  (SELECT COUNT(DISTINCT id_ejecucion)        FROM observaciones) AS n_ejecuciones"
sql_1b <- "
SELECT comercio,
       COUNT(DISTINCT sku)                           AS n_productos,
       SUM(comparable)                               AS n_comparables
FROM v_observaciones
WHERE id_ejecucion = ?
GROUP BY comercio
ORDER BY n_productos DESC"
r1a <- q0(sql_1a); r1b <- q(sql_1b)

# -----------------------------------------------------------------------------
# P2. En una misma ejecución, ¿hay al menos 24 observaciones comparables
#     (8 productos x 3 comercios)? ¿Hay registros repetidos del mismo producto
#     y comercio en esa ejecución?
# -----------------------------------------------------------------------------
sql_2a <- "
SELECT COUNT(*)                                        AS n_obs_comparables,
       COUNT(DISTINCT id_producto)                     AS n_presentaciones,
       COUNT(DISTINCT comercio)                        AS n_comercios,
       COUNT(DISTINCT id_producto || '|' || comercio)  AS n_pares_producto_comercio,
       CASE WHEN COUNT(DISTINCT id_producto || '|' || comercio) >= 24
            THEN 'cumple' ELSE 'no cumple' END        AS minimo_24
FROM v_observaciones
WHERE comparable = 1 AND id_ejecucion = ?"
sql_2b <- "
SELECT id_producto, comercio, COUNT(*) AS n_registros, GROUP_CONCAT(sku) AS skus
FROM v_observaciones
WHERE comparable = 1 AND id_ejecucion = ?
GROUP BY id_producto, comercio
HAVING COUNT(*) > 1"
r2a <- q(sql_2a); r2b <- q(sql_2b)

# -----------------------------------------------------------------------------
# P3. Para cada presentación comparable: precio mínimo, máximo y promedio,
#     y comercio con el menor precio (incluye empates).
# -----------------------------------------------------------------------------
sql_3 <- "
WITH base AS (
  SELECT v.id_producto, p.nombre_std, v.comercio, v.precio
  FROM v_observaciones v
  JOIN productos p ON p.id_producto = v.id_producto
  WHERE v.id_ejecucion = ? AND v.precio IS NOT NULL
),
est AS (
  SELECT id_producto, nombre_std,
         MIN(precio)            AS precio_min,
         MAX(precio)            AS precio_max,
         ROUND(AVG(precio), 0)  AS precio_promedio,
         COUNT(*)               AS n_comercios
  FROM base GROUP BY id_producto, nombre_std
)
SELECT e.id_producto, e.nombre_std, e.precio_min, e.precio_max, e.precio_promedio,
       GROUP_CONCAT(b.comercio, ', ') AS comercio_mas_barato,
       COUNT(b.comercio)              AS n_empatados
FROM est e
JOIN base b ON b.id_producto = e.id_producto AND b.precio = e.precio_min
GROUP BY e.id_producto
ORDER BY e.id_producto"
r3 <- q(sql_3)

# -----------------------------------------------------------------------------
# P4. Ahorro en COP y en % al elegir la oferta más económica de cada
#     presentación:  ahorro = Pmax - Pmin ;  ahorro % = 100 (Pmax - Pmin) / Pmax
# -----------------------------------------------------------------------------
sql_4 <- "
SELECT v.id_producto, p.nombre_std,
       MAX(v.precio)                                             AS precio_max,
       MIN(v.precio)                                             AS precio_min,
       MAX(v.precio) - MIN(v.precio)                             AS ahorro_cop,
       ROUND(100.0 * (MAX(v.precio) - MIN(v.precio)) / MAX(v.precio), 1) AS ahorro_pct
FROM v_observaciones v
JOIN productos p ON p.id_producto = v.id_producto
WHERE v.id_ejecucion = ? AND v.precio IS NOT NULL
GROUP BY v.id_producto
ORDER BY v.id_producto"
r4 <- q(sql_4)

# -----------------------------------------------------------------------------
# P5. Para tamaños distintos del mismo producto, ¿qué presentación y comercio
#     tienen el menor precio por unidad? (ranking separado por unidad: no se
#     mezclan mL, g y unidades)
# -----------------------------------------------------------------------------
sql_5 <- "
WITH familias AS (            -- productos con más de un tamaño en la misma unidad
  SELECT marca_std, unidad
  FROM productos
  GROUP BY marca_std, unidad
  HAVING COUNT(DISTINCT contenido) > 1
)
SELECT p.marca_std, p.unidad, p.nombre_std, v.comercio,
       v.precio, p.contenido,
       ROUND(v.precio / p.contenido, 1)                         AS precio_por_unidad,
       RANK() OVER (PARTITION BY p.marca_std, p.unidad
                    ORDER BY v.precio / p.contenido)            AS ranking
FROM v_observaciones v
JOIN productos p ON p.id_producto = v.id_producto
JOIN familias f  ON f.marca_std = p.marca_std AND f.unidad = p.unidad
WHERE v.id_ejecucion = ? AND v.precio IS NOT NULL
ORDER BY p.marca_std, ranking"
r5 <- q(sql_5)

# -----------------------------------------------------------------------------
# P6. Mayores diferencias de precio entre comercios, ordenadas por ahorro %
#     (con el comercio más caro y el más barato).
# -----------------------------------------------------------------------------
sql_6 <- "
WITH base AS (
  SELECT v.id_producto, p.nombre_std, v.comercio, v.precio
  FROM v_observaciones v
  JOIN productos p ON p.id_producto = v.id_producto
  WHERE v.id_ejecucion = ? AND v.precio IS NOT NULL
),
est AS (
  SELECT id_producto, nombre_std, MIN(precio) AS pmin, MAX(precio) AS pmax
  FROM base GROUP BY id_producto, nombre_std
)
SELECT e.id_producto, e.nombre_std,
       (SELECT GROUP_CONCAT(comercio, ', ') FROM base b
         WHERE b.id_producto = e.id_producto AND b.precio = e.pmax) AS comercio_mas_caro,
       (SELECT GROUP_CONCAT(comercio, ', ') FROM base b
         WHERE b.id_producto = e.id_producto AND b.precio = e.pmin) AS comercio_mas_barato,
       e.pmax - e.pmin                                   AS ahorro_cop,
       ROUND(100.0 * (e.pmax - e.pmin) / e.pmax, 1)      AS ahorro_pct
FROM est e
ORDER BY ahorro_pct DESC"
r6 <- q(sql_6)

# -----------------------------------------------------------------------------
# P7. ¿Qué datos faltantes o inconsistentes afectan la comparación y cuántos son?
# -----------------------------------------------------------------------------
sql_7a <- "                 -- faltantes por comercio en toda la categoría
SELECT comercio,
       COUNT(*)                                        AS n,
       SUM(precio IS NULL)                             AS sin_precio,
       SUM(marca IS NULL OR TRIM(marca) = '')          AS sin_marca,
       SUM(subcategoria IS NULL)                       AS sin_subcategoria,
       SUM(disponible IS NULL)                         AS sin_disponibilidad,
       SUM(sku LIKE 'sin_sku:%')                       AS sin_sku,
       SUM(promocion = 1)                              AS en_promocion
FROM observaciones
WHERE id_ejecucion = ?
GROUP BY comercio"
sql_7b <- "                 -- inconsistencias: precio antes menor o igual al precio
SELECT comercio, sku, nombre_original, precio, precio_antes
FROM observaciones
WHERE id_ejecucion = ? AND precio_antes IS NOT NULL AND precio_antes <= precio"
sql_7c <- "                 -- posibles duplicados: mismo nombre con SKU distinto
SELECT comercio, nombre_original, COUNT(*) AS n, GROUP_CONCAT(sku, ', ') AS skus
FROM observaciones
WHERE id_ejecucion = ?
GROUP BY comercio, LOWER(TRIM(nombre_original))
HAVING COUNT(*) > 1"
sql_7d <- "                 -- condiciones que afectan la comparación de los comparables
SELECT comercio,
       SUM(promocion = 1)        AS comparables_en_promocion,
       SUM(disponible = 0)       AS comparables_agotados,
       SUM(disponible IS NULL)   AS comparables_sin_dato_disponibilidad
FROM v_observaciones
WHERE comparable = 1 AND id_ejecucion = ?
GROUP BY comercio"
r7a <- q(sql_7a); r7b <- q(sql_7b); r7c <- q(sql_7c); r7d <- q(sql_7d)

# -----------------------------------------------------------------------------
# P8. ¿Cuántos productos se obtuvieron por comercio y ejecución? ¿Qué proporción
#     tiene descripción, marca y contenido identificables?
#     Contenido identificable: el nombre trae un número seguido de ml, g o mg,
#     o una cantidad del tipo "x 12".
# -----------------------------------------------------------------------------
sql_8 <- "
SELECT o.id_ejecucion, o.comercio, e.estado,
       COUNT(*) AS n_productos,
       ROUND(100.0 * AVG(o.descripcion_original IS NOT NULL
                         AND TRIM(o.descripcion_original) <> ''), 1) AS pct_descripcion,
       ROUND(100.0 * AVG(o.marca IS NOT NULL AND TRIM(o.marca) <> ''), 1) AS pct_marca,
       ROUND(100.0 * AVG(
         LOWER(o.nombre_original) GLOB '*[0-9]ml*'  OR LOWER(o.nombre_original) GLOB '*[0-9] ml*' OR
         LOWER(o.nombre_original) GLOB '*[0-9]g*'   OR LOWER(o.nombre_original) GLOB '*[0-9] g*'  OR
         LOWER(o.nombre_original) GLOB '*[0-9]mg*'  OR LOWER(o.nombre_original) GLOB '*[0-9] mg*' OR
         LOWER(o.nombre_original) GLOB '*x [0-9]*'  OR LOWER(o.nombre_original) GLOB '*x[0-9]*'
       ), 1) AS pct_contenido
FROM observaciones o
JOIN ejecuciones e ON e.id_ejecucion = o.id_ejecucion AND e.comercio = o.comercio
GROUP BY o.id_ejecucion, o.comercio
ORDER BY o.id_ejecucion, o.comercio"
r8 <- q0(sql_8)

# ---- Mostrar resultados ------------------------------------------------------
resultados <- list(P1a = r1a, P1b = r1b, P2a = r2a, P2b = r2b, P3 = r3, P4 = r4,
                   P5 = r5, P6 = r6, P7a = r7a, P7b = r7b, P7c = r7c, P7d = r7d, P8 = r8)
for (nm in names(resultados)) {
  cat("\n=====", nm, "=====\n")
  print(resultados[[nm]])
}

dbDisconnect(con)
