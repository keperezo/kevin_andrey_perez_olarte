# consultas_sql.R -- Taller 1: las 8 preguntas del taller en SQL (SQLite)
#
# Uso:
#   library(DBI); library(RSQLite)
#   con <- dbConnect(SQLite(), "comparador_precios.sqlite")
#   source("consultas_sql.R")
#   dbGetQuery(con, CONSULTAS$q3)          # una consulta
#   ejecutar_consultas(con)                # todas, con su título
#
# Convenciones:
# - v_comparables ya filtra comparable = 1 (incluido, con precio, con producto_id común).
# - Los precios y ahorros se calculan por ejecución: nunca se mezclan fechas distintas.
# - Los empates se listan todos (no se elige uno al azar).

CONSULTAS <- list(

# ---------------------------------------------------------------- Pregunta 1
q1_totales = "
SELECT (SELECT COUNT(*) FROM comercios)                                    AS comercios,
       (SELECT COUNT(DISTINCT comercio_id || '-' || product_id_comercio)
          FROM anuncios WHERE incluido = 1)                                AS productos_categoria,
       (SELECT COUNT(*) FROM anuncios WHERE incluido = 1)                  AS presentaciones_sku,
       (SELECT COUNT(*) FROM productos)                                    AS productos_comparables,
       (SELECT COUNT(*) FROM observaciones)                                AS observaciones_total,
       (SELECT COUNT(*) FROM observaciones o JOIN anuncios a USING (anuncio_id)
          WHERE a.incluido = 1)                                            AS observaciones_categoria
",

q1_por_comercio = "
SELECT c.nombre AS comercio,
       COUNT(DISTINCT a.product_id_comercio) AS productos,
       COUNT(*)                              AS presentaciones_sku
FROM anuncios a JOIN comercios c USING (comercio_id)
WHERE a.incluido = 1
GROUP BY c.nombre
ORDER BY productos DESC
",

# ---------------------------------------------------------------- Pregunta 2
q2_cobertura = "
SELECT ejecucion_id,
       COUNT(*)                     AS obs_comparables,
       COUNT(DISTINCT producto_id)  AS productos,
       COUNT(DISTINCT comercio)     AS comercios,
       CASE WHEN COUNT(*) >= 24 THEN 'Sí' ELSE 'No' END AS cumple_minimo_24
FROM v_comparables
GROUP BY ejecucion_id
",

q2_productos_incompletos = "
-- productos que NO están en los 3 comercios dentro de una misma ejecución (debe salir vacío)
SELECT ejecucion_id, producto_id, COUNT(DISTINCT comercio) AS comercios
FROM v_comparables
GROUP BY ejecucion_id, producto_id
HAVING COUNT(DISTINCT comercio) < (SELECT COUNT(*) FROM comercios)
",

q2_repetidos = "
-- mismo producto y comercio repetido en la misma ejecución (debe salir vacío)
SELECT ejecucion_id, comercio, producto_id, COUNT(*) AS repeticiones
FROM v_comparables
GROUP BY ejecucion_id, comercio, producto_id
HAVING COUNT(*) > 1
",

# ---------------------------------------------------------------- Pregunta 3
q3 = "
WITH t AS (
  SELECT ejecucion_id, producto_id, producto_estandar,
         MIN(precio_cop)           AS precio_min,
         MAX(precio_cop)           AS precio_max,
         ROUND(AVG(precio_cop), 0) AS precio_promedio
  FROM v_comparables
  GROUP BY ejecucion_id, producto_id, producto_estandar
)
SELECT t.*,
       (SELECT GROUP_CONCAT(v.comercio, ', ')
          FROM v_comparables v
         WHERE v.ejecucion_id = t.ejecucion_id
           AND v.producto_id  = t.producto_id
           AND v.precio_cop   = t.precio_min) AS comercio_mas_barato
FROM t
ORDER BY t.ejecucion_id, t.producto_id
",

# ---------------------------------------------------------------- Pregunta 4
q4 = "
WITH t AS (
  SELECT ejecucion_id, producto_id, producto_estandar,
         MIN(precio_cop) AS precio_min, MAX(precio_cop) AS precio_max
  FROM v_comparables
  GROUP BY ejecucion_id, producto_id, producto_estandar
)
SELECT ejecucion_id, producto_id, producto_estandar, precio_min, precio_max,
       precio_max - precio_min                                  AS ahorro_cop,
       ROUND(100.0 * (precio_max - precio_min) / precio_max, 1) AS ahorro_pct
FROM t
ORDER BY ejecucion_id, producto_id
",

# ---------------------------------------------------------------- Pregunta 5
# P01 (55 ml) y P02 (75 ml) son el mismo producto (Lander tradicional) en dos tamaños.
q5 = "
SELECT ejecucion_id, comercio, producto_id, producto_estandar,
       contenido AS contenido_ml, precio_cop,
       ROUND(1.0 * precio_cop / contenido, 1) AS cop_por_ml
FROM v_comparables
WHERE producto_id IN ('P01', 'P02') AND unidad = 'ml'
ORDER BY ejecucion_id, cop_por_ml
",

# ---------------------------------------------------------------- Pregunta 6
q6 = "
WITH t AS (
  SELECT ejecucion_id, producto_id, producto_estandar,
         MIN(precio_cop) AS precio_min, MAX(precio_cop) AS precio_max
  FROM v_comparables
  GROUP BY ejecucion_id, producto_id, producto_estandar
)
SELECT t.ejecucion_id, t.producto_id, t.producto_estandar,
       t.precio_min, t.precio_max,
       t.precio_max - t.precio_min                                    AS ahorro_cop,
       ROUND(100.0 * (t.precio_max - t.precio_min) / t.precio_max, 1) AS ahorro_pct,
       (SELECT GROUP_CONCAT(v.comercio, ', ') FROM v_comparables v
         WHERE v.ejecucion_id = t.ejecucion_id AND v.producto_id = t.producto_id
           AND v.precio_cop = t.precio_min) AS comercio_mas_barato,
       (SELECT GROUP_CONCAT(v.comercio, ', ') FROM v_comparables v
         WHERE v.ejecucion_id = t.ejecucion_id AND v.producto_id = t.producto_id
           AND v.precio_cop = t.precio_max) AS comercio_mas_caro
FROM t
ORDER BY t.ejecucion_id, ahorro_pct DESC
",

# ---------------------------------------------------------------- Pregunta 7
q7 = "
SELECT o.ejecucion_id, c.nombre AS comercio,
       COUNT(*)                                                         AS filas,
       SUM(o.precio_cop IS NULL)                                        AS sin_precio,
       SUM(o.precio_lista_cop IS NULL)                                  AS sin_precio_lista,
       SUM(o.precio_cop > o.precio_lista_cop)                           AS precio_mayor_que_lista,
       SUM(a.ean IS NULL OR a.ean = '')                                 AS sin_ean,
       SUM(a.marca_original IS NULL OR a.marca_original = '')           AS sin_marca,
       SUM(a.descripcion_original IS NULL OR a.descripcion_original = '') AS sin_descripcion,
       SUM(a.contenido_valor IS NULL)                                   AS sin_contenido
FROM observaciones o
JOIN anuncios  a ON a.anuncio_id  = o.anuncio_id
JOIN comercios c ON c.comercio_id = a.comercio_id
WHERE a.incluido = 1
GROUP BY o.ejecucion_id, c.nombre
ORDER BY o.ejecucion_id, c.nombre
",

# ---------------------------------------------------------------- Pregunta 8
q8 = "
SELECT o.ejecucion_id, c.nombre AS comercio,
       COUNT(DISTINCT a.product_id_comercio) AS productos,
       COUNT(*)                              AS presentaciones_sku,
       SUM(o.precio_cop IS NOT NULL)         AS con_precio,
       ROUND(100.0 * AVG(o.precio_cop IS NOT NULL), 1)                                            AS pct_con_precio,
       ROUND(100.0 * AVG(a.descripcion_original IS NOT NULL AND a.descripcion_original <> ''), 1) AS pct_con_descripcion,
       ROUND(100.0 * AVG(a.marca_original IS NOT NULL AND a.marca_original <> ''), 1)             AS pct_con_marca,
       ROUND(100.0 * AVG(a.contenido_valor IS NOT NULL), 1)                                       AS pct_con_contenido
FROM observaciones o
JOIN anuncios  a ON a.anuncio_id  = o.anuncio_id
JOIN comercios c ON c.comercio_id = a.comercio_id
WHERE a.incluido = 1
GROUP BY o.ejecucion_id, c.nombre
ORDER BY o.ejecucion_id, productos DESC
",

q8_subcategorias = "
SELECT c.nombre AS comercio, a.subcategoria, COUNT(*) AS presentaciones_sku
FROM anuncios a JOIN comercios c USING (comercio_id)
WHERE a.incluido = 1
GROUP BY c.nombre, a.subcategoria
ORDER BY c.nombre, presentaciones_sku DESC
",

# ---------------------------------------------------------------- Extra: cambios entre ejecuciones
q9_precios_por_ejecucion = "
SELECT producto_id, producto_estandar, comercio, ejecucion_id, fecha_hora_consulta, precio_cop
FROM v_comparables
ORDER BY producto_id, comercio, fecha_hora_consulta
"
)

ejecutar_consultas <- function(con, consultas = CONSULTAS) {
  for (nm in names(consultas)) {
    cat("\n=====", nm, "=====\n")
    print(DBI::dbGetQuery(con, consultas[[nm]]), row.names = FALSE)
  }
  invisible(NULL)
}

