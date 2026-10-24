-- Consultas SQL del Taller 1 (SQLite). ':id' = id_ejecucion analizada.

-- 1a. Conteo total de comercios, presentaciones y observaciones
-- (c1a_conteos_generales)
SELECT COUNT(DISTINCT comercio)                                         AS total_comercios,
       COUNT(DISTINCT comercio || '|' || sku_comercio)                  AS total_productos_presentaciones,
       COUNT(DISTINCT CASE WHEN comparable = 1 THEN id_producto_comun END) AS productos_comparables,
       COUNT(*)                                                         AS total_observaciones,
       COUNT(DISTINCT id_ejecucion)                                     AS total_ejecuciones
FROM observaciones;

-- 1b. Observaciones aportadas por cada comercio
-- (c1b_observaciones_por_comercio)
SELECT comercio,
       COUNT(*)                     AS observaciones,
       COUNT(DISTINCT sku_comercio) AS presentaciones,
       ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM observaciones), 2) AS pct_del_total
FROM observaciones
GROUP BY comercio
ORDER BY observaciones DESC;

-- 2a. Cobertura: >= 24 observaciones comparables en la misma ejecución
-- (c2a_cobertura_por_ejecucion)
SELECT id_ejecucion,
       COUNT(*)                          AS obs_comparables,
       COUNT(DISTINCT id_producto_comun) AS productos_distintos,
       COUNT(DISTINCT comercio)          AS comercios_distintos,
       CASE WHEN COUNT(*) >= 24 AND COUNT(DISTINCT id_producto_comun) >= 8
                 AND COUNT(DISTINCT comercio) >= 3
            THEN 'CUMPLE' ELSE 'NO CUMPLE' END AS verificacion_24
FROM observaciones
WHERE comparable = 1
GROUP BY id_ejecucion
ORDER BY id_ejecucion;

-- 2b. Cobertura de cada producto comparable en la ejecución analizada
-- (c2b_matriz_producto_comercio)
SELECT id_producto_comun,
       COUNT(*)                 AS n_observaciones,
       COUNT(DISTINCT comercio) AS n_comercios,
       GROUP_CONCAT(comercio, ', ') AS comercios
FROM observaciones
WHERE comparable = 1 AND id_ejecucion = :id
GROUP BY id_producto_comun
ORDER BY id_producto_comun;

-- 2c. Verificación de unicidad (los conteos de repetidos deben ser 0)
-- (c2c_registros_repetidos)
SELECT 'Repetidos por UNIQUE(comercio, sku_comercio, fecha_hora_consulta)' AS verificacion,
       COUNT(*) AS resultado
FROM (SELECT comercio, sku_comercio, fecha_hora_consulta
      FROM observaciones GROUP BY 1, 2, 3 HAVING COUNT(*) > 1)
UNION ALL
SELECT 'Comparables repetidos por (ejecución, comercio, producto)', COUNT(*)
FROM (SELECT id_ejecucion, comercio, id_producto_comun
      FROM observaciones WHERE comparable = 1 GROUP BY 1, 2, 3 HAVING COUNT(*) > 1)
UNION ALL
SELECT 'Restricciones UNIQUE declaradas en observaciones (debe ser >= 1)', COUNT(*)
FROM pragma_index_list('observaciones') WHERE "unique" = 1 AND origin = 'u';

-- 3. Precio mínimo, máximo, promedio y comercio(s) con el menor precio (empates incluidos)
-- (c3_min_max_promedio)
WITH comp AS (
  SELECT id_producto_comun, comercio, precio_cop
  FROM observaciones
  WHERE comparable = 1 AND id_ejecucion = :id
),
est AS (
  SELECT id_producto_comun,
         COUNT(*)                  AS n_comercios,
         MIN(precio_cop)           AS precio_min,
         MAX(precio_cop)           AS precio_max,
         ROUND(AVG(precio_cop), 2) AS precio_promedio
  FROM comp
  GROUP BY id_producto_comun
) 
SELECT e.id_producto_comun,
       p.descripcion,
       e.n_comercios,
       e.precio_min,
       e.precio_max,
       e.precio_promedio,
       (SELECT GROUP_CONCAT(c.comercio, ' = ') FROM comp c
         WHERE c.id_producto_comun = e.id_producto_comun AND c.precio_cop = e.precio_min) AS comercio_precio_menor,
       (SELECT COUNT(*) FROM comp c
         WHERE c.id_producto_comun = e.id_producto_comun AND c.precio_cop = e.precio_min) AS n_empatados_en_minimo
FROM est e
LEFT JOIN productos_comunes p ON p.id_producto_comun = e.id_producto_comun
ORDER BY e.id_producto_comun;

-- 4. Ahorro absoluto (Pmax - Pmin) y porcentual 100*(Pmax - Pmin)/Pmax
-- (c4_ahorro)
WITH comp AS (
  SELECT id_producto_comun, comercio, precio_cop
  FROM observaciones
  WHERE comparable = 1 AND id_ejecucion = :id
),
est AS (
  SELECT id_producto_comun,
         COUNT(*)                  AS n_comercios,
         MIN(precio_cop)           AS precio_min,
         MAX(precio_cop)           AS precio_max,
         ROUND(AVG(precio_cop), 2) AS precio_promedio
  FROM comp
  GROUP BY id_producto_comun
) 
SELECT id_producto_comun,
       precio_max,
       precio_min,
       (precio_max - precio_min)                                AS ahorro_absoluto_cop,
       ROUND(100.0 * (precio_max - precio_min) / precio_max, 2) AS ahorro_porcentual
FROM est
ORDER BY id_producto_comun;

-- 5a. Menor precio por unidad de contenido por familia (sin mezclar unidades)
-- (c5a_precio_unitario_mejor)
WITH base AS (
  SELECT familia_producto, comercio, sku_comercio, producto_nombre_original,
         contenido_num, unidad_medida, precio_cop,
         ROUND(precio_cop / contenido_num, 2) AS precio_por_unidad
  FROM observaciones
  WHERE id_ejecucion = :id AND familia_producto IS NOT NULL
    AND contenido_num > 0 AND unidad_medida IS NOT NULL
),
multi AS (   -- sólo familias con >1 presentación en la MISMA unidad
  SELECT familia_producto, unidad_medida
  FROM base GROUP BY familia_producto, unidad_medida
  HAVING COUNT(DISTINCT contenido_num) > 1
),
rk AS (
  SELECT b.*,
         RANK()   OVER (PARTITION BY b.familia_producto, b.unidad_medida ORDER BY b.precio_por_unidad) AS ranking,
         COUNT(*) OVER (PARTITION BY b.familia_producto, b.unidad_medida) AS n_presentaciones
  FROM base b
  JOIN multi m ON m.familia_producto = b.familia_producto AND m.unidad_medida = b.unidad_medida
)
SELECT familia_producto, 'COP/' || unidad_medida AS metrica, n_presentaciones,
       comercio, contenido_num, precio_cop, precio_por_unidad, producto_nombre_original
FROM rk
WHERE ranking = 1
ORDER BY familia_producto, metrica;

-- 5b. Ranking completo de precio por unidad de contenido dentro de cada familia y unidad
-- (c5b_precio_unitario_ranking)
WITH base AS (
  SELECT familia_producto, comercio, contenido_num, unidad_medida, precio_cop,
         ROUND(precio_cop / contenido_num, 2) AS precio_por_unidad
  FROM observaciones
  WHERE id_ejecucion = :id AND familia_producto IS NOT NULL
    AND contenido_num > 0 AND unidad_medida IS NOT NULL
),
multi AS (
  SELECT familia_producto, unidad_medida FROM base
  GROUP BY familia_producto, unidad_medida HAVING COUNT(DISTINCT contenido_num) > 1
)
SELECT b.familia_producto, 'COP/' || b.unidad_medida AS metrica,
       RANK() OVER (PARTITION BY b.familia_producto, b.unidad_medida ORDER BY b.precio_por_unidad) AS ranking,
       b.comercio, b.contenido_num, b.precio_cop, b.precio_por_unidad
FROM base b
JOIN multi m ON m.familia_producto = b.familia_producto AND m.unidad_medida = b.unidad_medida
ORDER BY b.familia_producto, metrica, ranking;

-- 6. Ranking de mayores diferencias de precio entre comercios (ahorro % descendente)
-- (c6_ranking_diferencias)
WITH comp AS (
  SELECT id_producto_comun, comercio, precio_cop
  FROM observaciones
  WHERE comparable = 1 AND id_ejecucion = :id
),
est AS (
  SELECT id_producto_comun,
         COUNT(*)                  AS n_comercios,
         MIN(precio_cop)           AS precio_min,
         MAX(precio_cop)           AS precio_max,
         ROUND(AVG(precio_cop), 2) AS precio_promedio
  FROM comp
  GROUP BY id_producto_comun
) 
SELECT RANK() OVER (ORDER BY 100.0 * (e.precio_max - e.precio_min) / e.precio_max DESC) AS ranking,
       e.id_producto_comun,
       (SELECT GROUP_CONCAT(c.comercio, ' = ') FROM comp c
         WHERE c.id_producto_comun = e.id_producto_comun AND c.precio_cop = e.precio_min) AS comercio_mas_barato,
       e.precio_min,
       (SELECT GROUP_CONCAT(c.comercio, ' = ') FROM comp c
         WHERE c.id_producto_comun = e.id_producto_comun AND c.precio_cop = e.precio_max) AS comercio_mas_caro,
       e.precio_max,
       (e.precio_max - e.precio_min)                                AS ahorro_absoluto_cop,
       ROUND(100.0 * (e.precio_max - e.precio_min) / e.precio_max, 2) AS ahorro_porcentual
FROM est e
ORDER BY ahorro_porcentual DESC, e.id_producto_comun;

-- 7. Auditoría de calidad: faltantes (NULL) e inconsistentes por campo
-- (c7_auditoria_calidad)
WITH t AS (SELECT COUNT(*) AS n FROM observaciones),
a AS (
  SELECT 'sku_comercio' AS campo,
         SUM(sku_comercio IS NULL OR TRIM(sku_comercio) = '') AS faltantes,
         0 AS inconsistentes, '-' AS regla_inconsistencia FROM observaciones
  UNION ALL SELECT 'producto_nombre_original',
         SUM(producto_nombre_original IS NULL OR TRIM(producto_nombre_original) = ''),
         SUM(producto_nombre_original <> TRIM(producto_nombre_original)),
         'espacios sobrantes (texto original intacto, informativo)' FROM observaciones
  UNION ALL SELECT 'marca_estandarizada', SUM(marca_estandarizada IS NULL),
         SUM(marca_estandarizada GLOB '*[0-9][0-9][0-9]*'), 'contiene un código numérico' FROM observaciones
  UNION ALL SELECT 'subcategoria_estandar', SUM(subcategoria_estandar IS NULL),
         SUM(subcategoria_estandar NOT IN ('Antihistamínicos', 'Antigripales / Descongestionantes', 'Antitusivos / Expectorantes', 'Enfermedades Respiratorias')), 'fuera del catálogo de 4 subcategorías' FROM observaciones
  UNION ALL SELECT 'contenido_num', SUM(contenido_num IS NULL),
         SUM(contenido_num <= 0), 'contenido <= 0' FROM observaciones
  UNION ALL SELECT 'unidad_medida', SUM(unidad_medida IS NULL),
         SUM((contenido_num IS NULL) <> (unidad_medida IS NULL)), 'contenido sin unidad o unidad sin contenido' FROM observaciones
  UNION ALL SELECT 'precio_cop', SUM(precio_cop IS NULL),
         SUM(precio_cop <= 0 OR precio_cop < 500 OR precio_cop > 2000000), 'fuera de [500; 2.000.000] COP' FROM observaciones
  UNION ALL SELECT 'precio_regular_cop', SUM(precio_regular_cop IS NULL),
         SUM(precio_regular_cop < precio_cop), 'precio regular menor que precio de venta' FROM observaciones
  UNION ALL SELECT 'es_promocion', SUM(es_promocion IS NULL),
         SUM((es_promocion = 1) <> (precio_cop < precio_regular_cop)), 'bandera no coincide con los precios' FROM observaciones
  UNION ALL SELECT 'url_producto', SUM(url_producto IS NULL OR TRIM(url_producto) = ''),
         SUM(url_producto NOT LIKE 'https://%'), 'no es URL https' FROM observaciones
  UNION ALL SELECT 'fecha_hora_consulta', SUM(fecha_hora_consulta IS NULL),
         SUM(fecha_hora_consulta NOT GLOB '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]-05:00'),
         'no es ISO 8601 con -05:00' FROM observaciones
  UNION ALL SELECT 'id_ejecucion', SUM(id_ejecucion IS NULL),
         SUM(id_ejecucion NOT IN (SELECT id_ejecucion FROM ejecuciones)), 'sin ejecución registrada' FROM observaciones
  UNION ALL SELECT 'presentacion_original', SUM(presentacion_original IS NULL), 0,
         'informativo: Farmatodo y Cruz Verde publican la presentación dentro del nombre' FROM observaciones
  UNION ALL SELECT 'descripcion_original', SUM(descripcion_original IS NULL), 0,
         'informativo: NULL = el servicio del comercio no publica descripción' FROM observaciones
  UNION ALL SELECT 'ficha_tecnica_original', SUM(ficha_tecnica_original IS NULL), 0,
         'informativo: NULL = el servicio del comercio no publica ficha técnica' FROM observaciones
  UNION ALL SELECT 'archivo_raw', SUM(archivo_raw IS NULL), 0,
         'trazabilidad: archivo de raw/ del que proviene la fila' FROM observaciones
)
SELECT a.campo, a.faltantes, ROUND(100.0 * a.faltantes / t.n, 2) AS pct_faltantes,
       a.inconsistentes, ROUND(100.0 * a.inconsistentes / t.n, 2) AS pct_inconsistentes,
       a.regla_inconsistencia
FROM a, t;

-- 8a. Tamaño de la oferta por comercio/ejecución y % con descripción, marca y contenido
-- (c8a_oferta_por_comercio)
SELECT o.id_ejecucion,
       o.comercio,
       e.estado,
       COUNT(*)                       AS observaciones,
       COUNT(DISTINCT o.sku_comercio) AS presentaciones,
       ROUND(100.0 * SUM(o.producto_nombre_original IS NOT NULL AND TRIM(o.producto_nombre_original) <> '') / COUNT(*), 2) AS pct_con_nombre_original,
       ROUND(100.0 * SUM(o.descripcion_original IS NOT NULL OR o.ficha_tecnica_original IS NOT NULL) / COUNT(*), 2) AS pct_con_descripcion_o_ficha,
       ROUND(100.0 * SUM(o.marca_estandarizada IS NOT NULL) / COUNT(*), 2)                                         AS pct_con_marca,
       ROUND(100.0 * SUM(o.contenido_num IS NOT NULL AND o.unidad_medida IS NOT NULL) / COUNT(*), 2)              AS pct_con_contenido,
       ROUND(100.0 * SUM((o.descripcion_original IS NOT NULL OR o.ficha_tecnica_original IS NOT NULL)
                         AND o.marca_estandarizada IS NOT NULL
                         AND o.contenido_num IS NOT NULL AND o.unidad_medida IS NOT NULL) / COUNT(*), 2)          AS pct_descripcion_marca_contenido,
       ROUND(100.0 * SUM(o.producto_nombre_original IS NOT NULL AND o.marca_estandarizada IS NOT NULL
                         AND o.contenido_num IS NOT NULL AND o.unidad_medida IS NOT NULL) / COUNT(*), 2)          AS pct_nombre_marca_contenido,
       SUM(o.es_promocion)            AS en_promocion
FROM observaciones o
LEFT JOIN ejecuciones e ON e.id_ejecucion = o.id_ejecucion AND e.comercio = o.comercio
GROUP BY o.id_ejecucion, o.comercio
ORDER BY o.id_ejecucion, o.comercio;

-- 8b. Oferta recolectada por comercio y subcategoría (ejecución analizada)
-- (c8b_oferta_por_subcategoria)
SELECT comercio, subcategoria_estandar, COUNT(*) AS observaciones,
       ROUND(AVG(precio_cop), 0) AS precio_promedio
FROM observaciones
WHERE id_ejecucion = :id
GROUP BY comercio, subcategoria_estandar
ORDER BY comercio, observaciones DESC;

