-- =============================================================================
-- consultas.sql · Consultas SQL del Taller 1 (SQLite)
-- Pegue aquí las consultas finales; aparecen en el visor de código del informe.
-- Vistas disponibles (ejecución más reciente):
--   v_categoria   : toda la categoría, con volumen_ml
--   v_comparables : solo comparable = 1 con id_producto_comun
-- Tablas base: observaciones (22 variables) y ejecuciones.
-- =============================================================================

-- 1. ¿Cuántos comercios, productos y observaciones contiene la base?

SELECT 
  comercio,
  COUNT(DISTINCT sku_codigo) AS productos_unicos,
  COUNT(*) AS total_observaciones,
  SUM(CASE WHEN comparable = 1 THEN 1 ELSE 0 END) AS obs_comparables
FROM observaciones
GROUP BY comercio

UNION ALL

SELECT 
  'TOTAL BASE' AS comercio,
  COUNT(DISTINCT comercio || '|' || sku_codigo) AS productos_unicos,
  COUNT(*) AS total_observaciones,
  SUM(CASE WHEN comparable = 1 THEN 1 ELSE 0 END) AS obs_comparables
FROM observaciones;

-- 2. ¿Hay al menos 24 observaciones comparables en una misma ejecución y registros repetidos?

SELECT 
  id_ejecucion,
  COUNT(DISTINCT comercio) AS comercios_activos,
  COUNT(DISTINCT id_producto_comun) AS presentaciones_comparables,
  COUNT(*) AS total_obs_comparables,
  CASE WHEN COUNT(*) >= 24 THEN 'CUMPLE (>= 24)' ELSE 'INSUFICIENTE' END AS evaluacion_cobertura
FROM observaciones
WHERE comparable = 1
GROUP BY id_ejecucion;

-- 3. Precio mínimo, máximo y promedio por presentación (con empates)

WITH estadisticas AS (
  SELECT 
    id_producto_comun,
    MIN(precio_cop) AS precio_min,
    MAX(precio_cop) AS precio_max,
    ROUND(AVG(precio_cop), 0) AS precio_promedio
  FROM v_comparables
  GROUP BY id_producto_comun
)
SELECT 
  e.id_producto_comun,
  e.precio_min,
  e.precio_max,
  e.precio_promedio,
  GROUP_CONCAT(c.comercio, ' | ') AS comercio_menor_precio
FROM estadisticas e
JOIN v_comparables c 
  ON e.id_producto_comun = c.id_producto_comun 
 AND e.precio_min = c.precio_cop
GROUP BY e.id_producto_comun, e.precio_min, e.precio_max, e.precio_promedio
ORDER BY e.id_producto_comun;

-- 4. Ahorro en COP y en porcentaje al elegir la oferta más económica

SELECT 
  id_producto_comun,
  MIN(precio_cop) AS precio_minimo,
  MAX(precio_cop) AS precio_maximo,
  (MAX(precio_cop) - MIN(precio_cop)) AS ahorro_cop,
  ROUND(((MAX(precio_cop) - MIN(precio_cop)) * 100.0 / MAX(precio_cop)), 2) AS ahorro_porcentaje
FROM v_comparables
GROUP BY id_producto_comun
ORDER BY ahorro_porcentaje DESC;

-- 5. Menor precio por unidad de contenido entre tamaños comparables

WITH ranking_unidad AS (
  SELECT 
    id_producto_comun,
    comercio,
    producto_raw,
    presentacion_raw,
    volumen_ml,
    precio_cop,
    ROUND(precio_por_litro, 2) AS precio_por_litro_cop,
    RANK() OVER (PARTITION BY id_producto_comun ORDER BY precio_por_litro ASC) AS rnk
  FROM v_comparables
  WHERE volumen_ml > 0 AND precio_cop > 0
)
SELECT 
  id_producto_comun,
  comercio,
  producto_raw,
  volumen_ml,
  precio_cop,
  precio_por_litro_cop
FROM ranking_unidad
WHERE rnk = 1
ORDER BY precio_por_litro_cop ASC;

-- 6. Mayores diferencias de precio entre comercios
SELECT 
  id_producto_comun,
  COUNT(DISTINCT comercio) AS num_comercios,
  MIN(precio_cop) AS precio_minimo,
  MAX(precio_cop) AS precio_maximo,
  (MAX(precio_cop) - MIN(precio_cop)) AS brecha_cop,
  ROUND(((MAX(precio_cop) - MIN(precio_cop)) * 100.0 / MIN(precio_cop)), 2) AS sobreprecio_relativo_pct
FROM v_comparables
GROUP BY id_producto_comun
HAVING num_comercios >= 3
ORDER BY sobreprecio_relativo_pct DESC;


-- 7. Datos faltantes o inconsistentes que afectan la comparación

SELECT 
  COUNT(*) AS total_observaciones_evaluadas,
  SUM(CASE WHEN precio_cop IS NULL OR precio_cop <= 0 THEN 1 ELSE 0 END) AS precios_invalidos_nulos,
  SUM(CASE WHEN contenido_num IS NULL OR contenido_num <= 0 THEN 1 ELSE 0 END) AS contenido_faltante,
  SUM(CASE WHEN unidad_medida IS NULL OR unidad_medida = '' THEN 1 ELSE 0 END) AS unidad_faltante,
  SUM(CASE WHEN marca_raw IS NULL OR marca_raw = '' OR marca_raw = 'OTRAS' THEN 1 ELSE 0 END) AS marca_no_identificada,
  SUM(CASE WHEN sku_codigo IS NULL OR sku_codigo = '' OR sku_codigo = 'N/A' THEN 1 ELSE 0 END) AS sku_faltante
FROM observaciones
WHERE id_ejecucion = (SELECT MAX(id_ejecucion) FROM observaciones);

-- 8. Tamaño de la oferta por comercio y ejecución, y calidad de la descripción
SELECT 
  e.id_ejecucion,
  e.inicio_ts AS fecha_inicio,
  o.comercio,
  COUNT(*) AS total_productos_extraidos,
  ROUND(SUM(CASE WHEN o.texto_original_anuncio IS NOT NULL AND LENGTH(o.texto_original_anuncio) > 10 THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 1) AS pct_con_descripcion,
  ROUND(SUM(CASE WHEN o.marca_raw IS NOT NULL AND o.marca_raw != '' AND o.marca_raw != 'OTRAS' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 1) AS pct_con_marca,
  ROUND(SUM(CASE WHEN o.contenido_num IS NOT NULL AND o.contenido_num > 0 THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 1) AS pct_con_contenido
FROM ejecuciones e
JOIN observaciones o ON e.id_ejecucion = o.id_ejecucion
GROUP BY e.id_ejecucion, o.comercio
ORDER BY e.id_ejecucion DESC, total_productos_extraidos DESC;

