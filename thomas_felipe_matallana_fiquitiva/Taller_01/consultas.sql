-- name: resumen

SELECT COUNT(DISTINCT comercio) AS comercios,
       COUNT(DISTINCT comercio || ':' || sku) AS anuncios_distintos,
       COUNT(DISTINCT producto_comun) AS presentaciones_comparables,
       COUNT(*) AS observaciones
FROM categoria;

-- name: por_comercio
SELECT comercio, COUNT(DISTINCT sku) AS anuncios,
       COUNT(*) AS observaciones
FROM categoria GROUP BY comercio;

-- name: cobertura_comparables
SELECT p.nombre AS producto, p.presentacion,
       COUNT(c.id) AS observaciones,
       COUNT(DISTINCT c.comercio) AS comercios,
       CASE WHEN COUNT(c.id)=3 AND COUNT(DISTINCT c.comercio)=3
            THEN 'Completa' ELSE 'Incompleta' END AS cobertura
FROM productos p LEFT JOIN comparables c
 ON p.id=c.producto_comun AND c.ejecucion=:run
GROUP BY p.id;

-- name: duplicados
SELECT producto_comun, comercio, COUNT(*) AS repeticiones
FROM categoria WHERE ejecucion=:run AND comparable=1
GROUP BY producto_comun,comercio HAVING COUNT(*)>1;

-- name: precios
WITH completos AS (
 SELECT producto_comun FROM comparables WHERE ejecucion=:run
 GROUP BY producto_comun HAVING COUNT(*)=3 AND COUNT(DISTINCT comercio)=3
), resumen AS (
 SELECT producto_comun, producto, presentacion, MIN(precio) AS minimo,
 MAX(precio) AS maximo, ROUND(AVG(precio),2) AS promedio
 FROM comparables WHERE ejecucion=:run AND producto_comun IN (SELECT * FROM completos)
 GROUP BY producto_comun
)
SELECT r.producto_comun,r.producto,r.presentacion,r.minimo,r.maximo,r.promedio,
 GROUP_CONCAT(c.comercio, ', ') AS comercios_mas_economicos
FROM resumen r JOIN comparables c ON c.producto_comun=r.producto_comun
 AND c.precio=r.minimo AND c.ejecucion=:run
GROUP BY r.producto_comun ORDER BY r.producto;

-- name: ahorro
SELECT producto_comun,producto,presentacion,MIN(precio) AS minimo,
 MAX(precio) AS maximo,MAX(precio)-MIN(precio) AS ahorro_cop,
 ROUND(100.0*(MAX(precio)-MIN(precio))/NULLIF(MAX(precio),0),2) AS ahorro_porcentaje
FROM comparables WHERE ejecucion=:run
GROUP BY producto_comun HAVING COUNT(*)=3 AND COUNT(DISTINCT comercio)=3
ORDER BY producto;

-- name: tamanos_equivalentes
WITH grupos AS (
 SELECT grupo_formulacion,unidad FROM productos
 GROUP BY grupo_formulacion,unidad HAVING COUNT(DISTINCT contenido_total)>1
), valores AS (
 SELECT c.producto,c.presentacion,c.comercio,c.unidad,
 c.precio/c.contenido_total AS precio_unidad,
 DENSE_RANK() OVER(PARTITION BY c.grupo_formulacion,c.unidad
 ORDER BY c.precio/c.contenido_total) AS puesto
 FROM comparables c JOIN grupos g USING(grupo_formulacion,unidad)
 WHERE c.ejecucion=:run
)
SELECT producto,presentacion,comercio,unidad,ROUND(precio_unidad,2) AS precio_unidad
FROM valores WHERE puesto=1;

-- name: ranking_ahorro
SELECT producto,presentacion,MAX(precio)-MIN(precio) AS ahorro_cop,
 ROUND(100.0*(MAX(precio)-MIN(precio))/NULLIF(MAX(precio),0),2) AS ahorro_porcentaje
FROM comparables WHERE ejecucion=:run
GROUP BY producto_comun HAVING COUNT(*)=3 AND COUNT(DISTINCT comercio)=3
ORDER BY ahorro_porcentaje DESC,producto;

-- name: calidad
SELECT comercio,COUNT(*) AS registros,
 SUM(precio IS NULL) AS sin_precio,
 SUM(disponible=0) AS no_disponibles,
 SUM(marca IS NULL) AS sin_marca,
 SUM(contenido_total IS NULL) AS sin_contenido_identificable,
 SUM(ean IS NULL) AS sin_ean,
 SUM(TRIM(COALESCE(descripcion_original,'') || COALESCE(descripcion_adicional,''))='') AS sin_descripcion,
 SUM(incidencia IS NOT NULL) AS incidencias_documentadas,
 SUM(fecha_hora IS NULL OR fecha_hora NOT LIKE '%-05:00') AS fecha_hora_invalida
FROM categoria WHERE ejecucion=:run GROUP BY comercio;

-- name: incidencias
SELECT comercio,nombre_original,incidencia FROM categoria
WHERE ejecucion=:run AND incidencia IS NOT NULL ORDER BY comercio,nombre_original;

-- name: oferta
SELECT ejecucion,comercio,COUNT(DISTINCT producto_sitio) AS productos_catalogo,
 COUNT(DISTINCT sku) AS presentaciones_sku,
 ROUND(100.0*AVG(TRIM(COALESCE(descripcion_original,'') || COALESCE(descripcion_adicional,''))<>''),1) AS pct_con_descripcion,
 ROUND(100.0*AVG(marca IS NOT NULL),1) AS pct_con_marca,
 ROUND(100.0*AVG(contenido_total IS NOT NULL),1) AS pct_con_contenido
FROM categoria GROUP BY ejecucion,comercio ORDER BY ejecucion,comercio;

-- name: cobertura_categoria
SELECT comercio,categoria,esperados,obtenidos,estado,error
FROM cobertura WHERE ejecucion=:run ORDER BY comercio,categoria;

-- name: exclusiones
SELECT comercio,motivo_exclusion,COUNT(*) AS registros
FROM observaciones WHERE ejecucion=:run AND incluido=0
GROUP BY comercio,motivo_exclusion ORDER BY comercio,registros DESC;

-- name: canasta
WITH completos AS (
 SELECT producto_comun FROM comparables WHERE ejecucion=:run
 GROUP BY producto_comun HAVING COUNT(*)=3 AND COUNT(DISTINCT comercio)=3
)
SELECT comercio,COUNT(*) AS presentaciones,SUM(precio) AS total_cop
FROM comparables WHERE ejecucion=:run AND producto_comun IN (SELECT * FROM completos)
GROUP BY comercio
HAVING COUNT(*)=(SELECT COUNT(*) FROM productos)
ORDER BY total_cop;
