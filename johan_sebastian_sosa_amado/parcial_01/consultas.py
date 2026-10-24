"""
consultas.py · Las 8 preguntas del enunciado (sección 6) respondidas con SQL.

Igual que en las clases 4 y 5 (Taller práctico de SQL, versión Python):
    con = sqlite3.connect(...)
    resultado = pd.read_sql_query(\"\"\" SELECT ... \"\"\", con)
Se usan SELECT, WHERE, GROUP BY, HAVING, ORDER BY, JOIN y LEFT JOIN (clase 4
y 5), UNION ALL (clase 5), subconsultas, CTE con WITH (clase 5, sección 4) y
funciones de ventana RANK() y LAG() con OVER (PARTITION BY ...) (clase 5,
sección 5). RANK() resuelve "incluir los empates" de la pregunta 3: dos
tiendas con el mismo precio mínimo quedan ambas con puesto 1.

Las comparaciones de precio (preguntas 3 a 6) usan la recolección
completa más reciente (las 3 tiendas en estado completa) y solo los
productos con precio en las 3 tiendas en esa recolección (CTE "ultima").
El precio es el que mostraba la tienda en ese momento, con la promoción
incluida si la había.

Cómo correrlo (en la carpeta del taller, con el entorno virtual activo):
    python consultas.py
El documento taller_1.qmd muestra estas mismas consultas.
"""

import sqlite3

import pandas as pd

RUTA_BASE = "comparador_precios.sqlite"

# Observaciones de los productos comparables, con su comercio y recolección.
# Se repite al inicio de varias consultas como CTE.
CTE_COMPARABLES = """
    comparables AS (
        SELECT p.producto_id, p.descripcion, p.contenido, p.unidad,
               c.nombre AS comercio, e.recoleccion_id,
               o.precio, o.precio_antes, o.promocion, o.fecha_hora_consulta
        FROM   observaciones AS o
        JOIN   anuncios      AS a ON a.anuncio_id   = o.anuncio_id
        JOIN   productos     AS p ON p.producto_id  = a.producto_id
        JOIN   comercios     AS c ON c.comercio_id  = a.comercio_id
        JOIN   ejecuciones   AS e ON e.ejecucion_id = o.ejecucion_id
    ),
    completas AS (     -- recolecciones con las 3 tiendas en estado completa
        SELECT   recoleccion_id
        FROM     ejecuciones
        WHERE    estado = 'completa'
        GROUP BY recoleccion_id
        HAVING   COUNT(DISTINCT comercio_id) = 3
    ),
    con_precio AS (    -- precios de la recolección completa más reciente
        SELECT *
        FROM   comparables
        WHERE  recoleccion_id = (SELECT MAX(recoleccion_id) FROM completas)
          AND  precio IS NOT NULL
    ),
    ultima AS (        -- solo los productos con precio en las 3 tiendas
        SELECT *
        FROM   con_precio
        WHERE  producto_id IN (SELECT   producto_id
                               FROM     con_precio
                               GROUP BY producto_id
                               HAVING   COUNT(DISTINCT comercio) = 3)
    )"""

CONSULTAS = {}

# 1. ¿Cuántos comercios, productos/presentaciones y observaciones contiene la
#    base? ¿Cuántos productos aporta cada comercio?
CONSULTAS["1a"] = """
SELECT (SELECT COUNT(*) FROM comercios)                          AS comercios,
       (SELECT COUNT(*) FROM anuncios)                           AS presentaciones_extraidas,
       (SELECT COUNT(*) FROM anuncios WHERE en_categoria = 1)    AS presentaciones_categoria,
       (SELECT COUNT(*) FROM productos)                          AS productos_comparables,
       (SELECT COUNT(*) FROM observaciones)                      AS observaciones,
       (SELECT COUNT(*) FROM ejecuciones)                        AS ejecuciones;
"""
CONSULTAS["1b"] = """
SELECT   c.nombre                                   AS comercio,
         COUNT(*)                                   AS presentaciones_extraidas,
         SUM(a.en_categoria)                        AS de_la_categoria,
         SUM(a.comparable)                          AS comparables,
         (SELECT COUNT(*)
          FROM   observaciones AS o
          JOIN   anuncios      AS a2 ON a2.anuncio_id = o.anuncio_id
          WHERE  a2.comercio_id = c.comercio_id)    AS observaciones
FROM     anuncios  AS a
JOIN     comercios AS c ON c.comercio_id = a.comercio_id
GROUP BY c.comercio_id
ORDER BY de_la_categoria DESC;
"""

# 2. En una misma ejecución, ¿al menos 24 observaciones comparables (8 x 3)?
#    ¿Hay registros repetidos del mismo producto y comercio en esa ejecución?
CONSULTAS["2a"] = f"""
WITH {CTE_COMPARABLES}
SELECT   recoleccion_id,
         COUNT(precio)                         AS observaciones_con_precio,
         COUNT(DISTINCT producto_id)           AS productos,
         COUNT(DISTINCT comercio)              AS comercios,
         CASE WHEN COUNT(precio) >= 24 THEN 'sí' ELSE 'no' END AS cumple_24
FROM     comparables
GROUP BY recoleccion_id
ORDER BY recoleccion_id;
"""
CONSULTAS["2b"] = f"""
WITH {CTE_COMPARABLES}
SELECT   recoleccion_id, producto_id, comercio, COUNT(*) AS veces
FROM     comparables
GROUP BY recoleccion_id, producto_id, comercio
HAVING   COUNT(*) > 1;
"""

# 3. Para cada presentación comparable: precio mínimo, máximo y promedio, y
#    qué comercio ofrece el menor precio (con empates).
CONSULTAS["3"] = f"""
WITH {CTE_COMPARABLES},
puestos AS (
    SELECT producto_id, descripcion, comercio, precio, promocion,
           RANK() OVER (PARTITION BY producto_id ORDER BY precio) AS puesto
    FROM   ultima
)
SELECT   producto_id, descripcion,
         MIN(precio)                AS precio_min,
         MAX(precio)                AS precio_max,
         ROUND(AVG(precio), 0)      AS precio_promedio,
         GROUP_CONCAT(CASE WHEN puesto = 1 THEN comercio END, ' y ') AS mas_barato,
         SUM(puesto = 1)            AS tiendas_empatadas,
         MAX(CASE WHEN puesto = 1 THEN promocion END) AS mas_barato_en_promocion
FROM     puestos
GROUP BY producto_id
ORDER BY producto_id;
"""

# 4. Ahorro en COP y en % al elegir la oferta más económica de cada
#    presentación (frente a la más cara y frente al promedio).
CONSULTAS["4"] = f"""
WITH {CTE_COMPARABLES},
resumen AS (
    SELECT producto_id, descripcion,
           MIN(precio) AS precio_min, MAX(precio) AS precio_max, AVG(precio) AS precio_promedio
    FROM   ultima
    GROUP BY producto_id
)
SELECT   producto_id, descripcion, precio_min, precio_max,
         precio_max - precio_min                                  AS ahorro_vs_mas_cara_cop,
         ROUND(100.0 * (precio_max - precio_min) / precio_max, 1) AS ahorro_vs_mas_cara_pct,
         ROUND(precio_promedio - precio_min, 0)                   AS ahorro_vs_promedio_cop,
         ROUND(100.0 * (precio_promedio - precio_min) / precio_promedio, 1) AS ahorro_vs_promedio_pct
FROM     resumen
ORDER BY producto_id;
"""

# 5. Tamaños comparables del mismo producto: ¿qué presentación y comercio
#    tienen el menor precio por unidad? Se agrupa por la presentación sin el
#    número de unidades ("... slip comfort L (21 und)" y "(9 und)").
CONSULTAS["5"] = f"""
WITH {CTE_COMPARABLES},
por_unidad AS (
    SELECT CASE WHEN instr(descripcion, ' (') > 0
                THEN substr(descripcion, 1, instr(descripcion, ' (') - 1)
                ELSE descripcion END             AS producto_base,
           producto_id, descripcion, contenido, unidad, comercio, precio,
           ROUND(precio / contenido, 0)          AS precio_por_unidad
    FROM   ultima
),
con_varios_tamanos AS (
    SELECT producto_base
    FROM   por_unidad
    GROUP BY producto_base
    HAVING COUNT(DISTINCT contenido) > 1
)
SELECT   producto_base, producto_id, contenido, unidad, comercio, precio, precio_por_unidad,
         RANK() OVER (PARTITION BY producto_base ORDER BY precio_por_unidad) AS puesto
FROM     por_unidad
WHERE    producto_base IN (SELECT producto_base FROM con_varios_tamanos)
ORDER BY producto_base, puesto;
"""
# Precio por unidad de todas las presentaciones (más barato por producto).
CONSULTAS["5b"] = f"""
WITH {CTE_COMPARABLES}
SELECT   producto_id, descripcion, contenido, unidad, comercio,
         ROUND(precio / contenido, 0) AS precio_por_unidad,
         RANK() OVER (PARTITION BY producto_id ORDER BY precio / contenido) AS puesto
FROM     ultima
ORDER BY producto_id, puesto;
"""

# 6. Mayores diferencias de precio entre comercios, ordenadas por ahorro %.
CONSULTAS["6"] = f"""
WITH {CTE_COMPARABLES},
extremos AS (
    SELECT producto_id, descripcion, comercio, precio,
           RANK() OVER (PARTITION BY producto_id ORDER BY precio)      AS puesto_barato,
           RANK() OVER (PARTITION BY producto_id ORDER BY precio DESC) AS puesto_caro
    FROM   ultima
)
SELECT   producto_id, descripcion,
         GROUP_CONCAT(CASE WHEN puesto_barato = 1 THEN comercio END, ' y ') AS mas_barato,
         MIN(precio)                                                      AS precio_min,
         GROUP_CONCAT(CASE WHEN puesto_caro = 1 THEN comercio END, ' y ')   AS mas_caro,
         MAX(precio)                                                      AS precio_max,
         MAX(precio) - MIN(precio)                                        AS diferencia_cop,
         ROUND(100.0 * (MAX(precio) - MIN(precio)) / MAX(precio), 1)       AS ahorro_pct
FROM     extremos
GROUP BY producto_id
ORDER BY ahorro_pct DESC;
"""
# Extra (clase 5, LAG): cambio de precio de cada producto y comercio entre recolecciones.
CONSULTAS["6b"] = f"""
WITH {CTE_COMPARABLES},
serie AS (
    SELECT producto_id, comercio, recoleccion_id, precio,
           LAG(precio, 1) OVER (PARTITION BY producto_id, comercio
                                ORDER BY recoleccion_id) AS precio_anterior
    FROM   comparables
)
SELECT   producto_id, comercio, precio_anterior, precio,
         precio - precio_anterior                                  AS cambio_cop,
         ROUND(100.0 * (precio - precio_anterior) / precio_anterior, 1) AS cambio_pct
FROM     serie
WHERE    precio_anterior IS NOT NULL
  AND    precio <> precio_anterior
ORDER BY cambio_pct;
"""

# 7. ¿Qué datos faltantes o inconsistentes afectan la comparación y cuántos son?
CONSULTAS["7"] = """
SELECT 'Observaciones sin precio (agotado), de la categoría' AS problema,
       COUNT(*) AS cantidad
FROM   observaciones AS o JOIN anuncios AS a ON a.anuncio_id = o.anuncio_id
WHERE  a.en_categoria = 1 AND o.precio IS NULL
UNION ALL
SELECT 'Observaciones de productos comparables sin precio', COUNT(*)
FROM   observaciones AS o JOIN anuncios AS a ON a.anuncio_id = o.anuncio_id
WHERE  a.comparable = 1 AND o.precio IS NULL
UNION ALL
SELECT 'Anuncios de la categoría sin EAN (no se pueden emparejar por llave)', COUNT(*)
FROM   anuncios WHERE en_categoria = 1 AND (ean IS NULL OR ean = '')
UNION ALL
SELECT 'Anuncios de la categoría sin contenido identificable', COUNT(*)
FROM   anuncios WHERE en_categoria = 1 AND contenido IS NULL
UNION ALL
SELECT 'Pañales y ropa interior sin talla identificable', COUNT(*)
FROM   anuncios
WHERE  en_categoria = 1 AND talla IS NULL
  AND  subcategoria_estandar IN ('Pañal', 'Ropa interior absorbente')
UNION ALL
SELECT 'Mismo EAN publicado con nombres distintos entre tiendas', COUNT(*)
FROM   (SELECT ean FROM anuncios WHERE ean <> '' AND en_categoria = 1
        GROUP BY ean HAVING COUNT(DISTINCT nombre_original) > 1)
UNION ALL
SELECT 'Mismo EAN repetido dentro de una tienda (anuncios de la categoría)', COUNT(*)
FROM   (SELECT comercio_id, ean FROM anuncios WHERE ean <> '' AND en_categoria = 1
        GROUP BY comercio_id, ean HAVING COUNT(*) > 1)
UNION ALL
SELECT 'Marcas escritas de varias formas (marcas estándar afectadas)', COUNT(*)
FROM   (SELECT marca_estandar FROM anuncios WHERE en_categoria = 1
        GROUP BY marca_estandar HAVING COUNT(DISTINCT marca) > 1)
UNION ALL
SELECT 'Precio antes del descuento menor que el precio', COUNT(*)
FROM   observaciones WHERE precio_antes < precio
UNION ALL
SELECT 'Anuncios fuera de la categoría (excluidos por clasificar)', COUNT(*)
FROM   anuncios WHERE en_categoria = 0;
"""
# Detalle de los faltantes de precio por comercio y recolección.
CONSULTAS["7b"] = """
SELECT   c.nombre AS comercio, e.recoleccion_id,
         COUNT(*)                                 AS observaciones_categoria,
         SUM(o.precio IS NULL)                    AS sin_precio,
         ROUND(100.0 * SUM(o.precio IS NULL) / COUNT(*), 1) AS pct_sin_precio
FROM     observaciones AS o
JOIN     anuncios      AS a ON a.anuncio_id   = o.anuncio_id
JOIN     comercios     AS c ON c.comercio_id  = a.comercio_id
JOIN     ejecuciones   AS e ON e.ejecucion_id = o.ejecucion_id
WHERE    a.en_categoria = 1
GROUP BY c.nombre, e.recoleccion_id
ORDER BY c.nombre, e.recoleccion_id;
"""

# EAN repetido dentro de una misma tienda (dos SKU para el mismo empaque).
CONSULTAS["7c"] = """
SELECT   c.nombre AS comercio, a.ean, MIN(a.nombre_original) AS nombre_original,
         COUNT(*) AS anuncios, MAX(a.comparable) AS comparable
FROM     anuncios  AS a
JOIN     comercios AS c ON c.comercio_id = a.comercio_id
WHERE    a.en_categoria = 1 AND a.ean <> ''
GROUP BY c.nombre, a.ean
HAVING   COUNT(*) > 1;
"""

# 8. Productos de la categoría por comercio y ejecución; tamaño de la oferta y
#    proporción con descripción, marca y contenido identificables.
CONSULTAS["8"] = """
SELECT   c.nombre AS comercio, e.recoleccion_id,
         COUNT(*)                                                    AS productos_categoria,
         SUM(o.disponible)                                           AS disponibles,
         ROUND(100.0 * SUM(a.descripcion_original <> ''
                           OR a.presentacion_original <> '') / COUNT(*), 1) AS pct_con_descripcion,
         ROUND(100.0 * SUM(a.marca_estandar IS NOT NULL) / COUNT(*), 1) AS pct_con_marca,
         ROUND(100.0 * SUM(a.contenido IS NOT NULL) / COUNT(*), 1)      AS pct_con_contenido
FROM     observaciones AS o
JOIN     anuncios      AS a ON a.anuncio_id   = o.anuncio_id
JOIN     comercios     AS c ON c.comercio_id  = a.comercio_id
JOIN     ejecuciones   AS e ON e.ejecucion_id = o.ejecucion_id
WHERE    a.en_categoria = 1
GROUP BY c.nombre, e.recoleccion_id
ORDER BY e.recoleccion_id, productos_categoria DESC;
"""


def main():
    pd.set_option("display.width", 250)
    pd.set_option("display.max_colwidth", 60)
    con = sqlite3.connect(RUTA_BASE)
    for clave, consulta in CONSULTAS.items():
        resultado = pd.read_sql_query(consulta, con)
        print(f"\n== Pregunta {clave}")
        print(resultado.to_string(index=False) if len(resultado) else "  (sin filas)")
    con.close()


if __name__ == "__main__":
    main()
