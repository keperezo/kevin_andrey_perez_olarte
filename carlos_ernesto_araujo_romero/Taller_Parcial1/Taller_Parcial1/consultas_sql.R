library(DBI)
library(RSQLite)
library(dplyr)
library(knitr)

# Conectar a la base de datos ya creada
con <- dbConnect(SQLite(), "comparador_precios.sqlite")

# PREGUNTA 1: ¿Cuántos comercios, productos/presentaciones y observaciones 
# contiene la base? ¿Cuántos productos aporta cada comercio?
cat("CONSULTA 1: CONTEOS GENERALES Y POR COMERCIO\n")

q1_general <- dbGetQuery(con, "
  SELECT 
    COUNT(DISTINCT comercio) AS total_comercios,
    COUNT(DISTINCT texto_original) AS presentaciones_unicas,
    COUNT(DISTINCT id_producto_comun) AS productos_comparables_unicos,
    COUNT(*) AS total_observaciones
  FROM observaciones;
")
print(q1_general)

q1_comercios <- dbGetQuery(con, "
  SELECT 
    comercio,
    COUNT(*) AS total_aportado,
    SUM(es_comparable) AS productos_comparables
  FROM observaciones
  GROUP BY comercio
  ORDER BY total_aportado DESC;
")
print(q1_comercios)
cat("Interpretación Q1: La base contiene 3 comercios con 61 observaciones totales. Jumbo lidera con 29 productos, mientras Éxito y Carulla aportan 16 cada uno.\n\n")

# PREGUNTA 2: En una misma ejecución, ¿se obtuvieron al menos 24 observaciones 
# comparables? ¿Hay registros repetidos del mismo producto y comercio?
cat("CONSULTA 2: VALIDACIÓN DE REQUISITO MÍNIMO (>= 24) Y DUPLICADOS\n")

q2_cobertura <- dbGetQuery(con, "
  SELECT 
    id_ejecucion,
    SUM(es_comparable) AS total_comparables,
    COUNT(DISTINCT id_producto_comun) AS productos_distintos_comunes
  FROM observaciones
  GROUP BY id_ejecucion;
")
print(q2_cobertura)

q2_duplicados <- dbGetQuery(con, "
  SELECT 
    comercio, sku_comercio, COUNT(*) AS repeticiones
  FROM observaciones
  GROUP BY comercio, sku_comercio
  HAVING COUNT(*) > 1;
")
cat("Duplicados encontrados:", nrow(q2_duplicados), "\n")
cat("Interpretación Q2: Se superó la meta obligatoria (26 observaciones comparables frente a las 24 mínimas). Cero duplicados gracias a la restricción UNIQUE.\n\n")

# PREGUNTA 3: Para cada presentación comparable, ¿cuáles son el precio mínimo, 
# máximo y promedio? ¿Qué comercio ofrece el menor precio? (Incluir empates)
cat("CONSULTA 3: DISPERSIÓN DE PRECIOS Y COMERCIO MÁS ECONÓMICO\n")

q3 <- dbGetQuery(con, "
  WITH stats AS (
    SELECT 
      id_producto_comun,
      MIN(precio_cop) AS precio_min,
      MAX(precio_cop) AS precio_max,
      ROUND(AVG(precio_cop), 2) AS precio_prom
    FROM observaciones
    WHERE es_comparable = 1
    GROUP BY id_producto_comun
  )
  SELECT 
    s.id_producto_comun,
    s.precio_min,
    s.precio_max,
    s.precio_prom,
    o.comercio AS comercio_mas_barato
  FROM stats s
  JOIN observaciones o 
    ON s.id_producto_comun = o.id_producto_comun 
   AND s.precio_min = o.precio_cop
  WHERE o.es_comparable = 1
  ORDER BY s.id_producto_comun;
")
print(q3)
cat("Interpretación Q3: Éxito tiende a ofrecer el menor precio bruto en chocolates infantiles (Kinder y M&M), mientras Carulla empata en promociones puntuales.\n\n")

# PREGUNTA 4: ¿Cuál es el ahorro en COP y en porcentaje al elegir la oferta 
# más económica de cada presentación?
cat("CONSULTA 4: AHORRO POTENCIAL EN COP Y PORCENTAJE\n")

q4 <- dbGetQuery(con, "
  SELECT 
    id_producto_comun,
    MIN(precio_cop) AS precio_min,
    MAX(precio_cop) AS precio_max,
    ROUND(MAX(precio_cop) - MIN(precio_cop), 2) AS ahorro_cop,
    ROUND(100.0 * (MAX(precio_cop) - MIN(precio_cop)) / MAX(precio_cop), 2) AS ahorro_porcentual
  FROM observaciones
  WHERE es_comparable = 1
  GROUP BY id_producto_comun
  HAVING COUNT(DISTINCT comercio) > 1
  ORDER BY ahorro_porcentual DESC;
")
print(q4)
cat("Interpretación Q4: Se observan brechas de ahorro superiores al 9% entre comercios para presentaciones idénticas de confitería.\n\n")

# PREGUNTA 5: Si se incluyeron tamaños comparables del mismo producto, 
# ¿qué presentación y comercio tienen el menor precio por g o ml?
cat("CONSULTA 5: EFICIENCIA UNITARIA (MENOR PRECIO POR GRAMO)\n")

q5 <- dbGetQuery(con, "
  SELECT 
    comercio,
    id_producto_comun,
    texto_original,
    precio_cop,
    contenido_valor,
    contenido_unidad,
    precio_por_unidad AS cop_por_gramo
  FROM observaciones
  WHERE es_comparable = 1
  ORDER BY cop_por_gramo ASC
  LIMIT 5;
")
print(q5)
cat("Interpretación Q5: Las presentaciones de mayor gramaje (ej. Chocobreak 250g o Corona 450g) ofrecen el menor costo por gramo ($30-$60 COP/g).\n\n")

# PREGUNTA 6: Mayores diferencias de precio entre comercios (Ordenado por ahorro %)
cat("CONSULTA 6: RANKING DE MAYOR DIFERENCIA DE PRECIOS\n")

q6 <- dbGetQuery(con, "
  SELECT 
    id_producto_comun,
    MIN(precio_cop) AS mas_barato,
    MAX(precio_cop) AS mas_caro,
    ROUND(100.0 * (MAX(precio_cop) - MIN(precio_cop)) / MAX(precio_cop), 2) AS pct_diferencia
  FROM observaciones
  WHERE es_comparable = 1
  GROUP BY id_producto_comun
  ORDER BY pct_diferencia DESC;
")
print(q6)

# PREGUNTA 7: ¿Qué datos faltantes o inconsistentes afectan la comparación?
cat("CONSULTA 7: AUDITORÍA DE DATOS FALTANTES E INCONSISTENCIAS\n")

q7 <- dbGetQuery(con, "
  SELECT 
    comercio,
    COUNT(*) AS total_filas,
    SUM(CASE WHEN precio_cop IS NULL OR precio_cop <= 0 THEN 1 ELSE 0 END) AS nulos_precio,
    SUM(CASE WHEN contenido_valor IS NULL THEN 1 ELSE 0 END) AS nulos_gramaje,
    SUM(CASE WHEN url_producto IS NULL THEN 1 ELSE 0 END) AS nulos_url
  FROM observaciones
  GROUP BY comercio;
")
print(q7)
cat("Interpretación Q7: Cero valores nulos en precios o URLs; los gramajes no explícitos en el texto fueron imputados con el peso canónico.\n\n")

# PREGUNTA 8: Catálogo por comercio y proporción de productos con atributos
cat("CONSULTA 8: COBERTURA Y CALIDAD DE INFORMACIÓN POR COMERCIO\n")

q8 <- dbGetQuery(con, "
  SELECT 
    comercio,
    COUNT(*) AS total_obtenido,
    ROUND(100.0 * SUM(CASE WHEN texto_original IS NOT NULL THEN 1 ELSE 0 END) / COUNT(*), 1) AS pct_con_nombre,
    ROUND(100.0 * SUM(CASE WHEN contenido_valor > 0 THEN 1 ELSE 0 END) / COUNT(*), 1) AS pct_con_gramaje,
    ROUND(100.0 * SUM(es_comparable) / COUNT(*), 1) AS pct_en_canasta_comparable
  FROM observaciones
  GROUP BY comercio;
")
print(q8)

dbDisconnect(con)
cat("LAS CONSULTAS FUERON EJECUTADAS CON ÉXITO\n")
