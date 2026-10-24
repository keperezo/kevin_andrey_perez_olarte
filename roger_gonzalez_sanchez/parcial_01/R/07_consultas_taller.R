# 07_consultas_taller.R  -  FASE 5: las 8 consultas SQL obligatorias (Sección 6)
# Las consultas marcadas con :id se ejecutan sobre una ejecución concreta
# (por defecto la última), de modo que los precios comparados sean de la misma
# corrida. Las demás recorren toda la base.

# CTE reutilizada por las consultas 3, 4 y 6.
CTE_COMPARABLES <- "
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
)"

# Reglas de la consulta 7: qué se considera faltante o inconsistente en cada
# campo y cómo afecta la comparación de precios.
REGLAS_CALIDAD <- tibble::tribble(
  ~campo, ~faltante, ~inconsistente, ~regla, ~efecto,
  "sku_comercio", "sku_comercio IS NULL OR TRIM(sku_comercio) = ''", "0", "-",
    "Sin SKU no se identifica la oferta ni se evitan duplicados",
  "producto_nombre_original", "producto_nombre_original IS NULL OR TRIM(producto_nombre_original) = ''",
    "producto_nombre_original <> TRIM(producto_nombre_original)", "espacios sobrantes (se conserva el texto original)",
    "No afecta: las reglas trabajan sobre el texto normalizado",
  "marca_estandarizada", "marca_estandarizada IS NULL", "marca_estandarizada GLOB '*[0-9][0-9][0-9]*'",
    "contiene un código numérico", "Sin marca el producto no puede ser comparable",
  "subcategoria_estandar", "subcategoria_estandar IS NULL",
    paste0("subcategoria_estandar NOT IN (", paste0("'", SUBCATEGORIAS_VALIDAS, "'", collapse = ", "), ")"),
    "fuera de las 4 subcategorías", "Sin subcategoría el registro sale de la categoría",
  "contenido_num", "contenido_num IS NULL", "contenido_num <= 0", "contenido <= 0",
    "Sin contenido no hay correspondencia de presentación ni precio por unidad",
  "unidad_medida", "unidad_medida IS NULL", "(contenido_num IS NULL) <> (unidad_medida IS NULL)",
    "contenido sin unidad o unidad sin contenido", "Sin unidad no se puede calcular el precio por unidad",
  "precio_cop", "precio_cop IS NULL", "precio_cop < 500 OR precio_cop > 2000000", "fuera de [500; 2.000.000] COP",
    "Un precio ausente o atípico invalida la comparación",
  "precio_regular_cop", "precio_regular_cop IS NULL", "precio_regular_cop < precio_cop",
    "precio regular menor que el de venta", "Solo afecta el indicador de promoción",
  "es_promocion", "es_promocion IS NULL", "(es_promocion = 1) <> (precio_cop < precio_regular_cop)",
    "bandera no coincide con los precios", "Solo afecta el indicador de promoción",
  "url_producto", "url_producto IS NULL OR TRIM(url_producto) = ''", "url_producto NOT LIKE 'https://%'",
    "no es URL https", "Solo afecta la trazabilidad",
  "fecha_hora_consulta", "fecha_hora_consulta IS NULL",
    "fecha_hora_consulta NOT GLOB '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]-05:00'",
    "no es ISO 8601 con -05:00", "Sin fecha no se ubica el precio en el tiempo",
  "id_ejecucion", "id_ejecucion IS NULL", "id_ejecucion NOT IN (SELECT id_ejecucion FROM ejecuciones)",
    "sin ejecución registrada", "Sin ejecución no se garantiza que los precios sean de la misma corrida",
  "disponible", "disponible IS NULL", "disponible = 0", "producto agotado (disponible = 0)",
    "Condición de compra: el precio está publicado pero el producto puede no estar disponible",
  "presentacion_original", "presentacion_original IS NULL", "0", "-",
    "Informativo: Farmatodo y Cruz Verde publican la presentación dentro del nombre",
  "descripcion_original", "descripcion_original IS NULL", "0", "-",
    "Informativo: el servicio del comercio no publica descripción",
  "ficha_tecnica_original", "ficha_tecnica_original IS NULL", "0", "-",
    "Informativo: el servicio del comercio no publica ficha técnica",
  "archivo_raw", "archivo_raw IS NULL", "0", "-", "Solo afecta la trazabilidad hacia raw/"
)

sql_auditoria_calidad <- function(reglas = REGLAS_CALIDAD) {
  filas <- pmap_chr(reglas, function(campo, faltante, inconsistente, regla, efecto) {
    paste0("  SELECT '", campo, "' AS campo,\n",
           "         COALESCE(SUM(", faltante, "), 0) AS faltantes,\n",
           "         COALESCE(SUM(", inconsistente, "), 0) AS inconsistentes,\n",
           "         COALESCE(SUM(comparable = 1 AND ((", faltante, ") OR (", inconsistente, "))), 0) AS en_comparables,\n",
           "         '", regla, "' AS regla_inconsistencia, '", efecto, "' AS efecto_en_la_comparacion\n",
           "  FROM observaciones WHERE id_ejecucion = :id")
  })
  paste0("WITH t AS (SELECT COUNT(*) AS n FROM observaciones WHERE id_ejecucion = :id),\na AS (\n",
         paste(filas, collapse = "\n  UNION ALL\n"), "\n)\n",
         "SELECT a.campo, a.faltantes, ROUND(100.0 * a.faltantes / t.n, 2) AS pct_faltantes,\n",
         "       a.inconsistentes, ROUND(100.0 * a.inconsistentes / t.n, 2) AS pct_inconsistentes,\n",
         "       a.en_comparables, a.regla_inconsistencia, a.efecto_en_la_comparacion\n",
         "FROM a, t")
}

CONSULTAS_TALLER <- list(


  c1a_conteos_generales = list(
    titulo = "1a. Conteo total de comercios, presentaciones y observaciones",
    sql = "
SELECT COUNT(DISTINCT comercio)                                         AS total_comercios,
       COUNT(DISTINCT comercio || '|' || sku_comercio)                  AS total_productos_presentaciones,
       COUNT(DISTINCT CASE WHEN comparable = 1 THEN id_producto_comun END) AS productos_comparables,
       COUNT(*)                                                         AS total_observaciones,
       COUNT(DISTINCT id_ejecucion)                                     AS total_ejecuciones
FROM observaciones"),

  c1b_observaciones_por_comercio = list(
    titulo = "1b. Observaciones aportadas por cada comercio",
    sql = "
SELECT comercio,
       COUNT(*)                     AS observaciones,
       COUNT(DISTINCT sku_comercio) AS presentaciones,
       ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM observaciones), 2) AS pct_del_total
FROM observaciones
GROUP BY comercio
ORDER BY observaciones DESC"),

  c2a_cobertura_por_ejecucion = list(
    titulo = "2a. Cobertura: >= 24 observaciones comparables en la misma ejecución",
    sql = "
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
ORDER BY id_ejecucion"),

  c2b_matriz_producto_comercio = list(
    titulo = "2b. Cobertura de cada producto comparable en la ejecución analizada",
    sql = "
SELECT id_producto_comun,
       COUNT(*)                 AS n_observaciones,
       COUNT(DISTINCT comercio) AS n_comercios,
       GROUP_CONCAT(comercio, ', ') AS comercios
FROM observaciones
WHERE comparable = 1 AND id_ejecucion = :id
GROUP BY id_producto_comun
ORDER BY id_producto_comun"),

  c2c_registros_repetidos = list(
    titulo = "2c. Registros repetidos (todos los conteos de repetidos deben ser 0)",
    sql = "
SELECT 'Mismo producto (SKU) y comercio repetido en la ejecución analizada' AS verificacion,
       COUNT(*) AS resultado
FROM (SELECT comercio, sku_comercio FROM observaciones
      WHERE id_ejecucion = :id GROUP BY 1, 2 HAVING COUNT(*) > 1)
UNION ALL
SELECT 'Producto comparable repetido en un comercio en la ejecución analizada', COUNT(*)
FROM (SELECT comercio, id_producto_comun FROM observaciones
      WHERE comparable = 1 AND id_ejecucion = :id GROUP BY 1, 2 HAVING COUNT(*) > 1)
UNION ALL
SELECT 'Repetidos por UNIQUE(comercio, sku_comercio, fecha_hora_consulta) en toda la base', COUNT(*)
FROM (SELECT comercio, sku_comercio, fecha_hora_consulta
      FROM observaciones GROUP BY 1, 2, 3 HAVING COUNT(*) > 1)
UNION ALL
SELECT 'Restricciones UNIQUE declaradas en observaciones (debe ser >= 1)', COUNT(*)
FROM pragma_index_list('observaciones') WHERE \"unique\" = 1 AND origin = 'u'"),

  c3_min_max_promedio = list(
    titulo = "3. Precio mínimo, máximo, promedio y comercio(s) con el menor precio (empates incluidos)",
    sql = paste(CTE_COMPARABLES, "
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
ORDER BY e.id_producto_comun")),

  c4_ahorro = list(
    titulo = "4. Ahorro absoluto (Pmax - Pmin) y porcentual 100*(Pmax - Pmin)/Pmax",
    sql = paste(CTE_COMPARABLES, "
SELECT id_producto_comun,
       precio_max,
       precio_min,
       (precio_max - precio_min)                                AS ahorro_absoluto_cop,
       ROUND(100.0 * (precio_max - precio_min) / precio_max, 2) AS ahorro_porcentual
FROM est
ORDER BY id_producto_comun")),

  c4b_canasta = list(
    titulo = "4b. Canasta de los 8 productos: costo en cada comercio y costo comprando cada uno donde es más barato",
    sql = "
WITH comp AS (
  SELECT id_producto_comun, comercio, precio_cop
  FROM observaciones WHERE comparable = 1 AND id_ejecucion = :id
),
n_canasta AS (SELECT COUNT(DISTINCT id_producto_comun) AS n FROM comp),
por_comercio AS (
  SELECT comercio, COUNT(DISTINCT id_producto_comun) AS productos, SUM(precio_cop) AS costo
  FROM comp GROUP BY comercio
),
optima AS (
  SELECT SUM(pmin) AS costo FROM (SELECT MIN(precio_cop) AS pmin FROM comp GROUP BY id_producto_comun)
)
SELECT p.comercio,
       p.productos || ' de ' || n.n AS productos_en_canasta,
       CASE WHEN p.productos = n.n THEN 'completa' ELSE 'incompleta (no comparable)' END AS cobertura,
       p.costo AS costo_canasta_cop,
       o.costo AS costo_canasta_optima_cop,
       p.costo - o.costo AS sobrecosto_frente_a_optima_cop,
       ROUND(100.0 * (p.costo - o.costo) / p.costo, 2) AS ahorro_pct_al_comprar_cada_uno_donde_es_mas_barato,
       CASE WHEN p.productos = n.n THEN RANK() OVER (PARTITION BY p.productos = n.n ORDER BY p.costo) END AS ranking
FROM por_comercio p, n_canasta n, optima o
ORDER BY p.productos = n.n DESC, p.costo"),

  c5a_precio_unitario_mejor = list(
    titulo = "5a. Menor precio por unidad de contenido por familia (sin mezclar unidades)",
    sql = "
WITH base AS (
  SELECT familia_producto, comercio, sku_comercio, producto_nombre_original,
         contenido_num, unidad_medida, precio_cop, disponible,
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
       comercio, contenido_num, precio_cop AS desembolso_cop, precio_por_unidad, disponible,
       producto_nombre_original
FROM rk
WHERE ranking = 1
ORDER BY familia_producto, metrica"),

  c5b_precio_unitario_ranking = list(
    titulo = "5b. Ranking completo de precio por unidad de contenido dentro de cada familia y unidad",
    sql = "
WITH base AS (
  SELECT familia_producto, comercio, sku_comercio, contenido_num, unidad_medida, precio_cop, disponible,
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
       b.comercio, b.sku_comercio, b.contenido_num, b.precio_cop AS desembolso_cop, b.precio_por_unidad, b.disponible
FROM base b
JOIN multi m ON m.familia_producto = b.familia_producto AND m.unidad_medida = b.unidad_medida
ORDER BY b.familia_producto, metrica, ranking"),

  c6_ranking_diferencias = list(
    titulo = "6. Ranking de mayores diferencias de precio entre comercios (ahorro % descendente)",
    sql = paste(CTE_COMPARABLES, "
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
ORDER BY ahorro_porcentual DESC, e.id_producto_comun")),

  c7_auditoria_calidad = list(
    titulo = "7. Datos faltantes (NULL) e inconsistentes por campo en la ejecución analizada, y cuántos afectan a los comparables",
    sql = sql_auditoria_calidad()),

  c8a_oferta_por_comercio = list(
    titulo = "8a. Tamaño de la oferta por comercio/ejecución y % con descripción, marca y contenido",
    sql = "
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
ORDER BY o.id_ejecucion, o.comercio"),

  c8b_oferta_por_subcategoria = list(
    titulo = "8b. Oferta recolectada por comercio y subcategoría (ejecución analizada)",
    sql = "
SELECT comercio, subcategoria_estandar, COUNT(*) AS observaciones,
       ROUND(AVG(precio_cop), 0) AS precio_promedio
FROM observaciones
WHERE id_ejecucion = :id
GROUP BY comercio, subcategoria_estandar
ORDER BY comercio, observaciones DESC")
)

ejecutar_consultas_taller <- function(db_path = DB_PATH, id_ejecucion = NULL, imprimir = TRUE) {
  con <- conectar_bd(db_path); on.exit(dbDisconnect(con))
  op <- options(width = max(getOption("width"), 200)); on.exit(options(op), add = TRUE)
  id <- id_ejecucion %||% ultima_ejecucion(con)
  if (imprimir) cat("\n######## FASE 5 - CONSULTAS SQL DEL TALLER (ejecución analizada:", id, ") ########\n")
  res <- imap(CONSULTAS_TALLER, function(q, nombre) {
    params <- if (str_detect(q$sql, ":id\\b")) list(id = id) else NULL
    out <- if (is.null(params)) dbGetQuery(con, q$sql) else dbGetQuery(con, q$sql, params = params)
    if (imprimir) {
      cat("\n---", q$titulo, "---\n")
      if (nrow(out) == 0) cat("(sin filas)\n") else print(out, row.names = FALSE, right = FALSE)
    }
    out
  })
  invisible(res)
}

# Exporta el SQL de las consultas a un archivo legible (anexo del informe).
exportar_sql_consultas <- function(ruta = file.path(DIR_PROYECTO, "sql", "consultas_taller.sql")) {
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  txt <- imap_chr(CONSULTAS_TALLER, ~ paste0("-- ", .x$titulo, "\n-- (", .y, ")\n", str_trim(.x$sql), ";\n"))
  writeLines(c("-- Consultas SQL del Taller 1 (SQLite). ':id' = id_ejecucion analizada.", "", txt), ruta, useBytes = TRUE)
  invisible(ruta)
}
