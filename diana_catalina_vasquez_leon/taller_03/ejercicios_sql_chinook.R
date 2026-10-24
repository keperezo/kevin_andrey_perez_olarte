# ==============================================================================
# Taller práctico de SQL - Ejercicios de clase (sección 11)
# Base de datos: Chinook (SQLite)
# ==============================================================================

# 0. Cargar paquetes y conectar -------------------------------------------------
paquetes <- c("DBI", "RSQLite", "dplyr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)
invisible(lapply(paquetes, library, character.only = TRUE))

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

con <- dbConnect(RSQLite::SQLite(), ruta_db)
cat("Conexión establecida.\n\n")

# ==============================================================================
# EJERCICIO 1: Análisis descriptivo de la colección musical
# ==============================================================================

cat("=====================================================\n")
cat("EJERCICIO 1: Análisis descriptivo de la colección musical\n")
cat("=====================================================\n\n")

# 1.1 ¿Cuántas pistas hay en total en la base de datos? ------------------------
cat("--- 1.1 Total de pistas ---\n")
total_pistas <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_pistas
  FROM   Track;
")
print(total_pistas)

# 1.2 ¿Cuántos géneros distintos existen? --------------------------------------
cat("\n--- 1.2 Géneros distintos ---\n")
total_generos <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_generos
  FROM   Genre;
")
print(total_generos)

# 1.3 Nombre del género, número de pistas y duración promedio (min) -----------
#     ordenado de mayor a menor número de pistas
cat("\n--- 1.3 Pistas y duración promedio por género ---\n")
generos_resumen <- dbGetQuery(con, "
  SELECT   g.Name                                AS genero,
           COUNT(t.TrackId)                      AS n_pistas,
           ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS duracion_media_min
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")
print(generos_resumen)

# 1.4 Las 5 pistas más largas: nombre, álbum y duración en minutos ------------
cat("\n--- 1.4 Top 5 pistas más largas (con álbum) ---\n")
top5_largas <- dbGetQuery(con, "
  SELECT   t.Name                              AS pista,
           al.Title                            AS album,
           ROUND(t.Milliseconds / 60000.0, 2) AS duracion_min
  FROM     Track t
  JOIN     Album al ON t.AlbumId = al.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT    5;
")
print(top5_largas)

# 1.5 Pistas con UnitPrice > $0.99 y su porcentaje sobre el total -------------
cat("\n--- 1.5 Pistas con precio > $0.99 y porcentaje del total ---\n")
pistas_caras <- dbGetQuery(con, "
  SELECT
    (SELECT COUNT(*) FROM Track WHERE UnitPrice > 0.99)          AS n_pistas_caras,
    (SELECT COUNT(*) FROM Track)                                  AS n_total_pistas,
    ROUND(
      100.0 * (SELECT COUNT(*) FROM Track WHERE UnitPrice > 0.99)
      / (SELECT COUNT(*) FROM Track),
    2)                                                             AS porcentaje
  ;
")
print(pistas_caras)

# 1.6 Media, mínimo, máximo y varianza de la duración (ms) de todas las pistas
#     SQLite no tiene VAR()/STDDEV(): usamos la fórmula manual
#     Var(X) = E[X^2] - (E[X])^2
cat("\n--- 1.6 Estadísticas descriptivas de duración (ms) ---\n")
stats_duracion <- dbGetQuery(con, "
  SELECT
    ROUND(AVG(Milliseconds), 2)                                    AS media_ms,
    MIN(Milliseconds)                                              AS minimo_ms,
    MAX(Milliseconds)                                              AS maximo_ms,
    ROUND(
      AVG(Milliseconds * Milliseconds) - AVG(Milliseconds) * AVG(Milliseconds),
    2)                                                              AS varianza_ms
  FROM Track;
")
print(stats_duracion)


# ==============================================================================
# EJERCICIO 2: Análisis descriptivo de ventas
# ==============================================================================

cat("\n\n=====================================================\n")
cat("EJERCICIO 2: Análisis descriptivo de ventas\n")
cat("=====================================================\n\n")

# 2.1 Total de ingresos generados por la tienda y promedio por factura --------
cat("--- 2.1 Ingresos totales y promedio por factura ---\n")
ingresos_totales <- dbGetQuery(con, "
  SELECT
    ROUND(SUM(Total), 2) AS ingresos_totales,
    ROUND(AVG(Total), 2) AS promedio_por_factura,
    COUNT(*)             AS n_facturas
  FROM Invoice;
")
print(ingresos_totales)

# 2.2 Facturas emitidas por año, ordenadas cronológicamente ------------------
cat("\n--- 2.2 Facturas emitidas por año ---\n")
facturas_por_anio <- dbGetQuery(con, "
  SELECT   SUBSTR(InvoiceDate, 1, 4) AS anio,
           COUNT(*)                 AS n_facturas
  FROM     Invoice
  GROUP BY anio
  ORDER BY anio ASC;
")
print(facturas_por_anio)

# 2.3 Top 5 países con más ingresos: país, n facturas, ingreso total, --------
#     promedio por factura
cat("\n--- 2.3 Top 5 países por ingresos ---\n")
top5_paises <- dbGetQuery(con, "
  SELECT   BillingCountry       AS pais,
           COUNT(*)             AS n_facturas,
           ROUND(SUM(Total), 2) AS ingreso_total,
           ROUND(AVG(Total), 2) AS promedio_por_factura
  FROM     Invoice
  GROUP BY BillingCountry
  ORDER BY ingreso_total DESC
  LIMIT    5;
")
print(top5_paises)

# 2.4 Desviación estándar del total de facturas -------------------------------
#     Varianza calculada en SQL (fórmula manual), raíz calculada en R
cat("\n--- 2.4 Varianza (SQL) y desviación estándar (R) del total de facturas ---\n")
varianza_facturas <- dbGetQuery(con, "
  SELECT
    ROUND(AVG(Total), 2)                                          AS media,
    ROUND(AVG(Total * Total) - AVG(Total) * AVG(Total), 4)        AS varianza_pob
  FROM Invoice;
")
desviacion_estandar <- sqrt(varianza_facturas$varianza_pob)

cat("Media:              ", varianza_facturas$media, "\n")
cat("Varianza (SQL):     ", varianza_facturas$varianza_pob, "\n")
cat("Desv. estándar (R): ", round(desviacion_estandar, 4), "\n")

# 2.5 Meses con mayor y menor ingreso promedio --------------------------------
#     Se agrupa por mes del año (01-12), combinando todos los años,
#     para identificar patrones estacionales de ingreso promedio.
cat("\n--- 2.5 Ingreso promedio por mes (todos los años combinados) ---\n")
ingreso_por_mes <- dbGetQuery(con, "
  SELECT   SUBSTR(InvoiceDate, 6, 2) AS mes,
           COUNT(*)                 AS n_facturas,
           ROUND(AVG(Total), 2)     AS ingreso_promedio
  FROM     Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio DESC;
")
print(ingreso_por_mes)

cat("\nMes con MAYOR ingreso promedio:\n")
print(ingreso_por_mes[which.max(ingreso_por_mes$ingreso_promedio), ])

cat("\nMes con MENOR ingreso promedio:\n")
print(ingreso_por_mes[which.min(ingreso_por_mes$ingreso_promedio), ])


# 3. Cerrar la conexión --------------------------------------------------------
dbDisconnect(con)
cat("\nConexión cerrada.\n")

