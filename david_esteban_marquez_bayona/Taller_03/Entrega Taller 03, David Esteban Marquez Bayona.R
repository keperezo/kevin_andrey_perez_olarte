# =============================================================================
# Taller práctico de SQL — Parte 1 · Ejercicios de clase
# Curso de Minería de Datos — UNAL, Departamento de Estadística
# Base de datos: Chinook (SQLite)
# =============================================================================

# ---- 0. Preparación ---------------------------------------------------------
paquetes   <- c("DBI", "RSQLite", "knitr")
pendientes <- setdiff(paquetes, rownames(installed.packages()))
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)
library(knitr)

url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"
if (!file.exists(ruta_db)) download.file(url_chinook, destfile = ruta_db, mode = "wb")

con <- dbConnect(RSQLite::SQLite(), ruta_db)

# Función auxiliar para imprimir tablas
mostrar_tabla <- function(df, caption = NULL, digits = 2) {
  print(kable(df, caption = caption, digits = digits,
              format.args = list(big.mark = ",", scientific = FALSE)))
  cat("\n")
}


# =============================================================================
# EJERCICIO 1: Análisis descriptivo de la colección musical
# =============================================================================

# ---- 1.1 ¿Cuántas pistas hay en total? --------------------------------------
e1_1 <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_pistas
  FROM   Track;
")
mostrar_tabla(e1_1, caption = "1.1 Total de pistas")


# ---- 1.2 ¿Cuántos géneros distintos existen? --------------------------------
# Se cuentan tanto los géneros del catálogo (tabla Genre) como los que
# efectivamente tienen pistas asociadas (Track.GenreId).
e1_2 <- dbGetQuery(con, "
  SELECT (SELECT COUNT(*)                FROM Genre) AS generos_catalogo,
         (SELECT COUNT(DISTINCT GenreId) FROM Track) AS generos_con_pistas;
")
mostrar_tabla(e1_2, caption = "1.2 Géneros distintos")


# ---- 1.3 Pistas y duración promedio por género -------------------------------
e1_3 <- dbGetQuery(con, "
  SELECT   g.Name                                  AS genero,
           COUNT(t.TrackId)                        AS n_pistas,
           ROUND(AVG(t.Milliseconds) / 60000.0, 2) AS duracion_media_min
  FROM     Track t
  JOIN     Genre g ON t.GenreId = g.GenreId
  GROUP BY g.GenreId, g.Name
  ORDER BY n_pistas DESC;
")
mostrar_tabla(e1_3, caption = "1.3 Número de pistas y duración media por género")


# ---- 1.4 Las 5 pistas más largas --------------------------------------------
e1_4 <- dbGetQuery(con, "
  SELECT   t.Name                               AS pista,
           a.Title                              AS album,
           ROUND(t.Milliseconds / 60000.0, 2)   AS duracion_min
  FROM     Track t
  JOIN     Album a ON t.AlbumId = a.AlbumId
  ORDER BY t.Milliseconds DESC
  LIMIT    5;
")
mostrar_tabla(e1_4, caption = "1.4 Las 5 pistas más largas")


# ---- 1.5 Pistas con UnitPrice > 0.99 y su porcentaje ------------------------
e1_5 <- dbGetQuery(con, "
  SELECT SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END)   AS pistas_precio_mayor,
         COUNT(*)                                            AS total_pistas,
         ROUND(100.0 * SUM(CASE WHEN UnitPrice > 0.99 THEN 1 ELSE 0 END)
               / COUNT(*), 2)                                AS porcentaje
  FROM   Track;
")
mostrar_tabla(e1_5, caption = "1.5 Pistas con precio superior a $0.99")


# ---- 1.6 Media, mínimo, máximo y varianza de Milliseconds --------------------
# SQLite no trae VARIANCE(): se usa la fórmula manual vista en clase.
#   Varianza poblacional: AVG(x^2) - AVG(x)^2
#   Varianza muestral   : varianza poblacional * n / (n - 1)
e1_6 <- dbGetQuery(con, "
  SELECT COUNT(*)                                             AS n,
         ROUND(AVG(Milliseconds), 2)                          AS media_ms,
         MIN(Milliseconds)                                    AS min_ms,
         MAX(Milliseconds)                                    AS max_ms,
         AVG(Milliseconds * Milliseconds)
           - AVG(Milliseconds) * AVG(Milliseconds)            AS varianza_pob_ms,
         (AVG(Milliseconds * Milliseconds)
           - AVG(Milliseconds) * AVG(Milliseconds))
           * COUNT(*) / (COUNT(*) - 1.0)                      AS varianza_muestral_ms
  FROM   Track;
")
mostrar_tabla(e1_6, caption = "1.6 Estadísticos de la duración (ms)")

# Verificación con R (var() usa n - 1)
ms <- dbGetQuery(con, "SELECT Milliseconds FROM Track;")$Milliseconds
cat("Verificación R -> media:", round(mean(ms), 2),
    "| varianza muestral:", format(var(ms), big.mark = ","), "\n\n")


# =============================================================================
# EJERCICIO 2: Análisis descriptivo de ventas
# =============================================================================

# ---- 2.1 Ingreso total y promedio por factura -------------------------------
# Ingreso total a partir del detalle (InvoiceLine) y promedio por factura (Invoice)
e2_1 <- dbGetQuery(con, "
  SELECT (SELECT ROUND(SUM(UnitPrice * Quantity), 2) FROM InvoiceLine) AS ingreso_total,
         (SELECT COUNT(*)                            FROM Invoice)     AS n_facturas,
         (SELECT ROUND(AVG(Total), 2)                FROM Invoice)     AS promedio_por_factura;
")
mostrar_tabla(e2_1, caption = "2.1 Ingreso total y promedio por factura")

# Comprobación: la suma de Invoice.Total coincide con la de InvoiceLine
dbGetQuery(con, "SELECT ROUND(SUM(Total), 2) AS total_desde_invoice FROM Invoice;")


# ---- 2.2 Facturas por año ---------------------------------------------------
e2_2 <- dbGetQuery(con, "
  SELECT   SUBSTR(InvoiceDate, 1, 4)  AS anio,
           COUNT(*)                   AS n_facturas,
           ROUND(SUM(Total), 2)       AS ingreso_total
  FROM     Invoice
  GROUP BY anio
  ORDER BY anio;
")
mostrar_tabla(e2_2, caption = "2.2 Facturas emitidas por año")


# ---- 2.3 Top 5 países por ingresos ------------------------------------------
e2_3 <- dbGetQuery(con, "
  SELECT   BillingCountry         AS pais,
           COUNT(*)               AS n_facturas,
           ROUND(SUM(Total), 2)   AS ingreso_total,
           ROUND(AVG(Total), 2)   AS promedio_por_factura
  FROM     Invoice
  GROUP BY BillingCountry
  ORDER BY SUM(Total) DESC
  LIMIT    5;
")
mostrar_tabla(e2_3, caption = "2.3 Los 5 países con más ingresos")


# ---- 2.4 Desviación estándar del total de facturas --------------------------
# Paso 1: varianza en SQL
e2_4 <- dbGetQuery(con, "
  SELECT COUNT(*)                                        AS n,
         AVG(Total * Total) - AVG(Total) * AVG(Total)    AS varianza_pob,
         (AVG(Total * Total) - AVG(Total) * AVG(Total))
           * COUNT(*) / (COUNT(*) - 1.0)                 AS varianza_muestral
  FROM   Invoice;
")

# Paso 2: raíz cuadrada en R
e2_4$desv_est_pob      <- sqrt(e2_4$varianza_pob)
e2_4$desv_est_muestral <- sqrt(e2_4$varianza_muestral)
mostrar_tabla(e2_4, caption = "2.4 Varianza (SQL) y desviación estándar (R) del total por factura",
              digits = 4)

# Verificación con sd() de R (muestral)
totales <- dbGetQuery(con, "SELECT Total FROM Invoice;")$Total
cat("Verificación R -> sd():", round(sd(totales), 4), "\n\n")


# ---- 2.5 Meses con mayor y menor ingreso promedio ---------------------------
# (a) Ingreso promedio por factura según el mes del año (enero a diciembre,
#     acumulando todos los años)
e2_5 <- dbGetQuery(con, "
  SELECT   CAST(SUBSTR(InvoiceDate, 6, 2) AS INTEGER)  AS mes,
           COUNT(*)                                    AS n_facturas,
           ROUND(SUM(Total), 2)                        AS ingreso_total,
           ROUND(AVG(Total), 4)                        AS ingreso_promedio
  FROM     Invoice
  GROUP BY mes
  ORDER BY ingreso_promedio DESC;
")
e2_5$nombre_mes <- month.name[e2_5$mes]
mostrar_tabla(e2_5, caption = "2.5a Ingreso promedio por factura según mes del año", digits = 4)

cat("Mes con MAYOR ingreso promedio:", e2_5$nombre_mes[1],
    "(", e2_5$ingreso_promedio[1], ")\n")
cat("Mes con MENOR ingreso promedio:", e2_5$nombre_mes[nrow(e2_5)],
    "(", e2_5$ingreso_promedio[nrow(e2_5)], ")\n\n")

# (b) Complemento: los mismos cálculos por año-mes (cada mes calendario concreto)
e2_5b <- dbGetQuery(con, "
  WITH por_mes AS (
    SELECT   SUBSTR(InvoiceDate, 1, 7)  AS anio_mes,
             COUNT(*)                   AS n_facturas,
             ROUND(SUM(Total), 2)       AS ingreso_total,
             ROUND(AVG(Total), 2)       AS ingreso_promedio
    FROM     Invoice
    GROUP BY anio_mes
  )
  SELECT 'Mayor' AS extremo, * FROM por_mes
  WHERE  ingreso_promedio = (SELECT MAX(ingreso_promedio) FROM por_mes)
  UNION ALL
  SELECT 'Menor' AS extremo, * FROM por_mes
  WHERE  ingreso_promedio = (SELECT MIN(ingreso_promedio) FROM por_mes);
")
mostrar_tabla(e2_5b, caption = "2.5b Año-mes con mayor y menor ingreso promedio por factura")


# ---- Cierre -----------------------------------------------------------------
dbDisconnect(con)