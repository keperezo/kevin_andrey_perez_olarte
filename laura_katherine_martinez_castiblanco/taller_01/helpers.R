# =============================================================================
# helpers.R · Funciones de apoyo del entorno del informe (NO contienen el taller)
# Ajustado al scraper.R de Éxito, Carulla y PriceSmart: tablas `ejecuciones` y `observaciones` (22 variables).
# Se cargan con source("helpers.R") desde el chunk `setup` de taller_1.Rmd.
# Para agregar una función nueva, escríbala aquí y úsela en cualquier sección.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(DT); library(htmltools); library(ggplot2)
})

# ---- Utilidades generales ---------------------------------------------------
# Escribe HTML crudo sin que Pandoc lo reinterprete (válido dentro de chunks results='asis')
raw_html <- function(x) cat("\n`````{=html}\n", paste(x, collapse = "\n"), "\n`````\n", sep = "")

# Busca un archivo en la carpeta actual o en la carpeta de entrega del repositorio
CARPETA_ENTREGA <- file.path("laura_katherine_martinez_castiblanco", "taller_01")
ruta <- function(f) {
  cand <- c(f, file.path(CARPETA_ENTREGA, f)); hit <- cand[file.exists(cand) | dir.exists(cand)]
  if (length(hit)) hit[1] else f
}

# Pesos colombianos: fmt_cop(12990) -> "$12.990"
fmt_cop <- function(x) ifelse(is.na(x), "sin dato",
  paste0("$", formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))

# Abre la base SQLite si existe; si no, devuelve una base vacía en memoria (el informe sigue tejiendo)
abrir_db <- function(path) {
  f <- ruta(path)
  if (file.exists(f)) dbConnect(SQLite(), f) else dbConnect(SQLite(), ":memory:")
}

# ---- Tarjetas de métricas (estilo etiqueta de precio) ------------------------
# tarjeta("3", "Comercios", "extraídos por scraping", tono = "good" | "tie" | "bad" | "")
tarjeta <- function(valor, etiqueta, nota = "", tono = "") {
  sprintf('<div class="metric %s"><div class="metric-in"><div class="metric-value">%s</div><div class="metric-label">%s</div><div class="metric-note">%s</div></div></div>',
          tono, htmlEscape(valor), htmlEscape(etiqueta), htmlEscape(nota))
}
# tarjetas(tarjeta(...), tarjeta(...)) dentro de un chunk results='asis'
tarjetas <- function(...) raw_html(c('<div class="metric-grid">', unlist(list(...)), "</div>"))


# ---- Vistas de análisis sobre el esquema de scraper.R -----------------------------------------
# Crea dos vistas temporales con la ejecución MÁS RECIENTE de `observaciones`:
#   v_categoria   : toda la categoría (precio > 0) + volumen_ml normalizado a mililitros
#   v_comparables : solo filas con comparable = 1 e id_producto_comun definido
# Úselas en las consultas SQL del informe (chunks {sql, connection=con}).
# Devuelve list(ok, n_obs, n_comp, ref) con la fecha de la ejecución de referencia.
crear_vistas <- function(con) {
  hay <- dbExistsTable(con, "observaciones") && dbGetQuery(con, "SELECT COUNT(*) n FROM observaciones")$n > 0
  for (v in c("v_comparables", "v_categoria")) dbExecute(con, paste0("DROP VIEW IF EXISTS ", v))
  if (!dbExistsTable(con, "observaciones")) return(list(ok = FALSE, n_obs = 0, n_comp = 0, ref = NA_character_))
  dbExecute(con, "CREATE TEMP VIEW v_categoria AS
    SELECT o.*, CASE WHEN LOWER(unidad_medida) = 'l' THEN contenido_num * 1000 ELSE contenido_num END AS volumen_ml
    FROM observaciones o
    WHERE id_ejecucion = (SELECT MAX(id_ejecucion) FROM observaciones) AND precio_cop > 0")
  dbExecute(con, "CREATE TEMP VIEW v_comparables AS
    SELECT * FROM v_categoria WHERE comparable = 1 AND id_producto_comun IS NOT NULL")
  n_obs  <- dbGetQuery(con, "SELECT COUNT(*) n FROM v_categoria")$n
  n_comp <- dbGetQuery(con, "SELECT COUNT(*) n FROM v_comparables")$n
  # Fecha de la ejecución de referencia. id_ejecucion puede ser numérico (época Unix, scraper actual)
  # o texto (versiones anteriores del scraper): se muestra como fecha si es numérico y tal cual si no.
  ref <- NA_character_
  if (hay) {
    m <- dbGetQuery(con, "SELECT MAX(id_ejecucion) AS m FROM observaciones")$m
    ref <- tryCatch(
      if (is.numeric(m) && !is.na(m)) format(as.POSIXct(m, origin = "1970-01-01", tz = "America/Bogota"), "%Y-%m-%d %H:%M")
      else as.character(m),
      error = function(e) as.character(m))
  }
  list(ok = hay, n_obs = n_obs, n_comp = n_comp, ref = ref)
}

# Tarjetas del resumen ejecutivo calculadas con v_categoria / v_comparables (vacías si no hay datos).
# Úselas en un chunk results='asis':  tarjetas_resumen(con)
tarjetas_resumen <- function(con) {
  q <- function(sql) tryCatch(dbGetQuery(con, sql), error = function(e) data.frame())
  n_com <- q("SELECT COUNT(DISTINCT comercio) n FROM v_categoria")$n
  if (!length(n_com) || is.na(n_com) || n_com == 0) {
    return(tarjetas(tarjeta("—", "Comercios con datos"), tarjeta("—", "Productos únicos"),
                    tarjeta("—", "Presentaciones comparables"), tarjeta("—", "Producto comparable más económico", tono = "good"),
                    tarjeta("—", "Última actualización")))
  }
  n_prod <- q("SELECT COUNT(DISTINCT comercio || '|' || sku_codigo) n FROM v_categoria")$n
  n_pc   <- q("SELECT COUNT(DISTINCT id_producto_comun) n FROM v_comparables")$n
  bar    <- q("SELECT id_producto_comun, comercio, precio_cop FROM v_comparables ORDER BY precio_cop LIMIT 1")
  ult    <- q("SELECT MAX(fecha_hora_consulta) f FROM observaciones")$f
  tarjetas(
    tarjeta(n_com, "Comercios con datos", "Éxito, Carulla y PriceSmart"),
    tarjeta(format(n_prod, big.mark = "."), "Productos únicos", "en la ejecución de referencia"),
    tarjeta(n_pc, "Presentaciones comparables", "marcadas con comparable = 1"),
    if (nrow(bar)) tarjeta(fmt_cop(bar$precio_cop), "Producto comparable más económico", paste(bar$id_producto_comun, "en", bar$comercio), "good")
      else tarjeta("—", "Producto comparable más económico", "defina comparable e id_producto_comun", "good"),
    tarjeta(substr(ult, 1, 10), "Última actualización", paste("hora", substr(ult, 12, 19), "(Colombia)"))
  )
}

# ---- Tablas interactivas (DT): búsqueda, paginación, filtros por columna ------
ES <- list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ filas", info = "Filas _START_ a _END_ de _TOTAL_",
           infoEmpty = "Sin filas", infoFiltered = "(filtrado de _MAX_)", zeroRecords = "Sin coincidencias",
           emptyTable = "Sin datos para mostrar",
           paginate = list(first = "Primera", last = "Última", `next` = "Siguiente", previous = "Anterior"))

# tabla_interactiva(df, cols_cop = c("precio"), cols_pct = "ahorro", filtros = "top" | "bottom" | "none", page = 10)
# Columnas de texto muy largo (HTML, URLs) se acortan en pantalla; el dato completo se conserva y se exporta.
tabla_interactiva <- function(df, cols_cop = NULL, cols_pct = NULL, filtros = "top", page = 10,
                              largas = c("texto_original_anuncio", "url_extraccion", "url_imagen"), ...) {
  idx <- which(names(df) %in% largas) - 1
  defs <- if (length(idx)) list(list(targets = idx, render = DT::JS(
    "function(d, type){ return (type === 'display' && typeof d === 'string' && d.length > 48) ? '<span title=\"' + d.replace(/\"/g,'&quot;').substr(0,400) + '\">' + d.substr(0,48).replace(/</g,'&lt;') + '…</span>' : d; }"))) else NULL
  w <- DT::datatable(
    df, rownames = FALSE, filter = filtros, extensions = "Buttons", class = "display compact stripe hover", escape = TRUE,
    options = list(
      pageLength = page, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE, dom = "Blfrtip", language = ES, columnDefs = defs,
      buttons = list(list(extend = "copy", text = "Copiar"), list(extend = "csv", text = "CSV"),
                     list(extend = "excel", text = "Excel"), list(extend = "colvis", text = "Columnas"))), ...)
  if (!is.null(cols_cop)) w <- DT::formatCurrency(w, cols_cop, currency = "$", digits = 0, mark = ".", dec.mark = ",")
  if (!is.null(cols_pct)) w <- DT::formatRound(w, cols_pct, digits = 1, dec.mark = ",")
  w
}
tabla_dt <- tabla_interactiva   # alias corto

# Varias tablas en un mismo chunk: varias_tablas(tabla_interactiva(a), tabla_interactiva(b))
varias_tablas <- function(...) htmltools::tagList(...)

# ---- Gráficos interactivos (plotly) -------------------------------------------
# Convierte un ggplot en gráfico interactivo (zoom, tooltip, ocultar series con la leyenda).
# Si plotly no está instalado, devuelve el ggplot estático sin romper el informe.
grafico_interactivo <- function(p, tooltip = "all", alto = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(p)
  g <- plotly::ggplotly(p, tooltip = tooltip, height = alto)
  g <- plotly::layout(g, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                      font = list(color = "#718096"), legend = list(orientation = "h", y = -0.2))
  plotly::config(g, displaylogo = FALSE, locale = "es",
                 modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# Tema de ggplot con fondo transparente y texto gris medio (legible en modo claro y oscuro)
tema_kdd <- function(base = 12) {
  gris <- "#718096"
  theme_minimal(base_size = base) +
    theme(text = element_text(color = gris), axis.text = element_text(color = gris),
          plot.title = element_text(face = "bold", size = base + 2, color = gris),
          plot.subtitle = element_text(color = gris), plot.caption = element_text(color = gris, hjust = 0),
          panel.grid.major = element_line(color = "#8a97a840"), panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = NA, color = NA), panel.background = element_blank(),
          legend.position = "bottom", legend.title = element_blank(), strip.text = element_text(color = gris, face = "bold"))
}
# Paleta fija por categoría (colores distinguibles, aptos para daltonismo)
paleta_kdd <- function(niveles) setNames(rep_len(c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9"), length(niveles)), niveles)

# ---- Checklist de entrega ------------------------------------------------------
check_item <- function(ok, label, detalle = "", warn = FALSE) {
  cls <- if (ok) "ok" else if (warn) "warn" else "bad"
  sprintf('<li class="%s"><span class="st">%s</span><span class="tx">%s<small>%s</small></span></li>', cls, if (ok) "✓" else "!", label, detalle)
}
manual_item <- function(label, detalle = "") {
  sprintf('<li class="manual" data-key="%s"><label><input type="checkbox"><span class="tx">%s<small>%s</small></span></label></li>',
          gsub("[^a-z0-9]+", "-", tolower(label)), label, detalle)
}
# checklist(check_item(...), manual_item(...)) en un chunk results='asis'
checklist <- function(...) raw_html(c('<div class="check-progress"><div class="bar"><i></i></div><b>0 / 0</b></div>',
                                       '<ul class="check-list">', unlist(list(...)), "</ul>"))

# ---- Entorno: datos que lee scripts.js (panel lateral, visor de código, descarga) ----
# Inserta (1) la configuración, (2) el código fuente de cada archivo para el visor y
# (3) la base de datos codificada, si pesa menos de `max_mb`; si no, deja un enlace relativo.
embed_entorno <- function(meta, archivos = character(), db_path = NULL, max_mb = 25) {
  proteger <- function(x) gsub("<!--", "<@@!--", gsub("</script", "<@@/script", x, fixed = TRUE), fixed = TRUE)
  lang_de <- function(f) switch(tolower(tools::file_ext(f)), r = "r", sql = "sql", py = "python", rmd = "markdown", "text")
  db <- list(name = NULL, embedded = FALSE, href = NULL, size_mb = NULL)
  out <- character()
  if (!is.null(db_path) && file.exists(ruta(db_path))) {
    f <- ruta(db_path); mb <- file.info(f)$size / 1024^2
    db$name <- basename(f); db$size_mb <- round(mb, 2)
    if (mb <= max_mb) {
      db$embedded <- TRUE
      out <- c(out, sprintf('<script type="text/plain" id="kdd-db-b64">%s</script>',
                            jsonlite::base64_enc(readBin(f, "raw", n = file.info(f)$size))))
    } else db$href <- db_path   # demasiado grande para incrustar: enlace relativo junto al HTML
  }
  fuentes <- character()
  for (f in archivos) {
    if (!file.exists(ruta(f))) next
    txt <- paste(readLines(ruta(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    fuentes <- c(fuentes, sprintf('<script type="text/plain" class="kdd-source" data-name="%s" data-lang="%s">%s</script>',
                                  basename(f), lang_de(f), proteger(txt)))
  }
  meta$db <- db; meta$archivos <- as.list(basename(archivos[file.exists(vapply(archivos, ruta, ""))]))
  cfg <- sprintf('<script type="application/json" id="kdd-config">%s</script>', jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null"))
  raw_html(c(cfg, out, fuentes))
}

# ---- Datos de ejemplo SOLO para la guía del entorno (se pueden borrar) -------------
demo_precios <- function() {
  set.seed(7)
  p <- expand.grid(producto = c("Agua 600 ml", "Gaseosa 1.5 L", "Jugo 1 L", "Té 400 ml", "Energizante 473 ml"),
                   comercio = c("Comercio A", "Comercio B", "Comercio C"), stringsAsFactors = FALSE)
  base <- c(2100, 4600, 5200, 3300, 6900)[match(p$producto, c("Agua 600 ml", "Gaseosa 1.5 L", "Jugo 1 L", "Té 400 ml", "Energizante 473 ml"))]
  p$precio_cop <- round(base * runif(nrow(p), .88, 1.18) / 10) * 10
  p
}
