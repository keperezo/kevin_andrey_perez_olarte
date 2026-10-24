# =============================================================================
# ejecutar_todo.R · Corre el flujo completo con una sola instrucción
# =============================================================================
# Abra el proyecto (taller_1.Rproj) en RStudio y escriba en la consola:
#
#   source("ejecutar_todo.R")
#
# Hace, en orden:
#   1. instala los paquetes que falten (solo la primera vez);
#   2. recolecta los tres comercios en línea (10 a 15 minutos) y guarda raw/;
#   3. estandariza, aplica exclusiones y empareja productos en la base;
#   4. escribe renv.lock con las versiones de los paquetes usados, solo si
#      todavía no existe (así no se reemplaza el que se entregó);
#   5. genera taller_1.html a partir de taller_1.qmd.
#
# Para una segunda ejecución otro día basta con volver a correrlo: agrega una
# nueva corrida a la base sin borrar las anteriores.
# =============================================================================

paquetes <- c("httr2", "rvest", "jsonlite", "dplyr", "tidyr", "purrr", "stringr",
              "stringi", "tibble", "DBI", "RSQLite", "ggplot2", "plotly", "patchwork",
              "gt", "knitr", "rmarkdown", "scales", "forcats", "quarto", "renv")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) {
  message("Instalando paquetes que faltan: ", paste(faltan, collapse = ", "))
  install.packages(faltan)
}

source("scraper.R", encoding = "UTF-8")
source("procesamiento.R", encoding = "UTF-8")

# 1) Recolección en línea --------------------------------------------------
resumen <- ejecutar_recoleccion()
print(resumen[, c("comercio", "estado", "productos", "observaciones_nuevas", "total_sitio")])
if (any(resumen$estado != "completa")) {
  message("\nAtención: algún comercio no quedó 'completa'. Revise la columna detalle:")
  print(resumen[, c("comercio", "detalle")])
}

# 2) Limpieza y correspondencia --------------------------------------------
con <- conectar_bd()
procesar_base(con)
ej <- ejecucion_analisis(con)
if (is.na(ej)) {
  message("\nTodavía no hay una ejecución con 8 presentaciones en los tres comercios.")
} else {
  n <- DBI::dbGetQuery(con, "
    SELECT COUNT(DISTINCT a.producto_id) AS n FROM observaciones o
      JOIN anuncios a ON a.anuncio_id = o.anuncio_id
     WHERE o.comparable = 1 AND o.id_ejecucion = ?", params = list(ej))$n
  message("\nEjecución para el análisis: ", ej, " · presentaciones comparables: ", n)
}
DBI::dbDisconnect(con)

# 3) Archivo de dependencias ------------------------------------------------
# force = TRUE: escribe el archivo aunque renv detecte un paquete sugerido que no
# está instalado (por ejemplo shiny, que renv asocia a los documentos con params).
if (!file.exists("renv.lock")) {
  renv::snapshot(type = "implicit", prompt = FALSE, force = TRUE)
} else {
  message("\nrenv.lock ya existe y no se reemplaza (para actualizarlo: renv::snapshot(type = \"implicit\")).")
}

# 4) Documento --------------------------------------------------------------
tryCatch(
  quarto::quarto_render("taller_1.qmd"),
  error = function(e) {
    message("\nNo se pudo generar el documento desde aquí (", conditionMessage(e), ").\n",
            "Abra taller_1.qmd en RStudio y presione el botón Render.")
  }
)

