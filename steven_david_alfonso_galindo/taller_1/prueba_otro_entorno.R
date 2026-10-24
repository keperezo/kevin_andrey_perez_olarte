# =============================================================================
# prueba_otro_entorno.R · Recolección desde un entorno distinto al computador
# personal (sugerencia de la sección 3.1 del enunciado)
# =============================================================================
# Sirve para Google Colab (entorno de ejecución R), GitHub Actions o cualquier
# servidor con R. Usa una base y una carpeta de copias propias, así que no
# mezcla sus datos con los de las corridas del análisis.
#
# En Google Colab:
#   1. Entorno de ejecución > Cambiar tipo de entorno de ejecución > R.
#   2. Suba la carpeta del proyecto comprimida (taller_1.zip) en el panel Archivos.
#   3. En una celda:
#        unzip("taller_1.zip"); setwd("taller_1"); source("prueba_otro_entorno.R")
#   4. Descargue datos/prueba_otro_entorno.csv y cópielo en la carpeta datos/ del
#      proyecto en su computador. Al volver a renderizar, taller_1.qmd muestra el
#      resultado en la sección "Prueba desde otro entorno".
#
# Si quiere, defina antes el nombre del entorno: Sys.setenv(ENTORNO_PRUEBA = "Google Colab")
# =============================================================================

paquetes <- c("httr2", "rvest", "jsonlite", "dplyr", "purrr", "stringr", "tibble", "DBI", "RSQLite")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)

source("scraper.R", encoding = "UTF-8")

# Nombre del entorno: el que defina el usuario o uno deducido.
entorno <- Sys.getenv("ENTORNO_PRUEBA")
if (entorno == "") {
  entorno <- if (Sys.getenv("GITHUB_ACTIONS") == "true") "GitHub Actions" else
    if (dir.exists("/content")) "Google Colab" else
      paste("Otro entorno", Sys.info()[["sysname"]])
}

CONFIG$ruta_raw <- "raw_otro_entorno"          # copias aparte de las del análisis
inicio <- ahora_iso()
resumen <- ejecutar_recoleccion(ruta_bd = "prueba_otro_entorno.sqlite")

salida <- resumen |>
  transmute(entorno = entorno,
            sistema = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
            version_r = R.version.string,
            inicio = inicio,
            comercio, estado, productos, total_sitio, detalle)
dir.create("datos", showWarnings = FALSE)
utils::write.csv(salida, file.path("datos", "prueba_otro_entorno.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(salida[, c("entorno", "comercio", "estado", "productos", "total_sitio")])
message("Resumen guardado en datos/prueba_otro_entorno.csv")
