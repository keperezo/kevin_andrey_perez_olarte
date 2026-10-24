# dependencias.R -- declara los paquetes del proyecto para que renv los detecte.
# (No se ejecuta en el análisis; solo sirve para renv::snapshot().)
library(httr2)       # peticiones HTTP (scraper.R)
library(jsonlite)    # lectura/escritura de JSON (scraper.R)
library(DBI)         # interfaz a bases de datos
library(RSQLite)     # SQLite
library(ggplot2)     # gráficos
library(scales)      # formatos de ejes
library(knitr)       # tablas y motor de Quarto para R
library(rmarkdown)   # requerido por el motor knitr de Quarto
