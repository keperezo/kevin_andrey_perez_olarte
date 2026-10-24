## Taller - Parcial 1 - Minería de Datos

Daniel Mauricio Vanegas Oliveros

*Estudiante de Pregrado en Estadística - Universidad Nacional de Colombia*

### Contenidos:

- Scripts de trabajo (``scraper.r``).
- Carpeta``\raw`` para datos crudos.
- Archivo de análisis (``mineria_1.qmd``).
- Informe de análisis (``mineria_1.http``).
- Informe **alternativo** de análisis (``mineria_1.pdf``).

### Software Utilizado

El taller es desarrollado en R, haciendo uso de los siguientes paquetes:

Paquetes:

- ``rvest``
- ``xml2``
- ``httr2``
- ``dplyr``
- ``stringr``
- ``purrr``
- ``tibble``
- ``janitor``
- ``readr``
- ``knitr``
- ``chromote``
- ``jsonlite``
- ``RSQLite``
- ``DBI``
- ``knitr``
- ``kableExtra``
- ``scales``
- ``ggplot2``

**Es necesario usar RStudio para que funcionen los scripts.** Esto debido al comando:

``setwd(dirname(rstudioapi::getSourceEditorContext()$path))``

### Uso de IA

Se hizo uso de IA con Claude - Sonnet 5.5 para los siguientes fines:

- Limpieza de código.
- Incorporación de funciones SQL.
- Asistencia con funciones de graficación para el análisis.
- Corregir la redacción.
- Mejorar los visuales de Quarto (``kableExtra`` y ``ggplot2``).