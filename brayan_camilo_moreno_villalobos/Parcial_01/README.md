------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# Comparador de Precios y Minería de Datos en Retail

## Descripción del Proyecto

Este proyecto implementa un flujo automatizado de *web scraping*, limpieza de datos, estandarización de unidades y análisis comparativo de precios en el sector de alimentos para mascotas a través de las tres principales cadenas de *retail* en Colombia: **Éxito, Jumbo y Olímpica**.

Los datos recolectados se almacenan en una base de datos relacional ligera (**SQLite**) y se explotan mediante consultas estructuradas en **SQL**, generando visualizaciones interactivas y un reporte ejecutivo integrado mediante **Quarto**.

------------------------------------------------------------------------

## Lenguaje y Tecnologías Utilizadas

- **Lenguaje:** R (Versión 4.x o superior)
- **Entorno de Reportes:** Quarto (`.qmd`)
- **Base de Datos:** SQLite (`DBI`, `RSQLite`)
- **Manipulación y Limpieza:** `tidyverse` (`dplyr`, `stringr`, `stringi`)
- **Visualización:** `ggplot2`
- **Web Scraping:** `rvest`

------------------------------------------------------------------------

## Estructura de Archivos

El proyecto está organizado de la siguiente manera dentro del directorio de trabajo:

``` text
├── scraper.r                  # Módulo principal de extracción web y función de limpieza
├── taller_1.qmd        # Documento Quarto principal con análisis, SQL y gráficos
├── styles.css                 # Hoja de estilos CSS personalizada para el formato HTML
├── comparador_precios.sqlite  # Base de datos SQLite generada automáticamente
└── README.md                  # Documentación del proyecto
```

------------------------------------------------------------------------

## Instrucciones de Ejecución

El proyecto soporta dos modalidades de ejecución según la disponibilidad de conexión a internet:

### 1. Modo en Línea (Ejecución Real con Scraping Activo)

Si deseas realizar una extracción fresca directamente desde las páginas web de los supermercados: 1. Asegúrate de tener conexión a internet estable. 2. Abre el archivo principal `informe_precios.qmd` en **RStudio**. 3. Configura el parámetro de ejecución en el script de R para habilitar la red: `r    df_crudo <- ejecutar_recoleccion_completa(max_paginas = 55, modo_offline = FALSE)` 4. Haz clic en el botón **Render** en RStudio para compilar el documento completo, actualizar la base de datos SQLite y generar la página web interactiva.

### 2. Modo Sin Conexión (Offline / Datos Pre-cargados)

Si las páginas de los comercios bloquean temporalmente el tráfico o no dispones de internet, puedes ejecutar el flujo utilizando respaldos locales almacenados: 1. Abre el archivo `informe_precios.qmd` en **RStudio**. 2. Configura el parámetro de ejecución en modo offline dentro de tu chunk de carga: `r    df_crudo <- ejecutar_recoleccion_completa(max_paginas = 55, modo_offline = TRUE)` *(Este modo cargará automáticamente el último respaldo local en formato `.html` ).* 3. Presiona **Render**. El sistema leerá la base SQLite preexistente y generará las consultas SQL y gráficos sin realizar solicitudes HTTP externas.

------------------------------------------------------------------------

## Autor

- **Brayan Camilo Moreno Villalobos**
