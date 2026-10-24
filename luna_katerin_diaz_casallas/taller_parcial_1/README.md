# Taller 1 – Comparador de precios de vitaminas

**Asignatura:** Minería de Datos (2016325) – Universidad Nacional de Colombia
**Autora:** Luna Katerin Diaz
**Lenguaje:** R (probado con R 4.6.1)

Comparador de precios construido con *web scraping* sobre tres droguerías con venta en línea en Colombia:

- **Cafam** (drogueriascafam.com.co)
- **La Rebaja Virtual** (larebajavirtual.com)
- **La Alemana** (tudrogueriavirtual.com)

Para la categoría **vitaminas**, el código extrae los productos que cada sitio lista, los limpia y los carga en SQLite. Después compara 10 presentaciones disponibles en los tres comercios mediante consultas SQL y gráficos.

Todo el proceso está en R: extracción, limpieza, carga en SQLite, consultas y gráficos.

## Estructura de archivos

```
taller_parcial_1/
├── taller_1.Rmd               # Documento reproducible: limpieza, SQLite, consultas SQL, gráficos e interpretación
├── scraper.R                  # Módulo de extracción (una función por comercio + función general)
├── comparador_precios.sqlite  # Base de datos (se reconstruye desde raw/ al compilar el documento)
├── renv.lock                  # Dependencias con versiones fijas
├── README.md                  # Este archivo
└── raw/                       # Copias crudas
    ├── cafam_vitaminas_resultados.csv     # Primera ejecución (11-oct-2026), productos ya extraídos
    ├── rebaja_vitaminas_resultados.csv
    ├── alemana_vitaminas_resultados.csv
    └── <comercio>/<id_ejecucion>/         # Ejecuciones siguientes
        ├── paginas.csv                    # Página, URL y fecha-hora de consulta
        └── pagina_NN.html.gz              # HTML original de cada página
```

`taller_1.html` es la salida de compilar el documento.

## Requisitos

- **R 4.2 o superior** (el `renv.lock` registra R 4.6.1).
- **Google Chrome o Chromium**, solo para el modo en línea: `chromote` controla un Chrome sin interfaz.

Para instalar las versiones exactas de los paquetes, abre R en esta carpeta y ejecuta:

```r
install.packages("renv")
renv::restore()
```

Si `renv` pregunta si quieres activar el proyecto, cualquiera de las dos opciones funciona.

Los paquetes principales son `rvest`, `xml2`, `httr2`, `chromote`, `dplyr`, `stringr`, `stringi`, `purrr`, `tibble`, `janitor`, `readr`, `lubridate`, `DBI`, `RSQLite`, `knitr`, `rmarkdown` y `ggplot2`.

## Cómo ejecutarlo

### Sin conexión (opción por defecto)

Abre `taller_1.Rmd` en RStudio y presiona **Knit**, o desde R:

```r
rmarkdown::render("taller_1.Rmd")
```

El documento llama a `ejecutar_recoleccion_completa(modo_offline = TRUE)`, que vuelve a extraer los productos del HTML guardado en `raw/` (y del CSV de la primera ejecución) sin consultar los sitios. Luego crea `comparador_precios.sqlite` si no existe y carga las ejecuciones. La carga es **idempotente**: compilar varias veces no duplica datos.

### En línea (nueva recolección)

```r
source("scraper.R")
ejecutar_recoleccion_completa(db_path = "comparador_precios.sqlite", modo_offline = FALSE)
```

Crea una ejecución nueva con identificador `AAAAMMDD_HHMMSS`, en hora de Colombia. Recorre las páginas de resultados de "vitaminas" de cada sitio hasta que una página ya no trae productos nuevos; esto toma unos 35 minutos por las pausas entre páginas. Guarda el HTML de cada página en `raw/<comercio>/<id_ejecucion>/`, registra el total de productos que muestra el sitio y agrega las observaciones a la base **sin sobrescribir las anteriores**.

Para limitar las páginas se pueden pasar `paginas_cafam`, `paginas_rebaja` o `paginas_alemana` (por ejemplo, `1:5`). Después de una recolección en línea, vuelve a compilar `taller_1.Rmd`: el análisis usa la ejecución más reciente que tenga los tres comercios.

## Organización del código

**`scraper.R`** contiene:

- **Una función por comercio** (`scrapear_pagina_cafam()`, `scrapear_pagina_rebaja()` y `scrapear_pagina_alemana()`, con sus `extraer_producto_*()`). Cada una abre la página con `chromote`, se desplaza para forzar la carga diferida, guarda el HTML, lo lee con `rvest` y devuelve siempre las mismas columnas.
- **`ejecutar_recoleccion_completa()`**, la función general. Ejecuta la recolección en línea o sin conexión, registra cada ejecución y carga los datos.
- **Funciones auxiliares:** `recorrer_paginas()` (paginación hasta el fin del listado), `leer_total_sitio()`, `guardar_html_crudo()`, `leer_copias_html()`, `leer_copias_csv()`, `inicializar_bd_sqlite()`, `cargar_comercio()` e `insertar_en_sqlite()`.

**`taller_1.Rmd`** llama a esas funciones sin duplicar su código. Luego hace la limpieza, la correspondencia de productos, las consultas SQL y los gráficos.

## Base de datos (`comparador_precios.sqlite`)

| Objeto | Contenido |
|---|---|
| `ejecuciones` | Una fila por corrida y comercio: identificador, fecha y hora de inicio y fin, modo, páginas, registros obtenidos, productos distintos, total que muestra el sitio y estado (`completa`, `parcial` o `fallida`) |
| `historico_productos` | Una fila por observación de precio: texto original del anuncio, URL, precio, precio antes del descuento, indicador de promoción, fecha y hora de consulta (ISO 8601, `-05:00`) y ejecución. Tiene restricción de unicidad sobre `(comercio, enlace, fecha_extraccion)` |
| `anuncios` | Una fila por producto de cada comercio: código interno tomado de la URL, campos estandarizados (nombre, marca, subcategoría, contenido y unidad), motivo de descarte, identificador común `id_producto` e indicador `comparable` |
| `productos_comparables` | Las 10 presentaciones comparables con su identificador común, marca, subcategoría, contenido y unidad |
| `v_observaciones`, `v_precios_comparables` | Vistas para las consultas: cada precio con su anuncio, y una observación por ejecución, comercio y presentación comparable, con precio por unidad |

`ejecuciones` y `historico_productos` las llena `scraper.R`. `anuncios` y `productos_comparables` se reconstruyen desde `historico_productos` en cada compilación del documento.

## Notas

- Los precios corresponden al momento de cada captura y no incluyen envío ni descuentos por afiliación o medio de pago.
- La primera ejecución (11-oct-2026) solo conserva en `raw/` el CSV con los productos ya extraídos. Desde la segunda se guarda el HTML original de cada página.
- Para La Alemana se usa `vitaminas?page=N`, la única forma de paginación que permite su `robots.txt`.
- El documento se compiló de principio a fin en Windows con R 4.6.1. Todavía no se ha probado en un entorno distinto, como Google Colab o GitHub Actions.
