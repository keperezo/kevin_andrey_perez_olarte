# Taller 1 — Comparador de precios: salud digestiva

Minería de Datos (2016325) · Universidad Nacional de Colombia
Autor: Andres Camilo Vargas Ramos

Comparador de precios de la categoría **salud digestiva** en tres droguerías con venta en línea en Colombia: **Colsubsidio**, **Farmatodo** y **Cruz Verde**. Los datos se recolectan con *web scraping*, se almacenan en SQLite y se analizan con consultas SQL y gráficos.

**Lenguaje:** R (todo el proceso: extracción, limpieza, carga en SQLite, consultas y gráficos).

## Estructura de archivos

| Archivo / carpeta | Contenido |
|---|---|
| `taller_1.qmd` | Documento reproducible: fuentes, preparación, base SQLite, consultas SQL, gráficos y conclusiones. |
| `taller_1.html` | Documento renderizado. |
| `scraper.R` | Módulo de extracción: una función por comercio y `ejecutar_recoleccion()`, que ejecuta la recolección completa, registra la ejecución e inserta en SQLite sin sobrescribir. |
| `correspondencia.R` | Productos comparables (identificador común) y correspondencia con el SKU de cada comercio. |
| `comparador_precios.sqlite` | Base de datos: tablas `ejecuciones`, `observaciones`, `productos`, `correspondencia` y vista `v_observaciones`. |
| `raw/` | Copias crudas comprimidas de las respuestas, organizadas como `raw/<comercio>/<id_ejecucion>/`. |
| `renv.lock` | Dependencias con versiones fijas. |
| `consultas_sql.R` | Las 8 consultas SQL de la sección 6 como script independiente (las mismas que muestra el documento). |

## Requisitos

- R ≥ 4.1 y [Quarto](https://quarto.org) (incluido en RStudio reciente).
- **Google Chrome** o Chromium instalado: Farmatodo y Cruz Verde se recolectan con un navegador controlado (`rvest::read_html_live()` + `chromote`). Colsubsidio usa peticiones HTTP directas y no necesita Chrome.
- Conexión a internet solo para la recolección en línea.

## Instalación

Abrir la carpeta como proyecto de RStudio (o fijar el directorio de trabajo en ella) y restaurar las dependencias:

```r
install.packages("renv")
renv::restore()
```

## Ejecución

### Recolección en línea

```r
source("scraper.R", encoding = "UTF-8")
resumen <- ejecutar_recoleccion()   # las tres tiendas en una misma ejecución
resumen
```

Cada ejecución recibe un identificador (por ejemplo `20261008T184604`), guarda las copias crudas en `raw/` y agrega las observaciones a la base sin borrar las anteriores. Para una sola tienda: `ejecutar_recoleccion(comercios = "Colsubsidio")`.

Luego se actualiza la correspondencia de los productos comparables:

```r
source("correspondencia.R", encoding = "UTF-8")
```

### Modo sin conexión

Reprocesa las copias de `raw/` sin consultar los sitios:

```r
source("scraper.R", encoding = "UTF-8")
ejecutar_recoleccion(offline = TRUE, id_ejecucion = "20261008T184604")
```

Las observaciones ya existentes no se duplican (restricción de unicidad sobre comercio, SKU y fecha y hora de consulta). Para revisar el resultado sin escribir en la base:

```r
scrape_cruzverde("20261008T184604", offline = TRUE)
```

### Documento

Abrir `taller_1.qmd` y presionar **Render**, o desde la consola:

```r
quarto::quarto_render("taller_1.qmd")
```

Por defecto el documento usa los datos ya recolectados (`ejecutar_scraping: false` en el encabezado). Para recolectar de nuevo al renderizar, cambiar ese parámetro a `true`.

## Ejecución principal y notas

- La ejecución usada en el análisis es **`20261008T184604`** (las tres tiendas en una misma corrida, 36 observaciones comparables). La base conserva también ejecuciones de prueba, registradas en la tabla `ejecuciones`.
- Los precios representan el momento de la captura y no son permanentes.
- Si un sitio cambia su estructura o bloquea el acceso, la extracción puede verificarse con el modo sin conexión a partir de `raw/`.
- El repositorio no contiene contraseñas, tokens ni claves de acceso.
