# Taller 1 — Comparador de precios: papel y desechables

Minería de Datos (2016325) · Universidad Nacional de Colombia
Autor/a: Sergio Martínez Vallejo

Comparador de precios de la categoría **papel y desechables** (papel higiénico, pañuelos, servilletas, toallas de cocina, y vasos, platos y cubiertos desechables) en tres supermercados con venta en línea en Colombia: **Éxito**, **Carulla** y **Olímpica**. Los datos se recolectan con *web scraping*, se almacenan en SQLite y se analizan con consultas SQL y gráficos.

**Lenguaje:** R (todo el proceso: extracción, limpieza, carga en SQLite, consultas y gráficos).

## Estructura de archivos

| Archivo / carpeta | Contenido |
|---|---|
| `taller_1.qmd` | Documento reproducible: fuentes, preparación, base SQLite, consultas SQL, gráficos y conclusiones. |
| `taller_1.html` | Documento renderizado. |
| `scraper.R` | Módulo de extracción: una función por comercio y `ejecutar_recoleccion()`, que ejecuta la recolección completa, registra la ejecución e inserta en SQLite sin sobrescribir. |
| `correspondencia.R` | Productos comparables (identificados por EAN) y correspondencia con el SKU de cada comercio. |
| `consultas_sql.R` | Las consultas SQL de la sección 6 como script independiente. |
| `comparador_precios.sqlite` | Base de datos: tablas `ejecuciones`, `observaciones`, `productos`, `correspondencia` y vista `v_observaciones`. |
| `raw/` | Copias crudas comprimidas de las respuestas, organizadas como `raw/<comercio>/<id_ejecucion>/`. |
| `renv.lock` | Dependencias con versiones fijas. |

## Requisitos

- R ≥ 4.1 y [Quarto](https://quarto.org) (incluido en RStudio reciente).
- Conexión a internet solo para la recolección en línea. No se necesita navegador: los tres comercios (plataforma VTEX) se consultan con peticiones HTTP directas.

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
source("correspondencia.R", encoding = "UTF-8")
```

Cada ejecución recibe un identificador (por ejemplo `20261011T101202`), guarda las copias crudas en `raw/` y agrega las observaciones a la base sin borrar las anteriores. Para revisar las rutas de categoría de una tienda: `listar_categorias_vtex("exito")`.

### Modo sin conexión

Reprocesa las copias de `raw/` sin consultar los sitios:

```r
source("scraper.R", encoding = "UTF-8")
ejecutar_recoleccion(offline = TRUE, id_ejecucion = "20261011T101202")
```

Las observaciones ya existentes no se duplican (restricción de unicidad sobre comercio, SKU y fecha y hora de consulta). Para revisar el resultado sin escribir en la base: `scrape_olimpica("20261011T101202", offline = TRUE)`.

### Documento

Abrir `taller_1.qmd` y presionar **Render**. Por defecto usa los datos ya recolectados (`ejecutar_scraping: false` en el encabezado).

## Ejecución principal y notas

- La ejecución usada en el análisis es **`20261011T101202`** (las tres tiendas en una misma corrida, 13 presentaciones comparables, 39 observaciones). La base conserva también una ejecución de prueba, registrada en la tabla `ejecuciones`.
- Los precios representan el momento de la captura y no son permanentes.
- El repositorio no contiene contraseñas, tokens ni claves de acceso.
