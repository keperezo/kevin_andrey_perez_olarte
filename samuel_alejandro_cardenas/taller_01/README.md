# Taller 1 — Minería de Datos (R)

**Categoría:** preservativos, lubricantes sexuales, pruebas de embarazo y
pruebas de ovulación; sin medicamentos anticonceptivos. Comercios:
Cafam, Locatel y Cruz Verde.


## Archivos necesarios

| Archivo o carpeta | Para qué sirve |
| --- | --- |
| `scraper.R` | Extracción de tres comercios, limpieza, guardado en SQLite, captura en vivo y validación de ocho productos |
| `taller_1.qmd` | Análisis reproducible, ocho consultas SQL, gráficos y conclusiones |
| `comparador_precios.sqlite` | Observaciones y ejecuciones reales, incluidas las anteriores |
| `config/equivalencias_revisar.csv` | Correspondencias por SKU para ocho presentaciones |
| `raw/` | Respuestas JSON/HTML originales, por tienda y ejecución |
| `historico/` | Material original adicional para revisión y pruebas |
| `pruebas/` | Prueba sin conexión, en una base temporal |
| `renv.lock` | Versiones de dependencias; debe generarse en el RStudio del estudiante |


## 1. Instalar paquetes

```r
paquetes <- c('jsonlite','rvest','xml2','httr2','DBI','RSQLite',
              'ggplot2','knitr','rmarkdown','quarto','renv')
faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes)) install.packages(faltantes)
```

También se necesita **Quarto CLI** instalado para generar el HTML.

## 2. Probar el proyecto sin internet

```r
source('pruebas/probar_sin_internet.R')
```

Utiliza las copias históricas y una base temporal, sin modificar
`comparador_precios.sqlite`. Comprueba los parsers y la lectura de SQLite.

## 3. Nueva extracción en vivo (solo si se desea otra captura)

```r
source('scraper.R')
extraer_en_vivo()
```

Esta función respalda SQLite en `respaldos/`, ejecuta la recolección de
Cafam, Locatel y Cruz Verde, conserva las copias crudas y muestra tres tablas:
ejecuciones, cobertura y candidatos. Una ruta `ruta_recorrida` **no garantiza**
por sí sola cobertura exhaustiva de todas las secciones de la categoría.


## 4. Validar las ocho presentaciones de una nueva ejecución

```r
source('scraper.R')
validar_ultima_ejecucion()
```

La función crea una copia de seguridad, comprueba las ocho ternas dentro de
la última ejecución y solo entonces marca `comparable=1` en SQLite. Si una
comprobación falla, la transacción se revierte. 

## 5. Generar el informe HTML

```r
quarto::quarto_render('taller_1.qmd')
```

Quarto utiliza los datos SQLite de la ejecución comparable más reciente,
sin volver a acceder a las tiendas.


## Limitaciones

Los precios se capturaron en una fecha especifica, para un contexto de
ubicación y promoción específico. Las observaciones comparables proceden de
una misma ejecución, pero la cobertura de rutas no certifica por sí sola que
se incluyeron todas las posibles subcategorías comerciales. Los precios Club
se guardan aparte de los precios normales. El scraper puede requerir ajustes
si las tiendas modifican las páginas o sus servicios internos.

Las respuestas originales de `raw/` y las reglas del archivo de equivalencias
permiten verificar el origen de las observaciones sin modificar los precios
históricos.
