# Taller 1 · Comparador de precios de medicamentos respiratorios

Minería de Datos, Universidad Nacional de Colombia.

Este proyecto compara precios de medicamentos para alergias y síntomas respiratorios (se excluyen antiinfecciosos) en tres farmacias en línea de Colombia: **Farmatodo**, **Cruz Verde** y **La Rebaja**. Además, se consultan otros comercios como respaldo en dado caso que no se puedan cruzar los 8 productos. Los precios se obtienen por scraping de los servicios JSON de cada sitio y se guardan en SQLite. El documento `taller_1.qmd` se encarga de la limpieza, la correspondencia de los 8 productos comparables, las consultas SQL, los gráficos y la interpretación.

- **Lenguaje:** R (versión del lockfile: 4.5.3), con documento reproducible en Quarto.
- **Paquetes:** `httr2`, `jsonlite`, `dplyr`, `stringr`, `stringi`, `purrr`, `tibble`, `DBI`, `RSQLite`, `ggplot2`, `knitr` y `rmarkdown`. Las versiones están fijadas en `renv.lock`.

## Estructura de archivos

```
.
├── README.md                    este archivo
├── taller_1.qmd                 documento reproducible (análisis completo)
├── scraper.R                    módulo de extracción: carga R/ y define ejecutar_pipeline()
├── R/
│   ├── 00_configuracion.R       comercios, servicios, listas negra/blanca, reglas de los 8 productos
│   ├── 01_utilidades_http_raw.R peticiones HTTP, copias crudas y lectura sin conexión
│   ├── 02_estandarizacion_filtro.R  marca, contenido, unidad, subcategoría y filtro de la categoría
│   ├── 03_scrapers.R            scrape_farmatodo(), scrape_cruzverde(), scrape_larebaja() (+ respaldos)
│   ├── 04_comparables.R         correspondencia de los 8 productos
│   ├── 05_viabilidad.R          evaluación previa de los 5 comercios candidatos
│   ├── 06_base_datos.R          esquema SQLite, inserción sin sobrescribir, marcado de comparables
│   └── 07_consultas_taller.R    consultas SQL de la sección 6
├── comparador_precios.sqlite    base de datos
├── raw/                         copias crudas de las respuestas (JSON comprimido)
│   ├── farmatodo/AAAA-MM-DD/    *.json.gz + manifiesto.jsonl
│   ├── cruzverde/AAAA-MM-DD/
│   ├── larebaja/AAAA-MM-DD/
│   └── viabilidad/AAAA-MM-DD/   escaneo previo de comercios (resultado_VIAB_*.rds)
├── sql/consultas_taller.sql     SQL de las consultas, exportado para consulta
└── renv.lock                    dependencias con versiones fijas
```

## Instalación

1. Instalar [R](https://cran.r-project.org/) (versión 4.3 o superior) y [Quarto](https://quarto.org/docs/get-started/) (versión 1.4 o superior). RStudio ya incluye Quarto.
2. Abrir R con el directorio de trabajo en esta carpeta y restaurar los paquetes:

   ```r
   install.packages("renv")
   renv::restore()
   ```

## Ejecución

Todos los comandos se ejecutan en la carpeta del proyecto.

### Generar el documento (sin conexión)

```bash
quarto render taller_1.qmd
```

El documento usa `comparador_precios.sqlite` y las copias de `raw/`, así que **no hace peticiones web**. Si falta la base pero existe `raw/`, la reconstruye en modo sin conexión.

### Recolección en línea

Se hace una nueva ejecución contra los sitios. Tarda unos 4 minutos y agrega observaciones a la base sin borrar las anteriores:

```bash
Rscript scraper.R completo
```

Desde R, el equivalente es:

```r
source("scraper.R", encoding = "UTF-8")
ejecutar_pipeline()
```

También se puede recolectar y generar el documento en un solo paso:

```bash
quarto render taller_1.qmd -P ejecutar_recoleccion:true
```

Cada ejecución guarda las respuestas en `raw/{comercio}/{fecha}/`, registra la corrida en la tabla `ejecuciones` (estado `COMPLETA`, `PARCIAL` o `FALLIDA`), inserta las observaciones y marca los 8 productos comparables.

### Modo sin conexión

Reprocesa las copias de `raw/` sin consultar los sitios. Conserva el identificador de ejecución y la fecha y hora originales, así que reconstruye la misma base:

```bash
Rscript scraper.R offline              # fecha más reciente en raw/
Rscript scraper.R offline 2026-10-10   # una fecha concreta
```

```r
ejecutar_pipeline(offline = TRUE, fecha_offline = "2026-10-10")
```

Si la base ya tiene esas observaciones, la restricción `UNIQUE(comercio, sku_comercio, fecha_hora_consulta)` impide duplicarlas.

### Otras tareas

| Tarea | Comando |
|---|---|
| Evaluar los 5 comercios candidatos (1 página por comercio, unos 1,5 min) | `Rscript scraper.R viabilidad` |
| Prueba rápida: 1 página por comercio, sin escribir en SQLite | `Rscript scraper.R prueba` |
| Borrar los archivos de prueba (`raw/test/` y `comparador_precios_test.sqlite`) | `Rscript scraper.R limpiar` |
| Ejecutar las consultas SQL sobre la base | `Rscript scraper.R consultas` |
| Repetir el escaneo de comercios dentro del documento | `quarto render taller_1.qmd -P ejecutar_viabilidad:true` |

## Fuentes consultadas

| Comercio | Servicio | Acceso |
|---|---|---|
| Farmatodo | Buscador Algolia del sitio (`vcojeyd2po-dsn.algolia.net`, índice `products-colombia`) | Llave pública de solo búsqueda, leída en cada ejecución del JavaScript de `www.farmatodo.com.co` |
| Cruz Verde | `api.cruzverde.com.co/product-service/products/search` | Sesión de invitado (`POST /customer-service/login`) |
| La Rebaja | API pública de catálogo VTEX (`www.larebajavirtual.com/api/catalog_system/pub/products/search`) | Sin autenticación |

El documento detalla en la sección 3.1 cómo se identificó cada servicio, qué campos se extraen y cómo se pagina.

## Notas

- El repositorio no contiene contraseñas, tokens ni claves. La llave de búsqueda de Farmatodo es pública: la publica el propio sitio y el código la obtiene al ejecutarse. Si el sitio cambia, se puede definir la variable de entorno `FARMATODO_ALGOLIA_KEY`.
- Las copias crudas están comprimidas (`.json.gz`) y ningún archivo supera 100 MB.
