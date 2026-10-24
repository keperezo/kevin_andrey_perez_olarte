# Taller 1 · Minería de Datos (2016325) — Comparador de precios: Cuidado y decoración de uñas

Universidad Nacional de Colombia · Entrega individual
**Autora:** Maria Camila Herrera Cortés

**Lenguaje elegido: R.** Todo el proceso (extracción, limpieza, carga en SQLite, consultas y gráficos) se hace en R.

## Descripción

Comparador de precios construido con *web scraping* de tres comercios en línea de Colombia (**Éxito, Jumbo y Olímpica**) para la categoría *Cuidado y decoración de uñas* (esmaltes, removedores, endurecedores y tratamientos cosméticos, uñas postizas, limas y cortaúñas). Los tres sitios usan VTEX; se consulta la API pública de catálogo que la propia página usa:

`/api/catalog_system/pub/products/search?fq=C:<ruta de categoría>/&_from=<n>&_to=<n+49>`

Se comparan 8 productos principales (más 2 de reserva) presentes en los tres comercios, emparejados por EAN y verificados manualmente.

## Estructura de archivos

| Archivo / carpeta | Contenido |
|---|---|
| `taller_1.qmd` | Documento reproducible (Quarto): scraping, limpieza, SQLite, consultas SQL, gráficos e interpretación. Llama a los módulos; no duplica su código. |
| `scraper.R` | Módulo de extracción: una función por comercio (`scrape_exito()`, `scrape_jumbo()`, `scrape_olimpica()`), recolección completa (`recolectar_todo()`), modo sin conexión (`reprocesar_raw()`) y función general (`ejecutar_y_guardar()`). |
| `limpieza_sqlite.R` | Filtro de la categoría, estandarización, correspondencia de productos, esquema SQLite y carga sin sobrescribir. |
| `consultas_sql.R` | Las 8 preguntas del taller en SQL (lista `CONSULTAS`). |
| `comparador_precios.sqlite` | Base de datos con todas las ejecuciones. |
| `raw/` | Copias crudas (`.json.gz`) de las respuestas originales, por comercio y fecha. |
| `brutos_run_*.rds` | Resumen de cada ejecución (total que reporta el sitio vs. productos obtenidos), usado en la sección de completitud. |
| `renv.lock` | Versiones fijas de las dependencias de R. |
| `dependencias.R`, `generar_renv.R` | Auxiliares para declarar y fijar las dependencias. |
| `README.md` | Este archivo. |

## Requisitos

- R ≥ 4.1 (se usa el operador `|>`).
- [Quarto](https://quarto.org/docs/get-started/) para renderizar el documento.
- Paquetes de R con versiones fijas en `renv.lock`.

## Instalación de dependencias

Abrir el proyecto en la carpeta de la entrega y ejecutar en R:

```r
install.packages("renv")   # solo si no está instalado
renv::restore()            # instala las versiones de renv.lock
```

## Ejecución en línea (nueva recolección)

Desde la carpeta del proyecto, en R:

```r
source("scraper.R")
ejecutar_y_guardar()       # scrapea los 3 comercios, guarda raw/, limpia y agrega a comparador_precios.sqlite
```

- Cada corrida genera un identificador `run_AAAAMMDD_HHMMSS`.
- Las observaciones nuevas **se agregan sin sobrescribir** las anteriores (restricción de unicidad por anuncio y fecha-hora de consulta).
- Las respuestas originales se guardan en `raw/<comercio>/<AAAA-MM-DD>/`.
- La recolección tarda varios minutos (pausa de 1 s entre páginas y reintentos ante errores transitorios).

Alternativa: en `taller_1.qmd` poner `RUN_SCRAPING <- TRUE` en el primer bloque y renderizar.

## Ejecución sin conexión (desde las copias crudas)

Reprocesa `raw/` sin consultar los sitios. Por defecto toma la ejecución más reciente; para no tocar la base oficial se recomienda cargar a una base temporal:

```r
source("scraper.R")
ejecutar_y_guardar(db = "prueba_offline.sqlite", sin_conexion = TRUE)
# o una ejecución específica:
ejecutar_y_guardar(db = "prueba_offline.sqlite", sin_conexion = TRUE, ejecucion = "run_20261008_234108")
```

Alternativa: en `taller_1.qmd` poner `RUN_OFFLINE <- TRUE`.

## Renderizar el documento

```bash
quarto render taller_1.qmd
```

Por defecto el documento **no vuelve a scrapear**: usa la base `comparador_precios.sqlite` ya cargada. Genera `taller_1.html` autocontenido.

## Prueba en otro entorno
Probado el 9 de octubre de 2026 en Google Colab con R (versión: [R.version.string]):
- Modo sin conexión: funciona. Reprocesó las copias crudas de `raw/` sin consultar los sitios
  e insertó 6.523 observaciones en una base nueva (`prueba_offline.sqlite`).
- Modo en línea: funciona en Jumbo y Olímpica (2.566 observaciones, 1.377 y 1.189 respectivamente,
  en una base nueva `prueba_online.sqlite`). Éxito no devolvió datos: el scraper no pudo leer
  el total de productos de sus dos categorías, así que la petición desde Colab no tuvo respuesta
  válida del sitio. Para Éxito la extracción se verifica con las copias crudas (modo sin conexión).

## Notas
- Los precios son los observados en la fecha y hora de cada captura (ISO 8601, hora de Colombia); no son permanentes.
- Precios ausentes se guardan como `NULL`, nunca como cero.
- Los anuncios fuera de la categoría no se borran: quedan en `anuncios` con `incluido = 0` y su `motivo_exclusion`.
- El texto original del anuncio (nombre, descripción, ficha técnica) se conserva sin modificar; las versiones estandarizadas están en campos aparte.
- No se incluyen contraseñas, tokens ni claves; ningún archivo supera 100 MB.
