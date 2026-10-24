# Comparador de precios de perfumes · Taller 1

**Asignatura:** Minería de Datos (2016325), Universidad Nacional de Colombia
**Autor:** Steven David Alfonso Galindo
**Lenguaje:** R (≥ 4.2), con Quarto para el documento
**Categoría:** perfumería (solo perfumes)
**Comercios:** La Riviera, Falabella y Farmatodo

El proyecto extrae con *web scraping* toda la categoría de perfumes de los tres
comercios. Guarda copias crudas de cada respuesta y carga los datos en SQLite.
Después estandariza los productos, empareja presentaciones idénticas entre
comercios y responde con SQL y gráficos dónde conviene comprar cada perfume.

## Estructura de archivos

```
taller_1/
├── README.md                      este archivo
├── taller_1.qmd                   documento reproducible (análisis completo)
├── taller_1.html                  documento generado
├── scraper.R                      módulo de extracción: una función por comercio,
│                                  copias crudas, modo sin conexión, registro de
│                                  ejecuciones e inserción en SQLite
├── procesamiento.R                limpieza, exclusiones y correspondencia de productos
├── ejecutar_todo.R                corre todo el flujo con una sola instrucción
├── prueba_otro_entorno.R          repite la recolección desde Colab o GitHub Actions
├── sql/esquema.sql                tablas, llaves, restricciones y vista de la base
├── datos/catalogo_comparables.csv presentaciones comparadas y su regla de correspondencia
├── comparador_precios.sqlite      base de datos (se genera)
├── raw/<comercio>/<fecha>/        respuestas originales comprimidas (.gz) y manifiestos
├── notas/                         reconocimiento de los sitios
├── renv.lock                      versiones fijas de los paquetes
└── taller_1.Rproj                 proyecto de RStudio
```

## Requisitos

- R 4.2 o superior y RStudio (incluye Quarto). Sin RStudio se necesita instalar
  [Quarto](https://quarto.org) aparte.
- Paquetes: `httr2`, `rvest`, `jsonlite`, `dplyr`, `tidyr`, `purrr`, `stringr`,
  `stringi`, `tibble`, `DBI`, `RSQLite`, `ggplot2`, `plotly`, `patchwork`, `gt`,
  `knitr`, `rmarkdown`, `scales`, `forcats`, `quarto` y `renv`.
- Versiones mínimas si no se usa `renv.lock`: dplyr 1.1, purrr 1.0, stringr 1.5,
  tidyr 1.3 y RSQLite 2.3 (trae SQLite 3.44; la carga usa `ON CONFLICT` y
  `UPDATE … FROM`).
- Para instalar las versiones exactas del archivo `renv.lock`:

  ```r
  install.packages("renv")
  renv::restore()   # si pregunta cómo continuar, elija activar el proyecto
  ```

Abra siempre el proyecto con `taller_1.Rproj`: todas las rutas son relativas a esta
carpeta.

## Ejecución rápida (en línea)

En la consola de RStudio:

```r
source("ejecutar_todo.R")
```

Este comando hace lo siguiente:

1. instala los paquetes que falten;
2. recolecta los tres comercios (unos 10 a 15 minutos);
3. procesa la base;
4. escribe `renv.lock`, solo si todavía no existe;
5. genera `taller_1.html`.

Para tener dos ejecuciones en fechas distintas, como recomienda el enunciado, vuelva
a correrlo otro día: cada corrida se agrega a la base sin borrar las anteriores, y el
documento muestra los cambios de precio entre la primera y la última.

## Ejecución paso a paso

```r
source("scraper.R", encoding = "UTF-8")
source("procesamiento.R", encoding = "UTF-8")

resumen <- ejecutar_recoleccion()   # en línea; registra la corrida y carga SQLite
resumen                             # estado por comercio: completa / parcial / fallida

con <- conectar_bd()
procesar_base(con)                  # estandarización, exclusiones y correspondencia
DBI::dbDisconnect(con)
```

Luego abra `taller_1.qmd` y presione **Render**, o use la terminal:

```bash
quarto render taller_1.qmd                                         # usa la base existente
quarto render taller_1.qmd -P recolectar:true                      # recolecta en línea y analiza
quarto render taller_1.qmd -P recolectar:true -P modo:sin_conexion # reprocesa raw/ y analiza
```

## Ejecución sin conexión (desde `raw/`)

Cada corrida guarda las respuestas originales en `raw/<comercio>/<fecha>/`, junto con
un manifiesto `<id_ejecucion>_manifiesto.csv` que registra URL, parámetros, estado
HTTP y hora de cada consulta. El mismo código de extracción puede leer esas copias
sin consultar los sitios:

```r
source("scraper.R", encoding = "UTF-8")
source("procesamiento.R", encoding = "UTF-8")

ejecutar_recoleccion(modo = "sin_conexion")                                   # última corrida
ejecutar_recoleccion(modo = "sin_conexion", id_ejecucion = "20261011T090000") # una en particular
reconstruir_desde_raw("comparador_reconstruido.sqlite")                       # base nueva, todas

con <- conectar_bd("comparador_reconstruido.sqlite"); procesar_base(con); DBI::dbDisconnect(con)
```

Si la observación ya existe en la base, la restricción de unicidad impide
duplicarla. Para generar el documento con una base reconstruida:

```bash
quarto render taller_1.qmd -P ruta_bd:comparador_reconstruido.sqlite
```

## Qué consulta cada función y cómo verifica que la categoría quedó completa

| Comercio | Fuente | Volumen por corrida | Estado "completa" si… |
|---|---|---|---|
| La Riviera | `https://www.lariviera.co/fragancias?page=N` (HTML, selectores `data-hook`) | ~27 consultas (26 páginas con productos y una vacía) | ninguna página falló y el recorrido terminó en una página vacía después de una página incompleta; si la página anterior estaba llena, la vacía se confirma con una segunda consulta |
| Falabella | `.../category/cat1461053/Perfumes?page=N&sortBy=...` (JSON `__NEXT_DATA__`), dos recorridos ordenados por precio | ~126 páginas | todas las páginas de los dos recorridos tienen datos válidos y los productos únicos son al menos el 98 % del total que informa el sitio |
| Farmatodo | `POST https://api-search.farmatodo.com/1/indexes/*/queries` (servicio interno de la página), por subcategoría y rango de precio | ~20 consultas | todas las consultas respondieron bien y cada tramo final tiene menos de 80 resultados (el tope del servicio) |

Una respuesta con estructura inesperada (página de bloqueo, JSON roto, cuerpo vacío)
marca esa página como fallida sin detener la recolección; el estado queda en
*parcial* y la columna `detalle` explica qué faltó.

Entre peticiones hay una pausa de 2 segundos y un *User-Agent* que identifica el
proyecto. La configuración está al inicio de `scraper.R` (objeto `CONFIG`). Los
detalles de cómo se identificó cada fuente están en
`notas/fase1_seleccion_comercios.md`.

Las copias crudas de Falabella pesan unos 260 KB por página comprimida (unos 33 MB
por corrida); ningún archivo se acerca al límite de 100 MB.

## Prueba desde otro entorno (opcional)

La sección 3.1 del enunciado sugiere probar el código fuera del computador personal.
`prueba_otro_entorno.R` repite la recolección con una base y una carpeta de copias
propias y guarda un resumen en `datos/prueba_otro_entorno.csv`. En Google Colab:

1. Entorno de ejecución → Cambiar tipo de entorno de ejecución → R.
2. Suba `taller_1.zip` (esta carpeta comprimida) en el panel Archivos.
3. En una celda: `unzip("taller_1.zip"); setwd("taller_1"); source("prueba_otro_entorno.R")`.
4. Descargue `datos/prueba_otro_entorno.csv`, cópielo en `datos/` de su proyecto y
   vuelva a renderizar: el documento muestra el resultado en la sección "Prueba desde
   otro entorno".

## Problemas frecuentes

- **Un comercio queda "fallida" o "parcial".** La columna `detalle` del resumen (y la
  tabla `ejecuciones`) dice cuántas páginas o consultas fallaron y por qué. Un 429 o
  5xx se reintenta solo. Si el error persiste, vuelva a correr más tarde.
- **El sitio cambió su estructura.** Los selectores y campos están documentados al
  inicio de cada sección de `scraper.R`. Mientras tanto, el modo sin conexión
  permite verificar lo ya recolectado.
- **Menos de 8 presentaciones comparables.** La disponibilidad cambia; en una nueva
  ejecución suelen aparecer. La tabla "Anuncios parecidos que no se emparejaron"
  del documento muestra por qué se descartó cada candidato.
- **La base está abierta en otro programa.** La conexión espera hasta 10 segundos;
  cierre el otro programa si el error continúa.

## Lista de verificación de la entrega

Antes de abrir el Pull Request, revise cada punto (entre paréntesis, dónde se
comprueba):

- [ ] El nombre del autor está en `taller_1.qmd` y en este README.
- [ ] La categoría y su delimitación coinciden con lo registrado en la planilla
      «Categorías de Productos - Estudiantes» (Introducción del documento).
- [ ] Al menos una corrida en línea con los tres comercios en estado *completa*
      (sección "Verificación de la recolección completa"). Si alguno quedó
      *parcial*, el detalle explica la causa.
- [ ] Esa misma corrida tiene al menos 8 presentaciones y 24 observaciones
      comparables (pregunta 2 del documento: "Cumple").
- [ ] Idealmente, una segunda corrida en otra fecha (sección "Cambios de precio
      entre ejecuciones").
- [ ] `comparador_precios.sqlite` está en la carpeta y `raw/` tiene una subcarpeta
      por comercio y fecha, con sus manifiestos.
- [ ] `renv.lock` existe.
- [ ] `taller_1.html` se generó sin errores después de la última corrida.
- [ ] El modo sin conexión funciona: `reconstruir_desde_raw("prueba.sqlite")` termina
      sin errores (puede borrar `prueba.sqlite` después).
- [ ] Opcional: `datos/prueba_otro_entorno.csv` con la prueba desde Colab o GitHub
      Actions.
- [ ] El Pull Request solo agrega archivos dentro de
      `steven_david_alfonso_galindo/taller_01/`, ningún archivo supera 100 MB y no hay
      contraseñas, tokens ni claves.

## Entrega (Pull Request)

Según la guía del curso: fork de `https://github.com/afflorezr/dm_2016325_2026_2`,
clonar el fork en RStudio (File > New Project > Version Control > Git), copiar esta
carpeta en `steven_david_alfonso_galindo/taller_01/` (carpeta personal en la raíz del
repositorio; el nombre de la subcarpeta debe ser el que indique el README del
curso), hacer commit y push desde la pestaña Git y crear el Pull Request hacia
`afflorezr/dm_2016325_2026_2`, rama `main`.

```bash
git add steven_david_alfonso_galindo/taller_01
git commit -m "Entrega taller 01, Steven David Alfonso Galindo"
git push origin main
```

No hay contraseñas, tokens ni claves en el proyecto: el servicio de Farmatodo se
consulta sin clave.
