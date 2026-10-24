# Taller 1 — Comparador de precios de medicamentos dermatológicos

## Descripción

Este proyecto construye un comparador reproducible de precios para la categoría académica **Medicamentos dermatológicos** a partir de tres comercios con venta en línea en Colombia:

- Droguerías Cafam
- Cruz Verde
- La Rebaja

La extracción se realiza mediante código en R. Las respuestas originales de los sitios se conservan en `raw/`, lo que permite volver a ejecutar el procesamiento sin conexión y sin depender de que las páginas mantengan exactamente la misma estructura después de la entrega.

El análisis final se almacena en una base SQLite y el informe se genera con Quarto.

---

## Estructura del proyecto

```text
taller_01/
├── taller_1.qmd
├── scraper.R
├── comparador_precios.sqlite
├── README.md
├── renv.lock
└── raw/
    ├── cafam/
    │   └── cafam_20261010_092736/
    ├── cruz_verde/
    │   └── cruzverde_20261010_123437/
    └── la_rebaja/
        └── larebaja_20261010_214127/
```

Los nombres concretos de las subcarpetas de `raw/` corresponden a identificadores de ejecución. Una nueva corrida en línea puede crear identificadores adicionales sin reemplazar las capturas históricas.

---

## Archivos principales

### `scraper.R`

Contiene la lógica completa de adquisición y procesamiento:

- funciones de scraping para cada comercio;
- guardado de respuestas crudas;
- modo `online`;
- modo `offline`;
- normalización de variables;
- clasificación dentro de la categoría académica;
- homologación de los ocho productos comparables;
- creación y actualización de SQLite;
- validaciones de cobertura y duplicados.

La función principal es:

```r
ejecutar_recoleccion()
```

### `taller_1.qmd`

Documento de análisis. Lee la base SQLite y genera:

- descripción metodológica;
- trazabilidad de la extracción;
- homologación P01–P08;
- las ocho consultas SQL solicitadas;
- controles de calidad;
- gráficos;
- análisis de ahorro;
- precio por unidad;
- comparación de canasta;
- conclusiones y limitaciones.

### `comparador_precios.sqlite`

Base relacional con las observaciones recolectadas y sus metadatos.

### `raw/`

Copia de las respuestas originales obtenidas durante el scraping. Estas respuestas permiten reprocesar la extracción sin consultar nuevamente los sitios.

### `renv.lock`

Registra las versiones de dependencias de R utilizadas por el proyecto.

---

## Dependencias

El proyecto fue desarrollado en **R** y requiere **Quarto** para renderizar el informe.

La forma recomendada de instalar las dependencias es mediante `renv`.

```r
install.packages("renv")
renv::restore()
```

Entre los paquetes utilizados se encuentran:

```text
DBI
RSQLite
dplyr
tidyr
stringr
purrr
httr2
rvest
jsonlite
readr
tibble
ggplot2
knitr
scales
```

La lista exacta de versiones se encuentra en `renv.lock`.

---

## Reproducción rápida

### 1. Restaurar dependencias

Desde la carpeta `taller_01/`:

```r
renv::restore()
```

### 2. Cargar el módulo de scraping

```r
source("scraper.R")
```

### 3. Reprocesar los datos guardados en `raw/`

La reproducción recomendada para evaluar el proyecto es el modo `offline`, porque utiliza exactamente las respuestas conservadas durante la recolección:

```r
resultado <- ejecutar_recoleccion(
  modo = "offline",
  db_path = "comparador_precios.sqlite",
  raw_root = "raw",
  ejecuciones_fuente = EJECUCIONES_OFFLINE_T1,
  reiniciar_db = TRUE
)
```

Cuando `reiniciar_db = TRUE`, el script crea un respaldo de la SQLite existente antes de reconstruirla.

### 4. Verificar la cobertura mínima

```r
resultado$validacion$cumple_24
```

El resultado esperado para los datos incluidos en la entrega es:

```text
TRUE
```

También puede verificarse la cobertura por producto:

```r
resultado$validacion$cobertura |>
  tibble::as_tibble()
```

Cada uno de los ocho productos debe aparecer en tres comercios y con tres observaciones de precio.

### 5. Renderizar el informe

```r
quarto::quarto_render("taller_1.qmd")
```

También puede utilizarse el botón **Render** de RStudio.

---

## Ejecución en línea

El proyecto también permite realizar una nueva captura de los tres comercios.

```r
source("scraper.R")

resultado_online <- ejecutar_recoleccion(
  modo = "online",
  db_path = "comparador_precios.sqlite",
  raw_root = "raw",
  reiniciar_db = FALSE
)
```

En este modo las tres fuentes se ejecutan desde la función general y reciben un identificador común de ejecución comparativa.

Una nueva ejecución puede producir resultados distintos a los incluidos en la entrega debido a:

- cambios de precio;
- promociones;
- disponibilidad;
- cambios en el catálogo;
- cambios en la estructura de los sitios.

Por esta razón la entrega conserva las capturas originales utilizadas para el análisis.

---

## Fuentes y estrategia de extracción

| Comercio | Tecnología observada | Estrategia |
|---|---|---|
| Droguerías Cafam | VTEX | recorrido de categoría y extracción de información de ficha técnica |
| Cruz Verde | Salesforce Commerce Cloud | paginación mediante `Search-UpdateGrid` y lectura de fichas |
| La Rebaja | VTEX Catalog API | recorrido de ramas del catálogo y unión de resultados |

La extracción no consiste en copiar manualmente precios de páginas individuales. Los precios y metadatos se obtienen mediante las funciones incluidas en `scraper.R`.

---

## Delimitación de la categoría

La categoría académica asignada es **Medicamentos dermatológicos**.

Se incluyeron productos con finalidad medicinal dermatológica, entre ellos:

- antimicóticos tópicos;
- medicamentos para acné;
- tratamientos para dermatitis;
- esteroides o inmunomoduladores dermatológicos;
- medicamentos para verrugas;
- tratamientos medicinales para piel, cabello o uñas;
- aciclovir tópico;
- medicamentos para rosácea;
- otros medicamentos de uso cutáneo compatibles con el alcance.

Se excluyeron productos cuya finalidad principal correspondía a otras categorías, por ejemplo:

- analgésicos o anestésicos tópicos;
- antisépticos para heridas;
- productos para heridas, quemaduras o cicatrización;
- sulfadiazina de plata;
- Fitostimoline;
- productos de silicona para cicatrices;
- repelentes;
- cosméticos, hidratantes o protectores sin finalidad medicinal;
- productos vaginales u óvulos;
- Caladryl Clear.

La clasificación conserva dos niveles de información:

- la categoría original del comercio;
- la decisión reproducible de inclusión o exclusión dentro de la categoría académica.

---

## Productos comparables

Se homologaron ocho presentaciones presentes en los tres comercios:

| ID | Producto comparable |
|---|---|
| P01 | Aciclovir La Santé crema 5 % 20 g |
| P02 | Aciclovir Genfar ungüento 5 % 15 g |
| P03 | Virex ungüento labial 5 % 10 g |
| P04 | Virex ungüento 5 % 15 g |
| P05 | Virex ungüento 5 % 30 g |
| P06 | Clotrimazol AG/Lafrancol crema 1 % 40 g |
| P07 | Clotrimazol La Santé crema 1 % 40 g |
| P08 | Caladryl loción 8 % 100 mL |

La equivalencia se revisó considerando, según disponibilidad:

```text
marca o variante
+ principio activo
+ concentración
+ forma farmacéutica
+ contenido
+ unidades
+ evidencia sanitaria
```

No se considera suficiente una coincidencia aproximada del nombre.

Las diferencias entre revisiones históricas del registro INVIMA se conservan como información de la fuente y no se utilizan automáticamente para concluir que dos presentaciones son distintas.

---

## Base SQLite

La base utiliza cinco tablas principales:

### `comercios`

Identifica las cadenas utilizadas.

### `productos_comunes`

Define P01–P08 y la identidad utilizada para comparar entre comercios.

### `ejecuciones`

Registra información de cada corrida:

- identificador;
- comercio;
- ejecución fuente;
- inicio;
- fin;
- modo;
- cantidad obtenida;
- cantidad dentro de la categoría;
- precios obtenidos;
- estado;
- carpeta `raw`.

### `anuncios`

Conserva la ficha del producto dentro de cada comercio. Incluye, entre otros:

- identificador interno;
- SKU/EAN cuando existen;
- producto original;
- marca;
- laboratorio;
- presentación;
- principio activo;
- concentración;
- forma;
- contenido y unidad;
- INVIMA;
- URL;
- descripción;
- texto original;
- categoría;
- indicador de comparabilidad.

### `observaciones`

Conserva los valores que pueden cambiar entre ejecuciones:

- precio actual;
- precio de lista;
- precio anterior;
- promoción;
- precio por unidad informado por la fuente;
- fecha y hora de consulta.

La separación entre `anuncios` y `observaciones` evita repetir toda la ficha técnica cuando se incorporen nuevas observaciones de precio.

---

## Integridad y trazabilidad

La base utiliza restricciones de unicidad para evitar insertar dos veces la misma observación del mismo producto, comercio y fecha de consulta.

Las fechas se almacenan en formato ISO 8601 para Colombia, por ejemplo:

```text
2026-10-10T14:27:56-05:00
```

No se fabrican precios cuando el sitio no proporciona un valor válido.

Los campos originales se conservan además de los campos estandarizados, de modo que las transformaciones puedan auditarse.

---

## Cobertura de la ejecución incluida

En la ejecución incluida en la entrega se obtuvieron:

| Comercio | Registros recolectados | Registros dentro de la categoría académica | Comparables |
|---|---:|---:|---:|
| Droguerías Cafam | 42 | 30 | 8 |
| Cruz Verde | 149 | 113 | 8 |
| La Rebaja | 2621 | 286 | 8 |

Los ocho productos comparables tienen una observación de precio en cada comercio:

```text
8 productos × 3 comercios = 24 observaciones comparables
```

La validación final no detectó duplicados dentro del conjunto comparable.

---

## Particularidades de La Rebaja

La taxonomía de La Rebaja es distinta a la de Cafam y Cruz Verde. Una parte importante de los productos de interés está dentro de ramas comerciales más amplias, por lo que la extracción recupera más registros antes de aplicar la clasificación académica.

Por esta razón:

- `registros_obtenidos` no equivale a `productos_categoria`;
- no se interpreta que los 2621 registros recolectados sean todos medicamentos dermatológicos;
- el análisis final utiliza únicamente los registros clasificados reproduciblemente dentro del alcance.

En el reprocesamiento histórico puede aparecer `NA` en `productos_reportados_sitio` para La Rebaja. Ese valor no se imputa manualmente porque no existe un único total agregado guardado como respuesta directa equivalente al de los otros dos comercios.

---

## Preguntas SQL incluidas en el informe

El `.qmd` responde las siguientes preguntas a partir de SQLite:

1. cantidad de comercios, productos/presentaciones y observaciones;
2. validación de las 24 observaciones comparables y búsqueda de duplicados;
3. precio mínimo, máximo, promedio y comercio con menor precio;
4. ahorro en COP y porcentaje;
5. precio por unidad para tamaños comparables;
6. ranking de mayores diferencias de precio;
7. datos faltantes e inconsistencias;
8. tamaño de la oferta y cobertura de atributos por comercio.

Los empates en el menor precio se conservan mediante la consulta SQL en lugar de seleccionar arbitrariamente un único comercio.

---

## Precio por unidad

El precio por unidad solo se calcula cuando existen presentaciones realmente comparables.

En el informe se utiliza el caso de:

```text
Virex ungüento 5 % — 15 g y 30 g
```

No se mezcla con Virex labial de 10 g porque corresponde a una variante comercial distinta.

El cálculo del costo por gramo es:

```text
precio_cop / contenido_g
```

Este análisis es económico y no debe interpretarse como recomendación terapéutica.

---

## Ahorro

Para cada presentación comparable se calcula:

```text
ahorro_cop = precio_maximo - precio_minimo
```

y:

```text
ahorro_porcentaje =
100 * (precio_maximo - precio_minimo) / precio_maximo
```

El denominador corresponde al mayor precio observado para esa presentación.

---

## Reprocesamiento offline

El modo `offline` es importante porque separa dos tareas:

1. **adquisición:** descargar y guardar la respuesta original;
2. **procesamiento:** convertir la respuesta en una tabla estructurada.

Esto permite corregir parsers, reglas de limpieza o clasificación sin volver a consultar el sitio y sin modificar el contenido original guardado.

---

## Notas sobre los resultados

Los resultados representan las condiciones observadas durante las ejecuciones almacenadas.

No deben generalizarse como:

- precios permanentes;
- disponibilidad nacional;
- recomendación médica;
- sustitución entre medicamentos;
- evaluación completa del surtido de una cadena.

El análisis de canasta se restringe a las ocho presentaciones comunes y no incorpora costos de envío, descuentos condicionados, membresías ni beneficios externos al precio observado.

---

## Comprobación rápida de la SQLite

Para verificar las tablas:

```r
library(DBI)
library(RSQLite)

con <- dbConnect(
  SQLite(),
  "comparador_precios.sqlite"
)

dbListTables(con)

dbGetQuery(
  con,
  "
  SELECT
      c.nombre AS comercio,
      COUNT(*) AS observaciones
  FROM observaciones o
  INNER JOIN comercios c
      ON o.comercio_id = c.comercio_id
  GROUP BY c.nombre;
  "
)

dbDisconnect(con)
```

---

## Render final

Antes de entregar se recomienda probar desde una sesión limpia:

```r
renv::restore()
quarto::quarto_render("taller_1.qmd")
```

El informe debe renderizar utilizando únicamente los archivos incluidos dentro de `taller_01/`.

---

## Recomendación para una nueva corrida

Si se desea añadir una segunda observación temporal, no debe eliminarse la SQLite existente.

```r
source("scraper.R")

nueva_corrida <- ejecutar_recoleccion(
  modo = "online",
  db_path = "comparador_precios.sqlite",
  raw_root = "raw",
  reiniciar_db = FALSE
)
```

De esta forma las observaciones históricas permanecen disponibles y pueden compararse con la nueva ejecución.
