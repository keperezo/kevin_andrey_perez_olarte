# Comparador de precios: higiene y cuidado del bebé

**Taller 1 – Minería de Datos (2016325) · Universidad Nacional de Colombia**
Autor: Juan Pablo Sosa Camargo · Lenguaje: R

## Qué hace el proyecto

Compara precios de productos de **higiene y cuidado del bebé** (jabones, champús, lociones, aceites, cremas hidratantes y colonias infantiles) entre tres comercios: **Éxito, Carulla y Jumbo**. El proyecto:

1. **Recolecta** los productos de cada sitio (scraping) y guarda una copia cruda de cada página.
2. **Limpia y estandariza** precios, marcas, subcategorías, contenido y unidades.
3. **Empareja** la misma presentación entre comercios y la registra como producto comparable.
4. **Guarda** todo en una base SQLite, sin sobrescribir ejecuciones anteriores.
5. **Analiza** en un documento Quarto: precio del empaque, precio por 100 mL/g, ahorro potencial y canasta de presentaciones idénticas.

El objetivo es indicar dónde conviene comprar cada producto y cuándo una presentación más grande reduce el precio por unidad, distinguiendo el *ahorro unitario* del *desembolso total*.

## Estructura de la carpeta

```
.
├── Scraper.R                  # Módulo principal: recolección, limpieza, comparables y registro en SQLite
├── Orquestador.R              # Script que encadena las ejecuciones del módulo
├── P1Mineria.qmd              # Documento Quarto: consultas SQL, gráficos e interpretación
├── P1Mineria.pdf              # Documento final renderizado
├── comparador_precios.sqlite  # Base de datos SQLite con todas las ejecuciones
├── raw/                       # Copias crudas de las páginas consultadas
│   └── <comercio>/<ejecución>/   # páginas .html.gz + indice.csv
├── renv/                      # Entorno reproducible de R (gestionado por renv)
├── renv.lock                  # Versiones fijas de los paquetes
└── README.md                  # Este archivo
```

| Elemento | Para qué sirve |
|---|---|
| `Scraper.R` | Contiene toda la lógica: configuración de los comercios (`COMERCIOS`), scraping, estandarización (`estandarizar()`), exclusiones (`motivo_exclusion()`), emparejamiento (`proponer_por_niveles()`, `fijar_comparables_linea()`) y registro en la base. Se usa con `ejecutar_recoleccion()` y `ejecutar_comparables()`. |
| `Orquestador.R` | Punto de entrada para correr el proceso de principio a fin sin escribir las llamadas a mano. |
| `P1Mineria.qmd` | Solo presenta resultados: consulta la base, grafica e interpreta. No duplica la lógica del scraper. |
| `P1Mineria.pdf` | Salida del `.qmd` (requiere LaTeX para generarse). |
| `comparador_precios.sqlite` | Base de datos con las observaciones de cada ejecución. |
| `raw/` | Evidencia de la extracción: cada página consultada, comprimida, organizada por comercio y ejecución, con un `indice.csv`. Permite reprocesar sin conexión. |
| `renv/`, `renv.lock` | Dependencias con versiones fijas para reproducir el entorno. |

## Base de datos (SQLite)

- **`ejecuciones`**: una fila por corrida y comercio (identificador, fecha y hora de inicio en ISO 8601, número de registros, estado `completa/parcial/fallida`, modo).
- **`productos`**: identificador común (`producto_id`) de los productos comparables.
- **`observaciones`**: un precio por producto, comercio y momento de consulta. Conserva el texto original del anuncio, URL, SKU, precio anterior, indicador de promoción, contenido, unidad, `precio_100` (COP por 100 mL/g), `comparable` y `ejecucion_id`.

Reglas importantes:

- Restricción de unicidad sobre `(comercio, clave_producto, fecha_hora_consulta)`: una misma observación no se inserta dos veces.
- Las ejecuciones nuevas **se agregan**; nunca sobrescriben las anteriores.
- Un precio no disponible queda en `NULL`, nunca en 0.
- El texto original del anuncio se conserva tal cual; las versiones estandarizadas viven en columnas propias (`marca_std`, `subcategoria_std`, `variante_std`).

## Requisitos

- **R** y los paquetes fijados en `renv.lock`.
- **Google Chrome** instalado, solo para el modo en línea (el scraping usa `chromote`).
- **Quarto** y **TinyTeX/LaTeX** para generar el PDF.

Instalar dependencias:

```r
renv::restore()
```

## Cómo usarlo

### 1. Recolección y comparables (en línea)

```r
source("Scraper.R")
ejecutar_recoleccion(modo = "online", db = "comparador_precios.sqlite")
ejecutar_comparables(db = "comparador_precios.sqlite", n_obj = 12, registrar = TRUE)
```

Cada corrida consulta los sitios, guarda las páginas en `raw/` y registra las observaciones en la base.

### 2. Reprocesar sin conexión (desde `raw/`)

```r
source("Scraper.R")
ejecutar_recoleccion(modo = "offline", db = "comparador_precios.sqlite")
ejecutar_comparables(db = "comparador_precios.sqlite", n_obj = 12, registrar = TRUE)
```

No consulta ningún sitio ni requiere Chrome. Sirve para verificar la extracción si un sitio cambia su estructura o bloquea el acceso.

### 3. Generar el documento

```bash
quarto render P1Mineria.qmd
```

Por defecto el documento se genera sobre los datos ya almacenados. Para volver a correr la recolección al renderizar, poner `EJECUTAR <- TRUE` en el chunk `setup` del `.qmd`.

## Notas y limitaciones

- Los precios corresponden al **momento de captura**; no son permanentes. Con una sola ejecución no se observan cambios de precio, por eso conviene repetir el scraping en fechas distintas.
- En Jumbo se compara el **precio público**, no el precio con tarjeta Jumbo Más.
- Los vendedores externos (marketplace) se excluyen del emparejamiento.
- Los precios no incluyen envío, montos mínimos ni descuentos que dependan de tarjetas o promociones temporales.
- La correspondencia entre comercios usa reglas documentadas (marca, tipo, unidad, similitud de nombre, rango de tamaño); los niveles B–D tienen menor certeza que la presentación idéntica (nivel A).
- Solo se consultan páginas públicas, con pausas aleatorias entre solicitudes, sin autenticación y sin evadir controles de acceso.
