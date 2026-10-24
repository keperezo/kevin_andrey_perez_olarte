# Taller 1 — Comparador de precios: Salud visual y lentes de contacto

**Lenguaje:** R (≥ 4.2). Todo el proceso (extracción, limpieza, carga en SQLite, consultas
y gráficos) está en R.

**Categoría (planilla):** Salud visual y lentes de contacto. Incluye lentes de lectura, lentes
de contacto, estuches y soluciones para limpieza y conservación de lentes. Excluye
medicamentos y lágrimas artificiales.

**Comercios:**

| Comercio | Página | Servicio consultado |
|---|---|---|
| Locatel | https://www.locatelcolombia.com/salud-y-medicamentos-salud-visual | VTEX, colección `productClusterIds:4147` |
| Farmatodo | https://www.farmatodo.com.co/categorias/salud-y-medicamentos/cuidado-de-la-vista | Algolia, `categorie:"Cuidado de la vista"` |
| Droguerías Colsubsidio | https://www.drogueriascolsubsidio.com/salud-visual | VTEX, categoría `C:/13/` |

## Estructura

```
taller_01/
├── taller_1.Rmd                 documento reproducible (llama a scraper.R)
├── scraper.R                    módulo de extracción, limpieza y carga en SQLite
├── prueba.R                     pruebas por comercio (opcional)
├── comparador_precios.sqlite    base de datos
├── raw/<comercio>/<corrida>/    respuestas JSON originales (.json.gz) + totales.json
├── renv.lock                    dependencias con versiones fijas
└── README.md
```

Funciones principales de `scraper.R`:

* `scrape_locatel()`, `scrape_farmatodo()`, `scrape_colsubsidio()` — una por comercio,
  mismas columnas; argumento `offline = TRUE` para leer de `raw/`.
* `ejecutar_recoleccion()` — corrida completa: descarga, guarda `raw/`, clasifica, registra la
  ejecución e inserta en SQLite sin sobrescribir observaciones anteriores; recalcula la
  correspondencia de productos.
* `clasificar_productos()` — aplica la delimitación de la planilla (`en_categoria`,
  `motivo_exclusion`) y estandariza marca, contenido y unidades.
* `actualizar_correspondencia()` — identificador común `SV-<EAN>` para las presentaciones
  presentes en los tres comercios (EAN idéntico + equivalencias manuales documentadas).

## Instalación

```r
install.packages("renv")
renv::restore()        # instala las versiones de renv.lock
```

## Ejecución en línea

Desde la carpeta del taller:

```r
source("scraper.R")
ejecutar_recoleccion()           # nueva corrida: consulta los 3 sitios y carga SQLite
```

o renderizar el documento consultando los sitios:

```r
rmarkdown::render("taller_1.Rmd", params = list(modo = "en_linea"))
```

La corrida tarda unos minutos (pausa de 1,5 s entre peticiones, ~25 peticiones).

## Ejecución sin conexión

Reprocesa las copias de `raw/` sin consultar los sitios:

```r
source("scraper.R")
ejecutar_recoleccion(offline = TRUE)                           # corrida más reciente
ejecutar_recoleccion(offline = TRUE, corrida = "20261011T190300")   # una corrida concreta
rmarkdown::render("taller_1.Rmd")                              # por defecto, modo sin conexión
```

Reprocesar no duplica datos: `observaciones` tiene `UNIQUE(comercio, sku, fecha_hora_consulta)`
y la hora de consulta original se lee del nombre de cada archivo crudo.

## Base de datos

`comercios`, `ejecuciones` (id, comercio, inicio, fin, modo, registros, total del sitio,
anuncios en la categoría, estado), `anuncios` (producto en cada comercio, campos
estandarizados, `en_categoria`, `comparable`, `id_producto`), `productos` (identificador común
y regla de correspondencia), `observaciones` (precio, precio de lista, promoción, URL, texto
original, fecha-hora ISO 8601) y la vista `v_observaciones`.

## Notas

* La clave de Algolia incluida en `scraper.R` es la clave pública de solo búsqueda que
  Farmatodo entrega a cualquier visitante en su JavaScript; no es una credencial privada.
* Colsubsidio responde 500 de forma intermitente; `solicitar()` reintenta hasta 5 veces.
  Si una corrida queda `parcial` en la tabla `ejecuciones`, basta con volver a ejecutar.
