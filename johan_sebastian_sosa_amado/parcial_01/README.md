# Taller 1 · Comparador de precios: incontinencia y cuidado del adulto

- **Estudiante:** Johan Sebastian Sosa Amado
- **Taller:** 01 · Minería de Datos (2016325), Universidad Nacional de Colombia, sede Bogotá
- **Lenguaje:** Python en todo el proceso (extracción, limpieza, SQLite, consultas y gráficos). El documento está en Quarto.

## Descripción

Comparador de precios de la categoría *Incontinencia y cuidado del adulto* en tres tiendas en línea colombianas: Éxito, La Rebaja y Olímpica.

1. `scraper.py` descarga la sección completa de cada tienda desde el servicio JSON que usa su propia página (plataforma VTEX). Guarda cada respuesta comprimida en `raw/` y carga los datos en `comparador_precios.sqlite`.
2. La limpieza estandariza marcas, contenido (unidades, mL, g) y tallas, y empareja por código de barras (EAN) los productos que venden las tres tiendas con la misma presentación. Resultado: 13 productos comparables.
3. `taller_1.qmd` responde las 8 preguntas del enunciado con SQL, presenta 3 gráficos y da conclusiones para el consumidor.

La base tiene dos recolecciones completas de las tres tiendas: 9 de octubre de 2026 a las 05:01 y 10 de octubre de 2026 a las 04:22, hora de Colombia.

## Archivos

| Archivo | Contenido |
|---|---|
| `taller_1.qmd` | Documento reproducible |
| `taller_1.html` | Documento renderizado, autocontenido (se abre sin internet) |
| `scraper.py` | Módulo de extracción, limpieza y carga en SQLite |
| `consultas.py` | Auxiliar: imprime en la terminal las 8 consultas del documento |
| `comparador_precios.sqlite` | Base de datos |
| `raw/` | Copias crudas de las respuestas, comprimidas: `raw/<tienda>/<fecha-hora>/*.json.gz`, más el `robots.txt` leído en esa corrida |
| `requirements.txt` | Dependencias de Python con versiones fijas |
| `.gitignore` | Archivos que no se suben (entorno virtual, cachés, salidas intermedias) |

La carpeta `raw/exito/2026-10-09T04-29-40/` es una prueba inicial de Éxito, anterior a la primera recolección. No está cargada en la base.

## Base de datos

| Tabla | Una fila por |
|---|---|
| `comercios` | Tienda |
| `ejecuciones` | Tienda en cada corrida; `recoleccion_id` agrupa las tres tiendas de una misma corrida |
| `anuncios` | Producto tal como aparece en una tienda (SKU), con su texto original, los campos limpios y el indicador `comparable` (1 o 0) |
| `observaciones` | Precio de un anuncio en una ejecución, con fecha y hora de consulta; única por anuncio y fecha-hora |
| `productos` | Producto comparable (P01 a P13), con su EAN; `anuncios.producto_id` lo enlaza con cada tienda |

Los precios que la tienda no muestra (producto agotado) quedan como `NULL`; no se imputan.

## Requisitos

- Python 3.13 (probado con 3.13.16 en Windows y Linux).
- Quarto, solo para renderizar el documento (probado con 1.6.40 y 1.10.19).

Instalación, desde esta carpeta:

```powershell
# Windows (PowerShell)
python -m venv .venv
.venv\Scripts\Activate.ps1
pip install -r requirements.txt
```

```bash
# macOS o Linux
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Ejecución

Todos los comandos se corren desde esta carpeta, con el entorno virtual activo. Las rutas son relativas.

**En línea** (descarga las tres tiendas):

```bash
python scraper.py
```

Descarga la categoría completa, guarda las respuestas en `raw/` y agrega una recolección nueva a la base sin borrar las anteriores. Al final repite la limpieza y el emparejamiento. Para una sola tienda: `python scraper.py exito` (o `larebaja`, `olimpica`). Hace una pausa de 2 segundos entre páginas y revisa el `robots.txt` de cada tienda antes de descargar.

**Sin conexión** (reprocesa las copias crudas):

```bash
python scraper.py --sin-conexion
```

Lee la copia más reciente de `raw/` de cada tienda y no se conecta a internet. Si esa copia ya está en la base, la base no cambia: las observaciones no se duplican.

Para reprocesar la copia de un día, se agrega la fecha:

```bash
python scraper.py --sin-conexion 2026-10-09
```

En cada tienda toma la última copia de ese día. La base también se puede reconstruir solo desde `raw/`: si se mueve o se borra `comparador_precios.sqlite`, correr `python scraper.py --sin-conexion 2026-10-09` y luego `python scraper.py --sin-conexion` da los mismos anuncios, observaciones y productos que la base entregada.

**Otras opciones:**

```bash
python scraper.py --limpiar     # solo la limpieza y el emparejamiento, sobre la base actual
python consultas.py             # las 8 consultas en la terminal
quarto render taller_1.qmd      # el documento
```

`quarto render` no descarga nada. Reprocesa sin conexión la copia más reciente de `raw/`, repite la limpieza y arma consultas, gráficos y textos con los datos de la base. Si Quarto no encuentra el Python del entorno virtual, indícalo antes de renderizar:

```powershell
$env:QUARTO_PYTHON = (Get-Command python).Source   # Windows (PowerShell)
```

```bash
export QUARTO_PYTHON="$(which python)"             # macOS o Linux
```

## Limitaciones

- Los precios son los que mostraba cada tienda en el momento de la consulta, con las promociones de ese día.
- El texto original de cada anuncio se guarda la primera vez que aparece (`INSERT OR IGNORE`). Si la tienda lo cambia después, el texto nuevo queda en las copias de `raw/`, pero no en la base.
