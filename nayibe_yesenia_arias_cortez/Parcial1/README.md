# Taller 1 — Comparador de precios: cuidado capilar

Minería de Datos (2016325), Universidad Nacional de Colombia — Nayibe Yesenia Arias Cortez.

* **Lenguaje:** Python 3.12 (extracción, limpieza, SQLite, consultas SQL y gráficos).
* **Categoría:** cuidado capilar (delimitación en `docs/delimitacion_categoria.md`).
* **Comercios:** La Rebaja, Locatel y Droguerías Cafam (fuentes, robots.txt y términos de uso en
  `docs/fuentes_scraping.md`).

## Estructura de archivos

| Archivo | Contenido |
|---|---|
| `taller_1.qmd` | Documento reproducible (Quarto): fuentes, preparación, base, 8 consultas SQL, gráficos y conclusiones. Llama a `scraper.py` y `comparables.py`; no duplica su código. |
| `scraper.py` | Módulo de extracción: `scrape_larebaja()`, `scrape_locatel()`, `scrape_cafam()` (mismas columnas) y `run_all(offline=False)`, que registra la ejecución e inserta en SQLite sin sobrescribir. |
| `comparables.py` | Correspondencia de productos comparables (mismo EAN en los tres comercios, misma ejecución, precio disponible, mismo contenido); marca `comparable = 1` e `id_producto_comun` en la base. |
| `comparador_precios.sqlite` | Base de datos: `comercios`, `anuncios`, `observaciones`, `ejecuciones`, `productos`, `correspondencia`, `lineas` y la vista `v_observaciones`. |
| `raw/<comercio>/<fecha>/<id_ejecucion>/` | Copias crudas comprimidas (`.json.gz` / `.html.gz`) de cada respuesta y un `manifiesto.json` con URL, fecha y hora y totales del sitio. |
| `requirements.txt` | Dependencias con versiones fijas (incluye Jupyter, Matplotlib y Quarto vía `quarto-cli`). |
| `docs/delimitacion_categoria.md` | Qué productos se incluyen y excluyen, y por qué. |
| `docs/fuentes_scraping.md` | Endpoints, cómo se identificaron, robots.txt, términos de uso y comercios descartados. |

## Instalación

```bash
python3.12 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

En Windows, reemplazar `.venv/bin/` por `.venv\Scripts\`.

## Ejecución sin conexión (reprocesa `raw/`, no hace ninguna petición)

```bash
.venv/bin/python scraper.py --offline          # reprocesa la última ejecución guardada en raw/
.venv/bin/python scraper.py --offline --id 20261009T123410   # o una ejecución específica
.venv/bin/python comparables.py                # marca los comparables y los muestra lado a lado
```

Para reconstruir la base completa desde cero, borrar `comparador_precios.sqlite` y reprocesar cada ejecución de
`raw/` con `--offline --id <id_ejecucion>`.

## Ejecución en línea (nueva recolección)

```bash
.venv/bin/python scraper.py        # ≈ 45 peticiones, ≈ 2 minutos; crea una ejecución nueva
.venv/bin/python comparables.py
```

Cada corrida crea un `id_ejecucion` nuevo, guarda sus copias crudas en `raw/` y agrega las observaciones a la base;
las anteriores no se sobrescriben. Si un comercio falla, los demás continúan y la tabla `ejecuciones` registra el
estado (`completa`, `parcial` o `fallida`).

## Renderizar el documento

```bash
QUARTO_PYTHON=.venv/bin/python .venv/bin/quarto render taller_1.qmd
```

Por defecto el documento trabaja sin conexión (`OFFLINE = True` en la primera celda) sobre la última ejecución
guardada en `raw/`. Para que haga una recolección nueva al renderizar, cambiar a `OFFLINE = False`.

Desde Python o Quarto también se pueden llamar directamente:

```python
import scraper, comparables
scraper.run_all(offline=True)          # o offline=False
comparables.marcar_comparables()
```

## Notas

* Las fechas y horas de consulta están en ISO 8601 con hora de Colombia (`-05:00`).
* Un precio no publicado queda en `NULL`; un producto agotado queda con `disponible = 0` y no se usa en las
  comparaciones.
* Los precios corresponden al momento de cada captura y pueden incluir promociones temporales (`es_promocion = 1`).
