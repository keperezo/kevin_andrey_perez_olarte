# Taller 1 — Comparador de precios: pañales, pañitos, cambiadores desechables y cremas de barrera

**Asignatura:** Minería de Datos (2016325) · Universidad Nacional de Colombia
**Lenguaje elegido:** Python (>= 3.11)
**Comercios (VTEX):** Éxito, Jumbo, Olímpica — configurables en `scraper.COMERCIOS_ACTIVOS`.

## Estructura de archivos

| Archivo | Descripción |
|---|---|
| `taller_1.qmd` | Documento reproducible (Quarto + Python): scraping, limpieza, SQLite, 8 consultas SQL, gráficos, conclusiones |
| `scraper.py` | Módulo de extracción: una función por comercio (`scrape_exito`, `scrape_jumbo`, `scrape_olimpica`), `ejecutar_recoleccion()` y `reprocesar_raw()` |
| `limpieza.py` | Delimitación de la categoría, estandarización y correspondencia de productos |
| `comparador_precios.sqlite` | Base de datos generada por el scraper |
| `raw/` | Copias crudas (JSON comprimido) por comercio y fecha |
| `requirements.txt` | Dependencias |
| `tests/test_pipeline.py` | Prueba con datos **sintéticos** que valida el código (no usa internet ni genera datos reales) |

## Ejecución

```bash
python -m venv .venv && source .venv/bin/activate      # Windows: .venv\Scripts\activate
pip install -r requirements.txt
```

### 1. Diagnóstico previo (recomendado)
```bash
python scraper.py probe
```
Muestra el `robots.txt`, el modo de API que responde en cada comercio y las categorías detectadas con el total que muestra el sitio.

Si un comercio falla, obtenga el detalle exacto (código HTTP, servidor, tiempo) con:
```bash
python scraper.py diagnose                                   # comercios activos
python scraper.py diagnose https://www.carulla.com https://www.locatelcolombia.com   # candidatos de reemplazo
```
Un `403`/timeout suele indicar bloqueo de la IP (frecuente al ejecutar desde Google Colab u otros servidores en la nube:
pruebe desde su computador y su red doméstica). Si un sitio sigue sin responder, cámbielo en `COMERCIOS_ACTIVOS`
por otro que sí responda (el enunciado lo exige: tres comercios con datos en la misma ejecución).

### 2. En línea
```bash
python scraper.py run            # o:  RECOLECTAR_EN_LINEA = True en taller_1.qmd
quarto render taller_1.qmd       # o ábralo en VS Code/Jupyter y ejecute todas las celdas
```
Cada ejecución **agrega** observaciones (no sobrescribe). Ejecute `run` en al menos dos fechas distintas para observar cambios de precio.

### 3. Sin conexión (desde `raw/`)
```bash
rm -f comparador_precios.sqlite
python scraper.py offline        # reconstruye la base solo con raw/, sin consultar ningún sitio
quarto render taller_1.qmd
```

### 4. Prueba de humo del código
```bash
python tests/test_pipeline.py    # debe terminar con "OK: pipeline completo y modo sin conexión verificados"
```
Se sugiere además ejecutar `python scraper.py run` una vez en Google Colab (`!git clone … && !pip install -r requirements.txt && !python scraper.py run`) y reportar el resultado en el documento.

## Notas
* Los precios representan el momento de la captura. No se incluyen costos de envío ni descuentos por tarjeta.
* No se incluyen contraseñas ni tokens; los sitios se consultan sin autenticación.
