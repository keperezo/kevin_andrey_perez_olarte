# Taller 1 – Comparador de precios (Categoría 42: Repelentes y control de insectos)
Lenguaje: **Python 3.10+** (probado con 3.13)

## Archivos
| Archivo | Qué es |
|---|---|
| `scraper.py` | Módulo de extracción (tu `Scrapper.py` renombrado, sin cambios): `scrape_uno_a`, `scrape_colsubsidio`, `scrape_locatel`, `setup_database`. |
| `procesamiento.py` | Limpieza (precios, marca, contenido), pertenencia a la categoría, correspondencia entre comercios y **modo sin conexión** (`leer_raw`, `cargar_raw_en_db`). |
| `taller_1.qmd` | Documento reproducible (Quarto + Python): scraping, limpieza, SQLite, 8 consultas SQL, gráficos, limitaciones y conclusiones. |
| `test_procesamiento.py` | Pruebas del procesamiento (funciones, modo sin conexión con HTML sintético y tu base real). |
| `comparador_precios.sqlite` | Base de datos generada por el scraper (tablas `ejecuciones`, `observaciones`) y por el procesamiento (`comercios`, `anuncios`, `precios`, `productos_comunes`, `exclusiones`). |
| `raw/<comercio>/<fecha>/pagina_*.html` | Copias crudas del HTML de cada ejecución. |
| `candidatos_correspondencia.csv` | Generado por el documento: grupos encontrados y anuncios sin contenido, para revisión manual. |

## Dependencias
`requests`, `beautifulsoup4`, `pandas`, `matplotlib`, `tzdata`, `jupyter`, `pyyaml` (y Quarto como programa aparte, https://quarto.org).

## Ejecución
```bash
pip install -r requirements.txt
python test_procesamiento.py comparador_precios.sqlite     # pruebas (sin red)
quarto render taller_1.qmd                                  # genera taller_1.html
```
**En línea (volver a consultar los sitios):** en `taller_1.qmd` poner `EJECUTAR_SCRAPING = True`.
Llama a `scraper.setup_database()` (no borra la base) y a las tres funciones de scraping; las nuevas
observaciones se agregan sin sobrescribir las anteriores. Para otros términos: `TERMINOS = ["repelente", "autan", ...]`.

**Sin conexión (desde `raw/`):** `USAR_OFFLINE = True`. Reconstruye `comparador_precios_offline.sqlite`
leyendo los HTML guardados (mismos selectores que `scraper.py`), sin consultar ningún sitio.

**Revisión manual de la correspondencia:** abrir `candidatos_correspondencia.csv`; completar contenido verificado
en `CONTENIDO_MANUAL = {url: (120, "ml")}` y/o fijar los productos comparables en `COMPARABLES_MANUALES = [ids]`.

## Notas
- La tabla `observaciones` (texto original y precio del scraper) no se modifica; lo derivado se reconstruye en tablas nuevas.
- Los precios de Colsubsidio quedaron con dígitos pegados en `observaciones` (p. ej. `3200024000`); `procesamiento.separar_precios`
  los recupera (menor = precio actual, mayor = precio antes del descuento). Con el modo sin conexión se leen limpios del HTML.
- Precios en COP; fecha y hora ISO 8601 con −05:00.
