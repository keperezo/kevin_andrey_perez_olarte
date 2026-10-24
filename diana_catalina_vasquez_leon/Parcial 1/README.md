# Taller 1 · Minería de Datos · Comparador de precios de dermocosmética facial

**Lenguaje:** Python 3 · **Autora:** Diana Catalina Vásquez León · **Comercios:** Dermatodo, Farmatodo, La Rebaja

## Estructura
| Archivo | Descripción |
|---|---|
| `taller_1.qmd` | Documento reproducible (código, SQL, gráficos, interpretación) |
| `custom.scss` | Estilos del informe |
| `scraper.py` | Extracción (una función por comercio + `recolectar_todo`) |
| `preparacion.py` | Estandarización y correspondencia de productos (`comparable`, `id_producto_comun`) |
| `comparador_precios.sqlite` | Base de datos |
| `raw/` | Copias crudas (JSON comprimido) por comercio/fecha/ejecución |
| `requirements.txt` | Dependencias con versiones fijas |

## Ejecución
```bash
pip install -r requirements.txt
export SCRAPER_CONTACTO="tu_correo@unal.edu.co"

# En línea (consulta los sitios, guarda raw/ e inserta en SQLite sin sobrescribir)
python -c "import scraper; scraper.recolectar_todo()"

# Sin conexión (reprocesa raw/)
python -c "import scraper; scraper.recolectar_todo(offline=True)"

# Correspondencia de productos y render del informe
python preparacion.py
quarto render taller_1.qmd
```
Variable `TALLER_MODO` (`en_linea` | `offline` | `existente`) controla qué hace el documento al iniciar.

## Probado en
✏️ Google Colab — fecha y resultado de la ejecución.
