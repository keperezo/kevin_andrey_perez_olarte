# Taller 1: comparador de precios de nutrición deportiva

Se comparan los precios de productos de nutrición deportiva (proteínas, creatina, aminoácidos, preentrenos, geles, barras y bebidas deportivas) en Éxito, Farmatodo y Locatel, con precios de Bogotá obtenidos por web scraping. 

Lenguaje: Python. Todos los comandos se ejecutan desde esta carpeta, porque las rutas son relativas.

## Estructura

```
taller_1.qmd                documento de análisis (Quarto)
scraper.py                  extracción, limpieza y carga en SQLite 
comparador_precios.sqlite   base con todas las ejecuciones
raw/                        respuestas originales comprimidas: raw/<comercio>/<fecha>/<id_ejecucion>/
requirements.txt            dependencias con versiones fijas
```

Cada carpeta de `raw/` tiene las respuestas descargadas (`pagina_*.json.gz`) y un `manifiesto.json` con el resumen de la ejecución. 

## Reprocesar sin conexión

Dejar `ONLINE = False` en la primera celda de código de `taller_1.qmd` y ejecutar:

```
quarto render taller_1.qmd
```

O abrirlo en RStudio, VS Code o Positron y ejecutar todas las celdas. El documento vuelve a leer las copias de `raw/`, las carga en `comparador_precios.sqlite` sin duplicar lo que ya estaba y repite la limpieza, la correspondencia, las consultas y los gráficos. No se conecta a ningún sitio.

Para reconstruir la base desde cero basta con borrar `comparador_precios.sqlite` antes de ejecutar, porque todo lo necesario está en `raw/`.

## Hacer una recolección nueva

Con `ONLINE = True` en la primera celda de código de `taller_1.qmd`, o desde la terminal:

```
python -c "import scraper; print(scraper.ejecutar_recoleccion(online=True))"
```

La función recolecta los tres comercios con un mismo identificador de ejecución, guarda las respuestas en `raw/` y agrega las observaciones a la base sin borrar las anteriores. Si un comercio falla, queda registrado como `fallida` y la función sigue con los demás. Una ejecución completa tarda alrededor de 5 minutos: unos 2 en Éxito, 1,5 en Farmatodo y 1 en Locatel.

