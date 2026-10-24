# Taller 1: comparador de precios de higiene corporal

**Autor:** Thomas Matallana. **Asignatura:** Minería de Datos, 2016325.
**Lenguaje:** Python. **Informe:** Quarto con motor Jupyter.

## Contenido

- `taller_1.qmd`: informe reproducible; llama a `scraper.py` y muestra SQL, resultados, gráficos e interpretaciones.
- `taller_1.html`: vista del informe para leer en un navegador, con gráficos incorporados.
- `exportar_html.py`: alternativa de exportación HTML mediante Jupyter y nbconvert.
- `scraper.py`: recolección HTTP, copias crudas, limpieza y carga incremental en SQLite.
- `comparador_precios.sqlite`: observaciones reales de dos capturas del 10 de octubre de 2026.
- `raw/`: respuestas originales comprimidas, metadatos de captura, hashes y manifiestos por comercio y ejecución.
- `correspondencias.json`: ocho presentaciones, SKU por tienda, EAN, contenido y evidencia; no contiene precios.
- `consultas.sql`: ocho preguntas del enunciado y consultas auxiliares de auditoría y canasta.
- `verificar.py`: pruebas de cobertura, integridad, reconstrucción sin red e idempotencia.
- `requirements.txt`: versiones exactas del entorno Python utilizado.
- `_quarto.yml` y `estilos.css`: configuración y presentación del informe.

## Instalación

Requiere Python 3.12 y [Quarto](https://quarto.org/docs/get-started/).
Desde esta carpeta, en una terminal:

```powershell
python -m venv .venv
.venv\Scripts\python -m pip install -r requirements.txt
$env:QUARTO_PYTHON = (Resolve-Path .venv\Scripts\python.exe).Path
```

Si Python está registrado como `py`, usar `py -3.12 -m venv .venv` en la primera línea.
En macOS/Linux, el ejecutable del entorno es `.venv/bin/python` y se selecciona con
`export QUARTO_PYTHON="$PWD/.venv/bin/python"`.

## Reproducción sin conexión (recomendada para revisar la entrega)

```powershell
.venv\Scripts\python scraper.py --offline
.venv\Scripts\python verificar.py
quarto render taller_1.qmd
```

El scraper no requiere paquetes externos: también puede ejecutarse con Python
estándar. Los paquetes del archivo de dependencias se utilizan para el análisis
y el motor Jupyter. Instalar dependencias requiere internet una sola vez.

Por defecto, el informe procesa la última ejecución compartida por las tres
tiendas desde `raw/`. Repetirlo no duplica filas. Para reconstruir una corrida
en una base nueva y comprobar que no depende de la base entregada:

```powershell
.venv\Scripts\python scraper.py --offline --db reconstruccion.sqlite
```

Para reconstruir **todo el histórico**, ejecutar ambas corridas sobre la misma base:

```powershell
.venv\Scripts\python scraper.py --offline --run-id 20261010T112641_53bf109b --db reconstruccion.sqlite
.venv\Scripts\python scraper.py --offline --run-id 20261010T112803_180f96c4 --db reconstruccion.sqlite
```

## Nueva recolección en línea

Antes de repetirla, revisar las condiciones de las tiendas y sus archivos
`robots.txt`. En la captura entregada, Colsubsidio y Cafam declaraban
`Disallow: /api/` para rastreadores generales. Esa restricción se documenta como
limitación; que el endpoint sea accesible no implica permiso de rastreo.

```powershell
.venv\Scripts\python scraper.py
quarto render taller_1.qmd
```

También puede ejecutarse una nueva captura directamente desde el Quarto:

```powershell
$env:HIGIENE_ONLINE = "1"
quarto render taller_1.qmd
Remove-Item Env:HIGIENE_ONLINE
```

Cada corrida crea otro ID y conserva los precios anteriores. La cobertura se
comprueba por presentación y comercio: no basta con sumar 24 filas. Si un producto
cambia de SKU/EAN o deja de estar disponible, debe revisarse la correspondencia;
el documento no continúa silenciosamente con una comparación incompleta.

## Alcance y limpieza

Se recorren todas las páginas de las secciones declaradas en el informe. Se
consulta el canal público del vendedor principal sin dirección de entrega ni
sesión. Los precios están en COP, sin envío ni descuentos condicionados.

Los datos se extraen de JSON de catálogos VTEX. Se conservan nombre, descripción,
ficha, URL de producto, URL exacta de extracción, fecha/hora `-05:00`, SKU, EAN,
precio, precio de lista, disponibilidad y ejecución. `NULL` representa faltantes.
Las copias originales pueden incluir firmas públicas de precio generadas por
VTEX; no se agregaron credenciales, claves privadas ni tokens de usuario.

Las tablas `observaciones` y `cobertura` conservan la auditoría. La vista
`categoria` excluye artículos fuera de la delimitación. La vista `comparables`
incluye solo los ocho productos emparejados con precio válido y disponibilidad.
`correspondencias.json` documenta correcciones de contenido sin cambiar las fichas
originales. No se compara precio por gramo entre formulaciones diferentes.

La primera ejecución fue parcial en una sección de La Rebaja. La segunda
recuperó los dos productos omitidos con paginación inversa y verificó todos los
totales. Ambas son del mismo día; no se presenta una tendencia entre fechas.

## Entrega en GitHub

Esta carpeta corresponde a:

```text
dm_2016325_2026_2/
└── thomas_felipe_matallana_fiquitiva/
    └── taller_01/
```

El fork del estudiante es <https://github.com/tmatallana/dm_2016325_2026_2>.
El repositorio de origen identificado es <https://github.com/afflorezr/dm_2016325_2026_2>.
Copiar la carpeta `taller_01` dentro de la carpeta personal del fork, sin alterar
otras entregas. Crear una rama y un Pull Request hacia `main` del repositorio de
origen, siguiendo el README del curso. Esta preparación local **no crea el PR**.

Título sugerido: `Taller 01, Thomas Felipe Matallana Fiquitiva`.

La base SQLite y `raw/` sí deben incluirse porque el enunciado los exige.
No incluir `.venv`, `__pycache__`, `.quarto` ni archivos temporales.

## Validación

`verificar.py` reconstruye todas las tablas en una base temporal y exige igualdad
con la base entregada. Bloquea las llamadas HTTP durante esa prueba, repite la
reconstrucción para detectar duplicaciones y ejecuta las 14 consultas SQL.
Valida 24 observaciones comparables, las ocho presentaciones en tres comercios,
fechas con zona `-05:00`, claves foráneas e integridad SQLite.

Las pruebas se realizaron en Windows en este computador. No se ejecutaron en
Google Colab ni en otro computador.

Los 17 bloques Python del `.qmd` se ejecutaron con Jupyter sin errores. Quarto
encontró restricciones de permisos al iniciar su caché/configuración, tanto con
la versión instalada como con una versión portable. Por ello, el HTML incluido
se generó con Jupyter y nbconvert, y **no se confirma un `quarto render` exitoso**
en este entorno. El `.qmd` conserva su configuración normal para renderizarlo
en el computador del docente o desde una terminal local con los permisos necesarios.

Para regenerar la misma vista HTML alternativa:

```powershell
.venv\Scripts\python exportar_html.py
```
