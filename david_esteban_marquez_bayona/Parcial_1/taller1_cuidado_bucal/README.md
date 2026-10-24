# Taller 1 · Comparador de precios de cuidado bucal

Minería de Datos (2016325) · Universidad Nacional de Colombia · **David Márquez**

- **Categoría:** cuidado bucal (cremas dentales, cepillos, enjuagues, seda dental, prótesis, blanqueamiento…).
- **Lenguaje:** Python ≥ 3.11 (todo el proceso: extracción, limpieza, SQLite, consultas y gráficos).
- **Comercios:** Éxito, Olímpica y Locatel (plataforma VTEX).

## Estructura

| Archivo / carpeta | Contenido |
|---|---|
| `scraper.py` | Módulo de extracción: una función por comercio (`extraer_exito`, `extraer_olimpica`, `extraer_locatel`) con las mismas columnas de salida, y `ejecutar_recoleccion()` que registra la ejecución e inserta en SQLite sin sobrescribir. |
| `esquema.sql` | Estructura de la base (tablas, restricciones de unicidad y vista); la ejecuta `scraper.conectar()`. |
| `limpieza.py` | Reglas documentadas R1–R9 de normalización (marca, subcategoría, contenido, unidades) y correspondencia de productos (manual → EAN → clave normalizada). |
| `taller_1.qmd` | Documento reproducible (Quarto): fuentes, preparación, base, 8 consultas SQL, gráficos, conclusiones. Llama a las funciones de los módulos, no duplica su código. |
| `comparador_precios.sqlite` | Base de datos (tablas `comercios`, `ejecuciones`, `secciones`, `anuncios`, `productos`, `observaciones`, `familias`, `parametros` y vista `v_observaciones`). |
| `raw/<comercio>/<fecha>/<ejecución>/` | Copias crudas comprimidas de cada respuesta JSON + `manifest.json` (URL, parámetros, fecha y hora, código HTTP). `raw/<comercio>/robots/` guarda `robots.txt`. |
| `correspondencias_manuales.csv` | Correcciones manuales de la correspondencia o exclusiones (opcional). |
| `conteo_sitio_web.csv` | Número de resultados que muestra la página web de la categoría, para verificar completitud (opcional, a mano). |
| `prueba_entorno.json` | Resultado de la prueba en un entorno distinto (Google Colab). |
| `requirements.txt` | Dependencias con versiones fijas. |

## Instalación

```bash
python -m venv .venv
source .venv/bin/activate            # Windows: .venv\Scripts\activate
pip install -r requirements.txt
```

Para renderizar el informe se necesita [Quarto](https://quarto.org) ≥ 1.4.

## Ejecución en línea

```bash
python scraper.py                    # nueva ejecución: consulta los 3 comercios, guarda raw/ y carga SQLite
quarto render taller_1.qmd           # genera taller_1.html a partir de la base
```

O en un solo paso: `quarto render taller_1.qmd -P EJECUTAR_SCRAPING:true`.

Cada ejecución crea un identificador (`E20261006T101500`), guarda las respuestas en `raw/` y agrega
observaciones nuevas sin borrar las anteriores. Se recomienda ejecutar al menos dos veces, en fechas
distintas, para observar cambios de precio.

Desde Python: `import scraper; scraper.ejecutar_recoleccion()`.

## Ejecución sin conexión

Reprocesa las copias de `raw/` sin consultar ningún sitio:

```bash
python scraper.py --sin-conexion                         # sobre la base del proyecto (no duplica nada)
python scraper.py --sin-conexion --db verificacion.sqlite  # reconstruye una base nueva desde raw/
```

El informe incluye una verificación automática (sección 7.3) que reconstruye la base desde `raw/` en una
carpeta temporal y comprueba que las observaciones sean idénticas.

## Diagnóstico y ajustes

```bash
python scraper.py --diagnostico exito          # secciones del sitio que se tomarán como «cuidado bucal»
python scraper.py --robots                     # revisa y guarda robots.txt (cada ejecución en línea también lo guarda)
```

- Si la sección detectada no es la correcta, se fijan los filtros a mano en el diccionario
  `FILTROS` de `scraper.py`, por ejemplo `FILTROS = {"exito": ["/12/345/"]}`.
- Si las reglas emparejan mal un producto, se corrige en `correspondencias_manuales.csv` y se vuelve a
  correr `limpieza.actualizar_correspondencias` (lo hace automáticamente `ejecutar_recoleccion` y también
  `--sin-conexion`); no hace falta volver a consultar los sitios.
- La delimitación de la categoría (subcategorías y exclusiones) está en `limpieza.SUBCATEGORIAS` y
  `limpieza.EXCLUSIONES`; debe coincidir con la planilla «Categorías de Productos - Estudiantes».

## Prueba en otro entorno (Google Colab)

```python
%cd /content
!rm -rf david_esteban_marquez_bayona
!git clone --depth 1 https://github.com/DEMB9/david_esteban_marquez_bayona
%cd /content/david_esteban_marquez_bayona/david_esteban_marquez_bayona/Parcial_1/taller1_cuidado_bucal
!pip install -q -r requirements.txt
!python scraper.py --probar              # recolección completa en una carpeta temporal (no toca la base ni raw/)
```

Descargar el `prueba_entorno.json` generado y subirlo a la carpeta: el informe lo muestra en la sección 2.5.
Las IP de Colab están fuera de Colombia; si algún sitio responde distinto, queda documentado ahí.

## Notas

- No se usan contraseñas, tokens ni claves. Pausa de 1 s entre peticiones y reintentos con espera exponencial.
- Los precios ausentes quedan como `NULL` (nunca 0); fechas en ISO 8601 con hora de Colombia (`-05:00`).
- Ningún archivo supera 100 MB: las respuestas crudas se guardan comprimidas con gzip.
