# Taller 1 — Comparador de precios de antiinfecciosos de uso sistémico

**Lenguaje elegido: Python** (≥ 3.10; probado con 3.13). Todo el proceso (extracción,
limpieza, carga en SQLite, consultas y gráficos) está en Python.

**Categoría:** Medicamentos antiinfecciosos de uso sistémico (antibióticos, antivirales,
antimicóticos sistémicos y antiparasitarios; excluye tratamientos locales de piel, ojos u
oídos).

**Comercios:** La Rebaja Virtual · Locatel Colombia · Droguerías Cafam.

## Estructura de archivos

```
taller_01/
├── taller_1.qmd                 documento reproducible (llama a scraper.py; no duplica código)
├── taller_1.html                documento renderizado
├── scraper.py                   módulo de extracción: una función por comercio + función general
├── categoria.py                 reglas que delimitan la categoría (principio activo y vía)
├── limpieza.py                  estandarización y apoyo a la correspondencia de productos
├── correspondencia.csv          correspondencia de las presentaciones comparables (verificada a mano)
├── correspondencia_propuesta.csv  candidatos generados por regla (salida del .qmd)
├── comparador_precios.sqlite    base de datos
├── raw/                         copias crudas: raw/<comercio>/<AAAA-MM-DD>/<id_ejecucion>/
│   └── .../pagina_0001.json.gz, manifest.json y arbol_categorias.json.gz
│       (en Cafam, subcategorias.json.gz)
├── requirements.txt             dependencias con versiones fijas
├── prueba_colab.ipynb           prueba en Google Colab (sin conexión y en línea)
└── README.md
```

## Instalación

```bash
python -m venv .venv
# Windows: .venv\Scripts\activate      macOS/Linux: source .venv/bin/activate
pip install -r requirements.txt
```

Para renderizar el documento también se necesita [Quarto](https://quarto.org/docs/get-started/).

## Ejecución

### 1. Recolección en línea (consulta los tres sitios)

```bash
python scraper.py
```

Tarda unos 25 a 30 minutos (una petición por segundo): unos 5 minutos La Rebaja, 5
Locatel y 17 Cafam, que se lee de a 12 productos por página. Cada corrida:

1. crea un identificador de ejecución (`E20261002T093000`, fecha y hora de Colombia);
2. recorre las secciones de medicamentos de cada comercio y guarda cada respuesta en
   `raw/<comercio>/<fecha>/<id_ejecucion>/`;
3. aplica la delimitación de la categoría (`categoria.py`) y la estandarización
   (`limpieza.py`);
4. inserta los resultados en `comparador_precios.sqlite` **sin sobrescribir** las
   observaciones anteriores (restricción `UNIQUE (id_comercio, sku_comercio, fecha_consulta)`);
5. registra la corrida en la tabla `ejecuciones` con estado `completa`, `parcial` o
   `fallida`.

Se recomienda correrlo **en dos fechas distintas** para observar cambios de precio.

Para un solo comercio: `python scraper.py --comercios locatel`.

### 2. Modo sin conexión (no consulta ningún sitio)

```bash
python scraper.py --sin-conexion                       # reprocesa todo raw/ sobre la base actual
python scraper.py --sin-conexion --db verificacion.sqlite   # reconstruye una base nueva desde raw/
```

Usa exactamente el mismo código de lectura y limpieza que el modo en línea, pero lee las
respuestas de `raw/`. Reprocesar sobre la misma base no duplica observaciones: si la
ejecución ya estaba en la base, sus observaciones y excluidos se vuelven a generar desde
`raw/` con las reglas actuales de la categoría, y su fila de `ejecuciones` (modo, fechas y
estado) se conserva. Las demás ejecuciones no se tocan.

Para reprocesar una sola ejecución: `python scraper.py --sin-conexion --id-ejecucion E20261009T235057`.

Desde Python o desde el `.qmd`:

```python
import scraper
scraper.ejecutar_recoleccion()                   # en línea
scraper.ejecutar_recoleccion(sin_conexion=True)  # sin conexión
```

### 3. Correspondencia de productos

1. Renderiza el documento una vez (paso 4) o, en Python, ejecuta
   `limpieza.proponer_correspondencias(con, id_ejecucion)`. Se genera
   `correspondencia_propuesta.csv`, con una fila por presentación y comercio.
   Regla: mismo EAN, mismas unidades o mismo contenido, y presencia en los tres comercios
   dentro de una misma ejecución.
2. Revisa cada candidato a mano: marca, variante, concentración, número de unidades y
   forma farmacéutica. Copia a `correspondencia.csv` al menos 8 presentaciones y pon
   `verificado = 1`.
3. Al renderizar, el `.qmd` carga `correspondencia.csv` en las tablas `productos` y
   `correspondencia` y marca `anuncios.comparable = 1`.

### 4. Documento

```bash
quarto render taller_1.qmd
```

Por defecto el documento lee la base existente. Para que también recolecte, cambia
`RECOLECTAR_EN_LINEA = True` (o `REPROCESAR_SIN_CONEXION = True`) al inicio del `.qmd`.

## Fuentes y estrategia de extracción

Los tres sitios usan la plataforma **VTEX**, pero no se leen igual: se respetó lo que
permite el `robots.txt` de cada uno.

**La Rebaja y Locatel:** las páginas de categoría cargan sus productos desde un servicio
interno que responde en JSON (su `robots.txt` no restringe esa ruta). Se identificó en la
pestaña *Red* (Network) de las herramientas de desarrollador del navegador:

```
GET https://<sitio>/api/catalog_system/pub/products/search?fq=C:/<ids>/&_from=0&_to=49&O=OrderByNameASC
```

**Cafam:** su `robots.txt` prohíbe `/api/`, pero permite las páginas de categoría con
`?page=N`. Cada página trae en el HTML un bloque JSON (`__STATE__`) con sus 12 productos:

```
GET https://www.drogueriascafam.com.co/salud-y-medicamentos/<subcategoría>?page=N
```

| Comercio | Sitio | Secciones recorridas |
|---|---|---|
| La Rebaja Virtual | larebajavirtual.com | `C:/9/` DROGUERIA (completa) |
| Locatel | locatelcolombia.com | `C:/4/25/` medicamentos · `C:/4/28/` Medicamentos OTC · `C:/4/26/168/` vitaminas > Antiinfecciosos |
| Cafam | drogueriascafam.com.co | `/salud-y-medicamentos` (completa), recorrida por sus 12 subcategorías |

- **Paginación:** en La Rebaja y Locatel, máximo 50 productos por petición; el total llega
  en la cabecera `resources` (`0-49/4306`). En Cafam, 12 productos por página; el total
  llega en el bloque `__STATE__` (`recordsFiltered`).
- **Límite de 2.500:** VTEX responde con error 400 si `_from` pasa de ~2.500. En La Rebaja
  y Locatel las secciones más grandes se parten por rangos de precio (`fq=P:[a TO b]`)
  hasta que cada parte tenga 2.500 productos o menos (`scraper._particionar`). El filtro
  se envía con los espacios como `%20`: con `+` el cortafuegos de los sitios responde 400.
  En Cafam, las páginas de categoría (VTEX IO) no muestran más de 50 páginas (600
  productos) por listado: se recorre cada subcategoría por separado y la más grande,
  Medicamentos RX (~1.970), se parte con el filtro de precio de la página
  (`?priceRange=A TO B`, permitido por su `robots.txt`) hasta que cada rango tenga 600
  productos o menos.
- **Completitud:** en cada sección se compara la cantidad de productos distintos
  descargados con el total que reporta el sitio. Si coinciden en todas y no hubo errores,
  la ejecución queda `completa`.
- **Catálogo en movimiento:** si el sitio actualiza su catálogo mientras se pasan las
  páginas, un producto puede salir dos veces y otro ninguna. Cuando a un rango (o a una
  subcategoría de Cafam) le faltan productos, se recorre una segunda vez y se juntan los
  resultados.
- **Productos que Cafam cuenta pero no muestra:** en Medicamentos RX hay páginas que traen
  11 productos en vez de 12, igual en las dos vueltas. Se anotan en el `manifest.json`
  (`no_mostrados_por_seccion`) y no impiden que la ejecución quede `completa`, porque se
  descargó todo lo que el sitio muestra.
- **¿Por qué se recorre toda la sección de medicamentos?** Ningún comercio separa los
  medicamentos con fórmula por grupo terapéutico como lo hace la planilla. La categoría se
  delimita después, con las reglas de `categoria.py` (principio activo, clasificación ATC y
  vía de administración). Lo que queda fuera se registra en la tabla `excluidos` con su
  motivo.

### Selección de los comercios

Antes de escribir el código se exploraron ocho droguerías en línea (2 de octubre de 2026):

| Sitio | Plataforma / acceso | Decisión |
|---|---|---|
| La Rebaja Virtual | VTEX, JSON público con principio activo, laboratorio, EAN y presentación | **elegido** |
| Locatel | VTEX, JSON público con sección "Antiinfecciosos" y principio activo | **elegido** |
| Cafam | VTEX IO; su robots.txt prohíbe `/api/`, pero las páginas de categoría traen los mismos datos (principio activo, laboratorio, vía de administración) | **elegido**, leído desde las páginas HTML |
| Colsubsidio | VTEX, JSON público | respaldo (`scrape_colsubsidio`), mismo problema de robots.txt que Cafam |
| Pasteur | VTEX | descartado: su robots.txt bloquea toda URL con parámetros (`Disallow: /*?`) y tiene menos productos de la categoría |
| Cruz Verde | API propia que exige sesión y zona de inventario, sin EAN | descartado |
| Farmatodo | búsqueda en Algolia con llaves de acceso | descartado: el taller prohíbe incluir claves |
| Éxito | VTEX | descartado: su robots.txt bloquea `/api/` |

En una búsqueda exploratoria por 47 principios activos, los tres comercios elegidos
compartieron más de 100 códigos EAN, así que tener ocho presentaciones comunes no es un
riesgo.

### Revisión de robots.txt y condiciones de uso

Revisión del 3 de octubre de 2026:

- **La Rebaja** y **Locatel:** su `robots.txt` no restringe `/api/` (Locatel bloquea
  `/_v/`, `/busca` y `/search`, que no se usan).
- **Cafam:** su `robots.txt` prohíbe `/api/`, `?O=`, `?PS=`, `?q=` y `map=ft`, pero no las
  páginas de categoría con `?page=N`. Por eso Cafam se lee desde esas páginas (HTML) y no
  desde el servicio `/api/`. Antes de tomar esta decisión, la primera ejecución (fallida) y
  unas pruebas hicieron cerca de 10 peticiones a `/api/` de Cafam.
- No se accede a páginas privadas (cuenta, carrito, pagos) ni se usan credenciales.

## Base de datos (`comparador_precios.sqlite`)

| Tabla | Contenido |
|---|---|
| `comercios` | comercio, URL, secciones recorridas, revisión de robots.txt |
| `ejecuciones` | una fila por (ejecución, comercio): inicio, fin, modo, páginas, total reportado por el sitio, productos recorridos, registros en la categoría, estado |
| `anuncios` | cada SKU de un comercio: campos originales (`*_original`) y estandarizados (`*_std`, unidades, contenido), `comparable`, `id_producto` |
| `observaciones` | precio de un SKU en una fecha y hora: precio, precio de lista, promoción, disponibilidad, URL de extracción, texto original (JSON), `id_ejecucion` |
| `productos` | identificador común de las presentaciones comparables |
| `correspondencia` | qué SKU de cada comercio corresponde a cada `id_producto` |
| `excluidos` | antiinfecciosos que quedaron fuera de la categoría y su motivo |
| `v_observaciones` (vista) | todos los campos mínimos del taller en una sola tabla |

Los precios faltantes se guardan como `NULL`, nunca como 0.

## Ejecuciones incluidas

La base y `raw/` traen todas las corridas, incluidas las que fallaron mientras se corregía
el scraper (quedan en la tabla `ejecuciones` con su estado). Las dos ejecuciones completas en
los tres comercios son `E20261004T182736` (4 de octubre) y `E20261009T235057` (9 de
octubre); el documento usa la segunda como ejecución de referencia y la primera para
comparar fechas. `raw/` pesa unos 170 MB (unos 35 MB por corrida completa, 2.052 archivos)
y ningún archivo pasa de 1 MB.

## Prueba en un entorno limpio

Se copió el proyecto (código, `raw/` y `correspondencia.csv`, sin la base) a una carpeta
nueva con un entorno virtual nuevo instalado solo desde `requirements.txt`. El modo sin
conexión reconstruyó una base idéntica a la entregada (mismas observaciones, anuncios y
excluidos) y el documento se renderizó sin errores.

## Prueba en un entorno distinto (Google Colab)

El cuaderno `prueba_colab.ipynb` hace la prueba completa: clona el repositorio, instala
`requirements.txt`, reconstruye la base sin conexión y la compara con la entregada, y corre
el scraper en línea con un comercio. Se abre directamente en Colab con este enlace y se
ejecuta con *Entorno de ejecución → Ejecutar todo*:

<https://colab.research.google.com/github/marlonjimenezv/marlon_jimenez_valencia/blob/main/marlon_jimenez_valencia/taller_01/prueba_colab.ipynb>

Los mismos pasos, a mano:

```python
!git clone https://github.com/marlonjimenezv/marlon_jimenez_valencia.git
%cd marlon_jimenez_valencia/marlon_jimenez_valencia/taller_01
!pip install -q -r requirements.txt
!python scraper.py --comercios locatel        # prueba en línea con un comercio
!python scraper.py --sin-conexion --db /tmp/verificacion.sqlite
```

Si un sitio responde con 403 desde Colab (algunos bloquean IP de centros de datos), queda
registrado como `fallida` en `ejecuciones`, y el modo sin conexión sigue funcionando con
las copias de `raw/`.

## Solución de problemas

- **Error 403 en un comercio:** el sitio puede rechazar el `User-Agent`. Prueba cambiando
  `CABECERAS["User-Agent"]` en `scraper.py` por el de un navegador.
- **Una ejecución queda `parcial`:** el detalle de las páginas que fallaron queda en
  `ejecuciones.detalle` y en `manifest.json`. Vuelve a ejecutar; las observaciones previas
  no se pierden.
- **Cambian los ids de categoría:** el árbol de categorías de cada corrida queda guardado en
  `raw/.../arbol_categorias.json.gz`. Corrige los `fq` en `scraper.COMERCIOS`.
