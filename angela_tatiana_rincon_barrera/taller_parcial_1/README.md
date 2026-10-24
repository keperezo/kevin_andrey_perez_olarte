# Taller 1: Minería de Datos - Comparador de Precios de Protectores Solares

## 1. Información General
- **Asignatura:** Minería de Datos (Código 2016325).
- **Categoría:** Protectores solares, faciales, corporales y labiales; productos para después del sol. Incluye protectores infantiles.
- **Lenguaje de Programación:** **Python 3.13**
- **Comercios en Línea Evaluados (Colombia):**
  1. **Bella Piel** (`https://www.bellapiel.com.co`) - API Catalog VTEX (`/proteccion-solar`)
  2. **Drogas La Rebaja** (`https://www.larebajavirtual.com`) - API Catalog VTEX (`fq=C:/5/35/` Cuidado personal › Solares y `fq=C:/8/465/` Dermocosmética › Solares)
  3. **Locatel Colombia** (`https://www.locatelcolombia.com`) - API Catalog VTEX (`fq=C:/2/17/` Dermocosmética / Cuidado Solar y `fq=C:/1/8/55/` Cuidado Personal / Cuidado Piel / Protección Solar)

### 1.1 Revisión Ética de Condiciones de Uso y Exclusión de Fuentes
En estricto cumplimiento de las buenas prácticas de web scraping y la directiva `robots.txt` de cada dominio:
- **Droguerías Colsubsidio (`drogueriascolsubsidio.com`):** Excluida formalmente del estudio debido a que su `robots.txt` prohíbe de manera explícita el acceso a endpoints de API (`Disallow: /api/`) y bloquea búsquedas con parámetros de consulta de catálogo (`Disallow: /*?*ft=*`).
- **Medipiel (`medipiel.com.co`):** Excluida formalmente del estudio debido a que su `robots.txt` incluye una regla restrictiva general que prohíbe cualquier URL con parámetros de consulta (`Disallow: /*?*`), lo cual impide paginar o consultar el catálogo de forma automatizada conforme a sus términos.
- **Comercios seleccionados (Bella Piel, La Rebaja, Locatel):** Sus archivos `robots.txt` no restringen la navegación académica ni los catálogos consultados. Se implementó una tasa de consulta cortés (`time.sleep(0.5)`) y un encabezado `User-Agent` que declara el propósito docente e investigativo (`MineriaDeDatos-UNAL-Taller1/1.0`).

---

## 2. Estructura del Repositorio

```text
TallerScrap/
├── raw/                                  # Copias crudas comprimidas (.json.gz) por comercio y fecha
│   ├── bellapiel/<fecha>/                # Respuestas crudas de Bella Piel
│   ├── larebaja/<fecha>/                 # Respuestas crudas de Drogas La Rebaja
│   └── locatel/<fecha>/                  # Respuestas crudas de Locatel Colombia
├── comparador_precios.sqlite             # Base de datos SQLite normalizada
├── scraper.py                            # Módulo independiente de extracción, estandarización y carga
├── taller_1.qmd                          # Documento reproducible en Quarto (análisis, SQL y visualizaciones)
├── requirements.txt                      # Dependencias de Python con versiones fijas
└── README.md                             # Guía de reproducción y documentación técnica
```

---

## 3. Instalación de Dependencias

Se recomienda utilizar un entorno virtual de Python:

```bash
# Crear entorno virtual (opcional pero recomendado)
python -m venv venv

# Activar en Windows
.\venv\Scripts\activate

# Instalar dependencias fijas
pip install -r requirements.txt
```

---

## 4. Instrucciones de Ejecución

### A. Modo Sin Conexión (Offline - Recomendado para evaluación)
Procesa las copias crudas guardadas en la estructura `raw/<comercio>/<fecha>/*.json.gz` sin realizar peticiones a los servidores externos:

```bash
# Reprocesar la ejecución más reciente en raw
python scraper.py --offline

# Reprocesar todas las ejecuciones históricas almacenadas en raw
python scraper.py --offline --reprocesar-todas
```

### B. Modo En Línea (Online)
Realiza las consultas directas contra los endpoints de catálogo de las tres tiendas, actualiza las copias comprimidas en `raw/<comercio>/<fecha>/` e inserta las nuevas observaciones en SQLite:

```bash
python scraper.py
```

### C. Renderizado del Reporte Reproducible (`taller_1.qmd`)
Si tiene Quarto instalado en su sistema:

```bash
quarto render taller_1.qmd --to html
```

También puede abrir `taller_1.qmd` en VS Code, Positron o RStudio y ejecutar las celdas interactivamente.

---

## 5. Estructura de la Base de Datos SQLite (`comparador_precios.sqlite`)

La base relacional consta de cuatro tablas con integridad referencial estricta:

1. **`corridas`**:
   - `id_corrida` (INTEGER PRIMARY KEY AUTOINCREMENT)
   - `fecha_hora_inicio`, `fecha_hora_fin` (TEXT ISO 8601 con zona horaria de Colombia `-05:00`)
   - `n_total_observaciones` (INTEGER)
   - `estado` (`completa`, `parcial`, `fallida`)
   - *Propósito:* Agrupa y unifica las recolecciones de los tres comercios (Locatel, Bella Piel y La Rebaja) bajo un identificador común (`id_corrida`), permitiendo analizar todas las tiendas en una misma ejecución lógica incluso si sus extracciones se originaron en fechas distintas (ej. 8 y 9 de octubre).

2. **`ejecuciones`**:
   - `id_ejecucion` (INTEGER PRIMARY KEY AUTOINCREMENT)
   - `id_corrida` (INTEGER FOREIGN KEY REFERENCES `corridas(id_corrida)`)
   - `fecha_hora_inicio`, `fecha_hora_fin` (TEXT ISO 8601 con zona horaria de Colombia `-05:00`)
   - `comercio` (TEXT)
   - `total_reportado_sitio` (INTEGER: suma de los totales del header `resources` por categoría; `NULL` para copias antiguas sin total reportado)
   - `total_obtenido` (INTEGER: cantidad de SKU distintos extraídos)
   - `n_registros` (INTEGER)
   - `estado` (`completa`, `parcial`, `fallida`)
   - *Criterios de Estado:*
     - `completa`: si los SKU distintos obtenidos (`total_obtenido`) son iguales o mayores que el total reportado por el sitio (`total_reportado_sitio`) y no falló ninguna petición HTTP.
     - `parcial`: si se obtuvieron menos registros que los reportados por el sitio, si falló alguna petición HTTP o si corresponde a una copia antigua sin total reportado (`total_reportado_sitio IS NULL`).
     - `fallida`: si no se obtuvo ningún producto (`total_obtenido = 0`).

3. **`productos_comparables`**:
   - Catálogo maestro de las 11 presentaciones comunes con `id_producto_comun` (PRIMARY KEY), `nombre_estandar`, `marca_estandar`, `categoria_estandar`, `presentacion_estandar`, `contenido_valor`, `contenido_unidad`, `ean`.

4. **`observaciones`**:
   - `id_observacion` (INTEGER PRIMARY KEY AUTOINCREMENT)
   - `id_corrida` (INTEGER FOREIGN KEY REFERENCES `corridas(id_corrida)`)
   - `id_ejecucion` (INTEGER FOREIGN KEY REFERENCES `ejecuciones(id_ejecucion)`)
   - Almacena el texto original íntegro del anuncio (`nombre_original`, `descripcion_original`), precios observados (`precio_actual` nullable para items sin precio, `precio_lista`, `es_promocion`), URL de extracción, campos estandarizados de contenido y costo unitario (`precio_por_unidad`), pertenencia a categoría (`en_categoria = 1` o `0`), motivo de exclusión (`motivo_exclusion`), código EAN (`ean`), e indicador `comparable`.
   - **Restricción de unicidad:** `UNIQUE (comercio, id_producto_comercio, fecha_hora_consulta)`.

---

## 6. Cobertura de Productos Comparables y Emparejamiento por EAN

En **Locatel**, el emparejamiento de los productos comparables se realiza **estrictamente por código EAN** (`items[].ean`). Se excluyeron las referencias *ISDIN Fusion Water*, *ISDIN Active Unify* e *ISDIN Spot Prevent* debido a que en Locatel presentan códigos EAN distintos a los de La Rebaja.

Se consolidaron **11 presentaciones idénticas** presentes simultáneamente en los **3 comercios** (**33 observaciones comparables**, superando el umbral mínimo de 24):

1. `3337875797597`: La Roche-Posay Anthelios UVMune 400 Fluido Invisible FPS 50+ (50 ml)
2. `3337875797641`: La Roche-Posay Anthelios UVMune 400 Fluido con Color FPS 50+ (50 ml)
3. `7612076457430`: Cetaphil Sun Kids Loción Liposomal FPS 50+ (150 ml)
4. `7612076457256`: Cetaphil Sun Gel Ligero FPS 50+ (100 ml)
5. `4005900994776`: Eucerin Sun Hydro Protect Ultra-Light Fluid FPS 50+ (50 ml)
6. `4005900265227`: Eucerin Sun Photoaging Control FPS 50+ (50 ml)
7. `4005800241901`: Eucerin Sun Pigment Control FPS 50+ (50 ml)
8. `4005900839633`: Eucerin Sun Gel-Crema Oil Control Toque Seco FPS 50+ Tono Medio (50 ml)
9. `7703281002451`: Umbrella Urban Fotoprotector FPS 50+ (50 g)
10. `7703281021551`: Umbrella Acqua Fotoprotector FPS 50+ (50 g)
11. `7707206270598`: Sunaid Evolution Protector Solar Gel FPS 50+ (40 g)

---

## 7. Principales Hallazgos para el Consumidor

Para consultar las cifras y resultados detallados de la corrida de referencia (totales y estado por comercio, liderazgo en precios por producto, estadísticas de precios mínimos, máximos y promedios, oportunidades de ahorro en COP y porcentaje, y la comparación de la canasta consolidada frente a la canasta óptima), consulte el documento reproducible [taller_1.html](taller_1.html) (generado a partir de [taller_1.qmd](taller_1.qmd)).

- **Ahorro Unitario vs. Desembolso Total:**
  - *Desembolso total (precio del empaque):* Es el dinero nominal desembolsado en caja, determinante para la restricción de liquidez y presupuesto inmediato del consumidor.
  - *Ahorro unitario (COP/mL o COP/g):* Mide eficiencia técnica por dosis entre tamaños de un **mismo producto** (misma marca, línea y FPS) en un mismo comercio; no aplica entre formulaciones distintas ni en la muestra actual al no existir tamaños múltiples de un mismo producto.
