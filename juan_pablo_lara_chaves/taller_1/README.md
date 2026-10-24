# Taller 1 — Comparador de precios: Ortopedia y rehabilitación

**Asignatura:** Minería de Datos
**Autor:** Juan Pablo Lara Chaves
**Lenguaje:** R

## Descripción

Comparador de precios construido a partir de web scraping en tres comercios
colombianos con venta en línea. Se identifican **8 productos o presentaciones**
disponibles simultáneamente en los tres comercios y se compara su precio para
orientar al consumidor sobre dónde conviene comprar cada uno.

**Comercios:**

- Farmatodo
- Locatel
- Ortopédicos Futuro

**Categoría:** Ortopedia y rehabilitación.

## Estructura del proyecto

```
taller_1/
├── taller_1.Rmd                    # Documento principal (reporte reproducible)
├── taller_1.html                   # Reporte compilado
├── scraper.R                       # Módulo de extracción (scraping)
├── comparador_precios.sqlite       # Base de datos SQLite
├── tabla_comparables.csv           # Respaldo de los 8 productos comparables
├── reglas_emparejamiento.csv       # Reglas de correspondencia de productos
├── README.md                       # Este archivo
├── renv.lock                       # Dependencias con versiones fijas
├── raw/                            # Copias crudas del scraping
│   ├── farmatodo/
│   ├── locatel/
│   └── ortopedicos_futuro/
└── renv/                           # Librería local de renv (no se sube a Git)
```

## Requisitos

- R 4.4 o superior
- RStudio
- Google Chrome (necesario para `read_html_live()` durante el modo en línea)

## Ejecución

### Modo sin conexión (por defecto)

Reproduce el análisis sin consultar los sitios, usando las copias crudas de `raw/`.

1. Abrir el proyecto en RStudio.
2. Restaurar las dependencias con versiones fijas:
   ```r
   renv::restore()
   ```
3. Verificar en el chunk `parametros` de `taller_1.Rmd` que `MODO <- "offline"`.
4. Hacer Knit al documento (Ctrl+Shift+K o botón Knit).

### Modo en línea

Vuelve a ejecutar el scraping consultando los sitios en vivo.

1. Abrir el proyecto en RStudio.
2. Restaurar dependencias con `renv::restore()`.
3. Cambiar en el chunk `parametros` de `taller_1.Rmd` a `MODO <- "online"`.
4. Hacer Knit al documento. Tarda ~20-30 minutos.

**Nota:** el modo en línea usa `read_html_live()` de `rvest`, que abre Chrome
para ejecutar el JavaScript de cada sitio. Es lento pero necesario porque los
tres comercios cargan productos dinámicamente.

## Estrategia de scraping

| Comercio | Plataforma | Contenedor de scroll | Paginación |
|---|---|---|---|
| Ortopédicos Futuro | VTEX | `window.scrollBy(0, 1000)` | `?page=N` (primera sin parámetro) |
| Locatel | VTEX | `window.scrollBy(0, 1000)` | `?page=N` |
| Farmatodo | Angular | `#app-component-router-outlet.scrollBy(0, 500)` | `?pag=N` (primera sin parámetro) |

**Detección del fin de categoría:** la extracción se detiene cuando una página devuelve 0 productos.

**User-Agent:** `TallerMineriaDatos-UNAL/1.0` (bot académico identificable, sin correo personal).

## Revisión de condiciones de uso

Se revisaron los `robots.txt` de los tres comercios antes de extraer datos:

- **Locatel:** prohíbe `/busca`, `/search`, `/checkout`, `/login`. Se usaron solo URLs de categoría.
- **Farmatodo:** prohíbe `/buscar*`, `/busqueda*`, `/checkout`, y bloquea Scrapy 2.7.1 y 2.8.0. Se usó `read_html_live()` en lugar de Scrapy, y se excluyó la subcategoría Muletas porque solo está disponible mediante URL de búsqueda.
- **Ortopédicos Futuro:** prohíbe `/busca/`, `/checkout`, `/login`, y permite explícitamente las URLs con `?page=` y `?fq=`.

## Productos comparables

Se identificaron 8 productos presentes en los 3 comercios:

| # | producto_id | Marca | Presentación |
|---|---|---|---|
| 1 | ACTIMOVE_RODILLA_PATELA_ABIERTA_M | Actimove | Soporte de rodilla patela abierta talla M |
| 2 | KAMEX_RODILLERA_MAGNETICA | Kamex | Rodillera magnética talla universal |
| 3 | NOVARIX_ANTIEMBOLICA_RODILLA_18-22_UNISEX | No-Varix | Media antiembólica a la rodilla 18-22 mmHg unisex |
| 4 | NOVARIX_MEDIA_HOMBRE_15-20 | No-Varix | Media de compresión 15-20 mmHg hombre |
| 5 | NOVARIX_MEDIA_HOMBRE_20-30 | No-Varix | Media de compresión 20-30 mmHg hombre |
| 6 | NOVARIX_MEDIA_MUJER_15-20_DIAMANTE | No-Varix | Media de compresión 15-20 mmHg mujer diseño diamante |
| 7 | NOVARIX_MEDIA_RODILLA_MUJER_8-15 | No-Varix | Media de compresión 8-15 mmHg mujer |
| 8 | RECOVERY_ANILLO_DINAMICO_SMALL | Recovery | Anillo dinámico (rehabilitación de mano) small |

**Correspondencia:** las reglas de emparejamiento están en `reglas_emparejamiento.csv`
y en la tabla `reglas_emparejamiento` de SQLite, con nivel de confianza (alta/media) y notas.

## Exclusión documentada

Se excluyó la subcategoría **Muletas** de Farmatodo porque su `robots.txt` bloquea
las rutas de búsqueda (`/buscar*`) y no existe una URL de categoría formal para ese
término. Las muletas sí se extrajeron de Locatel y Ortopédicos Futuro, donde existen
como categoría. Por eso la ejecución de Farmatodo se registra como **parcial** en la
tabla `ejecuciones`.

## Base de datos SQLite

**Estructura:**

- `comercios` — comercios, URL base, plataforma
- `productos` — catálogo de productos comparables
- `observaciones` — cada precio extraído (con producto_id, comparable, fecha ISO 8601)
- `ejecuciones` — registro de cada corrida del scraper
- `reglas_emparejamiento` — reglas de correspondencia

**Restricción de unicidad:** `(comercio, producto_comercio, fecha_hora)`, para que una
misma observación no se inserte dos veces.

## Reproducibilidad

El documento integra código de scraping (scraper.R), limpieza, creación e inserción
en SQLite, consultas SQL, gráficos e interpretación. Se puede ejecutar de principio
a fin con las instrucciones de arriba.

Las copias crudas en `raw/` permiten verificar la extracción incluso si los sitios
cambian o bloquean el acceso después de la entrega.

## Entrega

Subido al repositorio del curso mediante **Pull Request**, dentro de la carpeta
individual del estudiante. El PR solo modifica archivos dentro de esa carpeta.