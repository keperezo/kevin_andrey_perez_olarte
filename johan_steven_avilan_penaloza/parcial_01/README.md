# Taller 1 — Comparador de precios: Primeros auxilios y curación

**Autor:** Johan Steven Avilan Peñaloza  
**Curso:** Minería de Datos  
**Lenguaje elegido:** R (extracción, limpieza, carga en SQLite, consultas y gráficos)  

---

## Qué hace este proyecto

Extrae productos de la categoría *Primeros auxilios y curación* (gasas, vendas, apósitos, curitas, esparadrapos, antisépticos para heridas y kits de curación) de tres comercios en línea de Colombia, los almacena en una base de datos SQLite y compara el precio de ocho productos comunes a los tres comercios.

| Comercio | Sección consultada | Técnica de extracción |
|---|---|---|
| **Farmatodo** | `/categorias/salud-y-medicamentos/botiquin-y-primeros-auxilios` (subcategorías: curitas/algodones/adhesivos, antisépticos, vendas, compresas) | Chrome automatizado con `rvest::read_html_live()` (interacción con botón "Cargar más") |
| **La Rebaja** | `/drogueria/cuidado-de-la-herida` | Servicio API JSON de VTEX (`/api/catalog_system/pub/products/search`) |
| **Locatel** | `/drogueria/cuidado-heridas` | Servicio API JSON de VTEX (`/api/catalog_system/pub/products/search`) |


## Estructura de archivos

```text
parcial_01/
├── taller_1.qmd                # Informe reproducible (Quarto)
├── taller_1.html               # Informe renderizado interactivo
├── scraper.R                   # Módulo de extracción, limpieza, SQLite y correspondencia
├── comparables.csv             # Correspondencia de los 8 productos (generada por código)
├── comparador_precios.sqlite   # Base de datos SQLite consolidadora
├── raw/                        # Copias crudas por comercio y fecha de captura
│   ├── farmatodo/AAAA-MM-DD/*.html.gz
│   ├── la_rebaja/AAAA-MM-DD/pag_*.json(.gz)
│   └── locatel/AAAA-MM-DD/pag_*.json(.gz)
├── prueba_colab.ipynb          # Cuaderno para validación en Google Colab
├── renv.lock                   # Archivo de congelamiento de dependencias
└── README.md                   # Documentación del proyecto
```

---

---

## Prerrequisitos e instalación

1. **Entorno R:** requiere R v4.1 o superior.
3. **Navegador web:** Google Chrome, requerido únicamente para la captura en vivo de Farmatodo mediante `chromote`.

---

## Cómo descargar y cargar la carpeta del parcial

[Haz clic aquí para descargar la carpeta del parcial](https://download-directory.github.io/?url=https://github.com/JohanAv1018/dm_2016325_2026_2/tree/main/johan_steven_avilan_penaloza/parcial_01).

Una vez descargado el archivo `.zip`:

1. Descomprime la carpeta en tu equipo.
2. Abre **RStudio**.
3. En el menú superior, selecciona **Session → Set Working Directory → Choose Directory...**.
4. Busca y selecciona la carpeta `parcial_01` que descomprimiste.
5. Haz clic en **Open**.

## Preparación del entorno (Instalar paquetes)

1. En el panel **Files** de RStudio, busca el archivo `taller_1.qmd` y ábrelo.
2. En la pestaña **Console** (abajo a la izquierda), ejecuta:

   ```r
   renv::restore()
   ```

3. Si aparece el mensaje indicando que el proyecto aún no ha sido activado (*"It looks like you've called renv::restore() in a project that hasn't been activated yet"*), selecciona la opción **1: Activate the project and use the project library**. Para ello, escribe `1` en la consola y presiona **Enter**.

4. Si `renv` solicita confirmación para instalar los paquetes, acepta y espera a que termine el proceso.

5. Una vez finalizada la instalación, puedes continuar con las instrucciones de ejecución del proyecto.

**Nota:** la primera instalación puede tardar varios minutos, ya que se deben descargar e instalar las dependencias registradas en `renv.lock`. Se recomienda tener conexión a internet durante este paso.
## Instrucciones de ejecución

Este proyecto permite trabajar de dos formas desde **RStudio**:

- **Modo Offline (predeterminado):** no requiere conexión a internet ni Google Chrome. Procesa y analiza los datos crudos previamente descargados en la carpeta `raw/`.
- **Modo Online:** se conecta a los tres comercios, realiza el scraping en vivo, guarda copias de los datos en `raw/` y actualiza la base de datos SQLite.

### Opción 1: Modo Offline (Ver informe rápido)

1. Abre el archivo `taller_1.qmd` en RStudio.
2. Haz clic en el botón **Render**, ubicado en la barra superior del editor.
3. Se reconstruirá la base de datos a partir de los archivos de `raw/`, si no existe, y se generará el informe `taller_1.html`.

### Opción 2: Modo Online (Recolección de datos en vivo)

#### Paso A. Descargar datos actualizados

En la consola de RStudio, ejecuta:

```r
source("scraper.R")
ejecutar_recoleccion_completa(".", modo = "online")
```

Espera a que termine la recolección de datos de los tres comercios. Los archivos capturados se guardarán en `raw/` y las nuevas observaciones se incorporarán a la base de datos SQLite.

#### Paso B. Generar el informe con los nuevos datos

1. Abre `taller_1.qmd` en RStudio.
2. Haz clic en **Render** para generar el informe `taller_1.html` con los datos actualizados.

**Nota:** la recolección Online y la generación del informe son pasos independientes. Para consultar los datos ya almacenados sin realizar nuevas peticiones web, utiliza el modo Offline.

## Decisiones de diseño y arquitectura

- **Estandarización limpia:** la marca (`anuncios.marca`), el nombre (`nombre_estandar`), la subcategoría y las unidades de medida se limpian en campos dedicados. El texto original capturado se conserva intacto en columnas adyacentes.
- **Manejo de productos agotados:** los artículos sin stock conservan su registro, pero asignando `precio = NULL` (nunca cero) para evitar sesgar las métricas de precio.
- **Filtros de exclusión:** reglas explícitas descartan artículos fuera de alcance (guantes, tapabocas, jeringas, ostomías, etc.), registrando la razón en `anuncios.motivo_exclusion`.
- **Mapeo de comparables:** la vinculación de los ocho productos representativos utiliza el código de barras (EAN) para las tiendas VTEX (La Rebaja y Locatel) y expresiones regulares sobre el nombre comercial para Farmatodo.
- **Identificación única:** un producto es único por `(comercio, SKU)`, mientras que una observación es única por `(anuncio, fecha_captura)`.

## Consultas y análisis de datos

Las analíticas incluidas en el informe combinan sentencias SQL avanzadas (`INNER/LEFT JOIN`, subconsultas, `WITH` / CTEs encadenadas y funciones de ventana como `RANK() OVER` y `LAG`), ejecutadas sobre la base de datos SQLite mediante `DBI::dbGetQuery()`.

## Cumplimiento ético y límites

- **Frecuencia de peticiones:** pausas programadas entre peticiones HTTP (1 segundo en las API de VTEX y varios segundos durante la interacción con el DOM de Farmatodo).
- **Identificación de cliente:** reintentos limitados y uso de un *User-Agent* académico explícito.
- **Revisión de `robots.txt`:** se verificaron las políticas de scraping de los tres comercios, confirmando el acceso permitido a las rutas públicas de consulta.