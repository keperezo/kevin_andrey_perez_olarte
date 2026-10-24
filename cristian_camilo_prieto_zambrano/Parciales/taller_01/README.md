# Taller 1 — Comparador de precios: Nutrición clínica

**Asignatura:** Minería de Datos (2016325), Universidad Nacional de Colombia.

**Lenguaje:** R. Todo el proceso se hace en R: extracción, limpieza, carga en
SQLite, consultas y gráficos.

**Categoría:** Nutrición clínica. Incluye:

* fórmulas nutricionales completas;
* fórmulas especializadas (diabetes, renal, otras patologías);
* fórmulas clínicas pediátricas;
* módulos de proteína;
* espesantes.

**Comercios:**

* Locatel
* Cruz Verde
* Farmatodo

**Comparables:** 10 presentaciones idénticas, con identificador común P01–P10,
presentes en los tres comercios.

\---

## 1\. Contenido de la carpeta

|Archivo|Contenido|
|-|-|
|`taller\_1.Rmd`|Documento de análisis: fuentes, limpieza, base, consultas SQL, gráficos y conclusiones. Llama las funciones de `scraper.R`; no duplica su código.|
|`taller\_1.html`|El documento compilado.|
|`scraper.R`|Módulo de extracción: una función por comercio, limpieza, carga en SQLite y la función general `ejecutar\_recoleccion()`.|
|`comparador\_precios.sqlite`|Base con las tablas `comercios`, `productos`, `anuncios`, `observaciones` y `ejecuciones`.|
|`correspondencia.csv`|Asignación del identificador común (P01–P10) al SKU de cada comercio.|
|`raw/<comercio>/<corrida>/`|Copias crudas de las respuestas (`.json.gz`) y el total que informa cada sitio (`totales.csv`).|
|`renv.lock`|Versiones fijas de los paquetes.|

## 2\. Cómo ejecutar

El directorio de trabajo debe ser esta carpeta.

### Instalar las dependencias

```r
install.packages("renv")
renv::restore()
```

### Recolección en línea

Hace una corrida nueva en los tres comercios. Las observaciones se agregan a la base
sin sobrescribir las anteriores. Tarda unos 2–3 minutos, por las pausas entre
solicitudes.

```r
source("scraper.R")
ejecutar\_recoleccion()
```

Para un solo comercio:

```r
scrape\_locatel("20261010T213019")
scrape\_cruzverde("20261010T213019")
scrape\_farmatodo("20261010T213019")
```

### Recolección sin conexión

Reprocesa las copias crudas de una corrida sin consultar los sitios. Para no
modificar la base entregada, se puede usar otra base:

```r
source("scraper.R")
ejecutar\_recoleccion(db = "prueba\_offline.sqlite", offline = TRUE,
                     corrida = "20261010T213019")
```

### Documento

```r
rmarkdown::render("taller\_1.Rmd")                                     # analiza la base entregada
rmarkdown::render("taller\_1.Rmd", params = list(recolectar = TRUE))   # nueva corrida y análisis
rmarkdown::render("taller\_1.Rmd", params = list(recolectar = TRUE, offline = TRUE,
                                                corrida = "20261010T213019"))
```

Se recomienda compilar en una sesión de R limpia (*Session → Restart R*) o con el
botón **Knit**.

\---

## 3\. Selección de los comercios

Se buscaron comercios donde un consumidor en Colombia compra fórmulas nutricionales
de forma habitual. Estos productos se venden sobre todo en droguerías y tiendas de
salud, no en supermercados. Los tres elegidos:

* son cadenas de droguerías o tiendas de salud reconocidas, con presencia nacional
y venta en línea;
* tienen una sección propia de nutrición;
* manejan las mismas marcas clínicas (Abbott, Nestlé, Fresenius, Boydorr), lo que
permite encontrar presentaciones idénticas en los tres.

|Comercio|Perfil|Plataforma del sitio|
|-|-|-|
|**Locatel**|Cadena de tiendas de salud y bienestar. Tiene una sección amplia de *Alimentos nutricionales*.|VTEX|
|**Cruz Verde**|Cadena de droguerías. Tiene una sección de *Nutrición* dentro de *Bienestar y Nutrición*.|Salesforce Commerce Cloud, con API propia|
|**Farmatodo**|Cadena de farmacias de autoservicio. Tiene una sección de *Nutrición* dentro de *Bienestar y nutrición deportiva*.|Angular, con buscador Algolia|

**Comercio descartado: La Rebaja Virtual.** Se evaluó como tercer comercio, con la
misma plataforma VTEX de Locatel. Su sección de nutrición especializada tiene 80
productos, frente a 243 de Cruz Verde, y no tenía todos los productos candidatos en
una misma sección. Cruz Verde tenía los siete candidatos iniciales en su sección
*Nutrición*.

## 4\. Secciones consideradas equivalentes a la categoría

|Comercio|Sección del sitio|Filtro o identificador|Total del sitio (2026-10-10)|
|-|-|-|-|
|Locatel|Alimentos nutricionales > Suplementos|`fq=C:/6/41/`|383|
|Locatel|Alimentos nutricionales > Cereales > Regulares|`fq=C:/6/40/251/`|11|
|Cruz Verde|Bienestar y Nutrición > Nutrición|`cgid=bienynut-nutricion`, zona `COCV\_zona64`|243|
|Farmatodo|Bienestar y nutrición deportiva > Nutrición|`departments:"Bienestar y nutrición deportiva" AND categorie:"Nutrición" AND status:"A"`|166|

* **Locatel, *Suplementos* completo.** Glucerna polvo 400 g está en la subsección
*Complementos* y no en *Nutricionales*, así que se recolecta toda la sección.
* **Locatel, *Cereales > Regulares*.** Locatel clasifica allí Ensure Advance
líquido. Los cereales de esa sección se recolectan, pero se marcan como excluidos.
* **Farmatodo.** Se verificó que los productos que muestra la página
`/categorias/bienestar-y-nutricion-deportiva/nutricion` están todos dentro del
filtro. Solo se toman productos activos (`status:"A"`); el índice también contiene
productos descontinuados.
* **Lo que no pertenece a la categoría** según la delimitación de la planilla
(nutrición deportiva, vitaminas, cuidado personal, cereales) se recolecta igual y
se marca como excluido, con su motivo (sección 7).

## 5\. Estrategia de extracción

Las tres tiendas arman el listado de productos con JavaScript: `read\_html()` devuelve
la página sin productos. Por eso se consulta el servicio JSON que usa la propia
página para mostrar la categoría. Ese servicio se identificó en la pestaña *Red* de
las herramientas de desarrollador del navegador, al cargar la categoría. Las
peticiones son HTTP directas con `httr2`, sin automatizar el navegador.

|Comercio|Servicio consultado|Paginación|Total para verificar|
|-|-|-|-|
|Locatel|`GET https://www.locatelcolombia.com/api/catalog\_system/pub/products/search?fq=<filtro>\&\_from=<n>\&\_to=<n+49>`|50 por página|encabezado HTTP `resources` (ej. `0-49/383`)|
|Cruz Verde|`GET https://api.cruzverde.com.co/product-service/products/search?limit=48\&offset=<n>\&refine\[]=cgid=bienynut-nutricion\&inventoryId=COCV\_zona64`|48 por página|campo `total`|
|Farmatodo|`POST https://vcojeyd2po-dsn.algolia.net/1/indexes/products-colombia/query` con el filtro de la sección|100 por página|campo `nbHits`|

**Detalles por comercio:**

* **Cruz Verde:**

  * Primero abre una sesión de invitado (`POST /customer-service/login` con cuerpo
`{}`); sin ella el servicio responde 401.
  * La descripción de los productos se pide aparte a `product-summary`, con los 48
identificadores de cada página en una sola consulta.
  * El listado y los precios dependen de la zona de inventario (`COCV\_zona64`).
* **Farmatodo:** la aplicación (`VCOJEYD2PO`), el índice y la clave de búsqueda
están en el código JavaScript que carga la página. Esa clave es **pública y de
solo búsqueda**: la reciben todos los visitantes del sitio, no es una contraseña
ni una credencial privada.

**Buenas prácticas:**

* user-agent de navegador e idioma `es-CO`;
* pausa de 1,5 s entre solicitudes;
* tiempo máximo de 30 s y hasta 3 reintentos;
* `tryCatch` y `possibly()`: si un comercio falla, los demás continúan y la ejecución
queda registrada como fallida o parcial.

**Condiciones de uso:**

* Los `robots.txt` de Cruz Verde y Farmatodo prohíben las páginas de búsqueda del
sitio web (`/search`), no las páginas de categoría ni los servicios que las
alimentan. Solo se consulta la categoría.
* La cookie de sesión de Cruz Verde se guarda en un archivo temporal que se borra al
terminar.
* No se sube al repositorio ninguna contraseña, token ni credencial.

### Verificación de completitud (corrida `20261010T213019`)

|Comercio|Obtenidos|Total del sitio|Estado|
|-|-|-|-|
|Locatel|394|394 (383 + 11)|completa|
|Cruz Verde|243|243|completa|
|Farmatodo|166|166|completa|

## 6\. Organización del código (`scraper.R`)

|Sección|Contenido|
|-|-|
|1–4|Paquetes, configuración, funciones auxiliares (extracción de contenido y unidades), copias crudas y solicitud HTTP con manejo de errores|
|5–8|Funciones por comercio: `scrape\_locatel()`, `scrape\_cruzverde()`, `scrape\_farmatodo()`. Todas reciben `(corrida, offline)` y devuelven las mismas 18 columnas.|
|9|Limpieza: `limpiar()`|
|10|Base SQLite: `crear\_base()`, `insertar\_lote()`, `aplicar\_correspondencia()`, `verificar\_correspondencia()`|
|11|Función general: `ejecutar\_recoleccion()`|

**`ejecutar\_recoleccion()`** hace todo el proceso con una sola llamada:

1. Recolecta los tres comercios con el mismo identificador de corrida.
2. Limpia los datos.
3. Registra cada ejecución con su estado.
4. Inserta en SQLite sin sobrescribir.
5. Aplica la correspondencia y la verifica.

## 7\. Información recolectada y limpieza

### Campos por anuncio

Cada anuncio tiene:

* comercio, sección, SKU y EAN;
* nombre, marca y subcategoría tal como los publica el sitio;
* contenido y unidad;
* número de unidades;
* precio, precio de lista, precio Club (solo Cruz Verde) e indicador de promoción;
* disponibilidad;
* URL del producto;
* texto original del anuncio (nombre y descripción, sin limpiar);
* fecha y hora de consulta en ISO 8601, hora de Colombia (ej. `2026-10-10T21:30:19-05:00`);
* identificador de la ejecución.

### Reglas de limpieza

Las versiones estandarizadas se guardan en columnas propias. El texto original no
se modifica.

1. **Precios.** Los servicios entregan precios numéricos en COP. Un precio 0 o
ausente se guarda como `NULL`, nunca como 0.

   * El precio comparado es el de oferta abierta a todos si existe; si no, el de
lista.
   * El precio Club de Cruz Verde exige afiliación: se guarda en `precio\_club` y no
se compara.
2. **Contenido.** Se extrae del nombre y se normaliza a g o mL: 1 kg = 1000 g,
1 L = 1000 mL, 1 lb = 453,592 g, 1 oz = 28,35 g. Reconoce "400G", "400 gr",
"400 gramos", "220ml", "1 Lt".
3. **Unidades.** Se toman de expresiones como "x 6 und", "Caja por 6 unidades" o
"x 6 Tetrapack".
4. **Marca (`marca\_std`).** Se busca una marca conocida en el nombre. Si no hay, se
usa la marca del sitio. Motivo: el mismo producto llega como "Glucerna",
"GLUCERNA" o "Abbott" según la tienda.
5. **Subcategoría (`subcategoria\_std`).** Se asigna por reglas sobre el nombre, en
este orden:

   1. espesante;
   2. fórmula pediátrica;
   3. fórmula para diabetes;
   4. fórmula renal;
   5. fórmula especializada (otra patología);
   6. fórmula completa adulto;
   7. módulo de proteína.

   Las fórmulas completas van antes que los módulos: "Ensure… con Proteína" es una
fórmula completa.

6. **Paquetes (`es\_paquete`).** Ofertas compuestas ("850 g + 400 g", "x 6", "pack",
"kit", "lleve"). No se emparejan con la presentación individual.
7. **Duplicados.** Un mismo SKU puede aparecer en dos secciones; se conserva uno por
comercio, con `UNIQUE(id\_comercio, sku)`.

### Exclusiones

Se marcan con `excluido = 1` y `motivo\_exclusion`; no se borran.

* **Criterio.** Un anuncio pertenece a la categoría si su nombre contiene una marca o
un término de nutrición clínica: Ensure, Glucerna, PediaSure, ProWhey, Diben,
Nutren, Fresubin, Prosure, Oncare, Nepro, Gerifood, Casilan, "fórmula nutricional",
"complemento nutricional", etc.
* **Motivos de exclusión** para lo demás:

  * nutrición deportiva (creatina, gainer, BCAA, proteína *whey* deportiva);
  * vitaminas o suplemento dietario (multivitamínicos, colágeno, omega, cápsulas,
tabletas);
  * medicamento o producto de cuidado personal;
  * leche o fórmula infantil de consumo general;
  * cereal o alimento regular (sección *Cereales* de Locatel);
  * dispositivo médico (jeringas, sondas);
  * otro alimento o suplemento fuera de la delimitación.
* **Por qué lista blanca y no lista negra.** En la primera versión se excluía por
lista negra (creatina, vitaminas…). En la sección *Nutrición* de Cruz Verde
quedaban lubricantes, cremas, anticonceptivos y multivitamínicos. La lista blanca
corrige eso.

**Resultado (corrida `20261010T213019`):**

|Comercio|Recolectados|En la categoría|Excluidos|
|-|-|-|-|
|Locatel|394|115|279|
|Cruz Verde|243|47|196|
|Farmatodo|166|92|74|

### Correspondencia de productos

`correspondencia.csv` asigna el identificador común al SKU de cada comercio.
`aplicar\_correspondencia()` lo carga en la tabla `productos` y marca
`anuncios.comparable = 1`.

**Verificación.** `verificar\_correspondencia()` compara cada anuncio con su producto
en marca, contenido, unidad y número de unidades. Las 30 asignaciones coinciden. La
variante (sabor vainilla) se revisó en el nombre de cada anuncio.

**No se usa el EAN.** No coincide entre tiendas para el mismo producto; por ejemplo,
Glucerna 400 g tiene códigos distintos en Locatel y en Farmatodo. Además, Cruz Verde
no lo publica.

|Id|Presentación|Locatel|Cruz Verde|Farmatodo|
|-|-|-|-|-|
|P01|Ensure Advance vainilla polvo 400 g|15544|COCV\_111380|1043564|
|P02|Ensure Advance vainilla líquido 220 mL|35669|COCV\_402006|232300088|
|P03|Glucerna vainilla polvo 400 g|15541|COCV\_142214|1026565|
|P04|Glucerna vainilla líquido 237 mL|19624|COCV\_128687|1050402|
|P05|ProWhey DM vainilla polvo 400 g|18594|COCV\_125728|106811741|
|P06|ProWhey DM vainilla líquido 200 mL|61184|COCV\_575793|241300207|
|P07|Diben Drink vainilla 200 mL|23574|COCV\_155958|108112207|
|P08|Ensure Advance vainilla polvo 850 g|19359|COCV\_129816|1050596|
|P09|ProWhey Net vainilla polvo 434 g|20808|COCV\_134562|106811821|
|P10|ProWhey DM vainilla polvo 850 g|26616|COCV\_164643|204700084|

**Criterios de selección:**

* **Origen.** Los comparables se eligieron después de extraer la categoría completa,
entre productos presentes en la sección recolectada de los tres comercios.
* **Por qué diez.** Se marcan diez y no ocho para tener respaldo si en una corrida
falta alguno.
* **Pares de tamaños.** P01/P08 y P05/P10 permiten comparar el precio por gramo
entre tamaños del mismo producto.

**Candidatos descartados:**

* **ProWhey Net líquido.** En Locatel está en *Droguería > Medicina alternativa*,
fuera de las secciones recolectadas.
* **PediaSure polvo 400 g.** En Cruz Verde está en *Bebé y Maternidad*, fuera de la
sección *Nutrición*.
* **ProWhey Bariatric.** En Farmatodo está en *Multivitamínicos*.

## 8\. Base de datos SQLite

|Tabla|Contenido|Restricciones|
|-|-|-|
|`comercios`|id, nombre, URL base|PK `id\_comercio`|
|`ejecuciones`|corrida, comercio, inicio, fin, n.º de registros, total del sitio, estado (`completa` / `parcial` / `fallida`), modo (`en\_linea` / `sin\_conexion`)|PK `id\_ejecucion`|
|`productos`|identificador común P01–P10 con marca, variante, contenido, unidad y n.º de unidades|PK `id\_producto`|
|`anuncios`|cada producto en cada comercio: SKU, sección, EAN, texto original, URL, campos estandarizados, `es\_paquete`, `excluido`, `motivo\_exclusion`, `id\_producto`, `comparable`|`UNIQUE(id\_comercio, sku)`|
|`observaciones`|precio, precio de lista, precio Club, promoción, disponibilidad, fecha y hora de consulta, ejecución|`UNIQUE(id\_anuncio, fecha\_hora\_consulta)`|

* **Estado de la ejecución:**

  * `fallida`: 0 registros.
  * `parcial`: menos registros que el total que informa el sitio.
  * `completa`: en los demás casos.
* **Sin sobrescritura:** una nueva corrida agrega observaciones. Un anuncio existente
actualiza sus datos descriptivos, pero conserva su id y su correspondencia. Una
observación repetida (mismo anuncio y misma hora de consulta) se ignora.

## 9\. Copias crudas y modo sin conexión

* **Ruta:** `raw/<comercio>/<corrida>/<sección>\_p<página>\_\_<fecha-hora>.json.gz`.

  * En Cruz Verde, cada página tiene dos archivos: `\_busqueda` (listado y precios) y
`\_resumen` (descripciones).
  * Las respuestas se guardan comprimidas, antes de procesarlas.
* **Hora de consulta.** Va en el nombre del archivo; sin conexión se conserva la hora
original, no la del reprocesamiento.
* **`totales.csv`.** Guarda el total que informa cada sitio por sección, para repetir
la verificación de completitud sin conexión. En Locatel el total solo llega en un
encabezado HTTP, que no queda en el JSON.

## 10\. Declaración de uso de inteligencia artificial

En este taller se usó **Claude** (Anthropic), un asistente de inteligencia
artificial.

**Uso principal: el código de extracción (`scraper.R`).**

* Identificar los servicios JSON de cada comercio.
* Escribir las funciones de descarga y lectura de cada sitio.
* Manejar la sesión de Cruz Verde y la paginación.
* Escribir las funciones de limpieza y de carga en SQLite.

**Apoyo adicional:**

* la estructura y redacción de `taller\_1.Rmd`;
* la depuración de errores durante la ejecución.

## 11\. Limitaciones

* **Momento y lugar.** Los precios corresponden al momento de la captura y, en Cruz
Verde, a la zona de inventario `COCV\_zona64`.
* **Precios condicionados.** No se comparan el precio Club, los descuentos por
cantidad ni el costo de envío.
* **Delimitación.** Se hizo por reglas sobre el nombre; un nombre poco informativo
puede quedar mal clasificado. Los 30 anuncios comparables sí se verificaron uno a
uno.
* **Comparación.** Solo se compara cada presentación consigo misma. Un menor precio
no indica que una fórmula pueda reemplazar a otra.

