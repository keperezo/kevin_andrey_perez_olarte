# Fase 1 · Selección de comercios y reconocimiento (categoría: Perfumería)

**Revisiones:**
- **2026-10-08:** reconocimiento con un lector web sin JavaScript, la API pública de VTEX y búsquedas web.
- **2026-10-09:** verificación en Chrome sobre el HTML real y las peticiones de red de cada sitio, y revisión de las condiciones de uso.
- **2026-10-10:** segundo reconocimiento completo para diseñar `scraper.R`: estructuras exactas, orden de Falabella, filtro de Farmatodo y candidatos a comparables.

**Alcance:** esto es reconocimiento para elegir comercios, diseñar la extracción y escribir las reglas de correspondencia. **Ningún dato de esta nota entra al análisis**: los datos del taller salen de las corridas de `scraper.R` (sección 2.4 del enunciado).

---

## 1. Decisión

**Trío: La Riviera + Falabella + Farmatodo.** Fedco queda de respaldo.

| Comercio | Cómo se extrae | Tamaño de la categoría | robots.txt |
|---|---|---|---|
| La Riviera | HTML del servidor (Wix) con rvest; `?page=1…26` (+ la 27 vacía) | **774** productos (2026-10-09) | Permisivo |
| Falabella | JSON `__NEXT_DATA__` incrustado en el HTML; dos recorridos ordenados por precio | **3.003** según el sitio (2026-10-09) | Admite a todos los robots |
| Farmatodo | Servicio interno de búsqueda (POST JSON, sin clave), por subcategoría y rango de precio | **485** visibles en el sitio (1.072 si se cuentan los agotados) | Categorías permitidas; la API no tiene reglas |

Las tres fuentes se extraen con peticiones HTTP directas (httr2), sin automatizar el navegador, como recomienda la sección 3.1 del enunciado. Esa sección también acepta expresamente "la consulta de los servicios internos que el propio sitio usa", que es el caso de Falabella y Farmatodo.

---

## 2. Estrategia de extracción verificada

### La Riviera
- **URL:** `https://www.lariviera.co/fragancias?page=N`. Trae 30 productos por página (la 26 tiene 24) y la 27 sale vacía. El sitio no muestra un total, así que el fin se detecta con la primera página vacía.
- **Estructura de cada tarjeta** (`<div data-hook="product-item-root" data-slug="…">`):

  | Campo | Selector |
  |---|---|
  | id | atributo `data-slug` de la tarjeta (único: 774 de 774) |
  | nombre | `[data-hook="product-item-name"]` (`<p>`) |
  | precio a pagar | `[data-hook="product-item-price-to-pay"]` (`$ 460.000`, con espacio no separable) |
  | precio antes de descuento | `[data-hook="product-item-price-before-discount"]` (41 de 774) |
  | etiqueta | `[data-hook="RibbonDataHook.RibbonOnImage"]` (165 de 774) |
  | enlace | `[data-hook="product-item-product-details-link"]` (URL absoluta) |

- **Etiquetas observadas:**
  - Producto 2X1: 75
  - NUEVO EXCLUSIVO: 24
  - Novedad: 15
  - Descuentos "NN% OFF": 41
  - Top Seller: 7
  - NUEVO: 2
  - PRECIO ESPECIAL: 1
- **Nombre:** casi siempre `MARCA, LÍNEA [CONC], CÓDIGO, TAMAÑO` (763 de 774 tienen 4 partes).
  - A veces la marca va en segundo lugar, por ejemplo `TOUS L'EAU EDP SP, TOUS, …`.
  - A veces falta la concentración, por ejemplo `Armaf, Yum Yum, …`.
  - El código (`V115-016`, `LATT-003`…) sirve como SKU.
- **Venta en línea:** el pedido se hace en la web y luego un asesor se pone en contacto. El pago es contra entrega o por enlace, con envío a todo el país excepto San Andrés.

### Falabella
- **URL:** `…/falabella-co/category/cat1461053/Perfumes?page=N&sortBy=derived.price.search,asc` (y `desc`).
- **JSON:** `props.pageProps.results` trae 48 productos por página y `props.pageProps.pagination` trae `count` y `perPage`.
- **Campos usados:**
  - identificación: `skuId`, `productId`, `displayName`, `brand`, `sellerName`, `url`;
  - publicidad y descuentos: `isSponsored`, `discountBadge`;
  - `prices`: una lista con `type`, `crossed` y `price`.
- **Hallazgo clave: el orden por defecto no sirve para recorrer la categoría.**
  - Al recorrer las 63 páginas con el orden por defecto se obtuvieron 3.003 posiciones, pero solo 2.259 productos distintos: 653 productos aparecían 2 o 3 veces, en páginas lejanas entre sí.
  - Con el orden por precio, en 21 páginas solo hubo 14 repetidos (todos en páginas contiguas, por empates de precio) y aparecieron 258 productos que el recorrido por defecto no había mostrado.
  - Decisión: dos recorridos (precio ascendente y descendente) y unión por `skuId`.
  - Con el orden por precio, la página no incluye resultados patrocinados.
- **Tipos de precio** (página 1, por defecto):
  - `eventPrice`: 23
  - `internetPrice`: 33
  - `normalPrice` tachado: 53
  - `cmrPrice`: 4
- **Asignación de precios:**
  - Precio = el menor entre `eventPrice` e `internetPrice`.
  - Precio antes = el tachado.
  - `cmrPrice` se guarda como precio condicionado, porque requiere la tarjeta CMR.
- **Vendedores:** parte de la oferta es de *marketplace* (Paris Success, Bellaplus, Amber Oud…). Falabella misma y un vendedor a veces publican el mismo perfume con tamaños distintos, por ejemplo Club de Nuit Iconic "100 ml" frente a "105ml".
- **Peso:** 1,8–2,3 MB por página (unos 260 KB comprimida). Dos recorridos dan unas 126 páginas por corrida.
- **Carga:** al hacer muchas peticiones seguidas desde el navegador, las respuestas empezaron a tardar. Por eso el scraper usa 2 s de pausa y reintentos.

### Farmatodo
- **Servicio:** `POST https://api-search.farmatodo.com/1/indexes/*/queries` con cuerpo `{"requests":[{"indexName":"products-colombia","params":"query=&filters=…&hitsPerPage=100&page=0"}]}`.
- **Cabeceras:** `content-type: application/json` y `x-custom-city: BOG`. No requiere clave. `scraper.R` envía además el origen y la referencia del sitio, igual que la página.
- **Tope:** máximo 80 resultados por consulta (`nbHits` también llega en 80). Por eso se parte por `subCategory` (Hombre, Mujer, Fragancias y "otras") y por rangos de `fullPrice` divididos a la mitad.
- **Filtro de la página:** `outofstore:false AND classification.id:211 AND NOT requirePrescription:true AND NOT rms_class:SAMPLING`. Así se extraen los productos que el sitio muestra. Los `outofstore:true` son agotados y la página no los lista.
  - Con el filtro: 485 productos (Hombre 195, Mujer 290).
  - Sin el filtro: 1.072, de los cuales 113 no tienen subcategoría. La partición del 2026-10-09 no los contaba.
- **Campos:**
  - el nombre está en `mediaDescription` (no hay `description`);
  - la marca en `marca` (`brand` es un código interno) y el EAN en `barcode`;
  - precios en `fullPrice` (igual a `fullPriceByCity` de BOG), `offerPrice` y `primePrice` (los dos últimos en 0 el 10-10);
  - además `subCategory`, `outofstore` y `url`. La ficha es `https://www.farmatodo.com.co/producto/<url>`.
- **`hasStock`:** es `false` en todos sin una tienda seleccionada, así que no se usa.
- **Registros a excluir:**
  - productos de prueba ("Producto prueba", "Test");
  - body spray (clasificación interna "CREMA/LOCION CORPORAL");
  - combos;
  - perfume capilar y aceites.

---

## 3. Condiciones de uso (revisión del 2026-10-09)

| Comercio | Términos y condiciones | Otros documentos |
|---|---|---|
| La Riviera | Los contenidos son de La Riviera; se prohíbe su reproducción o almacenamiento. **No menciona robots ni extracción automática.** | `/legales` no trata el tema |
| Falabella | Sección 23 (propiedad intelectual): incluye "compilaciones de datos"; no pueden reproducirse sin consentimiento escrito. **No menciona robots.** | — |
| Farmatodo | Sección 5: prohíbe afectar el funcionamiento del sitio. Sección 11: contenidos protegidos. Sección 16: precios en línea distintos a tiendas. **No menciona robots ni scraping.** | `llms.txt`: uso informativo con atribución; "El scraping intensivo no está autorizado"; respetar robots.txt |

**Mitigación aplicada en `scraper.R`:**
- uso académico y bajo volumen (unas 170 peticiones por corrida, con pausas);
- User-Agent que identifica el proyecto, sin inicio de sesión ni evasión;
- modo sin conexión para no repetir consultas;
- atribución de las fuentes en el documento.

---

## 4. Presentaciones comparables (catálogo)

Con el filtro de la página de Farmatodo, las 7 coincidencias del 2026-10-09 se reducen a 5, porque Odyssey Spectra y Checkmate King estaban agotados. El reconocimiento del 10-10 encontró 24 presentaciones con marca, línea, concentración y tamaño explícitos en los tres comercios. Esas 24 forman `datos/catalogo_comparables.csv`:

| Marca | Presentaciones |
|---|---|
| Versace | Eros EDT 100, Eros Parfum 100, Pour Homme EDT 100, Yellow Diamond EDT 90, Dylan Blue pour Femme EDP 100, Crystal Noir EDT 90 |
| Armaf | Odyssey Limoni EDP 100, Odyssey Candee EDP 100, Club de Nuit Iconic EDP 105 |
| Tommy Hilfiger | Impact EDT 100, New York EDT 100 |
| Moschino | Toy Boy EDP 100, Toy 2 EDP 100 |
| French Avenue | Azzure Aoud EDP 100, Divin Aoud EDP 100, Spectre Ghost EDP 80 |
| Ahmed Al Maghribi | Joud EDP 100, Hirfah EDP 75, Oud Couture Extrait 100 |
| Afnan | Kiaana Angel EDP 100, 9 AM Dive EDP 100 |
| Swiss Arabian | Shaghaf Oud Aswad EDP 75 |
| Rayhaan | Ocean Rush EDP 100 |
| Reyane Tradition | Insurrection II Pure EDT 90 |

Además hay dos familias de tamaños para el precio por mL:
- **Versace Eros EDP:** 100 ml (La Riviera y Falabella) y 200 ml (Falabella).
- **Dolce & Gabbana Light Blue pour Homme EDT, nueva versión:** 100 ml (Falabella y Farmatodo) y 200 ml (La Riviera).

**Casos que no se comparan (regla estricta):**
- Sin concentración en el nombre de La Riviera: Yum Yum, Club de Nuit Intense Woman, Eau Fraîche Extreme, Mandarin Sky Elixir, Rasasi Aliza y Lattafa Asad.
- Tamaños distintos: Club de Nuit Imperiale y Untold (La Riviera y Farmatodo 105 ml, Falabella 100 ml).
- Concentración distinta: Bright Crystal 90 ml (Farmatodo dice EDP; La Riviera y Falabella, EDT o Parfum).
- Sets: los Eros, Toy 2 y Bright Crystal de La Riviera.

---

## 5. Candidatos descartados y respaldo

| Comercio | Motivo |
|---|---|
| Fedco (respaldo) | VTEX IO; su robots.txt prohíbe toda URL con `?` (`Disallow: /*?`), incluida la paginación y la API |
| Éxito | Productos armados con JavaScript; robots.txt `Disallow: /api/` |
| Cruz Verde | Aplicación de una sola página con HTML vacío; sin evidencia de las marcas compartidas |
| Jumbo | robots.txt ilegible |
| La Rebaja | Categoría PERFUMES con 1 producto |
| Locatel | Subcategoría "Perfume" vacía |

---

## 6. Decisiones tomadas para la Fase 2

1. **Farmatodo:** el mismo filtro de la página (solo visibles). Precios de Bogotá. Prime como precio condicionado.
2. **Falabella:** dos recorridos por precio. Precio = el menor entre `eventPrice` e `internetPrice`; tachado = precio antes; CMR = condicionado; patrocinados descartados.
3. **La Riviera:** promoción si hay precio tachado o etiqueta OFF, 2X1 o "precio especial". El 2X1 no se descuenta del precio.
4. **User-Agent:** `Mozilla/5.0 (compatible; TallerMineriaDatosUNAL/1.0; +https://github.com/afflorezr/dm_2016325)`.
5. **Completitud:** cada comercio tiene un control propio (página vacía verificada en La Riviera, total del sitio en Falabella, tope de 80 resultados en Farmatodo). Una respuesta inesperada marca la página como fallida y la corrida como parcial, sin detener la recolección.

---

## 7. Fuentes
- robots.txt: https://www.lariviera.co/robots.txt · https://www.falabella.com.co/robots.txt · https://www.farmatodo.com.co/robots.txt · https://api-search.farmatodo.com/robots.txt · https://www.fedco.com.co/robots.txt · https://www.exito.com/robots.txt
- Términos: https://www.lariviera.co/terminos-y-condiciones · https://www.falabella.com.co/falabella-co/page/terminos-y-condiciones · https://www.farmatodo.com.co/politicas/terminos-y-condiciones · https://www.farmatodo.com.co/llms.txt
- Categorías: https://www.lariviera.co/fragancias · https://www.falabella.com.co/falabella-co/category/cat1461053/Perfumes · https://www.farmatodo.com.co/categorias/belleza/perfumes
