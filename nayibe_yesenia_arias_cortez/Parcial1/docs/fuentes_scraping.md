# Fuentes y estrategia de extracción

Revisión realizada el 2026-10-09. Lenguaje: Python 3.12 (`requests`, `pandas`, `sqlite3`, `gzip`, `json`, `re`).

## Comercios

| Comercio | Plataforma | Cómo se identificó | Fuente de datos usada | Paginación |
|---|---|---|---|---|
| La Rebaja (larebajavirtual.com) | VTEX IO (`x-vtex-product: store`) | Cabeceras HTTP de la página de inicio; categorías desde `/api/catalog_system/pub/category/tree/3` | API pública de catálogo VTEX `GET /api/catalog_system/pub/products/search?fq=C:<ruta>&_from=i&_to=j` (JSON) | `_from`/`_to` de 50 en 50 hasta el total de la cabecera `resources: 0-49/<total>` |
| Locatel (locatelcolombia.com) | VTEX IO | Igual que La Rebaja | Igual que La Rebaja | Igual que La Rebaja |
| Droguerías Cafam (drogueriascafam.com.co) | VTEX IO | Cabeceras HTTP; rutas de categoría desde `/sitemap/category-0.xml`; en el navegador (pestaña Network) se vio que la página pide los productos a `/_v/segment/…` (prohibido por robots.txt), pero el HTML servido ya trae los productos en `__STATE__ = {…}` | HTML de la categoría: JSON `__STATE__` (caché de Apollo GraphQL) dentro de un `<script>`; objeto `productSearch` con `recordsFiltered` y `products` | `?page=N` (12 productos por página) hasta `ceil(recordsFiltered/12)` |

Campos tomados de cada producto VTEX (ambas fuentes tienen la misma estructura): `productId`, `productName`, `brand`,
`categories`, `description`, ficha técnica (`allSpecifications` / `specificationGroups`), `link`, y por SKU
(`items[]`): `itemId`, `ean`, `nameComplete`, `sellers[].commertialOffer` (`Price`, `ListPrice`, `AvailableQuantity`,
`Teasers`). Precio = `Price` del vendedor con existencias; precio antes del descuento = `ListPrice` si es mayor que
`Price`; agotado (`AvailableQuantity = 0`) → `disponible = 0`.

## Buenas prácticas

User-Agent identificable con enlace al repositorio, 1 s de pausa entre peticiones, timeout de 30 s, 4 reintentos con
backoff exponencial ante 429/5xx, una sola corrida completa ≈ 45 peticiones (≈ 2 minutos). Copias crudas
comprimidas en `raw/<comercio>/<fecha>/<id_ejecucion>/` con `manifiesto.json` (URL, fecha y hora, totales).

## robots.txt (2026-10-09)

| Comercio | Relevante | Cumplimiento |
|---|---|---|
| La Rebaja | Prohíbe `/account/`, `/login/`, `/checkout/`, `/busca/`… No prohíbe `/api/`. | ✅ |
| Locatel | Prohíbe `/busca`, `/search`, `/_v/`, `/checkout/`… No prohíbe `/api/`. | ✅ |
| Cafam | Prohíbe `/api/`, `/_v/segment`, `/_v/private`, `/busca/`, `?O=`, `?order=`, `?PS=`… Permite `?page=N`. | ✅ solo se usa `/<categoría>?page=N` |

## Términos de uso (2026-10-09)

| Comercio | Cláusulas relevantes |
|---|---|
| La Rebaja | "Términos de uso" (`/terminos-de-uso`): prohíbe de forma genérica la copia, reproducción, modificación o distribución de los "Contenidos" por cualquier medio sin permiso escrito de Copservir Ltda.; prohíbe deeplinking y framing. No menciona robots, spiders ni extracción automatizada. |
| Locatel | `/terminos-y-condiciones`: prohíbe copiar o adaptar el código HTML del sitio para generar páginas propias y vulnerar la seguridad o acceder a información no autorizada. No menciona robots ni extracción. |
| Cafam | `/terminos-y-condiciones`: sin cláusulas sobre robots, extracción, copia o reproducción. |

Uso que se hace de los datos: académico, sin fines comerciales; solo información pública de productos (nombre,
precio, presentación, ficha técnica, descripción). Riesgo identificado: el texto original de La Rebaja se guarda en un
repositorio público.

## Comercios evaluados y descartados

| Comercio | Motivo |
|---|---|
| Éxito, Carulla | robots.txt prohíbe `/api/`; los productos solo llegan por `/api/graphql`. |
| Olímpica | Términos §2.4.1 prohíben usar "spiders", "site copiers" u otros programas para acceder, copiar o monitorear el contenido. |
| Farmacias Pasteur | robots.txt prohíbe toda URL con `?`, `&` o `%` (no permite paginar); términos con la misma cláusula genérica de copia. |
| Drogas La Economía | robots.txt `Crawl-delay: 3600`; plataforma Instaleap; certificado SSL incompleto. |
| Cruz Verde | API propia sin EAN en el listado y precios por zona de inventario. |
| Farmatodo | Solo 24 productos en el HTML; el resto vía Algolia; 2 EAN comunes en la muestra. |

## Resultado de la ejecución 1 (20261009T123410)

| Comercio | Rama | Total del sitio | Obtenidos |
|---|---|---|---|
| La Rebaja | `/5/29/`, `/8/585/`, `/1316/` | 375 + 8 + 3 = 386 | 386 |
| Locatel | `/1/9/`, `/2/15/`, `/269/438/455/` | 712 + 60 + 5 = 777 productos (788 SKU) | 777 productos |
| Cafam | cuidado capilar, dermocosmética capilar | 207 + 26 = 233 | 233 |
