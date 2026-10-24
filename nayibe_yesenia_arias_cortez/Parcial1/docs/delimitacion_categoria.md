# Delimitación de la categoría: Cuidado capilar

Categoría registrada en la planilla: **Cuidado capilar** — "Champús, acondicionadores, mascarillas, tintes y
productos para peinar. Excluye medicamentos y productos infantiles." (`docs/delimitacion_categoria.Rmd`).

## Se incluye

| Subcategoría estandarizada | Productos |
|---|---|
| `shampoo` | Champús (incluye 2 en 1 y anticaspa cosméticos). |
| `acondicionador` | Acondicionadores y enjuagues. |
| `mascarilla_tratamiento` | Mascarillas, tratamientos capilares y **tratamientos afines: óleos, selladores de puntas y sérums**. |
| `coloracion` | Tintes, kits de color, decolorantes, aguas oxigenadas y **aclaradores capilares** (p. ej. Sol Eclair). |
| `peinado` | Productos para peinar: cremas de peinar, geles, lacas, pomadas, ceras, mousses y fijadores. |

**Decisión documentada (tratamientos afines):** los óleos, selladores de puntas y sérums no aparecen en la
redacción de la planilla. Se incluyen porque cumplen la misma función que las mascarillas y tratamientos
(cuidado cosmético del cabello), y porque los tres comercios los clasifican dentro de su categoría capilar.
Se agrupan en `mascarilla_tratamiento`.

**Decisión documentada (aclarador):** el aclarador capilar (p. ej. Sol Eclair Manzanilla) modifica el color del
cabello, por lo que se clasifica como `coloracion`.

## Se excluye (y se registra con su motivo)

| Motivo | Criterio |
|---|---|
| `medicamento` | Productos con principio activo farmacológico o de venta como medicamento: minoxidil (incl. Pilogán, "solución tópica"), ketoconazol (p. ej. Medicasp), clobetasol, finasterida. También todo producto cuya ficha diga "Producto RX/OTC: SI" (La Rebaja) o "Requiere prescripción médica: Sí" (Cafam). |
| `infantil` | Productos para bebés o niños: "baby", "bebé", "kids", "niños", "infantil", personajes (Disney, Marvel, Frozen, etc.), o ramas del comercio de bebé/maternidad. |
| `accesorio` | Accesorios, utensilios y aparatos: cepillos, peinillas, moños, ganchos, gorros, toallas, rulos, planchas, secadores, patilleras, estilizadores eléctricos. |
| `fuera_de_categoria` | Productos que el comercio lista en la categoría pero no son de cuidado capilar: bronceadores, productos corporales o para pies, pruebas de embarazo, **suplementos orales** (cápsulas, tabletas, gomitas, Edapil) y **productos para barba y bigote** (incluidos tintes de barba). |

Los excluidos no se borran: quedan en la base con `incluido = 0` y su `motivo_exclusion`, para poder auditarlos.

## Cómo se aplica

`scraper.clasificar()` aplica las reglas en este orden: señales de medicamento en la ficha → exclusiones por palabras
clave del nombre (y de la ruta de categoría del comercio, para infantil y accesorio) → subcategoría según la palabra
de tipo de producto que aparece **primero** en el nombre ("Shampoo… con keratina" → `shampoo`); si el nombre no la
trae, se usa la ruta de categoría del comercio; si tampoco, `otro_capilar` (se mantiene incluido).
Los productos anticaída **cosméticos** (p. ej. Piloskin, Folifort) se incluyen; los que contienen minoxidil no.

## Ramas de cada comercio consideradas equivalentes a la categoría

| Comercio | Ramas (equivalentes a "cuidado capilar") | Notas |
|---|---|---|
| La Rebaja | Cuidado personal > Cuidado capilar (`fq=C:/5/29/`); Dermocosmética > Cuidado capilar (`/8/585/`); Crema de peinar (`/1316/`) | Se excluyen Maternidad > shampoo/acondicionador baby. |
| Locatel | Cuidado personal > Cuidado cabello (`/1/9/`); Dermocosmética > Capilar (`/2/15/`); Marketplace > Belleza > Cuidado capilar (`/269/438/455/`) | Se excluye Maternidad > shampoo-acondicionadores. |
| Droguerías Cafam | `cuidado-personal/cuidado-capilar`; `dermocosmetica/cuidado-capilar` | Cafam no tiene rama de coloración ni tintes (sitemap de categorías revisado 2026-10-09). |
