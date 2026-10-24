# =============================================================================
# correspondencia.R — Productos comparables y correspondencia entre comercios
# (taller_1.qmd lo ejecuta en la sección "Preparación de los datos")
#
# Regla de correspondencia: una presentación es comparable cuando el MISMO
# código de barras (EAN) aparece en los tres comercios en la ejecución principal.
# El EAN identifica de forma única marca, variante, contenido y número de unidades,
# así que la correspondencia no depende de cómo cada tienda escribe el nombre.
# =============================================================================

library(DBI); library(RSQLite); library(tibble); library(dplyr)

ID_PRINCIPAL <- "20261013T183453"

# ---- Presentaciones comparables (identificador común) ------------------------
# contenido  = cantidad total del empaque en la unidad indicada
# n_unidades = número de rollos o piezas del empaque
productos <- tribble(
  ~id_producto, ~ean,            ~nombre_std,                                   ~marca_std, ~subcategoria_std,    ~contenido, ~unidad,      ~n_unidades,
  "P01",        "7702120012736", "Papel higiénico Suave Gold triple hoja x4",   "Suave",    "Papel higiénico",    128,        "m",          4,
  "P02",        "7702120012743", "Papel higiénico Suave Gold triple hoja x12",  "Suave",    "Papel higiénico",    384,        "m",          12,
  "P03",        "7702120014150", "Papel higiénico Rosal triple hoja x4",        "Rosal",    "Papel higiénico",    120,        "m",          4,
  "P04",        "7702120014174", "Papel higiénico Rosal triple hoja x12",       "Rosal",    "Papel higiénico",    360,        "m",          12,
  "P05",        "7702026159849", "Papel higiénico Familia Green x4",            "Familia",  "Papel higiénico",    118.4,      "m",          4,
  "P06",        "7702026159955", "Papel higiénico Familia Green x12",           "Familia",  "Papel higiénico",    355.2,      "m",          12,
  "P07",        "7702026020156", "Servilletas Familia Acolchamax x100",         "Familia",  "Servilletas",        100,        "servilleta", 100,
  "P08",        "7702026025229", "Servilletas Familia Acolchamax x300",         "Familia",  "Servilletas",        300,        "servilleta", 300,
  "P09",        "7702026155179", "Toalla de cocina Familia Green día a día 70 h", "Familia", "Toallas de cocina",  70,         "hoja",       1,
  "P10",        "7702120010121", "Toalla de cocina Rosal Practisec 80 h",       "Rosal",    "Toallas de cocina",  80,         "hoja",       1,
  "P11",        "7702458001938", "Vasos desechables Tami 7 oz x25",             "Tami",     "Vasos desechables",  25,         "vaso",       25,
  "P12",        "7702458245301", "Platos desechables Darnel pando 23 cm x20",   "Darnel",   "Platos desechables", 20,         "plato",      20,
  "P13",        "7702251075808", "Tenedores desechables Domingo x25",           "Domingo",  "Cubiertos desechables", 25,      "tenedor",    25
)

# ---- Guardar en SQLite --------------------------------------------------------
con <- dbConnect(SQLite(), "comparador_precios.sqlite")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS productos (
    id_producto      TEXT PRIMARY KEY,
    ean              TEXT NOT NULL UNIQUE,
    nombre_std       TEXT NOT NULL,
    marca_std        TEXT,
    subcategoria_std TEXT,
    contenido        REAL,
    unidad           TEXT,
    n_unidades       INTEGER
  )")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS correspondencia (
    id_producto TEXT NOT NULL REFERENCES productos (id_producto),
    comercio    TEXT NOT NULL,
    sku         TEXT NOT NULL,
    verificado  INTEGER NOT NULL,
    nota        TEXT,
    PRIMARY KEY (comercio, sku)
  )")

# Tablas de referencia: se reemplazan completas en cada corrida
dbExecute(con, "DELETE FROM correspondencia")
dbExecute(con, "DELETE FROM productos")
dbAppendTable(con, "productos", productos)

# Correspondencia automática: el SKU de cada comercio que tiene el EAN del producto
dbExecute(con, "
  INSERT OR IGNORE INTO correspondencia (id_producto, comercio, sku, verificado, nota)
  SELECT p.id_producto, o.comercio, MIN(o.sku), 1, 'mismo EAN ' || p.ean
  FROM productos p
  JOIN observaciones o ON o.ean = p.ean AND o.id_ejecucion = ?
  GROUP BY p.id_producto, o.comercio", params = list(ID_PRINCIPAL))

# Vista con el indicador comparable para todas las observaciones
dbExecute(con, "DROP VIEW IF EXISTS v_observaciones")
dbExecute(con, "
  CREATE VIEW v_observaciones AS
  SELECT o.*, c.id_producto,
         CASE WHEN c.id_producto IS NULL THEN 0 ELSE 1 END AS comparable
  FROM observaciones o
  LEFT JOIN correspondencia c ON c.comercio = o.comercio AND c.sku = o.sku")

# ---- Verificación de cobertura en la ejecución principal ---------------------
cobertura <- dbGetQuery(con, "
  SELECT p.id_producto, p.nombre_std,
         COUNT(DISTINCT v.comercio) AS n_comercios,
         GROUP_CONCAT(v.comercio || ': ' || v.precio, ' | ') AS precios
  FROM productos p
  LEFT JOIN v_observaciones v
         ON v.id_producto = p.id_producto AND v.id_ejecucion = ?
  GROUP BY p.id_producto
  ORDER BY p.id_producto", params = list(ID_PRINCIPAL))
print(cobertura)

cat("\nObservaciones comparables en la ejecución principal:",
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM v_observaciones
                     WHERE comparable = 1 AND id_ejecucion = ?",
               params = list(ID_PRINCIPAL))$n, "\n")

dbDisconnect(con)
