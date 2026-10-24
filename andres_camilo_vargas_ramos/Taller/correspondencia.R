# =============================================================================
# correspondencia.R — Productos comparables y correspondencia entre comercios

# =============================================================================

library(DBI); library(RSQLite); library(tibble); library(dplyr)

ID_PRINCIPAL <- "20261008T184604"

# ---- Productos comparables (identificador común) ----------------------------

productos <- tribble(
  ~id_producto, ~nombre_std,                            ~marca_std,   ~subcategoria_std,         ~contenido, ~unidad,  ~n_unidades,
  "P01",        "Electrolit Fresa 625 ml",              "Electrolit", "Sueros de rehidratación",  625,       "ml",     1,
  "P02",        "Electrolit Uva 625 ml",                "Electrolit", "Sueros de rehidratación",  625,       "ml",     1,
  "P03",        "Electrolit Coco 625 ml",               "Electrolit", "Sueros de rehidratación",  625,       "ml",     1,
  "P04",        "Pedialyte Max 60 Manzana 500 ml",      "Pedialyte",  "Sueros de rehidratación",  500,       "ml",     1,
  "P05",        "Pedialyte Max 60 Uva 500 ml",          "Pedialyte",  "Sueros de rehidratación",  500,       "ml",     1,
  "P06",        "Hidraplus Zinc 75 mEq Coco 400 ml",    "Hidraplus",  "Sueros de rehidratación",  400,       "ml",     1,
  "P07",        "Hidraplus Zinc 75 mEq Manzana 400 ml", "Hidraplus",  "Sueros de rehidratación",  400,       "ml",     1,
  "P08",        "Solhidrex Uva sobre 20,7 g",           "Solhidrex",  "Sales de rehidratación",   20.7,      "g",      1,
  "P09",        "Dulcolax 5 mg x 10 grageas",           "Dulcolax",   "Laxantes",                 10,        "gragea", 10,
  "P10",        "Dulcolax 5 mg x 20 grageas",           "Dulcolax",   "Laxantes",                 20,        "gragea", 20,
  "P11",        "Mareol 50 mg x 12 tabletas",           "Mareol",     "Productos para el mareo",  12,        "tableta",12,
  "P12",        "Gaviscon Doble Acción x 12 sachets",   "Gaviscon",   "Antiácidos",               12,        "sachet", 12
)

# ---- Correspondencia: cada SKU de cada comercio con su id común ------------
correspondencia <- tribble(
  ~id_producto, ~comercio,     ~sku,        ~verificado, ~nota,
  "P01", "Colsubsidio", "6128",      1, "web: FRASCO X 625 ML; EAN 7501125176470",
  "P01", "Cruz Verde",  "147248",    1, NA,
  "P01", "Farmatodo",   "1054483",   1, NA,
  "P02", "Colsubsidio", "6132",      1, "web: FRASCO X 625 ML; EAN 7501125176548",
  "P02", "Cruz Verde",  "146775",    1, NA,
  "P02", "Farmatodo",   "1054486",   1, NA,
  "P03", "Colsubsidio", "6127",      1, "web: FRASCO X 625 ML; EAN 7501125176456",
  "P03", "Cruz Verde",  "146774",    1, NA,
  "P03", "Farmatodo",   "1054482",   1, NA,
  "P04", "Colsubsidio", "833",       1, "web: FRASCO X 500 ML; EAN 7501033957369",
  "P04", "Cruz Verde",  "96736",     1, NA,
  "P04", "Farmatodo",   "1036528",   1, NA,
  "P05", "Colsubsidio", "834",       1, "web: FRASCO X 500 ML; EAN 7501033957376",
  "P05", "Cruz Verde",  "96738",     1, NA,
  "P05", "Farmatodo",   "1036529",   1, NA,
  "P06", "Colsubsidio", "1125",      1, "web: FRASCO X 400 ML; EAN 7702057012298",
  "P06", "Cruz Verde",  "117848",    1, NA,
  "P06", "Farmatodo",   "1046964",   1, NA,
  "P07", "Colsubsidio", "11253",     1, "web: FRASCO X 400 ML; EAN 7702057015428",
  "P07", "Cruz Verde",  "567835",    1, NA,
  "P07", "Farmatodo",   "240600072", 1, NA,
  "P08", "Colsubsidio", "13802",     1, NA,
  "P08", "Cruz Verde",  "98336",     1, NA,
  "P08", "Farmatodo",   "266700303", 1, NA,
  "P09", "Colsubsidio", "5321",      1, "web: CAJA X 10 GRG; EAN 7703202221169",
  "P09", "Cruz Verde",  "20258",     1, NA,
  "P09", "Farmatodo",   "1003509",   1, NA,
  "P10", "Colsubsidio", "5320",      1, "web: CAJA X 20 GRG; EAN 7703202221152",
  "P10", "Cruz Verde",  "71766",     1, NA,
  "P10", "Farmatodo",   "1029133",   1, NA,
  "P11", "Colsubsidio", "1540",      1, "12 tabs en el nombre; 50 mg en la descripción",
  "P11", "Cruz Verde",  "20253",     1, "CV 78183 (Caja x 12) parece la misma presentación",
  "P11", "Farmatodo",   "1003131",   1, NA,
  "P12", "Colsubsidio", "1966",      1, "web: CAJA X 12 SACHET; EAN 7702626211770",
  "P12", "Cruz Verde",  "117792",    1, NA,
  "P12", "Farmatodo",   "1046948",   1, NA
)

# ---- Guardar en SQLite --------------------------------------------------------
con <- dbConnect(SQLite(), "comparador_precios.sqlite")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS productos (
    id_producto      TEXT PRIMARY KEY,
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

# Son tablas de referencia curadas a mano: se reemplazan completas en cada corrida
dbExecute(con, "DELETE FROM correspondencia")
dbExecute(con, "DELETE FROM productos")
dbAppendTable(con, "productos", productos)
dbAppendTable(con, "correspondencia", correspondencia)

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

# Enlaces de Colsubsidio por verificar
por_verificar <- dbGetQuery(con, "
  SELECT c.id_producto, o.nombre_original, o.url, c.nota
  FROM correspondencia c
  JOIN observaciones o ON o.comercio = c.comercio AND o.sku = c.sku
  WHERE c.verificado = 0 AND o.id_ejecucion = ?", params = list(ID_PRINCIPAL))
print(por_verificar)

dbDisconnect(con)
