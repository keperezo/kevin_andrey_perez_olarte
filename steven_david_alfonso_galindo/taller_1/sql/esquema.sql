-- =============================================================================
-- Esquema de la base comparador_precios.sqlite
-- Taller 1 · Minería de Datos · Categoría: perfumería (solo perfumes)
--
-- scraper.R ejecuta este archivo cada vez que abre la base. Todas las
-- sentencias usan IF NOT EXISTS, así que nunca borran ni sobrescriben datos.
-- Las llaves foráneas se activan en cada conexión con PRAGMA foreign_keys = ON.
-- =============================================================================

-- Comercios de los que se extraen datos (uno por fila).
CREATE TABLE IF NOT EXISTS comercios (
  comercio_id        INTEGER PRIMARY KEY,
  nombre             TEXT    NOT NULL UNIQUE,
  clave              TEXT    NOT NULL UNIQUE,      -- nombre corto usado en raw/
  sitio_web          TEXT    NOT NULL,
  url_categoria      TEXT    NOT NULL,
  metodo_extraccion  TEXT    NOT NULL
);

-- Una fila por comercio y corrida del scraper.
-- id_ejecucion es el mismo para los tres comercios de una corrida.
CREATE TABLE IF NOT EXISTS ejecuciones (
  id_ejecucion       TEXT    NOT NULL,
  comercio_id        INTEGER NOT NULL REFERENCES comercios (comercio_id),
  fecha_hora_inicio  TEXT    NOT NULL,             -- ISO 8601 con -05:00
  fecha_hora_fin     TEXT,
  modo               TEXT    NOT NULL CHECK (modo IN ('en_linea', 'sin_conexion')),
  n_registros        INTEGER NOT NULL DEFAULT 0,   -- productos únicos obtenidos
  n_total_sitio      INTEGER,                      -- cifra que muestra el sitio, si existe
  estado             TEXT    NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
  detalle            TEXT,
  PRIMARY KEY (id_ejecucion, comercio_id)
);

-- Identificador común de las presentaciones que se comparan entre comercios.
-- Lo llena procesamiento.R a partir de datos/catalogo_comparables.csv.
CREATE TABLE IF NOT EXISTS productos (
  producto_id        TEXT    PRIMARY KEY,          -- p. ej. VERSACE-EROS-EDT-100
  familia            TEXT    NOT NULL,             -- mismo perfume en distintos tamaños
  marca              TEXT    NOT NULL,
  linea              TEXT    NOT NULL,
  concentracion      TEXT    NOT NULL CHECK (concentracion IN ('Extrait', 'Parfum', 'EDP', 'EDT', 'EDC')),
  contenido          REAL    NOT NULL CHECK (contenido > 0),
  unidad             TEXT    NOT NULL CHECK (unidad = 'ml'),
  unidades           INTEGER NOT NULL DEFAULT 1 CHECK (unidades >= 1),
  genero             TEXT    NOT NULL CHECK (genero IN ('Hombre', 'Mujer', 'Unisex')),
  subcategoria       TEXT    NOT NULL,
  descripcion        TEXT
);

-- Cada producto tal como lo publica un comercio (un anuncio).
-- Los campos *_std son la versión estandarizada; el texto original de cada
-- consulta se guarda sin cambios en observaciones.texto_original.
CREATE TABLE IF NOT EXISTS anuncios (
  anuncio_id             INTEGER PRIMARY KEY,
  comercio_id            INTEGER NOT NULL REFERENCES comercios (comercio_id),
  id_producto_comercio   TEXT    NOT NULL,         -- identificador del producto en el comercio
  sku                    TEXT,
  ean                    TEXT,
  url_producto           TEXT,
  marca_std              TEXT,
  linea_std              TEXT,
  concentracion_std      TEXT,
  contenido_ml           REAL,
  unidades               INTEGER,
  genero_std             TEXT,
  subcategoria_std       TEXT,
  excluido               INTEGER NOT NULL DEFAULT 0 CHECK (excluido IN (0, 1)),
  motivo_exclusion       TEXT,
  producto_id            TEXT    REFERENCES productos (producto_id),
  regla_correspondencia  TEXT,
  UNIQUE (comercio_id, id_producto_comercio)
);

-- Una fila por precio observado: anuncio, comercio, ejecución y momento.
CREATE TABLE IF NOT EXISTS observaciones (
  observacion_id         INTEGER PRIMARY KEY,
  anuncio_id             INTEGER NOT NULL REFERENCES anuncios (anuncio_id),
  comercio_id            INTEGER NOT NULL REFERENCES comercios (comercio_id),
  id_ejecucion           TEXT    NOT NULL,
  fecha_hora_consulta    TEXT    NOT NULL
                         CHECK (fecha_hora_consulta LIKE '____-__-__T__:__:__-05:00'),
  nombre_original        TEXT    NOT NULL,
  marca_original         TEXT,
  subcategoria_original  TEXT,
  presentacion_original  TEXT,
  contenido_original     REAL,
  unidad_original        TEXT,
  texto_original         TEXT    NOT NULL,
  precio                 REAL    CHECK (precio IS NULL OR precio > 0),
  precio_antes           REAL    CHECK (precio_antes IS NULL OR precio_antes > 0),
  promocion              INTEGER CHECK (promocion IN (0, 1)),
  etiqueta_promocion     TEXT,
  precio_condicionado    REAL    CHECK (precio_condicionado IS NULL OR precio_condicionado > 0),
  condicion_precio       TEXT,
  vendedor               TEXT,
  disponible             INTEGER CHECK (disponible IN (0, 1)),
  url_extraccion         TEXT    NOT NULL,
  archivo_raw            TEXT,
  comparable             INTEGER NOT NULL DEFAULT 0 CHECK (comparable IN (0, 1)),
  FOREIGN KEY (id_ejecucion, comercio_id) REFERENCES ejecuciones (id_ejecucion, comercio_id),
  UNIQUE (comercio_id, anuncio_id, fecha_hora_consulta)
);

CREATE INDEX IF NOT EXISTS idx_obs_ejecucion ON observaciones (id_ejecucion, comercio_id);
CREATE INDEX IF NOT EXISTS idx_anuncios_producto ON anuncios (producto_id);

-- Vista de trabajo: cada observación con su comercio, su anuncio y su producto común.
CREATE VIEW IF NOT EXISTS v_observaciones AS
SELECT
  o.observacion_id,
  o.id_ejecucion,
  c.nombre            AS comercio,
  o.fecha_hora_consulta,
  a.anuncio_id,
  a.id_producto_comercio,
  o.nombre_original,
  a.marca_std,
  a.subcategoria_std,
  a.concentracion_std,
  a.contenido_ml,
  a.excluido,
  a.motivo_exclusion,
  a.producto_id,
  p.familia,
  p.marca             AS producto_marca,
  p.linea             AS producto_linea,
  p.concentracion     AS producto_concentracion,
  p.contenido         AS producto_contenido,
  p.genero            AS producto_genero,
  o.precio,
  o.precio_antes,
  o.promocion,
  o.etiqueta_promocion,
  o.precio_condicionado,
  o.condicion_precio,
  o.vendedor,
  o.comparable,
  o.url_extraccion,
  a.url_producto
FROM observaciones o
JOIN comercios c ON c.comercio_id = o.comercio_id
JOIN anuncios  a ON a.anuncio_id  = o.anuncio_id
LEFT JOIN productos p ON p.producto_id = a.producto_id;
