-- Esquema de comparador_precios.sqlite (lo ejecuta scraper.conectar). Taller 1 · cuidado bucal.
PRAGMA foreign_keys = ON;
CREATE TABLE IF NOT EXISTS comercios (
    comercio_id TEXT PRIMARY KEY, nombre TEXT NOT NULL, tipo TEXT, url_base TEXT NOT NULL, plataforma TEXT);
CREATE TABLE IF NOT EXISTS ejecuciones (                -- una fila por corrida y comercio
    ejecucion_id TEXT NOT NULL, comercio_id TEXT NOT NULL REFERENCES comercios, inicio TEXT NOT NULL, fin TEXT,
    modo_carga TEXT NOT NULL CHECK (modo_carga IN ('en_linea', 'sin_conexion')),
    n_registros INTEGER NOT NULL DEFAULT 0, n_productos INTEGER NOT NULL DEFAULT 0, total_reportado_sitio INTEGER,
    estado TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')), mensaje TEXT, carpeta_raw TEXT,
    PRIMARY KEY (ejecucion_id, comercio_id));
CREATE TABLE IF NOT EXISTS secciones (                  -- secciones del sitio equivalentes a la categoría
    ejecucion_id TEXT NOT NULL, comercio_id TEXT NOT NULL, seccion_id TEXT NOT NULL, nombre TEXT,
    ruta_nombres TEXT, filtro TEXT, url TEXT, api TEXT, total_reportado INTEGER, n_obtenidos INTEGER, error TEXT,
    PRIMARY KEY (ejecucion_id, comercio_id, seccion_id),
    FOREIGN KEY (ejecucion_id, comercio_id) REFERENCES ejecuciones (ejecucion_id, comercio_id));
CREATE TABLE IF NOT EXISTS productos (                  -- identificador común entre comercios (limpieza.py)
    producto_id TEXT PRIMARY KEY, nombre_estandar TEXT NOT NULL, marca TEXT, subcategoria TEXT, familia TEXT,
    contenido_unitario REAL, n_unidades INTEGER, contenido_total REAL, unidad TEXT, concentracion TEXT,
    ean TEXT, metodo_correspondencia TEXT, n_comercios INTEGER, alerta TEXT,
    comparable INTEGER NOT NULL DEFAULT 0 CHECK (comparable IN (0, 1)));
CREATE TABLE IF NOT EXISTS familias (familia TEXT PRIMARY KEY, nombre_familia TEXT);
CREATE TABLE IF NOT EXISTS anuncios (                   -- cada producto tal como aparece en un comercio
    anuncio_id INTEGER PRIMARY KEY AUTOINCREMENT, comercio_id TEXT NOT NULL REFERENCES comercios,
    sku_comercio TEXT NOT NULL, product_id_comercio TEXT, ean TEXT, referencia TEXT,
    -- texto publicado por el comercio, sin modificar
    nombre_original TEXT, nombre_item_original TEXT, marca_original TEXT, categoria_original TEXT,
    descripcion_original TEXT, ficha_tecnica_original TEXT, texto_original TEXT NOT NULL, url_producto TEXT,
    seccion_origen TEXT, primera_consulta TEXT NOT NULL, ultima_consulta TEXT NOT NULL,
    -- campos estandarizados por limpieza.py
    nombre_std TEXT, marca_std TEXT, subcategoria_std TEXT, contenido_unitario REAL, n_unidades INTEGER,
    contenido_total REAL, unidad TEXT, concentracion TEXT, familia TEXT, ean_valido TEXT,
    incluido INTEGER NOT NULL DEFAULT 1 CHECK (incluido IN (0, 1)), motivo_exclusion TEXT, alerta_limpieza TEXT,
    metodo_correspondencia TEXT, producto_id TEXT REFERENCES productos,
    comparable INTEGER NOT NULL DEFAULT 0 CHECK (comparable IN (0, 1)),
    UNIQUE (comercio_id, sku_comercio));
CREATE TABLE IF NOT EXISTS observaciones (              -- precio de un anuncio en una ejecución
    observacion_id INTEGER PRIMARY KEY AUTOINCREMENT, ejecucion_id TEXT NOT NULL, comercio_id TEXT NOT NULL,
    anuncio_id INTEGER NOT NULL REFERENCES anuncios, sku_comercio TEXT NOT NULL,
    fecha_hora_consulta TEXT NOT NULL CHECK (fecha_hora_consulta GLOB
        '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]-05:00'),
    precio REAL CHECK (precio > 0), precio_lista REAL CHECK (precio_lista > 0),
    es_promocion INTEGER NOT NULL CHECK (es_promocion IN (0, 1)), texto_promocion TEXT,
    disponible INTEGER CHECK (disponible IN (0, 1)), vendedor TEXT, url_fuente TEXT NOT NULL,
    comparable INTEGER NOT NULL DEFAULT 0 CHECK (comparable IN (0, 1)),
    FOREIGN KEY (ejecucion_id, comercio_id) REFERENCES ejecuciones (ejecucion_id, comercio_id),
    UNIQUE (comercio_id, sku_comercio, fecha_hora_consulta),   -- una observación no se inserta dos veces
    UNIQUE (ejecucion_id, comercio_id, sku_comercio));
CREATE TABLE IF NOT EXISTS parametros (clave TEXT PRIMARY KEY, valor TEXT);
CREATE INDEX IF NOT EXISTS ix_obs_ejecucion ON observaciones (ejecucion_id, comercio_id);
CREATE INDEX IF NOT EXISTS ix_anuncios_producto ON anuncios (producto_id);
CREATE VIEW IF NOT EXISTS v_observaciones AS
SELECT o.observacion_id, o.ejecucion_id, o.comercio_id, c.nombre AS comercio, a.anuncio_id, a.sku_comercio,
       a.ean_valido AS ean, a.producto_id, p.nombre_estandar, a.marca_std AS marca, a.subcategoria_std AS subcategoria,
       a.contenido_unitario, a.n_unidades, a.contenido_total, a.unidad, a.familia, o.precio, o.precio_lista,
       o.es_promocion, o.texto_promocion, o.disponible, o.fecha_hora_consulta, o.url_fuente, a.url_producto,
       o.comparable, a.incluido, a.nombre_original, a.texto_original
FROM observaciones o JOIN anuncios a ON a.anuncio_id = o.anuncio_id JOIN comercios c ON c.comercio_id = o.comercio_id
LEFT JOIN productos p ON p.producto_id = a.producto_id;
