# -*- coding: utf-8 -*-
"""
preparacion.py - Taller 1, Minería de Datos (UNAL)
Limpieza, estandarización y correspondencia de productos sobre la base SQLite.

Reglas documentadas
-------------------
1. Los textos originales (titulo_original, marca_original, texto_original) NUNCA se modifican.
2. La marca estandarizada (marca_estandar) se obtiene quitando tildes, pasando a minúsculas,
   colapsando espacios y aplicando un diccionario de alias; luego se escribe en formato título.
3. Correspondencia entre comercios: una observación es comparable (comparable = 1) si
   (a) está en alcance, (b) tiene precio vigente, y (c) su código de barras (EAN) está en la tabla
   `productos`, cuyos 10 EAN se verificaron A MANO (marca, variante, contenido, unidades).
4. Cada producto comparable recibe un id_producto_comun (P01...P10) idéntico en los tres comercios.
5. El proceso es idempotente: se puede volver a ejecutar tras cada nueva corrida del scraper.
"""
import re
import sqlite3
import unicodedata

# EAN verificados manualmente: misma marca, variante, contenido y unidades en los 3 comercios.
PRODUCTOS_COMPARABLES = [
    # id,   EAN,             marca,            nombre estándar,                          contenido, unidad
    ("P01", "3337875722827", "La Roche-Posay", "Effaclar Sérum Ultra Concentrado",        30,  "mL"),
    ("P02", "3499320011655", "Cetaphil",       "Optimal Hydration Crema Facial Día",      48,  "g"),
    ("P03", "3499320015530", "Cetaphil",       "Limpiador Piel Grasa",                    473, "mL"),
    ("P04", "3701129804995", "Bioderma",       "Sensibio Defensive Sérum",                30,  "mL"),
    ("P05", "4005900436986", "Eucerin",        "DermoPure Gel Limpiador",                 200, "mL"),
    ("P06", "8429420171718", "Isdin",          "Nutradeica Gel Crema Facial",             50,  "mL"),
    ("P07", "8429979355355", "Sesderma",       "Sesvitamin-C Sérum Liposomal",            30,  "mL"),
    ("P08", "8429979475213", "Sesderma",       "Sesvitamin-C5 Sérum Liposomal",           30,  "mL"),
    ("P09", "8470003406581", "Sesderma",       "Sesvitamin-C Radiance Fluido Luminoso",   50,  "mL"),
    ("P10", "8470003523028", "Sesderma",       "Sesvitamin-C Contorno de Ojos",           15,  "mL"),
]

# Mismo producto en distintos tamaños (para precio por unidad de contenido, sección 4.2 del enunciado).
# La Rebaja publica la versión de 237 mL con otro EAN (3499320003254); la equivalencia se verificó a mano
# por marca, variante (piel grasa) y contenido.
FAMILIAS_TAMANO = [
    ("F01", "Cetaphil Limpiador Piel Grasa", "3499320012812"),   # 237 mL (Dermatodo, Farmatodo)
    ("F01", "Cetaphil Limpiador Piel Grasa", "3499320003254"),   # 237 mL (La Rebaja)
    ("F01", "Cetaphil Limpiador Piel Grasa", "3499320015530"),   # 473 mL (los tres comercios)
]

ALIAS_MARCA = {
    "la roche posay": "La Roche-Posay", "la roche-posay": "La Roche-Posay", "lrp": "La Roche-Posay",
    "isdin": "Isdin", "cerave": "CeraVe", "avene": "Avène", "skinceuticals": "SkinCeuticals",
}


def _sin_tildes(s):
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn")


def estandarizar_marca(m):
    """Marca en formato canónico; None si no hay marca (nunca se reemplaza por cadena vacía)."""
    if m is None or not str(m).strip():
        return None
    clave = re.sub(r"\s+", " ", _sin_tildes(str(m)).casefold().strip())
    return ALIAS_MARCA.get(clave, re.sub(r"\s+", " ", str(m).strip()).title() if clave.isupper() or clave.islower() else str(m).strip())


def preparar(db_path="comparador_precios.sqlite"):
    conn = sqlite3.connect(db_path)
    conn.create_function("MARCA_STD", 1, estandarizar_marca)
    conn.executescript("""
        CREATE TABLE IF NOT EXISTS productos (
            id_producto_comun TEXT PRIMARY KEY,
            codigo_barras     TEXT NOT NULL UNIQUE,
            marca_estandar    TEXT NOT NULL,
            nombre_estandar   TEXT NOT NULL,
            contenido_num     REAL NOT NULL,
            unidad            TEXT NOT NULL
        );
    """)
    conn.execute("""CREATE TABLE IF NOT EXISTS familias_tamano (
        id_familia TEXT NOT NULL, nombre_familia TEXT NOT NULL, codigo_barras TEXT NOT NULL UNIQUE)""")
    conn.executemany("INSERT OR REPLACE INTO familias_tamano VALUES (?,?,?)", FAMILIAS_TAMANO)
    cols = {r[1] for r in conn.execute("PRAGMA table_info(observaciones)")}
    for col, tipo in (("marca_estandar", "TEXT"), ("id_producto_comun", "TEXT"), ("comparable", "INTEGER DEFAULT 0")):
        if col not in cols:
            conn.execute(f"ALTER TABLE observaciones ADD COLUMN {col} {tipo}")
    conn.executemany("INSERT OR REPLACE INTO productos VALUES (?,?,?,?,?,?)",
                     [(i, e, m, n, c, u) for i, e, m, n, c, u in PRODUCTOS_COMPARABLES])
    conn.execute("UPDATE observaciones SET marca_estandar = MARCA_STD(marca_original)")
    conn.execute("""
        UPDATE observaciones SET
            id_producto_comun = (SELECT p.id_producto_comun FROM productos p
                                 WHERE p.codigo_barras = observaciones.codigo_barras
                                   AND p.contenido_num = observaciones.contenido_num
                                   AND p.unidad = observaciones.unidad),
            comparable = 0
    """)
    conn.execute("""
        UPDATE observaciones SET comparable = 1
        WHERE id_producto_comun IS NOT NULL AND en_alcance = 1 AND precio_vigente IS NOT NULL
    """)
    conn.execute("UPDATE observaciones SET id_producto_comun = NULL WHERE comparable = 0")
    conn.execute("CREATE INDEX IF NOT EXISTS idx_obs_comp ON observaciones (comparable, id_producto_comun)")
    conn.execute("DROP VIEW IF EXISTS v_comparables")
    conn.execute("""
        CREATE VIEW v_comparables AS
        SELECT o.id_ejecucion, o.comercio, p.id_producto_comun, p.marca_estandar,
               p.nombre_estandar, p.contenido_num, p.unidad,
               o.precio_lista, o.precio_vigente, o.promocion, o.url, o.titulo_original,
               o.fecha_consulta, o.sku_comercio
        FROM observaciones o JOIN productos p USING (id_producto_comun)
        WHERE o.comparable = 1
    """)
    conn.commit()
    resumen = conn.execute("SELECT COUNT(*), COUNT(DISTINCT id_producto_comun) FROM observaciones WHERE comparable=1").fetchone()
    conn.close()
    return {"observaciones_comparables": resumen[0], "productos_comparables": resumen[1]}


if __name__ == "__main__":
    print(preparar())
