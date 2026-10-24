"""
limpieza.py - Estandarización, delimitación de la categoría y correspondencia de productos.

Principios
----------
* El texto original (nombre_original, descripcion_original, ficha_tecnica_original) NUNCA se modifica;
  las versiones estandarizadas viven en columnas *_std.
* Todo lo que se excluye de la categoría queda en anuncios.incluido = 0 con su motivo_descarte.
* La correspondencia entre comercios usa una CLAVE explícita:
      marca | subcategoría | variante | etapa/talla | contenido total + unidad
  Dos anuncios son "el mismo producto" solo si la clave coincide completa (marca, variante,
  etapa, número de unidades). Es una regla documentada y auditable (ver `tabla_revision`).
"""
from __future__ import annotations

import json
import re
import sqlite3
import unicodedata

import pandas as pd

SUBCATS = ("pañal", "pañitos húmedos", "cambiador desechable", "crema de barrera")


def norm(s) -> str:
    s = unicodedata.normalize("NFKD", str(s or ""))
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = re.sub(r"\s+", " ", s.lower()).strip()
    return s.replace("pa#al", "panal")  # algunos sitios publican "PA#AL" por un error de codificación


# ---------------------------------------------------------------------------------- delimitación
# Paquete promocional "pañal + toallitas gratis": se conserva, pero como presentación DISTINTA
# (el precio incluye un regalo), por eso entra a la variante como "+N toallitas".
_REGALO = r"(?:\+|\bgratis\b|\bgts?\b)\s*(?:gratis\s*)?(?:toa\w*|paquete|pack|panitos)?\s*x?\s*(\d+)"

EXCLUSIONES = [
    (r"\b(adulto|adultos|adult|incontinencia|tena|plenitud|rely|content|geriatric\w*)\b|proteccion sanitaria",
     "pañal/producto de adulto"),
    (r"\b(mascota|mascotas|perro|perros|gato|gatos|canino)\b", "producto para mascotas"),
    (r"\bpanalera\b|\bbolso\b|\bmaleta\b", "accesorio (pañalera/bolso)"),
    (r"\b(mobiliario|pared|mesa|mueble|colchoneta|tapete|gimnasio|rubbermaid)\b", "mueble/accesorio no desechable"),
    (r"\b(nistatina|clotrimazol|miconazol|hidrocortisona|ketoconazol|betametasona)\b", "medicinal (antimicótico/corticoide)"),
    (r"desmaquill|\bintim[ao]s?\b|\bfaciales?\b", "toallitas no infantiles"),
    (r"ecologic\w*|reutilizable\w*|lavable\w*|de tela|inserto|entrenador|calzon panty|cubre panal", "pañal/cambiador reutilizable (no desechable)"),
    (r"\b(muneca|muneco|juguete|vacinilla|peluche|lampara|cuadro|puff|colchon|cojin|mouse|moto|carro|llanta|"
     r"contenedor|papelera|repuesto|mochila|morral|bolsa|detergente|banera|cuna|corral|toldillo|tocadiscos|microfono|"
     r"tienda|camping|organizador\w*|tocador|comoda|sabana|almohada|bacinilla|sarten\w*)\b", "accesorio/producto fuera de la categoría"),
]
SENAL_BEBE = r"\b(bebe|bebes|baby|infantil|recien nacido|panal|panales|huggies|pampers|winny|babysec|johnson|pequenin|chicco|mustela)\b"


# Secciones de cada comercio consideradas equivalentes a la categoría del taller (documentado en el informe).
CATEGORIAS_VALIDAS = (r"panales|panitos|panos humedos|nalguita|aseo del bebe|higiene (para bebes|infantil)|cuidado del bebe|"
                      r"cremas antipanalitis|accesorios e higiene|cuidado de la piel|salud e higiene del bebe")


def categoria_valida(categoria_sitio: str) -> bool:
    return bool(re.search(CATEGORIAS_VALIDAS, norm(categoria_sitio)))


def regalo_toallitas(nombre: str):
    """Devuelve (n_toallitas_de_regalo, posición_donde_empieza_el_regalo) o (None, None)."""
    m = re.search(_REGALO, norm(nombre))
    return (int(m.group(1)), m.start()) if m else (None, None)


def clasificar(nombre: str, categoria_sitio: str = "", descripcion: str = "") -> tuple[str | None, str | None]:
    """Devuelve (subcategoría, motivo_descarte). Si subcategoría es None, el anuncio se excluye."""
    n = norm(nombre)
    ctx = norm(f"{nombre} {categoria_sitio}")
    for pat, motivo in EXCLUSIONES:
        if re.search(pat, ctx):
            return None, motivo
    if categoria_sitio and not categoria_valida(categoria_sitio):
        return None, "sección del sitio ajena a la categoría"
    regalo, pos = regalo_toallitas(nombre)
    if regalo and re.search(r"\bpanal(es)?\b", n[:pos]):
        return "pañal", None                      # pañal con toallitas de regalo
    if "+" in n and re.search(r"\bpanal(es)?\b", n) and re.search(r"toa|panitos|humed", n):
        return None, "combo pañal + pañitos sin cantidad identificable"
    if re.search(r"\bcambiador(es)?\b|\btendido\b|protector(es)? (de )?(cambio|cama|desechable)", n):
        if re.search(r"desechab|descartab|un solo uso", norm(f"{nombre} {descripcion}")):
            return "cambiador desechable", None
        return None, "cambiador no desechable"
    if re.search(r"antipanalitis|panalitis|oxido de zinc|(crema|pomada|unguento|pasta|balsamo)\b.*\b(panal|barrera|rozadura|bebe)|crema (para|de) cambio", n):
        return "crema de barrera", None
    if re.search(r"toallit|panitos|humedas?\b|\bwipes\b", n):
        if re.search(SENAL_BEBE, ctx):
            return "pañitos húmedos", None
        return None, "toallitas sin señal infantil"
    if re.search(r"\bpanal(es)?\b|\bpanty\b|calzon de entrenamiento|pants\b", n):
        return "pañal", None
    return None, "fuera de la delimitación de la categoría"


# ---------------------------------------------------------------------------------- atributos
MARCAS = {
    "huggies": "Huggies", "pampers": "Pampers", "winny": "Winny", "babysec": "Babysec", "pequenin": "Pequeñín",
    "johnson": "Johnson's", "chicco": "Chicco", "mustela": "Mustela", "desitin": "Desitin", "bepanthol": "Bepanthol",
    "baby fresh": "Baby Fresh", "cuidado puro": "Huggies", "panalin": "Pañalín",
    "sanitas": "Sanitas", "nosotras": "Nosotras", "kleenex": "Kleenex", "dodot": "Dodot", "fresh baby": "Baby Fresh",
    "nativa": "Nativa", "bebesito": "Bebesito", "tippy": "Tippy", "ternura": "Ternura", "suppra": "Suppra Care",
}
# Una variante es el CONJUNTO de rasgos detectados (p. ej. "active sec" + "pants"): así "Active Sec" y
# "Active Sec Pants" nunca se confunden.
VARIANTES = [
    (r"natural care|puro y natural|cuidado puro", "natural care"),
    (r"active sec|act ?sec", "active sec"), (r"dermacare|derma care", "dermacare"),
    (r"supreme", "supreme"), (r"triple proteccion", "triple proteccion"),
    (r"confortsec|confort sec", "confortsec"), (r"premium care", "premium care"),
    (r"baby ?dry", "baby dry"), (r"ultra ?trim", "ultratrim"), (r"ultra ?protec\w*", "ultraprotect"),
    (r"ultra ?sec", "ultrasec"), (r"\bgold\b", "gold"), (r"\bfit\b", "fit"),
    (r"little swimmers", "little swimmers"), (r"total comfort|total confort", "total comfort"),
    (r"active fresh", "active fresh"), (r"soft ?conf\w*", "soft comfort"),
    (r"primdias|primeros dias", "primeros dias"), (r"ajustab\w*", "ajustable"), (r"disney", "disney"),
    (r"sin fragancia|sin perfume|fragrance free", "sin fragancia"), (r"sensitiv\w*|piel sensible", "sensitive"),
    (r"aloe", "aloe"), (r"manzanilla", "manzanilla"), (r"avena", "avena"),
    (r"\bpants\b|calzon de entrenamiento", "pants"), (r"\bnocturn\w*|overnite", "nocturno"),
    (r"cuidado total", "cuidado total"),
    # Líneas de toallitas / pañitos y otros rasgos que cambian el producto
    (r"manitos y carit\w*", "manitos y carita"), (r"cuidado 4 ?en ?1|\b4 ?en ?1\b", "4 en 1"),
    (r"limpieza cotidian\w*", "limpieza cotidiana"), (r"one ?(?:&|and)? ?done", "one&done"),
    (r"acolchamax", "acolchamax"), (r"vitamina e", "vitamina e"), (r"almendra", "almendra"),
    (r"dermo ?proteccion", "dermo proteccion"), (r"\brendi\w*", "rendi"), (r"maxi protect", "maxi protect"),
    (r"extra ?proteccion", "extraproteccion"), (r"\bkit\b", "kit"), (r"pack\s*x\s*\d+", "pack"),
]
# Talla normalizada (índice): RN=0, P=1, M=2, G=3, XG=4, XXG=5, XXXG=6.
TALLA_A_IDX = {"rn": 0, "p": 1, "m": 2, "g": 3, "gx": 4, "xg": 4, "xxg": 5, "xxxg": 6}
IDX_A_TALLA = {0: "RN", 1: "P", 2: "M", 3: "G", 4: "XG", 5: "XXG", 6: "XXXG"}
_TALLAS = r"(xxxg|xxg|xg|gx|g|m|p)"


def _sin_regalo(nombre: str) -> str:
    n = norm(nombre)
    _, pos = regalo_toallitas(nombre)
    if pos is None and " + " in n:
        pos = n.index(" + ")
    return n[:pos] if pos is not None else n


def extraer_marca(marca_original, nombre) -> str | None:
    texto = norm(f"{marca_original or ''} {nombre or ''}")
    for k, v in MARCAS.items():
        if re.search(rf"\b{k}\b", texto):
            return v
    return str(marca_original).strip().title() if marca_original else None


def extraer_variante(nombre, subcat: str | None = None) -> str | None:
    n = _sin_regalo(nombre)
    tokens = {canon for pat, canon in VARIANTES if re.search(pat, n)}
    if subcat == "pañitos húmedos" and re.search(r"recien nacido|\brn\b", n):
        tokens.add("recien nacido")
    regalo, _ = regalo_toallitas(nombre)
    if regalo:
        tokens.add(f"+{regalo} extra")
    elif " + " in norm(nombre):
        tokens.add("+combo")
    return " ".join(sorted(tokens)) or None


def extraer_etapa(nombre) -> int | None:
    """Devuelve el índice de talla normalizado (0=RN … 6=XXXG). La LETRA de talla manda sobre el número de etapa,
    porque en algunas líneas «Etapa 1» es RN y en otras es P."""
    n = _sin_regalo(nombre)
    if re.search(rf"etapa\s*\d\s*-\s*\d|\b\d\s*-\s*\d\s*/|\b{_TALLAS}\s*-\s*{_TALLAS}\b", n):
        return None                                   # rango de tallas (p. ej. 1-2/P-M): no es una talla única
    m = re.search(rf"\b[0-6]\s*/?\s*{_TALLAS}\b", n) or re.search(rf"\b{_TALLAS}\s*/\s*[0-6]\b", n)
    if m:
        letra = next(g for g in m.groups() if g and re.fullmatch(_TALLAS, g))
        return TALLA_A_IDX[letra]
    if re.search(r"recien nacido|\brn\b|rec/?nacid", n):
        return 0
    m = re.search(r"\bet(?:apa)?\s*([0-6])\b", n)
    if m:
        return int(m.group(1))
    m = re.search(r"\btalla\s*(rn|xxxg|xxg|xg|gx|g|m|p)\b", n)
    if m:
        return TALLA_A_IDX[m.group(1)]
    sin_cont = re.sub(r"\bx\s*\d+\s*(?:un\w*|u)?\b|\b\d+\s*(?:un\w*|u)\b", " ", n)
    m = re.search(rf"\b{_TALLAS}\b", sin_cont)
    return TALLA_A_IDX[m.group(1)] if m else None


_UNIDADES_CONTABLES = r"(?:und|unds|unid|unds?\.|unidad|unidades|uds?|un|u|panales|panal|toallitas|toallas|panitos|hojas|piezas|pz)"
_FACTOR = {"kg": ("g", 1000), "g": ("g", 1), "gr": ("g", 1), "grs": ("g", 1), "gramos": ("g", 1), "mg": ("g", 0.001),
           "l": ("ml", 1000), "lt": ("ml", 1000), "lts": ("ml", 1000), "litros": ("ml", 1000),
           "ml": ("ml", 1), "cc": ("ml", 1)}


def extraer_contenido(nombre: str, subcat: str | None):
    """Devuelve (contenido_total, unidad_base, unidades_por_paquete). unidad_base ∈ {unidad, g, ml}.
    En paquetes con regalo se cuenta solo el producto principal (el regalo va en la variante)."""
    t = _sin_regalo(nombre)
    if subcat in ("pañal", "pañitos húmedos"):
        t = re.sub(r"\bet(?:apa)?\s*\d+(\s*/\s*[a-z]+)?", " ", t)
        t = re.sub(r"talla\s*[a-z]+", " ", t)
        t = re.sub(rf"\b\d\s*/?\s*{_TALLAS}\b", " ", t)
        t = re.sub(rf"\b{_TALLAS}\s*/\s*\d\b", " ", t)
    if subcat == "crema de barrera":
        m = re.search(r"(\d+(?:[.,]\d+)?)\s*(kg|gramos|grs|gr|g|mg|ml|litros|lts|lt|l|cc)\b", t)
        if m:
            u, f = _FACTOR[m.group(2)]
            return round(float(m.group(1).replace(",", ".")) * f, 3), u, 1.0
    m = re.search(r"\b(\d+)\s*(?:pack\s*x|x|paquetes? de|packs? de)\s*(\d+)(?=\D|$)", t)   # multipaquete: 3 x 80, 12packx80u
    if m:
        a, b = int(m.group(1)), int(m.group(2))
        if a <= 12 and b >= 2:
            return float(a * b), "unidad", float(b)
    factor = 1
    mp = re.search(r"pack\s*x\s*(\d+)", t)                                        # "x100u packx2" = 2 paquetes de 100
    if mp:
        factor = int(mp.group(1))
    m = re.search(rf"(\d+)\s*{_UNIDADES_CONTABLES}\b", t) or re.search(r"\bx\s*(\d+)\b", t)
    if m:
        q = float(m.group(1))
        return q * factor, "unidad", q
    return None, None, None


def _cantidad_ficha(ficha: str | None):
    """Cantidad declarada en la ficha técnica del sitio (solo para detectar inconsistencias)."""
    if not ficha:
        return None
    m = re.search(r"cantidad\W{0,6}(\d+)", norm(ficha))
    return float(m.group(1)) if m else None


def construir_clave(r: dict) -> tuple[str | None, str | None]:
    """Devuelve (clave, grupo). None si faltan atributos necesarios para una correspondencia segura."""
    if not (r["marca_std"] and r["contenido_total"] and r["unidad_base"]):
        return None, None
    if r["subcategoria_std"] == "pañal" and r["etapa_std"] is None:
        return None, None
    partes = [norm(r["marca_std"]), r["subcategoria_std"], r["variante_std"] or "-",
              "e" + str(r["etapa_std"]) if r["etapa_std"] is not None else "-"]
    grupo = "|".join(partes)
    return f"{grupo}|{r['contenido_total']:g}{r['unidad_base']}", grupo


# ---------------------------------------------------------------------------------- proceso
def normalizar_anuncios(con: sqlite3.Connection) -> pd.DataFrame:
    """Llena los campos *_std, decide inclusión/exclusión y calcula la clave. Idempotente."""
    df = pd.read_sql_query("SELECT * FROM anuncios", con)
    for r in df.to_dict("records"):
        sub, motivo = clasificar(r["nombre_original"], r["categoria_sitio"] or "", r["descripcion_original"] or "")
        marca = extraer_marca(r["marca_original"], r["nombre_original"])
        variante = extraer_variante(r["nombre_original"], sub) if sub else None
        etapa = extraer_etapa(r["nombre_original"]) if sub == "pañal" else None
        cont, unidad, upp = extraer_contenido(r["nombre_original"], sub) if sub else (None, None, None)
        q_ficha = _cantidad_ficha(r["ficha_tecnica_original"])
        incons = int(bool(q_ficha and cont and unidad == "unidad" and abs(q_ficha - cont) > 0.5))
        reg = {"marca_std": marca, "subcategoria_std": sub, "variante_std": variante, "etapa_std": etapa,
               "contenido_total": cont, "unidad_base": unidad}
        clave, _ = construir_clave(reg) if sub else (None, None)
        con.execute("""UPDATE anuncios SET marca_std=?, subcategoria_std=?, variante_std=?, etapa_std=?, contenido_total=?,
                       unidad_base=?, unidades_por_paquete=?, incluido=?, motivo_descarte=?, inconsistencia_contenido=?, clave=?
                       WHERE anuncio_id=?""",
                    (marca, sub, variante, etapa, cont, unidad, upp, int(sub is not None), motivo, incons, clave, r["anuncio_id"]))
    con.commit()
    return pd.read_sql_query("SELECT * FROM anuncios", con)


def marcar_comparables(con: sqlite3.Connection, ejecucion_id: str, excluir_claves=(), forzar: dict | None = None,
                       minimo: int = 8, min_comercios: int = 3) -> pd.DataFrame:
    """
    Productos comparables = claves presentes (con precio) en TODOS los comercios de la ejecución.
    `forzar` {(clave_comercio, sku): clave} permite corregir manualmente una correspondencia;
    `excluir_claves` descarta claves que la revisión manual considere dudosas.
    Escribe productos, anuncios.producto_id y anuncios.comparable.
    """
    for (comercio, sku), clave in (forzar or {}).items():
        con.execute("""UPDATE anuncios SET clave=? WHERE sku=? AND comercio_id=(SELECT comercio_id FROM comercios WHERE clave=?)""",
                    (clave, str(sku), comercio))
    con.execute("UPDATE anuncios SET comparable=0, producto_id=NULL")
    con.execute("DELETE FROM productos")
    n_com = con.execute("SELECT COUNT(DISTINCT comercio_id) FROM ejecuciones WHERE ejecucion_id=? AND estado!='fallida'",
                        (ejecucion_id,)).fetchone()[0]
    if n_com < min_comercios:
        con.commit()
        print(f"ADVERTENCIA: la ejecución {ejecucion_id} tiene datos de {n_com} comercio(s); se requieren {min_comercios}. "
              "No se marcó ningún producto como comparable. Revise `ejecuciones` (estado/notas) y vuelva a recolectar.")
        return tabla_revision(con)
    comunes = pd.read_sql_query("""
        SELECT a.clave, COUNT(DISTINCT o.comercio_id) AS n_comercios
        FROM observaciones o JOIN anuncios a ON a.anuncio_id=o.anuncio_id
        WHERE o.ejecucion_id=? AND o.precio IS NOT NULL AND o.disponible=1 AND a.incluido=1 AND a.clave IS NOT NULL
        GROUP BY a.clave HAVING n_comercios=?""", con, params=(ejecucion_id, n_com))
    claves = [c for c in comunes["clave"] if c not in set(excluir_claves)]
    for clave in claves:
        a = con.execute("SELECT marca_std, subcategoria_std, variante_std, etapa_std, contenido_total, unidad_base "
                        "FROM anuncios WHERE clave=? LIMIT 1", (clave,)).fetchone()
        grupo = clave.rsplit("|", 1)[0]
        nombre = " ".join(x for x in [a[0], a[1], a[2] if a[2] else None,
                                      f"talla {IDX_A_TALLA[a[3]]}" if a[3] is not None else None,
                                      f"x {a[4]:g} {a[5]}"] if x)
        cur = con.execute("INSERT INTO productos(clave, grupo, nombre_std, marca_std, subcategoria_std, variante_std, etapa_std,"
                          "contenido_total, unidad_base) VALUES (?,?,?,?,?,?,?,?,?)", (clave, grupo, nombre, *a))
        con.execute("UPDATE anuncios SET producto_id=?, comparable=1 WHERE clave=?", (cur.lastrowid, clave))
    con.commit()
    if len(claves) < minimo:
        print(f"ADVERTENCIA: solo {len(claves)} productos comunes a los {n_com} comercios (mínimo {minimo}). "
              "Cambie algún comercio o amplíe las consultas.")
    return tabla_revision(con)


def tabla_revision(con: sqlite3.Connection) -> pd.DataFrame:
    """Auditoría de la correspondencia: nombre original de cada comercio lado a lado."""
    df = pd.read_sql_query("""
        SELECT p.producto_id, p.nombre_std, c.nombre AS comercio, a.sku, a.nombre_original, a.url
        FROM anuncios a JOIN productos p ON p.producto_id=a.producto_id JOIN comercios c ON c.comercio_id=a.comercio_id
        ORDER BY p.producto_id, c.nombre""", con)
    return df


def resumen_descartes(con: sqlite3.Connection) -> pd.DataFrame:
    return pd.read_sql_query("""
        SELECT c.nombre AS comercio, COALESCE(a.motivo_descarte, 'incluido') AS motivo, COUNT(*) AS n
        FROM anuncios a JOIN comercios c ON c.comercio_id=a.comercio_id
        GROUP BY 1, 2 ORDER BY 1, n DESC""", con)
