"""
procesamiento.py - Limpieza, normalización, correspondencia y modo sin conexión.

Trabaja SOBRE lo que deja `scraper.py` (tablas `observaciones` y `ejecuciones`) sin modificarlo.
Todo lo derivado se guarda en tablas nuevas de la misma base SQLite:

    comercios, anuncios, precios, productos_comunes, exclusiones

El texto original (`observaciones.texto_original`) nunca se modifica.
"""
from __future__ import annotations

import csv
import html
import re
import sqlite3
import unicodedata
from datetime import datetime, timezone, timedelta
from difflib import SequenceMatcher
from pathlib import Path
from typing import Optional
from urllib.parse import urlsplit, urlunsplit

TZ_COL = timezone(timedelta(hours=-5))

# ---------------------------------------------------------------------------
# 1. Precios
# ---------------------------------------------------------------------------
def precio_colombiano(txt) -> Optional[float]:
    """'$ 12.500' | '12.500,00' | 12500.0  ->  12500.0   (None si no hay precio)."""
    if txt is None:
        return None
    if isinstance(txt, (int, float)):
        return float(txt) if txt > 0 else None
    s = re.sub(r"[^\d.,]", "", str(txt))
    if not s:
        return None
    if "," in s and "." in s:
        s = s.replace(".", "").replace(",", ".")
    elif "," in s:
        s = s.replace(",", ".") if re.search(r",\d{1,2}$", s) else s.replace(",", "")
    elif re.search(r"\.\d{3}$", s):
        s = s.replace(".", "")
    try:
        v = float(s)
    except ValueError:
        return None
    return v if v > 0 else None


def precios_desde_texto(txt: str) -> tuple[Optional[float], Optional[float]]:
    """Del texto de una tarjeta ('$32.000 $24.000 $361.667 por Mililitro') devuelve
    (precio_actual, precio_lista). Usa los DOS primeros valores con '$'; el menor es el actual."""
    txt = re.split(r"mililitro|gramo|unidad|por\s", txt or "", flags=re.I)[0]
    toks = re.findall(r"\$\s*\d[\d.,]*", txt)
    vals = [v for v in (precio_colombiano(t) for t in toks[:2]) if v]
    if not vals:
        v = precio_colombiano(txt)
        return (v, None)
    if len(vals) == 1:
        return vals[0], None
    lo, hi = min(vals), max(vals)
    return (lo, hi) if lo < hi else (lo, None)


def separar_precios(valor: Optional[float]) -> tuple[Optional[float], Optional[float], str]:
    """Repara precios que quedaron con los dígitos de varios valores pegados.

    El scraper conservaba solo los dígitos del texto: '$32.000 $24.000' -> 3200024000.
    Regla: si el número tiene más de 6 dígitos se busca la partición en dos precios
    (4-6 dígitos cada uno, sin ceros a la izquierda, razón menor/mayor >= 0.4) que consuma
    más dígitos. El menor es el precio actual y el mayor el precio antes del descuento.
    Devuelve (precio, precio_lista, regla)."""
    if valor is None or valor <= 0:
        return None, None, "sin_precio"
    d = str(int(round(valor)))
    if len(d) <= 6:
        return float(valor), None, "directo"
    mejores = []
    for i in range(4, 7):
        for j in range(4, 7):
            if i + j > len(d):
                continue
            a, b = d[:i], d[i:i + j]
            if a[0] == "0" or b[0] == "0":
                continue
            A, B = int(a), int(b)
            lo, hi = min(A, B), max(A, B)
            if lo / hi < 0.4:
                continue
            mejores.append((-(i + j), abs(i - j), -(lo / hi), lo, hi))
    if not mejores:
        return None, None, "irrecuperable"
    _, _, _, lo, hi = sorted(mejores)[0]
    return float(lo), (float(hi) if hi > lo else None), "separado_en_dos"


# ---------------------------------------------------------------------------
# 2. Normalización de texto, marca, contenido, formato
# ---------------------------------------------------------------------------
def quitar_tildes(s: str) -> str:
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn")


def norm(s: Optional[str]) -> str:
    s = quitar_tildes(html.unescape(s or "")).lower()
    s = re.sub(r"[^a-z0-9%+.,x ]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


def canon_url(url: Optional[str]) -> Optional[str]:
    """Quita parámetros de seguimiento (?_pos=1&_sid=...) para identificar el anuncio."""
    if not url:
        return None
    p = urlsplit(url)
    return urlunsplit((p.scheme, p.netloc, p.path, "", ""))


MARCAS = [  # orden importa: 'stay off' antes que 'off'
    ("STAY OFF", r"stay\s*off"), ("OFF", r"\boff!?(?=\s|$)"), ("AUTAN", r"\bautan\b"),
    ("NOPIKEX", r"nopikex"), ("PICOSIN", r"picosin"), ("MOSKIU", r"moskiu"),
    ("BACTERION", r"bacterion"), ("FLY OUT", r"fly\s*out"), ("FLY FREE", r"fly\s*free"),
    ("ECOTU", r"ecotu"), ("WELLNEZZ", r"wellnezz|mosqoff"), ("JOHNSON'S BABY", r"johnson"),
    ("CALADERM", r"caladerm"), ("BOTANITAS", r"botanitas"), ("OUT", r"mosquitos? out|\bout\b"),
    ("ZECTO", r"zecto"), ("RAID", r"\braid\b"), ("BAYGON", r"baygon"),
]


def inferir_marca(nombre: Optional[str]) -> tuple[Optional[str], Optional[str]]:
    """(marca_estandarizada, texto_de_la_marca_encontrado). La base no trae marca: se infiere del nombre."""
    n = norm(nombre)
    for canon, rx in MARCAS:
        m = re.search(rx, n)
        if m:
            return canon, m.group(0)
    return None, None


_UNIT = {"ml": ("ml", 1), "cc": ("ml", 1), "l": ("ml", 1000), "lt": ("ml", 1000), "lts": ("ml", 1000),
         "litro": ("ml", 1000), "litros": ("ml", 1000), "g": ("g", 1), "gr": ("g", 1), "grs": ("g", 1),
         "grm": ("g", 1), "grms": ("g", 1), "gramo": ("g", 1), "gramos": ("g", 1), "kg": ("g", 1000),
         "mg": ("g", 0.001), "oz": ("g", 28.3495)}
RX_CONT = re.compile(r"(?:(?P<mult>\d{1,3})\s*[xX]\s*)?(?P<val>\d{1,4}(?:[.,]\d{1,2})?)\s*"
                     r"(?P<un>ml|cc|lts?|litros?|l|gramos?|grms?|grs?|gr|g|kg|mg|oz)\b", re.I)
RX_UND = re.compile(r"(\d{1,3})\s*(?:unidades|unds?|uds?|tabletas?|pastillas?|parches|toallitas?|"
                    r"pulseras?|espirales|varillas|sobres|discos|velas|stickers?|stcker)\b", re.I)
RX_PCT = re.compile(r"(\d{1,3}(?:[.,]\d{1,2})?)\s*%")


def es_combo(nombre: Optional[str]) -> bool:
    """Paquetes de productos distintos ('120 ml + Crema 60 gr'): no son una presentación única."""
    return bool(re.search(r"\s\+\s|\bcombo\b|\bkit\b|\bpack\b", html.unescape(nombre or ""), re.I))


def parse_contenido(texto: Optional[str]) -> dict:
    t = RX_PCT.sub(" ", html.unescape(texto or ""))
    out = dict(valor=None, unidad=None, n_unidades=1, contenido_total=None, unidad_base=None)
    m = RX_CONT.search(t)
    if m:
        un = m.group("un").lower()
        base, f = _UNIT[un]
        val = float(m.group("val").replace(",", "."))
        n = int(m.group("mult")) if m.group("mult") else 1
        out.update(valor=val, unidad=un, n_unidades=n, contenido_total=round(val * f * n, 3), unidad_base=base)
        return out
    u = RX_UND.search(t)
    if u:
        n = int(u.group(1))
        out.update(valor=n, unidad="und", contenido_total=float(n), unidad_base="und")
    return out


def parse_concentracion(texto: Optional[str]) -> Optional[float]:
    m = RX_PCT.search(html.unescape(texto or ""))
    if not m:
        return None
    v = float(m.group(1).replace(",", "."))
    return v if 0 < v <= 100 else None


_FORMATOS = [(r"\broll ?on\b", "roll-on"), (r"\b(spray|sprayx\w*|aerosol|atomizador)\b", "spray"),
             (r"\b(toallitas?)\b", "toallitas"), (r"\b(sticker|stcker|parche)s?\b", "parches"),
             (r"\b(gel)\b", "gel"), (r"\b(locion|lotion)\b", "locion"), (r"\b(crema)\b", "crema"),
             (r"\b(barra|stick)\b", "barra"), (r"\bjabon\b", "jabon"), (r"\b(espiral|pastilla|tableta)s?\b", "dispositivo")]


def formato_std(nombre: Optional[str]) -> Optional[str]:
    n = norm(nombre)
    for rx, v in _FORMATOS:
        if re.search(rx, n):
            return v
    return None


def publico_std(nombre: Optional[str]) -> str:
    n = norm(nombre)
    if re.search(r"\b(ninos?|kids?|bebes?|baby|infantil)\b", n):
        return "infantil"
    return "general"


STOP = set("""repelente repelentes de del la el en y con para sin por al los las un una x ml g gr grm und
unidades und antimosquito antimosquitos mosquitos mosquito insectos insecto spray crema gel locion barra
rollon roll on aerosol sprayx super oferta""".split())


def tokens_linea(nombre: Optional[str], marca_txt: Optional[str]) -> list[str]:
    n = norm(nombre)
    if marca_txt:
        n = n.replace(marca_txt, " ")
    n = RX_PCT.sub(" ", n)
    n = re.sub(r"\d+([.,]\d+)?\s*(ml|cc|l|lt|g|gr|grm|kg|mg|oz|und|unidades)?\b", " ", n)
    n = re.sub(r"\b(ninos?|kids?|bebes?|baby|infantil)\b", " ", n)   # el público va aparte
    return sorted({w for w in re.findall(r"[a-z]+", n) if w not in STOP and len(w) > 2})


def similitud(a: list[str], b: list[str]) -> float:
    """Proporción de tokens de la línea de producto que coinciden (con tolerancia a variantes ortográficas)."""
    if not a and not b:
        return 1.0
    if not a or not b:
        return 0.0
    hit = sum(1 for x in a if any(SequenceMatcher(None, x, y).ratio() >= 0.85 for y in b))
    return hit / max(len(a), len(b))


# ---------------------------------------------------------------------------
# 3. Pertenencia a la categoría 42 (delimitación de la planilla)
# ---------------------------------------------------------------------------
RX_INCLUIR = re.compile(r"\b(repelentes?|insecticidas?|antimosquitos?|mosquit\w*|zancud\w*|cucarach\w*)\b", re.I)
RX_EXCLUIR = [
    ("producto_para_mascotas", re.compile(r"\b(perros?|gatos?|mascotas?|antipulgas|pulgas|garrapatas?)\b", re.I)),
    ("medicamento_picaduras_o_piojos", re.compile(r"\b(piojos?|pediculicida\w*|picaduras?|after ?bite|antihistam\w*)\b", re.I)),
    ("repelente_no_insectos", re.compile(r"\b(repelente al agua|impermeab\w*|grasa|lubricante|antipolilla)\b", re.I)),
]


def clasificar(nombre: Optional[str]) -> tuple[int, str]:
    t = nombre or ""
    for motivo, rx in RX_EXCLUIR:
        if rx.search(t):
            return 0, motivo
    if not RX_INCLUIR.search(t):
        return 0, "sin_indicio_de_repelente"
    return 1, "ok"


# ---------------------------------------------------------------------------
# 4. Tablas derivadas
# ---------------------------------------------------------------------------
SCHEMA = """
DROP TABLE IF EXISTS exclusiones; DROP TABLE IF EXISTS precios; DROP TABLE IF EXISTS anuncios;
DROP TABLE IF EXISTS productos_comunes; DROP TABLE IF EXISTS comercios;

CREATE TABLE comercios (id INTEGER PRIMARY KEY, nombre TEXT UNIQUE NOT NULL);

CREATE TABLE productos_comunes (
    id INTEGER PRIMARY KEY AUTOINCREMENT, descripcion TEXT, marca TEXT, formato TEXT, publico TEXT,
    contenido_total REAL, unidad_base TEXT, n_comercios INTEGER, comparable INTEGER NOT NULL DEFAULT 0, regla TEXT);

CREATE TABLE anuncios (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    comercio_id INTEGER NOT NULL REFERENCES comercios(id),
    url TEXT,                                -- sin parámetros de seguimiento
    texto_original TEXT NOT NULL,            -- tal como se publicó
    marca TEXT, formato TEXT, publico TEXT, linea TEXT,
    concentracion REAL, contenido_valor REAL, contenido_unidad TEXT, n_unidades INTEGER,
    contenido_total REAL, unidad_base TEXT, contenido_fuente TEXT,     -- 'nombre' | 'manual'
    es_combo INTEGER NOT NULL DEFAULT 0,
    pertenece INTEGER NOT NULL DEFAULT 1,    -- 1 = pertenece a la categoría 42
    producto_comun_id INTEGER REFERENCES productos_comunes(id),
    comparable INTEGER NOT NULL DEFAULT 0,
    UNIQUE (comercio_id, url));

CREATE TABLE precios (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    anuncio_id INTEGER NOT NULL REFERENCES anuncios(id),
    obs_id INTEGER REFERENCES observaciones(id),
    id_ejecucion TEXT NOT NULL,
    fecha_hora TEXT NOT NULL, fecha TEXT NOT NULL, hora TEXT NOT NULL,   -- ISO 8601 -05:00
    precio REAL, precio_lista REAL, promocion INTEGER,
    valor_original REAL,                     -- lo que guardó el scraper
    regla_precio TEXT NOT NULL,              -- directo | separado_en_dos | irrecuperable | sin_precio
    comparable INTEGER NOT NULL DEFAULT 0,
    UNIQUE (anuncio_id, fecha_hora));

CREATE TABLE exclusiones (
    id INTEGER PRIMARY KEY AUTOINCREMENT, obs_id INTEGER, comercio TEXT, texto_original TEXT,
    url TEXT, motivo TEXT NOT NULL);
"""


def reconstruir_base(conn: sqlite3.Connection) -> dict:
    """Crea las tablas derivadas desde `observaciones`. Se puede repetir las veces que haga falta."""
    conn.executescript(SCHEMA)
    obs = conn.execute("""SELECT id, comercio, precio, url_extraccion, texto_original,
                                 fecha_hora_consulta, id_ejecucion, promocion FROM observaciones ORDER BY id""").fetchall()
    resumen = {"observaciones": len(obs), "anuncios": 0, "excluidas": 0, "precio_reparado": 0, "precio_irrecuperable": 0}
    for (oid, comercio, valor, url, texto, fh, ide, promo_orig) in obs:
        cid = conn.execute("SELECT id FROM comercios WHERE nombre=?", (comercio,)).fetchone()
        if cid is None:
            cid = conn.execute("INSERT INTO comercios(nombre) VALUES (?)", (comercio,)).lastrowid
        else:
            cid = cid[0]
        pert, motivo = clasificar(texto)
        u = canon_url(url)
        if not pert:
            conn.execute("INSERT INTO exclusiones(obs_id, comercio, texto_original, url, motivo) VALUES (?,?,?,?,?)",
                         (oid, comercio, texto, u, motivo))
            resumen["excluidas"] += 1
            continue
        marca, marca_txt = inferir_marca(texto)
        cont = parse_contenido(texto)
        combo = es_combo(texto)
        if combo:                      # varias piezas distintas: no hay contenido único
            cont = dict(valor=None, unidad=None, n_unidades=1, contenido_total=None, unidad_base=None)
        conn.execute("""INSERT OR IGNORE INTO anuncios (comercio_id, url, texto_original, marca, formato, publico,
                        linea, concentracion, contenido_valor, contenido_unidad, n_unidades, contenido_total,
                        unidad_base, contenido_fuente, es_combo) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)""",
                     (cid, u, texto, marca, formato_std(texto), publico_std(texto),
                      " ".join(tokens_linea(texto, marca_txt)), parse_concentracion(texto), cont["valor"],
                      cont["unidad"], cont["n_unidades"], cont["contenido_total"], cont["unidad_base"],
                      "nombre" if cont["contenido_total"] else None, int(combo)))
        aid = conn.execute("SELECT id FROM anuncios WHERE comercio_id=? AND url IS ?", (cid, u)).fetchone()[0]
        precio, lista, regla = separar_precios(valor)
        if regla == "separado_en_dos":
            resumen["precio_reparado"] += 1
        if regla == "irrecuperable":
            resumen["precio_irrecuperable"] += 1
        promo = None if precio is None else int(lista is not None and lista > precio)
        conn.execute("""INSERT OR IGNORE INTO precios (anuncio_id, obs_id, id_ejecucion, fecha_hora, fecha, hora,
                        precio, precio_lista, promocion, valor_original, regla_precio) VALUES (?,?,?,?,?,?,?,?,?,?,?)""",
                     (aid, oid, ide, fh, fh[:10], fh[11:19], precio, lista, promo, valor, regla))
    conn.commit()
    resumen["anuncios"] = conn.execute("SELECT COUNT(*) FROM anuncios").fetchone()[0]
    return resumen


def completar_contenido(conn: sqlite3.Connection, manual: dict) -> int:
    """Completa contenido verificado a mano en la página del producto: {url: (valor, 'ml'|'g'|'und')}.
    Queda marcado como contenido_fuente='manual' (trazabilidad)."""
    n = 0
    for url, (valor, unidad) in manual.items():
        un = unidad.lower()
        base, f = _UNIT.get(un, (un, 1))
        cur = conn.execute("""UPDATE anuncios SET contenido_valor=?, contenido_unidad=?, n_unidades=1,
                              contenido_total=?, unidad_base=?, contenido_fuente='manual' WHERE url=?""",
                           (valor, un, round(valor * f, 3), base, canon_url(url)))
        n += cur.rowcount
    conn.commit()
    return n


# ---------------------------------------------------------------------------
# 5. Correspondencia entre comercios
# ---------------------------------------------------------------------------
def correspondencia(conn: sqlite3.Connection, comercios: list[str], umbral: float = 0.7) -> int:
    """Agrupa el MISMO producto y presentación entre comercios. Reglas (documentadas):
    1) misma marca; 2) mismo formato (spray, crema...); 3) mismo público (infantil/general);
    4) mismo contenido total y unidad base (ambos con contenido, sin combos);
    5) similitud de la línea de producto >= umbral (tokens sin marca/contenido/palabras vacías);
    6) un anuncio por comercio por grupo. Comparable = 1 si el grupo cubre TODOS los comercios dados.
    Devuelve el número de productos comunes comparables."""
    conn.execute("UPDATE anuncios SET producto_comun_id=NULL, comparable=0")
    conn.execute("UPDATE precios SET comparable=0")
    conn.execute("DELETE FROM productos_comunes")
    ph = ",".join("?" * len(comercios))
    rows = conn.execute(f"""
        SELECT a.*, c.nombre AS comercio FROM anuncios a JOIN comercios c ON c.id=a.comercio_id
        WHERE a.pertenece=1 AND a.es_combo=0 AND a.marca IS NOT NULL AND a.contenido_total IS NOT NULL
          AND c.nombre IN ({ph})
          AND EXISTS (SELECT 1 FROM precios p WHERE p.anuncio_id=a.id AND p.precio IS NOT NULL)""", comercios).fetchall()
    cols = [d[0] for d in conn.execute("SELECT a.*, '' AS comercio FROM anuncios a LIMIT 0").description]
    items = [dict(zip(cols, r)) for r in rows]
    bloques: dict[tuple, list] = {}
    for it in items:
        k = (it["marca"], it["publico"], round(it["contenido_total"], 1), it["unidad_base"])
        bloques.setdefault(k, []).append(it)
    n_comp = 0
    for (marca, publico, total, unidad), lista in bloques.items():
        por_c: dict[str, list] = {}
        for it in lista:
            por_c.setdefault(it["comercio"], []).append(it)
        if len(por_c) < 2:
            continue
        usados: set[int] = set()
        ancla_c = max(por_c, key=lambda c: len(por_c[c]))
        for ancla in por_c[ancla_c]:
            ta = ancla["linea"].split()
            grupo = {ancla_c: (ancla, 1.0)}
            for c, cands in por_c.items():
                if c == ancla_c:
                    continue
                mejor, ms = None, 0.0
                for cd in cands:
                    if cd["id"] in usados:
                        continue
                    if cd["formato"] and ancla["formato"] and cd["formato"] != ancla["formato"]:
                        continue
                    s = similitud(ta, cd["linea"].split())
                    if s > ms:
                        mejor, ms = cd, s
                if mejor is not None and ms >= umbral:
                    grupo[c] = (mejor, ms)
            if len(grupo) < 2:
                continue
            comparable = int(all(c in grupo for c in comercios))
            desc = f"{marca} {ancla['formato'] or ''} {ancla['linea']} {total:g} {unidad} ({publico})".replace("  ", " ")
            pcid = conn.execute("""INSERT INTO productos_comunes (descripcion, marca, formato, publico, contenido_total,
                                   unidad_base, n_comercios, comparable, regla) VALUES (?,?,?,?,?,?,?,?,?)""",
                                (desc, marca, ancla["formato"], publico, total, unidad, len(grupo), comparable,
                                 f"marca+formato+publico+contenido+similitud>={umbral}")).lastrowid
            for c, (a, s) in grupo.items():
                usados.add(a["id"])
                conn.execute("UPDATE anuncios SET producto_comun_id=?, comparable=? WHERE id=?", (pcid, comparable, a["id"]))
            n_comp += comparable
    conn.execute("UPDATE precios SET comparable=1 WHERE anuncio_id IN (SELECT id FROM anuncios WHERE comparable=1)")
    conn.commit()
    return n_comp


def marcar_comparables(conn: sqlite3.Connection, ids: list[int]) -> None:
    """Deja comparable=1 solo en los productos comunes elegidos tras la revisión manual."""
    ph = ",".join("?" * len(ids))
    conn.execute("UPDATE productos_comunes SET comparable=0")
    conn.execute("UPDATE anuncios SET comparable=0")
    conn.execute("UPDATE precios SET comparable=0")
    conn.execute(f"UPDATE productos_comunes SET comparable=1 WHERE id IN ({ph})", ids)
    conn.execute(f"UPDATE anuncios SET comparable=1 WHERE producto_comun_id IN ({ph})", ids)
    conn.execute("UPDATE precios SET comparable=1 WHERE anuncio_id IN (SELECT id FROM anuncios WHERE comparable=1)")
    conn.commit()


def exportar_candidatos(conn: sqlite3.Connection, ruta="candidatos_correspondencia.csv") -> Path:
    """CSV de revisión manual: (1) grupos encontrados y (2) anuncios SIN contenido que podrían completarse a mano."""
    ruta = Path(ruta)
    with ruta.open("w", newline="", encoding="utf-8-sig") as fh:
        w = csv.writer(fh)
        w.writerow(["tipo", "producto_comun", "comparable", "comercio", "texto_original", "marca", "formato",
                    "publico", "contenido_total", "unidad_base", "precio", "url"])
        for r in conn.execute("""SELECT 'grupo', a.producto_comun_id, a.comparable, c.nombre, a.texto_original, a.marca,
                a.formato, a.publico, a.contenido_total, a.unidad_base,
                (SELECT precio FROM precios p WHERE p.anuncio_id=a.id ORDER BY fecha_hora DESC LIMIT 1), a.url
                FROM anuncios a JOIN comercios c ON c.id=a.comercio_id WHERE a.producto_comun_id IS NOT NULL
                ORDER BY a.comparable DESC, a.producto_comun_id, c.nombre"""):
            w.writerow(list(r))
        for r in conn.execute("""SELECT 'sin_contenido', NULL, 0, c.nombre, a.texto_original, a.marca, a.formato,
                a.publico, NULL, NULL,
                (SELECT precio FROM precios p WHERE p.anuncio_id=a.id ORDER BY fecha_hora DESC LIMIT 1), a.url
                FROM anuncios a JOIN comercios c ON c.id=a.comercio_id
                WHERE a.pertenece=1 AND a.es_combo=0 AND a.contenido_total IS NULL AND a.marca IS NOT NULL
                ORDER BY a.marca, c.nombre"""):
            w.writerow(list(r))
    return ruta


# ---------------------------------------------------------------------------
# 6. Modo sin conexión: reprocesar raw/ (HTML guardado por scraper.py)
# ---------------------------------------------------------------------------
SELECTORES = {
    "Droguerias Uno A": dict(base="https://drogueriasunoa.com", contenedor="div.product-details",
                             nombre="div.product-title h4 a", precio="div.product-price h6", enlace="div.product-title h4 a"),
    "Colsubsidio": dict(base="https://www.drogueriascolsubsidio.com", contenedor="div.ProductItemCard__info--wrap",
                        nombre="div.ProductItemCard__info--wrap__title",
                        precio="div.ProductItemCard__info--wrap__prices--inner", enlace=None),
    "Locatel": dict(base="https://www.locatelcolombia.com", contenedor="section.vtex-product-summary-2-x-container",
                    nombre="span.vtex-product-summary-2-x-productBrand",
                    precio="span.vtex-product-price-1-x-sellingPriceValue", enlace="a.vtex-product-summary-2-x-clearLink"),
}


def leer_raw(raw_dir: str | Path, comercios: Optional[list[str]] = None) -> list[dict]:
    """Reprocesa las páginas HTML de raw/<comercio>/<fecha>/pagina_<epoch>.html SIN consultar los sitios.
    Usa los mismos selectores que scraper.py y extrae precio y precio de lista del texto de la tarjeta."""
    from bs4 import BeautifulSoup
    out = []
    raw_dir = Path(raw_dir)
    for com, sel in SELECTORES.items():
        if comercios and com not in comercios:
            continue
        for f in sorted((raw_dir / com).glob("*/pagina_*.html")):
            m = re.search(r"pagina_(\d+)", f.name)
            ts = datetime.fromtimestamp(int(m.group(1)), TZ_COL).isoformat() if m else None
            soup = BeautifulSoup(f.read_bytes().decode("utf-8", "ignore"), "html.parser")
            for item in soup.select(sel["contenedor"]):
                n = item.select_one(sel["nombre"])
                p = item.select_one(sel["precio"])
                if not n or not p:
                    continue
                href = ""
                if sel["enlace"]:
                    a = item.select_one(sel["enlace"])
                    href = a.get("href", "") if a else ""
                else:
                    href = item.get("data-event-link-url", "")
                if href.startswith("/"):
                    href = sel["base"] + href
                precio, lista = precios_desde_texto(p.get_text(" ", strip=True))
                if not precio:
                    continue
                out.append(dict(comercio=com, texto_original=n.get_text(strip=True), precio=precio,
                                precio_lista=lista, url=href, fecha_hora=ts, archivo=str(f)))
    return out


def cargar_raw_en_db(raw_dir: str | Path, db_path: str | Path, id_ejecucion: str = "offline") -> int:
    """Modo sin conexión: reconstruye `observaciones` y `ejecuciones` en `db_path` desde raw/ (con precios bien
    separados). Útil si el sitio cambió o bloquea. Devuelve el número de observaciones insertadas."""
    regs = leer_raw(raw_dir)
    conn = sqlite3.connect(str(db_path))
    conn.executescript("""
    CREATE TABLE IF NOT EXISTS ejecuciones (id INTEGER PRIMARY KEY AUTOINCREMENT, id_ejecucion TEXT, fecha_inicio TEXT,
        comercio TEXT, registros_obtenidos INTEGER, estado TEXT);
    CREATE TABLE IF NOT EXISTS observaciones (id INTEGER PRIMARY KEY AUTOINCREMENT, comercio TEXT, producto TEXT,
        marca_subcategoria TEXT, presentacion TEXT, precio REAL, promocion INTEGER, url_extraccion TEXT,
        texto_original TEXT, fecha_hora_consulta TEXT, comparable INTEGER DEFAULT 0, id_ejecucion TEXT);""")
    conn.execute("DELETE FROM observaciones WHERE id_ejecucion=?", (id_ejecucion,))
    conn.execute("DELETE FROM ejecuciones WHERE id_ejecucion=?", (id_ejecucion,))
    for r in regs:
        conn.execute("""INSERT INTO observaciones (comercio, marca_subcategoria, precio, promocion, url_extraccion,
                        texto_original, fecha_hora_consulta, id_ejecucion) VALUES (?,?,?,?,?,?,?,?)""",
                     (r["comercio"], "Por clasificar", r["precio"], int(bool(r["precio_lista"])), r["url"],
                      r["texto_original"], r["fecha_hora"], id_ejecucion))
    for com in {r["comercio"] for r in regs}:
        n = sum(1 for r in regs if r["comercio"] == com)
        conn.execute("INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado) "
                     "VALUES (?,?,?,?,?)", (id_ejecucion, datetime.now(TZ_COL).isoformat(), com, n, "offline"))
    conn.commit()
    conn.close()
    return len(regs)
