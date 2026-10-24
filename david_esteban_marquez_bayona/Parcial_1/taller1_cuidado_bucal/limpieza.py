"""
limpieza.py — Normalización y correspondencia de productos.
Taller 1 · Minería de Datos (2016325) · UNAL · Categoría: cuidado bucal.

Este módulo NO consulta ningún sitio: trabaja sobre los campos originales que
`scraper.py` guarda en SQLite y escribe las versiones estandarizadas en campos
propios (nunca modifica el texto original).

Reglas documentadas (se citan como R1…R9 en el informe):

  R1  Texto: minúsculas, sin tildes, coma decimal -> punto, sin signos.
  R2  Marca: se busca primero una marca conocida en el nombre del anuncio
      (MARCAS); si no aparece se usa el campo marca del comercio.
  R3  Subcategoría: expresiones regulares en orden de prioridad sobre
      nombre + ruta de categoría del comercio (SUBCATEGORIAS).
  R4  Exclusión: anuncios que, aun estando en la sección del comercio, no
      pertenecen a cuidado bucal según la delimitación (EXCLUSIONES).
  R5  Contenido: se extrae del nombre (o de la ficha técnica si el nombre no
      lo trae) y se convierte a mL, g o m (1 L = 1000 mL, 1 kg = 1000 g,
      1 g = 1000 mg, 1 yd = 0,9144 m). Número de unidades del empaque
      ("x 3 und", "3 x 75 ml", "pack x2", "lleve 3 pague 2"). Contenido
      total = contenido unitario × unidades. En cepillos la unidad es "und".
  R6  Concentración de flúor (ppm) o de principio activo (%) si aparece.
  R7  Familia: nombre sin tamaño, empaque, marca ni palabras genéricas;
      tokens ordenados. Agrupa presentaciones del mismo producto. Las
      familias de distintos comercios se unen cuando comparten un EAN.
  R8  Correspondencia entre comercios, en este orden:
        (a) manual: archivo correspondencias_manuales.csv;
        (b) código de barras EAN/GTIN con dígito de control válido, separado
            por número de unidades si un comercio usa el EAN unitario en un
            paquete;
        (c) clave normalizada subcategoría|marca|familia|contenido|unidades|ppm.
  R9  Comparable: presentación con precio disponible en TODOS los comercios
      de la ejecución de referencia.
"""
from __future__ import annotations

import csv
import hashlib
import re
import sqlite3
import unicodedata
from collections import Counter, defaultdict
from pathlib import Path

RAIZ = Path(__file__).resolve().parent
ARCHIVO_MANUAL = RAIZ / "correspondencias_manuales.csv"

# ---------------------------------------------------------------------------
# R1 · Texto
# ---------------------------------------------------------------------------

def normalizar_texto(s) -> str:
    """Minúsculas, sin tildes ni signos; conserva números decimales, % y +."""
    if s is None:
        return ""
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(ch for ch in s if not unicodedata.combining(ch)).lower()
    s = re.sub(r"<[^>]+>", " ", s)                 # etiquetas HTML de descripciones
    s = re.sub(r"(\d),(\d)", r"\1.\2", s)          # coma decimal
    s = s.replace("-", " ")
    s = re.sub(r"[^a-z0-9.%+ ]+", " ", s)
    s = re.sub(r"(?<!\d)\.|\.(?!\d)", " ", s)      # puntos que no son decimales
    # Protege números que forman parte del nombre comercial
    s = re.sub(r"\btotal\s*12\b", "total12", s)
    s = re.sub(r"\b3\s*d\b", "3d", s)
    return re.sub(r"\s+", " ", s).strip()


# ---------------------------------------------------------------------------
# R2 · Marcas
# ---------------------------------------------------------------------------
# patrón normalizado -> marca canónica (se prueban primero los más largos)
MARCAS = {
    "colgate palmolive": "Colgate", "colgate": "Colgate", "plax": "Colgate",
    "periogard": "Colgate", "oral b": "Oral-B", "oralb": "Oral-B",
    "sensodyne": "Sensodyne", "parodontax": "Parodontax", "listerine": "Listerine",
    "close up": "Close Up", "closeup": "Close Up", "fortident": "Fortident",
    "pepsodent": "Pepsodent", "crest": "Crest", "aquafresh": "Aquafresh",
    "sunstar gum": "GUM", "gum": "GUM", "corega": "Corega", "fixodent": "Fixodent",
    "polident": "Polident", "kin": "Kin", "vitis": "Vitis", "bexident": "Bexident",
    "lacer": "Lacer", "elmex": "Elmex", "meridol": "Meridol", "curaprox": "Curaprox",
    "tepe": "TePe", "philips": "Philips", "sonicare": "Philips", "kolynos": "Kolynos",
    "biotene": "Biotène", "perioaid": "PerioAid", "perio aid": "PerioAid",
    "fluocaril": "Fluocaril", "signal": "Signal", "dentiplus": "Dentiplus",
    "encident": "Encident", "johnson": "Johnson's", "hexident": "Hexident",
    "clorhexidina": None,  # principio activo, no marca
}
_MARCAS_ORDEN = sorted((k for k, v in MARCAS.items() if v), key=len, reverse=True)
_LINEAS = {"plax", "periogard", "sonicare"}   # identifican la marca pero también la línea
_MARCAS_GENERICAS = {"", "generico", "generica", "sin marca", "varios", "otros", "otro", "na", "n a"}


def estandarizar_marca(nombre: str, marca_sitio: str | None) -> str | None:
    texto = f" {normalizar_texto(nombre)} "
    mejor = None
    for patron in _MARCAS_ORDEN:
        m = re.search(rf"(?<![a-z0-9]){re.escape(patron)}(?![a-z0-9])", texto)
        if m and (mejor is None or m.start() < mejor[0]):
            mejor = (m.start(), MARCAS[patron])
    if mejor:
        return mejor[1]
    b = normalizar_texto(marca_sitio)
    if b in _MARCAS_GENERICAS:
        return None
    for patron in _MARCAS_ORDEN:
        if b == patron or b.startswith(patron + " "):
            return MARCAS[patron]
    return b.title()


# ---------------------------------------------------------------------------
# R3 · Subcategorías y R4 · exclusiones
# ---------------------------------------------------------------------------
SUBCATEGORIAS = [
    ("Kits y combos", r"\b(kit|kits|combo|combos|estuche|set)\b"),
    ("Cuidado de prótesis dental", r"\b(protesis|dentadura|dentaduras|adhesivo|adhesiva|fijador|corega|fixodent|polident|retenedor|retenedores)\b"),
    ("Seda e hilo dental", r"\b(seda|sedas|hilo|hilos|cinta) dental(es)?\b|\bfloss|\bflosser|\bportahilo|\barcos? dental"),
    ("Cepillo interdental", r"\b(interdental|interdentales|interproximal|interproximales)\b"),
    ("Repuesto de cepillo eléctrico", r"^(repuesto|repuestos|cabezal|cabezales|refill)\b|\b(repuesto|repuestos|cabezales|refill) (de |para )?cepillo"),
    ("Cepillo dental eléctrico", r"\bcepillos?\b.*\b(electrico|electricos|bateria|baterias|recargable|pilas|sonico|sonic)\b"),
    ("Cepillo dental", r"\bcepillos?\b"),
    ("Crema dental", r"\b(cremas?|pastas?|geles?|gel) (de )?(dental|dentales|dientes)\b|\bdentifrico|\btoothpaste"),
    ("Enjuague bucal", r"\b(enjuague|enjuagues|colutorio|antiseptico bucal|agua bucal|mouthwash|plax|listerine)\b"),
    ("Blanqueamiento dental", r"\b(blanqueador|blanqueadora|blanqueamiento|tiras blanqueadoras|lapiz blanqueador)\b"),
    ("Aliento y spray bucal", r"\b(spray|aliento|refrescante bucal)\b"),
    ("Limpiador lingual", r"\b(lingual|raspador|limpiador de lengua|limpia lengua)\b"),
]
_SUBCAT_RE = [(n, re.compile(p)) for n, p in SUBCATEGORIAS]
SUBCATEGORIA_RESTO = "Otros cuidado bucal"

# Anuncios que aparecen en la sección del comercio pero no son cuidado bucal.
# AJUSTAR según la delimitación de la planilla «Categorías de Productos - Estudiantes».
EXCLUSIONES = re.compile(
    r"\b(chicle|chicles|goma de mascar|caramelo|caramelos|bombon|bombones|labial|labiales|"
    r"balsamo labial|protector labial|mascota|mascotas|perro|perros|gato|gatos|tapabocas|"
    r"termometro|preservativo|preservativos|condon|condones|desodorante|shampoo|jabon corporal)\b"
)


def clasificar(nombre: str, categoria_sitio: str | None) -> tuple[str | None, str | None]:
    """Devuelve (subcategoría, motivo_exclusión). Si se excluye, subcategoría = None."""
    n = normalizar_texto(nombre)
    if EXCLUSIONES.search(n):
        return None, "R4: no pertenece a cuidado bucal (" + EXCLUSIONES.search(n).group(0) + ")"
    # combos sin la palabra kit: dos tipos de producto unidos por + / gratis / obsequio
    tipos = sum(bool(re.search(p, n)) for p in (r"\bcrema|\bpasta", r"\bcepillo", r"\benjuague", r"\bseda|\bhilo"))
    if tipos >= 2 and re.search(r"\+|\bgratis\b|\bobsequio\b|\bmas\b", n):
        return "Kits y combos", None
    for texto in (n, f"{n} {normalizar_texto(categoria_sitio)}"):
        for nombre_sub, rx in _SUBCAT_RE:
            if rx.search(texto):
                return nombre_sub, None
    return SUBCATEGORIA_RESTO, None


# ---------------------------------------------------------------------------
# R5 · Contenido y unidades · R6 · concentración
# ---------------------------------------------------------------------------
UNIDADES = {
    "ml": ("ml", 1), "mililitros": ("ml", 1), "mililitro": ("ml", 1), "cc": ("ml", 1),
    "l": ("ml", 1000), "lt": ("ml", 1000), "lts": ("ml", 1000), "litro": ("ml", 1000), "litros": ("ml", 1000),
    "g": ("g", 1), "gr": ("g", 1), "grs": ("g", 1), "gramos": ("g", 1), "gramo": ("g", 1),
    "kg": ("g", 1000), "mg": ("g", 0.001),
    "m": ("m", 1), "mt": ("m", 1), "mts": ("m", 1), "metro": ("m", 1), "metros": ("m", 1),
    "yd": ("m", 0.9144), "yds": ("m", 0.9144), "yardas": ("m", 0.9144),
    "oz": ("oz", 1), "onzas": ("oz", 1),
}
_U = "|".join(sorted(map(re.escape, UNIDADES), key=len, reverse=True))
_UND = r"(?:und|unid|unids|unidad|unidades|uds|ud|u|un|tubos|cepillos|piezas|pzs)"
_EMPAQUE = r"(?:caja|tubo|tubos|frasco|frascos|bolsa|paquete|pack|blister|sobre|tarro|botella|und|unidad)"

RE_CONT = re.compile(rf"(?<![a-z0-9.])(\d+(?:\.\d+)?)\s*({_U})(?![a-z0-9])")
RE_N_ANTES = re.compile(rf"(?<![a-z0-9.])(\d{{1,2}})\s*x\s*(\d+(?:\.\d+)?)\s*({_U})(?![a-z0-9])")
RE_N_DESPUES = re.compile(
    rf"(\d+(?:\.\d+)?)\s*(?:{_U})(?:\s+{_EMPAQUE})?\s*x\s*(\d{{1,2}})(?![0-9.])(?!\s*(?:{_U})(?![a-z0-9]))")
RE_N_UND = re.compile(rf"(?<![a-z0-9.])(\d{{1,2}})\s*{_UND}(?![a-z0-9])")
RE_PROMO = re.compile(r"\blleve\s*(\d{1,2})\s*pague\s*\d{1,2}\b|\bpague\s*\d{1,2}\s*lleve\s*(\d{1,2})\b"
                      r"|(?<![a-z0-9.])(\d)\s*x\s*(\d)(?![a-z0-9.])")
RE_N_SOLO = re.compile(rf"(?:\bx|\bpack|\bpaquete|\bpaq)\s*(\d{{1,2}})(?![0-9.])(?!\s*(?:{_U})(?![a-z0-9]))")
PALABRAS_N = {r"\b(duo|dupla|doble|bipack|twin pack|twinpack)\b": 2,
              r"\b(tripack|tri pack|trio|triple)\b": 3, r"\b(tetrapack|cuadruple)\b": 4}
RE_PPM = re.compile(r"(\d{3,5})\s*ppm\b")
RE_PCT = re.compile(r"(?<![a-z0-9.])(\d+(?:\.\d+)?)\s*%")
RE_ACTIVO = re.compile(r"clorhexidina|cetilpiridinio|peroxido|triclosan|fluoruro|hexetidina")

SUBCAT_CONTABLES = {"Cepillo dental", "Cepillo dental eléctrico", "Cepillo interdental",
                    "Repuesto de cepillo eléctrico", "Limpiador lingual"}


def _a_base(valor: float, unidad: str) -> tuple[float, str]:
    base, factor = UNIDADES[unidad]
    return round(valor * factor, 4), base


def extraer_unidades(n: str) -> tuple[int, str | None]:
    """Número de unidades del empaque y regla que lo determinó."""
    m = RE_N_ANTES.search(n)
    if m and int(m.group(1)) >= 2:
        return int(m.group(1)), "N x contenido"
    m = RE_N_DESPUES.search(n)
    if m and int(m.group(2)) >= 2:
        return int(m.group(2)), "contenido x N"
    m = RE_PROMO.search(n)
    if m:
        g = [x for x in m.groups() if x]
        if m.group(3) and m.group(4):            # forma "3x2": lleve 3 pague 2
            if int(m.group(3)) > int(m.group(4)):
                return int(m.group(3)), "promoción lleve N pague M"
        elif g:
            return int(g[0]), "promoción lleve N pague M"
    m = RE_N_UND.search(n)
    if m and int(m.group(1)) >= 1:
        return int(m.group(1)), "N und"
    m = RE_N_SOLO.search(n)
    if m and int(m.group(1)) >= 2:
        return int(m.group(1)), "x N"
    for patron, k in PALABRAS_N.items():
        if re.search(patron, n):
            return k, "palabra de paquete"
    return 1, None


def extraer_contenido(nombre: str, ficha: str | None = None, subcategoria: str | None = None) -> dict:
    """Contenido unitario, unidades, contenido total y unidad base (R5, R6)."""
    n = normalizar_texto(nombre)
    alerta = []
    conts = [(float(v), u) for v, u in RE_CONT.findall(n)]
    fuente = "nombre"
    if not conts and ficha:
        f = normalizar_texto(ficha)
        for clave in ("contenido", "cantidad", "peso", "volumen", "tamano", "presentacion"):
            m = re.search(rf"\b{clave}\w*\s+(\d+(?:\.\d+)?)\s*({_U})(?![a-z0-9])", f)
            if m:
                conts, fuente = [(float(m.group(1)), m.group(2))], "ficha técnica"
                break
    n_und, regla_und = extraer_unidades(n)
    if regla_und is None and ficha:
        m = re.search(r"\bunidades\w*( por)?( paquete| empaque)?\s+(\d{1,2})\b", normalizar_texto(ficha))
        if m:
            n_und, regla_und = int(m.group(3)), "ficha técnica"

    cont_unit, unidad = None, None
    if conts:
        cont_unit, unidad = _a_base(*conts[0])
        mismos = [_a_base(*c) for c in conts]
        if len({c for c in mismos}) > 1:
            if "+" in n and len({u for _, u in mismos}) == 1:
                cont_unit = round(sum(v for v, _ in mismos), 4)
                alerta.append("contenido adicional sumado (+)")
            else:
                alerta.append("varios contenidos en el nombre; se usa el primero")
    elif subcategoria in SUBCAT_CONTABLES:
        cont_unit, unidad = 1.0, "und"

    ppm = RE_PPM.search(n)
    pct = RE_PCT.search(n) if RE_ACTIVO.search(n) else None
    concentracion = f"{ppm.group(1)} ppm" if ppm else (f"{pct.group(1)} %" if pct and float(pct.group(1)) > 0 else None)

    total = round(cont_unit * n_und, 4) if cont_unit is not None else None
    return {
        "contenido_unitario": cont_unit, "n_unidades": n_und, "contenido_total": total,
        "unidad": unidad, "concentracion": concentracion,
        "fuente_contenido": fuente if conts else (None if unidad is None else "conteo"),
        "regla_unidades": regla_und, "alerta": "; ".join(alerta) or None,
    }


# ---------------------------------------------------------------------------
# R7 · Familia (producto sin tamaño)
# ---------------------------------------------------------------------------
_GENERICAS = set("""
crema cremas pasta pastas dental dentales dientes gel enjuague enjuagues bucal bucales cepillo cepillos
seda hilo cinta oral de del la el los las con para y en x und unid unids unidad unidades uds ud u un
caja tubo tubos frasco frascos bolsa paquete pack paq blister sobre tarro botella oferta precio especial
promo promocion gratis obsequio lleve pague mas antiseptico antibacterial adultos adulto producto
""".split())


def familia_de(nombre: str, marca: str | None) -> tuple[str, str]:
    """(clave de familia con tokens ordenados, nombre legible en el orden original)."""
    n = normalizar_texto(nombre)
    for rx in (RE_N_ANTES, RE_N_DESPUES, RE_CONT, RE_PROMO, RE_N_UND, RE_N_SOLO, RE_PPM):
        n = rx.sub(" ", n)
    marca_n = set(normalizar_texto(marca).split()) if marca else set()
    for patron in _MARCAS_ORDEN:              # quita las variantes de la marca (no las líneas)
        if marca and MARCAS[patron] == marca and patron not in _LINEAS:
            marca_n |= set(patron.split())
    orden = []
    for t in n.split():
        if t not in _GENERICAS and t not in marca_n and (len(t) > 1 or t.isdigit()) and t not in orden:
            orden.append(t)
    legible = " ".join(orden).replace("total12", "total 12").title()
    return " ".join(sorted(orden)), legible


def nombre_para_analisis(nombre_producto: str | None, nombre_item: str | None) -> str:
    """Nombre del SKU: el del ítem si contiene al del producto; si no, ambos."""
    p, i = (nombre_producto or "").strip(), (nombre_item or "").strip()
    if not i or not p:
        return i or p
    if normalizar_texto(p) in normalizar_texto(i):
        return i
    if normalizar_texto(i) in normalizar_texto(p):
        return p
    return f"{i} {p}"


# ---------------------------------------------------------------------------
# Estandarización completa de un anuncio
# ---------------------------------------------------------------------------

def estandarizar(nombre: str, marca_sitio: str | None, categoria_sitio: str | None,
                 ficha: str | None) -> dict:
    subcat, motivo = clasificar(nombre, categoria_sitio)
    marca = estandarizar_marca(nombre, marca_sitio)
    cont = extraer_contenido(nombre, ficha, subcat)
    fam, legible = familia_de(nombre, marca)
    tam = ""
    if cont["contenido_total"] is not None:
        tam = f"{cont['contenido_total']:g} {cont['unidad']}"
        if cont["n_unidades"] > 1 and cont["unidad"] != "und":
            tam = f"{cont['n_unidades']} x {cont['contenido_unitario']:g} {cont['unidad']}"
    if cont["concentracion"]:
        tam = f"{tam} ({cont['concentracion']})".strip()
    nombre_std = f"{marca or 'Sin marca'} {legible}".strip() + (f" — {tam}" if tam else "")
    return {"nombre_std": nombre_std, "marca_std": marca, "subcategoria_std": subcat,
            "familia": f"{subcat}|{marca}|{fam}", "incluido": int(motivo is None),
            "motivo_exclusion": motivo, **cont}


# ---------------------------------------------------------------------------
# R8 · Correspondencia
# ---------------------------------------------------------------------------

def gtin_valido(codigo) -> str | None:
    """Devuelve el GTIN normalizado a 14 dígitos si el dígito de control es válido."""
    s = re.sub(r"\D", "", str(codigo or ""))
    if len(s) not in (8, 12, 13, 14) or set(s) == {"0"}:
        return None
    d = [int(c) for c in s]
    cuerpo = d[:-1][::-1]
    suma = sum(x * (3 if i % 2 == 0 else 1) for i, x in enumerate(cuerpo))
    return s.zfill(14) if (10 - suma % 10) % 10 == d[-1] else None


def _clave_nombre(r: dict) -> str:
    tam = f"{r['contenido_total']:g}{r['unidad']}" if r["contenido_total"] is not None else "?"
    return "|".join([r["subcategoria_std"] or "", r["marca_std"] or "", r["familia"],
                     tam, str(r["n_unidades"]), r["concentracion"] or ""])


def _id_nombre(clave: str) -> str:
    return "N" + hashlib.sha1(clave.encode()).hexdigest()[:12]


def leer_manuales(ruta: Path = ARCHIVO_MANUAL) -> dict:
    """{(comercio_id, sku_comercio): (producto_id, nota)}. producto_id = EXCLUIR excluye."""
    if not Path(ruta).exists():
        return {}
    with open(ruta, encoding="utf-8") as f:
        filas = [r for r in csv.DictReader(f) if r.get("comercio_id") and not r["comercio_id"].startswith("#")]
    return {(r["comercio_id"].strip(), r["sku_comercio"].strip()): (r["producto_id"].strip(), r.get("nota", ""))
            for r in filas}


class _UnionFind:
    def __init__(self):
        self.p = {}

    def find(self, x):
        self.p.setdefault(x, x)
        while self.p[x] != x:
            self.p[x] = self.p[self.p[x]]
            x = self.p[x]
        return x

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra != rb:
            self.p[max(ra, rb)] = min(ra, rb)


def actualizar_correspondencias(con: sqlite3.Connection, ejecucion_ref: str | None = None,
                                ruta_manual: Path = ARCHIVO_MANUAL, min_comercios: int = 3) -> dict:
    """Re-estandariza todos los anuncios, asigna producto_id común y marca comparables.

    Es idempotente: puede volver a correrse después de ajustar reglas o el archivo
    de correspondencias manuales, sin volver a consultar los sitios.
    """
    con.row_factory = sqlite3.Row
    anuncios = [dict(r) for r in con.execute(
        "SELECT anuncio_id, comercio_id, sku_comercio, ean, nombre_original, nombre_item_original, "
        "marca_original, categoria_original, ficha_tecnica_original FROM anuncios")]
    manuales = leer_manuales(ruta_manual)

    # 1. Estandarización (R1–R7)
    for a in anuncios:
        nombre = nombre_para_analisis(a["nombre_original"], a["nombre_item_original"])
        a.update(estandarizar(nombre, a["marca_original"], a["categoria_original"], a["ficha_tecnica_original"]))
        a["gtin"] = gtin_valido(a["ean"])
        a["metodo"] = None
        a["alerta_corr"] = None
        man = manuales.get((a["comercio_id"], str(a["sku_comercio"])))
        if man and man[0].upper() == "EXCLUIR":
            a["incluido"], a["motivo_exclusion"] = 0, "Manual: " + (man[1] or "excluido")

    incl = [a for a in anuncios if a["incluido"]]

    # 2. Grupos por GTIN, separados por número de unidades (R8b)
    por_gtin = defaultdict(list)
    for a in incl:
        if a["gtin"]:
            por_gtin[a["gtin"]].append(a)
    for gtin, grupo in por_gtin.items():
        moda_n = Counter(x["n_unidades"] for x in grupo).most_common(1)[0][0]
        conts = {x["contenido_total"] for x in grupo if x["contenido_total"] is not None and x["n_unidades"] == moda_n}
        for x in grupo:
            if x["n_unidades"] == moda_n:
                x["producto_id"], x["metodo"] = "G" + gtin, "EAN"
                if len(conts) > 1:
                    x["alerta_corr"] = "EAN con contenidos distintos entre comercios"
            else:
                x["producto_id"], x["metodo"] = f"G{gtin}-x{x['n_unidades']}", "EAN + unidades"
                x["alerta_corr"] = "mismo EAN con distinto número de unidades"

    # 3. Clave normalizada para los que no tienen GTIN válido (R8c). Si la clave coincide
    #    sin ambigüedad con la de un grupo EAN de otro comercio, se une a ese grupo.
    clave_a_ean = defaultdict(set)
    for a in incl:
        if a.get("metodo"):
            clave_a_ean[_clave_nombre(a)].add(a["producto_id"])
    for a in incl:
        if not a.get("metodo"):
            clave = _clave_nombre(a)
            if len(clave_a_ean.get(clave, ())) == 1:
                a["producto_id"], a["metodo"] = next(iter(clave_a_ean[clave])), "clave normalizada (unida a EAN)"
            else:
                a["producto_id"], a["metodo"] = _id_nombre(clave), "clave normalizada"

    # 4. Correspondencias manuales (R8a) — tienen prioridad
    for a in incl:
        man = manuales.get((a["comercio_id"], str(a["sku_comercio"])))
        if man and man[0].upper() != "EXCLUIR":
            a["producto_id"], a["metodo"] = man[0], "manual"

    # 5. Familias unidas a través de productos compartidos (R7)
    uf = _UnionFind()
    por_prod = defaultdict(list)
    for a in incl:
        uf.find(a["familia"])
        por_prod[a["producto_id"]].append(a)
    for grupo in por_prod.values():
        for x in grupo[1:]:
            if x["unidad"] == grupo[0]["unidad"]:
                uf.union(grupo[0]["familia"], x["familia"])
    for a in incl:
        a["familia"] = uf.find(a["familia"])

    # 6. Escritura: primero productos (tabla referenciada), luego anuncios
    con.execute("UPDATE anuncios SET producto_id = NULL, comparable = 0")
    con.execute("UPDATE observaciones SET comparable = 0")
    con.execute("DELETE FROM productos")
    filas = []
    for pid, grupo in por_prod.items():
        def val(campo, g=grupo):
            c = Counter(x[campo] for x in g if x[campo] is not None).most_common(1)
            return c[0][0] if c else None
        alertas = sorted({x["alerta_corr"] for x in grupo if x["alerta_corr"]}
                         | ({"marcas distintas en el grupo"} if len({x["marca_std"] for x in grupo}) > 1 else set())
                         | ({"subcategorías distintas en el grupo"} if len({x["subcategoria_std"] for x in grupo}) > 1 else set())
                         | ({"contenidos distintos en el grupo"} if len({x["contenido_total"] for x in grupo}) > 1 else set()))
        nombre = min((x["nombre_std"] for x in grupo),
                     key=lambda s, g=grupo: (-sum(y["nombre_std"] == s for y in g), len(s)))
        filas.append({
            "producto_id": pid, "nombre_estandar": nombre, "marca": val("marca_std"),
            "subcategoria": val("subcategoria_std"), "familia": val("familia"),
            "contenido_unitario": val("contenido_unitario"), "n_unidades": val("n_unidades"),
            "contenido_total": val("contenido_total"), "unidad": val("unidad"),
            "concentracion": val("concentracion"), "ean": next((x["gtin"] for x in grupo if x["gtin"]), None),
            "metodo_correspondencia": val("metodo"), "n_comercios": len({x["comercio_id"] for x in grupo}),
            "alerta": "; ".join(alertas) or None,
        })
    repetidos = Counter(f["nombre_estandar"] for f in filas)
    for f in filas:                       # nombres únicos para tablas y gráficos
        if repetidos[f["nombre_estandar"]] > 1:
            f["nombre_estandar"] += f" [{f['ean'][-6:] if f['ean'] else f['producto_id'][-6:]}]"
    con.executemany(
        """INSERT INTO productos (producto_id, nombre_estandar, marca, subcategoria, familia, contenido_unitario,
               n_unidades, contenido_total, unidad, concentracion, ean, metodo_correspondencia, n_comercios, alerta)
           VALUES (:producto_id, :nombre_estandar, :marca, :subcategoria, :familia, :contenido_unitario,
               :n_unidades, :contenido_total, :unidad, :concentracion, :ean, :metodo_correspondencia,
               :n_comercios, :alerta)""", filas)
    con.executemany(
        """UPDATE anuncios SET nombre_std=:nombre_std, marca_std=:marca_std, subcategoria_std=:subcategoria_std,
               contenido_unitario=:contenido_unitario, n_unidades=:n_unidades, contenido_total=:contenido_total,
               unidad=:unidad, concentracion=:concentracion, familia=:familia, incluido=:incluido,
               motivo_exclusion=:motivo_exclusion, alerta_limpieza=:alerta, ean_valido=:gtin,
               metodo_correspondencia=:metodo
           WHERE anuncio_id=:anuncio_id""", anuncios)
    con.executemany("UPDATE anuncios SET producto_id=:producto_id WHERE anuncio_id=:anuncio_id", incl)

    # Nombre legible de la familia: el de su presentación más pequeña
    con.execute("DELETE FROM familias")
    con.execute("""INSERT INTO familias (familia, nombre_familia)
                   SELECT familia, MIN(CASE WHEN instr(nombre_estandar, ' — ') > 0
                                            THEN substr(nombre_estandar, 1, instr(nombre_estandar, ' — ') - 1)
                                            ELSE nombre_estandar END)
                   FROM productos GROUP BY familia""")

    resultado = marcar_comparables(con, ejecucion_ref, min_comercios)
    con.commit()
    con.row_factory = None
    return resultado


# ---------------------------------------------------------------------------
# R9 · Comparables
# ---------------------------------------------------------------------------

def elegir_ejecucion_referencia(con: sqlite3.Connection, min_comercios: int = 3) -> str | None:
    """Ejecución más reciente con ≥ min_comercios no fallidos y más presentaciones comunes."""
    candidatas = con.execute(
        """SELECT ejecucion_id, COUNT(*) FROM ejecuciones WHERE estado <> 'fallida'
           GROUP BY ejecucion_id HAVING COUNT(*) >= ? ORDER BY ejecucion_id DESC""", (min_comercios,)).fetchall()
    mejor = None
    for (eid, n_com) in candidatas:
        n_comunes = len(_comunes(con, eid))
        if n_comunes >= 8:
            return eid
        if mejor is None or n_comunes > mejor[1]:
            mejor = (eid, n_comunes)
    return mejor[0] if mejor else None


def _comercios_ejecucion(con, eid):
    return [r[0] for r in con.execute(
        "SELECT comercio_id FROM ejecuciones WHERE ejecucion_id=? AND estado <> 'fallida' ORDER BY comercio_id", (eid,))]


def _comunes(con, eid) -> list[str]:
    comercios = _comercios_ejecucion(con, eid)
    if not comercios:
        return []
    marcas = ",".join("?" * len(comercios))
    filas = con.execute(
        f"""SELECT a.producto_id FROM observaciones o JOIN anuncios a ON a.anuncio_id = o.anuncio_id
            WHERE o.ejecucion_id = ? AND a.incluido = 1 AND a.producto_id IS NOT NULL
              AND o.precio IS NOT NULL AND o.disponible = 1 AND o.comercio_id IN ({marcas})
            GROUP BY a.producto_id HAVING COUNT(DISTINCT o.comercio_id) = ?""",
        (eid, *comercios, len(comercios))).fetchall()
    return [f[0] for f in filas]


def marcar_comparables(con: sqlite3.Connection, ejecucion_ref: str | None = None, min_comercios: int = 3) -> dict:
    eid = ejecucion_ref or elegir_ejecucion_referencia(con, min_comercios)
    con.execute("UPDATE productos SET comparable = 0")
    con.execute("UPDATE anuncios SET comparable = 0")
    con.execute("UPDATE observaciones SET comparable = 0")
    if eid is None:
        return {"ejecucion_referencia": None, "comercios": [], "n_comparables": 0}
    comunes = _comunes(con, eid)
    comercios = _comercios_ejecucion(con, eid)
    con.executemany("UPDATE productos SET comparable = 1 WHERE producto_id = ?", [(p,) for p in comunes])
    marcas = ",".join("?" * len(comercios))
    con.execute(f"""UPDATE anuncios SET comparable = 1
                    WHERE producto_id IN (SELECT producto_id FROM productos WHERE comparable = 1)
                      AND incluido = 1 AND comercio_id IN ({marcas})""", comercios)
    con.execute("""UPDATE observaciones SET comparable = 1
                   WHERE anuncio_id IN (SELECT anuncio_id FROM anuncios WHERE comparable = 1)""")
    con.execute("DELETE FROM parametros WHERE clave IN ('ejecucion_referencia', 'comercios_referencia')")
    con.executemany("INSERT INTO parametros (clave, valor) VALUES (?, ?)",
                    [("ejecucion_referencia", eid), ("comercios_referencia", ",".join(comercios))])
    return {"ejecucion_referencia": eid, "comercios": comercios, "n_comparables": len(comunes)}
