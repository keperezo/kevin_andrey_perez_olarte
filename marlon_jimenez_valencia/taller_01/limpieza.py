"""
limpieza.py — Estandarización de anuncios y apoyo a la correspondencia de productos.

Nada de lo que hay aquí modifica el texto original: cada función recibe el
texto tal como lo publicó el comercio y devuelve valores estandarizados que se
guardan en columnas propias (sufijo _std o columnas numéricas nuevas).

Reglas de transformación (se documentan también en el README y en el .qmd):
  1. Precio: el JSON de VTEX ya trae el precio como número en COP. Se guarda
     como REAL. Si el producto no está disponible y el precio llega en 0, se
     guarda NULL (nunca 0).
  2. Texto: mayúsculas, sin tildes, sin signos de puntuación sobrantes y sin
     sufijos societarios (S.A., S.A.S., LTDA) para marca y laboratorio.
  3. Forma farmacéutica: se infiere de palabras clave del nombre y la ficha.
  4. Unidades por empaque y contenido: primero se usan los campos estructurados
     de la ficha del comercio; si no existen, se extraen del nombre con
     expresiones regulares. Se registra la fuente en `fuente_cantidad`.
  5. Conversión de unidades de contenido: L -> mL (x1000), kg -> g (x1000),
     g -> mg (x1000) cuando haga falta comparar.
"""
from __future__ import annotations

import re

import pandas as pd

from categoria import normalizar

# ---------------------------------------------------------------------------
# Marca y laboratorio
# ---------------------------------------------------------------------------
SUFIJOS_SOCIETARIOS = r"\b(s\s?a\s?s|s\s?a|ltda|limitada|inc|s\s?en\s?c|y cia|& cia|cia)\b\.?"

ALIAS_LABORATORIO = {
    "TECNOQUIMICAS": "TECNOQUIMICAS",
    "MK": "TECNOQUIMICAS",
    "GENFAR": "GENFAR",
    "GEF": "GENFAR",
    "LA SANTE": "LA SANTE",
    "LABORATORIOS LA SANTE": "LA SANTE",
    "AG": "AMERICAN GENERICS",
    "AMERICAN GENERICS": "AMERICAN GENERICS",
    "PROCAPS": "PROCAPS",
    "COLMED": "PROCAPS",
    "LAFRANCOL": "LAFRANCOL",
    "ECAR": "ECAR",
    "LABORATORIOS ECAR": "ECAR",
    "SIEGFRIED": "SIEGFRIED",
    "PFIZER": "PFIZER",
    "ABBOTT": "ABBOTT",
    "SANOFI": "SANOFI",
    "SANOFI AVENTIS DE COLOMBIA": "SANOFI",
    "BAYER": "BAYER",
    "GLAXOSMITHKLINE": "GSK",
    "GSK": "GSK",
    "NOVARTIS": "NOVARTIS",
    "SANDOZ": "SANDOZ",
    "VITALIS": "VITALIS",
}


def texto_std(valor) -> str | None:
    """Mayúsculas, sin tildes ni puntuación; None si viene vacío."""
    if valor is None or (isinstance(valor, float) and pd.isna(valor)):
        return None
    t = normalizar(valor)
    t = re.sub(r"[^\w\s/&+%-]", " ", t)
    t = re.sub(r"\s+", " ", t).strip()
    return t.upper() or None


def laboratorio_std(valor) -> str | None:
    t = texto_std(valor)
    if not t:
        return None
    t = re.sub(SUFIJOS_SOCIETARIOS, " ", t.lower())
    t = re.sub(r"\blaboratorios?\b", " ", t)
    t = re.sub(r"\s+", " ", t).strip().upper()
    for clave, estandar in ALIAS_LABORATORIO.items():
        if t == clave or t.startswith(clave + " "):
            return estandar
    return t or None


def marca_std(valor) -> str | None:
    t = texto_std(valor)
    if not t:
        return None
    return ALIAS_LABORATORIO.get(t, t) if t in ("MK", "AG", "GEF") else t


# ---------------------------------------------------------------------------
# Forma farmacéutica
# ---------------------------------------------------------------------------
FORMAS = [
    ("POLVO PARA SUSPENSION ORAL", r"polvo (para )?(reconstituir|suspension|preparar)|pol sus|granulos? para suspension"
                                   r"|suspension (en )?polvo|\bpolvo\b.*\bsuspension"),
    ("SUSPENSION ORAL", r"suspension|\bsusp\b|\bsus oral\b"),
    ("JARABE", r"jarabe"),
    # va antes que "gotas": unas gotas oftálmicas no son una solución oral
    ("SOLUCION TOPICA / OFTALMICA / OTICA", r"oftalm|ofteno|\botic|topic"),
    ("SOLUCION ORAL", r"solucion oral|gotas|\bsol oral\b"),
    ("INYECTABLE", r"inyectable|\binyet|ampoll|\bvial(es)?\b|intramuscular|intravenos|\bmui\b"
                   r"|millones (de )?ui|polvo para (solucion )?inyec"),
    ("SOBRE / POLVO ORAL", r"\bsobres?\b|granulad"),
    ("TABLETA", r"tableta|\btab\b|\btabs\b|comprimido|\bcomp\b|grageas?"),
    ("CAPSULA", r"capsula|\bcap\b|\bcaps\b"),
    ("OVULO", r"ovulo"),
    ("CREMA / GEL / UNGUENTO", r"crema|\bgel\b|unguento|pomada"),
]


def forma_std(*textos) -> str | None:
    """
    Revisa los textos en el orden en que llegan (nombre, presentación, ...) y usa el
    primero que diga la forma. Así "CAJA X 14 CAP" en la presentación de La Rebaja
    gana sobre un "Tab" genérico que venga en otro campo.
    """
    for texto in textos:
        t = normalizar(str(texto)) if texto else ""
        for nombre, patron in FORMAS:
            if re.search(patron, t):
                return nombre
    return None


# ---------------------------------------------------------------------------
# Unidades por empaque y contenido (mL)
# ---------------------------------------------------------------------------
PALABRAS_UNIDAD = (r"(tabletas?|tabs?|comprimidos?|comp|capsulas?|caps?|sobres?|"
                   r"unidades|unds?|un|ovulos?|ampollas?|viales?|grageas?)")

# "(?:[.,]0+)?" acepta "X 20.00 Tab" (así escribe La Rebaja) y "(?<![\d.,])" evita
# leer el "00" de los decimales como si fuera el número de unidades.
_RE_X_UNIDADES = re.compile(r"\bx\s*(\d{1,3})(?:[.,]0+)?\s*" + PALABRAS_UNIDAD + r"\b")
_RE_N_UNIDADES = re.compile(r"(?<![\d.,])(\d{1,3})(?:[.,]0+)?\s*" + PALABRAS_UNIDAD + r"\b")
_RE_CON_UNIDADES = re.compile(r"\bcon\s+(\d{1,3})\s+" + PALABRAS_UNIDAD)
_RE_ML = re.compile(r"(?<![/\d.,])\s*(\d+(?:[.,]\d+)?)\s*(ml|mililitros?|cc)\b")
_RE_ML_CONCENTRACION = re.compile(r"/\s*\d*(?:[.,]\d+)?\s*(ml|cc)\b")
_RE_CONCENTRACION = re.compile(
    r"(\d+(?:[.,]\d+)?(?:\s*/\s*\d+(?:[.,]\d+)?)?)\s*(mg|g|mcg|ui|%)"
    r"(?:\s*/\s*(\d+(?:[.,]\d+)?)?\s*(ml))?")

UNIDADES_CONTABLES = {"CAPSULA", "TABLETA", "UNIDAD", "UND", "UN", "SOBRE", "OVULO",
                      "AMPOLLA", "VIAL", "COMPRIMIDO", "CAP", "TAB"}
UNIDADES_VOLUMEN = {"ML", "MILILITRO", "MILILITROS", "CC"}
UNIDADES_MASA = {"G", "GR", "GRS", "GRAMO", "GRAMOS"}


def _numero(texto) -> float | None:
    if texto is None:
        return None
    try:
        return float(str(texto).replace(",", "."))
    except ValueError:
        return None


def cantidad_desde_ficha(unidad_medida, cantidad) -> tuple[float | None, str | None]:
    """Interpreta los campos estructurados (unidad de medida + cantidad) de la ficha."""
    u = texto_std(unidad_medida) or ""
    n = _numero(cantidad)
    if n is None or n <= 0:
        return None, None
    if u in UNIDADES_CONTABLES or u.rstrip("S") in UNIDADES_CONTABLES:
        return n, "UNIDAD"
    if u in UNIDADES_VOLUMEN:
        return n, "ML"
    if u in UNIDADES_MASA:
        return n, "G"
    return None, None


def unidades_desde_texto(*textos) -> int | None:
    t = normalizar(" ".join(str(x) for x in textos if x))
    for patron in (_RE_X_UNIDADES, _RE_CON_UNIDADES, _RE_N_UNIDADES):
        m = patron.search(t)
        if m and int(m.group(1)) > 0:
            return int(m.group(1))
    return None


def ml_desde_texto(*textos) -> float | None:
    """Volumen total del envase en mL (ignora el '5 mL' de las concentraciones)."""
    t = normalizar(" ".join(str(x) for x in textos if x))
    t = _RE_ML_CONCENTRACION.sub(" ", t)   # quita "/5 ml"
    valores = [float(v.replace(",", ".")) for v, _ in _RE_ML.findall(t)]
    return max(valores) if valores else None


def concentracion_std(*textos) -> str | None:
    t = normalizar(" ".join(str(x) for x in textos if x))
    # "amoxicilina 600/ acido clavulanico 42.9 mg" -> "600/42.9 mg" (si no, salía solo 42.9 mg)
    t = re.sub(r"(\d+(?:[.,]\d+)?)\s*/\s*[a-z][a-z ]*?\s(\d)", r"\1/\2", t)
    m = _RE_CONCENTRACION.search(t)
    if not m:
        return None
    valor, unidad, vol, ml = m.groups()
    valor = re.sub(r"\s+", "", valor).replace(",", ".")
    texto = f"{valor} {unidad.upper()}"
    if ml:
        texto += f"/{(vol or '1').replace(',', '.')} ML"
    return texto


def estandarizar(df: pd.DataFrame) -> pd.DataFrame:
    """
    Agrega las columnas estandarizadas a un DataFrame con las columnas que
    devuelve cada función de scraper.py. No toca las columnas *_original.
    """
    df = df.copy()
    df["marca_std"] = df["marca_original"].map(marca_std)
    df["laboratorio_std"] = [
        laboratorio_std(lab) or laboratorio_std(marca)
        for lab, marca in zip(df["laboratorio_original"], df["marca_original"])
    ]
    df["forma_std"] = [forma_std(n, p, pa, um) for n, p, pa, um in
                       zip(df["nombre_original"], df["presentacion_original"],
                           df["principio_activo_original"], df["unidad_medida_original"])]
    # La concentración se toma del nombre; si no aparece, de la ficha del principio activo.
    df["concentracion_std"] = [concentracion_std(n) or concentracion_std(pa) for n, pa in
                               zip(df["nombre_original"], df["principio_activo_original"])]

    unidades, contenido, unidad_contenido, fuente = [], [], [], []
    for _, fila in df.iterrows():
        n, u = cantidad_desde_ficha(fila.get("unidad_medida_original"), fila.get("cantidad_original"))
        fuente_fila = "ficha" if n is not None else None
        n_texto = unidades_desde_texto(fila["nombre_original"], fila.get("presentacion_original"),
                                       fila.get("nombre_sku_original"))
        ml_texto = ml_desde_texto(fila["nombre_original"], fila.get("presentacion_original"),
                                  fila.get("nombre_sku_original"))
        # Si la ficha y el nombre no coinciden, le creemos al nombre y dejamos la
        # diferencia anotada: en Locatel el campo "PUM - Medida" a veces trae otro
        # número (p. ej. Bactrim F "Susp X 100Ml" con PUM - Medida = 20).
        if u == "ML" and ml_texto is not None and ml_texto != n:
            fuente_fila = f"nombre (la ficha decía {n:g} ML)"
            n = ml_texto
        elif u == "UNIDAD" and n_texto is not None and n_texto != n:
            fuente_fila = f"nombre (la ficha decía {n:g} unidades)"
            n = n_texto
        if u == "UNIDAD":
            unidades.append(int(n)); contenido.append(None); unidad_contenido.append(None)
        elif u in ("ML", "G"):
            unidades.append(n_texto); contenido.append(n); unidad_contenido.append(u)
        elif ml_texto is not None:
            unidades.append(n_texto); contenido.append(ml_texto); unidad_contenido.append("ML")
            fuente_fila = "nombre"
        elif n_texto is not None:
            unidades.append(n_texto); contenido.append(None); unidad_contenido.append(None)
            fuente_fila = "nombre"
        else:
            unidades.append(None); contenido.append(None); unidad_contenido.append(None)
        fuente.append(fuente_fila)
    df["unidades"] = pd.array(unidades, dtype="Int64")
    df["contenido"] = pd.array(contenido, dtype="Float64")
    df["unidad_contenido"] = unidad_contenido
    df["fuente_cantidad"] = fuente
    return df


# ---------------------------------------------------------------------------
# Correspondencia entre comercios
# ---------------------------------------------------------------------------
def ean_valido(ean) -> bool:
    """EAN-13/UPC-12 con dígito de control correcto (descarta códigos internos)."""
    if ean is None:
        return False
    e = str(ean).strip()
    if not re.fullmatch(r"\d{12,13}", e):
        return False
    e = e.zfill(13)
    suma = sum(int(d) * (3 if i % 2 else 1) for i, d in enumerate(e[:12]))
    return (10 - suma % 10) % 10 == int(e[12])


SQL_CANDIDATOS = """
-- Regla documentada para proponer correspondencias:
-- mismo EAN + mismas unidades por empaque (o mismo contenido en mL)
-- + presente en TODOS los comercios dentro de una MISMA ejecución.
WITH ult AS (
    SELECT a.id_comercio, a.sku_comercio, a.ean, a.nombre_original,
           a.principio_activo_std, a.concentracion_std, a.forma_std,
           a.laboratorio_std, a.unidades, a.contenido, a.unidad_contenido,
           o.precio
    FROM anuncios a
    JOIN observaciones o
      ON o.id_comercio = a.id_comercio AND o.sku_comercio = a.sku_comercio
    WHERE o.id_ejecucion = :id_ejecucion
      AND a.ean_valido = 1
      AND o.precio IS NOT NULL
)
SELECT ean,
       COALESCE(CAST(unidades AS TEXT), '') || '|' || COALESCE(CAST(contenido AS TEXT), '') AS tamano,
       COUNT(DISTINCT id_comercio) AS n_comercios,
       MIN(principio_activo_std) AS principio_activo,
       MIN(concentracion_std)    AS concentracion,
       MIN(forma_std)            AS forma,
       MIN(laboratorio_std)      AS laboratorio,
       MIN(unidades)             AS unidades,
       MIN(contenido)            AS contenido_ml,
       GROUP_CONCAT(id_comercio || ':' || sku_comercio, ' ; ') AS skus,
       GROUP_CONCAT(nombre_original, ' || ') AS nombres
FROM ult
GROUP BY ean, tamano
HAVING COUNT(DISTINCT id_comercio) = :n_comercios
ORDER BY principio_activo, concentracion
"""


def proponer_correspondencias(con, id_ejecucion: str, n_comercios: int = 3) -> pd.DataFrame:
    """
    Propone presentaciones que aparecen en todos los comercios de una ejecución.
    Es solo una PROPUESTA: la correspondencia final se verifica a mano
    (marca, variante, concentración, número de unidades) y se registra en
    correspondencia.csv antes de cargarla con cargar_correspondencia().
    """
    return pd.read_sql_query(SQL_CANDIDATOS, con,
                             params={"id_ejecucion": id_ejecucion, "n_comercios": n_comercios})


def cargar_correspondencia(con, ruta_csv) -> pd.DataFrame:
    """
    Lee correspondencia.csv (verificada a mano) y la registra en la base:
      - tabla productos: un registro por presentación común (id_producto)
      - tabla correspondencia: qué SKU de cada comercio es ese producto
      - anuncios.id_producto y anuncios.comparable = 1
    Columnas esperadas en el CSV:
      id_producto, id_comercio, sku_comercio, descripcion, principio_activo,
      concentracion, forma, laboratorio, unidades, contenido_ml, regla, verificado
    """
    corr = pd.read_csv(ruta_csv, dtype=str).fillna("")
    corr = corr[corr["verificado"].str.strip().isin(["1", "si", "sí", "SI", "Sí"])]
    cur = con.cursor()
    cur.execute("UPDATE anuncios SET comparable = 0, id_producto = NULL")
    cur.execute("DELETE FROM correspondencia")
    cur.execute("DELETE FROM productos")
    productos = corr.drop_duplicates("id_producto")
    for _, p in productos.iterrows():
        cur.execute(
            """INSERT INTO productos (id_producto, descripcion, principio_activo, concentracion,
                                      forma, laboratorio, unidades, contenido_ml)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
            (p["id_producto"], p["descripcion"], p["principio_activo"], p["concentracion"],
             p["forma"], p["laboratorio"],
             int(p["unidades"]) if p["unidades"] else None,
             float(p["contenido_ml"]) if p["contenido_ml"] else None))
    for _, f in corr.iterrows():
        cur.execute(
            """INSERT INTO correspondencia (id_producto, id_comercio, sku_comercio, regla)
               VALUES (?, ?, ?, ?)""",
            (f["id_producto"], f["id_comercio"], f["sku_comercio"], f["regla"]))
        cur.execute("UPDATE anuncios SET comparable = 1, id_producto = ? WHERE id_comercio = ? AND sku_comercio = ?",
                    (f["id_producto"], f["id_comercio"], f["sku_comercio"]))
    con.commit()
    return corr


def plantilla_correspondencia(propuesta: pd.DataFrame) -> pd.DataFrame:
    """
    Convierte la propuesta (una fila por presentación) al formato de
    correspondencia.csv (una fila por presentación y comercio). La columna
    `verificado` queda vacía: se pone 1 solo después de revisar a mano.
    """
    filas = []
    for i, p in enumerate(propuesta.itertuples(index=False), start=1):
        tamano = (f"x{int(p.unidades)}" if pd.notna(p.unidades)
                  else f"{p.contenido_ml:g} mL" if pd.notna(p.contenido_ml) else "")
        descripcion = " ".join(str(x) for x in (p.principio_activo, p.concentracion, p.forma,
                                                tamano, f"({p.laboratorio})") if x and str(x) != "None")
        for par in str(p.skus).split(" ; "):
            comercio, sku = par.split(":", 1)
            filas.append({
                "id_producto": f"P{i:02d}", "id_comercio": comercio, "sku_comercio": sku,
                "ean": p.ean, "descripcion": descripcion, "principio_activo": p.principio_activo,
                "concentracion": p.concentracion, "forma": p.forma, "laboratorio": p.laboratorio,
                "unidades": "" if pd.isna(p.unidades) else int(p.unidades),
                "contenido_ml": "" if pd.isna(p.contenido_ml) else p.contenido_ml,
                "regla": "mismo EAN + mismas unidades/contenido, en los 3 comercios (misma ejecución)",
                "verificado": "",
            })
    return pd.DataFrame(filas)
