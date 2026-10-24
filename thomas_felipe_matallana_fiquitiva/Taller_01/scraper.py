from __future__ import annotations

import argparse
import gzip
import hashlib
import html
import json
import re
import sqlite3
import time
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
import uuid
from datetime import datetime, timedelta, timezone
from contextlib import closing
from pathlib import Path

ROOT = Path(__file__).resolve().parent
COLOMBIA = timezone(timedelta(hours=-5))
SOURCES = {
    "colsubsidio": {"nombre": "Colsubsidio", "host": "www.drogueriascolsubsidio.com",
                   "categorias": ["5/521/52129", "5/521/52130", "5/521/52132"]},
    "cafam": {"nombre": "Cafam", "host": "www.drogueriascafam.com.co",
              "categorias": ["2/53/59", "2/53/75", "2/53/78", "2/53/58"]},
    "larebaja": {"nombre": "La Rebaja", "host": "www.larebajavirtual.com",
                 "categorias": ["5/31", "5/28/168", "5/28/580", "5/28/166"]},
}


def now():
    return datetime.now(COLOMBIA).isoformat(timespec="seconds")


def normalized(value):
    text = html.unescape(re.sub(r"<[^>]*>", " ", str(value or "")))
    text = "".join(c for c in unicodedata.normalize("NFKD", text) if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", text).strip().lower()


def first(p, key):
    value = p.get(key)
    return str(value[0]) if isinstance(value, list) and value else ""


def save_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8")


def fetch(url, dest):
    """Archiva bytes originales y metadatos; reintenta solo errores transitorios."""
    for attempt in range(3):
        try:
            request = urllib.request.Request(url, headers={
                "User-Agent": "HigieneCorporalAcademicScraper/1.0",
                "Accept": "application/json,text/html,text/plain",
            })
            with urllib.request.urlopen(request, timeout=45) as response:
                body = response.read()
                meta = {"url": url, "fecha_hora": now(), "status": response.status,
                        "resources": response.headers.get("resources"),
                        "content_type": response.headers.get("Content-Type"),
                        "sha256": hashlib.sha256(body).hexdigest()}
            dest.parent.mkdir(parents=True, exist_ok=True)
            dest.write_bytes(gzip.compress(body, mtime=0))
            save_json(dest.with_suffix(".meta.json"), meta)
            time.sleep(0.6)
            return body, meta
        except urllib.error.HTTPError as exc:
            if exc.code not in {429, 500, 502, 503, 504} or attempt == 2:
                raise
        except (urllib.error.URLError, TimeoutError):
            if attempt == 2:
                raise
        time.sleep(2 ** (attempt + 1))
    raise RuntimeError("No fue posible completar la consulta")


def collect_store(store, run_id, raw_root):
    source = SOURCES[store]
    directory = raw_root / store / run_id
    manifest = {"ejecucion": run_id, "comercio": store, "inicio": now(),
                "estado": "fallida", "categorias": [], "auxiliares": []}
    # El árbol documenta la taxonomía del comercio y robots.txt la consulta de reglas.
    for suffix, name in [("/api/catalog_system/pub/category/tree/3", "categorias.json.gz"),
                         ("/robots.txt", "robots.txt.gz")]:
        try:
            fetch("https://" + source["host"] + suffix, directory / name)
            manifest["auxiliares"].append(name)
        except Exception as exc:
            manifest["auxiliares"].append({"archivo": name, "error": str(exc)})
    for category in source["categorias"]:
        coverage = {"categoria": category, "esperados": None, "obtenidos": 0,
                    "estado": "parcial", "paginas": [], "error": None}
        seen = set()
        try:
            start = 0
            while start < 2500:
                query = urllib.parse.urlencode({"fq": "C:/" + category + "/",
                    "_from": start, "_to": start + 49, "O": "OrderByNameASC"})
                url = "https://" + source["host"] + "/api/catalog_system/pub/products/search?" + query
                name = category.replace("/", "-") + f"_{start:04d}.json.gz"
                body, meta = fetch(url, directory / name)
                products = json.loads(body)
                if not isinstance(products, list):
                    raise ValueError("Respuesta inesperada: se esperaba una lista JSON")
                coverage["paginas"].append(name)
                if meta["resources"]:
                    total = int(meta["resources"].split("/")[-1])
                    if coverage["esperados"] is None:
                        coverage["esperados"] = total
                    elif coverage["esperados"] != total:
                        raise ValueError("El total cambió durante la paginación; repetir captura")
                ids = {str(p["productId"]) for p in products}
                if products and not ids - seen:
                    raise ValueError("La paginación repitió una página completa")
                seen.update(ids)
                start += 50
                print(f"{store}: {category}: {len(seen)}/{coverage['esperados']}", flush=True)
                if coverage["esperados"] is not None and start >= coverage["esperados"]:
                    break
                if len(products) < 50:
                    break
            # Algunos catálogos cambian el orden de nombres repetidos entre páginas.
            # Una segunda pasada inversa conserva evidencia y recupera los omitidos.
            if coverage["esperados"] is not None and len(seen) < coverage["esperados"]:
                for offset in range(0, coverage["esperados"], 50):
                    query = urllib.parse.urlencode({"fq": "C:/" + category + "/",
                        "_from": offset, "_to": offset + 49, "O": "OrderByNameDESC"})
                    url = "https://" + source["host"] + "/api/catalog_system/pub/products/search?" + query
                    name = category.replace("/", "-") + f"_desc_{offset:04d}.json.gz"
                    body, meta = fetch(url, directory / name)
                    products = json.loads(body)
                    coverage["paginas"].append(name)
                    if meta["resources"] and int(meta["resources"].split("/")[-1]) != coverage["esperados"]:
                        raise ValueError("El total cambió durante la segunda pasada")
                    seen.update(str(p["productId"]) for p in products)
                print(f"{store}: {category}: segunda pasada {len(seen)}/{coverage['esperados']}", flush=True)
            coverage["obtenidos"] = len(seen)
            if coverage["esperados"] is not None and len(seen) == coverage["esperados"]:
                coverage["estado"] = "completa"
            else:
                coverage["error"] = "No se pudo verificar igualdad con el total publicado"
        except Exception as exc:
            coverage["error"] = str(exc)
            coverage["obtenidos"] = len(seen)
        manifest["categorias"].append(coverage)
        save_json(directory / "manifest.json", manifest)
    if all(c["estado"] == "completa" for c in manifest["categorias"]):
        manifest["estado"] = "completa"
    elif any(c["paginas"] for c in manifest["categorias"]):
        manifest["estado"] = "parcial"
    manifest["fin"] = now()
    save_json(directory / "manifest.json", manifest)
    return manifest


def category_decision(p):
    """Delimitación explícita, independiente de los ocho productos elegidos."""
    name = normalized(p.get("productName"))
    path = normalized(" ".join(p.get("categories", [])))
    if re.search(r"\b(baby|bebe|bebes|infantil|kids|ninos|ninas|intimo|intima|vaginal)\b", name + " " + path):
        return False, None, "Higiene íntima o infantil"
    if re.search(r"facial|rostro|acne|asepxia|jabon.*azufre|antimicot|terbinaf|clotrimazol", name):
        return False, None, "Cuidado facial o tratamiento farmacológico"
    if any(str(i.get("ean")) == "7702057087814" for i in p.get("items", [])):
        return True, "Talcos", None  # Yodora 2x60 g: Cafam usa el nombre abreviado Of.yodora.
    if "talco" in name or "polvo desodorante" in name:
        return True, "Talcos", None
    if re.search(r"desodor|antitrans|antiperspir|deopies|deo spray|\bdte\b|(?:mexsana|yodora).*spray", name):
        return True, "Desodorantes y antitranspirantes", None
    if "desodorantes/" in path:
        return True, "Desodorantes y antitranspirantes", None
    if re.search(r"gel antibacterial|alcohol|sanitizante|desinfectante|jabon.*manos", name):
        return False, None, "Higiene de manos o desinfección, fuera de la delimitación"
    if re.search(r"polvos.*pies", name):
        return True, "Talcos", None
    if re.search(r"jabon|jabpon|\bjab\b|gel.*(ducha|bano)|body wash", name):
        return True, "Jabones y geles de ducha", None
    return False, None, "Producto fuera de alcance o identidad insuficiente"


def parse_content(text):
    """Reglas conservadoras: si hay ambigüedad no se inventa contenido."""
    text = normalized(text)
    unit = r"(kilogramos?|kg|mililitros?|ml|litros?|gramos?|grs?|g|mg)\b"
    matches = list(re.finditer(r"(\d+(?:[.,]\d+)?)\s*" + unit, text))
    if len(matches) != 1:
        return None, None
    m = matches[0]
    amount = float(m[1].replace(",", "."))
    u = m[2]
    if u.startswith("k"):
        amount *= 1000; u = "g"
    elif u.startswith("l"):
        amount *= 1000; u = "mL"
    elif u.startswith("mil") or u == "ml":
        u = "mL"
    elif u != "mg":
        u = "g"
    before, after = text[:m.start()], text[m.end():]
    multi = re.search(r"(\d+)\s*x\s*$", before) or re.search(r"^\s*x\s*(\d+)\b", after)
    if multi:
        amount *= int(multi[1])
    if "+" in text or "gratis" in text:
        return None, None
    return (amount, u) if amount > 0 else (None, None)


def general_content(p, store):
    if store == "colsubsidio":
        presentation = first(p, "Presentación")
        return parse_content(presentation)
    if store == "larebaja":
        return parse_content(first(p, "Presentacionunidadmedida"))
    # Cafam suele publicar el contenido de una unidad, no del paquete.
    name = p.get("productName", "")
    content = first(p, "Contenido")
    amount, unit = parse_content(content)
    if amount is None:
        return parse_content(name)
    n = normalized(name)
    multipack = re.search(r"(\d+)\s*(?:unds?\b|unidades\b|frascos\b|barras\b)", n)
    if multipack:
        amount *= int(multipack[1])
    elif re.search(r"oferta|\bof\.|\bx\s*2\b|\+", n):
        return None, None
    return amount, unit


def load_matches():
    return json.loads((ROOT / "correspondencias.json").read_text(encoding="utf-8"))


def parse_store(store, run_id, raw_root):
    directory = raw_root / store / run_id
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    mapping = {str(p["skus"][store]): p for p in load_matches()}
    rows, seen = [], set()
    for coverage in manifest["categorias"]:
        for name in coverage["paginas"]:
            path = directory / name
            body = gzip.decompress(path.read_bytes())
            meta = json.loads(path.with_suffix(".meta.json").read_text(encoding="utf-8"))
            if hashlib.sha256(body).hexdigest() != meta["sha256"]:
                raise ValueError(f"La copia cruda fue modificada: {path}")
            for p in json.loads(body):
                included, subcategory, reason = category_decision(p)
                for item in p.get("items", []):
                    sku = str(item["itemId"])
                    if sku in seen:
                        continue  # mismo SKU listado en varias categorías
                    seen.add(sku)
                    sellers = item.get("sellers", [])
                    seller = next((s for s in sellers if s.get("sellerId") == "1"), None)
                    offer = seller.get("commertialOffer", {}) if seller else {}
                    price = offer.get("Price")
                    price = float(price) if isinstance(price, (int, float)) and price > 0 else None
                    old_price = offer.get("ListPrice")
                    old_price = float(old_price) if isinstance(old_price, (int, float)) and old_price > 0 else None
                    available = int(bool(offer.get("IsAvailable")))
                    specs = {k: p[k] for k in p.get("allSpecifications", []) if k in p}
                    original_name = p.get("productName", "")
                    description = p.get("description", "")
                    extra_description = first(p, "DescripcionContenido")
                    original = json.dumps({"nombre": original_name, "nombre_sku": item.get("name"),
                        "descripcion": description, "ficha": specs}, ensure_ascii=False)
                    brand = first(p, "Marca Comercial") or first(p, "ML_Marca") or p.get("brand")
                    content, unit = general_content(p, store)
                    match = mapping.get(sku)
                    issue = None
                    common_id = None
                    if match:
                        expected = match["ean"].lstrip("0")
                        actual = str(item.get("ean") or "").lstrip("0")
                        if actual != expected:
                            issue = "EAN cambió: requiere revisión de correspondencia"
                        elif not included:
                            issue = "Producto seleccionado quedó fuera de categoría"
                        else:
                            common_id = match["id"]
                            brand = match["marca"]
                            content, unit = match["contenido_total"], match["unidad"]
                            issue = match.get("incidencias", {}).get(store)
                    rows.append({"ejecucion": run_id, "comercio": store, "sku": sku,
                        "producto_sitio": str(p["productId"]), "ean": item.get("ean") or None,
                        "producto_comun": common_id, "nombre_original": original_name,
                        "nombre_limpio": normalized(original_name), "marca": normalized(brand) or None,
                        "subcategoria": subcategory, "presentacion_original": first(p, "Presentación") or first(p, "Presentacionunidadmedida"),
                        "descripcion_original": description, "descripcion_adicional": extra_description,
                        "texto_original": original, "precio": price, "precio_lista": old_price,
                        "promocion": int(price < old_price) if price and old_price else None,
                        "disponible": available, "url": p.get("link"), "url_extraccion": meta["url"],
                        "fecha_hora": meta["fecha_hora"], "contenido_total": content, "unidad": unit,
                        "comparable": int(common_id is not None), "incluido": int(included),
                        "motivo_exclusion": reason, "incidencia": issue,
                        "archivo_raw": path.relative_to(raw_root.parent).as_posix()})
    return rows, manifest


def scrape_colsubsidio(run_id, raw_root=ROOT / "raw", offline=False):
    if not offline:
        collect_store("colsubsidio", run_id, Path(raw_root))
    return parse_store("colsubsidio", run_id, Path(raw_root))


def scrape_cafam(run_id, raw_root=ROOT / "raw", offline=False):
    if not offline:
        collect_store("cafam", run_id, Path(raw_root))
    return parse_store("cafam", run_id, Path(raw_root))


def scrape_larebaja(run_id, raw_root=ROOT / "raw", offline=False):
    if not offline:
        collect_store("larebaja", run_id, Path(raw_root))
    return parse_store("larebaja", run_id, Path(raw_root))


SCHEMA = """
PRAGMA foreign_keys=ON;
CREATE TABLE IF NOT EXISTS comercios(id TEXT PRIMARY KEY, nombre TEXT NOT NULL, dominio TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS productos(
 id TEXT PRIMARY KEY, nombre TEXT NOT NULL, marca TEXT NOT NULL, variante TEXT NOT NULL,
 presentacion TEXT NOT NULL, contenido_total REAL NOT NULL CHECK(contenido_total>0),
 unidad TEXT NOT NULL, unidades_empaque INTEGER NOT NULL, grupo_formulacion TEXT NOT NULL,
 ean TEXT NOT NULL, evidencia TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS ejecuciones(
 id TEXT NOT NULL, comercio TEXT NOT NULL REFERENCES comercios(id), inicio TEXT NOT NULL,
 fin TEXT, estado TEXT NOT NULL, registros INTEGER NOT NULL, incluidos INTEGER NOT NULL,
 PRIMARY KEY(id,comercio));
CREATE TABLE IF NOT EXISTS cobertura(
 ejecucion TEXT NOT NULL, comercio TEXT NOT NULL, categoria TEXT NOT NULL,
 esperados INTEGER, obtenidos INTEGER, estado TEXT, error TEXT,
 PRIMARY KEY(ejecucion,comercio,categoria),
 FOREIGN KEY(ejecucion,comercio) REFERENCES ejecuciones(id,comercio));
CREATE TABLE IF NOT EXISTS observaciones(
 id INTEGER PRIMARY KEY, ejecucion TEXT NOT NULL, comercio TEXT NOT NULL,
 sku TEXT NOT NULL, producto_sitio TEXT NOT NULL, ean TEXT,
 producto_comun TEXT REFERENCES productos(id), nombre_original TEXT NOT NULL,
 nombre_limpio TEXT, marca TEXT, subcategoria TEXT, presentacion_original TEXT,
 descripcion_original TEXT, descripcion_adicional TEXT, texto_original TEXT NOT NULL,
 precio REAL CHECK(precio IS NULL OR precio>0), precio_lista REAL,
 promocion INTEGER, disponible INTEGER NOT NULL, url TEXT, url_extraccion TEXT NOT NULL,
 fecha_hora TEXT NOT NULL, contenido_total REAL, unidad TEXT,
 comparable INTEGER NOT NULL, incluido INTEGER NOT NULL, motivo_exclusion TEXT,
 incidencia TEXT, archivo_raw TEXT NOT NULL,
 UNIQUE(comercio,sku,fecha_hora), UNIQUE(ejecucion,comercio,sku),
 FOREIGN KEY(ejecucion,comercio) REFERENCES ejecuciones(id,comercio));
CREATE INDEX IF NOT EXISTS idx_comparacion ON observaciones(ejecucion,producto_comun,comercio);
CREATE VIEW IF NOT EXISTS categoria AS SELECT * FROM observaciones WHERE incluido=1;
CREATE VIEW IF NOT EXISTS comparables AS
 SELECT o.*, p.nombre AS producto, p.presentacion, p.grupo_formulacion,
 p.unidades_empaque FROM observaciones o JOIN productos p ON p.id=o.producto_comun
 WHERE o.incluido=1 AND o.comparable=1 AND o.precio IS NOT NULL AND o.disponible=1;
"""


def insert_rows(db, rows, manifest):
    with closing(sqlite3.connect(db)) as con:
        con.executescript(SCHEMA)
        for store, source in SOURCES.items():
            con.execute("INSERT OR IGNORE INTO comercios VALUES(?,?,?)", (store, source["nombre"], source["host"]))
        for p in load_matches():
            con.execute("INSERT OR IGNORE INTO productos VALUES(?,?,?,?,?,?,?,?,?,?,?)",
                (p["id"], p["nombre"], p["marca"], p["variante"], p["presentacion"],
                 p["contenido_total"], p["unidad"], p["unidades_empaque"], p["grupo_formulacion"], p["ean"], p["evidencia"]))
        con.execute("INSERT OR IGNORE INTO ejecuciones VALUES(?,?,?,?,?,?,?)",
            (manifest["ejecucion"], manifest["comercio"], manifest["inicio"], manifest.get("fin"),
             manifest["estado"], len(rows), sum(r["incluido"] for r in rows)))
        for c in manifest["categorias"]:
            con.execute("INSERT OR IGNORE INTO cobertura VALUES(?,?,?,?,?,?,?)",
                (manifest["ejecucion"], manifest["comercio"], c["categoria"], c["esperados"], c["obtenidos"], c["estado"], c["error"]))
        if rows:
            keys = list(rows[0])
            con.executemany(f"INSERT OR IGNORE INTO observaciones({','.join(keys)}) VALUES({','.join('?' for _ in keys)})",
                            [tuple(r[k] for k in keys) for r in rows])
        con.commit()


def run_collection(offline=False, run_id=None, db=ROOT / "comparador_precios.sqlite", raw_root=ROOT / "raw"):
    """Una corrida global; mismo ID para las tres tiendas. Inserción idempotente."""
    raw_root, db = Path(raw_root), Path(db)
    db.parent.mkdir(parents=True, exist_ok=True)
    if offline and run_id is None:
        sets = [{p.name for p in (raw_root / store).glob("*") if (p / "manifest.json").exists()} for store in SOURCES]
        common = set.intersection(*sets)
        if not common:
            raise FileNotFoundError("No hay una ejecución archivada para los tres comercios")
        run_id = sorted(common)[-1]
    run_id = run_id or datetime.now(COLOMBIA).strftime("%Y%m%dT%H%M%S") + "_" + uuid.uuid4().hex[:8]
    for func in [scrape_colsubsidio, scrape_cafam, scrape_larebaja]:
        rows, manifest = func(run_id, raw_root, offline)
        insert_rows(db, rows, manifest)
    return run_id


def validate(db, run_id):
    with closing(sqlite3.connect(db)) as con:
        cells = con.execute("SELECT producto_comun,comercio,COUNT(*) FROM comparables WHERE ejecucion=? GROUP BY 1,2", (run_id,)).fetchall()
        expected = {(p["id"], s) for p in load_matches() for s in SOURCES}
        actual = {(p,s) for p,s,n in cells if n == 1}
        return {"ejecucion": run_id, "observaciones_comparables": sum(n for _,_,n in cells),
                "faltantes_o_duplicados": sorted(expected - actual),
                "cobertura_completa": con.execute("SELECT COUNT(*) FROM ejecuciones WHERE id=? AND estado!='completa'", (run_id,)).fetchone()[0] == 0,
                "integridad": con.execute("PRAGMA integrity_check").fetchone()[0]}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true", help="Reprocesar raw sin conexiones HTTP")
    parser.add_argument("--run-id", help="ID archivado en modo offline")
    parser.add_argument("--db", type=Path, default=ROOT / "comparador_precios.sqlite")
    args = parser.parse_args()
    run_id = run_collection(offline=args.offline, run_id=args.run_id, db=args.db)
    print(json.dumps(validate(args.db, run_id), ensure_ascii=False, indent=2))
