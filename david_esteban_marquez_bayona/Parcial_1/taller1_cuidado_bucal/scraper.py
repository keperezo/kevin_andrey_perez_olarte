"""
scraper.py — Extracción del Taller 1 · Minería de Datos (2016325) · UNAL · categoría: cuidado bucal.

Éxito, Olímpica y Locatel usan la plataforma VTEX: sus páginas de categoría se llenan con servicios
JSON públicos del propio sitio, que aquí se consultan con peticiones HTTP directas (requests):
  1. GET /api/catalog_system/pub/category/tree/4 -> se eligen las secciones cuyo nombre es
     «cuidado oral/bucal/dental» (o, si no existen, «cremas dentales», «cepillos dentales», …).
  2. GET /api/catalog_system/pub/products/search?fq=C:/<ids>/&_from=&_to= -> 50 productos por
     página; el encabezado `resources: 0-49/N` informa el total N que reporta el sitio.
  3. Cada respuesta se guarda sin modificar en raw/<comercio>/<fecha>/<ejecución>/ (gzip) con un
     manifest.json. Los datos SIEMPRE se procesan desde raw/, así que en línea y sin conexión el
     resultado es idéntico.

Uso:  python scraper.py                    nueva ejecución en línea
      python scraper.py --sin-conexion     reprocesa raw/ sin consultar los sitios
      python scraper.py --diagnostico exito | --robots | --probar
"""
import argparse, gzip, json, os, platform, re, sqlite3, sys, tempfile, time
from datetime import datetime
from pathlib import Path
from urllib.robotparser import RobotFileParser
from zoneinfo import ZoneInfo

import pandas as pd
import requests

import limpieza

TZ = ZoneInfo("America/Bogota")
RAIZ = Path(__file__).resolve().parent
DB_PATH, RAW_DIR = RAIZ / "comparador_precios.sqlite", RAIZ / "raw"
PAGINA, LIMITE, PAUSA, INTENTOS = 50, 2500, 1.0, 4   # VTEX no pagina más allá de _from = 2500
USER_AGENT = ("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) "
              "Chrome/129.0 Safari/537.36 TallerMineriaDatosUNAL/1.0 (uso academico)")
ARBOL, BUSQUEDA = "/api/catalog_system/pub/category/tree/4", "/api/catalog_system/pub/products/search"
PATRON = r"\b(cuidado|salud|higiene)\s+(oral|bucal|dental)\b"
RESPALDO = r"\b(cremas?|pastas?) (dental|dentales)\b|\bcepillos? (dental|dentales)\b|\benjuagues? bucal|\b(seda|hilo) dental"
EXCLUIR = r"\b(mascotas?|perros?|gatos?)\b"

COMERCIOS = {   # comercio_id: (nombre, tipo, url_base)
    "exito": ("Éxito", "Gran superficie", "https://www.exito.com"),
    "olimpica": ("Olímpica", "Supermercado y droguería", "https://www.olimpica.com"),
    "locatel": ("Locatel", "Droguería / salud", "https://www.locatelcolombia.com"),
}
FILTROS = {}    # para fijar secciones a mano en vez de buscarlas, p. ej. {"exito": ["/12/345/"]}

COLUMNAS = [
    "comercio_id", "ejecucion_id", "fecha_hora_consulta", "url_fuente", "seccion_id", "seccion_nombre",
    "product_id_comercio", "sku_comercio", "ean", "referencia",
    "nombre_original", "nombre_item_original", "marca_original", "categoria_original",
    "descripcion_original", "ficha_tecnica_original", "texto_original", "url_producto",
    "precio", "precio_lista", "es_promocion", "texto_promocion", "disponible", "vendedor",
]


def ahora_iso():
    """Fecha y hora de Colombia en ISO 8601, p. ej. 2026-10-07T10:35:00-05:00."""
    return datetime.now(TZ).replace(microsecond=0).isoformat()


def nueva_sesion():
    s = requests.Session()
    s.headers.update({"User-Agent": USER_AGENT, "Accept": "application/json, text/plain, */*",
                      "Accept-Language": "es-CO,es;q=0.9"})
    return s


def _get(sesion, url, params=None):
    """GET con reintentos y espera exponencial ante 429/5xx o fallas de red."""
    error = None
    for i in range(INTENTOS):
        try:
            r = sesion.get(url, params=params, timeout=30)
            if r.status_code not in (429, 500, 502, 503, 504):
                return r
            error = f"HTTP {r.status_code}"
        except requests.RequestException as e:
            error = repr(e)
        time.sleep(PAUSA * 2 ** (i + 1))
    raise RuntimeError(f"{url}: {error}")


def _num(v):
    try:
        v = float(v)
        return v if v > 0 else None      # un precio 0 o ausente queda como NULL, nunca 0
    except (TypeError, ValueError):
        return None


# ---------------------------------------------------------------------------
# Fase en línea: consultar el sitio y guardar las respuestas en raw/
# ---------------------------------------------------------------------------

def secciones(arbol):
    """Secciones más altas del árbol que corresponden a cuidado bucal (sin bajar dentro de ellas)."""
    def buscar(patron, nodos, ids=(), nombres=()):
        for n in nodos or []:
            ids2, nombres2 = ids + (str(n["id"]),), nombres + (n.get("name", ""),)
            ruta = " > ".join(nombres2)
            if re.search(patron, limpieza.normalizar_texto(n.get("name"))) and \
                    not re.search(EXCLUIR, limpieza.normalizar_texto(ruta)):
                yield {"seccion_id": str(n["id"]), "nombre": n.get("name"), "ruta_nombres": ruta,
                       "filtro": "/" + "/".join(ids2) + "/", "url": n.get("url")}
            else:
                yield from buscar(patron, n.get("children"), ids2, nombres2)
    return list(buscar(PATRON, arbol)) or list(buscar(RESPALDO, arbol))


def descargar(cid, ejecucion_id, raw_dir=RAW_DIR, sesion=None):
    """Recorre todas las páginas de la categoría y guarda cada respuesta original. Devuelve la carpeta."""
    nombre, _, base = COMERCIOS[cid]
    sesion, inicio = sesion or nueva_sesion(), ahora_iso()
    carpeta = Path(raw_dir) / cid / inicio[:10] / ejecucion_id
    carpeta.mkdir(parents=True, exist_ok=True)
    man = {"ejecucion_id": ejecucion_id, "comercio_id": cid, "comercio": nombre, "url_base": base, "inicio": inicio,
           "fin": None, "user_agent": USER_AGENT, "secciones": [], "peticiones": [], "errores": []}

    def guardar(r, tipo, seccion_id=None, params=None):
        archivo = f"{len(man['peticiones']):04d}_{tipo}" + (f"_s{seccion_id}" if seccion_id else "") + ".json.gz"
        (carpeta / archivo).write_bytes(gzip.compress(r.content))
        man["peticiones"].append({"archivo": archivo, "tipo": tipo, "seccion_id": seccion_id, "url": r.url,
                                  "params": params, "fecha_hora_consulta": ahora_iso(), "status": r.status_code,
                                  "resources": r.headers.get("resources"), "bytes": len(r.content)})
        r.raise_for_status()
        return r.json()

    try:
        if cid in FILTROS:
            secs = [{"seccion_id": f"manual{i}", "nombre": f, "ruta_nombres": f, "filtro": f, "url": None}
                    for i, f in enumerate(FILTROS[cid])]
        else:
            secs = secciones(guardar(_get(sesion, base + ARBOL), "arbol"))
            if not secs:
                raise RuntimeError("no se encontró una sección de cuidado bucal (ver --diagnostico)")
        for sec in secs:
            sec["api"], sec["total_reportado"], desde = "catalogo", None, 0
            try:
                while desde < LIMITE:
                    time.sleep(PAUSA)
                    params = {"fq": f"C:{sec['filtro']}", "_from": desde, "_to": desde + PAGINA - 1, "O": "OrderByNameASC"}
                    r = _get(sesion, base + BUSQUEDA, params)
                    lote = guardar(r, "productos", sec["seccion_id"], params)
                    total = re.search(r"/(\d+)\s*$", r.headers.get("resources") or "")
                    sec["total_reportado"] = int(total.group(1)) if total else sec["total_reportado"]
                    desde += PAGINA
                    if len(lote) < PAGINA or desde >= (sec["total_reportado"] or LIMITE):
                        break
            except Exception as e:
                sec["error"] = str(e)
                man["errores"].append(f"sección {sec['seccion_id']}: {e}")
            man["secciones"].append(sec)
    except Exception as e:
        man["errores"].append(f"general: {e}")
    man["fin"] = ahora_iso()
    (carpeta / "manifest.json").write_text(json.dumps(man, ensure_ascii=False, indent=1), encoding="utf-8")
    return carpeta


# ---------------------------------------------------------------------------
# Procesamiento de las copias crudas (igual en línea y sin conexión)
# ---------------------------------------------------------------------------

def parsear_producto(prod, man, pet, sec):
    """Un registro por SKU del producto VTEX, con las columnas de COLUMNAS."""
    base = man["url_base"]
    ficha = {n: prod.get(n) for n in prod.get("allSpecifications") or []}
    ficha_txt = json.dumps(ficha, ensure_ascii=False) if ficha else None
    url = prod.get("link") or (f"{base}/{prod['linkText']}/p" if prod.get("linkText") else None)
    url = base + url if url and url.startswith("/") else url
    filas = []
    for item in prod.get("items") or []:
        vendedores = item.get("sellers") or [{}]
        vendedor = next((v for v in vendedores if v.get("sellerDefault")), None) or next(
            (v for v in vendedores if _num((v.get("commertialOffer") or {}).get("Price"))), vendedores[0])
        o = vendedor.get("commertialOffer") or {}
        precio, lista = _num(o.get("Price")), _num(o.get("ListPrice")) or _num(o.get("PriceWithoutDiscount"))
        promos = [t.get("name") or t.get("<Name>k__BackingField") for t in (o.get("Teasers") or [])
                  + (o.get("PromotionTeasers") or []) + (o.get("DiscountHighLight") or []) if isinstance(t, dict)]
        disponible = o.get("IsAvailable")
        if disponible is None and o.get("AvailableQuantity") is not None:
            disponible = o["AvailableQuantity"] > 0
        nombre_item = item.get("nameComplete") or item.get("name")
        ref = item.get("referenceId")
        ref = ";".join(str(x.get("Value")) for x in ref if x.get("Value")) if isinstance(ref, list) else ref
        filas.append({
            "comercio_id": man["comercio_id"], "ejecucion_id": man["ejecucion_id"],
            "fecha_hora_consulta": pet["fecha_hora_consulta"], "url_fuente": pet["url"],
            "seccion_id": sec.get("seccion_id"), "seccion_nombre": sec.get("ruta_nombres"),
            "product_id_comercio": str(prod.get("productId")), "sku_comercio": str(item.get("itemId")),
            "ean": item.get("ean") or None, "referencia": ref or None,
            "nombre_original": prod.get("productName"), "nombre_item_original": nombre_item,
            "marca_original": prod.get("brand"), "categoria_original": (prod.get("categories") or [None])[0],
            "descripcion_original": prod.get("description") or None, "ficha_tecnica_original": ficha_txt,
            "texto_original": "\n".join(x for x in [prod.get("productName"), nombre_item if nombre_item !=
                                                    prod.get("productName") else None, prod.get("description"), ficha_txt] if x),
            "url_producto": url, "precio": precio, "precio_lista": lista,
            "es_promocion": int(precio is not None and lista is not None and lista - precio >= 1),
            "texto_promocion": "; ".join(t for t in promos if t) or None,
            "disponible": None if disponible is None else int(bool(disponible)), "vendedor": vendedor.get("sellerName"),
        })
    return filas


def procesar_raw(carpeta):
    """Lee manifest.json y las respuestas guardadas. Devuelve el DataFrame estándar; en df.attrs
    quedan la metadata de la ejecución (estado, totales) y las secciones recorridas."""
    carpeta = Path(carpeta)
    man = json.loads((carpeta / "manifest.json").read_text(encoding="utf-8"))
    secs = {s["seccion_id"]: s for s in man["secciones"]}
    filas, ids = [], {}
    for pet in man["peticiones"]:
        if pet["tipo"].startswith("productos") and pet["status"] in (200, 206):
            datos = json.loads(gzip.decompress((carpeta / pet["archivo"]).read_bytes()))
            for prod in datos["products"] if isinstance(datos, dict) else datos:
                ids.setdefault(pet["seccion_id"], set()).add(str(prod.get("productId")))
                filas += parsear_producto(prod, man, pet, secs.get(pet["seccion_id"], {}))
    df = pd.DataFrame(filas, columns=COLUMNAS)
    repetidos = int(df.duplicated("sku_comercio").sum())
    df = df.drop_duplicates("sku_comercio").reset_index(drop=True)
    notas, parcial = list(man["errores"]), False
    for sid, s in secs.items():
        s["n_obtenidos"] = len(ids.get(sid, ()))
        if s.get("error") or (s.get("total_reportado") or 0) > s["n_obtenidos"]:
            parcial = True
            notas.append(f"sección {sid}: {s['n_obtenidos']} de {s.get('total_reportado')} productos")
    if repetidos:
        notas.append(f"{repetidos} SKU repetidos entre páginas eliminados")
    df.attrs["secciones"] = list(secs.values())
    df.attrs["meta"] = {
        "ejecucion_id": man["ejecucion_id"], "comercio_id": man["comercio_id"], "inicio": man["inicio"],
        "fin": man["fin"], "n_registros": len(df), "n_productos": int(df["product_id_comercio"].nunique()),
        "total_reportado_sitio": sum(s.get("total_reportado") or 0 for s in secs.values()) or None,
        "estado": "fallida" if df.empty else "parcial" if parcial else "completa",
        "mensaje": " | ".join(notas) or None, "carpeta_raw": carpeta.relative_to(carpeta.parents[3]).as_posix(),
    }
    return df


def _extraer(cid, ejecucion_id, modo, raw_dir, sesion):
    if modo == "en_linea":
        return procesar_raw(descargar(cid, ejecucion_id, raw_dir, sesion))
    manifiestos = sorted(Path(raw_dir).glob(f"{cid}/*/{ejecucion_id}/manifest.json"))
    if not manifiestos:
        raise FileNotFoundError(f"no hay copias crudas de {cid} para {ejecucion_id} en {raw_dir}")
    return procesar_raw(manifiestos[0].parent)


# Una función por comercio: misma firma y mismas columnas de salida (COLUMNAS)
def extraer_exito(ejecucion_id, modo="en_linea", raw_dir=RAW_DIR, sesion=None):
    """Éxito (exito.com): categoría de cuidado oral vía API de catálogo VTEX."""
    return _extraer("exito", ejecucion_id, modo, raw_dir, sesion)


def extraer_olimpica(ejecucion_id, modo="en_linea", raw_dir=RAW_DIR, sesion=None):
    """Olímpica (olimpica.com): categoría de cuidado oral vía API de catálogo VTEX."""
    return _extraer("olimpica", ejecucion_id, modo, raw_dir, sesion)


def extraer_locatel(ejecucion_id, modo="en_linea", raw_dir=RAW_DIR, sesion=None):
    """Locatel (locatelcolombia.com): categoría de cuidado oral vía API de catálogo VTEX."""
    return _extraer("locatel", ejecucion_id, modo, raw_dir, sesion)


FUNCIONES = {"exito": extraer_exito, "olimpica": extraer_olimpica, "locatel": extraer_locatel}

# ---------------------------------------------------------------------------
# Base de datos SQLite (estructura en esquema.sql)
# ---------------------------------------------------------------------------
ESQUEMA = (RAIZ / "esquema.sql").read_text(encoding="utf-8")   # tablas, restricciones y vista


def conectar(db_path=DB_PATH):
    con = sqlite3.connect(db_path)
    con.executescript(ESQUEMA)
    return con


def insertar(con, cid, df, modo):
    """Inserta la ejecución de un comercio sin sobrescribir. Devuelve el número de observaciones nuevas."""
    meta = df.attrs["meta"]
    con.execute("INSERT OR IGNORE INTO comercios VALUES (?, ?, ?, ?, 'VTEX')", (cid, *COMERCIOS[cid]))
    nueva = con.execute("""INSERT OR IGNORE INTO ejecuciones VALUES (:ejecucion_id, :comercio_id, :inicio, :fin, :modo,
        :n_registros, :n_productos, :total_reportado_sitio, :estado, :mensaje, :carpeta_raw)""", {**meta, "modo": modo})
    if nueva.rowcount == 0:          # esa ejecución ya estaba cargada
        return 0
    con.executemany("""INSERT OR IGNORE INTO secciones VALUES (:e, :c, :seccion_id, :nombre, :ruta_nombres, :filtro, :url, :api,
        :total_reportado, :n_obtenidos, :error)""",
                    [{"e": meta["ejecucion_id"], "c": cid, "url": None, "api": None, "total_reportado": None,
                      "n_obtenidos": None, "error": None, **s} for s in df.attrs["secciones"]])
    nuevas = 0
    for fila in df.to_dict("records"):
        r = {k: None if pd.isna(v) else v for k, v in fila.items()}
        con.execute("""INSERT INTO anuncios (comercio_id, sku_comercio, product_id_comercio, ean, referencia,
                nombre_original, nombre_item_original, marca_original, categoria_original, descripcion_original,
                ficha_tecnica_original, texto_original, url_producto, seccion_origen, primera_consulta, ultima_consulta)
            VALUES (:comercio_id, :sku_comercio, :product_id_comercio, :ean, :referencia, :nombre_original,
                :nombre_item_original, :marca_original, :categoria_original, :descripcion_original,
                :ficha_tecnica_original, :texto_original, :url_producto, :seccion_nombre, :fecha_hora_consulta,
                :fecha_hora_consulta)
            ON CONFLICT (comercio_id, sku_comercio) DO UPDATE SET
                primera_consulta = MIN(primera_consulta, excluded.primera_consulta),
                ultima_consulta = MAX(ultima_consulta, excluded.ultima_consulta)""", r)
        r["anuncio_id"] = con.execute("SELECT anuncio_id FROM anuncios WHERE comercio_id = ? AND sku_comercio = ?",
                                      (cid, r["sku_comercio"])).fetchone()[0]
        nuevas += con.execute("""INSERT OR IGNORE INTO observaciones (ejecucion_id, comercio_id, anuncio_id,
                sku_comercio, fecha_hora_consulta, precio, precio_lista, es_promocion, texto_promocion, disponible,
                vendedor, url_fuente)
            VALUES (:ejecucion_id, :comercio_id, :anuncio_id, :sku_comercio, :fecha_hora_consulta, :precio,
                :precio_lista, :es_promocion, :texto_promocion, :disponible, :vendedor, :url_fuente)""", r).rowcount
    con.commit()
    return nuevas


def ejecutar_recoleccion(comercios=None, modo="en_linea", db_path=DB_PATH, raw_dir=RAW_DIR,
                         ejecucion_id=None, verbose=True):
    """Recolección completa con una sola llamada: extrae cada comercio, registra la ejecución, inserta
    en SQLite sin sobrescribir y actualiza la correspondencia de productos (limpieza.py).
    modo="sin_conexion" reprocesa las copias crudas de `ejecucion_id` sin consultar los sitios."""
    comercios = comercios or list(COMERCIOS)
    if modo == "en_linea":
        ejecucion_id = ejecucion_id or "E" + datetime.now(TZ).strftime("%Y%m%dT%H%M%S")
        revisar_robots(comercios, raw_dir)
    con, sesion, resumen = conectar(db_path), nueva_sesion(), []
    for cid in comercios:
        if verbose:
            print(f"[{ejecucion_id}] {COMERCIOS[cid][0]}…", flush=True)
        try:
            df = FUNCIONES[cid](ejecucion_id, modo, raw_dir, sesion)
        except Exception as e:
            df = pd.DataFrame(columns=COLUMNAS)
            df.attrs = {"secciones": [], "meta": {
                "ejecucion_id": ejecucion_id, "comercio_id": cid, "inicio": ahora_iso(), "fin": ahora_iso(),
                "n_registros": 0, "n_productos": 0, "total_reportado_sitio": None, "estado": "fallida",
                "mensaje": str(e)[:500], "carpeta_raw": None}}
        m = df.attrs["meta"]
        resumen.append({"ejecucion_id": ejecucion_id, "comercio": COMERCIOS[cid][0], "estado": m["estado"],
                        "productos": m["n_productos"], "registros_sku": m["n_registros"],
                        "total_reportado_sitio": m["total_reportado_sitio"],
                        "observaciones_nuevas": insertar(con, cid, df, modo), "mensaje": m["mensaje"]})
        if verbose:
            print("    " + ", ".join(f"{k}={v}" for k, v in list(resumen[-1].items())[2:]), flush=True)
    corr = limpieza.actualizar_correspondencias(con)
    con.close()
    if verbose:
        print(f"Ejecución de referencia {corr['ejecucion_referencia']}: {corr['n_comparables']} presentaciones "
              f"comparables en {len(corr['comercios'])} comercios.")
    return pd.DataFrame(resumen)


def reprocesar_sin_conexion(db_path=DB_PATH, raw_dir=RAW_DIR, ejecuciones=None, verbose=True):
    """Modo sin conexión: carga en la base todas las ejecuciones guardadas en raw/ (o las indicadas)."""
    grupos = {}
    for m in sorted(Path(raw_dir).glob("*/*/*/manifest.json")):
        eid, cid = m.parent.name, m.parents[2].name
        if cid in COMERCIOS and (ejecuciones is None or eid in ejecuciones):
            grupos.setdefault(eid, []).append(cid)
    if not grupos:
        raise FileNotFoundError(f"no hay copias crudas en {raw_dir}")
    return pd.concat([ejecutar_recoleccion(sorted(c), "sin_conexion", db_path, raw_dir, e, verbose)
                      for e, c in sorted(grupos.items())], ignore_index=True)


# ---------------------------------------------------------------------------
# Condiciones de uso, diagnóstico y prueba en otro entorno
# ---------------------------------------------------------------------------

def revisar_robots(comercios=None, raw_dir=RAW_DIR, modo="en_linea"):
    """¿Permite robots.txt las rutas usadas? En línea guarda una copia en raw/<comercio>/robots/."""
    filas = []
    for cid in comercios or COMERCIOS:
        nombre, _, base = COMERCIOS[cid]
        carpeta = Path(raw_dir) / cid / "robots"
        try:
            if modo == "en_linea":
                r = _get(nueva_sesion(), base + "/robots.txt")
                carpeta.mkdir(parents=True, exist_ok=True)
                (carpeta / f"robots_{ahora_iso()[:10]}.txt.gz").write_bytes(gzip.compress(r.content))
            copias = sorted(carpeta.glob("robots_*.txt.gz"))
            if not copias:
                raise FileNotFoundError("sin copia de robots.txt (ejecute python scraper.py --robots)")
            rp = RobotFileParser()
            rp.parse(gzip.decompress(copias[-1].read_bytes()).decode("utf-8", "replace").splitlines())
            filas += [{"comercio": nombre, "ruta": ruta, "permitido": rp.can_fetch(USER_AGENT, base + ruta),
                       "crawl_delay": rp.crawl_delay(USER_AGENT), "fecha_revision": copias[-1].name[7:17]}
                      for ruta in (ARBOL, BUSQUEDA)]
        except Exception as e:
            filas.append({"comercio": nombre, "error": str(e)[:200]})
    return pd.DataFrame(filas)


def diagnostico(cid):
    """Secciones del árbol del comercio que se tomarían como cuidado bucal."""
    r = _get(nueva_sesion(), COMERCIOS[cid][2] + ARBOL)
    r.raise_for_status()
    return pd.DataFrame(secciones(r.json()))


def probar_entorno(salida=RAIZ / "prueba_entorno.json"):
    """Recolección completa en una carpeta temporal (no toca la base ni raw/ del proyecto)."""
    with tempfile.TemporaryDirectory() as tmp:
        res = ejecutar_recoleccion(None, "en_linea", Path(tmp) / "prueba.sqlite", Path(tmp) / "raw", verbose=False)
    info = {"fecha_hora": ahora_iso(), "python": sys.version.split()[0], "sistema": platform.platform(),
            "colab": any(k.startswith("COLAB_") for k in os.environ), "resultados": res.to_dict("records")}
    Path(salida).write_text(json.dumps(info, ensure_ascii=False, indent=1, default=str), encoding="utf-8")
    return info


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Scraper de cuidado bucal: Éxito, Olímpica y Locatel")
    ap.add_argument("--sin-conexion", action="store_true", help="reprocesa raw/ sin consultar los sitios")
    ap.add_argument("--db", default=DB_PATH, help="base SQLite de destino")
    ap.add_argument("--diagnostico", choices=list(COMERCIOS), help="muestra las secciones que se recorrerán")
    ap.add_argument("--robots", action="store_true", help="revisa y guarda robots.txt")
    ap.add_argument("--probar", action="store_true", help="prueba en otro entorno (p. ej. Google Colab)")
    a = ap.parse_args()
    pd.set_option("display.width", 200)
    if a.diagnostico:
        print(diagnostico(a.diagnostico).to_string())
    elif a.robots:
        print(revisar_robots().to_string())
    elif a.probar:
        print(json.dumps(probar_entorno(), ensure_ascii=False, indent=1, default=str))
    elif a.sin_conexion:
        print(reprocesar_sin_conexion(a.db).to_string())
    else:
        print(ejecutar_recoleccion(db_path=a.db).to_string())
