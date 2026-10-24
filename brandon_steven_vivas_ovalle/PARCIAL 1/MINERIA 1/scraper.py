"""
scraper.py - Taller 1 Minería de Datos (UNAL)
Categoría: pañales infantiles, pañitos, cambiadores desechables y cremas de barrera no medicinales.

Estrategia
----------
Los tres comercios por defecto (Éxito, Jumbo, Olímpica) usan la plataforma VTEX, que expone
los servicios JSON que la propia página usa para listar productos:
  * Modo "legacy":      GET {base}/api/catalog_system/pub/products/search?fq=C:/id/  |  ft=texto&_from=0&_to=49
  * Modo "intelligent": GET {base}/api/io/_v/api/intelligent-search/product_search?query=texto&page=1&count=50
El modo se detecta automáticamente por comercio (ver `detectar_modo`). Para cada comercio:
  1. Se descubre el árbol de categorías (modo legacy) y se recorren los nodos cuyo nombre sugiere
     la categoría (pañal, toallita, cambiador, antipañalitis...).
  2. Se complementa con búsquedas por texto (CONSULTAS_TEXTO) para no perder productos mal ubicados.
  3. Se deduplica por SKU. La clasificación fina (qué entra y qué se excluye) se hace en limpieza.py
     y todo lo excluido queda registrado con su motivo.

Uso (línea de comandos)
-----------------------
  python scraper.py probe      # diagnóstico: modo, categorías encontradas, robots.txt
  python scraper.py diagnose   # diagnóstico detallado de HTTP por sitio (úselo si un comercio falla)
  python scraper.py run        # recolección completa en línea (guarda raw/ y SQLite)
  python scraper.py offline    # reprocesa raw/ sin conexión y reconstruye la base
"""
from __future__ import annotations

import gzip
import json
import logging
import re
import sqlite3
import time
import unicodedata
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import quote, quote_plus, urlencode, urljoin, urlsplit

import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

log = logging.getLogger("scraper")

# Colombia no tiene horario de verano: UTC-5 fijo.
TZ_CO = timezone(timedelta(hours=-5))
PAGINA = 50          # máximo de VTEX por petición
MAX_OFFSET = 2500    # VTEX no pagina más allá de 2500 resultados por consulta


def ahora_iso() -> str:
    """Fecha y hora de Colombia en ISO 8601, p. ej. 2026-10-06T10:35:00-05:00."""
    return datetime.now(TZ_CO).isoformat(timespec="seconds")


def _slug(s: str) -> str:
    """Nombre de archivo seguro: ASCII, sin ':' ni espacios (válido en Windows, Mac y Linux)."""
    return re.sub(r"[^a-z0-9]+", "_", _norm(s)).strip("_")[:60]


def _norm(s: str) -> str:
    s = unicodedata.normalize("NFKD", s or "")
    s = "".join(c for c in s if not unicodedata.combining(c))
    return re.sub(r"\s+", " ", s.lower()).strip()


# --------------------------------------------------------------------------------------
# Configuración de comercios
# --------------------------------------------------------------------------------------
@dataclass
class Comercio:
    clave: str
    nombre: str
    base_url: str
    # Nombres de nodos del árbol de categorías que se consideran equivalentes a la categoría
    patron_categorias: str = (r"pa(n|ñ)al|toallit|pa(n|ñ)itos|cambiador|antipa(n|ñ)alitis|barrera|nalguita|"
                              r"aseo del beb|cuidado del beb|higiene del beb")
    modo: str | None = None  # "legacy" | "intelligent" (autodetectado)


COMERCIOS: dict[str, Comercio] = {
    "exito": Comercio("exito", "Éxito", "https://www.exito.com"),
    "jumbo": Comercio("jumbo", "Jumbo", "https://www.jumbocolombia.com"),
    "olimpica": Comercio("olimpica", "Olímpica", "https://www.olimpica.com"),
    # Alternativas VTEX si alguno bloquea el acceso (cambiar en COMERCIOS_ACTIVOS):
    "carulla": Comercio("carulla", "Carulla", "https://www.carulla.com"),
}
COMERCIOS_ACTIVOS = ["exito", "jumbo", "olimpica"]

CONSULTAS_TEXTO = [
    # Palabras sueltas (las más robustas: algunos sitios responden HTTP 400 a consultas de varias palabras)
    "pañales", "pañal", "toallitas", "pañitos", "cambiador", "cambiadores", "antipañalitis", "nalguita",
    # Frases (si el sitio las rechaza, queda registrado en la ejecución y se continúa)
    "pañales etapa", "pañales recién nacido", "pañales huggies", "pañales pampers", "pañales winny",
    "pañales babysec", "pañales de entrenamiento", "toallitas húmedas bebé", "pañitos húmedos",
    "cambiador desechable", "protector cambiador", "crema antipañalitis", "crema para pañal",
    "crema barrera bebé", "pasta de óxido de zinc bebé",
]

COLUMNAS = [
    "ejecucion_id", "comercio", "sku", "product_id", "ean", "url",
    "nombre_original", "descripcion_original", "ficha_tecnica_original", "marca_original",
    "categoria_sitio", "precio", "precio_lista", "es_promocion", "disponible",
    "unidad_medida_sitio", "multiplicador_unidad", "via", "fecha_hora_consulta",
]


# --------------------------------------------------------------------------------------
# Cliente HTTP
# --------------------------------------------------------------------------------------
class ErrorHTTP(RuntimeError):
    pass


class Cliente:
    """Sesión HTTP con reintentos y pausa entre peticiones (buenas prácticas de recolección)."""

    def __init__(self, pausa: float = 1.0, timeout: int = 30):
        self.pausa, self.timeout = pausa, timeout
        self.s = requests.Session()
        retry = Retry(total=3, backoff_factor=1.5, status_forcelist=(429, 500, 502, 503, 504),
                      allowed_methods=("GET",))
        self.s.mount("https://", HTTPAdapter(max_retries=retry))
        self.s.headers.update({
            "User-Agent": ("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) "
                           "Chrome/124.0 Safari/537.36"),
            "Accept": "application/json, text/plain, */*",
            "Accept-Language": "es-CO,es;q=0.9",
        })

    @staticmethod
    def _detalle(r) -> str:
        return f"HTTP {r.status_code} en {r.url} [server={r.headers.get('server')}] {r.text[:100]!r}"

    def get_json(self, url: str, params: dict | None = None):
        time.sleep(self.pausa)
        origen = "{0.scheme}://{0.netloc}/".format(urlsplit(url))
        r = None
        # Primero espacios como %20; si el sitio responde 400, se reintenta con '+' (algunos WAF lo exigen).
        for codificador in (quote, quote_plus):
            qs = urlencode(params or {}, quote_via=codificador, safe="/:,")
            r = self.s.get(f"{url}?{qs}" if qs else url, timeout=self.timeout, headers={"Referer": origen})
            if r.status_code != 400:
                break
        if r.status_code not in (200, 206):
            raise ErrorHTTP(self._detalle(r))
        try:
            data = r.json()
        except ValueError as e:
            raise ErrorHTTP("Respuesta no JSON: " + self._detalle(r)) from e
        return data, dict(r.headers), r.url

    def get_text(self, url: str) -> str:
        time.sleep(self.pausa)
        r = self.s.get(url, timeout=self.timeout)
        r.raise_for_status()
        return r.text


# --------------------------------------------------------------------------------------
# Copias crudas (raw/)
# --------------------------------------------------------------------------------------
def guardar_raw(raw_dir: Path, comercio: str, nombre: str, envoltura: dict) -> Path:
    """Guarda la respuesta original (+ metadatos de la consulta) comprimida: raw/<comercio>/<fecha>/<nombre>.json.gz"""
    fecha = envoltura["consulta"][:10]
    carpeta = Path(raw_dir) / comercio / fecha
    carpeta.mkdir(parents=True, exist_ok=True)
    ruta = carpeta / f"{nombre}.json.gz"
    with gzip.open(ruta, "wt", encoding="utf-8") as f:
        json.dump(envoltura, f, ensure_ascii=False)
    return ruta


def leer_raw(ruta: Path) -> dict:
    with gzip.open(ruta, "rt", encoding="utf-8") as f:
        return json.load(f)


# --------------------------------------------------------------------------------------
# Parseo de productos VTEX (común a legacy e intelligent search)
# --------------------------------------------------------------------------------------
def _num(x):
    try:
        v = float(x)
    except (TypeError, ValueError):
        return None
    return v if v > 0 else None  # 0 = no disponible -> NULL, nunca se reemplaza por cero


def _ficha(p: dict) -> str:
    """Ficha técnica tal como la publica el sitio (JSON)."""
    out = {}
    for nombre in p.get("allSpecifications") or []:
        out[nombre] = p.get(nombre)
    if p.get("properties"):
        out["properties"] = p["properties"]
    return json.dumps(out, ensure_ascii=False) if out else None


def parsear_producto(p: dict, cfg: Comercio, via: str) -> list[dict]:
    """Un producto VTEX puede traer varios SKU (items); cada SKU es una presentación."""
    filas = []
    categorias = p.get("categories") or []
    cat = (categorias[0] if categorias else "") or ""
    cat = " > ".join(x for x in cat.strip("/").split("/") if x)
    link = p.get("link") or (f"/{p['linkText']}/p" if p.get("linkText") else None)
    url = urljoin(cfg.base_url + "/", link) if link else None
    for it in p.get("items") or [{}]:
        oferta = {}
        for s in it.get("sellers") or []:
            o = s.get("commertialOffer") or {}
            if o.get("AvailableQuantity", 0) and _num(o.get("Price")):
                oferta = o
                break
        if not oferta and it.get("sellers"):
            oferta = it["sellers"][0].get("commertialOffer") or {}
        precio = _num(oferta.get("Price"))
        lista = _num(oferta.get("ListPrice")) or _num(oferta.get("PriceWithoutDiscount"))
        sin_desc = _num(oferta.get("PriceWithoutDiscount"))
        if sin_desc and (not lista or sin_desc > lista):
            lista = sin_desc
        promo = int(bool(precio and lista and precio < lista))
        filas.append({
            "comercio": cfg.clave,
            "sku": str(it.get("itemId") or p.get("productId")),
            "product_id": str(p.get("productId")),
            "ean": it.get("ean") or None,
            "url": url,
            "nombre_original": it.get("nameComplete") or it.get("name") or p.get("productName"),
            "descripcion_original": p.get("description") or None,
            "ficha_tecnica_original": _ficha(p),
            "marca_original": p.get("brand") or None,
            "categoria_sitio": cat or None,
            "precio": precio,
            "precio_lista": lista if promo else (lista or None),
            "es_promocion": promo,
            "disponible": int(bool(oferta.get("AvailableQuantity", 0))),
            "unidad_medida_sitio": it.get("measurementUnit"),
            "multiplicador_unidad": it.get("unitMultiplier"),
            "via": via,
        })
    return filas


def _items_y_total(data, headers: dict | None, modo: str):
    if modo == "legacy":
        total = None
        m = re.search(r"/(\d+)$", (headers or {}).get("resources", ""))
        if m:
            total = int(m.group(1))
        return (data if isinstance(data, list) else []), total
    return data.get("products", []), data.get("recordsFiltered")


# --------------------------------------------------------------------------------------
# Recorrido de un comercio VTEX
# --------------------------------------------------------------------------------------
def _url_y_params(cfg: Comercio, modo: str, consulta: dict, offset: int) -> tuple[str, dict]:
    if modo == "legacy":
        url = f"{cfg.base_url}/api/catalog_system/pub/products/search"
        params = {"_from": offset, "_to": offset + PAGINA - 1}
        params["fq" if consulta["tipo"] == "categoria" else "ft"] = consulta["valor"]
    else:
        url = f"{cfg.base_url}/api/io/_v/api/intelligent-search/product_search"
        params = {"query": consulta["valor"], "page": offset // PAGINA + 1, "count": PAGINA,
                  "locale": "es-CO", "hideUnavailableItems": "false"}
    return url, params


def detectar_modo(cli: Cliente, cfg: Comercio) -> str:
    errores = []
    for modo in ("legacy", "intelligent"):
        try:
            url, params = _url_y_params(cfg, modo, {"tipo": "texto", "valor": "pañales"}, 0)
            data, headers, _ = cli.get_json(url, params)
            items, _ = _items_y_total(data, headers, modo)
            if items:
                return modo
            errores.append(f"{modo}: respuesta sin productos")
        except Exception as e:  # noqa: BLE001
            errores.append(f"{modo}: {type(e).__name__}: {str(e)[:220]}")
            log.warning("%s: modo %s no disponible (%s)", cfg.clave, modo, e)
    raise ErrorHTTP(f"{cfg.clave}: ningún endpoint VTEX respondió productos -> " + " | ".join(errores))


def descubrir_categorias(cli: Cliente, cfg: Comercio) -> list[dict]:
    """Árbol de categorías (solo modo legacy). Devuelve nodos equivalentes a la categoría del taller."""
    data, _, _ = cli.get_json(f"{cfg.base_url}/api/catalog_system/pub/category/tree/4")
    patron = re.compile(cfg.patron_categorias, re.I)
    nodos = []

    def rec(lista, ids, nombres):
        for n in lista:
            i, nom = ids + [str(n["id"])], nombres + [n["name"]]
            if patron.search(_norm(n["name"])):
                nodos.append({"ruta_ids": i, "ruta": " > ".join(nom), "nombre": n["name"]})
                continue  # el nodo ya incluye a sus hijos en VTEX
            rec(n.get("children") or [], i, nom)

    rec(data, [], [])
    return nodos


def _recorrer_consulta(cli, cfg, modo, consulta, etiqueta, ejec, raw_dir, filas_por_sku, meta):
    offset, total, pag = 0, None, 0
    while offset < MAX_OFFSET:
        url, params = _url_y_params(cfg, modo, consulta, offset)
        data, headers, url_final = cli.get_json(url, params)
        ts = ahora_iso()
        items, tot = _items_y_total(data, headers, modo)
        total = tot if tot is not None else total
        guardar_raw(raw_dir, cfg.clave, f"{ejec}__{etiqueta}__p{pag:03d}", {
            "ejecucion_id": ejec, "comercio": cfg.clave, "modo": modo, "consulta": ts,
            "url": url_final, "etiqueta": etiqueta, "total_sitio": total, "tipo_consulta": consulta["tipo"],
            "respuesta": data,
        })
        for p in items:
            for fila in parsear_producto(p, cfg, etiqueta):
                fila["fecha_hora_consulta"] = ts
                meta["items_crudos"] += 1
                if fila["sku"] in filas_por_sku:
                    meta["duplicados_sku"] += 1
                    filas_por_sku[fila["sku"]]["via"] += f"|{etiqueta}"
                else:
                    filas_por_sku[fila["sku"]] = fila
        pag += 1
        offset += PAGINA
        if len(items) < PAGINA or (total is not None and offset >= total):
            break
    return total


def _scrape_vtex(cfg: Comercio, cli: Cliente, ejec: str, raw_dir: Path) -> pd.DataFrame:
    inicio = ahora_iso()
    meta = {"inicio": inicio, "items_crudos": 0, "duplicados_sku": 0, "n_sitio": None,
            "notas": [], "estado": "completa", "consultas": []}
    filas: dict[str, dict] = {}
    try:
        cfg.modo = detectar_modo(cli, cfg)
        consultas = []
        if cfg.modo == "legacy":
            try:
                for nodo in descubrir_categorias(cli, cfg):
                    consultas.append(({"tipo": "categoria", "valor": "C:/" + "/".join(nodo["ruta_ids"]) + "/"},
                                      "cat:" + nodo["ruta"]))
            except Exception as e:  # noqa: BLE001
                meta["notas"].append(f"No se pudo leer el árbol de categorías: {e}")
        consultas += [({"tipo": "texto", "valor": t}, "q:" + t) for t in CONSULTAS_TEXTO]
        suma_sitio = 0
        for consulta, etiqueta in consultas:
            try:
                tot = _recorrer_consulta(cli, cfg, cfg.modo, consulta, _slug(etiqueta),
                                         ejec, raw_dir, filas, meta)
                meta["consultas"].append({"etiqueta": etiqueta, "total_sitio": tot})
                if consulta["tipo"] == "categoria" and tot:
                    suma_sitio += tot
            except Exception as e:  # noqa: BLE001
                meta["estado"] = "parcial"
                meta["notas"].append(f"Falló {etiqueta}: {e}")
                log.error("%s %s: %s", cfg.clave, etiqueta, e)
        meta["n_sitio"] = suma_sitio or None
        if not filas:
            meta["estado"] = "fallida"
    except Exception as e:  # noqa: BLE001
        meta["estado"] = "fallida" if not filas else "parcial"
        meta["notas"].append(str(e))
        log.error("%s: %s", cfg.clave, e)
    meta["fin"] = ahora_iso()
    meta["modo_api"] = cfg.modo
    df = pd.DataFrame(list(filas.values()))
    for c in COLUMNAS:
        if c not in df.columns:
            df[c] = None
    df["ejecucion_id"] = ejec
    df = df[COLUMNAS]
    df.attrs["meta"] = meta
    guardar_raw(raw_dir, cfg.clave, f"{ejec}__MANIFIESTO", {"ejecucion_id": ejec, "comercio": cfg.clave,
                "consulta": meta["inicio"], "manifiesto": meta})
    return df


# Una función por comercio: siempre devuelven las mismas columnas (COLUMNAS).
def scrape_exito(cli: Cliente, ejec: str, raw_dir: Path) -> pd.DataFrame:
    return _scrape_vtex(COMERCIOS["exito"], cli, ejec, raw_dir)


def scrape_jumbo(cli: Cliente, ejec: str, raw_dir: Path) -> pd.DataFrame:
    return _scrape_vtex(COMERCIOS["jumbo"], cli, ejec, raw_dir)


def scrape_olimpica(cli: Cliente, ejec: str, raw_dir: Path) -> pd.DataFrame:
    return _scrape_vtex(COMERCIOS["olimpica"], cli, ejec, raw_dir)


def scrape_carulla(cli: Cliente, ejec: str, raw_dir: Path) -> pd.DataFrame:
    return _scrape_vtex(COMERCIOS["carulla"], cli, ejec, raw_dir)


FUNCIONES = {"exito": scrape_exito, "jumbo": scrape_jumbo, "olimpica": scrape_olimpica, "carulla": scrape_carulla}


# --------------------------------------------------------------------------------------
# SQLite
# --------------------------------------------------------------------------------------
ESQUEMA = """
PRAGMA foreign_keys = ON;
CREATE TABLE IF NOT EXISTS comercios (
    comercio_id INTEGER PRIMARY KEY, clave TEXT UNIQUE NOT NULL, nombre TEXT NOT NULL, url_base TEXT);
CREATE TABLE IF NOT EXISTS ejecuciones (
    ejecucion_id TEXT NOT NULL, comercio_id INTEGER NOT NULL REFERENCES comercios(comercio_id),
    inicio TEXT NOT NULL, fin TEXT, n_registros INTEGER, n_sitio_reportado INTEGER,
    estado TEXT NOT NULL CHECK (estado IN ('completa','parcial','fallida')),
    modo TEXT CHECK (modo IN ('en_linea','sin_conexion')), modo_api TEXT, notas TEXT,
    PRIMARY KEY (ejecucion_id, comercio_id));
CREATE TABLE IF NOT EXISTS productos (          -- identificador común entre comercios (solo comparables)
    producto_id INTEGER PRIMARY KEY, clave TEXT UNIQUE NOT NULL, grupo TEXT NOT NULL,
    nombre_std TEXT, marca_std TEXT, subcategoria_std TEXT, variante_std TEXT, etapa_std INTEGER,
    contenido_total REAL, unidad_base TEXT);
CREATE TABLE IF NOT EXISTS anuncios (           -- producto tal como aparece en un comercio
    anuncio_id INTEGER PRIMARY KEY, comercio_id INTEGER NOT NULL REFERENCES comercios(comercio_id),
    sku TEXT NOT NULL, product_id TEXT, ean TEXT, url TEXT NOT NULL,
    nombre_original TEXT NOT NULL, descripcion_original TEXT, ficha_tecnica_original TEXT,
    marca_original TEXT, categoria_sitio TEXT, via TEXT,
    -- campos estandarizados (los llena limpieza.py; el texto original no se modifica)
    marca_std TEXT, subcategoria_std TEXT, variante_std TEXT, etapa_std INTEGER,
    contenido_total REAL, unidad_base TEXT, unidades_por_paquete REAL,
    incluido INTEGER, motivo_descarte TEXT, inconsistencia_contenido INTEGER DEFAULT 0,
    clave TEXT, producto_id INTEGER REFERENCES productos(producto_id), comparable INTEGER DEFAULT 0,
    UNIQUE (comercio_id, sku));
CREATE TABLE IF NOT EXISTS observaciones (
    obs_id INTEGER PRIMARY KEY, ejecucion_id TEXT NOT NULL, comercio_id INTEGER NOT NULL,
    anuncio_id INTEGER NOT NULL REFERENCES anuncios(anuncio_id),
    fecha_hora_consulta TEXT NOT NULL, fecha_consulta TEXT NOT NULL,
    precio REAL CHECK (precio IS NULL OR precio > 0), precio_lista REAL,
    es_promocion INTEGER CHECK (es_promocion IN (0,1)), disponible INTEGER,
    FOREIGN KEY (ejecucion_id, comercio_id) REFERENCES ejecuciones(ejecucion_id, comercio_id),
    UNIQUE (anuncio_id, fecha_hora_consulta));
CREATE INDEX IF NOT EXISTS ix_obs_ejec ON observaciones(ejecucion_id, comercio_id);
DROP VIEW IF EXISTS v_precios_comparables;
CREATE VIEW v_precios_comparables AS   -- 1 fila por ejecución x comercio x producto (solo ofertas con precio y disponibles)
SELECT o.ejecucion_id, c.nombre AS comercio, p.producto_id, p.nombre_std, p.grupo, p.contenido_total,
       p.unidad_base, MIN(o.precio) AS precio, MIN(COALESCE(o.precio_lista, o.precio)) AS precio_regular,
       COUNT(*) AS n_observaciones, MAX(o.es_promocion) AS alguna_promocion
FROM observaciones o
JOIN anuncios a ON a.anuncio_id = o.anuncio_id AND a.comparable = 1
JOIN productos p ON p.producto_id = a.producto_id
JOIN comercios c ON c.comercio_id = o.comercio_id
WHERE o.precio IS NOT NULL AND o.disponible = 1
GROUP BY o.ejecucion_id, c.comercio_id, p.producto_id;
"""


def conectar(db_path: str | Path) -> sqlite3.Connection:
    con = sqlite3.connect(db_path)
    con.executescript(ESQUEMA)
    return con


def nueva_ejecucion_id() -> str:
    return "run_" + datetime.now(TZ_CO).strftime("%Y%m%dT%H%M%S")


def _comercio_id(con, clave: str) -> int:
    cfg = COMERCIOS[clave]
    con.execute("INSERT OR IGNORE INTO comercios(clave, nombre, url_base) VALUES (?,?,?)",
                (clave, cfg.nombre, cfg.base_url))
    return con.execute("SELECT comercio_id FROM comercios WHERE clave=?", (clave,)).fetchone()[0]


def insertar_resultado(con: sqlite3.Connection, df: pd.DataFrame, modo: str = "en_linea") -> dict:
    """Inserta anuncios + observaciones sin sobrescribir observaciones anteriores."""
    meta = df.attrs["meta"]
    clave = df["comercio"].iloc[0] if len(df) else meta.get("comercio")
    clave = clave or meta["clave"]
    cid = _comercio_id(con, clave)
    ejec = meta["ejecucion_id"]
    n_sitio = meta.get("n_sitio")
    con.execute("""INSERT OR REPLACE INTO ejecuciones(ejecucion_id, comercio_id, inicio, fin, n_registros, n_sitio_reportado,
                   estado, modo, modo_api, notas) VALUES (?,?,?,?,?,?,?,?,?,?)""",
                (ejec, cid, meta["inicio"], meta.get("fin"), len(df), n_sitio, meta["estado"], modo,
                 meta.get("modo_api"), "; ".join(meta.get("notas", [])) or None))
    nuevas = 0
    for r in df.to_dict("records"):
        con.execute("""INSERT INTO anuncios(comercio_id, sku, product_id, ean, url, nombre_original, descripcion_original,
                       ficha_tecnica_original, marca_original, categoria_sitio, via) VALUES (?,?,?,?,?,?,?,?,?,?,?)
                       ON CONFLICT(comercio_id, sku) DO UPDATE SET url=excluded.url, via=excluded.via""",
                    (cid, r["sku"], r["product_id"], r["ean"], r["url"], r["nombre_original"], r["descripcion_original"],
                     r["ficha_tecnica_original"], r["marca_original"], r["categoria_sitio"], r["via"]))
        aid = con.execute("SELECT anuncio_id FROM anuncios WHERE comercio_id=? AND sku=?", (cid, r["sku"])).fetchone()[0]
        cur = con.execute("""INSERT OR IGNORE INTO observaciones(ejecucion_id, comercio_id, anuncio_id, fecha_hora_consulta,
                             fecha_consulta, precio, precio_lista, es_promocion, disponible) VALUES (?,?,?,?,?,?,?,?,?)""",
                          (ejec, cid, aid, r["fecha_hora_consulta"], r["fecha_hora_consulta"][:10], r["precio"],
                           r["precio_lista"], r["es_promocion"], r["disponible"]))
        nuevas += cur.rowcount
    con.commit()
    return {"comercio": clave, "ejecucion_id": ejec, "registros": len(df), "obs_nuevas": nuevas, "estado": meta["estado"]}


def registrar_fallo(con, clave: str, ejec: str, meta: dict, modo="en_linea"):
    cid = _comercio_id(con, clave)
    con.execute("""INSERT OR REPLACE INTO ejecuciones(ejecucion_id, comercio_id, inicio, fin, n_registros, estado, modo, notas)
                   VALUES (?,?,?,?,0,'fallida',?,?)""", (ejec, cid, meta.get("inicio", ahora_iso()), ahora_iso(), modo,
                                                         "; ".join(meta.get("notas", []))))
    con.commit()


# --------------------------------------------------------------------------------------
# Funciones generales
# --------------------------------------------------------------------------------------
def ejecutar_recoleccion(db_path="comparador_precios.sqlite", raw_dir="raw", comercios=None,
                         cliente: Cliente | None = None, ejecucion_id: str | None = None) -> pd.DataFrame:
    """Recolección completa en línea: una función por comercio, raw/ y SQLite (sin sobrescribir)."""
    comercios = comercios or COMERCIOS_ACTIVOS
    cliente = cliente or Cliente()
    ejec = ejecucion_id or nueva_ejecucion_id()
    raw_dir = Path(raw_dir)
    con = conectar(db_path)
    resumen = []
    for clave in comercios:
        log.info("== %s (%s)", clave, ejec)
        df = FUNCIONES[clave](cliente, ejec, raw_dir)
        df.attrs["meta"].update({"ejecucion_id": ejec, "clave": clave})
        if df.empty:
            registrar_fallo(con, clave, ejec, df.attrs["meta"])
            resumen.append({"comercio": clave, "ejecucion_id": ejec, "registros": 0, "obs_nuevas": 0, "estado": "fallida"})
        else:
            resumen.append(insertar_resultado(con, df, "en_linea"))
    con.close()
    return pd.DataFrame(resumen)


def reprocesar_raw(db_path="comparador_precios.sqlite", raw_dir="raw", fecha: str | None = None,
                   comercios=None) -> pd.DataFrame:
    """Modo sin conexión: reconstruye la base solo desde raw/ (no consulta ningún sitio)."""
    raw_dir = Path(raw_dir)
    con = conectar(db_path)
    grupos: dict[tuple, list] = {}
    for ruta in sorted(raw_dir.glob("*/*/*.json.gz")):
        env = leer_raw(ruta)
        if (comercios and env["comercio"] not in comercios) or (fecha and not env["consulta"].startswith(fecha)):
            continue
        grupos.setdefault((env["ejecucion_id"], env["comercio"]), []).append(env)
    resumen = []
    for (ejec, clave), envs in grupos.items():
        cfg = COMERCIOS[clave]
        manifiesto = next((e["manifiesto"] for e in envs if "manifiesto" in e), {"inicio": envs[0]["consulta"], "notas": []})
        filas: dict[str, dict] = {}
        for e in envs:
            if "respuesta" not in e:
                continue
            items, _ = _items_y_total(e["respuesta"], None, e["modo"])
            for p in items:
                for fila in parsear_producto(p, cfg, e["etiqueta"]):
                    fila["fecha_hora_consulta"], fila["ejecucion_id"] = e["consulta"], ejec
                    if fila["sku"] in filas:
                        filas[fila["sku"]]["via"] += "|" + e["etiqueta"]
                    else:
                        filas[fila["sku"]] = fila
        df = pd.DataFrame(list(filas.values()))
        for c in COLUMNAS:
            if c not in df.columns:
                df[c] = None
        df = df[COLUMNAS]
        manifiesto.update({"ejecucion_id": ejec, "clave": clave})
        df.attrs["meta"] = manifiesto
        resumen.append(insertar_resultado(con, df, "sin_conexion") if len(df) else
                       {"comercio": clave, "ejecucion_id": ejec, "registros": 0, "obs_nuevas": 0, "estado": "fallida"})
    con.close()
    return pd.DataFrame(resumen)


def probar_sitios(comercios=None) -> None:
    """Diagnóstico rápido: robots.txt, modo de API, categorías detectadas y total del sitio."""
    cli = Cliente(pausa=0.5)
    for clave in comercios or COMERCIOS_ACTIVOS:
        cfg = COMERCIOS[clave]
        print(f"\n=== {cfg.nombre} ({cfg.base_url})")
        try:
            robots = cli.get_text(cfg.base_url + "/robots.txt")
            print("robots.txt (primeras líneas):", " | ".join(robots.splitlines()[:6]))
        except Exception as e:  # noqa: BLE001
            print("robots.txt no disponible:", e)
        try:
            cfg.modo = detectar_modo(cli, cfg)
            print("modo API:", cfg.modo)
            if cfg.modo == "legacy":
                for n in descubrir_categorias(cli, cfg):
                    url, params = _url_y_params(cfg, "legacy", {"tipo": "categoria", "valor": "C:/" + "/".join(n["ruta_ids"]) + "/"}, 0)
                    _, h, _ = cli.get_json(url, params)
                    print("  categoría:", n["ruta"], "-> total sitio:", h.get("resources"))
        except Exception as e:  # noqa: BLE001
            print("ERROR:", e)


def diagnosticar(objetivos=None) -> None:
    """
    Diagnóstico detallado: para cada comercio (clave de COMERCIOS o URL base) prueba la página principal, robots.txt,
    los dos endpoints VTEX y el árbol de categorías, e imprime código HTTP, servidor y los primeros bytes de la
    respuesta. Sirve para entender POR QUÉ un sitio no responde (bloqueo 403, timeout, otra plataforma, etc.).
    Uso:  python scraper.py diagnose            (comercios activos)
          python scraper.py diagnose https://www.locatelcolombia.com https://www.carulla.com
    """
    s = requests.Session()
    s.headers.update(Cliente().s.headers)
    for obj in objetivos or COMERCIOS_ACTIVOS:
        base = COMERCIOS[obj].base_url if obj in COMERCIOS else obj.rstrip("/")
        print(f"\n=== {obj}  ({base})")
        pruebas = [
            ("portada", "/"), ("robots.txt", "/robots.txt"),
            ("vtex legacy", "/api/catalog_system/pub/products/search?ft=pa%C3%B1ales&_from=0&_to=1"),
            ("vtex intelligent", "/api/io/_v/api/intelligent-search/product_search?query=pa%C3%B1ales&page=1&count=2&locale=es-CO"),
            ("arbol categorias", "/api/catalog_system/pub/category/tree/1"),
        ]
        for nombre, ruta in pruebas:
            t0 = time.time()
            try:
                r = s.get(base + ruta, timeout=20, headers={"Referer": base + "/"})
                print(f"  {nombre:17} HTTP {r.status_code}  {r.headers.get('content-type', '')[:28]:28} "
                      f"server={r.headers.get('server')}  {len(r.content)} bytes  {time.time() - t0:.1f}s  :: {r.text[:90]!r}")
            except Exception as e:  # noqa: BLE001
                print(f"  {nombre:17} ERROR {type(e).__name__} tras {time.time() - t0:.1f}s: {str(e)[:140]}")
            time.sleep(0.7)


if __name__ == "__main__":
    import sys
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    orden = sys.argv[1] if len(sys.argv) > 1 else "run"
    if orden == "probe":
        probar_sitios()
    elif orden == "diagnose":
        diagnosticar(sys.argv[2:] or None)
    elif orden == "offline":
        print(reprocesar_raw())
    else:
        print(ejecutar_recoleccion())
