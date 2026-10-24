"""
scraper.py — Taller 1 Minería de Datos (UNAL): comparador de precios de cuidado capilar.

Comercios y estrategia de extracción (los tres usan la plataforma VTEX):

* La Rebaja (www.larebajavirtual.com) y Locatel (www.locatelcolombia.com): API pública de catálogo VTEX
  ``/api/catalog_system/pub/products/search?fq=C:<ruta de categoría>&_from=<i>&_to=<j>``, de 50 en 50.
  El total de productos de cada rama lo da la cabecera ``resources: 0-49/<total>``.
  Los IDs de categoría (``fq=C:/5/29/``…) se obtuvieron de ``/api/catalog_system/pub/category/tree/3``.
  robots.txt de ambos sitios (revisado 2026-10-09) no prohíbe ``/api/``.
* Droguerías Cafam (www.drogueriascafam.com.co): páginas HTML de categoría ``/<ruta>?page=N`` (VTEX IO). Cada
  página trae los productos en un JSON embebido (caché de Apollo GraphQL) ``__STATE__ = {…}`` dentro de un
  ``<script>`` (otras tiendas VTEX IO usan ``<template data-varname="__STATE__">``; se aceptan ambos formatos).
  Su robots.txt (revisado 2026-10-09) prohíbe ``/api/`` y ``/_v/segment``, pero permite ``?page=N``; por eso se
  usa el HTML y no la API. Límite de la plataforma: VTEX IO sirve como máximo 50 páginas por listado.

Términos de uso revisados el 2026-10-09 (detalle en docs/fuentes_scraping.md): ninguno de los tres prohíbe
expresamente robots o extracción automatizada; La Rebaja (Copservir) prohíbe de forma genérica copiar o reproducir
sus "Contenidos" sin permiso escrito. Se extraen solo datos de producto con fines académicos, sin redistribuirlos.

Cada respuesta se guarda sin modificar en ``raw/<comercio>/<YYYY-MM-DD>/<id_ejecucion>/`` (gzip) junto con un
``manifiesto.json`` (URL, fecha y hora de cada petición, totales del sitio). El procesamiento SIEMPRE lee de esos
archivos, de modo que ``run_all(offline=True)`` reproduce exactamente la misma extracción sin hacer peticiones.

Los productos comparables (``comparable = 1`` e ``id_producto_comun``) los marca ``comparables.py`` sobre la base ya
cargada; ``run_all`` crea esos campos y los conserva entre ejecuciones.

Uso:
    import scraper
    scraper.run_all()               # recolección en línea (ejecución nueva)
    scraper.run_all(offline=True)   # reprocesa la última ejecución guardada en raw/

    python scraper.py [--offline] [--id ID_EJECUCION]
"""
from __future__ import annotations

import argparse
import gzip
import json
import math
import re
import sqlite3
import time
import unicodedata
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

# --------------------------------------------------------------------------------------------------------------
# Configuración
# --------------------------------------------------------------------------------------------------------------
BASE_DIR = Path(__file__).resolve().parent
RAW_DIR = BASE_DIR / "raw"
DB_PATH = BASE_DIR / "comparador_precios.sqlite"

TZ_CO = timezone(timedelta(hours=-5))  # hora de Colombia (sin horario de verano)
USER_AGENT = ("TallerMineriaDatos-UNAL/1.0 (curso Mineria de Datos, Universidad Nacional de Colombia; "
              "uso academico; +https://github.com/Yesenia-AriasC/dm_2016325_2026_2)")
PAUSA_S = 1.0      # pausa entre peticiones
TIMEOUT_S = 30     # timeout por petición
PASO_API = 50      # tamaño de página de la API de catálogo VTEX (máximo permitido: 50)
LIMITE_API = 2500  # VTEX no entrega más allá de _from=2500
LIMITE_PAGINAS_IO = 50  # VTEX IO no entrega más allá de ?page=50

# Ramas de cada comercio consideradas equivalentes a la categoría registrada (ver docs/delimitacion_categoria.md)
COMERCIOS = {
    "larebaja": {
        "nombre": "La Rebaja", "dominio": "www.larebajavirtual.com",
        "metodo": "API de catálogo VTEX /api/catalog_system/pub/products/search (fq=C:, _from/_to)",
        "ramas": {
            "/5/29/": "Cuidado personal > Cuidado capilar",
            "/8/585/": "Dermocosmética > Cuidado capilar",
            "/1316/": "Crema de peinar",
        },
    },
    "locatel": {
        "nombre": "Locatel", "dominio": "www.locatelcolombia.com",
        "metodo": "API de catálogo VTEX /api/catalog_system/pub/products/search (fq=C:, _from/_to)",
        "ramas": {
            "/1/9/": "Cuidado personal > Cuidado cabello",
            "/2/15/": "Dermocosmética > Capilar",
            "/269/438/455/": "Marketplace > Belleza > Cuidado capilar",
        },
    },
    "cafam": {
        "nombre": "Droguerías Cafam", "dominio": "www.drogueriascafam.com.co",
        "metodo": "HTML de categoría VTEX IO (?page=N), JSON embebido __STATE__",
        # Cafam no tiene rama de coloración/tintes (sitemap de categorías revisado 2026-10-09)
        "ramas": {
            "cuidado-personal/cuidado-capilar": "Cuidado personal > Cuidado capilar",
            "dermocosmetica/cuidado-capilar": "Dermocosmética > Cuidado capilar",
        },
    },
}

# Columnas que devuelven TODAS las funciones scrape_*(), en este orden
COLUMNAS = [
    "comercio", "sku_comercio", "product_id", "ean", "nombre_original", "texto_original_anuncio",
    "marca_original", "subcategoria_original", "rama_origen", "presentacion_ficha", "tiene_descripcion",
    "precio", "precio_antes_descuento", "es_promocion", "promocion_texto", "disponible", "vendedor",
    "url", "fecha_hora_consulta", "id_ejecucion",
]

# Campos de la ficha técnica que son texto legal repetido en todos los productos (no describen el producto)
FICHA_OMITIDA = {"Devoluciones", "Garantía", "Términos y condiciones", "Vendedor", "Ciudades Exentas",
                 "allSpecifications"}


def ahora_iso() -> str:
    """Fecha y hora actual de Colombia en ISO 8601, p. ej. 2026-10-09T10:35:00-05:00."""
    return datetime.now(TZ_CO).isoformat(timespec="seconds")


def nuevo_id_ejecucion() -> str:
    return datetime.now(TZ_CO).strftime("%Y%m%dT%H%M%S")


# --------------------------------------------------------------------------------------------------------------
# Peticiones HTTP (solo en modo en línea)
# --------------------------------------------------------------------------------------------------------------
_SESION: requests.Session | None = None


def _sesion() -> requests.Session:
    """Sesión con User-Agent identificable y reintentos con backoff exponencial (429 y 5xx)."""
    global _SESION
    if _SESION is None:
        s = requests.Session()
        s.headers.update({"User-Agent": USER_AGENT, "Accept-Language": "es-CO,es;q=0.9"})
        reintentos = Retry(total=4, backoff_factor=2, status_forcelist=[429, 500, 502, 503, 504],
                           allowed_methods=["GET"], respect_retry_after_header=True)
        s.mount("https://", HTTPAdapter(max_retries=reintentos))
        _SESION = s
    return _SESION


def _get(url: str, params: dict | None = None) -> requests.Response:
    r = _sesion().get(url, params=params, timeout=TIMEOUT_S)
    r.raise_for_status()
    time.sleep(PAUSA_S)
    return r


# --------------------------------------------------------------------------------------------------------------
# Copias crudas
# --------------------------------------------------------------------------------------------------------------
def _carpeta_ejecucion(raw_dir: Path, comercio: str, id_ejecucion: str) -> Path:
    fecha = datetime.strptime(id_ejecucion[:8], "%Y%m%d").strftime("%Y-%m-%d")
    return raw_dir / comercio / fecha / id_ejecucion


def _guardar_crudo(carpeta: Path, nombre: str, texto: str) -> None:
    with gzip.open(carpeta / nombre, "wt", encoding="utf-8") as f:
        f.write(texto)


def _leer_crudo(carpeta: Path, nombre: str) -> str:
    with gzip.open(carpeta / nombre, "rt", encoding="utf-8") as f:
        return f.read()


def _slug(texto: str) -> str:
    return re.sub(r"[^a-z0-9]+", "_", texto.lower()).strip("_")


def ultima_ejecucion_guardada(raw_dir: Path = RAW_DIR) -> str | None:
    """id de la ejecución más reciente que tiene manifiesto en raw/ (los id son ordenables por fecha)."""
    ids = sorted(p.parent.name for p in raw_dir.glob("*/*/*/manifiesto.json"))
    return ids[-1] if ids else None


def _cargar_manifiesto(raw_dir: Path, comercio: str, id_ejecucion: str) -> tuple[Path, dict]:
    carpeta = _carpeta_ejecucion(raw_dir, comercio, id_ejecucion)
    archivo = carpeta / "manifiesto.json"
    if not archivo.exists():
        raise FileNotFoundError(f"No hay copias crudas de {comercio} para la ejecución {id_ejecucion}: {archivo}")
    return carpeta, json.loads(archivo.read_text(encoding="utf-8"))


def _nuevo_manifiesto(comercio: str, id_ejecucion: str) -> dict:
    return {"comercio": comercio, "id_ejecucion": id_ejecucion, "inicio": ahora_iso(), "fin": None,
            "user_agent": USER_AGENT, "estado": None, "error": None, "ramas": {}, "archivos": []}


def _cerrar_manifiesto(carpeta: Path, m: dict) -> None:
    """Estado: completa si cada rama entregó todos los productos que reporta el sitio; parcial si falta alguno
    o hubo un error después de obtener datos; fallida si no se obtuvo nada."""
    m["fin"] = ahora_iso()
    obtenidos = sum(r["productos_obtenidos"] for r in m["ramas"].values())
    completo = m["ramas"] and all(r["productos_obtenidos"] >= (r["total_sitio"] or 0) for r in m["ramas"].values())
    if obtenidos == 0:
        m["estado"] = "fallida"
    elif m["error"] or not completo or len(m["ramas"]) < len(COMERCIOS[m["comercio"]]["ramas"]):
        m["estado"] = "parcial"
    else:
        m["estado"] = "completa"
    (carpeta / "manifiesto.json").write_text(json.dumps(m, ensure_ascii=False, indent=1), encoding="utf-8")


# --------------------------------------------------------------------------------------------------------------
# Descarga: API de catálogo VTEX (La Rebaja, Locatel)
# --------------------------------------------------------------------------------------------------------------
def _descargar_vtex_api(comercio: str, id_ejecucion: str, raw_dir: Path) -> tuple[Path, dict]:
    cfg = COMERCIOS[comercio]
    carpeta = _carpeta_ejecucion(raw_dir, comercio, id_ejecucion)
    carpeta.mkdir(parents=True, exist_ok=True)
    m = _nuevo_manifiesto(comercio, id_ejecucion)
    url = f"https://{cfg['dominio']}/api/catalog_system/pub/products/search"
    try:
        for rama in cfg["ramas"]:
            info = m["ramas"][rama] = {"total_sitio": None, "productos_obtenidos": 0, "peticiones": 0, "nota": None}
            desde = 0
            while info["total_sitio"] is None or desde < info["total_sitio"]:
                if desde >= LIMITE_API:
                    info["nota"] = f"La API no entrega más allá de _from={LIMITE_API}"
                    break
                r = _get(url, {"fq": f"C:{rama}", "_from": desde, "_to": desde + PASO_API - 1})
                fecha = ahora_iso()
                info["total_sitio"] = int(r.headers["resources"].split("/")[-1])
                nombre = f"C{_slug(rama)}__from{desde:05d}.json.gz"
                _guardar_crudo(carpeta, nombre, r.text)
                n = len(r.json())
                m["archivos"].append({"archivo": nombre, "rama": rama, "url": r.url, "status": r.status_code,
                                      "fecha_hora_consulta": fecha, "resources": r.headers["resources"],
                                      "productos": n})
                info["productos_obtenidos"] += n
                info["peticiones"] += 1
                if n == 0:
                    break
                desde += PASO_API
    except Exception as e:  # se registra el error; lo ya descargado se conserva
        m["error"] = f"{type(e).__name__}: {e}"
    _cerrar_manifiesto(carpeta, m)
    return carpeta, m


# --------------------------------------------------------------------------------------------------------------
# Descarga: HTML VTEX IO (Cafam)
# --------------------------------------------------------------------------------------------------------------
def _leer_state(html: str) -> dict:
    """Extrae el JSON __STATE__ (caché de Apollo) que VTEX IO incrusta en el HTML de la página.
    Formato 1: <template data-varname="__STATE__"><script>{...}</script>
    Formato 2 (Cafam): <script> ... __STATE__ = {...}"""
    m = re.search(r'<template[^>]*data-varname="__STATE__"[^>]*>\s*<script>(.*?)</script>', html, re.S)
    if m:
        return json.loads(m.group(1))
    m = re.search(r"\b__STATE__\s*=\s*(?=\{)", html)
    if m:
        return json.JSONDecoder().raw_decode(html, m.end())[0]
    raise ValueError("La página no contiene __STATE__ (¿cambió la estructura del sitio?)")


def _busqueda_state(st: dict) -> dict:
    """Objeto de la consulta productSearch: contiene recordsFiltered y la lista de productos de la página."""
    for v in st.values():
        if isinstance(v, dict) and "recordsFiltered" in v and "products" in v:
            return v
    raise ValueError("No se encontró productSearch en __STATE__")


def _descargar_vtex_io(comercio: str, id_ejecucion: str, raw_dir: Path) -> tuple[Path, dict]:
    cfg = COMERCIOS[comercio]
    carpeta = _carpeta_ejecucion(raw_dir, comercio, id_ejecucion)
    carpeta.mkdir(parents=True, exist_ok=True)
    m = _nuevo_manifiesto(comercio, id_ejecucion)
    try:
        for rama in cfg["ramas"]:
            info = m["ramas"][rama] = {"total_sitio": None, "productos_obtenidos": 0, "peticiones": 0, "nota": None}
            pagina, paginas = 1, 1
            while pagina <= paginas:
                r = _get(f"https://{cfg['dominio']}/{rama}?page={pagina}")
                fecha = ahora_iso()
                nombre = f"{_slug(rama)}__p{pagina:03d}.html.gz"
                _guardar_crudo(carpeta, nombre, r.text)  # primero la copia cruda, luego el análisis
                busq = _busqueda_state(_leer_state(r.text))
                n = len(busq["products"])
                m["archivos"].append({"archivo": nombre, "rama": rama, "url": r.url, "status": r.status_code,
                                      "fecha_hora_consulta": fecha, "recordsFiltered": busq["recordsFiltered"],
                                      "productos": n})
                info["productos_obtenidos"] += n
                info["peticiones"] += 1
                if pagina == 1:
                    info["total_sitio"] = busq["recordsFiltered"]
                    paginas_sitio = math.ceil(info["total_sitio"] / max(n, 1))
                    paginas = min(paginas_sitio, LIMITE_PAGINAS_IO)
                    if paginas_sitio > LIMITE_PAGINAS_IO:
                        info["nota"] = (f"El sitio reporta {info['total_sitio']} productos en {paginas_sitio} páginas "
                                        f"de {n}, pero VTEX IO solo sirve hasta ?page={LIMITE_PAGINAS_IO}.")
                if n == 0:
                    break
                pagina += 1
    except Exception as e:
        m["error"] = f"{type(e).__name__}: {e}"
    _cerrar_manifiesto(carpeta, m)
    return carpeta, m


# --------------------------------------------------------------------------------------------------------------
# Procesamiento común de productos VTEX (desde las copias crudas)
# --------------------------------------------------------------------------------------------------------------
def _nombres_promocion(oferta: dict) -> str | None:
    """Nombres de promociones/teasers que publica la oferta (p. ej. '2x1'), si existen."""
    nombres = []
    for clave in ("Teasers", "PromotionTeasers", "DiscountHighLight", "teasers", "discountHighlights"):
        for t in oferta.get(clave) or []:
            if isinstance(t, dict):
                nombres += [str(v) for k, v in t.items() if "name" in k.lower() and v]
    return "; ".join(dict.fromkeys(nombres)) or None


def _oferta(vendedores: list[dict]) -> dict:
    """Elige la oferta del vendedor con existencias (prefiere el predeterminado).
    Precio 0 o ausente -> None (nunca 0). precio_antes_descuento solo si ListPrice > Price."""
    con_stock = [v for v in vendedores if (v["oferta"].get("AvailableQuantity") or 0) > 0]
    candidatos = con_stock or vendedores
    elegido = next((v for v in candidatos if v.get("sellerDefault")), candidatos[0] if candidatos else None)
    if elegido is None:
        return {"precio": None, "precio_antes_descuento": None, "es_promocion": 0, "promocion_texto": None,
                "disponible": 0, "vendedor": None}
    o = elegido["oferta"]
    precio = o.get("Price") or None
    lista = o.get("ListPrice") or None
    antes = lista if (precio and lista and lista > precio) else None
    return {"precio": float(precio) if precio else None,
            "precio_antes_descuento": float(antes) if antes else None,
            "es_promocion": int(antes is not None),
            "promocion_texto": _nombres_promocion(o),
            "disponible": int(bool(con_stock)),
            "vendedor": elegido.get("sellerName")}


def _texto_ficha(ficha: dict) -> str:
    return "\n".join(f"{k}: {', '.join(map(str, v)) if isinstance(v, list) else v}"
                     for k, v in ficha.items() if k not in FICHA_OMITIDA and v not in (None, [], ""))


def _presentacion_ficha(ficha: dict) -> str | None:
    """Contenido declarado en la ficha técnica, tal como viene (La Rebaja, Locatel y Cafam lo publican)."""
    pares = [("Cantidadunidadesmedida", "Unidadmedida"), ("PUM - Medida", "PUM - Unidad de Medida"),
             ("Cantidad por unidad de medida", "Unidad de medida")]
    for cant, uni in pares:
        if ficha.get(cant) and ficha.get(uni):
            return f"{ficha[cant][0]} {ficha[uni][0]}"
    return None


def _filas_producto(comercio: str, p: dict, items: list[dict], ficha: dict, url_producto: str,
                    fecha: str, rama: str, id_ejecucion: str) -> list[dict]:
    filas = []
    categorias = p.get("categories") or [""]
    texto_ficha = _texto_ficha(ficha)
    # 1 si el comercio publica una descripción con texto (sin contar etiquetas HTML ni espacios)
    tiene_desc = int(bool(re.sub(r"<[^>]*>|&nbsp;|\s", "", p.get("description") or "")))
    for it in items:
        nombre = it.get("nameComplete") if len(items) > 1 else p.get("productName")
        nombre = nombre or it.get("name") or p.get("productName")
        texto = "\n".join(x for x in [p.get("productName"), it.get("nameComplete"), texto_ficha,
                                      p.get("description")] if x)
        url = url_producto + (f"?skuId={it['itemId']}" if len(items) > 1 else "")
        filas.append({
            "comercio": comercio, "sku_comercio": str(it["itemId"]), "product_id": str(p.get("productId")),
            "ean": (it.get("ean") or None), "nombre_original": nombre, "texto_original_anuncio": texto,
            "marca_original": p.get("brand"), "subcategoria_original": categorias[0], "rama_origen": rama,
            "presentacion_ficha": _presentacion_ficha(ficha), "tiene_descripcion": tiene_desc,
            **_oferta(it.get("vendedores", [])),
            "url": url, "fecha_hora_consulta": fecha, "id_ejecucion": id_ejecucion,
        })
    return filas


def _procesar_vtex_api(comercio: str, carpeta: Path, m: dict) -> list[dict]:
    filas = []
    for a in m["archivos"]:
        for p in json.loads(_leer_crudo(carpeta, a["archivo"])):
            ficha = {k: p.get(k) for k in p.get("allSpecifications") or []}
            items = []
            for it in p.get("items", []):
                vend = [{"sellerName": s.get("sellerName"), "sellerDefault": s.get("sellerDefault"),
                         "oferta": s.get("commertialOffer") or {}} for s in it.get("sellers", [])]
                items.append({**it, "vendedores": vend})
            url = p.get("link") or f"https://{COMERCIOS[comercio]['dominio']}/{p['linkText']}/p"
            filas += _filas_producto(comercio, p, items, ficha, url, a["fecha_hora_consulta"], a["rama"],
                                     m["id_ejecucion"])
    return filas


def _deref(st: dict, v, prof: int = 0):
    """Resuelve las referencias {'type': 'id', 'id': ...} de la caché de Apollo en un objeto anidado."""
    if prof > 15:
        return None
    if isinstance(v, dict):
        if v.get("type") == "id" and "id" in v:
            return _deref(st, st.get(v["id"]), prof + 1)
        if v.get("type") == "json" and "json" in v:
            return v["json"]
        return {k: _deref(st, x, prof + 1) for k, x in v.items()}
    if isinstance(v, list):
        return [_deref(st, x, prof + 1) for x in v]
    return v


def _procesar_vtex_io(comercio: str, carpeta: Path, m: dict) -> list[dict]:
    filas = []
    for a in m["archivos"]:
        st = _leer_state(_leer_crudo(carpeta, a["archivo"]))
        for ref in _busqueda_state(st)["products"]:
            p = _deref(st, ref)
            ficha = {}
            for g in p.get("specificationGroups") or []:
                if g.get("name") == "allSpecifications":
                    continue
                for s in g.get("specifications") or []:
                    ficha[s.get("name")] = s.get("values")
            items = []
            for it in p.get('items({"filter":"ALL"})') or p.get("items") or []:
                vend = [{"sellerName": s.get("sellerName"), "sellerDefault": s.get("sellerDefault"),
                         "oferta": s.get("commertialOffer") or {}} for s in it.get("sellers") or []]
                items.append({**it, "vendedores": vend})
            url = f"https://{COMERCIOS[comercio]['dominio']}{p['link']}"
            filas += _filas_producto(comercio, p, items, ficha, url, a["fecha_hora_consulta"], a["rama"],
                                     m["id_ejecucion"])
    return filas


_DESCARGA = {"larebaja": _descargar_vtex_api, "locatel": _descargar_vtex_api, "cafam": _descargar_vtex_io}
_PROCESO = {"larebaja": _procesar_vtex_api, "locatel": _procesar_vtex_api, "cafam": _procesar_vtex_io}


# --------------------------------------------------------------------------------------------------------------
# Funciones por comercio (misma firma, mismas columnas)
# --------------------------------------------------------------------------------------------------------------
def _scrape(comercio: str, id_ejecucion: str | None, offline: bool, raw_dir: Path) -> pd.DataFrame:
    if offline:
        id_ejecucion = id_ejecucion or ultima_ejecucion_guardada(raw_dir)
        carpeta, m = _cargar_manifiesto(raw_dir, comercio, id_ejecucion)
    else:
        id_ejecucion = id_ejecucion or nuevo_id_ejecucion()
        carpeta, m = _DESCARGA[comercio](comercio, id_ejecucion, raw_dir)
    df = pd.DataFrame(_PROCESO[comercio](comercio, carpeta, m), columns=COLUMNAS)
    df.attrs["manifiesto"] = m
    return df


def scrape_larebaja(id_ejecucion: str | None = None, offline: bool = False, raw_dir: Path = RAW_DIR) -> pd.DataFrame:
    """Categoría capilar de La Rebaja (API de catálogo VTEX). Devuelve un DataFrame con COLUMNAS."""
    return _scrape("larebaja", id_ejecucion, offline, raw_dir)


def scrape_locatel(id_ejecucion: str | None = None, offline: bool = False, raw_dir: Path = RAW_DIR) -> pd.DataFrame:
    """Categoría capilar de Locatel (API de catálogo VTEX). Devuelve un DataFrame con COLUMNAS."""
    return _scrape("locatel", id_ejecucion, offline, raw_dir)


def scrape_cafam(id_ejecucion: str | None = None, offline: bool = False, raw_dir: Path = RAW_DIR) -> pd.DataFrame:
    """Categoría capilar de Droguerías Cafam (HTML VTEX IO ?page=N). Devuelve un DataFrame con COLUMNAS."""
    return _scrape("cafam", id_ejecucion, offline, raw_dir)


SCRAPERS = {"larebaja": scrape_larebaja, "locatel": scrape_locatel, "cafam": scrape_cafam}


# --------------------------------------------------------------------------------------------------------------
# Limpieza y estandarización (columnas propias; el texto original no se modifica)
# --------------------------------------------------------------------------------------------------------------
def normalizar(texto) -> str:
    """Minúsculas, sin tildes, solo letras/dígitos/.,&+ y espacios simples."""
    t = unicodedata.normalize("NFKD", str(texto or "")).encode("ascii", "ignore").decode().lower()
    t = re.sub(r"[^a-z0-9.,&+ ]+", " ", t)
    return re.sub(r"\s+", " ", t).strip()


def normalizar_ean(ean) -> str | None:
    """EAN/UPC como GTIN-13: solo dígitos, sin ceros a la izquierda y rellenado a 13. Códigos < 8 dígitos -> None."""
    d = re.sub(r"\D", "", str(ean or "")).lstrip("0")
    return d.zfill(13) if len(d) >= 8 else None


ALIAS_MARCA = {
    "h & s": "HEAD & SHOULDERS", "h&s": "HEAD & SHOULDERS", "head and shoulders": "HEAD & SHOULDERS",
    "head shoulders": "HEAD & SHOULDERS", "head & shoulders": "HEAD & SHOULDERS",
    "kler lac": "KLEER LAC", "kleer lac": "KLEER LAC", "soleclair": "SOL ECLAIR", "sol eclair": "SOL ECLAIR",
    "igora vital": "IGORA", "igora brillance": "IGORA", "igora royal": "IGORA",
    "loreal elvive": "ELVIVE", "elvive loreal": "ELVIVE", "loreal paris elvive": "ELVIVE",
    "tio nacho": "TIO NACHO", "cms cosmos": "COSMOS",
}


def marca_std(marca) -> str | None:
    m = normalizar(marca)
    return ALIAS_MARCA.get(m, m.upper()) if m else None


def nombre_std(nombre) -> str:
    t = normalizar(nombre)
    t = re.sub(r"\bsku\s*\d+\b", " ", t)      # La Rebaja agrega 'SKU123' al nombre
    t = re.sub(r"^oferta\s+", "", t)
    return re.sub(r"\s+", " ", t).strip().upper()


# Contenido: número + unidad. Conversión a mL o g (1 L = 1000 mL; 1 kg = 1000 g; 1 fl oz = 29.5735 mL)
_RX_CONT = re.compile(r"(\d+(?:[.,]\d+)?)\s*(ml|mililitros?|cc|litros?|lts?|l|gramos?|grs?|gr|g|kg|kilos?|oz)\b")
_UNIDADES = {"ml": ("ml", 1), "mililitro": ("ml", 1), "mililitros": ("ml", 1), "cc": ("ml", 1),
             "l": ("ml", 1000), "lt": ("ml", 1000), "lts": ("ml", 1000), "litro": ("ml", 1000),
             "litros": ("ml", 1000), "g": ("g", 1), "gr": ("g", 1), "grs": ("g", 1), "gramo": ("g", 1),
             "gramos": ("g", 1), "kg": ("g", 1000), "kilo": ("g", 1000), "kilos": ("g", 1000),
             "oz": ("ml", 29.5735)}
_UNIDADES_FICHA = {"mililitro": ("ml", 1), "mililitros": ("ml", 1), "ml": ("ml", 1), "centimetro cubico": ("ml", 1),
                   "litro": ("ml", 1000), "litros": ("ml", 1000), "gramo": ("g", 1), "gramos": ("g", 1),
                   "g": ("g", 1), "gr": ("g", 1), "kilogramo": ("g", 1000), "unidad": ("un", 1), "unidades": ("un", 1),
                   "un": ("un", 1),
                   "tubo": ("un", 1), "sobre": ("un", 1)}
_RX_PACK = [
    re.compile(r"\b(\d{1,2})\s*(?:unidades|unds?|uds?|unid)\b"),             # "x 2 unidades", "x2 unds"
    re.compile(r"\bpack\s*(?:x\s*)?(\d{1,2})\b"),                            # "pack 2", "pack x 3"
    re.compile(r"\b(\d{1,2})\s*x\s*\d+(?:[.,]\d+)?\s*(?:ml|g|gr)\b"),       # "2 x 400 ml"
    re.compile(r"(?:ml|g|gr)\s*x\s*(\d{1,2})(?!\d)(?![.,]?\d*\s*(?:ml|g|gr|l)\b)"),  # "400ml x 2"
]


def _numero(v: str) -> float:
    return float(v.replace(".", "")) if re.fullmatch(r"\d{1,3}\.\d{3}", v) else float(v.replace(",", "."))


def extraer_contenido(nombre, presentacion_ficha=None) -> tuple:
    """(contenido_unitario, unidad, n_unidades, contenido_total) en mL, g o 'un'.
    Regla: primero el nombre del anuncio; si no trae contenido, la ficha técnica del comercio.
    En paquetes múltiples el contenido total = n × contenido unitario; combos 'A ml + B ml' se suman."""
    t = normalizar(nombre)
    n = 1
    for rx in _RX_PACK:
        mp = rx.search(t)
        if mp and 2 <= int(mp.group(1)) <= 24:
            n = int(mp.group(1))
            break
    coinc = list(_RX_CONT.finditer(t))
    if len(coinc) >= 2 and "+" in t[coinc[0].end():coinc[-1].start()]:
        partes = [(_numero(c.group(1)), *_UNIDADES[c.group(2)]) for c in coinc]
        if len({u for _, u, _ in partes}) == 1:
            total = sum(v * f for v, _, f in partes)
            return None, partes[0][1], len(partes), round(total, 1)
    if coinc:
        v, u = coinc[0].group(1), coinc[0].group(2)
        unidad, f = _UNIDADES[u]
        c = round(_numero(v) * f, 1)
        return c, unidad, n, round(c * n, 1)
    if re.search(r"\blitro\b", t):
        return 1000.0, "ml", n, 1000.0 * n
    if presentacion_ficha:
        mf = re.match(r"\s*([\d.,]+)\s+(.+)", normalizar(presentacion_ficha))
        if mf and mf.group(2) in _UNIDADES_FICHA:
            unidad, f = _UNIDADES_FICHA[mf.group(2)]
            c = round(_numero(mf.group(1)) * f, 1)
            if c > 0:
                return c, unidad, n, round(c * n, 1)
    return None, None, None, None


# Delimitación (docs/delimitacion_categoria.md). Exclusiones en orden de prioridad.
_EXCLUSIONES = [
    ("medicamento", r"\b(minoxidil|ketoconazol|medicasp|clobetasol|ciclopirox|finasterida|regaxidil|pilogan|"
                    r"solucion topica)\b"),
    ("infantil", r"\b(baby|bebes?|kids?|ninos?|ninas?|infantil|junior|disney|marvel|frozen|princesas?|toy story|"
                 r"avengers|spider ?man|minnie|mickey|paw patrol|barbie|peppa|hello kitty|arrurru|nenuco)\b"),
    ("accesorio", r"\b(cepillos?|peinillas?|peines?|monos?|ganchos?|pinzas?|diademas?|balacas?|gorros?|toallas?|"
                  r"rulos?|cauchos?|scrunchies?|hebillas?|accesorios?|secador|plancha|rizador|onduladora|"
                  r"cortadora|maquina|tijeras?|redecillas?|turbantes?|patillera|estilizador|flexstyle)\b"),
    ("fuera_de_categoria", r"\b(bronceador\w*|bloqueador|protector solar|spf|corporal|desodorante|dental|afeitar|"
                           r"depilat\w*|barba|beard|bigote|capsulas?|tabletas?|comprimidos?|gomitas?|softgels?|"
                           r"suplemento|edapil|embarazo|pies)\b"),
]
# Señales de medicamento en la ficha: La Rebaja ("Producto RX/OTC: SI") y Cafam ("Requiere prescripción médica: Sí")
_RX_RX_OTC = re.compile(r"Producto (?:RX|OTC): S[IÍ]|Requiere prescripci[oó]n m[eé]dica: S[IÍ]", re.I)
_SUBCATEGORIAS = [
    ("shampoo", r"\b(shampoo|shampo|champu|champo|shamp)\b"),
    ("acondicionador", r"\b(acondicionador\w*|acond|rinse|enjuague)\b"),
    ("mascarilla_tratamiento", r"\b(mascarillas?|mascara capilar|tratamientos?|tratam|ampollas?|ampolletas?|oleo|"
                               r"aceites?|serum|sellador|elixir|keratina|botox|termoprotector|protector termico|"
                               r"reparador|leave in|crema capilar|tonico|locion|suero)\b"),
    ("peinado", r"\b(cremas? (?:para |de )?peinar|peinar|peinado|gel|geles|laca|laka|pomada|pomade|cera|mousse|"
                r"espuma|fijador\w*|moldeador\w*|modelador\w*|gomina|texturizador|spray|rizos|curl|styling)\b"),
    ("coloracion", r"\b(tintes?|tintura|color creme|coloracion|colorante|decolorante|decoloracion|oxigenada|"
                   r"peroxido|revelador|oxidante|aclarador|retocador|kit color|henna)\b"),
]


def clasificar(nombre, subcategoria_original, texto_original) -> tuple[str | None, int, str | None]:
    """(subcategoria_std, incluido, motivo_exclusion) según la delimitación de la categoría."""
    t, ruta = normalizar(nombre), normalizar(subcategoria_original)
    if _RX_RX_OTC.search(str(texto_original or "")):
        return None, 0, "medicamento"
    for motivo, rx in _EXCLUSIONES:
        if re.search(rx, t) or (motivo in ("infantil", "accesorio") and re.search(rx, ruta)) \
                or (motivo == "infantil" and re.search(r"\b(bebe|maternidad)\b", ruta)):
            return None, 0, motivo
    # subcategoría = palabra clave de tipo que aparece primero en el nombre; si no hay, la ruta del comercio
    for texto in (t, ruta):
        hallazgos = [(m.start(), sub) for sub, rx in _SUBCATEGORIAS if (m := re.search(rx, texto))]
        if hallazgos:
            return min(hallazgos)[1], 1, None
    return "otro_capilar", 1, None


def estandarizar(df: pd.DataFrame) -> pd.DataFrame:
    """Agrega columnas estandarizadas sin tocar las originales."""
    out = df.copy()
    out["ean_norm"] = out["ean"].map(normalizar_ean)
    out["marca_std"] = out["marca_original"].map(marca_std)
    out["nombre_std"] = out["nombre_original"].map(nombre_std)
    cont = [extraer_contenido(n, f) for n, f in zip(out["nombre_original"], out["presentacion_ficha"])]
    out[["contenido_unitario", "unidad", "n_unidades", "contenido"]] = pd.DataFrame(cont, index=out.index)
    clas = [clasificar(n, s, t) for n, s, t in
            zip(out["nombre_original"], out["subcategoria_original"], out["texto_original_anuncio"])]
    out[["subcategoria_std", "incluido", "motivo_exclusion"]] = pd.DataFrame(clas, index=out.index)
    return out


# --------------------------------------------------------------------------------------------------------------
# Base SQLite
# --------------------------------------------------------------------------------------------------------------
ESQUEMA = """
CREATE TABLE IF NOT EXISTS comercios (
    id_comercio TEXT PRIMARY KEY, nombre TEXT NOT NULL, dominio TEXT NOT NULL, metodo_extraccion TEXT,
    paginas_consideradas TEXT
);
CREATE TABLE IF NOT EXISTS productos (           -- identificador común de los productos comparables (comparables.py)
    id_producto_comun TEXT PRIMARY KEY,           -- EAN/GTIN-13 normalizado
    marca_std TEXT, nombre_std TEXT, subcategoria_std TEXT,
    contenido REAL, unidad TEXT, n_unidades INTEGER,
    linea TEXT,                                   -- agrupa tamaños de un mismo producto (precio por mL/g)
    regla_correspondencia TEXT, id_ejecucion_verificacion TEXT
);
CREATE TABLE IF NOT EXISTS anuncios (            -- el producto tal como aparece en un comercio
    id_comercio TEXT NOT NULL REFERENCES comercios(id_comercio),
    sku_comercio TEXT NOT NULL, product_id TEXT, ean TEXT, ean_norm TEXT,
    nombre_original TEXT, texto_original_anuncio TEXT, marca_original TEXT, subcategoria_original TEXT,
    rama_origen TEXT, presentacion_ficha TEXT, tiene_descripcion INTEGER, url TEXT,
    marca_std TEXT, nombre_std TEXT, subcategoria_std TEXT,
    contenido_unitario REAL, unidad TEXT, n_unidades INTEGER, contenido REAL,
    incluido INTEGER NOT NULL, motivo_exclusion TEXT,
    comparable INTEGER NOT NULL DEFAULT 0,
    id_producto_comun TEXT REFERENCES productos(id_producto_comun),
    primera_ejecucion TEXT, ultima_ejecucion TEXT,
    PRIMARY KEY (id_comercio, sku_comercio)
);
CREATE TABLE IF NOT EXISTS observaciones (       -- precio de un anuncio en un momento dado
    id_observacion INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ejecucion TEXT NOT NULL, id_comercio TEXT NOT NULL, sku_comercio TEXT NOT NULL,
    fecha_hora_consulta TEXT NOT NULL,            -- ISO 8601, hora de Colombia (-05:00)
    precio REAL, precio_antes_descuento REAL, es_promocion INTEGER NOT NULL, promocion_texto TEXT,
    disponible INTEGER NOT NULL, vendedor TEXT, url TEXT,
    comparable INTEGER NOT NULL DEFAULT 0, id_producto_comun TEXT,
    UNIQUE (id_comercio, sku_comercio, fecha_hora_consulta),
    FOREIGN KEY (id_comercio, sku_comercio) REFERENCES anuncios(id_comercio, sku_comercio)
);
CREATE TABLE IF NOT EXISTS ejecuciones (
    id_ejecucion TEXT NOT NULL, id_comercio TEXT NOT NULL,
    fecha_hora_inicio TEXT NOT NULL, fecha_hora_fin TEXT,
    n_registros INTEGER, n_productos INTEGER, total_sitio INTEGER, n_incluidos INTEGER,
    estado TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
    detalle_ramas TEXT, nota TEXT,
    PRIMARY KEY (id_ejecucion, id_comercio)
);
CREATE TABLE IF NOT EXISTS correspondencia (     -- qué anuncio de cada comercio corresponde a cada producto común
    id_producto_comun TEXT NOT NULL REFERENCES productos(id_producto_comun),
    id_comercio TEXT NOT NULL, sku_comercio TEXT NOT NULL, id_ejecucion TEXT NOT NULL,
    nombre_original TEXT, marca_std TEXT, contenido REAL, unidad TEXT, n_unidades INTEGER, precio REAL,
    regla TEXT, verificacion TEXT,
    PRIMARY KEY (id_producto_comun, id_comercio, sku_comercio, id_ejecucion)
);
CREATE TABLE IF NOT EXISTS lineas (              -- tamaños distintos de un mismo producto (comparables.py)
    linea TEXT NOT NULL, id_producto_comun TEXT NOT NULL,   -- id = EAN normalizado de cada tamaño
    nota TEXT,
    PRIMARY KEY (linea, id_producto_comun)
);
CREATE INDEX IF NOT EXISTS ix_obs_ejec ON observaciones(id_ejecucion, id_comercio);
CREATE INDEX IF NOT EXISTS ix_obs_prod ON observaciones(id_producto_comun);
DROP VIEW IF EXISTS v_observaciones;
CREATE VIEW v_observaciones AS                   -- todos los campos mínimos de la sección 5 en una sola vista
SELECT o.id_ejecucion, c.nombre AS comercio, o.id_comercio, o.sku_comercio,
       a.id_producto_comun, p.nombre_std AS producto_comun, a.marca_std, a.subcategoria_std,
       a.contenido, a.unidad, a.n_unidades, o.precio, o.precio_antes_descuento, o.es_promocion, o.disponible,
       o.url, a.nombre_original, a.texto_original_anuncio, o.fecha_hora_consulta, o.comparable,
       a.incluido, a.motivo_exclusion, p.linea
FROM observaciones o
JOIN anuncios a ON a.id_comercio = o.id_comercio AND a.sku_comercio = o.sku_comercio
JOIN comercios c ON c.id_comercio = o.id_comercio
LEFT JOIN productos p ON p.id_producto_comun = a.id_producto_comun;
"""

_COLS_ANUNCIO = ["id_comercio", "sku_comercio", "product_id", "ean", "ean_norm", "nombre_original",
                 "texto_original_anuncio", "marca_original", "subcategoria_original", "rama_origen",
                 "presentacion_ficha", "tiene_descripcion", "url", "marca_std", "nombre_std", "subcategoria_std", "contenido_unitario",
                 "unidad", "n_unidades", "contenido", "incluido", "motivo_exclusion",
                 "primera_ejecucion", "ultima_ejecucion"]
_COLS_OBS = ["id_ejecucion", "id_comercio", "sku_comercio", "fecha_hora_consulta", "precio", "precio_antes_descuento",
             "es_promocion", "promocion_texto", "disponible", "vendedor", "url"]


def _sql_valor(v):
    """Convierte NaN/NA de pandas en NULL y los tipos numpy en tipos nativos de Python."""
    if v is None or (not isinstance(v, str) and pd.isna(v)):
        return None
    return v.item() if hasattr(v, "item") else v


def conectar(db_path: Path = DB_PATH) -> sqlite3.Connection:
    con = sqlite3.connect(db_path)
    con.execute("PRAGMA foreign_keys = ON")
    con.executescript(ESQUEMA)
    # Migración: bases creadas antes de agregar columnas nuevas
    existentes = {r[1] for r in con.execute("PRAGMA table_info(anuncios)")}
    if "tiene_descripcion" not in existentes:
        con.execute("ALTER TABLE anuncios ADD COLUMN tiene_descripcion INTEGER")
    return con


def _guardar(con: sqlite3.Connection, df: pd.DataFrame, ejecuciones: pd.DataFrame) -> int:
    """Inserta todo en una transacción. Observaciones: INSERT OR IGNORE (nunca sobrescribe).
    `comparable` e `id_producto_comun` no se tocan en anuncios existentes (los asigna comparables.py); las
    observaciones nuevas los heredan de su anuncio. Devuelve el número de observaciones nuevas."""
    with con:
        con.executemany("INSERT OR REPLACE INTO comercios VALUES (?,?,?,?,?)",
                        [(k, v["nombre"], v["dominio"], v["metodo"],
                          "; ".join(f"{r} ({d})" for r, d in v["ramas"].items())) for k, v in COMERCIOS.items()])
        a = df.rename(columns={"comercio": "id_comercio"}).assign(primera_ejecucion=df["id_ejecucion"],
                                                                 ultima_ejecucion=df["id_ejecucion"])
        actualizar = [c for c in _COLS_ANUNCIO if c not in ("id_comercio", "sku_comercio", "primera_ejecucion")]
        con.executemany(
            f"INSERT INTO anuncios ({','.join(_COLS_ANUNCIO)}) VALUES ({','.join('?' * len(_COLS_ANUNCIO))}) "
            f"ON CONFLICT(id_comercio, sku_comercio) DO UPDATE SET "
            + ", ".join(f"{c}=excluded.{c}" for c in actualizar)
            + " WHERE excluded.ultima_ejecucion >= anuncios.ultima_ejecucion",
            [tuple(map(_sql_valor, r)) for r in a[_COLS_ANUNCIO].itertuples(index=False)])
        antes = con.execute("SELECT COUNT(*) FROM observaciones").fetchone()[0]
        con.executemany(f"INSERT OR IGNORE INTO observaciones ({','.join(_COLS_OBS)}) "
                        f"VALUES ({','.join('?' * len(_COLS_OBS))})",
                        [tuple(map(_sql_valor, r)) for r in a[_COLS_OBS].itertuples(index=False)])
        nuevas = con.execute("SELECT COUNT(*) FROM observaciones").fetchone()[0] - antes
        con.execute("""UPDATE observaciones SET
                           comparable = (SELECT a.comparable FROM anuncios a WHERE a.id_comercio = observaciones.id_comercio
                                         AND a.sku_comercio = observaciones.sku_comercio),
                           id_producto_comun = (SELECT a.id_producto_comun FROM anuncios a
                                                WHERE a.id_comercio = observaciones.id_comercio
                                                AND a.sku_comercio = observaciones.sku_comercio)
                       WHERE id_ejecucion = ?""", (df["id_ejecucion"].iloc[0],))
        con.executemany(f"INSERT OR IGNORE INTO ejecuciones ({','.join(ejecuciones.columns)}) "
                        f"VALUES ({','.join('?' * len(ejecuciones.columns))})",
                        [tuple(map(_sql_valor, r)) for r in ejecuciones.itertuples(index=False)])
    return nuevas


# --------------------------------------------------------------------------------------------------------------
# Ejecución completa
# --------------------------------------------------------------------------------------------------------------
def run_all(offline: bool = False, id_ejecucion: str | None = None, db_path: Path = DB_PATH,
            raw_dir: Path = RAW_DIR, verbose: bool = True) -> dict:
    """Ejecuta los tres comercios, estandariza, aplica la delimitación y guarda en SQLite.
    Los comparables se marcan después con comparables.py.

    offline=False: hace las peticiones, guarda las copias crudas en raw/ y crea una ejecución nueva.
    offline=True : no hace ninguna petición; reprocesa las copias crudas de `id_ejecucion` (por defecto, la última).
    Las observaciones se insertan con INSERT OR IGNORE: nunca se sobrescriben observaciones anteriores.
    """
    if offline:
        id_ejecucion = id_ejecucion or ultima_ejecucion_guardada(raw_dir)
        if id_ejecucion is None:
            raise FileNotFoundError(f"No hay ejecuciones guardadas en {raw_dir}")
    else:
        id_ejecucion = id_ejecucion or nuevo_id_ejecucion()
    log = print if verbose else (lambda *a, **k: None)
    log(f"Ejecución {id_ejecucion} ({'sin conexión' if offline else 'en línea'})")

    partes, ejec = [], []
    for comercio, funcion in SCRAPERS.items():
        try:
            d = funcion(id_ejecucion=id_ejecucion, offline=offline, raw_dir=raw_dir)
            m = d.attrs["manifiesto"]
        except Exception as e:  # el fallo de un comercio no detiene a los demás
            d = pd.DataFrame(columns=COLUMNAS)
            m = {"inicio": ahora_iso(), "fin": ahora_iso(), "estado": "fallida", "ramas": {},
                 "error": f"{type(e).__name__}: {e}"}
        n_antes = len(d)
        # El mismo SKU puede listarse en dos ramas del comercio: se conserva una fila y se anotan ambas ramas
        if len(d):
            ramas = d.groupby("sku_comercio")["rama_origen"].agg(lambda s: "; ".join(dict.fromkeys(s)))
            d = d.drop_duplicates("sku_comercio").assign(rama_origen=lambda x: x["sku_comercio"].map(ramas))
        notas = [r["nota"] for r in m["ramas"].values() if r.get("nota")]
        if m.get("error"):
            notas.append(f"Error: {m['error']}")
        if n_antes > len(d):
            notas.append(f"{n_antes - len(d)} filas repetidas entre ramas del mismo comercio (se conservó una)")
        ejec.append({"id_ejecucion": id_ejecucion, "id_comercio": comercio, "fecha_hora_inicio": m["inicio"],
                     "fecha_hora_fin": m["fin"], "n_registros": len(d),
                     "n_productos": d["product_id"].nunique() if len(d) else 0,
                     "total_sitio": sum(r["total_sitio"] or 0 for r in m["ramas"].values()),
                     "estado": m["estado"],
                     "detalle_ramas": "; ".join(f"{k}: {r['productos_obtenidos']}/{r['total_sitio']}"
                                                for k, r in m["ramas"].items()),
                     "nota": " | ".join(notas) or None})
        partes.append(d)
        log(f"  {comercio:9s} {m['estado']:9s} registros={len(d):5d}  ramas: {ejec[-1]['detalle_ramas']}")

    todo = pd.concat([p for p in partes if len(p)], ignore_index=True)
    todo = estandarizar(todo)
    ejecuciones = pd.DataFrame(ejec)
    ejecuciones["n_incluidos"] = ejecuciones["id_comercio"].map(
        todo[todo["incluido"] == 1].groupby("comercio").size()).fillna(0).astype(int)

    con = conectar(db_path)
    try:
        nuevas = _guardar(con, todo, ejecuciones)
    finally:
        con.close()
    log(f"  Observaciones nuevas en {Path(db_path).name}: {nuevas}")
    return {"id_ejecucion": id_ejecucion, "ejecuciones": ejecuciones, "observaciones_nuevas": nuevas, "datos": todo}


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Recolección de cuidado capilar: La Rebaja, Locatel y Droguerías Cafam")
    ap.add_argument("--offline", action="store_true", help="reprocesar raw/ sin hacer peticiones")
    ap.add_argument("--id", default=None, help="id de ejecución a reprocesar (por defecto, la última)")
    a = ap.parse_args()
    run_all(offline=a.offline, id_ejecucion=a.id)
