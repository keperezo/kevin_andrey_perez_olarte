"""
scraper.py — Módulo de extracción del Taller 1 de Minería de Datos (UNAL, 2026-2).

Categoría: Medicamentos antiinfecciosos de uso sistémico.
Comercios: La Rebaja Virtual, Locatel Colombia y Droguerías Cafam.
           (Colsubsidio queda implementado como comercio de respaldo.)

Los cuatro sitios usan la plataforma VTEX.

Cafam: su robots.txt prohíbe /api/, así que leemos sus páginas de categoría
(HTML), que sí están permitidas: /salud-y-medicamentos/<subcategoría>?page=N.
Cada página trae sus 12 productos en un bloque JSON llamado __STATE__
(ver _descargar_cafam).

La Rebaja y Locatel: la página de cada comercio carga sus productos desde el
servicio interno de catálogo de VTEX, que su robots.txt permite:

    GET {sitio}/api/catalog_system/pub/products/search
        ?fq=C:/<ids de la categoría>/      (filtro por categoría)
        &fq=P:[<min> TO <max>]              (filtro por rango de precio, opcional)
        &_from=<i>&_to=<j>                  (paginación: máximo 50 por página)
        &O=OrderByNameASC                   (orden estable entre páginas)

La respuesta es JSON y trae el total en la cabecera HTTP `resources`
("0-49/4306" significa 4306 productos). VTEX no deja paginar más allá de la
posición ~2500, así que cuando una sección tiene más de 2500 productos se
parte por rangos de precio hasta que cada parte quede por debajo del límite.

Uso desde Python o desde el .qmd:
    import scraper
    scraper.ejecutar_recoleccion()                     # en línea
    scraper.ejecutar_recoleccion(sin_conexion=True)    # sin conexión, desde raw/

Uso desde la terminal:
    python scraper.py                  # en línea, los tres comercios
    python scraper.py --sin-conexion   # reprocesa todas las copias de raw/
"""
from __future__ import annotations

import argparse
import gzip
import html
import json
import math
import re
import sqlite3
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import parse_qs, quote, urlencode, urlparse

import pandas as pd
import requests

import categoria
import limpieza

# ---------------------------------------------------------------------------
# Configuración general
# ---------------------------------------------------------------------------
BASE_DIR = Path(__file__).resolve().parent
DB_PATH = BASE_DIR / "comparador_precios.sqlite"
RAW_DIR = BASE_DIR / "raw"

HORA_COLOMBIA = timezone(timedelta(hours=-5))   # Colombia no tiene horario de verano
PAUSA_SEGUNDOS = 1.0          # pausa entre peticiones (buena práctica)
TAMANO_PAGINA = 50            # máximo que acepta VTEX
LIMITE_PAGINACION = 2500      # VTEX responde 400 si _from supera ~2500
PRECIO_MAXIMO = 5_000_000     # cota superior (COP) para partir por rangos de precio
REINTENTOS = 4

CABECERAS = {
    "User-Agent": ("Mozilla/5.0 (compatible; TallerMineriaDatosUNAL/1.0; "
                   "uso academico; consulta 1 pagina por segundo)"),
    "Accept": "application/json",
    "Accept-Language": "es-CO,es;q=0.9",
}

COMERCIOS = {
    "larebaja": {
        "nombre": "La Rebaja Virtual (Copservir)",
        "url_base": "https://www.larebajavirtual.com",
        # El departamento DROGUERIA completo: los medicamentos con fórmula están
        # todos en una sola subcategoría, sin separar por grupo terapéutico.
        "secciones": [("C:/9/", "DROGUERIA")],
        "robots": "robots.txt (revisado 2026-10-02) no restringe /api/.",
    },
    "locatel": {
        "nombre": "Locatel Colombia",
        "url_base": "https://www.locatelcolombia.com",
        "secciones": [("C:/4/25/", "Drogueria > medicamentos"),
                      ("C:/4/28/", "Drogueria > Medicamentos Otc"),
                      ("C:/4/26/168/", "Drogueria > vitaminas > Antiinfecciosos")],
        "robots": "robots.txt (revisado 2026-10-02) no restringe /api/.",
    },
    "cafam": {
        "nombre": "Droguerías Cafam",
        "url_base": "https://www.drogueriascafam.com.co",
        "plataforma": "VTEX IO (páginas HTML de categoría)",
        # Se recorren las subcategorías de esta sección, cada una con menos de
        # 2500 productos (el máximo que deja paginar VTEX).
        "secciones": [("/salud-y-medicamentos", "Salud y medicamentos")],
        "robots": ("robots.txt (revisado 2026-10-03) prohíbe /api/ y los parámetros ?O=, ?PS=, "
                   "?q= y map=ft, pero no las páginas de categoría con ?page=N. Por eso se leen "
                   "esas páginas (HTML) y no el servicio /api/."),
    },
    "colsubsidio": {   # respaldo: no se usa por defecto
        "nombre": "Droguerías Colsubsidio",
        "url_base": "https://www.drogueriascolsubsidio.com",
        "secciones": [("C:/1/", "Medicamentos")],
        "robots": ("robots.txt (revisado 2026-10-02) incluye 'Disallow: /api/' para "
                   "rastreadores."),
    },
}
COMERCIOS_POR_DEFECTO = ["larebaja", "locatel", "cafam"]

# Columnas que devuelve SIEMPRE cada función de comercio (mismo orden y formato)
COLUMNAS = [
    "comercio", "id_producto_comercio", "sku_comercio", "ean",
    "nombre_original", "nombre_sku_original", "texto_original",
    "marca_original", "laboratorio_original", "principio_activo_original",
    "presentacion_original", "unidad_medida_original", "cantidad_original",
    "via_original", "requiere_formula", "categoria_comercio",
    "url_producto", "url_extraccion",
    "precio", "precio_lista", "promocion", "disponible",
    "fecha_consulta", "id_ejecucion",
]


# ---------------------------------------------------------------------------
# Utilidades
# ---------------------------------------------------------------------------
def ahora_iso() -> str:
    """Fecha y hora actual de Colombia en ISO 8601, p. ej. 2026-10-02T10:35:00-05:00."""
    return datetime.now(HORA_COLOMBIA).replace(microsecond=0).isoformat()


def nuevo_id_ejecucion() -> str:
    return "E" + datetime.now(HORA_COLOMBIA).strftime("%Y%m%dT%H%M%S")


def _total_de_cabecera(resources: str | None) -> int | None:
    """'0-49/4306' -> 4306"""
    if not resources or "/" not in resources:
        return None
    try:
        return int(resources.split("/")[-1])
    except ValueError:
        return None


def _spec(p: dict, *nombres: str) -> str | None:
    """Valor de una especificación VTEX (vienen como listas). Une varios con ' | '."""
    for nombre in nombres:
        valor = p.get(nombre)
        if valor:
            if isinstance(valor, list):
                valor = " | ".join(str(v) for v in valor if v not in (None, ""))
            valor = str(valor).strip()
            if valor:
                return valor
    return None


def _guardar_json_gz(ruta: Path, objeto) -> None:
    ruta.parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(ruta, "wt", encoding="utf-8") as f:
        json.dump(objeto, f, ensure_ascii=False)


def _leer_json_gz(ruta: Path):
    with gzip.open(ruta, "rt", encoding="utf-8") as f:
        return json.load(f)


# ---------------------------------------------------------------------------
# Peticiones HTTP
# ---------------------------------------------------------------------------
def _nueva_sesion() -> requests.Session:
    s = requests.Session()
    s.headers.update(CABECERAS)
    return s


def _get(sesion: requests.Session, url: str, params: list[tuple],
         cabeceras: dict | None = None) -> requests.Response:
    """
    GET con reintentos y espera creciente ante 429/5xx o fallas de red.

    Los espacios se codifican como %20 y no como "+": el firewall de La Rebaja
    y Locatel responde 400 ("Scripts are not allowed!") a `P:[0+TO+500000]`.
    Otros errores 4xx no se reintentan, porque repetir la petición no los arregla.
    """
    consulta = urlencode(params, quote_via=quote)
    url_completa = f"{url}?{consulta}" if consulta else url
    ultimo_error = None
    for intento in range(REINTENTOS):
        try:
            r = sesion.get(url_completa, timeout=40, headers=cabeceras)
            if r.status_code in (200, 206):
                return r
            if r.status_code in (429, 500, 502, 503, 504):
                ultimo_error = f"HTTP {r.status_code}"
            else:
                raise RuntimeError(f"No se pudo consultar {url_completa}: HTTP {r.status_code} "
                                   f"{r.text[:100]!r}")
        except requests.RequestException as e:   # incluye timeouts
            ultimo_error = repr(e)
        time.sleep(PAUSA_SEGUNDOS * 2 ** (intento + 1))
    raise RuntimeError(f"No se pudo consultar {url} {params}: {ultimo_error}")


def _contar(sesion, url_busqueda: str, filtros: list[str]) -> int:
    params = [("fq", f) for f in filtros] + [("_from", 0), ("_to", 0)]
    r = _get(sesion, url_busqueda, params)
    time.sleep(PAUSA_SEGUNDOS)
    total = _total_de_cabecera(r.headers.get("resources"))
    return total if total is not None else len(r.json())


def _num(x: float) -> str:
    return str(int(x)) if float(x).is_integer() else f"{x:.2f}"


def _particionar(sesion, url_busqueda, fq_cat, minimo, maximo) -> list[tuple[list[str], int]]:
    """Parte una sección por rangos de precio hasta que cada parte sea paginable."""
    filtro_precio = f"P:[{_num(minimo)} TO {_num(maximo)}]"
    total = _contar(sesion, url_busqueda, [fq_cat, filtro_precio])
    if total <= LIMITE_PAGINACION or maximo - minimo < 1:
        return [([fq_cat, filtro_precio], total)] if total > 0 else []
    medio = round((minimo + maximo) / 2, 2)
    return (_particionar(sesion, url_busqueda, fq_cat, minimo, medio)
            + _particionar(sesion, url_busqueda, fq_cat, round(medio + 0.01, 2), maximo))


# ---------------------------------------------------------------------------
# Recolección genérica VTEX (en línea) y lectura de copias crudas (sin conexión)
# ---------------------------------------------------------------------------
def _descargar_vtex(clave: str, carpeta: Path, sesion=None) -> dict:
    """
    Recorre todas las secciones configuradas de un comercio, guarda cada
    respuesta en `carpeta` (JSON comprimido) y devuelve un manifiesto con los
    conteos que se usan para verificar que la recolección quedó completa.
    """
    cfg = COMERCIOS[clave]
    sesion = sesion or _nueva_sesion()
    url_busqueda = cfg["url_base"] + "/api/catalog_system/pub/products/search"
    carpeta.mkdir(parents=True, exist_ok=True)

    # Árbol de categorías: se guarda para documentar qué secciones se usaron.
    r_arbol = _get(sesion, cfg["url_base"] + "/api/catalog_system/pub/category/tree/4", [])
    _guardar_json_gz(carpeta / "arbol_categorias.json.gz",
                     {"url": r_arbol.url, "fecha_consulta": ahora_iso(), "respuesta": r_arbol.json()})
    time.sleep(PAUSA_SEGUNDOS)

    manifiesto = {"comercio": clave, "secciones": [], "paginas": 0, "errores": []}
    n_pagina = 0
    for fq_cat, nombre_seccion in cfg["secciones"]:
        total_seccion = _contar(sesion, url_busqueda, [fq_cat])
        consultas = ([([fq_cat], total_seccion)] if total_seccion <= LIMITE_PAGINACION
                     else _particionar(sesion, url_busqueda, fq_cat, 0, PRECIO_MAXIMO))
        manifiesto["secciones"].append({
            "fq": fq_cat, "nombre": nombre_seccion, "total_reportado": total_seccion,
            "particiones": [{"filtros": f, "total": t} for f, t in consultas],
        })
        for parte, (filtros, total) in enumerate(consultas):
            vistos, errores = set(), []
            for vuelta in (1, 2):
                for desde in range(0, total, TAMANO_PAGINA):
                    hasta = min(desde + TAMANO_PAGINA - 1, total - 1)
                    params = ([("fq", f) for f in filtros]
                              + [("_from", desde), ("_to", hasta), ("O", "OrderByNameASC")])
                    n_pagina += 1
                    try:
                        r = _get(sesion, url_busqueda, params)
                        productos = r.json()
                        vistos.update(str(p.get("productId")) for p in productos)
                        pagina = {"url": r.url, "status": r.status_code,
                                  "resources": r.headers.get("resources"),
                                  "fecha_consulta": ahora_iso(), "seccion": nombre_seccion,
                                  "respuesta": productos}
                        _guardar_json_gz(carpeta / f"pagina_{n_pagina:04d}.json.gz", pagina)
                    except Exception as e:   # se anota y se sigue con la siguiente página
                        errores.append({"pagina": n_pagina, "params": params, "error": repr(e)})
                    time.sleep(PAUSA_SEGUNDOS)
                # Si faltan productos, el catálogo cambió mientras pasábamos las páginas
                # (un producto se corrió de página y otro no alcanzó a salir). Damos una
                # segunda vuelta y juntamos lo que salga.
                if len(vistos) >= total:
                    break
            particion = manifiesto["secciones"][-1]["particiones"][parte]
            particion.update({"productos_distintos": len(vistos), "vueltas": vuelta})
            # Los errores de página solo cuentan si ni la segunda vuelta los cubrió. Si
            # faltan productos, _estado lo detecta al comparar la sección completa con
            # su total (un producto que cambió de precio pudo salir en otro rango).
            if len(vistos) < total:
                manifiesto["errores"].extend(errores)
    manifiesto["paginas"] = n_pagina
    return manifiesto


# ---------------------------------------------------------------------------
# Cafam: páginas de categoría en HTML (su robots.txt prohíbe /api/)
# ---------------------------------------------------------------------------
# Cafam usa VTEX IO. Cada página de categoría trae dentro del HTML un bloque
# "__STATE__ = {...}" con los 12 productos que muestra. Leemos ese bloque en vez
# del servicio /api/ y recorremos las páginas con "?page=N", que sí está permitido.
PRODUCTOS_POR_PAGINA_HTML = 12
# VTEX IO no muestra más de 50 páginas por listado (600 productos). Si una
# subcategoría tiene más, se parte por rangos de precio con "?priceRange=A TO B"
# (robots.txt no lo prohíbe; sí prohíbe initialMap=, map=ft, ?O=, ?PS= y ?q=).
MAXIMO_LISTADO_HTML = 50 * PRODUCTOS_POR_PAGINA_HTML


def _estado_de_html(texto: str) -> dict:
    """Saca el JSON que viene después de '__STATE__ = ' en el HTML."""
    inicio = texto.find("__STATE__ = ")
    if inicio < 0:
        raise ValueError("la página no trae el bloque __STATE__")
    estado, _ = json.JSONDecoder().raw_decode(texto, inicio + len("__STATE__ = "))
    return estado


def _clave_busqueda(estado: dict) -> str:
    """Clave del bloque que tiene la búsqueda de productos de la página."""
    return next(k for k, v in estado.items()
                if k.startswith("$ROOT_QUERY.productSearch(") and isinstance(v, dict) and "products" in v)


def _referencias(valor) -> list[str]:
    """Ids a los que apunta un valor del __STATE__ (ahí todo se guarda "aplanado")."""
    if isinstance(valor, dict):
        if valor.get("type") == "id" and "id" in valor:
            return [valor["id"]]
        return [r for v in valor.values() for r in _referencias(v)]
    if isinstance(valor, list):
        return [r for v in valor for r in _referencias(v)]
    return []


def _recortar_estado(estado: dict) -> dict:
    """
    Copia cruda que guardamos en raw/: la búsqueda de productos y todo lo que cuelga
    de ella (producto, SKU, vendedor, oferta, especificaciones). Dejamos por fuera las
    facetas de los filtros (la mitad del bloque) y el 'priceToken', que es una firma
    de la sesión y no un dato del producto.
    """
    guardar, pendientes = {}, [_clave_busqueda(estado)]
    while pendientes:
        clave = pendientes.pop()
        if clave in guardar or clave not in estado:
            continue
        valor = estado[clave]
        if isinstance(valor, dict) and "priceToken" in valor:
            valor = {k: v for k, v in valor.items() if k != "priceToken"}
        guardar[clave] = valor
        pendientes.extend(_referencias(valor))
    return guardar


def _productos_de_estado(estado: dict, url_base: str) -> list[dict]:
    """
    Pasa los productos del __STATE__ al mismo formato del servicio de catálogo de
    VTEX (el de La Rebaja y Locatel). Así _fila_base y _spec sirven igual para Cafam.
    """
    def ver(x):   # sigue una referencia del bloque
        if isinstance(x, dict) and x.get("type") == "id":
            return estado.get(x["id"], {})
        if isinstance(x, dict) and x.get("type") == "json":
            return x.get("json")
        return x

    productos = []
    for ref in estado[_clave_busqueda(estado)]["products"]:
        p = ver(ref)
        items = []
        # la página pide los SKU con el filtro FIRST_AVAILABLE: trae uno por producto
        clave_items = next((k for k in p if k.startswith("items")), None)
        for ref_item in p.get(clave_items) or []:
            item = ver(ref_item)
            vendedores = []
            for ref_vendedor in item.get("sellers") or []:
                vendedor = ver(ref_vendedor)
                oferta = {k: v for k, v in (ver(vendedor.get("commertialOffer")) or {}).items()
                          if not isinstance(v, (dict, list))}
                # la página no trae IsAvailable: está disponible si hay unidades
                oferta["IsAvailable"] = (oferta.get("AvailableQuantity") or 0) > 0
                vendedores.append({"sellerId": vendedor.get("sellerId"),
                                   "sellerDefault": vendedor.get("sellerDefault"),
                                   "commertialOffer": oferta})
            items.append({"itemId": item.get("itemId"), "name": item.get("name"),
                          "nameComplete": item.get("nameComplete"), "ean": item.get("ean"),
                          "sellers": vendedores})
        link = p.get("link") or ""
        producto = {"productId": p.get("productId"), "productName": p.get("productName"),
                    "brand": p.get("brand"), "description": p.get("description"),
                    "link": url_base + link if link.startswith("/") else link,
                    "categories": ver(p.get("categories")) or [], "items": items,
                    "allSpecifications": []}
        for ref_prop in p.get("properties") or []:
            prop = ver(ref_prop)
            if prop.get("name"):
                producto[prop["name"]] = ver(prop.get("values"))
                producto["allSpecifications"].append(prop["name"])
        productos.append(producto)
    return productos


def _descargar_cafam(clave: str, carpeta: Path, sesion=None) -> dict:
    """
    Recorre las subcategorías de "Salud y medicamentos" página por página y guarda
    en `carpeta` el bloque de productos de cada página. Devuelve el manifiesto.
    """
    cfg = COMERCIOS[clave]
    sesion = sesion or _nueva_sesion()
    carpeta.mkdir(parents=True, exist_ok=True)
    ruta_categoria, nombre_categoria = cfg["secciones"][0]

    def pedir(ruta, pagina, rango=None):
        params = ([("priceRange", rango)] if rango else []) + [("page", pagina)]
        r = _get(sesion, f"{cfg['url_base']}{ruta}", params, cabeceras={"Accept": "text/html"})
        time.sleep(PAUSA_SEGUNDOS)
        return r.url, r.status_code, _estado_de_html(r.text)

    def partir(ruta, minimo, maximo):
        """Parte [minimo, maximo] a la mitad hasta que cada rango tenga ≤ 600 productos."""
        rango = f"{minimo} TO {maximo}"
        _, _, estado = pedir(ruta, 1, rango)
        total = estado[_clave_busqueda(estado)]["recordsFiltered"]
        if total <= MAXIMO_LISTADO_HTML or maximo - minimo <= 1:
            return [(rango, total)]
        medio = (minimo + maximo) // 2
        # los extremos son inclusivos: un producto con precio = medio sale en los dos
        # rangos, y _recolectar descarta el SKU repetido
        return partir(ruta, minimo, medio) + partir(ruta, medio, maximo)

    # 1. Primera página de la sección: el total que muestra el sitio y las subcategorías
    url, _, estado = pedir(ruta_categoria, 1)
    total_sitio = estado[_clave_busqueda(estado)]["recordsFiltered"]
    subcategorias = sorted({(v["value"], v["name"], v["quantity"]) for v in estado.values()
                            if isinstance(v, dict) and v.get("key") == "category-2"})
    _guardar_json_gz(carpeta / "subcategorias.json.gz", {
        "url": url, "fecha_consulta": ahora_iso(), "seccion": nombre_categoria,
        "total_reportado": total_sitio,
        "subcategorias": [{"ruta": f"{ruta_categoria}/{s}", "nombre": n, "cantidad": c}
                          for s, n, c in subcategorias]})

    manifiesto = {"comercio": clave, "secciones": [], "paginas": 0, "errores": [],
                  "total_sitio": total_sitio,
                  "nota": "páginas HTML de categoría (?page=N); en raw/ se guarda el bloque "
                          "__STATE__ con los productos de cada página"}
    # 2. Listados que se van a recorrer: uno por subcategoría, o varios rangos de
    #    precio si la subcategoría tiene más de 600 productos. Para empezar a partir
    #    se usan los rangos de la faceta "Precio" que muestra el propio sitio.
    listados = []   # (ruta, nombre, rango de precio o None)
    for slug, nombre, cantidad in subcategorias:
        ruta = f"{ruta_categoria}/{slug}"
        if cantidad <= MAXIMO_LISTADO_HTML:
            listados.append((ruta, nombre, None))
            continue
        _, _, estado = pedir(ruta, 1)
        bordes = sorted({x for k, v in estado.items() if k.endswith(".range") and isinstance(v, dict)
                         for x in (v["from"], v["to"])})
        if len(bordes) < 2:            # si el sitio no muestra la faceta de precio
            bordes = [0, PRECIO_MAXIMO]
        bordes[0], bordes[-1] = 0, bordes[-1] * 2   # holgura por si un precio cambia en la corrida
        rangos = [r for a, b in zip(bordes, bordes[1:]) for r in partir(ruta, a, b)]
        if sum(t for _, t in rangos) < cantidad:
            manifiesto["errores"].append({"seccion": nombre,
                                          "error": "los rangos de precio no cubren la subcategoría"})
        listados += [(ruta, f"{nombre} [{rango}]", rango) for rango, t in rangos if t > 0]

    n_pagina = 0
    # 3. Cada listado, página por página. El orden de la página es "por
    #    relevancia" (robots.txt no deja cambiarlo con ?O=), así que si al final
    #    faltan productos damos una segunda vuelta y juntamos lo que salga.
    for ruta, nombre, rango in listados:
        vistos, total, n_paginas, errores = set(), None, None, []
        for vuelta in (1, 2):
            pagina = 1
            while n_paginas is None or pagina <= n_paginas:
                n_pagina += 1
                try:
                    url, status, estado = pedir(ruta, pagina, rango)
                    busqueda = estado[_clave_busqueda(estado)]
                    total = busqueda["recordsFiltered"]
                    n_paginas = math.ceil(total / PRODUCTOS_POR_PAGINA_HTML)
                    vistos.update(ref["id"] for ref in busqueda["products"])
                    _guardar_json_gz(carpeta / f"pagina_{n_pagina:04d}.json.gz", {
                        "url": url, "status": status, "fecha_consulta": ahora_iso(),
                        "seccion": nombre, "estado": _recortar_estado(estado)})
                except Exception as e:   # se anota y se sigue con la siguiente página
                    errores.append({"pagina": n_pagina, "url": f"{cfg['url_base']}{ruta}",
                                    "rango_precio": rango, "page": pagina, "error": repr(e)})
                    if n_paginas is None:
                        break
                pagina += 1
            if total is not None and len(vistos) >= total:
                break
        completa = total is not None and len(vistos) >= total
        manifiesto["secciones"].append({"fq": ruta, "rango_precio": rango, "nombre": nombre,
                                        "total_reportado": total or 0, "vueltas": vuelta,
                                        "errores_recuperados": len(errores) if completa else 0})
        # si faltan productos, _estado lo detecta al contar por sección; aquí solo
        # se anotan las peticiones que fallaron
        if not completa:
            manifiesto["errores"].extend(errores)
    manifiesto["paginas"] = n_pagina
    return manifiesto


def _leer_paginas(carpeta: Path) -> list[dict]:
    return [_leer_json_gz(ruta) for ruta in sorted(carpeta.glob("pagina_*.json.gz"))]


def _oferta(item: dict) -> dict:
    vendedores = item.get("sellers") or []
    vendedor = next((s for s in vendedores if s.get("sellerDefault")), vendedores[0] if vendedores else {})
    return vendedor.get("commertialOffer") or {}


def _texto_original(p: dict, item: dict) -> str:
    """Texto del anuncio tal como se publicó: nombre, nombre del SKU, descripción y ficha."""
    especificaciones = {k: p.get(k) for k in (p.get("allSpecifications") or [])
                        if not k.startswith("ML_")}   # ML_* = campos internos de sincronización
    return json.dumps({
        "nombre": p.get("productName"),
        "nombre_sku": item.get("nameComplete") or item.get("name"),
        "descripcion": p.get("description"),
        "especificaciones": especificaciones,
    }, ensure_ascii=False)


def _fila_base(clave: str, p: dict, item: dict, pagina: dict, id_ejecucion: str) -> dict:
    oferta = _oferta(item)
    precio = oferta.get("Price")
    lista = oferta.get("ListPrice") or oferta.get("PriceWithoutDiscount")
    disponible = bool(oferta.get("IsAvailable"))
    if precio is None or (precio == 0 and not disponible):
        precio = None                       # nunca se reemplaza un faltante por 0
    if lista == 0:
        lista = None
    promocion = None if precio is None or lista is None else int(precio < lista)
    return {
        "comercio": clave,
        "id_producto_comercio": str(p.get("productId")),
        "sku_comercio": str(item.get("itemId")),
        "ean": (item.get("ean") or None),
        "nombre_original": p.get("productName"),
        "nombre_sku_original": item.get("nameComplete") or item.get("name"),
        "texto_original": _texto_original(p, item),
        "marca_original": p.get("brand"),
        "categoria_comercio": (p.get("categories") or [None])[0],
        "url_producto": p.get("link"),
        "url_extraccion": pagina.get("url"),
        "precio": float(precio) if precio is not None else None,
        "precio_lista": float(lista) if lista is not None else None,
        "promocion": promocion,
        "disponible": int(disponible),
        "fecha_consulta": pagina.get("fecha_consulta"),
        "id_ejecucion": id_ejecucion,
    }


def _via_desde_descripcion(descripcion: str | None) -> str | None:
    """
    Locatel escribe la vía en la descripción de varias formas: "Vía de administración:
    Oral", "Vía Oftálmica", "Via otica", "Gotas óticas". Tomamos la primera mención
    que sea una vía conocida (así no confundimos "vía urinaria" con una vía).
    """
    if not descripcion:
        return None
    texto = categoria.normalizar(re.sub(r"<[^>]+>", " ", html.unescape(descripcion)))
    for m in re.finditer(r"\bvia\s+(?:de\s+)?(?:administracion\s*)?:?\s*(?:via\s+)?([a-z]+)", texto):
        via = m.group(1).upper()
        if via.startswith(categoria.VIAS_SISTEMICAS + categoria.VIAS_LOCALES):
            return via
    if re.search(r"gotas\s+o(?:p)?ticas", texto):      # Locatel escribe a veces "ópticas"
        return "OTICA"
    if re.search(r"gotas\s+oftalmicas|solucion\s+oftalmica", texto):
        return "OFTALMICA"
    return None


def _principio_desde_descripcion(descripcion: str | None) -> str | None:
    """
    Algunos productos de Locatel no traen la ficha "Principio activo", pero lo
    escriben en la descripción: 'Principio Activo: CEFALEXINA Registro Sanitario: ...'
    o 'Principio activo": ["ITRACONAZOL, SECNIDAZOL"]'. Pedimos los dos puntos para
    no tomar frases como "alergia al principio activo o a los excipientes".
    """
    if not descripcion:
        return None
    texto = " ".join(re.sub(r"<[^>]+>", " ", html.unescape(descripcion)).split())
    m = re.search(r'principio\s+activo"?\s*:\s*\[?\s*"?\s*([^"\]\[:;]{3,80}?)\s*(?:"|\]|registro|$)',
                  texto, re.I)
    return m.group(1).strip(" .,") if m else None


def _a_dataframe(filas: list[dict], manifiesto: dict) -> pd.DataFrame:
    df = pd.DataFrame(filas, columns=COLUMNAS)
    df.attrs["manifiesto"] = manifiesto
    return df


def _recolectar(clave: str, mapear, id_ejecucion: str, carpeta: Path,
                sin_conexion: bool, sesion=None, descargar=None) -> pd.DataFrame:
    """Flujo común: descargar (o leer raw/) y convertir cada SKU en una fila."""
    if sin_conexion:
        manifiesto = json.loads((carpeta / "manifest.json").read_text(encoding="utf-8"))
    else:
        manifiesto = (descargar or _descargar_vtex)(clave, carpeta, sesion)
    url_base = COMERCIOS[clave]["url_base"]
    filas, productos_vistos, skus_vistos, repetidos = [], set(), set(), 0
    por_seccion = {}   # productos distintos de cada sección, para verificar que esté completa
    huecos = {}        # Cafam: (sección, página) -> productos que faltan en cada vuelta
    for pagina in _leer_paginas(carpeta):
        productos = pagina.get("respuesta")
        if productos is None and "estado" in pagina:      # Cafam: bloque __STATE__ del HTML
            productos = _productos_de_estado(pagina["estado"], url_base)
            # Cafam a veces cuenta un producto que no muestra: una página trae 11 en
            # vez de 12. Se anota lo que falta en cada vuelta para revisarlo abajo.
            total = pagina["estado"][_clave_busqueda(pagina["estado"])]["recordsFiltered"]
            n = int(parse_qs(urlparse(pagina["url"]).query)["page"][0])
            esperados = min(PRODUCTOS_POR_PAGINA_HTML, total - PRODUCTOS_POR_PAGINA_HTML * (n - 1))
            huecos.setdefault((pagina.get("seccion"), n), []).append(max(esperados - len(productos), 0))
        for p in productos or []:
            productos_vistos.add(str(p.get("productId")))
            por_seccion.setdefault(pagina.get("seccion"), set()).add(str(p.get("productId")))
            for item in p.get("items") or []:
                # Un producto con varios SKU de distinto precio puede salir en dos
                # rangos de precio: se conserva solo la primera aparición.
                if str(item.get("itemId")) in skus_vistos:
                    repetidos += 1
                    continue
                skus_vistos.add(str(item.get("itemId")))
                fila = _fila_base(clave, p, item, pagina, id_ejecucion)
                fila.update(mapear(p, item))
                filas.append(fila)
    manifiesto["productos_unicos"] = len(productos_vistos)
    manifiesto["productos_por_seccion"] = {s: len(v) for s, v in por_seccion.items()}
    # Solo cuenta como "no mostrado por el sitio" si el hueco se repitió en las dos
    # vueltas (en la misma página). Si salió en una sola vuelta, no se descuenta.
    no_mostrados = {}
    for (seccion, _), faltan in huecos.items():
        if len(faltan) >= 2 and min(faltan) > 0:
            no_mostrados[seccion] = no_mostrados.get(seccion, 0) + min(faltan)
    if huecos:
        manifiesto["no_mostrados_por_seccion"] = no_mostrados
    manifiesto["skus"] = len(filas)
    manifiesto["skus_repetidos_descartados"] = repetidos
    return _a_dataframe(filas, manifiesto)


# ---------------------------------------------------------------------------
# Una función por comercio (todas devuelven las columnas de COLUMNAS)
# ---------------------------------------------------------------------------
def scrape_larebaja(id_ejecucion: str, carpeta: Path, sin_conexion: bool = False,
                    sesion=None) -> pd.DataFrame:
    def mapear(p, item):
        return {
            "laboratorio_original": _spec(p, "Proveedor", "ML_Laboratorio", "ML_Fabricante"),
            "principio_activo_original": _spec(p, "Principio activo"),
            "presentacion_original": _spec(p, "Presentacionunidadmedida", "Presentacion"),
            "unidad_medida_original": _spec(p, "Unidadmedida"),
            "cantidad_original": _spec(p, "Cantidadunidadesmedida"),
            "via_original": None,
            "requiere_formula": _spec(p, "Producto RX"),
        }
    return _recolectar("larebaja", mapear, id_ejecucion, carpeta, sin_conexion, sesion)


def scrape_locatel(id_ejecucion: str, carpeta: Path, sin_conexion: bool = False,
                   sesion=None) -> pd.DataFrame:
    def mapear(p, item):
        return {
            "laboratorio_original": None,          # Locatel no publica el laboratorio
            "principio_activo_original": (_spec(p, "Principio activo")
                                          or _principio_desde_descripcion(p.get("description"))),
            "presentacion_original": " | ".join(x for x in (item.get("name"), _spec(p, "Presentación")) if x),
            "unidad_medida_original": _spec(p, "PUM - Unidad de Medida"),
            "cantidad_original": _spec(p, "PUM - Medida"),
            "via_original": _via_desde_descripcion(p.get("description")),
            "requiere_formula": None,
        }
    return _recolectar("locatel", mapear, id_ejecucion, carpeta, sin_conexion, sesion)


def scrape_cafam(id_ejecucion: str, carpeta: Path, sin_conexion: bool = False,
                 sesion=None) -> pd.DataFrame:
    def mapear(p, item):
        return {
            "laboratorio_original": _spec(p, "Laboratorio fabricante"),
            "principio_activo_original": _spec(p, "Principio activo"),
            "presentacion_original": " | ".join(x for x in (_spec(p, "Presentación"), _spec(p, "Unidades por Empaque"),
                                                            _spec(p, "Contenido")) if x),
            "unidad_medida_original": _spec(p, "Unidad de medida"),
            "cantidad_original": _spec(p, "Cantidad por unidad de medida"),
            "via_original": _spec(p, "Vía de administración"),
            "requiere_formula": _spec(p, "Requiere prescripción médica"),
        }
    return _recolectar("cafam", mapear, id_ejecucion, carpeta, sin_conexion, sesion,
                       descargar=_descargar_cafam)


def scrape_colsubsidio(id_ejecucion: str, carpeta: Path, sin_conexion: bool = False,
                       sesion=None) -> pd.DataFrame:
    def mapear(p, item):
        return {
            "laboratorio_original": None,
            "principio_activo_original": " ".join(x for x in (_spec(p, "Principio Activo"), _spec(p, "Concentración")) if x) or None,
            "presentacion_original": _spec(p, "Presentación"),
            "unidad_medida_original": "UNIDAD" if _spec(p, "Unidades") else None,
            "cantidad_original": _spec(p, "Unidades"),
            "via_original": None,
            "requiere_formula": _spec(p, "Producto Formulado", "Aplica Formula"),
        }
    return _recolectar("colsubsidio", mapear, id_ejecucion, carpeta, sin_conexion, sesion)


FUNCIONES = {
    "larebaja": scrape_larebaja,
    "locatel": scrape_locatel,
    "cafam": scrape_cafam,
    "colsubsidio": scrape_colsubsidio,
}


# ---------------------------------------------------------------------------
# Base de datos SQLite
# ---------------------------------------------------------------------------
ESQUEMA = """
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS comercios (
    id_comercio  TEXT PRIMARY KEY,
    nombre       TEXT NOT NULL,
    url_base     TEXT NOT NULL,
    plataforma   TEXT,
    secciones    TEXT,            -- secciones del sitio recorridas (JSON)
    robots       TEXT             -- resultado de la revisión de robots.txt
);

CREATE TABLE IF NOT EXISTS ejecuciones (
    id_ejecucion          TEXT NOT NULL,
    id_comercio           TEXT NOT NULL REFERENCES comercios(id_comercio),
    inicio                TEXT NOT NULL,     -- ISO 8601, hora de Colombia
    fin                   TEXT,
    modo                  TEXT NOT NULL,     -- 'en_linea' o 'sin_conexion'
    paginas               INTEGER,
    total_reportado_sitio INTEGER,           -- suma de la cabecera 'resources' de cada sección
    productos_recorridos  INTEGER,           -- productos únicos descargados
    registros_categoria   INTEGER,           -- SKUs que quedaron dentro de la categoría
    excluidos             INTEGER,           -- antiinfecciosos excluidos (uso local, etc.)
    estado                TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
    detalle               TEXT,
    PRIMARY KEY (id_ejecucion, id_comercio)
);

-- Un anuncio = un SKU de un comercio (con sus campos originales y estandarizados)
CREATE TABLE IF NOT EXISTS anuncios (
    id_comercio               TEXT NOT NULL REFERENCES comercios(id_comercio),
    sku_comercio              TEXT NOT NULL,
    id_producto_comercio      TEXT,
    ean                       TEXT,
    ean_valido                INTEGER,
    nombre_original           TEXT NOT NULL,
    marca_original            TEXT,
    laboratorio_original      TEXT,
    principio_activo_original TEXT,
    presentacion_original     TEXT,
    via_original              TEXT,
    requiere_formula          TEXT,
    categoria_comercio        TEXT,
    url_producto              TEXT,
    -- campos estandarizados (no reemplazan a los originales)
    principio_activo_std      TEXT,
    subcategoria              TEXT,
    marca_std                 TEXT,
    laboratorio_std           TEXT,
    forma_std                 TEXT,
    concentracion_std         TEXT,
    unidades                  INTEGER,
    contenido                 REAL,
    unidad_contenido          TEXT,
    fuente_cantidad           TEXT,
    -- correspondencia entre comercios (se llena con limpieza.cargar_correspondencia)
    id_producto               TEXT REFERENCES productos(id_producto),
    comparable                INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (id_comercio, sku_comercio)
);

-- Identificador común de las presentaciones comparables
CREATE TABLE IF NOT EXISTS productos (
    id_producto      TEXT PRIMARY KEY,
    descripcion      TEXT NOT NULL,
    principio_activo TEXT,
    concentracion    TEXT,
    forma            TEXT,
    laboratorio      TEXT,
    unidades         INTEGER,
    contenido_ml     REAL
);

CREATE TABLE IF NOT EXISTS correspondencia (
    id_producto  TEXT NOT NULL REFERENCES productos(id_producto),
    id_comercio  TEXT NOT NULL,
    sku_comercio TEXT NOT NULL,
    regla        TEXT,
    PRIMARY KEY (id_comercio, sku_comercio),
    FOREIGN KEY (id_comercio, sku_comercio) REFERENCES anuncios(id_comercio, sku_comercio)
);

-- Una observación = el precio de un SKU en un momento dado
CREATE TABLE IF NOT EXISTS observaciones (
    id_observacion  INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ejecucion    TEXT NOT NULL,
    id_comercio     TEXT NOT NULL,
    sku_comercio    TEXT NOT NULL,
    fecha_consulta  TEXT NOT NULL,           -- ISO 8601, hora de Colombia (obligatoria)
    precio          REAL,                    -- COP; NULL si no hay precio publicado
    precio_lista    REAL,                    -- precio antes del descuento, si existe
    promocion       INTEGER,                 -- 1 si precio < precio_lista
    disponible      INTEGER,
    url_extraccion  TEXT NOT NULL,           -- petición de la que salió el precio
    texto_original  TEXT NOT NULL,           -- anuncio tal como se publicó (JSON)
    UNIQUE (id_comercio, sku_comercio, fecha_consulta),
    FOREIGN KEY (id_comercio, sku_comercio) REFERENCES anuncios(id_comercio, sku_comercio),
    FOREIGN KEY (id_ejecucion, id_comercio) REFERENCES ejecuciones(id_ejecucion, id_comercio)
);

-- Productos descartados por no pertenecer a la categoría según su delimitación
CREATE TABLE IF NOT EXISTS excluidos (
    id_ejecucion         TEXT NOT NULL,
    id_comercio          TEXT NOT NULL,
    sku_comercio         TEXT NOT NULL,
    nombre_original      TEXT,
    principio_activo     TEXT,
    categoria_comercio   TEXT,
    motivo               TEXT NOT NULL,
    PRIMARY KEY (id_ejecucion, id_comercio, sku_comercio)
);

-- Vista con todos los campos mínimos que pide el taller, una fila por observación
CREATE VIEW IF NOT EXISTS v_observaciones AS
SELECT o.id_ejecucion,
       c.nombre                AS comercio,
       o.id_comercio,
       a.id_producto,
       p.descripcion           AS producto,
       a.sku_comercio,
       a.ean,
       a.nombre_original,
       a.marca_std             AS marca,
       a.laboratorio_std       AS laboratorio,
       a.subcategoria,
       a.principio_activo_std  AS principio_activo,
       a.concentracion_std     AS concentracion,
       a.forma_std             AS forma,
       a.unidades,
       a.contenido,
       a.unidad_contenido,
       o.precio,
       o.precio_lista,
       o.promocion,
       o.disponible,
       a.url_producto,
       o.url_extraccion,
       o.texto_original,
       o.fecha_consulta,
       a.comparable
FROM observaciones o
JOIN anuncios  a ON a.id_comercio = o.id_comercio AND a.sku_comercio = o.sku_comercio
JOIN comercios c ON c.id_comercio = o.id_comercio
LEFT JOIN productos p ON p.id_producto = a.id_producto;
"""


def conectar(db_path: Path | str = DB_PATH) -> sqlite3.Connection:
    Path(db_path).parent.mkdir(parents=True, exist_ok=True)
    con = sqlite3.connect(db_path)
    con.executescript(ESQUEMA)
    return con


def _registrar_comercios(con, claves) -> None:
    for clave in claves:
        cfg = COMERCIOS[clave]
        con.execute(
            "INSERT OR REPLACE INTO comercios VALUES (?, ?, ?, ?, ?, ?)",
            (clave, cfg["nombre"], cfg["url_base"], cfg.get("plataforma", "VTEX"),
             json.dumps(cfg["secciones"], ensure_ascii=False), cfg["robots"]))


def _py(v):
    """Convierte valores de pandas/numpy a tipos que entiende sqlite3 (NaN -> NULL)."""
    if v is None:
        return None
    try:
        if pd.isna(v):
            return None
    except (TypeError, ValueError):
        pass
    if isinstance(v, bool):
        return int(v)
    return v.item() if hasattr(v, "item") else v


def _insertar(con, df: pd.DataFrame, reemplazar: bool = False) -> int:
    """
    Inserta anuncios (upsert), observaciones (sin duplicar) y excluidos.
    Con reemplazar=True (reproceso sin conexión de una ejecución que ya estaba en la
    base) primero borra las observaciones y los excluidos de ESA ejecución y comercio:
    todo eso sale de raw/, así que se vuelve a generar igual, pero con las reglas de
    la categoría actualizadas. Las demás ejecuciones no se tocan.
    """
    incluidos = df[df["incluido"]]
    excluidos = _excluidos(df)
    if reemplazar and len(df):
        clave = (df["id_ejecucion"].iloc[0], df["comercio"].iloc[0])
        con.execute("DELETE FROM observaciones WHERE id_ejecucion = ? AND id_comercio = ?", clave)
        con.execute("DELETE FROM excluidos WHERE id_ejecucion = ? AND id_comercio = ?", clave)

    columnas_anuncio = ["id_comercio", "sku_comercio", "id_producto_comercio", "ean", "ean_valido",
                        "nombre_original", "marca_original", "laboratorio_original",
                        "principio_activo_original", "presentacion_original", "via_original",
                        "requiere_formula", "categoria_comercio", "url_producto",
                        "principio_activo_std", "subcategoria", "marca_std", "laboratorio_std",
                        "forma_std", "concentracion_std", "unidades", "contenido",
                        "unidad_contenido", "fuente_cantidad"]
    actualizar = ", ".join(f"{c} = excluded.{c}" for c in columnas_anuncio[2:])
    sql_anuncio = (f"INSERT INTO anuncios ({', '.join(columnas_anuncio)}) "
                   f"VALUES ({', '.join('?' * len(columnas_anuncio))}) "
                   f"ON CONFLICT(id_comercio, sku_comercio) DO UPDATE SET {actualizar}")
    datos = incluidos.rename(columns={"comercio": "id_comercio"})
    for fila in datos[columnas_anuncio].itertuples(index=False):
        con.execute(sql_anuncio, [_py(v) for v in fila])

    antes = con.total_changes
    con.executemany(
        """INSERT OR IGNORE INTO observaciones
           (id_ejecucion, id_comercio, sku_comercio, fecha_consulta, precio, precio_lista,
            promocion, disponible, url_extraccion, texto_original)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
        [tuple(_py(v) for v in fila) for fila in datos[
            ["id_ejecucion", "id_comercio", "sku_comercio", "fecha_consulta", "precio",
             "precio_lista", "promocion", "disponible", "url_extraccion", "texto_original"]
        ].itertuples(index=False)])
    nuevas = con.total_changes - antes

    con.executemany(
        "INSERT OR IGNORE INTO excluidos VALUES (?, ?, ?, ?, ?, ?, ?)",
        [tuple(_py(v) for v in fila) for fila in excluidos[
            ["id_ejecucion", "comercio", "sku_comercio", "nombre_original",
             "principio_activo_std", "categoria_comercio", "motivo_exclusion"]
        ].itertuples(index=False)])
    return nuevas


def _excluidos(df: pd.DataFrame) -> pd.DataFrame:
    """Antiinfecciosos (o productos de secciones antiinfecciosas) que quedan fuera."""
    return df[~df["incluido"].astype(bool) & (df["motivo_exclusion"] != "no es antiinfeccioso")]


def _procesar(df: pd.DataFrame) -> pd.DataFrame:
    """Aplica la delimitación de la categoría y la estandarización."""
    if df.empty:
        for c in ("incluido", "motivo_exclusion", "principio_activo_std", "subcategoria", "ean_valido"):
            df[c] = pd.Series(dtype=object)
        return df
    clasif = [categoria.clasificar(pa, n, pr, v, sec) for pa, n, pr, v, sec in zip(
        df["principio_activo_original"], df["nombre_original"], df["presentacion_original"],
        df["via_original"], df["categoria_comercio"])]
    clasif = pd.DataFrame(clasif, index=df.index)
    df = pd.concat([df, clasif], axis=1)
    df["ean_valido"] = df["ean"].map(limpieza.ean_valido).astype(int)
    return limpieza.estandarizar(df)


def _estado(manifiesto: dict) -> str:
    """
    'completa' si no hubo errores y CADA sección trae todos los productos que dice el
    sitio. Se revisa sección por sección porque un mismo producto puede estar en dos
    secciones (entonces los productos distintos suman menos que los totales).
    En Cafam se descuentan los productos que el sitio cuenta pero no muestra
    (confirmados en las dos vueltas, ver _recolectar).
    """
    if manifiesto.get("productos_unicos", 0) == 0:
        return "fallida"
    por_seccion = manifiesto.get("productos_por_seccion", {})
    no_mostrados = manifiesto.get("no_mostrados_por_seccion", {})
    falta_en_seccion = any(por_seccion.get(s["nombre"], 0) < s["total_reportado"] - no_mostrados.get(s["nombre"], 0)
                           for s in manifiesto["secciones"])
    falta_en_total = (manifiesto["productos_unicos"]
                      < manifiesto.get("total_sitio", 0) - sum(no_mostrados.values()))
    if manifiesto["errores"] or falta_en_seccion or falta_en_total:
        return "parcial"
    return "completa"


# ---------------------------------------------------------------------------
# Función general
# ---------------------------------------------------------------------------
def ejecutar_recoleccion(comercios: list[str] | None = None, sin_conexion: bool = False,
                         db_path: Path | str = DB_PATH, raw_dir: Path | str = RAW_DIR,
                         id_ejecucion: str | None = None, verbose: bool = True) -> pd.DataFrame:
    """
    Ejecuta la recolección completa y la guarda en SQLite sin sobrescribir.

    En línea (por defecto): consulta cada comercio, guarda las respuestas en
        raw/<comercio>/<AAAA-MM-DD>/<id_ejecucion>/ y las inserta en la base.
    Sin conexión (sin_conexion=True): no consulta ningún sitio; vuelve a procesar
        todas las copias guardadas en raw/ (o solo las de `id_ejecucion`).

    Devuelve un DataFrame con el resumen de cada (ejecución, comercio).
    """
    comercios = comercios or COMERCIOS_POR_DEFECTO
    raw_dir = Path(raw_dir)
    con = conectar(db_path)
    _registrar_comercios(con, COMERCIOS.keys())

    tareas = []   # (id_ejecucion, clave, carpeta)
    if sin_conexion:
        for manifest in sorted(raw_dir.glob("*/*/*/manifest.json")):
            carpeta = manifest.parent
            clave, ejec = carpeta.parent.parent.name, carpeta.name
            if clave in comercios and (id_ejecucion is None or ejec == id_ejecucion):
                tareas.append((ejec, clave, carpeta))
        if not tareas:
            raise FileNotFoundError(f"No hay copias crudas en {raw_dir} para {comercios}")
    else:
        id_ejecucion = id_ejecucion or nuevo_id_ejecucion()
        fecha = id_ejecucion[1:5] + "-" + id_ejecucion[5:7] + "-" + id_ejecucion[7:9]
        tareas = [(id_ejecucion, clave, raw_dir / clave / fecha / id_ejecucion) for clave in comercios]

    resumen = []
    sesion = None if sin_conexion else _nueva_sesion()
    for ejec, clave, carpeta in tareas:
        inicio = ahora_iso()
        if verbose:
            print(f"[{inicio}] {clave}: {'leyendo raw/' if sin_conexion else 'consultando el sitio'}...", flush=True)
        try:
            df = FUNCIONES[clave](ejec, carpeta, sin_conexion=sin_conexion, sesion=sesion)
            manifiesto = df.attrs["manifiesto"]
            if not sin_conexion:
                manifiesto.update({"id_ejecucion": ejec, "inicio": inicio, "fin": ahora_iso()})
                manifiesto["estado"] = _estado(manifiesto)
                (carpeta / "manifest.json").write_text(
                    json.dumps(manifiesto, ensure_ascii=False, indent=1), encoding="utf-8")
            df = _procesar(df)
            fila = {
                "id_ejecucion": ejec, "id_comercio": clave,
                "inicio": manifiesto.get("inicio", inicio), "fin": manifiesto.get("fin", ahora_iso()),
                "modo": "en_linea" if not sin_conexion else "sin_conexion",
                "paginas": manifiesto["paginas"],
                "total_reportado_sitio": (manifiesto.get("total_sitio")
                                          or sum(s["total_reportado"] for s in manifiesto["secciones"])),
                "productos_recorridos": manifiesto["productos_unicos"],
                "registros_categoria": int(df["incluido"].sum()) if len(df) else 0,
                "excluidos": len(_excluidos(df)) if len(df) else 0,
                "estado": manifiesto.get("estado") or _estado(manifiesto),
                "detalle": json.dumps({"errores": manifiesto["errores"][:20]}, ensure_ascii=False),
            }
        except Exception as e:
            fila = {"id_ejecucion": ejec, "id_comercio": clave, "inicio": inicio, "fin": ahora_iso(),
                    "modo": "en_linea" if not sin_conexion else "sin_conexion", "paginas": None,
                    "total_reportado_sitio": None, "productos_recorridos": 0,
                    "registros_categoria": 0, "excluidos": 0, "estado": "fallida",
                    "detalle": repr(e)}
        # La fila original de una ejecución en línea no se reemplaza al reprocesar sin conexión
        verbo = "INSERT OR IGNORE" if sin_conexion else "INSERT OR REPLACE"
        con.execute(f"{verbo} INTO ejecuciones VALUES ({', '.join('?' * 12)})", list(fila.values()))
        if sin_conexion and fila["estado"] != "fallida":
            # se conserva la fila original, pero los conteos de la categoría se
            # actualizan con las reglas de ahora
            con.execute("UPDATE ejecuciones SET registros_categoria = ?, excluidos = ? "
                        "WHERE id_ejecucion = ? AND id_comercio = ?",
                        (fila["registros_categoria"], fila["excluidos"], ejec, clave))
        if fila["estado"] != "fallida":
            try:
                fila["observaciones_nuevas"] = _insertar(con, df, reemplazar=sin_conexion)
            except Exception as e:   # error al guardar: la ejecución queda como fallida
                con.rollback()
                fila.update({"estado": "fallida", "detalle": f"error al insertar: {e!r}"})
                con.execute(f"INSERT OR REPLACE INTO ejecuciones VALUES ({', '.join('?' * 12)})",
                            [fila[k] for k in list(fila)[:12]])
        con.commit()
        resumen.append(fila)
        if verbose:
            print(f"    -> {fila['estado']}: {fila['productos_recorridos']} productos recorridos "
                  f"de {fila['total_reportado_sitio']} reportados; {fila['registros_categoria']} "
                  f"SKUs en la categoría", flush=True)
    if sin_conexion:
        # Un anuncio sin ninguna observación viene de una versión anterior de las reglas
        # (antes estaba dentro de la categoría y ya no). Se borra para que la base quede
        # igual a la que se reconstruye desde cero con raw/.
        con.execute("""DELETE FROM anuncios WHERE comparable = 0 AND NOT EXISTS (
                           SELECT 1 FROM observaciones o
                           WHERE o.id_comercio = anuncios.id_comercio
                             AND o.sku_comercio = anuncios.sku_comercio)""")
        con.commit()
    con.close()
    return pd.DataFrame(resumen)


def reprocesar_sin_conexion(**kwargs) -> pd.DataFrame:
    """Atajo: ejecutar_recoleccion(sin_conexion=True, ...)."""
    return ejecutar_recoleccion(sin_conexion=True, **kwargs)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Scraper del Taller 1 (antiinfecciosos sistémicos)")
    parser.add_argument("--sin-conexion", action="store_true",
                        help="reprocesa las copias de raw/ sin consultar los sitios")
    parser.add_argument("--comercios", nargs="+", default=COMERCIOS_POR_DEFECTO,
                        choices=list(COMERCIOS), help="comercios a recolectar")
    parser.add_argument("--db", default=str(DB_PATH), help="ruta de la base SQLite")
    parser.add_argument("--id-ejecucion", default=None,
                        help="en modo sin conexión, reprocesa solo esta ejecución")
    args = parser.parse_args()
    print(ejecutar_recoleccion(args.comercios, args.sin_conexion, args.db,
                               id_ejecucion=args.id_ejecucion).to_string(index=False))
