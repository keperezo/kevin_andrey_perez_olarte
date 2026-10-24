# -*- coding: utf-8 -*-
"""
scraper.py - Taller 1, Minería de Datos (UNAL)
Comparador de precios de DERMOCOSMÉTICOS (cuidado facial) en Colombia.

COMERCIOS Y ESTRATEGIA DE EXTRACCIÓN (todo con peticiones HTTP directas, sin navegador)
--------------------------------------------------------------------------------------
La Rebaja  (VTEX)    API pública de catálogo:
                     GET larebajavirtual.com/api/catalog_system/pub/products/search
                     filtro fq=C:/8/42/<id>/ (Dermocosmética > Rostro > subcategoría),
                     paginado de 50 en 50 con _from/_to. El total sale del encabezado
                     'resources' ("0-49/55") y se compara con lo recibido.
Dermatodo  (Shopify) GET dermatodo.com/collections/<handle>/products.json (250 por página).
                     La categoría equivalente es la colección 'facial'; las subcolecciones
                     (limpiadores, sueros, tónicos...) se usan para asignar la subcategoría y
                     detectar exclusiones (solares, maquillaje, corporal). El total declarado
                     sale de /collections.json (products_count).
Farmatodo  (Angular) La página trae solo 24 productos; el resto sale del buscador Algolia que usa
                     la propia web (POST api-search.farmatodo.com, índice products-colombia).
                     El buscador devuelve como máximo 80 resultados por consulta, así que cada
                     categoría se divide por rangos de precio hasta que cada tramo tenga menos de
                     80. La clave pública del buscador se lee de la propia página en cada ejecución
                     y SOLO se usa en memoria: nunca se imprime ni se guarda (ni en raw/ ni en la BD).

COPIAS CRUDAS Y MODO SIN CONEXIÓN
---------------------------------
Cada respuesta se guarda en raw/<comercio>/<AAAA-MM-DD>/<id_ejecucion>/<nombre>.json.gz
junto con sus metadatos (URL, parámetros, estado HTTP, fecha y hora de consulta).
Con offline=True se reprocesan esas copias sin consultar los sitios.

USO
---
    import scraper
    scraper.recolectar_todo()                     # en línea: consulta, guarda raw/ e inserta en SQLite
    scraper.recolectar_todo(offline=True)         # sin conexión: reprocesa la última copia de raw/
    python scraper.py --comercios "La Rebaja"     # desde la terminal
    Defina su correo en la variable de entorno SCRAPER_CONTACTO (va en el User-Agent).

Las observaciones nunca se sobrescriben: INSERT OR IGNORE con UNIQUE(comercio, sku, fecha_consulta).
Los precios ausentes quedan como NULL (nunca como 0).
"""
from __future__ import annotations

import argparse
import gzip
import json
import os
import re
import sqlite3
import sys
import time
from collections import Counter
from datetime import datetime, timedelta, timezone
from urllib.parse import urlencode

import requests

# --------------------------------------------------------------------------- configuración
TZ_CO = timezone(timedelta(hours=-5))          # Colombia no tiene horario de verano
CONTACTO = os.environ.get("SCRAPER_CONTACTO", "tu_correo@unal.edu.co")
USER_AGENT = f"Taller-MineriaDatos-UNAL (contacto: {CONTACTO})"
PAUSA_SEG = 1.0                                # pausa entre peticiones reales
TIMEOUT = 60
COMERCIOS = ("La Rebaja", "Dermatodo", "Farmatodo")
SITIOS = {
    "La Rebaja": "https://www.larebajavirtual.com",
    "Dermatodo": "https://dermatodo.com",
    "Farmatodo": "https://www.farmatodo.com.co",
}

COLUMNAS = [
    "comercio", "sku_comercio", "codigo_barras", "id_producto_comercio", "url",
    "titulo_original", "marca_original", "presentacion_original", "texto_original",
    "categoria_origen", "categorias_origen", "en_alcance", "motivo_exclusion",
    "precio_lista", "precio_vigente", "precio_prime", "promocion", "precio_promocional",
    "tiendas_con_oferta", "disponible", "es_marketplace", "contenido_num", "unidad",
    "nota_calidad", "fecha_consulta",
]

# Exclusión por palabras clave (delimitación de la categoría: sin maquillaje ni protectores solares).
PATRON_EXCLUSION = re.compile(
    r"protector(?:es)?\s+solar|fotoprotec|bloqueador|sunscreen|\bsolar\b|anthelios|uvmune|heliocare|maquillaj|"
    r"base\s+(?:l[ií]quida|de\s+maquillaje)|polvo\s+compacto|l[aá]piz|labial|"
    r"p[eé]ta[ñn]ina|r[ií]mel|m[aá]scara\s+de\s+pesta|"
    r"champ[uú]|shampoo|acondicionador\s+capilar|desodorante|antitranspirante",
    re.I,
)
PATRON_KIT = re.compile(r"\b(kit|combo|pack|set)\b", re.I)
PATRON_CORPORAL = re.compile(r"corporal|ducha|\bcuerpo\b", re.I)
PATRON_FACIAL = re.compile(r"facial|rostro|\bcara\b", re.I)


def motivo_texto(titulo: str, extra: str = "") -> str | None:
    """Devuelve el motivo de exclusión según palabras clave, o None si queda dentro del alcance."""
    t = f"{titulo or ''} {extra or ''}"
    m = PATRON_EXCLUSION.search(t)
    if m:
        return f"palabra clave de exclusión: {m.group(0).lower()}"
    m = PATRON_CORPORAL.search(t)
    if m and not PATRON_FACIAL.search(t):
        return f"producto corporal: {m.group(0).lower()}"
    return None


# --------------------------------------------------------------------------- utilidades
def ahora_iso() -> str:
    """Fecha y hora de Colombia en ISO 8601, p. ej. 2026-10-03T10:35:00-05:00."""
    return datetime.now(TZ_CO).isoformat(timespec="seconds")


def _slug(nombre: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", nombre.lower())


def _num(x):
    """Convierte a float; devuelve None si falta o es <= 0 (un precio 0 no es un precio)."""
    if x is None:
        return None
    if isinstance(x, (int, float)):
        v = float(x)
    else:
        s = str(x).replace("$", "").replace(" ", "").strip()
        if not s:
            return None
        if re.fullmatch(r"\d+(\.\d+)?", s):
            v = float(s)
        elif re.fullmatch(r"\d{1,3}(\.\d{3})+(,\d+)?", s):
            v = float(s.replace(".", "").replace(",", "."))
        else:
            try:
                v = float(s.replace(",", ""))
            except ValueError:
                return None
    return v if v > 0 else None


def _ean(x):
    if x is None:
        return None
    s = re.sub(r"\D", "", str(x))
    return s if 8 <= len(s) <= 14 else None


def _spec(p: dict, nombre: str):
    v = p.get(nombre)
    if isinstance(v, list):
        v = next((x for x in v if x not in (None, "")), None)
    return v


def _unico(partes) -> str:
    vistos = dict.fromkeys(str(x).strip() for x in partes if x not in (None, "") and str(x).strip())
    return " | ".join(vistos)


_UNIDADES = {
    "ml": ("mL", 1.0), "mililitro": ("mL", 1.0), "mililitros": ("mL", 1.0), "cc": ("mL", 1.0),
    "l": ("mL", 1000.0), "lt": ("mL", 1000.0), "litro": ("mL", 1000.0), "litros": ("mL", 1000.0),
    "g": ("g", 1.0), "gr": ("g", 1.0), "grs": ("g", 1.0), "gramo": ("g", 1.0), "gramos": ("g", 1.0),
    "kg": ("g", 1000.0), "kilo": ("g", 1000.0), "kilos": ("g", 1000.0), "mg": ("g", 0.001),
    "und": ("und", 1.0), "unds": ("und", 1.0), "unidad": ("und", 1.0), "unidades": ("und", 1.0),
    "u": ("und", 1.0),
}
_RE_CONTENIDO = re.compile(
    r"(\d+(?:[.,]\d+)?)\s*(mililitros?|litros?|gramos?|kilos?|unidades|unidad|unds?|und|"
    r"ml|cc|lt|kg|mg|grs?|g|l|u)\b",
    re.I,
)


def normalizar_contenido(valor, unidad):
    """(valor, unidad) -> (cantidad en unidad base, unidad base). Bases: mL, g, und."""
    v = _num(valor)
    if v is None or not unidad:
        return None, None
    clave = str(unidad).strip().lower().rstrip(".")
    if clave not in _UNIDADES:
        return None, None
    base, factor = _UNIDADES[clave]
    return round(v * factor, 4), base


def extraer_contenido_texto(texto):
    """Busca el contenido en un texto ("... | 473 ml", "POTE X 89 GRS"); usa la última coincidencia."""
    if not texto:
        return None, None
    ms = list(_RE_CONTENIDO.finditer(str(texto)))
    if not ms:
        return None, None
    m = ms[-1]
    return normalizar_contenido(m.group(1).replace(",", "."), m.group(2))


def marcar_calidad(registros):
    """Anota en 'nota_calidad' los datos faltantes o dudosos (sirve para la pregunta 7 del taller)."""
    for r in registros:
        notas = [r["nota_calidad"]] if r.get("nota_calidad") else []
        if r.get("contenido_num") is None:
            notas.append("contenido no identificable")
        if not r.get("codigo_barras"):
            notas.append("sin código de barras")
        if PATRON_KIT.search(r.get("titulo_original") or ""):
            notas.append("kit/combo/paquete: no comparable por presentación individual")
        r["nota_calidad"] = " | ".join(notas) or None
    return registros


def nuevo_registro(**kw) -> dict:
    r = {c: None for c in COLUMNAS}
    r.update(kw)
    return r


# --------------------------------------------------------------------------- red y copias crudas
def _peticion(sesion, metodo, url, intentos=4, **kw):
    """Petición con reintentos y esperas crecientes (5, 10, 20 s) ante errores temporales del sitio."""
    ultimo = None
    for i in range(intentos):
        try:
            r = sesion.request(metodo, url, timeout=TIMEOUT, **kw)
            if r.status_code in (429, 500, 502, 503, 504):
                ultimo = f"estado {r.status_code}"
            else:
                return r
        except requests.RequestException as e:
            ultimo = type(e).__name__
        if i < intentos - 1:
            time.sleep(5 * (2 ** i))
    desc = url.split("?")[0]
    fq = (kw.get("params") or {}).get("fq")
    if fq:
        desc += f" (fq={fq})"
    raise RuntimeError(f"Falló la petición tras {intentos} intentos ({ultimo}): {desc}")


def _ubicar_run_offline(raw_dir, comercio, id_pedido):
    base = os.path.join(raw_dir, _slug(comercio))
    if not os.path.isdir(base):
        raise FileNotFoundError(f"No hay copias crudas de {comercio} en {base}")
    cand = []
    for fecha in sorted(os.listdir(base)):
        pf = os.path.join(base, fecha)
        if not os.path.isdir(pf):
            continue
        for rid in sorted(os.listdir(pf)):
            if os.path.isdir(os.path.join(pf, rid)):
                cand.append((rid, os.path.join(pf, rid)))
    if id_pedido:
        cand = [c for c in cand if c[0] == id_pedido]
    if not cand:
        raise FileNotFoundError(f"No hay copias crudas de {comercio}" + (f" para {id_pedido}" if id_pedido else ""))
    return sorted(cand)[-1]


class Capturador:
    """Hace (o relee) las peticiones de un comercio y guarda cada respuesta en raw/."""

    def __init__(self, comercio, id_ejec, raw_dir="raw", offline=False, pausa=PAUSA_SEG):
        self.comercio, self.offline, self.pausa = comercio, offline, pausa
        self.n_reales = 0
        self.fecha_inicio = ahora_iso()
        if offline:
            self.sesion = None
            self.id_ejec, self.dir = _ubicar_run_offline(raw_dir, comercio, id_ejec)
            meta = os.path.join(self.dir, "_ejecucion.json")
            if os.path.exists(meta):
                with open(meta, encoding="utf-8") as f:
                    self.fecha_inicio = json.load(f).get("fecha_inicio", self.fecha_inicio)
        else:
            self.sesion = requests.Session()
            self.sesion.headers.update({"User-Agent": USER_AGENT, "Accept-Language": "es-CO,es;q=0.9"})
            self.id_ejec = id_ejec
            m = re.fullmatch(r"EJEC_(\d{4})(\d{2})(\d{2})_\d{6}", id_ejec)
            fecha = f"{m.group(1)}-{m.group(2)}-{m.group(3)}" if m else datetime.now(TZ_CO).strftime("%Y-%m-%d")
            self.dir = os.path.join(raw_dir, _slug(comercio), fecha, id_ejec)
            os.makedirs(self.dir, exist_ok=True)

    def pausar(self):
        if self.n_reales:
            time.sleep(self.pausa)

    def obtener(self, nombre, fetch):
        """fetch() -> (datos_json, meta). Devuelve (datos, meta) con meta['fecha_consulta']."""
        ruta = os.path.join(self.dir, nombre + ".json.gz")
        if self.offline:
            with gzip.open(ruta, "rt", encoding="utf-8") as f:
                doc = json.load(f)
            return doc["data"], doc["_meta"]
        self.pausar()
        datos, meta = fetch()
        meta = dict(meta)
        meta["fecha_consulta"] = ahora_iso()
        meta["comercio"] = self.comercio
        with gzip.open(ruta, "wt", encoding="utf-8") as f:
            json.dump({"_meta": meta, "data": datos}, f, ensure_ascii=False)
        self.n_reales += 1
        return datos, meta

    def cerrar(self, estado):
        if self.offline:
            return
        with open(os.path.join(self.dir, "_ejecucion.json"), "w", encoding="utf-8") as f:
            json.dump({"id_ejecucion": self.id_ejec, "comercio": self.comercio,
                       "fecha_inicio": self.fecha_inicio, "fecha_fin": ahora_iso(),
                       "estado": estado}, f, ensure_ascii=False, indent=2)


# =========================================================================== LA REBAJA (VTEX)
LR_BASE = SITIOS["La Rebaja"]
# id -> (nombre, en_alcance, ruta VTEX). Las categorías fuera de alcance también se consultan
# para documentar cuántos productos se excluyeron.
LR_CATEGORIAS = {
    224: ("Rostro > Limpieza", 1, "8/42/224"),
    461: ("Rostro > Acné", 1, "8/42/461"),
    462: ("Rostro > Antiedad", 1, "8/42/462"),
    463: ("Rostro > Despigmentante", 1, "8/42/463"),
    464: ("Rostro > Humectación", 1, "8/42/464"),
    225: ("Rostro > Maquillaje", 0, "8/42/225"),
    466: ("Solares > Protector solar adultos", 0, "8/465/466"),
    467: ("Solares > Protector solar niños", 0, "8/465/467"),
}


def _registro_larebaja(p, item, cat_nombre, cat_alcance, t):
    oferta = ((item.get("sellers") or [{}])[0] or {}).get("commertialOffer", {}) or {}
    vigente, lista = _num(oferta.get("Price")), _num(oferta.get("ListPrice"))
    if lista is None:
        lista = vigente
    promo = 1 if (vigente and lista and vigente < lista) else 0
    cant = _spec(p, "Cantidadunidadesmedida")
    contenido, unidad = normalizar_contenido(cant, _spec(p, "Unidadmedida"))
    pres = _spec(p, "Presentacionunidadmedida")
    if contenido is None:
        contenido, unidad = extraer_contenido_texto(pres)
    if contenido is None:
        contenido, unidad = extraer_contenido_texto(p.get("productName"))
    disp = oferta.get("AvailableQuantity")
    titulo = p.get("productName")
    return nuevo_registro(
        comercio="La Rebaja", sku_comercio=str(item.get("itemId")),
        codigo_barras=_ean(item.get("ean")) or _ean(_spec(p, "ML_EAN")),
        id_producto_comercio=str(p.get("productId")), url=p.get("link"),
        titulo_original=titulo, marca_original=p.get("brand"), presentacion_original=pres,
        texto_original=_unico([titulo, item.get("name"), pres, p.get("description"),
                               _spec(p, "DescripcionContenido")]),
        categoria_origen=cat_nombre, categorias_origen=cat_nombre,
        en_alcance=1 if cat_alcance else 0,
        motivo_exclusion=None if cat_alcance else f"categoría fuera de alcance: {cat_nombre}",
        precio_lista=lista, precio_vigente=vigente, promocion=promo,
        precio_promocional=vigente if promo else None,
        disponible=None if disp is None else int(disp > 0), es_marketplace=0,
        contenido_num=contenido, unidad=unidad, fecha_consulta=t,
    )


def scrape_larebaja(cap: Capturador) -> dict:
    registros, resumen, completa, declarados = {}, [], True, 0
    for cid, (nombre, alcance, ruta) in LR_CATEGORIAS.items():
        desde, total, recibidos = 0, None, 0
        try:
            while True:
                def fetch(desde=desde, ruta=ruta, cid=cid):
                    params = {"fq": f"C:/{ruta}/", "_from": desde, "_to": desde + 49}
                    r = _peticion(cap.sesion, "GET", LR_BASE + "/api/catalog_system/pub/products/search",
                                  params=params, headers={"Accept": "application/json"})
                    if r.status_code not in (200, 206):
                        raise RuntimeError(f"estado {r.status_code} en la categoría {cid}")
                    return r.json(), {"url": LR_BASE + "/api/catalog_system/pub/products/search",
                                      "metodo": "GET", "parametros": params, "status": r.status_code,
                                      "resources": r.headers.get("resources")}
                datos, meta = cap.obtener(f"categoria_{cid}_desde_{desde}", fetch)
                if total is None:
                    res = meta.get("resources") or ""
                    total = int(res.split("/")[-1]) if "/" in res else len(datos)
                recibidos += len(datos)
                for p in datos:
                    for item in p.get("items", []):
                        sku = str(item.get("itemId"))
                        if sku in registros:
                            r0 = registros[sku]
                            r0["categorias_origen"] += " | " + nombre
                            if alcance and not r0["en_alcance"]:
                                r0.update(en_alcance=1, motivo_exclusion=None, categoria_origen=nombre)
                            continue
                        registros[sku] = _registro_larebaja(p, item, nombre, alcance, meta["fecha_consulta"])
                desde += 50
                if not datos or desde >= total:
                    break
        except Exception as e:       # una categoría que falla no tumba a las demás
            completa = False
            resumen.append(f"{nombre}: ERROR ({str(e)[:160]}); recibidos {recibidos}")
            continue
        resumen.append(f"{nombre}: sitio {total}, recibidos {recibidos}")
        if recibidos != total:
            completa = False
        declarados += total
    if not registros:
        raise RuntimeError("La Rebaja no entregó ninguna categoría. " + "; ".join(resumen))
    for r in registros.values():   # exclusión adicional por palabras clave
        if r["en_alcance"]:
            mot = motivo_texto(r["titulo_original"])
            if mot:
                r.update(en_alcance=0, motivo_exclusion=mot)
    return {"registros": marcar_calidad(list(registros.values())), "declarados": declarados,
            "estado": "completa" if completa else "parcial",
            "detalle": "Cifra declarada = suma por categoría (un producto puede estar en varias). "
                       + "; ".join(resumen)}


# =========================================================================== DERMATODO (Shopify)
DT_BASE = SITIOS["Dermatodo"]
DT_FACIAL = "facial"
DT_SUBCOLECCIONES = {   # handle -> subcategoría de origen
    "limpiadores": "Limpiadores", "limpiador-facial": "Limpiadores", "desmaquillantes": "Limpiadores",
    "exfoliantes": "Limpiadores",
    "sueros": "Sérums", "sueros-1": "Sérums",
    "tonicos": "Tónicos",
    "contorno-de-ojos": "Contorno de ojos",
    "acne": "Acné", "seborreguladores": "Acné",
    "manchas": "Manchas", "facial-despigmentantes": "Manchas", "antimanchas": "Manchas",
    "antiedad": "Antiedad", "antiedad-1": "Antiedad",
    "hidratantes-faciales": "Cremas y cuidado facial", "reparador-facial": "Cremas y cuidado facial",
    "facial-calmantes": "Cremas y cuidado facial",
}
DT_EXCLUIR = {"fotoproteccion", "proteccion-solar", "maquillaje", "bases", "correctores", "polvos",
              "maquillaje-acne", "maquillaje-antiedad", "maquillaje-despigmentantes", "labios"}
# Nota: las colecciones corporales de Dermatodo incluyen limpiadores faciales (p. ej. Cetaphil Piel Grasa),
# por eso NO se usan para excluir; la categoría es la colección 'facial' del sitio.
_JSON = {"Accept": "application/json"}


def _dt_coleccion(cap: Capturador, handle: str) -> list:
    """Devuelve [(producto, fecha_consulta)] de una colección, recorriendo las páginas."""
    productos, pag = [], 1
    while True:
        def fetch(pag=pag):
            url = f"{DT_BASE}/collections/{handle}/products.json"
            params = {"limit": 250, "page": pag}
            r = _peticion(cap.sesion, "GET", url, params=params, headers=_JSON)
            if r.status_code == 404:
                return {"products": []}, {"url": url, "metodo": "GET", "parametros": params, "status": 404}
            if r.status_code != 200:
                raise RuntimeError(f"estado {r.status_code} en la colección {handle}")
            return r.json(), {"url": url, "metodo": "GET", "parametros": params, "status": 200}
        datos, meta = cap.obtener(f"coleccion_{handle}_p{pag}", fetch)
        lote = datos.get("products", [])
        productos += [(p, meta["fecha_consulta"]) for p in lote]
        if len(lote) < 250:
            break
        pag += 1
    return productos


def scrape_dermatodo(cap: Capturador) -> dict:
    def fetch_cols():
        params = {"limit": 250, "page": 1}
        r = _peticion(cap.sesion, "GET", DT_BASE + "/collections.json", params=params, headers=_JSON)
        if r.status_code != 200:
            raise RuntimeError(f"estado {r.status_code} en collections.json")
        return r.json(), {"url": DT_BASE + "/collections.json", "metodo": "GET", "parametros": params, "status": 200}
    cols, _ = cap.obtener("collections", fetch_cols)
    declarado = next((c.get("products_count") for c in cols.get("collections", [])
                      if c.get("handle") == DT_FACIAL), None)

    facial = _dt_coleccion(cap, DT_FACIAL)
    miembros = {}
    for handle in sorted(set(DT_SUBCOLECCIONES) | DT_EXCLUIR):
        miembros[handle] = {p["id"] for p, _ in _dt_coleccion(cap, handle)}

    registros = {}
    for p, t_consulta in facial:
        pid = p["id"]
        pertenece = {h for h, ids in miembros.items() if pid in ids}
        subs = [DT_SUBCOLECCIONES[h] for h in DT_SUBCOLECCIONES if h in pertenece]
        subcat = subs[0] if subs else "Facial (sin subcolección)"
        titulo = p.get("title") or ""
        if pertenece & DT_EXCLUIR:
            motivo = "colección de exclusión: " + ", ".join(sorted(pertenece & DT_EXCLUIR))
        else:
            motivo = motivo_texto(titulo, p.get("product_type") or "")
        categorias = " | ".join(sorted(pertenece | {DT_FACIAL}))
        for v in p.get("variants", []):
            sku = (v.get("sku") or "").strip() or f"{pid}-{v.get('id')}"
            vigente, lista = _num(v.get("price")), _num(v.get("compare_at_price"))
            promo = 1 if (vigente and lista and lista > vigente) else 0
            if not promo:
                lista = vigente
            contenido, unidad = extraer_contenido_texto(titulo)
            vt = v.get("title")
            registros[sku] = nuevo_registro(
                comercio="Dermatodo", sku_comercio=sku, codigo_barras=_ean(sku),
                id_producto_comercio=str(pid), url=f"{DT_BASE}/products/{p.get('handle')}",
                titulo_original=titulo, marca_original=p.get("vendor"),
                presentacion_original=None if vt in (None, "", "Default Title") else vt,
                texto_original=_unico([titulo, p.get("product_type"), p.get("body_html")]),
                categoria_origen=subcat, categorias_origen=categorias,
                en_alcance=0 if motivo else 1, motivo_exclusion=motivo,
                precio_lista=lista, precio_vigente=vigente, promocion=promo,
                precio_promocional=vigente if promo else None,
                disponible=None if v.get("available") is None else int(bool(v.get("available"))),
                es_marketplace=0, contenido_num=contenido, unidad=unidad,
                fecha_consulta=t_consulta,
            )
    estado = "completa" if (declarado is None or len(facial) >= declarado) else "parcial"
    detalle = (f"colección 'facial': el sitio declara {declarado} productos (collections.json) y el feed "
               f"público entrega {len(facial)}; la diferencia no se pudo explicar con los datos del sitio")
    return {"registros": marcar_calidad(list(registros.values())), "declarados": declarado,
            "estado": estado, "detalle": detalle}



# =========================================================================== FARMATODO (Algolia)
FT_BASE = SITIOS["Farmatodo"]
FT_PAGINA = FT_BASE + "/categorias/dermocosmetica/facial/limpiadores-faciales"
FT_API = "https://api-search.farmatodo.com/1/indexes/*/queries"
FT_INDICE = "products-colombia"
FT_CLASIFICACIONES = {612: "Facial"}    # classification.id (incluye limpiadores 615, serums 624, acné 621)
FT_TOPE = 80                            # máximo de resultados que devuelve el buscador por consulta
FT_PRECIO_MAX = 2_000_000


def _ft_credenciales(cap: Capturador):
    """Lee de la propia web el id y la clave pública del buscador. Solo viven en memoria."""
    cap.pausar()
    pagina = _peticion(cap.sesion, "GET", FT_PAGINA).text
    cap.n_reales += 1
    srcs = [x for x in re.findall(r'src=["\']([^"\']+)["\']', pagina) if "main-es2020" in x]
    if not srcs:
        raise RuntimeError("No se encontró el script principal de la web de Farmatodo")
    src = srcs[0].split("?")[0]
    url = src if src.startswith("http") else FT_BASE + "/" + src.lstrip("/")
    cap.pausar()
    js = _peticion(cap.sesion, "GET", url).text
    app = re.search(r'prod:\{id:"([A-Z0-9]{10})"', js)
    claves = re.findall(r'prod:"([A-Za-z0-9]{32})"', js)
    if not app or not claves:
        raise RuntimeError("No se encontraron las credenciales públicas del buscador")
    return app.group(1), claves[0]


def _ft_consulta(cap: Capturador, cred, nombre, filtros, pagina):
    def fetch():
        p = {"query": "", "hitsPerPage": 24, "page": pagina, "filters": filtros}
        cuerpo = {"requests": [{"indexName": FT_INDICE, "params": urlencode(p)}]}
        r = _peticion(cap.sesion, "POST", FT_API,
                      params={"x-algolia-agent": "Algolia for JavaScript (4.26.0); Browser"},
                      headers={"X-Algolia-Application-Id": cred[0], "X-Algolia-Api-Key": cred[1],
                               "Content-Type": "application/json", "Origin": FT_BASE,
                               "Referer": FT_BASE + "/"},
                      data=json.dumps(cuerpo))
        if r.status_code != 200:
            raise RuntimeError(f"estado {r.status_code} en el buscador de Farmatodo")
        return r.json()["results"][0], {
            "url": FT_API, "metodo": "POST", "status": 200,
            "parametros": {"indexName": FT_INDICE, "filters": filtros, "page": pagina, "hitsPerPage": 24}}
    return cap.obtener(nombre, fetch)


def _precios_por_tienda(lista, claves):
    """Extrae precios y tiendas de listas como offerPriceByStore: [{'offerPrice': 90320, 'stores': [1, 2]}]."""
    precios, tiendas = [], set()
    for o in lista or []:
        if not isinstance(o, dict):
            continue
        for k in claves:
            p = _num(o.get(k))
            if p:
                precios.append(p)
                break
        tiendas.update(o.get("stores") or [])
    return precios, tiendas


def _registro_farmatodo(h, t):
    lleno, oferta, prime = _num(h.get("fullPrice")), _num(h.get("offerPrice")), _num(h.get("primePrice"))
    # Las ofertas de Farmatodo vienen POR TIENDA (offerPriceByStore). Se guarda el precio normal como
    # precio_lista/precio_vigente y el precio promocional más bajo aparte. (offerStartDate/offerEndDate
    # no son confiables: traen fechas viejas aunque la oferta esté activa.)
    p_of, tiendas = _precios_por_tienda(h.get("offerPriceByStore"), ("offerPrice",))
    if oferta:
        p_of.append(oferta)
    p_of = [p for p in p_of if lleno and p < lleno]
    promo_min = min(p_of) if p_of else None
    p_pr, _ = _precios_por_tienda(h.get("primePriceByStore"), ("primePrice", "offerPrice", "price"))
    if prime:
        p_pr.append(prime)
    p_pr = [p for p in p_pr if lleno and p < lleno]

    nota = []
    lab = (h.get("labelPum") or "").lower()
    u_txt = ("ml" if "mililit" in lab else "g" if "gram" in lab else
             "und" if "unid" in lab else "l" if "litro" in lab else None)
    c_campo, u_campo = normalizar_contenido(h.get("measurePum"), u_txt) if u_txt else (None, None)
    titulo = h.get("mediaDescription") or h.get("description")
    c_tit, u_tit = extraer_contenido_texto(titulo)
    contenido, unidad = c_campo, u_campo
    if c_tit is not None and (c_campo is None or u_campo != u_tit
                              or abs(c_campo - c_tit) / max(c_campo, c_tit) > 0.01):
        if c_campo is not None:
            nota.append(f"contenido: campo={c_campo:g} {u_campo} vs título={c_tit:g} {u_tit} (se usó el título)")
        contenido, unidad = c_tit, u_tit

    url = h.get("url") or (f"{h.get('objectID')}-{h.get('seoUrl')}" if h.get("seoUrl") else None)
    subcat, cat = h.get("subCategory"), h.get("categorie")
    motivo = motivo_texto(titulo, subcat or "")
    fuera = h.get("outofstore")      # hasStock viene en False para todos: no es confiable
    return nuevo_registro(
        comercio="Farmatodo", sku_comercio=str(h.get("objectID")), codigo_barras=_ean(h.get("barcode")),
        id_producto_comercio=str(h.get("id") or h.get("objectID")),
        url=(FT_BASE + "/producto/" + url) if url else None,
        titulo_original=titulo, marca_original=h.get("marca"),
        presentacion_original=h.get("presentación") if isinstance(h.get("presentación"), str) else None,
        texto_original=_unico([h.get("mediaDescription"), h.get("description"), h.get("detailDescription"),
                               h.get("largeDescription"), h.get("grayDescription")]),
        categoria_origen=f"{cat} > {subcat}", categorias_origen=f"{cat} > {subcat}",
        en_alcance=0 if motivo else 1, motivo_exclusion=motivo,
        precio_lista=lleno, precio_vigente=lleno, precio_prime=min(p_pr) if p_pr else None,
        promocion=1 if promo_min else 0, precio_promocional=promo_min,
        tiendas_con_oferta=len(tiendas) if promo_min and tiendas else None,
        disponible=None if fuera is None else int(not fuera),
        es_marketplace=int(bool(h.get("isMarketPlace"))),
        contenido_num=contenido, unidad=unidad,
        nota_calidad=" | ".join(nota) or None, fecha_consulta=t,
    )


def scrape_farmatodo(cap: Capturador) -> dict:
    cred = None if cap.offline else _ft_credenciales(cap)
    hallados, avisos, consultas, notas, declarado = {}, [], 0, [], 0
    for cid, nombre_cat in FT_CLASIFICACIONES.items():
        base = f"classification.id:{cid}"
        res, _ = _ft_consulta(cap, cred, f"algolia_{cid}_sitio", base, 0)
        declarado += res.get("nbHits", 0)
        notas.append(f"{nombre_cat}: el sitio declara {res.get('nbHits')} (el buscador corta en {FT_TOPE})")
        pila = [(0, FT_PRECIO_MAX)]
        while pila:
            lo, hi = pila.pop()
            filtros = f"{base} AND fullPrice >= {lo} AND fullPrice < {hi}"
            res, meta = _ft_consulta(cap, cred, f"algolia_{cid}_{lo}_{hi}_p0", filtros, 0)
            consultas += 1
            n = res.get("nbHits", 0)
            if n == 0:
                continue
            if n >= FT_TOPE and hi - lo > 1:
                mid = (lo + hi) // 2
                pila.append((mid, hi))
                pila.append((lo, mid))
                continue
            if n >= FT_TOPE:
                avisos.append(f"tramo {lo}-{hi} con {n} resultados (posible corte)")
            for pag in range(res.get("nbPages", 1)):
                if pag > 0:
                    res, meta = _ft_consulta(cap, cred, f"algolia_{cid}_{lo}_{hi}_p{pag}", filtros, pag)
                    consultas += 1
                for h in res.get("hits", []):
                    hallados.setdefault(str(h.get("objectID")), (h, meta["fecha_consulta"]))
    registros = marcar_calidad([_registro_farmatodo(h, t) for h, t in hallados.values()])
    subs = Counter(r["categoria_origen"] for r in registros)
    detalle = (f"{'; '.join(notas)}. Recolección por rangos de precio: {len(registros)} productos únicos "
               f"en {consultas} consultas. Por subcategoría: {dict(subs.most_common(12))}. "
               f"El listado del sitio oculta agotados y productos de fórmula; aquí se incluyen todos "
               f"(campo 'disponible'). Precios de la zona por defecto del sitio.")
    if avisos:
        detalle += " AVISOS: " + "; ".join(avisos[:5])
    return {"registros": registros, "declarados": declarado,
            "estado": "completa" if not avisos else "parcial", "detalle": detalle}


# =========================================================================== base de datos
ESQUEMA = """
CREATE TABLE IF NOT EXISTS comercios (
    comercio TEXT PRIMARY KEY,
    sitio_web TEXT
);
CREATE TABLE IF NOT EXISTS ejecuciones (
    id_ejecucion TEXT NOT NULL,
    comercio TEXT NOT NULL,
    fecha_inicio TEXT NOT NULL,
    fecha_fin TEXT,
    modo TEXT,
    registros_obtenidos INTEGER,
    registros_insertados INTEGER,
    registros_declarados_sitio INTEGER,
    estado TEXT CHECK (estado IN ('completa','parcial','fallida')),
    detalle TEXT,
    PRIMARY KEY (id_ejecucion, comercio)
);
CREATE TABLE IF NOT EXISTS observaciones (
    id_observacion INTEGER PRIMARY KEY AUTOINCREMENT,
    id_ejecucion TEXT NOT NULL,
    comercio TEXT NOT NULL,
    sku_comercio TEXT NOT NULL,
    codigo_barras TEXT,
    id_producto_comercio TEXT,
    url TEXT,
    titulo_original TEXT,
    marca_original TEXT,
    presentacion_original TEXT,
    texto_original TEXT,
    categoria_origen TEXT,
    categorias_origen TEXT,
    en_alcance INTEGER,
    motivo_exclusion TEXT,
    precio_lista REAL,
    precio_vigente REAL,
    precio_prime REAL,
    promocion INTEGER,
    precio_promocional REAL,
    tiendas_con_oferta INTEGER,
    disponible INTEGER,
    es_marketplace INTEGER,
    contenido_num REAL,
    unidad TEXT,
    nota_calidad TEXT,
    fecha_consulta TEXT NOT NULL,
    UNIQUE (comercio, sku_comercio, fecha_consulta),
    FOREIGN KEY (id_ejecucion, comercio) REFERENCES ejecuciones (id_ejecucion, comercio)
);
CREATE INDEX IF NOT EXISTS idx_obs_sku ON observaciones (comercio, sku_comercio);
CREATE INDEX IF NOT EXISTS idx_obs_cb ON observaciones (codigo_barras);
CREATE INDEX IF NOT EXISTS idx_obs_ejec ON observaciones (id_ejecucion, comercio);
"""


def abrir_db(db_path="comparador_precios.sqlite"):
    conn = sqlite3.connect(db_path)
    conn.execute("PRAGMA foreign_keys = ON")
    conn.executescript(ESQUEMA)
    existentes = {r[1] for r in conn.execute("PRAGMA table_info(observaciones)")}
    for col, tipo in (("precio_promocional", "REAL"), ("tiendas_con_oferta", "INTEGER"), ("nota_calidad", "TEXT")):
        if col not in existentes:                       # bases creadas con versiones anteriores
            conn.execute(f"ALTER TABLE observaciones ADD COLUMN {col} {tipo}")
    for nombre, sitio in SITIOS.items():
        conn.execute("INSERT OR IGNORE INTO comercios VALUES (?, ?)", (nombre, sitio))
    conn.commit()
    return conn


def _guardar(conn, cap: Capturador, res: dict, modo: str):
    regs = res.get("registros", [])
    cur = conn.execute(
        "INSERT OR IGNORE INTO ejecuciones (id_ejecucion, comercio, fecha_inicio, fecha_fin, modo, "
        "registros_obtenidos, registros_insertados, registros_declarados_sitio, estado, detalle) "
        "VALUES (?,?,?,?,?,?,?,?,?,?)",
        (cap.id_ejec, cap.comercio, cap.fecha_inicio, ahora_iso(), modo, len(regs), 0,
         res.get("declarados"), res["estado"], (res.get("detalle") or "")[:2000]))
    nueva = cur.rowcount == 1
    antes = conn.total_changes
    cols = ", ".join(COLUMNAS)
    marcas = ", ".join("?" for _ in COLUMNAS)
    conn.executemany(
        f"INSERT OR IGNORE INTO observaciones (id_ejecucion, {cols}) VALUES (?, {marcas})",
        [(cap.id_ejec, *[r[c] for c in COLUMNAS]) for r in regs])
    insertados = conn.total_changes - antes
    if nueva:
        conn.execute("UPDATE ejecuciones SET registros_insertados = ? WHERE id_ejecucion = ? AND comercio = ?",
                     (insertados, cap.id_ejec, cap.comercio))
    conn.commit()
    return insertados


SCRAPERS = {"La Rebaja": scrape_larebaja, "Dermatodo": scrape_dermatodo, "Farmatodo": scrape_farmatodo}


def recolectar_todo(comercios=None, db_path="comparador_precios.sqlite", raw_dir="raw",
                    offline=False, id_ejecucion=None, pausa=PAUSA_SEG, verbose=True):
    """Ejecuta la recolección completa: consulta (o relee raw/), guarda copias e inserta en SQLite
    sin sobrescribir observaciones anteriores. Devuelve una lista con el resumen por comercio."""
    comercios = list(comercios or COMERCIOS)
    if not offline and CONTACTO == "tu_correo@unal.edu.co":
        print("AVISO: defina su correo en la variable de entorno SCRAPER_CONTACTO "
              "(se envía en el User-Agent para que los sitios puedan contactarlo).")
    nuevo_id = "EJEC_" + datetime.now(TZ_CO).strftime("%Y%m%d_%H%M%S")
    id_ejec = id_ejecucion if offline else (id_ejecucion or nuevo_id)   # offline: id pedido o última copia
    conn = abrir_db(db_path)
    modo = "offline" if offline else "en_linea"
    salida = []
    for nombre in comercios:
        cap = None
        try:
            cap = Capturador(nombre, id_ejec, raw_dir, offline, pausa)
            res = SCRAPERS[nombre](cap)
        except Exception as e:                      # un comercio que falla no detiene a los demás
            if cap is None:
                cap = Capturador.__new__(Capturador)
                cap.comercio, cap.id_ejec, cap.fecha_inicio, cap.offline = nombre, (id_ejec or nuevo_id), ahora_iso(), offline
                cap.dir = None
            res = {"registros": [], "declarados": None, "estado": "fallida",
                   "detalle": f"{type(e).__name__}: {str(e)[:300]}"}
        insertados = _guardar(conn, cap, res, modo)
        if cap.dir and not offline:
            cap.cerrar(res["estado"])
        fila = {"comercio": nombre, "id_ejecucion": cap.id_ejec, "estado": res["estado"],
                "registros": len(res["registros"]), "insertados": insertados,
                "declarados_sitio": res.get("declarados"), "detalle": res.get("detalle")}
        salida.append(fila)
        if verbose:
            print(f"[{res['estado']:8}] {nombre}: {fila['registros']} registros, {insertados} nuevos "
                  f"(sitio declara {fila['declarados_sitio']})")
            if res["estado"] != "completa":
                print("           ", fila["detalle"][:300])
    conn.close()
    return salida


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Recolección de precios de dermocosméticos")
    ap.add_argument("--comercios", nargs="*", default=None, choices=COMERCIOS)
    ap.add_argument("--db", default="comparador_precios.sqlite")
    ap.add_argument("--raw", default="raw")
    ap.add_argument("--offline", action="store_true", help="reprocesa las copias de raw/ sin consultar los sitios")
    ap.add_argument("--id-ejecucion", default=None)
    ap.add_argument("--pausa", type=float, default=PAUSA_SEG)
    a = ap.parse_args()
    out = recolectar_todo(a.comercios, a.db, a.raw, a.offline, a.id_ejecucion, a.pausa)
    sys.exit(0 if all(f["estado"] != "fallida" for f in out) else 1)
