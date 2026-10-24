"""
Módulo de Extracción, Estandarización y Carga: scraper.py
Taller 1: Minería de Datos - Comparador de Precios de Protectores Solares

Comercios seleccionados (Colombia):
- Bella Piel (API Catalog VTEX)
- Drogas La Rebaja (API Catalog VTEX)
- Locatel Colombia (API Catalog VTEX)

Nota sobre exclusión de Medipiel y Colsubsidio:
- Droguerías Colsubsidio fue excluida porque su robots.txt prohíbe explícitamente consultas a /api/ y el parámetro 'ft'.
- Medipiel fue excluida porque su robots.txt prohíbe de forma general toda URL con parámetros de consulta (Disallow: /*?*).
"""

import os
import json
import gzip
import time
import sqlite3
import re
import urllib.parse
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import List, Dict, Any, Tuple, Optional
import requests

# Zona horaria oficial de Colombia (UTC-5)
TZ_COLOMBIA = timezone(timedelta(hours=-5))

USER_AGENT = (
    "MineriaDeDatos-UNAL-Taller1/1.0 (Academic Research; Universidad Nacional de Colombia; "
    "contacto: estudiante@unal.edu.co) Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)

HEADERS_DEFAULT = {
    "User-Agent": USER_AGENT,
    "Accept": "application/json, text/plain, */*",
    "Accept-Language": "es-CO,es;q=0.9",
}

# =====================================================================
# CATÁLOGO DE PRODUCTOS COMPARABLES (EMPAREJAMIENTO POR EAN Y REGLAS)
# En Locatel se empareja estrictamente por EAN (items[].ean).
# Se excluyen Fusion Water, Active Unify y Spot Prevent por discrepancias de EAN.
# Catálogo unificado de 11 presentaciones con presencia verificada (33 observaciones comparables).
# =====================================================================
CATALOGO_COMPARABLES = [
    {
        "id_producto_comun": "ANTHELIOS_UVMUNE_400_50ML",
        "nombre_estandar": "La Roche-Posay Anthelios UVMune 400 Fluido Invisible FPS 50+",
        "marca_estandar": "La Roche-Posay",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 50 ml",
        "contenido_valor": 50.0,
        "contenido_unidad": "mL",
        "ean": "3337875797597",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "anthelios" in n.lower() and "uvmune" in n.lower() and "fluido" in n.lower() and "color" not in n.lower() and "oil" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "3337875797597" or ("anthelios" in n.lower() and "uv mune" in n.lower() and "color" not in n.lower() and "oil" not in n.lower()),
        }
    },
    {
        "id_producto_comun": "ANTHELIOS_UVMUNE_COLOR_50ML",
        "nombre_estandar": "La Roche-Posay Anthelios UVMune 400 Fluido con Color FPS 50+",
        "marca_estandar": "La Roche-Posay",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 50 ml",
        "contenido_valor": 50.0,
        "contenido_unidad": "mL",
        "ean": "3337875797641",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "anthelios" in n.lower() and "uvmune" in n.lower() and "color" in n.lower() and "oil" not in n.lower() and "gel" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "3337875797641" or ("anthelios" in n.lower() and "uv mune" in n.lower() and "color" in n.lower() and "oil" not in n.lower()),
        }
    },
    {
        "id_producto_comun": "CETAPHIL_SUN_KIDS_150ML",
        "nombre_estandar": "Cetaphil Sun Kids Loción Liposomal FPS 50+",
        "marca_estandar": "Cetaphil",
        "categoria_estandar": "Protector Infantil",
        "presentacion_estandar": "Frasco 150 ml",
        "contenido_valor": 150.0,
        "contenido_unidad": "mL",
        "ean": "7612076457430",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "cetaphil" in n.lower() and "kids" in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "7612076457430" or ("cetaphil" in n.lower() and "kids" in n.lower()),
        }
    },
    {
        "id_producto_comun": "CETAPHIL_SUN_GEL_LIGERO_100ML",
        "nombre_estandar": "Cetaphil Sun Gel Ligero FPS 50+",
        "marca_estandar": "Cetaphil",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 100 ml",
        "contenido_valor": 100.0,
        "contenido_unidad": "mL",
        "ean": "7612076457256",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "cetaphil" in n.lower() and "gel" in n.lower() and "ligero" in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "7612076457256" or ("cetaphil" in n.lower() and "gel" in n.lower() and "ligero" in n.lower()),
        }
    },
    {
        "id_producto_comun": "EUCERIN_HYDRO_FLUID_50ML",
        "nombre_estandar": "Eucerin Sun Hydro Protect Ultra-Light Fluid FPS 50+",
        "marca_estandar": "Eucerin",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 50 ml",
        "contenido_valor": 50.0,
        "contenido_unidad": "mL",
        "ean": "4005900994776",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "hydro fluid" in n.lower() and "tono" not in n.lower() and "kit" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "4005900994776" or ("hydro fluid" in n.lower() and "tono" not in n.lower()),
        }
    },
    {
        "id_producto_comun": "EUCERIN_PHOTOAGING_CONTROL_50ML",
        "nombre_estandar": "Eucerin Sun Photoaging Control FPS 50+",
        "marca_estandar": "Eucerin",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 50 ml",
        "contenido_valor": 50.0,
        "contenido_unidad": "mL",
        "ean": "4005900265227",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "eucerin" in n.lower() and ("photoaging" in n.lower() or "antiedad" in n.lower()) and "color" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "4005900265227" or ("eucerin" in n.lower() and "antiedad" in n.lower() and "color" not in n.lower()),
        }
    },
    {
        "id_producto_comun": "EUCERIN_PIGMENT_CONTROL_50ML",
        "nombre_estandar": "Eucerin Sun Pigment Control FPS 50+",
        "marca_estandar": "Eucerin",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 50 ml",
        "contenido_valor": 50.0,
        "contenido_unidad": "mL",
        "ean": "4005800241901",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "pigment control" in n.lower() and "tono" not in n.lower() and "color" not in n.lower() and "tinted" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "4005800241901" or ("antipigmento" in n.lower() and "tono" not in n.lower()),
        }
    },
    {
        "id_producto_comun": "EUCERIN_OIL_CONTROL_MEDIO_50ML",
        "nombre_estandar": "Eucerin Sun Gel-Crema Oil Control FPS 50+ Tono Medio",
        "marca_estandar": "Eucerin",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Frasco 50 ml",
        "contenido_valor": 50.0,
        "contenido_unidad": "mL",
        "ean": "4005900839633",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "oil control" in n.lower() and ("color medio" in n.lower() or "tono medio" in n.lower() or "medio" in n.lower()),
            "La Rebaja": lambda n, d, b, e: e == "4005900839633" or (("oil control" in n.lower() or "toque seco" in n.lower()) and "medio" in n.lower()),
        }
    },
    {
        "id_producto_comun": "UMBRELLA_URBAN_SPF50_50G",
        "nombre_estandar": "Umbrella Urban Fotoprotector FPS 50+",
        "marca_estandar": "Umbrella",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Tubo 50 g",
        "contenido_valor": 50.0,
        "contenido_unidad": "g",
        "ean": "7703281002451",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "umbrella" in n.lower() and "urban" in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "7703281002451" or "umbrella urban" in n.lower(),
        }
    },
    {
        "id_producto_comun": "UMBRELLA_ACQUA_SPF50_50G",
        "nombre_estandar": "Umbrella Acqua Fotoprotector FPS 50+",
        "marca_estandar": "Umbrella",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Tubo 50 g",
        "contenido_valor": 50.0,
        "contenido_unidad": "g",
        "ean": "7703281021551",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "umbrella" in n.lower() and "acqua" in n.lower() and "kit" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "7703281021551" or "umbrella acqua" in n.lower(),
        }
    },
    {
        "id_producto_comun": "SUNAID_EVOLUTION_SPF50_40G",
        "nombre_estandar": "Sunaid Evolution Protector Solar Gel FPS 50+",
        "marca_estandar": "Sunaid",
        "categoria_estandar": "Protector Facial",
        "presentacion_estandar": "Tubo 40 g",
        "contenido_valor": 40.0,
        "contenido_unidad": "g",
        "ean": "7707206270598",
        "reglas": {
            "Bella Piel": lambda n, d, b, e: "sunaid" in n.lower() and "evolution" in n.lower() and "color" not in n.lower(),
            "La Rebaja": lambda n, d, b, e: e == "7707206270598" or ("sunaid" in n.lower() and "evolution" in n.lower() and "color" not in n.lower()),
        }
    }
]


def obtener_timestamp_colombia() -> str:
    """Retorna la fecha y hora actual de Colombia en formato ISO 8601."""
    return datetime.now(TZ_COLOMBIA).isoformat(timespec="seconds")


def extraer_contenido_y_unidad(texto: str) -> Tuple[Optional[float], Optional[str]]:
    """
    Extrae el valor numérico y unidad de medida (g, mL, mg, unidad) usando regex.
    """
    if not texto:
        return None, None
    texto_norm = texto.lower()
    
    m_ml = re.search(r'(\d+(?:[.,]\d+)?)\s*(?:ml|m\.|mililitros?)\b', texto_norm)
    if m_ml:
        val = float(m_ml.group(1).replace(",", "."))
        return val, "mL"

    m_mg = re.search(r'(\d+(?:[.,]\d+)?)\s*(?:mg|miligramos?)\b', texto_norm)
    if m_mg:
        val = float(m_mg.group(1).replace(",", "."))
        return val, "mg"

    m_g = re.search(r'(\d+(?:[.,]\d+)?)\s*(?:g|gr|gramos?)\b', texto_norm)
    if m_g:
        val = float(m_g.group(1).replace(",", "."))
        return val, "g"

    m_u = re.search(r'(\d+)\s*(?:und|unid|unidades?|piezas?)\b', texto_norm)
    if m_u:
        return float(m_u.group(1)), "unidad"

    return None, None


def clasificar_pertenencia_categoria(
    nombre: str,
    presentacion: str = "",
    descripcion: str = "",
    subcategoria: str = ""
) -> Tuple[int, Optional[str]]:
    """
    Determina si un producto pertenece a la categoría delimitada por el taller:
    - Incluidos (en_categoria = 1): Fotoprotectores faciales (con o sin color),
      corporales, infantiles y de bebé, protectores labiales con FPS, after sun y geles para después del sol.
    - Excluidos (en_categoria = 0, con motivo_exclusion documentado):
      1. Kits o combos que mezclan productos distintos.
      2. Limpiadores, jabones o aguas micelares.
      3. Suplementos o fotoprotectores orales.
      4. Repelentes de insectos.
      5. Autobronceadores sin filtro UV primario.
      6. Maquillaje o cosméticos con FPS como atributo secundario.
      7. Serums cosméticos sin fotoprotección primaria.
      8. Cremas hidratantes o de tratamiento con FPS secundario.
    """
    n_lower = (nombre or "").lower()
    p_lower = (presentacion or "").lower()
    d_lower = (descripcion or "").lower()
    s_lower = (subcategoria or "").lower()

    # 1. Kits o combos que mezclan productos distintos
    if any(k in n_lower for k in ["kit ", "kit-", "combo", "pack "]) or ("+" in n_lower and any(w in n_lower for w in ["mela", "serum", "agua micelar", "termal", "mini", "crema", "obsequio", "regalo", "cosmetiquera", "mist", "reparador labial", "toalla"])):
        return 0, "Kit o combo que mezcla productos distintos"

    # 2. Limpiadores, jabones o aguas micelares
    if any(k in n_lower for k in ["limpiador", "limpiadora", "gel moussant", "moussant", "agua micelar", "cleansing", "syndet", "espuma de limpieza", "espuma limpiadora", "jabon", "jabón", "desmaquillante"]):
        return 0, "Limpiador o jabón facial o corporal"

    # 3. Suplementos o fotoprotección oral
    if any(k in n_lower for k in ["cápsulas", "capsulas", "comprimidos", "tabletas", "fotoprotector oral", "suplemento"]):
        return 0, "Suplemento o fotoprotector oral"

    # 4. Repelentes
    if "repelente" in n_lower:
        return 0, "Repelente de insectos"

    # 5. Autobronceadores sin filtro UV
    if any(k in n_lower for k in ["autobronceador", "autobronceante"]) and not any(k in n_lower for k in ["protector solar", "fps", "spf"]):
        return 0, "Autobronceador sin filtro UV primario"

    # 6. Maquillaje cosmético con FPS secundario
    if any(k in n_lower for k in ["polvo compacto", "polvo traslúcido", "polvo translúcido", "corrector", "pestañina", "primer", "rubor", "blush", "base isdin coverag"]):
        return 0, "Maquillaje o cosmético con FPS secundario"

    # 7. Serums cosméticos sin fotoprotección primaria
    if any(k in n_lower for k in ["sérum", "serum"]) and not any(k in n_lower for k in ["sun serum", "protector solar", "fotoprotector"]):
        return 0, "Serum cosmético sin fotoprotección primaria"

    # 8. Cremas hidratantes o de tratamiento antiedad/aclaradoras con FPS secundario
    if any(k in n_lower for k in ["crema de noche", "crema facial de día", "crema de día", "hidratante facial", "contorno de ojos", "crema aclaradora", "crema hidratante"]):
        if not any(k in n_lower for k in ["protector solar", "bloqueador", "fotoprotector"]):
            return 0, "Crema hidratante o antiedad con FPS secundario"

    if any(k in n_lower for k in ["cicatricure porcelana", "anti-pigment crema", "overhuden", "eclarife", "acladerm", "depigmenting"]):
        if not any(k in n_lower for k in ["protector solar", "bloqueador", "fotoprotector"]):
            return 0, "Crema despigmentante o antiarrugas con FPS secundario"

    # Pertenece legítimamente a la categoría delimitada
    return 1, None


def etiquetar_y_estandarizar_observaciones(
    productos: List[Dict[str, Any]],
    comercio: str
) -> List[Dict[str, Any]]:
    """
    Aplica la lógica de normalización de contenido, unidades, cálculo de precio
    por unidad, pertenencia a categoría y correspondencia con el catálogo común.
    """
    asignados_comun = set()

    for p in productos:
        nombre = p.get("nombre_original", "") or ""
        desc = p.get("descripcion_original", "") or ""
        marca = p.get("marca_original", "") or ""
        pres = p.get("presentacion_original", "") or ""
        precio = p.get("precio_actual")

        texto_analisis = f"{nombre} {pres} {desc}"
        val_cont, unid_cont = extraer_contenido_y_unidad(texto_analisis)

        p["contenido_valor"] = val_cont
        p["contenido_unidad"] = unid_cont
        p["precio_por_unidad"] = round(precio / val_cont, 4) if (precio and val_cont and val_cont > 0) else None

        # Determinar pertenencia a la categoría delimitada y motivo de exclusión
        en_cat, mot_excl = clasificar_pertenencia_categoria(
            nombre=nombre,
            presentacion=pres,
            descripcion=desc,
            subcategoria=p.get("subcategoria_original", "")
        )
        p["en_categoria"] = en_cat
        p["motivo_exclusion"] = mot_excl

        p["comparable"] = 0
        p["id_producto_comun"] = None

        # Solo se comparan productos formalmente incluidos en la categoría
        if en_cat == 1:
            for item_cat in CATALOGO_COMPARABLES:
                cid = item_cat["id_producto_comun"]
                if cid in asignados_comun:
                    continue

                ean_item = str(p.get("ean", "")).strip()

                if comercio == "Locatel":
                    # En Locatel emparejar estrictamente por EAN (campo items[].ean), no por nombre
                    if ean_item and ean_item == item_cat.get("ean"):
                        p["comparable"] = 1
                        p["id_producto_comun"] = cid
                        p["contenido_valor"] = item_cat["contenido_valor"]
                        p["contenido_unidad"] = item_cat["contenido_unidad"]
                        p["precio_por_unidad"] = round(precio / item_cat["contenido_valor"], 4) if precio else None
                        asignados_comun.add(cid)
                        break
                else:
                    regla = item_cat["reglas"].get(comercio)
                    if regla and regla(nombre, desc, marca, ean_item):
                        p["comparable"] = 1
                        p["id_producto_comun"] = cid
                        p["contenido_valor"] = item_cat["contenido_valor"]
                        p["contenido_unidad"] = item_cat["contenido_unidad"]
                        p["precio_por_unidad"] = round(precio / item_cat["contenido_valor"], 4) if precio else None
                        asignados_comun.add(cid)
                        break

    return productos


# =====================================================================
# GESTIÓN DE COPIAS CRUDAS Y PARSING COMÚN
# =====================================================================
class ListaProductos(list):
    """Lista de productos que almacena atributos de auditoría de la recolección."""
    def __init__(self, items=None, total_reportado: Optional[int] = None, fallo: bool = False):
        super().__init__(items or [])
        self.total_reportado = total_reportado
        self.fallo = fallo


def leer_copia_cruda(archivo: Path) -> Tuple[str, Optional[int], List[Dict[str, Any]]]:
    """Lee un archivo individual de copia cruda (.json.gz o .json) y extrae el total reportado."""
    if archivo.name.endswith(".gz"):
        with gzip.open(archivo, "rt", encoding="utf-8") as f:
            datos = json.load(f)
    else:
        with open(archivo, "r", encoding="utf-8") as f:
            datos = json.load(f)

    tot = datos.get("total_esperado")
    if isinstance(tot, dict):
        total_esperado = tot.get("total_esperado")
    elif isinstance(tot, (int, float)):
        total_esperado = int(tot)
    else:
        total_esperado = None

    return datos.get("timestamp", obtener_timestamp_colombia()), total_esperado, datos.get("pages", [])


def cargar_copia_cruda(directorio: Path) -> Tuple[str, Optional[int], List[Dict[str, Any]]]:
    """Carga la copia cruda más reciente (.json.gz preferido) desde el directorio o subcarpetas de fecha."""
    archivos = sorted([f for f in directorio.glob("**/*.json.gz") if f.is_file()])
    if not archivos:
        archivos = sorted([f for f in directorio.glob("**/*.json") if f.is_file()])
    if not archivos:
        raise FileNotFoundError(f"No hay copias crudas en {directorio} para ejecutar en modo sin conexión.")
    return leer_copia_cruda(archivos[-1])


def guardar_copia_cruda(
    directorio: Path,
    prefijo: str,
    timestamp: str,
    total_esperado: Any,
    pages: List[Dict[str, Any]]
) -> None:
    """Guarda copia cruda comprimida en raw/<comercio>/<fecha>/prefijo_timestamp.json.gz"""
    fecha_carpeta = timestamp.split("T")[0]
    carpeta_destino = directorio / fecha_carpeta
    carpeta_destino.mkdir(parents=True, exist_ok=True)
    fecha_tag = timestamp.replace(":", "-")
    archivo_guardar = carpeta_destino / f"{prefijo}_{fecha_tag}.json.gz"
    with gzip.open(archivo_guardar, "wt", encoding="utf-8") as f:
        json.dump({"timestamp": timestamp, "total_esperado": total_esperado, "pages": pages}, f, ensure_ascii=False, indent=2)


def extraer_productos_de_paginas(
    raw_pages: List[Dict[str, Any]],
    comercio: str,
    timestamp: str,
    subcategoria_defecto: str = "Dermocosmética / Protección Solar"
) -> List[Dict[str, Any]]:
    productos: List[Dict[str, Any]] = []
    vistos = set()

    for page in raw_pages:
        for p in page.get("items", []):
            product_id = str(p.get("productId", ""))
            product_name = p.get("productName", "") or ""
            brand = p.get("brand", "") or ""
            link = p.get("link", "") or ""
            description = p.get("description", "") or ""
            categories = p.get("categories", [])
            subcategory = categories[-1].strip("/ ") if categories else subcategoria_defecto

            items_sku = p.get("items", [])
            for sku in items_sku:
                sku_id = str(sku.get("itemId", product_id))
                clave_unicidad = (comercio, sku_id)
                if clave_unicidad in vistos:
                    continue
                vistos.add(clave_unicidad)

                sku_name = sku.get("name", "")
                sellers = sku.get("sellers", [])
                
                price = None
                list_price = None
                for s in sellers:
                    comm = s.get("commertialOffer", {})
                    p_val = comm.get("Price")
                    if p_val and p_val > 0:
                        price = p_val
                        list_price = comm.get("ListPrice")
                        break

                price_val = float(price) if (price is not None and price > 0) else None
                list_price_val = float(list_price) if (list_price is not None and list_price > 0) else None
                is_promo = 1 if (list_price_val and price_val and list_price_val > price_val) else 0

                productos.append({
                    "comercio": comercio,
                    "id_producto_comercio": sku_id,
                    "nombre_original": product_name,
                    "presentacion_original": sku_name,
                    "descripcion_original": description,
                    "marca_original": brand,
                    "subcategoria_original": subcategory,
                    "precio_actual": price_val,
                    "precio_lista": list_price_val,
                    "es_promocion": is_promo,
                    "url": link,
                    "pum_original": None,
                    "fecha_hora_consulta": timestamp,
                    "ean": str(sku.get("ean", "")).strip(),
                })

    return etiquetar_y_estandarizar_observaciones(productos, comercio)


def scrape_tienda_vtex(
    nombre_comercio: str,
    base_url: str,
    subcategorias: List[str],
    offline: bool = False,
    raw_dir: Optional[str] = None,
    step: int = 50,
    subcategoria_defecto: str = "Dermocosmética / Protección Solar",
    is_fq: bool = False
) -> List[Dict[str, Any]]:
    directorio = Path(raw_dir) if raw_dir else Path("raw") / nombre_comercio.lower().replace(" ", "")
    directorio.mkdir(parents=True, exist_ok=True)
    timestamp = obtener_timestamp_colombia()

    raw_pages: List[Dict[str, Any]] = []
    fallo_peticion = False
    total_global_esperado = None

    if offline:
        timestamp, total_global_esperado, raw_pages = cargar_copia_cruda(directorio)
    else:
        totales_categoria: Dict[str, int] = {}
        for subcat in subcategorias:
            start = 0
            total_esperado = None
            while True:
                end = start + step - 1
                if is_fq:
                    url = f"{base_url}/api/catalog_system/pub/products/search?fq={subcat}&O=OrderByNameASC&_from={start}&_to={end}"
                else:
                    url = f"{base_url}/api/catalog_system/pub/products/search/{subcat}?_from={start}&_to={end}"

                try:
                    resp = requests.get(url, headers=HEADERS_DEFAULT, timeout=25)
                except Exception:
                    fallo_peticion = True
                    break

                if resp.status_code not in (200, 206):
                    fallo_peticion = True
                    break

                resources_header = resp.headers.get("resources", "")
                if resources_header and "/" in resources_header:
                    try:
                        total_esperado = int(resources_header.split("/")[-1])
                        totales_categoria[subcat] = total_esperado
                    except ValueError:
                        pass

                try:
                    items = resp.json()
                except Exception:
                    fallo_peticion = True
                    items = []

                if not items:
                    break

                raw_pages.append({
                    "subcategoria_slug": subcat,
                    "page_start": start,
                    "page_end": end,
                    "resources_header": resources_header,
                    "items": items
                })

                start += step
                if total_esperado and start >= total_esperado:
                    break
                if len(items) < step:
                    break
                time.sleep(0.5)

        prefijo = nombre_comercio.lower().replace(" ", "")
        total_global_esperado = sum(totales_categoria.values()) if totales_categoria else None
        metadata_totales = {
            "total_esperado": total_global_esperado,
            "totales_por_categoria": totales_categoria
        }
        guardar_copia_cruda(directorio, prefijo, timestamp, metadata_totales, raw_pages)

    prods = extraer_productos_de_paginas(raw_pages, nombre_comercio, timestamp, subcategoria_defecto)
    return ListaProductos(prods, total_reportado=total_global_esperado, fallo=fallo_peticion)


# =====================================================================
# FUNCIONES POR COMERCIO (INTERFAZ MODULAR DEL TALLER)
# =====================================================================
def scrape_bellapiel(
    offline: bool = False,
    raw_dir: Optional[str] = None,
    term: str = "protector solar"
) -> List[Dict[str, Any]]:
    return scrape_tienda_vtex(
        nombre_comercio="Bella Piel",
        base_url="https://www.bellapiel.com.co",
        subcategorias=["proteccion-solar"],
        offline=offline,
        raw_dir=raw_dir,
        step=50,
        subcategoria_defecto="Dermocosmética / Protección Solar",
        is_fq=False
    )


def scrape_larebaja(
    offline: bool = False,
    raw_dir: Optional[str] = None,
    term: str = "protector solar"
) -> List[Dict[str, Any]]:
    return scrape_tienda_vtex(
        nombre_comercio="La Rebaja",
        base_url="https://www.larebajavirtual.com",
        subcategorias=[
            "C:/5/35/",
            "C:/8/465/"
        ],
        offline=offline,
        raw_dir=raw_dir,
        step=50,
        subcategoria_defecto="Farmacia / Protección Solar",
        is_fq=True
    )


def scrape_locatel(
    offline: bool = False,
    raw_dir: Optional[str] = None,
    term: str = "protector solar"
) -> List[Dict[str, Any]]:
    return scrape_tienda_vtex(
        nombre_comercio="Locatel",
        base_url="https://www.locatelcolombia.com",
        subcategorias=[
            "C:/2/17/",
            "C:/1/8/55/"
        ],
        offline=offline,
        raw_dir=raw_dir,
        step=50,
        subcategoria_defecto="Dermocosmética / Cuidado Solar",
        is_fq=True
    )


# =====================================================================
# 6. GESTIÓN DE BASE DE DATOS SQLITE Y CARGA
# =====================================================================
def inicializar_bd(db_path: str = "comparador_precios.sqlite") -> None:
    """Crea el esquema relacional en SQLite cumpliendo las especificaciones del taller."""
    conn = sqlite3.connect(db_path, timeout=30.0)
    cur = conn.cursor()
    cur.execute("PRAGMA journal_mode = WAL;")
    cur.execute("PRAGMA foreign_keys = ON;")

    # Verificar si la base existente carece de id_corrida, columnas de reporte o tiene restricción obsoleta
    cur.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='ejecuciones';")
    if cur.fetchone():
        cur.execute("PRAGMA table_info(ejecuciones);")
        cols = [r[1] for r in cur.fetchall()]
        cur.execute("SELECT sql FROM sqlite_master WHERE type='table' AND name='observaciones';")
        row_obs_sql = cur.fetchone()
        obs_sql = row_obs_sql[0] if row_obs_sql else ""
        if (
            "id_corrida" not in cols
            or "total_reportado_sitio" not in cols
            or "total_obtenido" not in cols
            or "uq_comercio_prod_fecha" not in obs_sql
        ):
            cur.execute("DROP TABLE IF EXISTS observaciones;")
            cur.execute("DROP TABLE IF EXISTS ejecuciones;")
            cur.execute("DROP TABLE IF EXISTS corridas;")

    # Tabla de corridas (lote de recolección transversal común a todos los comercios)
    cur.execute("""
    CREATE TABLE IF NOT EXISTS corridas (
        id_corrida INTEGER PRIMARY KEY AUTOINCREMENT,
        fecha_hora_inicio TEXT NOT NULL,
        fecha_hora_fin TEXT,
        n_total_observaciones INTEGER DEFAULT 0,
        estado TEXT CHECK(estado IN ('completa', 'parcial', 'fallida')) NOT NULL
    );
    """)

    # Tabla de ejecuciones por comercio pertenecientes a la corrida
    cur.execute("""
    CREATE TABLE IF NOT EXISTS ejecuciones (
        id_ejecucion INTEGER PRIMARY KEY AUTOINCREMENT,
        id_corrida INTEGER NOT NULL,
        fecha_hora_inicio TEXT NOT NULL,
        fecha_hora_fin TEXT,
        comercio TEXT NOT NULL,
        total_reportado_sitio INTEGER,
        total_obtenido INTEGER DEFAULT 0,
        n_registros INTEGER DEFAULT 0,
        estado TEXT CHECK(estado IN ('completa', 'parcial', 'fallida')) NOT NULL,
        FOREIGN KEY (id_corrida) REFERENCES corridas(id_corrida)
    );
    """)

    # Migración de columnas si la tabla ya existía sin ellas
    cur.execute("PRAGMA table_info(ejecuciones)")
    columnas_ej = [row[1] for row in cur.fetchall()]
    if "total_reportado_sitio" not in columnas_ej:
        cur.execute("ALTER TABLE ejecuciones ADD COLUMN total_reportado_sitio INTEGER")
    if "total_obtenido" not in columnas_ej:
        cur.execute("ALTER TABLE ejecuciones ADD COLUMN total_obtenido INTEGER DEFAULT 0")

    # Tabla de catálogo maestro de productos comparables
    cur.execute("""
    CREATE TABLE IF NOT EXISTS productos_comparables (
        id_producto_comun TEXT PRIMARY KEY,
        nombre_estandar TEXT NOT NULL,
        marca_estandar TEXT NOT NULL,
        categoria_estandar TEXT NOT NULL,
        presentacion_estandar TEXT NOT NULL,
        contenido_valor REAL NOT NULL,
        contenido_unidad TEXT NOT NULL,
        ean TEXT
    );
    """)

    # Población / actualización del catálogo maestro de comparables
    for item in CATALOGO_COMPARABLES:
        cur.execute("""
        INSERT INTO productos_comparables (
            id_producto_comun, nombre_estandar, marca_estandar,
            categoria_estandar, presentacion_estandar, contenido_valor, contenido_unidad, ean
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id_producto_comun) DO UPDATE SET
            nombre_estandar = excluded.nombre_estandar,
            marca_estandar = excluded.marca_estandar,
            categoria_estandar = excluded.categoria_estandar,
            presentacion_estandar = excluded.presentacion_estandar,
            contenido_valor = excluded.contenido_valor,
            contenido_unidad = excluded.contenido_unidad,
            ean = excluded.ean;
        """, (
            item["id_producto_comun"],
            item["nombre_estandar"],
            item["marca_estandar"],
            item["categoria_estandar"],
            item["presentacion_estandar"],
            item["contenido_valor"],
            item["contenido_unidad"],
            item.get("ean")
        ))

    # Tabla principal de observaciones con clave foránea a corridas y ejecuciones
    cur.execute("""
    CREATE TABLE IF NOT EXISTS observaciones (
        id_observacion INTEGER PRIMARY KEY AUTOINCREMENT,
        id_corrida INTEGER NOT NULL,
        id_ejecucion INTEGER NOT NULL,
        comercio TEXT NOT NULL,
        id_producto_comercio TEXT NOT NULL,
        nombre_original TEXT NOT NULL,
        presentacion_original TEXT,
        descripcion_original TEXT,
        marca_original TEXT,
        subcategoria_original TEXT,
        precio_actual REAL,
        precio_lista REAL,
        es_promocion INTEGER NOT NULL DEFAULT 0,
        url TEXT,
        pum_original TEXT,
        fecha_hora_consulta TEXT NOT NULL,
        comparable INTEGER NOT NULL DEFAULT 0,
        id_producto_comun TEXT,
        contenido_valor REAL,
        contenido_unidad TEXT,
        precio_por_unidad REAL,
        en_categoria INTEGER NOT NULL DEFAULT 1,
        motivo_exclusion TEXT,
        ean TEXT,
        FOREIGN KEY (id_corrida) REFERENCES corridas(id_corrida),
        FOREIGN KEY (id_ejecucion) REFERENCES ejecuciones(id_ejecucion),
        FOREIGN KEY (id_producto_comun) REFERENCES productos_comparables(id_producto_comun),
        CONSTRAINT uq_comercio_prod_fecha UNIQUE (comercio, id_producto_comercio, fecha_hora_consulta)
    );
    """)

    cur.execute("CREATE INDEX IF NOT EXISTS idx_obs_corrida ON observaciones(id_corrida);")
    cur.execute("CREATE INDEX IF NOT EXISTS idx_obs_comun ON observaciones(id_producto_comun);")
    cur.execute("CREATE INDEX IF NOT EXISTS idx_obs_comercio ON observaciones(comercio);")
    cur.execute("CREATE INDEX IF NOT EXISTS idx_obs_ejecucion ON observaciones(id_ejecucion);")
    cur.execute("CREATE INDEX IF NOT EXISTS idx_obs_en_cat ON observaciones(en_categoria);")

    conn.commit()
    conn.close()


def insertar_observaciones(
    conn: sqlite3.Connection,
    id_corrida: int,
    id_ejecucion: int,
    productos: List[Dict[str, Any]]
) -> int:
    cur = conn.cursor()
    insertados = 0
    sql = """
    INSERT OR IGNORE INTO observaciones (
        id_corrida, id_ejecucion, comercio, id_producto_comercio, nombre_original,
        presentacion_original, descripcion_original, marca_original,
        subcategoria_original, precio_actual, precio_lista, es_promocion,
        url, pum_original, fecha_hora_consulta, comparable, id_producto_comun,
        contenido_valor, contenido_unidad, precio_por_unidad,
        en_categoria, motivo_exclusion, ean
    ) VALUES (
        ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
    )
    """

    for p in productos:
        cur.execute(sql, (
            id_corrida,
            id_ejecucion,
            p["comercio"],
            p["id_producto_comercio"],
            p["nombre_original"],
            p.get("presentacion_original"),
            p.get("descripcion_original"),
            p.get("marca_original"),
            p.get("subcategoria_original"),
            p.get("precio_actual"),
            p.get("precio_lista"),
            p.get("es_promocion", 0),
            p.get("url"),
            p.get("pum_original"),
            p["fecha_hora_consulta"],
            p.get("comparable", 0),
            p.get("id_producto_comun", None),
            p.get("contenido_valor"),
            p.get("contenido_unidad"),
            p.get("precio_por_unidad"),
            p.get("en_categoria", 1),
            p.get("motivo_exclusion"),
            p.get("ean")
        ))
        if cur.rowcount > 0:
            insertados += 1

    conn.commit()
    return insertados


# =====================================================================
# 7. ORQUESTADOR GENERAL
# =====================================================================
def ejecutar_recoleccion(
    offline: bool = False,
    db_path: str = "comparador_precios.sqlite",
    raw_base: str = "raw",
    reprocesar_todas: bool = False
) -> Dict[str, Any]:
    """
    Ejecuta la recolección completa en los tres comercios (Bella Piel, La Rebaja, Locatel),
    asociando todas las extracciones a un id_corrida común que representa el lote de recolección.
    En modo sin conexión (offline), si las copias crudas más recientes ya están cargadas en la BD,
    reutiliza la corrida existente y no crea registros duplicados.
    """
    inicializar_bd(db_path)

    scrapers = [
        ("Bella Piel", "bellapiel", scrape_bellapiel),
        ("La Rebaja", "larebaja", scrape_larebaja),
        ("Locatel", "locatel", scrape_locatel),
    ]

    conn = sqlite3.connect(db_path, timeout=30.0)
    cur = conn.cursor()
    cur.execute("PRAGMA journal_mode = WAL;")
    cur.execute("PRAGMA foreign_keys = ON;")

    # Comprobar si las copias crudas más recientes ya están cargadas en la BD en modo offline
    if offline and not reprocesar_todas:
        candidatos_archivos = {}
        ya_cargadas = True
        for nombre_comercio, safe_dir, _ in scrapers:
            raw_dir_comercio = Path(raw_base) / safe_dir
            archivos = sorted([f for f in raw_dir_comercio.glob("**/*.json.gz") if f.is_file()])
            if not archivos:
                archivos = sorted([f for f in raw_dir_comercio.glob("**/*.json") if f.is_file()])
            if not archivos:
                ya_cargadas = False
                break
            arch_reciente = archivos[-1]
            candidatos_archivos[nombre_comercio] = arch_reciente
            ts, _, _ = leer_copia_cruda(arch_reciente)
            cur.execute("""
                SELECT COUNT(*) FROM observaciones
                WHERE comercio = ? AND fecha_hora_consulta = ?
            """, (nombre_comercio, ts))
            cnt = cur.fetchone()[0]
            if cnt == 0:
                ya_cargadas = False
                break

        if ya_cargadas and len(candidatos_archivos) == len(scrapers):
            # Las copias ya están en la base de datos: reutilizar la corrida existente y saltar inserción
            cur.execute("""
                SELECT id_corrida FROM corridas
                ORDER BY id_corrida DESC
                LIMIT 1
            """)
            row_c = cur.fetchone()
            existing_id_corrida = row_c[0] if row_c else 1

            resumen_existente: Dict[str, Any] = {}
            for nombre_comercio, safe_dir, _ in scrapers:
                cur.execute("""
                    SELECT id_ejecucion, total_reportado_sitio, total_obtenido, n_registros, estado
                    FROM ejecuciones
                    WHERE id_corrida = ? AND comercio = ?
                    ORDER BY id_ejecucion DESC
                    LIMIT 1
                """, (existing_id_corrida, nombre_comercio))
                row_e = cur.fetchone()
                arch = candidatos_archivos.get(nombre_comercio)
                if row_e:
                    id_ej, tot_rep, tot_obt, n_reg, est = row_e
                else:
                    cur.execute("""
                        SELECT COUNT(*) FROM observaciones
                        WHERE id_corrida = ? AND comercio = ?
                    """, (existing_id_corrida, nombre_comercio))
                    n_reg = cur.fetchone()[0]
                    id_ej = existing_id_corrida
                    tot_rep = None
                    tot_obt = n_reg
                    est = "completa"

                resumen_existente[nombre_comercio] = {
                    "id_corrida": existing_id_corrida,
                    "comercio": nombre_comercio,
                    "id_ejecucion": id_ej,
                    "archivo": arch.name if arch else "ultima_copia",
                    "total_reportado_sitio": tot_rep,
                    "total_obtenido": tot_obt,
                    "total_extraidos": tot_obt,
                    "total_insertados": n_reg,
                    "estado": est
                }
            conn.close()
            return resumen_existente

    inicio_corrida = obtener_timestamp_colombia()
    cur.execute("""
    INSERT INTO corridas (fecha_hora_inicio, n_total_observaciones, estado)
    VALUES (?, 0, 'parcial')
    """, (inicio_corrida,))
    id_corrida = cur.lastrowid
    conn.commit()

    resumen: Dict[str, Any] = {}
    total_observaciones_corrida = 0
    hubo_fallo = False

    for nombre_comercio, safe_dir, func_scraper in scrapers:
        raw_dir_comercio = Path(raw_base) / safe_dir

        if offline and reprocesar_todas:
            archivos = sorted([f for f in raw_dir_comercio.glob("**/*.json.gz") if f.is_file()])
            if not archivos:
                archivos = sorted([f for f in raw_dir_comercio.glob("**/*.json") if f.is_file()])
            archivos_a_procesar = archivos if archivos else [None]
        else:
            archivos_a_procesar = [None]

        for arch in archivos_a_procesar:
            inicio = obtener_timestamp_colombia()
            cur.execute("""
            INSERT INTO ejecuciones (id_corrida, fecha_hora_inicio, comercio, total_reportado_sitio, total_obtenido, n_registros, estado)
            VALUES (?, ?, ?, NULL, 0, 0, 'parcial')
            """, (id_corrida, inicio, nombre_comercio))
            id_ejecucion = cur.lastrowid
            conn.commit()

            try:
                if arch:
                    ts, total_rep, pages = leer_copia_cruda(arch)
                    prods = extraer_productos_de_paginas(pages, nombre_comercio, ts)
                    total_reportado_sitio = total_rep
                    fallo_peticion = False
                else:
                    prods = func_scraper(offline=offline, raw_dir=str(raw_dir_comercio))
                    total_reportado_sitio = getattr(prods, "total_reportado", None)
                    fallo_peticion = getattr(prods, "fallo", False)

                # SKU distintos extraídos
                total_obtenido = len({str(p.get("id_producto_comercio")) for p in prods if p.get("id_producto_comercio")})
                n_insertados = insertar_observaciones(conn, id_corrida, id_ejecucion, prods)
                fin = obtener_timestamp_colombia()

                # Criterio estricto de estado de la ejecución:
                # - "fallida" si no se obtuvo nada (total_obtenido == 0).
                # - "completa" si total_obtenido >= total_reportado_sitio y no falló ninguna petición.
                # - "parcial" si total_obtenido < total_reportado_sitio o falló alguna petición.
                # - Para copias antiguas sin total (total_reportado_sitio is NULL), se asigna "parcial".
                if total_obtenido == 0:
                    estado_ej = "fallida"
                elif total_reportado_sitio is not None and total_obtenido >= total_reportado_sitio and not fallo_peticion:
                    estado_ej = "completa"
                else:
                    estado_ej = "parcial"

                cur.execute("""
                UPDATE ejecuciones
                SET fecha_hora_fin = ?, total_reportado_sitio = ?, total_obtenido = ?, n_registros = ?, estado = ?
                WHERE id_ejecucion = ?
                """, (fin, total_reportado_sitio, total_obtenido, n_insertados, estado_ej, id_ejecucion))
                conn.commit()

                total_observaciones_corrida += n_insertados
                clave = f"{nombre_comercio}_{arch.stem}" if arch else nombre_comercio
                resumen[clave] = {
                    "id_corrida": id_corrida,
                    "comercio": nombre_comercio,
                    "id_ejecucion": id_ejecucion,
                    "archivo": arch.name if arch else "ultima_copia",
                    "total_reportado_sitio": total_reportado_sitio,
                    "total_obtenido": total_obtenido,
                    "total_extraidos": total_obtenido,
                    "total_insertados": n_insertados,
                    "estado": estado_ej
                }
            except Exception as e:
                hubo_fallo = True
                fin = obtener_timestamp_colombia()
                cur.execute("""
                UPDATE ejecuciones
                SET fecha_hora_fin = ?, total_reportado_sitio = NULL, total_obtenido = 0, n_registros = 0, estado = 'fallida'
                WHERE id_ejecucion = ?
                """, (fin, id_ejecucion))
                conn.commit()
                clave = f"{nombre_comercio}_{arch.stem}" if arch else nombre_comercio
                resumen[clave] = {
                    "id_corrida": id_corrida,
                    "comercio": nombre_comercio,
                    "id_ejecucion": id_ejecucion,
                    "error": str(e),
                    "estado": "fallida"
                }

    fin_corrida = obtener_timestamp_colombia()
    cur.execute("SELECT COUNT(*) FROM ejecuciones WHERE id_corrida = ? AND estado != 'completa'", (id_corrida,))
    n_incompletas = cur.fetchone()[0]
    estado_corrida = "fallida" if total_observaciones_corrida == 0 else ("parcial" if n_incompletas > 0 else "completa")
    cur.execute("""
    UPDATE corridas
    SET fecha_hora_fin = ?, n_total_observaciones = ?, estado = ?
    WHERE id_corrida = ?
    """, (fin_corrida, total_observaciones_corrida, estado_corrida, id_corrida))
    conn.commit()

    conn.close()
    return resumen


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Módulo de extracción de precios")
    parser.add_argument("--offline", action="store_true", help="Ejecutar en modo sin conexión usando copias raw")
    parser.add_argument("--reprocesar-todas", action="store_true", help="Reprocesar todas las copias crudas históricas")
    parser.add_argument("--db", default="comparador_precios.sqlite", help="Ruta de la base SQLite")
    parser.add_argument("--raw", default="raw", help="Ruta de copias crudas")
    args = parser.parse_args()

    print(f"Iniciando recolección (Modo offline: {args.offline}, Reprocesar todas: {args.reprocesar_todas})...")
    res = ejecutar_recoleccion(
        offline=args.offline,
        db_path=args.db,
        raw_base=args.raw,
        reprocesar_todas=args.reprocesar_todas
    )
    for c, r in res.items():
        print(f"[{c}] -> ID Corrida: {r.get('id_corrida')}, ID Ejecución: {r.get('id_ejecucion')}, Estado: {r.get('estado')}, Registros: {r.get('total_insertados')}")
