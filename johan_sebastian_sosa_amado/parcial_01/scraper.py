"""
scraper.py · Comparador de precios, categoría "Incontinencia y cuidado del adulto".

Trae la categoría completa de las 3 tiendas (Éxito, La Rebaja y Olímpica).
Sigue la estructura de los cuadernos de clase (web scraping, partes 1 y 2):

    clase (Amazon, libros)              este scraper
    requests.get(...) + raise_for_status()  igual, en scrapear_pagina_tienda
    ----------------------------------  ------------------------------------
    construir_url_amazon(termino, pag)  construir_url_tienda(clave, pagina)
    extraer_producto_amazon(nodo)       extraer_producto_vtex(producto, item, pagina_cruda)
    scrapear_pagina_amazon(..., pausa)  scrapear_pagina_tienda(clave, pagina, ..., pausa)
    tabla_productos_vacia()             tabla_productos_vacia()
    scrapear_libros_seguro(url)         scrapear_pagina_segura(...)
    pd.concat([... for p in paginas])   unir_tablas([... for p in paginas_a_consultar])
    validación con pd.DataFrame([{}])   validar(tabla)
    .to_csv("resultados/...")           .to_csv("resultados/fase3_tiendas.csv")

Diferencias con la clase, y por qué
-----------------------------------
1. No usamos BeautifulSoup ni selectores CSS. Las páginas de estas tiendas se
   arman con JavaScript (la clase lo advierte en el ejercicio de Éxito,
   Carulla y Jumbo: el HTML solo trae bloques de "carga"). En vez de abrir
   Chrome (read_html_live en clase), leemos el JSON que la propia página pide
   a la tienda. El enunciado (3.1) lo acepta y recomienda peticiones HTTP
   directas, y la clase dice "prefiere una API cuando esté disponible".
   Por eso cada "nodo" de producto es un elemento del JSON, no un <div>.
2. Revisamos robots.txt con código propio porque urllib.robotparser no
   entiende comodines (* y $), y Olímpica los usa (ver más abajo).
3. Guardamos cada respuesta en raw/ comprimida y se puede reprocesar sin
   conexión: lo exige el enunciado (3.3).

Cómo se obtienen los datos
--------------------------
Las tres tiendas usan la plataforma VTEX.
* Éxito y La Rebaja: buscador de catálogo
      /api/catalog_system/pub/products/search?fq=C:/<sección>/&_from=0&_to=49
  50 productos por página; el total viene en el encabezado "resources".
* Olímpica: su robots.txt prohíbe las direcciones con "&" o "%"
  ("Disallow: /*&", "Disallow: /*%"). Usamos el buscador de su propia tienda
  (VTEX Intelligent Search), con la página en un único parámetro:
      /api/io/_v/api/intelligent-search/product_search/category-1/...?page=1
  24 productos por página; el total viene en "recordsFiltered".

Qué se considera de la categoría
--------------------------------
Se descarga TODA la sección equivalente de cada tienda y cada fila queda
marcada con en_categoria (1 o 0) y, si se excluye, motivo_exclusion.
Las reglas están en clasificar() y son las mismas para las 3 tiendas.

Cómo correrlo (en la carpeta del taller, con el entorno virtual activo):
    python scraper.py                  descarga las 3 tiendas (modo en línea)
    python scraper.py olimpica         descarga solo una tienda (exito, larebaja u olimpica)
    python scraper.py --sin-conexion   reprocesa la última copia guardada en raw/
    python scraper.py --sin-conexion 2026-10-09   reprocesa la copia de esa fecha
    python scraper.py --limpiar        solo la fase 5 (limpieza) sobre la base que ya existe
En los dos modos los datos se guardan en comparador_precios.sqlite (fase 4)
sin borrar lo anterior: cada corrida en línea agrega una ejecución nueva.
"""

import gzip
import json
import math
import re
import sqlite3
import sys
import time
import unicodedata
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import urljoin, urlsplit

import numpy as np
import pandas as pd
import requests

# Colombia está en UTC-5 todo el año (no tiene horario de verano).
HORA_COLOMBIA = timezone(timedelta(hours=-5))

CARPETA_RAW = Path("raw")
CARPETA_RESULTADOS = Path("resultados")
PRODUCTOS_POR_PAGINA = 50   # buscador de catálogo (Éxito y La Rebaja)
MAXIMO_PAGINAS = 100        # tope de seguridad

# Definimos un user-agent similar al de un navegador real (como en clase)
user_agent_navegador = ("Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                        "AppleWebKit/537.36 (KHTML, like Gecko) "
                        "Chrome/122.0.0.0 Safari/537.36")
ENCABEZADOS = {
    "User-Agent": user_agent_navegador,
    "Accept-Language": "es-CO,es;q=0.9",
}

RUTA_CATALOGO = "/api/catalog_system/pub/products/search"
RUTA_BUSQUEDA_INTELIGENTE = "/api/io/_v/api/intelligent-search/product_search"

# Columnas que devuelve cada función de tienda, siempre en este orden.
COLUMNAS = [
    "ejecucion_id", "comercio", "seccion", "pagina", "product_id", "sku", "ean",
    "marca", "subcategoria", "nombre_original", "descripcion_original",
    "presentacion_original",
    "precio", "precio_antes", "promocion", "disponible", "vendedor", "url",
    "fecha_hora_consulta", "en_categoria", "motivo_exclusion",
]

# ---------------------------------------------------------------- tiendas
# Sección de cada tienda que tomamos como equivalente a la categoría de la
# planilla. Se encontraron en el árbol de categorías de cada tienda
# (/api/catalog_system/pub/category/tree/4) y buscando dónde guarda cada una
# los productos comunes de la fase 1 (explorar_fase3.py, 9 oct 2026).

TIENDAS = {
    "exito": {
        "comercio": "Exito",
        "base": "https://www.exito.com",
        "metodo": "catalogo",
        "seccion": "Cuidado personal > Higiene íntima > Pañales y protección incontinencia",
        "ruta_ids": "/34185083/34185115/34185397/",
        "url_web": "https://www.exito.com/cuidado-personal/higiene-intima/"
                   "panales-y-proteccion-incontinencia",
    },
    "larebaja": {
        "comercio": "La Rebaja",
        "base": "https://www.larebajavirtual.com",
        "metodo": "catalogo",
        # Incluye sus subsecciones: fuerte, leve, moderada, toallas húmedas.
        "seccion": "Cuidado personal > Incontinencia",
        "ruta_ids": "/5/33/",
        "url_web": "https://www.larebajavirtual.com/cuidado-personal/incontinencia",
    },
    "olimpica": {
        "comercio": "Olimpica",
        "base": "https://www.olimpica.com",
        "metodo": "busqueda_inteligente",
        # Sección más amplia que nuestra categoría (también trae toallas
        # femeninas, tampones, jabones íntimos): se marca con clasificar().
        "seccion": "Supermercado > Cuidado Personal y Belleza > Protección Sanitaria",
        "ruta_ids": "/900000000/900110000/900110900/",
        "facetas": "category-1/supermercado/category-2/cuidado-personal-y-belleza/"
                   "category-3/proteccion-sanitaria",
        "url_web": "https://www.olimpica.com/supermercado/"
                   "cuidado-personal-y-belleza/proteccion-sanitaria",
    },
}


# ---------------------------------------------------------------- utilidades

def ahora_colombia():
    """Fecha y hora actual en formato ISO 8601 con zona de Colombia."""
    return datetime.now(HORA_COLOMBIA).isoformat(timespec="seconds")


def guardar_crudo(carpeta, nombre, contenido):
    """Guarda un diccionario como JSON comprimido (.json.gz)."""
    carpeta.mkdir(parents=True, exist_ok=True)
    with gzip.open(carpeta / nombre, "wt", encoding="utf-8") as f:
        json.dump(contenido, f, ensure_ascii=False)


def leer_crudo(ruta):
    with gzip.open(ruta, "rt", encoding="utf-8") as f:
        return json.load(f)


def sin_tildes(texto):
    """'Pañal Íntimo' -> 'panal intimo' (para comparar palabras)."""
    t = unicodedata.normalize("NFKD", texto or "")
    return "".join(c for c in t if not unicodedata.combining(c)).lower()


# ---------------------------------------------------------------- robots.txt
# Como pide la clase, antes de extraer revisamos robots.txt. La librería
# urllib.robotparser de Python no entiende los comodines (* y $), así que
# lo revisamos nosotros: se toman las reglas del grupo "User-agent: *" y gana
# la regla más larga que coincida con la dirección (si empatan, gana Allow).

def reglas_robots(texto):
    reglas, agentes, en_reglas = [], [], False
    for linea in texto.splitlines():
        linea = linea.split("#", 1)[0].strip()
        if ":" not in linea:
            continue
        campo, valor = (x.strip() for x in linea.split(":", 1))
        campo = campo.lower()
        if campo == "user-agent":
            if en_reglas:                 # empieza un grupo nuevo
                agentes, en_reglas = [], False
            agentes.append(valor)
        elif campo in ("allow", "disallow"):
            en_reglas = True
            if "*" in agentes and valor:
                reglas.append((campo, valor))
    return reglas


def coincide(patron, ruta):
    regex = re.escape(patron).replace(r"\*", ".*")
    if regex.endswith(r"\$"):
        regex = regex[:-2] + "$"
    return re.match(regex, ruta) is not None


def permitido_por_robots(texto_robots, url):
    partes = urlsplit(url)
    ruta = partes.path + ("?" + partes.query if partes.query else "")
    mejor = None
    for tipo, patron in reglas_robots(texto_robots):
        if coincide(patron, ruta):
            clave = (len(patron), tipo == "allow")
            if mejor is None or clave > mejor[0]:
                mejor = (clave, tipo, patron)
    if mejor is None:
        return True, "ninguna regla aplica"
    return mejor[1] == "allow", f"{mejor[1]}: {mejor[2]}"


def descargar_robots(base, carpeta):
    try:
        texto = requests.get(base + "/robots.txt", headers=ENCABEZADOS, timeout=30).text
    except requests.RequestException:
        texto = ""   # sin robots.txt legible no hay prohibición escrita
    carpeta.mkdir(parents=True, exist_ok=True)
    (carpeta / "robots.txt").write_text(texto, encoding="utf-8")
    return texto


# ---------------------------------------------- tabla vacía y URL (como en clase)

def tabla_productos_vacia():
    """Mantiene la estructura esperada cuando una página no devuelve productos."""
    return pd.DataFrame(columns=COLUMNAS)


def unir_tablas(tablas):
    """pd.concat como en clase, dejando fuera las tablas vacías (pandas avisa
    con un FutureWarning si se le pasan tablas vacías)."""
    con_datos = [t for t in tablas if not t.empty]
    if not con_datos:
        return tabla_productos_vacia()
    return pd.concat(con_datos, ignore_index=True)


def construir_url_tienda(clave, pagina=1):
    """URL de una página de resultados (pagina empieza en 1, como en clase).
    Se arma a mano, sin codificar ":" ni "/", para que sea exactamente la
    dirección que se revisa contra robots.txt."""
    tienda = TIENDAS[clave]
    if tienda["metodo"] == "catalogo":
        desde = (pagina - 1) * PRODUCTOS_POR_PAGINA
        return (f"{tienda['base']}{RUTA_CATALOGO}?fq=C:{tienda['ruta_ids']}"
                f"&_from={desde}&_to={desde + PRODUCTOS_POR_PAGINA - 1}"
                f"&O=OrderByNameASC")
    return f"{tienda['base']}{RUTA_BUSQUEDA_INTELIGENTE}/{tienda['facetas']}?page={pagina}"


# ---------------------------------------------- extraer cada producto

# Marcas de productos para incontinencia y cuidado del adulto.
MARCAS_ADULTO = ["tena", "content", "plenitud", "rely", "prevail", "nateen",
                 "personal choice", "senior wipes", "precision care", "depend"]
# Palabras que indican un producto de la categoría aunque la marca no lo diga
# (por ejemplo, vendedores externos de Éxito que ponen su nombre como marca).
PALABRAS_ADULTO = ["adult", "incontinen", "panal", "pants", "sabanilla",
                   "protector de cama", "ropa interior desechable"]
# Productos que se excluyen aunque estén en la sección, con su motivo.
EXCLUSIONES = [
    (["tenscare"], "aparato electrónico de piso pélvico, no es absorbente ni de higiene", "marca"),
    (["electrodo", "sonda", "tonificador", "estimulador", "masajeador", "kegel",
      "piso pelvico", "suelo pelvico", "colico", "embarazo"],
     "aparato o accesorio, no es absorbente ni de higiene", "nombre"),
    (["orinal"], "orinal portátil, no es absorbente ni de higiene", "nombre"),
    (["prostata"], "suplemento, no es de la categoría", "nombre"),
    (["lubricante"], "lubricante íntimo, no es de la categoría", "nombre"),
    (["lactancia"], "protector de lactancia (maternidad)", "nombre"),
    (["tampon"], "higiene menstrual", "nombre"),
    # Solo si el nombre EMPIEZA por jabón: "Toallas TENA ... + jabón" sí es de la categoría.
    (["jabon", "jab "], "jabón íntimo (higiene femenina)", "inicio"),
    (["papel higienico"], "papel higiénico", "nombre"),
    (["galleta"], "alimento mal clasificado por la tienda", "nombre"),
]


def clasificar(marca, nombre):
    """Decide si un producto pertenece a 'Incontinencia y cuidado del adulto'
    (pañales para adultos, absorbentes, protectores de cama, pañitos y
    productos de higiene para personas dependientes).
    Devuelve (en_categoria, motivo_exclusion)."""
    m, n = sin_tildes(marca), sin_tildes(nombre)
    for palabras, motivo, donde in EXCLUSIONES:
        if donde == "inicio":
            if any(n.startswith(p) for p in palabras):
                return 0, motivo
        elif any(p in (m if donde == "marca" else n) for p in palabras):
            return 0, motivo
    if any(p in m for p in MARCAS_ADULTO) or any(p in n for p in PALABRAS_ADULTO):
        return 1, ""
    return 0, "higiene femenina u otro producto fuera de la categoría"


def vendedor_principal(item):
    """El vendedor que muestra la tienda por defecto (sellerDefault)."""
    vendedores = item.get("sellers") or [{}]
    for v in vendedores:
        if v.get("sellerDefault"):
            return v
    return vendedores[0]


def extraer_producto_vtex(producto, item, pagina_cruda):
    """Equivale a extraer_producto_amazon(nodo) de la clase: recibe un
    producto del JSON (y una de sus presentaciones o SKU) y devuelve una fila."""
    vendedor = vendedor_principal(item)
    oferta = vendedor.get("commertialOffer") or {}
    if "IsAvailable" in oferta:
        disponible = bool(oferta.get("IsAvailable"))
    else:
        disponible = (oferta.get("AvailableQuantity") or 0) > 0
    # VTEX pone precio 0 cuando no hay existencias: eso es un dato
    # ausente, no un precio de cero. Lo dejamos vacío (NULL).
    precio = oferta.get("Price") or None
    precio_antes = oferta.get("ListPrice") or None
    if not disponible:
        precio = precio_antes = None
    promocion = (precio is not None and precio_antes is not None
                 and precio_antes > precio)
    nombre = item.get("nameComplete") or producto.get("productName", "")
    en_categoria, motivo = clasificar(producto.get("brand", ""), nombre)
    return {
        "ejecucion_id": pagina_cruda["ejecucion_id"],
        "comercio": pagina_cruda["comercio"],
        "seccion": pagina_cruda["seccion"],
        "pagina": pagina_cruda["pagina"],
        "product_id": producto.get("productId"),
        "sku": item.get("itemId"),
        "ean": (item.get("ean") or "").strip(),
        "marca": producto.get("brand", ""),
        "subcategoria": (producto.get("categories") or [""])[0],
        "nombre_original": nombre,
        "descripcion_original": producto.get("description", ""),
        # Ficha técnica de la tienda, cuando la publica (La Rebaja: "PAQUETE X 21 UND").
        "presentacion_original": " ".join(producto.get("Presentacionunidadmedida") or []),
        "precio": precio,
        "precio_antes": precio_antes,
        "promocion": int(promocion),
        "disponible": int(disponible),
        "vendedor": vendedor.get("sellerName", ""),
        "url": urljoin(pagina_cruda["base"], producto.get("link", "")),
        "fecha_hora_consulta": pagina_cruda["fecha_hora_consulta"],
        "en_categoria": en_categoria,
        "motivo_exclusion": motivo,
    }


def productos_de(pagina_cruda):
    """Lista de productos de una respuesta, según el buscador usado."""
    respuesta = pagina_cruda.get("respuesta")
    if isinstance(respuesta, list):                     # buscador de catálogo
        return respuesta
    if isinstance(respuesta, dict):                     # búsqueda inteligente
        return respuesta.get("products") or []
    return []


def tabla_desde_pagina(pagina_cruda):
    """Convierte una respuesta guardada en raw/ en un DataFrame.
    Una fila por SKU (cada SKU es una presentación del producto).
    La usan el modo en línea y el modo sin conexión, así ambos dan lo mismo."""
    filas = [extraer_producto_vtex(p, item, pagina_cruda)
             for p in productos_de(pagina_cruda)
             for item in (p.get("items") or [])]
    if not filas:
        return tabla_productos_vacia()
    # Los precios siempre como números decimales (NaN si faltan), para que
    # pd.concat no tenga que adivinar el tipo cuando una página no trae precios.
    return pd.DataFrame(filas, columns=COLUMNAS).astype(
        {"precio": "float64", "precio_antes": "float64"})


def total_informado(pagina_cruda):
    """Total de productos que el propio servicio dice que tiene la sección."""
    respuesta = pagina_cruda.get("respuesta")
    if isinstance(respuesta, dict):
        return respuesta.get("recordsFiltered")
    try:   # el encabezado 'resources' viene como '0-49/167'
        return int(pagina_cruda.get("resources").split("/")[1])
    except (AttributeError, IndexError, ValueError):
        return None


# ---------------------------------------------- scrapear una página (como en clase)

def scrapear_pagina_tienda(clave, pagina, carpeta, robots, ejecucion_id, pausa=2):
    """Equivale a scrapear_pagina_amazon(termino, pagina, pausa) de la clase:
    descarga una página de resultados, la guarda en raw/ y devuelve un DataFrame.
    El total que informa el servicio queda en tabla.attrs["total"]."""
    tienda = TIENDAS[clave]
    # Construimos la URL de la página usando la tienda y el número de página
    url = construir_url_tienda(clave, pagina)
    permitido, regla = permitido_por_robots(robots, url)
    if not permitido:
        print(f"    página {pagina}: robots.txt la prohíbe ({regla})")
        return tabla_productos_vacia()

    # Introducimos una pausa para no hacer solicitudes demasiado seguidas
    time.sleep(pausa)
    # Enviamos la solicitud con user-agent y encabezados de navegador
    respuesta = requests.get(url, headers=ENCABEZADOS, timeout=30)

    try:
        contenido = respuesta.json()
    except ValueError:
        contenido = respuesta.text
    pagina_cruda = {
        "url": url,
        "base": tienda["base"],
        "comercio": tienda["comercio"],
        "seccion": tienda["seccion"],
        "pagina": pagina,
        "ejecucion_id": ejecucion_id,
        "fecha_hora_consulta": ahora_colombia(),
        "codigo_http": respuesta.status_code,
        "resources": respuesta.headers.get("resources"),
        "respuesta": contenido,
    }
    # Guardamos la respuesta tal cual llegó (enunciado 3.3)
    guardar_crudo(carpeta, f"pagina_{pagina:03d}.json.gz", pagina_cruda)

    # Como en clase: raise_for_status() detiene la página si el código es >= 400
    # (scrapear_pagina_segura lo atrapa), y una respuesta que no es JSON
    # indica un posible bloqueo o un cambio en el sitio.
    respuesta.raise_for_status()
    posible_bloqueo = isinstance(contenido, str)
    if posible_bloqueo:
        print(f"    página {pagina}: la respuesta no es JSON, posible bloqueo")
        return tabla_productos_vacia()

    tabla = tabla_desde_pagina(pagina_cruda)
    tabla.attrs["total"] = total_informado(pagina_cruda)
    print(f"    página {pagina}: {len(productos_de(pagina_cruda))} productos")
    return tabla


def scrapear_pagina_segura(*args, **kwargs):
    """Como scrapear_libros_seguro() de la clase: si algo falla, avisa y
    devuelve una tabla vacía con las mismas columnas en vez de detener todo."""
    try:
        return scrapear_pagina_tienda(*args, **kwargs)
    except Exception as e:
        pagina = args[1] if len(args) > 1 else kwargs.get("pagina", "?")
        print(f"    página {pagina}: error ({e})")
        return tabla_productos_vacia()


# ------------------------------------------------------- funciones por tienda

def scrapear_tienda(clave, sin_conexion=False, pausa=2, fecha=None):
    """Devuelve un DataFrame con toda la sección de una tienda.
    En línea: pide la página 1, lee el total, calcula cuántas páginas faltan
    y las recorre (como paginas_a_consultar en clase). Todo queda en
    raw/<tienda>/<fecha_hora>/.
    Sin conexión: reprocesa la carpeta más reciente de raw/<tienda>/ o, si se
    da fecha (por ejemplo "2026-10-09"), la más reciente de ese día."""
    tienda = TIENDAS[clave]
    if sin_conexion:
        carpetas = sorted(p for p in (CARPETA_RAW / clave).glob("*") if p.is_dir())
        if fecha:
            # Solo las copias de ese día: el nombre de la carpeta empieza por la fecha.
            carpetas = [p for p in carpetas if p.name.startswith(fecha)]
        if not carpetas:
            print(f"  no hay copias guardadas en raw/{clave}/" + (f" del {fecha}" if fecha else ""))
            return tabla_productos_vacia()
        carpeta = carpetas[-1]
        print(f"  reprocesando sin conexión: {carpeta}")
        tablas, total = [], None
        for numero, ruta in enumerate(sorted(carpeta.glob("*.json.gz")), start=1):
            pagina_cruda = leer_crudo(ruta)
            # Las copias de la fase 2 no traían estos datos: se completan aquí.
            pagina_cruda.setdefault("pagina", numero)
            pagina_cruda.setdefault("comercio", tienda["comercio"])
            pagina_cruda.setdefault("ejecucion_id", f"{clave}_{carpeta.name}")
            pagina_cruda.setdefault("base", tienda["base"])
            pagina_cruda["seccion"] = tienda["seccion"]
            if total is None:
                total = total_informado(pagina_cruda)
            tablas.append(tabla_desde_pagina(pagina_cruda))
        if not tablas:
            return tabla_productos_vacia()
        tabla = unir_tablas(tablas)
        tabla.attrs["total"] = total
        tabla.attrs["carpeta"] = carpeta
        tabla.attrs["ejecucion_id"] = (tabla["ejecucion_id"].iloc[0] if len(tabla)
                                       else f"{clave}_{carpeta.name}")
        return tabla

    sello = datetime.now(HORA_COLOMBIA).strftime("%Y-%m-%dT%H-%M-%S")
    carpeta = CARPETA_RAW / clave / sello
    ejecucion_id = f"{clave}_{sello}"
    robots = descargar_robots(tienda["base"], carpeta)
    print(f"  sección: {tienda['seccion']}")

    primera = scrapear_pagina_segura(clave, 1, carpeta, robots, ejecucion_id, pausa)
    total = primera.attrs.get("total")
    print(f"    el servicio dice que la sección tiene {total} productos")
    # Datos de la ejecución para la tabla ejecuciones de SQLite (fase 4)
    primera.attrs["carpeta"] = carpeta
    primera.attrs["ejecucion_id"] = ejecucion_id
    if not total or primera.empty:
        return primera
    por_pagina = primera["product_id"].nunique()
    n_paginas = min(math.ceil(total / por_pagina), MAXIMO_PAGINAS)
    paginas_a_consultar = range(2, n_paginas + 1)

    tabla = unir_tablas(
        [primera] + [scrapear_pagina_segura(clave, p, carpeta, robots, ejecucion_id, pausa)
                     for p in paginas_a_consultar]
    )
    tabla.attrs["total"] = total
    tabla.attrs["carpeta"] = carpeta
    tabla.attrs["ejecucion_id"] = ejecucion_id
    return tabla


def scrapear_exito(sin_conexion=False, fecha=None):
    return scrapear_tienda("exito", sin_conexion, fecha=fecha)


def scrapear_larebaja(sin_conexion=False, fecha=None):
    return scrapear_tienda("larebaja", sin_conexion, fecha=fecha)


def scrapear_olimpica(sin_conexion=False, fecha=None):
    return scrapear_tienda("olimpica", sin_conexion, fecha=fecha)


FUNCIONES = {
    "exito": scrapear_exito,
    "larebaja": scrapear_larebaja,
    "olimpica": scrapear_olimpica,
}


# ------------------------------------------------------------- validación

def validar(tabla):
    """Validación como en clase (12.2): filas, únicos, faltantes y duplicados."""
    dentro = tabla[tabla["en_categoria"] == 1]
    resumen = pd.DataFrame([{
        "total_servicio":    tabla.attrs.get("total"),
        "filas":             len(tabla),
        "productos_unicos":  tabla["product_id"].nunique(),
        "sku_duplicados":    int(tabla["sku"].duplicated().sum()),
        "disponibles":       int(tabla["disponible"].sum()),
        "faltantes_precio":  int(tabla["precio"].isna().sum()),
        "en_promocion":      int(tabla["promocion"].sum()),
        "sin_ean":           int((tabla["ean"] == "").sum()),
        "de_la_categoria":   len(dentro),
    }])
    print("\n" + resumen.to_string(index=False))

    excluidos = (tabla[tabla["en_categoria"] == 0]
                 .groupby("motivo_exclusion")
                 .size()
                 .reset_index(name="n")
                 .sort_values("n", ascending=False))
    if len(excluidos):
        print("\n  excluidos:\n" + excluidos.to_string(index=False))

    marcas = (dentro
              .groupby("marca")
              .size()
              .reset_index(name="n")
              .sort_values("n", ascending=False))
    print("\n  de la categoría, por marca:\n" + marcas.to_string(index=False))


# ============================================================ fase 4: SQLite
# Como en las clases 4 y 5 (SQL): sqlite3, que viene con Python, una conexión
# con sqlite3.connect(), un cursor con con.cursor() y pd.read_sql_query()
# para leer. La clase solo LEE una base ya hecha (chinook.db); aquí además hay
# que crearla y llenarla, así que usamos dos piezas que la clase no muestra:
#   * CREATE TABLE (el "DDL" de la tabla de la clase 4) para definir las
#     tablas con sus claves y la restricción de unicidad que pide el enunciado.
#   * DataFrame.to_sql() de pandas, para pasar la tabla del scraper a SQLite.
# Para no sobrescribir ni repetir observaciones, la tabla del scraper se
# copia primero a una tabla temporal "carga" y de ahí se inserta con
# INSERT OR IGNORE ... SELECT ... JOIN: las filas que ya existen se ignoran.

RUTA_BASE = Path("comparador_precios.sqlite")

TABLAS_SQL = [
    # Un comercio por fila.
    """
    CREATE TABLE IF NOT EXISTS comercios (
        comercio_id INTEGER PRIMARY KEY,
        clave       TEXT NOT NULL UNIQUE,     -- exito, larebaja, olimpica
        nombre      TEXT NOT NULL UNIQUE,     -- Exito, La Rebaja, Olimpica
        url_base    TEXT NOT NULL,
        seccion     TEXT NOT NULL,            -- sección tomada como la categoría
        url_web     TEXT
    );
    """,
    # Identificador común de los productos comparables. Se llena en la fase 5.
    """
    CREATE TABLE IF NOT EXISTS productos (
        producto_id  TEXT PRIMARY KEY,        -- por ejemplo P01
        descripcion  TEXT NOT NULL,           -- nombre estandarizado
        marca        TEXT,
        subcategoria TEXT,
        contenido    REAL,                    -- cantidad del empaque
        unidad       TEXT,                    -- und, mL, g
        ean          TEXT,                    -- llave común en las 3 tiendas
        talla        TEXT
    );
    """,
    # Cada producto tal como aparece en un comercio (un SKU), con su texto original.
    """
    CREATE TABLE IF NOT EXISTS anuncios (
        anuncio_id           INTEGER PRIMARY KEY,
        comercio_id          INTEGER NOT NULL REFERENCES comercios(comercio_id),
        sku                  TEXT NOT NULL,   -- identificador interno de la tienda
        product_id           TEXT,
        ean                  TEXT,            -- código de barras ('' si la tienda no lo da)
        marca                TEXT,            -- marca tal como la publica la tienda
        subcategoria         TEXT,            -- ruta de categorías de la tienda
        nombre_original      TEXT NOT NULL,   -- texto original, sin limpiar
        descripcion_original TEXT,
        presentacion_original TEXT,           -- ficha técnica de la tienda, si la hay
        url                  TEXT NOT NULL,   -- página del producto
        en_categoria         INTEGER NOT NULL,   -- 1 si es de la categoría (clasificar)
        motivo_exclusion     TEXT,
        -- Campos de la fase 5 (limpieza), los llena limpiar_y_emparejar():
        marca_estandar        TEXT,
        subcategoria_estandar TEXT,
        contenido             REAL,
        unidad                TEXT,
        talla                 TEXT,
        producto_id           TEXT REFERENCES productos(producto_id),  -- NULL = no comparable
        comparable            INTEGER,        -- 1 = producto comparable, 0 = resto (enunciado 2.4)
        UNIQUE (comercio_id, sku)
    );
    """,
    # Una fila por tienda en cada corrida del scraper.
    """
    CREATE TABLE IF NOT EXISTS ejecuciones (
        ejecucion_id      TEXT PRIMARY KEY,   -- por ejemplo exito_2026-10-10T10-15-00
        recoleccion_id    TEXT NOT NULL,      -- agrupa las 3 tiendas de una misma corrida
        comercio_id       INTEGER NOT NULL REFERENCES comercios(comercio_id),
        fecha_hora_inicio TEXT NOT NULL,      -- ISO 8601, hora de Colombia
        fecha_hora_fin    TEXT,
        total_servicio    INTEGER,            -- productos que dice tener la sección
        productos_unicos  INTEGER,
        registros         INTEGER NOT NULL,   -- filas (SKU) obtenidas
        estado            TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
        carpeta_raw       TEXT,
        cargada_desde     TEXT                -- 'en línea' o 'raw (sin conexión)'
    );
    """,
    # El precio de un anuncio en un momento dado.
    """
    CREATE TABLE IF NOT EXISTS observaciones (
        observacion_id      INTEGER PRIMARY KEY,
        anuncio_id          INTEGER NOT NULL REFERENCES anuncios(anuncio_id),
        ejecucion_id        TEXT NOT NULL REFERENCES ejecuciones(ejecucion_id),
        precio              REAL,             -- COP; NULL si no se muestra (agotado)
        precio_antes        REAL,             -- precio antes del descuento
        promocion           INTEGER NOT NULL,
        disponible          INTEGER NOT NULL,
        vendedor            TEXT,
        pagina              INTEGER,
        fecha_hora_consulta TEXT NOT NULL,    -- ISO 8601, hora de Colombia
        -- Restricción recomendada por el enunciado: comercio + producto del
        -- comercio (el anuncio ya incluye el comercio) + fecha y hora.
        UNIQUE (anuncio_id, fecha_hora_consulta)
    );
    """,
]


def conectar_base(ruta=RUTA_BASE):
    """Abre (o crea) la base y sus tablas, como sqlite3.connect() en clase."""
    con = sqlite3.connect(ruta)
    cur = con.cursor()
    cur.execute("PRAGMA foreign_keys = ON;")   # que SQLite revise las claves foráneas
    for sql in TABLAS_SQL:
        cur.execute(sql)
    # Columnas agregadas después de crear la base (fase 5). Con PRAGMA
    # table_info, como en la clase 4, se revisa si ya existen.
    nuevas = {"anuncios": {"presentacion_original": "TEXT", "talla": "TEXT", "comparable": "INTEGER"},
              "productos": {"ean": "TEXT", "talla": "TEXT"}}
    for tabla, columnas in nuevas.items():
        existentes = pd.read_sql_query(f"PRAGMA table_info({tabla});", con)["name"].tolist()
        for columna, tipo in columnas.items():
            if columna not in existentes:
                cur.execute(f"ALTER TABLE {tabla} ADD COLUMN {columna} {tipo};")
    # Los comercios salen del diccionario TIENDAS.
    comercios = pd.DataFrame([
        {"clave": clave, "nombre": t["comercio"], "url_base": t["base"],
         "seccion": t["seccion"], "url_web": t["url_web"]}
        for clave, t in TIENDAS.items()
    ])
    comercios.to_sql("carga_comercios", con, if_exists="replace", index=False)
    cur.execute("""
        INSERT OR IGNORE INTO comercios (clave, nombre, url_base, seccion, url_web)
        SELECT clave, nombre, url_base, seccion, url_web
        FROM   carga_comercios;
    """)
    cur.execute("DROP TABLE carga_comercios;")
    con.commit()
    return con


def estado_ejecucion(tabla):
    """completa si llegaron todos los productos que informa el servicio,
    parcial si llegaron algunos y fallida si no llegó ninguno."""
    total = tabla.attrs.get("total")
    if tabla.empty:
        return "fallida"
    if total and tabla["product_id"].nunique() >= total:
        return "completa"
    return "parcial"


def guardar_en_sqlite(con, clave, tabla, recoleccion_id, sin_conexion):
    """Registra la ejecución de una tienda e inserta sus anuncios y
    observaciones sin sobrescribir ni repetir lo que ya estaba en la base."""
    cur = con.cursor()
    fechas = tabla["fecha_hora_consulta"] if len(tabla) else pd.Series(dtype=str)
    ejecucion = pd.DataFrame([{
        "ejecucion_id":      tabla.attrs.get("ejecucion_id"),
        "recoleccion_id":    recoleccion_id,
        "clave":             clave,
        "fecha_hora_inicio": fechas.min() if len(fechas) else ahora_colombia(),
        "fecha_hora_fin":    fechas.max() if len(fechas) else None,
        "total_servicio":    tabla.attrs.get("total"),
        "productos_unicos":  tabla["product_id"].nunique(),
        "registros":         len(tabla),
        "estado":            estado_ejecucion(tabla),
        "carpeta_raw":       str(tabla.attrs.get("carpeta", "")),
        "cargada_desde":     "raw (sin conexión)" if sin_conexion else "en línea",
    }])
    ejecucion.to_sql("carga_ejecucion", con, if_exists="replace", index=False)
    cur.execute("""
        INSERT OR IGNORE INTO ejecuciones
              (ejecucion_id, recoleccion_id, comercio_id, fecha_hora_inicio,
               fecha_hora_fin, total_servicio, productos_unicos, registros,
               estado, carpeta_raw, cargada_desde)
        SELECT e.ejecucion_id, e.recoleccion_id, c.comercio_id, e.fecha_hora_inicio,
               e.fecha_hora_fin, e.total_servicio, e.productos_unicos, e.registros,
               e.estado, e.carpeta_raw, e.cargada_desde
        FROM   carga_ejecucion AS e
        JOIN   comercios       AS c ON c.clave = e.clave;
    """)
    cur.execute("DROP TABLE carga_ejecucion;")
    if tabla.empty:
        con.commit()
        return 0, 0

    # La tabla del scraper pasa a SQLite tal cual, en una tabla temporal.
    tabla.to_sql("carga", con, if_exists="replace", index=False)

    # Anuncios nuevos (un comercio + SKU que no estaba antes).
    cur.execute("""
        INSERT OR IGNORE INTO anuncios
              (comercio_id, sku, product_id, ean, marca, subcategoria,
               nombre_original, descripcion_original, presentacion_original,
               url, en_categoria, motivo_exclusion)
        SELECT c.comercio_id, ca.sku, ca.product_id, ca.ean, ca.marca, ca.subcategoria,
               ca.nombre_original, ca.descripcion_original, ca.presentacion_original,
               ca.url, ca.en_categoria, ca.motivo_exclusion
        FROM   carga     AS ca
        JOIN   comercios AS c ON c.nombre = ca.comercio;
    """)
    anuncios_nuevos = cur.rowcount
    # Anuncios que ya estaban antes de existir la columna presentacion_original:
    # se completa (solo si está vacía, el resto no se toca).
    cur.execute("""
        UPDATE anuncios
        SET    presentacion_original = (
                   SELECT ca.presentacion_original
                   FROM   carga     AS ca
                   JOIN   comercios AS c ON c.nombre = ca.comercio
                   WHERE  c.comercio_id = anuncios.comercio_id
                     AND  ca.sku        = anuncios.sku
                   LIMIT 1)
        WHERE  presentacion_original IS NULL;
    """)

    # Observaciones: si ya existe la misma (anuncio, fecha y hora) se ignora.
    cur.execute("""
        INSERT OR IGNORE INTO observaciones
              (anuncio_id, ejecucion_id, precio, precio_antes, promocion,
               disponible, vendedor, pagina, fecha_hora_consulta)
        SELECT a.anuncio_id, ca.ejecucion_id, ca.precio, ca.precio_antes, ca.promocion,
               ca.disponible, ca.vendedor, ca.pagina, ca.fecha_hora_consulta
        FROM   carga     AS ca
        JOIN   comercios AS c ON c.nombre      = ca.comercio
        JOIN   anuncios  AS a ON a.comercio_id = c.comercio_id
                             AND a.sku         = ca.sku;
    """)
    observaciones_nuevas = cur.rowcount
    cur.execute("DROP TABLE carga;")
    con.commit()
    return anuncios_nuevos, observaciones_nuevas


def resumen_base(con):
    """Cuántas filas tiene cada tabla y cada ejecución, leído con
    pd.read_sql_query como en clase."""
    conteo = pd.read_sql_query("""
        SELECT 'comercios'     AS tabla, COUNT(*) AS filas FROM comercios
        UNION ALL SELECT 'productos',     COUNT(*) FROM productos
        UNION ALL SELECT 'anuncios',      COUNT(*) FROM anuncios
        UNION ALL SELECT 'observaciones', COUNT(*) FROM observaciones
        UNION ALL SELECT 'ejecuciones',   COUNT(*) FROM ejecuciones;
    """, con)
    print("\n" + conteo.to_string(index=False))
    ejecuciones = pd.read_sql_query("""
        SELECT   e.recoleccion_id, c.nombre AS comercio, e.fecha_hora_inicio,
                 e.total_servicio, e.registros, e.estado, e.cargada_desde,
                 COUNT(o.observacion_id) AS observaciones
        FROM     ejecuciones AS e
        JOIN     comercios   AS c ON c.comercio_id = e.comercio_id
        LEFT JOIN observaciones AS o ON o.ejecucion_id = e.ejecucion_id
        GROUP BY e.ejecucion_id
        ORDER BY e.fecha_hora_inicio;
    """, con)
    print("\n" + ejecuciones.to_string(index=False))


# ============================================================ fase 5: limpieza
# Como en la clase 7 (limpieza y transformación, versión Python):
#   * texto: minúsculas, sin tildes y espacios repetidos fuera (" ".join(x.split()))
#   * str.extract() con expresiones regulares para sacar talla y contenido
#     ("(8  und)", "X20 Unds", "Lx32", "100 ml")
#   * np.select() (el case_when de la clase) para la subcategoría estándar
#   * isna() para el diagnóstico de faltantes
# y la clase 8 (integración de fuentes): una llave común para unir las tres
# tiendas. Usamos el EAN (código de barras), que identifica un empaque exacto:
# marca, variante, talla y número de unidades. Los nombres no sirven como
# llave: La Rebaja llama igual "PAÑAL ADULTO TENA SLIP COMFORT L" al paquete
# de 9 y al de 21, y llama "pants comfort" a los que las otras dos llaman
# "pants clásicos". El texto original nunca se modifica: lo estandarizado
# va en columnas propias de anuncios (marca_estandar, subcategoria_estandar,
# talla, contenido, unidad, producto_id).
# Fuera de la clase: UPDATE y ALTER TABLE (la clase 4 los nombra como DML y
# DDL pero no los usa), necesarios para escribir lo limpio en la base.

# Marcas conocidas de la categoría, con el nombre estándar.
MARCAS_ESTANDAR = {
    "tena": "TENA", "content": "CONTENT", "plenitud": "PLENITUD", "rely": "RELY",
    "prevail": "PREVAIL", "nateen": "NATEEN", "personal choice": "PERSONAL CHOICE",
    "senior wipes": "SENIOR WIPES", "precision care": "PRECISION CARE",
    "depend": "PLENITUD",
}

PATRONES_CONTENIDO = [
    (r"(\d+(?:[.,]\d+)?)\s*(ml|g|gr)\b", None),                  # 100 ml, 20 g
    (r"\(\s*(\d+)\s*und\s*\)", "und"),                           # (8  und)
    (r"(\d+)\s*(?:und|unds|unidades|un|u)\b", "und"),            # 20 und, x 9un, 30u
    (r"x\s*(\d+)\b", "und"),                                     # X20, Lx32
    (r"\bpor\s+(\d+)\b", "und"),                                 # Por 42
    (r"\b(?:box of|caja de)\s+(\d+)\b", "und"),                    # Caja De 48
]

PATRON_TALLA = (r"\b(?:talla\s+)?(xs|s|m|l|xl|xxl|g/xg|p/m|(?<=talla )\d|small|medium|mediano|"
                r"mediana|large|grande|larga)(?=\b|x\d)")
TALLAS_ESTANDAR = {"small": "S", "medium": "M", "mediano": "M", "mediana": "M",
                   "large": "L", "grande": "L", "larga": "L"}


def normalizar_texto(serie):
    """Minúsculas, sin tildes ni espacios repetidos (clase 7)."""
    return serie.fillna("").map(sin_tildes).map(lambda x: " ".join(x.split()))


def estandarizar_marca(marca, nombre):
    """Marca estándar: primero se busca en la marca que publica la tienda y,
    si es un vendedor (MUNDO BEBE, GENERICO...), en el nombre del anuncio."""
    for texto in (sin_tildes(marca), sin_tildes(nombre)):
        for clave, estandar in MARCAS_ESTANDAR.items():
            if clave in texto:
                return estandar
    return " ".join((marca or "").upper().split()) or None


def extraer_contenido(nombre):
    """Devuelve (contenido, unidad) a partir del texto, o (None, None)."""
    texto = " ".join(sin_tildes(nombre).replace("sku", " sku").split())
    texto = re.sub(r"\bsku\d+|\bkit\d+", " ", texto)      # el SKU no es contenido
    for patron, unidad in PATRONES_CONTENIDO:
        m = re.search(patron, texto)
        if m:
            numero = float(m.group(1).replace(",", "."))
            if unidad is None:
                unidad = "mL" if m.group(2) == "ml" else "g"
            return numero, unidad
    return None, None


def limpiar_anuncios(anuncios):
    """Agrega las columnas estandarizadas sin tocar el texto original."""
    limpio = anuncios.copy()
    texto = normalizar_texto(limpio["nombre_original"])

    limpio["marca_estandar"] = [estandarizar_marca(m, n) for m, n in
                                zip(limpio["marca"], limpio["nombre_original"])]

    # case_when -> np.select (clase 7). El orden importa: "Pañal TENA pants"
    # es ropa interior absorbente, no pañal de cinta.
    condiciones = [
        texto.str.contains(r"toalla(?:s)? humeda|toallita"),
        texto.str.contains(r"sabanilla|protector de cama"),
        texto.str.contains(r"crema|zinc"),
        texto.str.contains(r"pants|pantis|ropa interior|\binterior\b"),
        texto.str.contains(r"panal|pan adulto|slip|diaper|ultrasec|incontinencia"),
        texto.str.contains(r"toalla|protector|proteccion"),
    ]
    valores = ["Toallitas húmedas", "Protector de cama", "Cuidado de la piel",
               "Ropa interior absorbente", "Pañal", "Toalla o protector"]
    limpio["subcategoria_estandar"] = np.select(condiciones, valores, default="Otro")

    # La talla solo aplica a pañales y ropa interior ("medium" en una toalla
    # es el nivel de absorción, no una talla).
    tallas = texto.str.extract(PATRON_TALLA)[0]
    con_talla = limpio["subcategoria_estandar"].isin(["Pañal", "Ropa interior absorbente"])
    limpio["talla"] = [TALLAS_ESTANDAR.get(t, t.upper()) if isinstance(t, str) and aplica
                       else None for t, aplica in zip(tallas, con_talla)]

    # Contenido: primero el nombre; si no lo dice, la ficha técnica de la tienda.
    contenido = [extraer_contenido(n) if extraer_contenido(n)[0] is not None
                 else extraer_contenido(p or "")
                 for n, p in zip(limpio["nombre_original"], limpio["presentacion_original"])]
    limpio["contenido"] = [c for c, _ in contenido]
    limpio["unidad"] = [u for _, u in contenido]
    return limpio


def elegir_comparables(con, limpio):
    """Regla documentada para los productos comparables:
    1. anuncio de la categoría (en_categoria = 1) con EAN;
    2. el mismo EAN en las 3 tiendas;
    3. con precio (no NULL) en las 3 tiendas en al menos una recolección;
    4. talla y contenido que se pueden leer en el texto no se contradicen.
    Devuelve (productos, descartados)."""
    precios = pd.read_sql_query("""
        SELECT   a.anuncio_id, e.recoleccion_id,
                 MAX(o.precio IS NOT NULL) AS con_precio
        FROM     observaciones AS o
        JOIN     anuncios      AS a ON a.anuncio_id   = o.anuncio_id
        JOIN     ejecuciones   AS e ON e.ejecucion_id = o.ejecucion_id
        GROUP BY a.anuncio_id, e.recoleccion_id;
    """, con)
    candidatos = limpio[(limpio["en_categoria"] == 1) & (limpio["ean"] != "")]
    n_tiendas = candidatos.groupby("ean")["comercio"].nunique()
    en_las_3 = candidatos[candidatos["ean"].isin(n_tiendas[n_tiendas == 3].index)]

    # Tiendas con precio por EAN y recolección
    cobertura = (en_las_3.merge(precios, on="anuncio_id")
                 .query("con_precio == 1")
                 .groupby(["ean", "recoleccion_id"])["comercio"].nunique()
                 .groupby("ean").max())

    filas, descartados = [], []
    for ean, grupo in en_las_3.groupby("ean"):
        nombre = grupo.sort_values("comercio")["nombre_original"].iloc[0]
        if cobertura.get(ean, 0) < 3:
            descartados.append({"ean": ean, "nombre": nombre,
                                "motivo": "sin precio en las 3 tiendas en una misma recolección"})
            continue
        tallas = grupo["talla"].dropna().unique()
        contenidos = grupo["contenido"].dropna().unique()
        if len(tallas) > 1 or len(contenidos) > 1:
            descartados.append({"ean": ean, "nombre": nombre,
                                "motivo": f"el texto no coincide: tallas {list(tallas)}, "
                                          f"contenidos {list(contenidos)}"})
            continue
        # Descripción estándar: el nombre de Éxito, que siempre trae las unidades.
        exito = grupo[grupo["comercio"] == TIENDAS["exito"]["comercio"]]
        base = exito["nombre_original"].iloc[0] if len(exito) else nombre
        filas.append({
            "ean": ean,
            "descripcion": " ".join(base.split()),
            "marca": grupo["marca_estandar"].mode().iloc[0],
            "subcategoria": grupo["subcategoria_estandar"].mode().iloc[0],
            "talla": tallas[0] if len(tallas) else None,
            "contenido": contenidos[0] if len(contenidos) else None,
            "unidad": grupo["unidad"].dropna().iloc[0] if grupo["unidad"].notna().any() else None,
        })

    productos = (pd.DataFrame(filas)
                 .sort_values(["subcategoria", "descripcion"])
                 .reset_index(drop=True))
    productos.insert(0, "producto_id", [f"P{i:02d}" for i in range(1, len(productos) + 1)])
    return productos, pd.DataFrame(descartados, columns=["ean", "nombre", "motivo"])


def limpiar_y_emparejar(con):
    """Fase 5 completa: estandariza los anuncios, elige los comparables y lo
    guarda en SQLite (productos y columnas de anuncios). Se puede repetir:
    cada vez recalcula todo a partir del texto original."""
    cur = con.cursor()
    anuncios = pd.read_sql_query("""
        SELECT a.anuncio_id, c.nombre AS comercio, a.sku, a.ean, a.marca,
               a.nombre_original, a.presentacion_original, a.en_categoria
        FROM   anuncios  AS a
        JOIN   comercios AS c ON c.comercio_id = a.comercio_id;
    """, con)
    limpio = limpiar_anuncios(anuncios)
    productos, descartados = elegir_comparables(con, limpio)
    limpio = limpio.merge(productos[["producto_id", "ean"]], on="ean", how="left")
    # Solo los anuncios de la categoría quedan unidos a un producto comparable.
    limpio.loc[limpio["en_categoria"] == 0, "producto_id"] = None

    # Se borra lo calculado antes y se vuelve a escribir.
    cur.execute("UPDATE anuncios SET producto_id = NULL;")
    cur.execute("DELETE FROM productos;")
    productos[["producto_id", "descripcion", "marca", "subcategoria", "contenido",
               "unidad", "ean", "talla"]].to_sql("productos", con,
                                                 if_exists="append", index=False)
    limpio[["anuncio_id", "marca_estandar", "subcategoria_estandar", "talla",
            "contenido", "unidad", "producto_id"]].to_sql(
        "carga_limpieza", con, if_exists="replace", index=False)
    cur.execute("""
        UPDATE anuncios
        SET marca_estandar        = (SELECT l.marca_estandar        FROM carga_limpieza AS l WHERE l.anuncio_id = anuncios.anuncio_id),
            subcategoria_estandar = (SELECT l.subcategoria_estandar FROM carga_limpieza AS l WHERE l.anuncio_id = anuncios.anuncio_id),
            talla                 = (SELECT l.talla                 FROM carga_limpieza AS l WHERE l.anuncio_id = anuncios.anuncio_id),
            contenido             = (SELECT l.contenido             FROM carga_limpieza AS l WHERE l.anuncio_id = anuncios.anuncio_id),
            unidad                = (SELECT l.unidad                FROM carga_limpieza AS l WHERE l.anuncio_id = anuncios.anuncio_id),
            producto_id           = (SELECT l.producto_id           FROM carga_limpieza AS l WHERE l.anuncio_id = anuncios.anuncio_id);
    """)
    # Indicador que pide el enunciado (2.4): comparable = 1 o 0.
    cur.execute("""
        UPDATE anuncios
        SET    comparable = CASE WHEN producto_id IS NULL THEN 0 ELSE 1 END;
    """)
    cur.execute("DROP TABLE carga_limpieza;")
    con.commit()
    return limpio, productos, descartados


def informe_limpieza(con, limpio, productos, descartados):
    """Muestra y guarda en resultados/ lo que se decidió en la fase 5."""
    dentro = limpio[limpio["en_categoria"] == 1]
    print("\n  de la categoría, por subcategoría estándar:")
    print(dentro.groupby(["subcategoria_estandar", "comercio"]).size()
          .unstack(fill_value=0).to_string())
    print("\n  de la categoría, faltantes en lo que se lee del texto (clase 7, isna;"
          " la talla solo cuenta en pañales y ropa interior):")
    con_talla = dentro["subcategoria_estandar"].isin(["Pañal", "Ropa interior absorbente"])
    faltantes = pd.DataFrame({
        "anuncios": dentro.groupby("comercio").size(),
        "sin_marca": dentro["marca_estandar"].isna().groupby(dentro["comercio"]).sum(),
        "sin_talla": (dentro["talla"].isna() & con_talla).groupby(dentro["comercio"]).sum(),
        "sin_contenido": dentro["contenido"].isna().groupby(dentro["comercio"]).sum(),
    })
    print(faltantes.to_string())

    print(f"\n  productos comparables: {len(productos)}")
    print(productos[["producto_id", "ean", "talla", "contenido", "descripcion"]]
          .to_string(index=False))
    if len(descartados):
        print("\n  descartados (mismo EAN en las 3 tiendas):")
        print(descartados.to_string(index=False))

    # Cobertura por recolección (enunciado: 8 productos x 3 comercios = 24)
    cobertura = pd.read_sql_query("""
        SELECT   e.recoleccion_id,
                 COUNT(*)                    AS observaciones_comparables,
                 COUNT(DISTINCT a.producto_id) AS productos,
                 COUNT(DISTINCT a.comercio_id) AS comercios
        FROM     observaciones AS o
        JOIN     anuncios      AS a ON a.anuncio_id   = o.anuncio_id
        JOIN     ejecuciones   AS e ON e.ejecucion_id = o.ejecucion_id
        WHERE    a.producto_id IS NOT NULL
          AND    o.precio      IS NOT NULL
        GROUP BY e.recoleccion_id;
    """, con)
    print("\n  cobertura de precios comparables por recolección:")
    print(cobertura.to_string(index=False))

    CARPETA_RESULTADOS.mkdir(exist_ok=True)
    detalle = (limpio[limpio["producto_id"].notna()]
               .sort_values(["producto_id", "comercio"])
               [["producto_id", "comercio", "sku", "ean", "nombre_original",
                 "marca_estandar", "subcategoria_estandar", "talla", "contenido", "unidad"]])
    detalle.to_csv(CARPETA_RESULTADOS / "fase5_comparables.csv", index=False, encoding="utf-8-sig")
    descartados.to_csv(CARPETA_RESULTADOS / "fase5_descartados.csv", index=False, encoding="utf-8-sig")
    print(f"\n  guardados resultados/fase5_comparables.csv ({len(detalle)} anuncios) "
          f"y resultados/fase5_descartados.csv ({len(descartados)})")


# ------------------------------------------------------ función general

def ejecutar_recoleccion(tiendas=None, sin_conexion=False, ruta_base=RUTA_BASE, fecha=None):
    """Función general que pide el enunciado (3.2): ejecuta la recolección
    completa, registra la ejecución e inserta los resultados en SQLite sin
    sobrescribir observaciones anteriores. Devuelve la tabla unida."""
    tiendas = tiendas or list(FUNCIONES)
    recoleccion_id = datetime.now(HORA_COLOMBIA).strftime("%Y-%m-%dT%H-%M-%S")
    resultados = {}
    for clave in tiendas:
        print(f"\n== {TIENDAS[clave]['comercio']}" + (" (sin conexión)" if sin_conexion else ""))
        tabla = FUNCIONES[clave](sin_conexion, fecha)
        validar(tabla)
        resultados[clave] = tabla

    # Sin conexión, la recolección se identifica por la copia más antigua
    # que se reprocesó (así el 9 oct queda como "2026-10-09T05-01-44").
    if sin_conexion:
        carpetas = [t.attrs["carpeta"].name for t in resultados.values()
                    if "carpeta" in t.attrs]
        if carpetas:
            recoleccion_id = min(carpetas)

    con = conectar_base(ruta_base)
    print(f"\n== Base de datos {ruta_base} (recolección {recoleccion_id})")
    for clave, tabla in resultados.items():
        if sin_conexion and "carpeta" not in tabla.attrs:
            continue     # no había copias de esta tienda en raw/
        anuncios, observaciones = guardar_en_sqlite(con, clave, tabla,
                                                    recoleccion_id, sin_conexion)
        print(f"  {TIENDAS[clave]['comercio']}: {len(tabla)} filas, "
              f"{anuncios} anuncios nuevos, {observaciones} observaciones nuevas "
              f"({len(tabla) - observaciones} ya estaban)")
    print("\n== Limpieza y productos comparables (fase 5)")
    informe_limpieza(con, *limpiar_y_emparejar(con))
    resumen_base(con)
    con.close()

    # Unimos las tablas de las tiendas (como en clase) y guardamos el resultado
    productos_tiendas = unir_tablas(list(resultados.values()))
    CARPETA_RESULTADOS.mkdir(exist_ok=True)
    ruta_salida = CARPETA_RESULTADOS / "fase3_tiendas.csv"
    productos_tiendas.to_csv(ruta_salida, index=False, encoding="utf-8-sig")
    print(f"\n  guardado {ruta_salida} ({len(productos_tiendas)} filas), se abre con Excel")
    return productos_tiendas


def main():
    if "--limpiar" in sys.argv:
        # Solo la fase 5, sobre la base que ya existe (no descarga nada).
        con = conectar_base()
        informe_limpieza(con, *limpiar_y_emparejar(con))
        con.close()
        return
    sin_conexion = "--sin-conexion" in sys.argv
    # Fecha opcional de la copia que se reprocesa, por ejemplo 2026-10-09.
    fechas = [a for a in sys.argv[1:] if re.match(r"\d{4}-\d{2}-\d{2}", a)]
    elegidas = [a for a in sys.argv[1:] if a in FUNCIONES] or list(FUNCIONES)
    ejecutar_recoleccion(elegidas, sin_conexion, fecha=fechas[0] if fechas else None)


if __name__ == "__main__":
    main()
