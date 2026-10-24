import time, gzip, json, re, sqlite3, unicodedata
from pathlib import Path
from datetime import datetime
from zoneinfo import ZoneInfo
import requests
import pandas as pd

TZ_CO = ZoneInfo("America/Bogota")
RAW = Path("raw")
BASE = Path("comparador_precios.sqlite")
PAUSA = 1.0
HEADERS = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                         "(KHTML, like Gecko) Chrome/126.0 Safari/537.36"}

# Columnas comunes de los 3 comercios
COLUMNAS = ["comercio", "id_producto", "sku", "ean", "nombre", "marca", "categoria_sitio",
            "precio", "precio_antes", "promocion", "disponible", "vendedor", "url",
            "texto_original", "descripcion", "fecha_consulta", "id_ejecucion"]


def ahora_iso():
    return datetime.now(TZ_CO).isoformat(timespec="seconds")


def nuevo_id():
    return datetime.now(TZ_CO).strftime("%Y%m%dT%H%M%S")


def nueva_carpeta(clave, id_ej):
    """Crea raw/<comercio>/<AAAA-MM-DD>/<id_ejecucion>/ con la fecha del identificador."""
    carpeta = RAW / clave / f"{id_ej[:4]}-{id_ej[4:6]}-{id_ej[6:8]}" / id_ej
    carpeta.mkdir(parents=True, exist_ok=True)
    return carpeta


def pedir(url, params=None, cuerpo=None, intentos=4):
    """GET (o POST si se pasa cuerpo) que reintenta los errores temporales del servidor."""
    for i in range(intentos):
        try:
            if cuerpo is None:
                r = requests.get(url, params=params, headers=HEADERS, timeout=30)
            else:
                r = requests.post(url, json=cuerpo, headers=HEADERS, timeout=30)
            if r.status_code in (429, 500, 502, 503, 504):
                raise requests.HTTPError(f"Error temporal {r.status_code}")
            r.raise_for_status()
            return r
        except (requests.HTTPError, requests.ConnectionError, requests.Timeout) as e:
            if i == intentos - 1:
                raise
            print(f"  {e}. Reintento en {2 * 2 ** i} s")
            time.sleep(2 * 2 ** i)


def guardar_gz(ruta, datos):
    with gzip.open(ruta, "wt", encoding="utf-8") as f:
        json.dump(datos, f, ensure_ascii=False)


def leer_gz(ruta):
    with gzip.open(ruta, "rt", encoding="utf-8") as f:
        return json.load(f)


def escribir_manifiesto(carpeta, m):
    (carpeta / "manifiesto.json").write_text(json.dumps(m, ensure_ascii=False, indent=2), encoding="utf-8")


def leer_manifiesto(carpeta):
    return json.loads((Path(carpeta) / "manifiesto.json").read_text(encoding="utf-8"))


# ---------------- Éxito y Locatel (VTEX) ----------------
URL_EXITO = "https://www.exito.com/api/catalog_system/pub/products/search"
FQ_EXITO = "C:/34185083/34185118/34185412/"   # Vitaminas, proteínas y suplementos
CON_PRECIO = "P:[1 TO 99999999]"               # solo productos con precio, como los muestra la página

URL_LOC = "https://www.locatelcolombia.com/api/catalog_system/pub/products/search"
URL_REGION_LOC = "https://www.locatelcolombia.com/api/checkout/pub/regions"
CODIGO_BOGOTA = "11001"   # código DANE de Bogotá; el sitio lo acepta como código postal
FQ_LOC = {   # Locatel no tiene una categoría deportiva: los productos están repartidos en estas tres
    "C:/6/41/": "Alimentos nutricionales > Suplementos",
    "C:/6/39/245/": "Alimentos nutricionales > Bebidas > Rehidratantes",
    "C:/6/39/241/": "Alimentos nutricionales > Bebidas > Energizantes",
}


def descargar_vtex(url, fqs, carpeta, prefijo, extra=None):
    """Recorre las páginas de 50 productos y guarda cada una; devuelve (total del sitio, descargados)."""
    desde = descargados = 0
    while True:
        r = pedir(url, {"fq": fqs, "_from": desde, "_to": desde + 49, **(extra or {})})
        pagina = r.json()
        guardar_gz(carpeta / f"pagina_{prefijo}_{desde:05d}.json.gz",
                   {"url": r.url, "consultado": ahora_iso(), "cuerpo": pagina})
        total = int(r.headers["resources"].split("/")[1])
        descargados += len(pagina)
        desde += 50
        if not pagina or desde >= total:
            return total, descargados
        time.sleep(PAUSA)


def aplanar_vtex(productos, comercio, fecha, id_ej):
    filas = []
    for p in productos:
        for item in p.get("items", []):
            vendedores = item.get("sellers") or [{}]
            vendedor = next((v for v in vendedores if v.get("sellerDefault")), vendedores[0])
            oferta = vendedor.get("commertialOffer", {})
            precio = oferta.get("Price") or None
            antes = oferta.get("ListPrice") or None
            if antes == precio:
                antes = None
            filas.append({
                "comercio": comercio,
                "id_producto": p.get("productId"),
                "sku": item.get("itemId"),
                "ean": item.get("ean") or None,
                "nombre": p.get("productName"),
                "marca": p.get("brand"),
                "categoria_sitio": (p.get("categories") or [None])[0],
                "precio": precio,
                "precio_antes": antes,
                "promocion": int(bool(precio and antes and precio < antes)),
                "disponible": int((oferta.get("AvailableQuantity") or 0) > 0),
                "vendedor": vendedor.get("sellerName"),
                "url": p.get("link"),
                "texto_original": " | ".join(filter(None, [
                    p.get("productName"), item.get("name"), p.get("description")])),
                "descripcion": p.get("description") or None,
                "fecha_consulta": fecha,
                "id_ejecucion": id_ej,
            })
    return pd.DataFrame(filas, columns=COLUMNAS)


def procesar_vtex(carpeta, comercio, fecha, id_ej):
    productos = {}
    for archivo in sorted(Path(carpeta).glob("pagina_*.json.gz")):
        for p in leer_gz(archivo)["cuerpo"]:
            productos[p["productId"]] = p
    return aplanar_vtex(productos.values(), comercio, fecha, id_ej)


def recolectar_exito(online=True, carpeta=None, id_ej=None):
    if not online:
        m = leer_manifiesto(carpeta)
        return procesar_vtex(carpeta, "Exito", m["fecha_consulta"], m["id_ejecucion"]), m

    id_ej = id_ej or nuevo_id()
    fecha = ahora_iso()
    carpeta = nueva_carpeta("exito", id_ej)
    total, bajados = descargar_vtex(URL_EXITO, [FQ_EXITO, CON_PRECIO], carpeta, "cat")
    print(f"Vitaminas, proteínas y suplementos: {bajados} de {total} productos con precio")

    df = procesar_vtex(carpeta, "Exito", fecha, id_ej)
    unicos = int(df["id_producto"].nunique())
    m = {"comercio": "Exito", "id_ejecucion": id_ej, "carpeta": carpeta.as_posix(),
         "fecha_consulta": fecha, "fecha_fin": ahora_iso(), "fuente": URL_EXITO,
         "filtros": [FQ_EXITO, CON_PRECIO], "total_sitio": total, "productos_unicos": unicos,
         "filas": len(df), "estado": "completa" if unicos >= total else "parcial"}
    escribir_manifiesto(carpeta, m)
    return df, m


def recolectar_locatel(online=True, carpeta=None, id_ej=None):
    if not online:
        m = leer_manifiesto(carpeta)
        return procesar_vtex(carpeta, "Locatel", m["fecha_consulta"], m["id_ejecucion"]), m

    id_ej = id_ej or nuevo_id()
    fecha = ahora_iso()
    carpeta = nueva_carpeta("locatel", id_ej)
    # Región de Bogotá: el sitio la usa para mostrar la disponibilidad según la dirección
    region = pedir(URL_REGION_LOC, {"country": "COL", "postalCode": CODIGO_BOGOTA}).json()[0]["id"]
    total_sitio = 0
    for i, (fq, nombre) in enumerate(FQ_LOC.items()):
        total, bajados = descargar_vtex(URL_LOC, [fq], carpeta, f"c{i}", {"regionId": region})
        total_sitio += total
        print(f"{nombre}: {bajados} de {total} productos")
        time.sleep(PAUSA)

    df = procesar_vtex(carpeta, "Locatel", fecha, id_ej)
    unicos = int(df["id_producto"].nunique())
    m = {"comercio": "Locatel", "id_ejecucion": id_ej, "carpeta": carpeta.as_posix(),
         "fecha_consulta": fecha, "fecha_fin": ahora_iso(), "fuente": URL_LOC, "region_bogota": region,
         "filtros": list(FQ_LOC), "total_sitio": total_sitio, "productos_unicos": unicos,
         "filas": len(df), "estado": "completa" if unicos >= total_sitio else "parcial"}
    escribir_manifiesto(carpeta, m)
    return df, m


# ---------------- Farmatodo (Algolia) ----------------
URL_FT = "https://api-search.farmatodo.com/1/indexes/*/queries"
INDICE_FT = "products-colombia"
CATEGORIA_FT = "Suplementos deportivos"
TOPE_FT = 80   # lo que muestra la página de la categoría
# El servicio no deja pedir la categoría y entrega máximo 24 productos por búsqueda,
# así que se busca por tipo de producto, por marca y por las líneas más vendidas.
TERMINOS_FT = [
    "proteina", "whey", "isolate", "creatina", "aminoacidos", "bcaa", "glutamina", "preentreno",
    "pre workout", "ganador de peso", "hipercalorica", "gel energetico", "barra proteina",
    "bebida hidratante", "isotonica", "electrolitos", "suplementos deportivos", "proteinas deportivas",
    "snacks de proteina", "energizantes deportivos", "hidratantes deportivos", "creatinas",
    "nutramerican", "megaplex", "bipro", "crea stack", "mega soy", "creatine power", "radical power",
    "100% whey protein", "gainz", "mega carbs", "nitro shock", "smart muscle", "muscletech",
    "optimum nutrition", "dymatize", "healthy sports", "vitanas", "sascha fitness", "going", "benet",
    "benet whey", "gu", "fitbar", "quest", "gatorade", "powerade", "elemental",
]


def aplanar_farmatodo(productos, fecha, id_ej):
    filas = []
    for h in productos:
        lleno = h.get("fullPrice") or None
        oferta = h.get("offerPrice") or None
        en_oferta = bool(lleno and oferta and oferta < lleno)
        filas.append({
            "comercio": "Farmatodo",
            "id_producto": h.get("objectID"),
            "sku": str(h.get("item") or h.get("objectID")),
            "ean": h.get("barcode") or None,
            "nombre": h.get("mediaDescription"),
            "marca": h.get("marca") or None,
            "categoria_sitio": " > ".join(filter(None, [h.get("categorie"), h.get("subCategory")])) or None,
            "precio": oferta if en_oferta else lleno,
            "precio_antes": lleno if en_oferta else None,
            "promocion": int(en_oferta),
            "disponible": int(bool(h.get("hasStock"))),
            "vendedor": "Farmatodo",
            "url": f"https://www.farmatodo.com.co/producto/{h['url']}" if h.get("url") else None,
            "texto_original": " | ".join(filter(None, [
                h.get("mediaDescription"), h.get("grayDescription"), h.get("largeDescription")])),
            "descripcion": h.get("largeDescription") or None,
            "fecha_consulta": fecha,
            "id_ejecucion": id_ej,
        })
    return pd.DataFrame(filas, columns=COLUMNAS)


def procesar_farmatodo(carpeta, fecha, id_ej):
    productos = {}
    for archivo in sorted(Path(carpeta).glob("pagina_*.json.gz")):
        for h in leer_gz(archivo)["cuerpo"]["hits"]:
            productos[h["objectID"]] = h
    return aplanar_farmatodo(productos.values(), fecha, id_ej)


def recolectar_farmatodo(online=True, carpeta=None, id_ej=None):
    if not online:
        m = leer_manifiesto(carpeta)
        return procesar_farmatodo(carpeta, m["fecha_consulta"], m["id_ejecucion"]), m

    id_ej = id_ej or nuevo_id()
    fecha = ahora_iso()
    carpeta = nueva_carpeta("farmatodo", id_ej)
    cortadas = []
    for i, termino in enumerate(TERMINOS_FT):
        cuerpo = {"requests": [{"indexName": INDICE_FT, "query": termino, "hitsPerPage": 24, "page": 0}]}
        res = pedir(URL_FT, cuerpo=cuerpo).json()["results"][0]
        guardar_gz(carpeta / f"pagina_t{i:03d}.json.gz",
                   {"url": URL_FT, "termino": termino, "consultado": ahora_iso(), "cuerpo": res})
        print(f"{termino:24} {len(res['hits']):3} de {res['nbHits']:3}")
        if res["nbHits"] > len(res["hits"]):
            cortadas.append(termino)
        time.sleep(PAUSA)

    df = procesar_farmatodo(carpeta, fecha, id_ej)
    en_cat = int(df["categoria_sitio"].str.startswith(CATEGORIA_FT, na=False).sum())
    m = {"comercio": "Farmatodo", "id_ejecucion": id_ej, "carpeta": carpeta.as_posix(),
         "fecha_consulta": fecha, "fecha_fin": ahora_iso(), "fuente": URL_FT, "indice": INDICE_FT,
         "terminos": TERMINOS_FT, "busquedas_cortadas": cortadas, "productos_unicos": int(df["id_producto"].nunique()),
         "filas": len(df), "en_suplementos_deportivos": en_cat, "total_sitio": TOPE_FT,
         # sin un total oficial y con búsquedas cortadas en 24, no se puede asegurar que esté todo
         "estado": "parcial" if cortadas else "completa"}
    escribir_manifiesto(carpeta, m)
    return df, m


# ---------------- Limpieza ----------------
def normalizar(texto):
    """Minúsculas, sin tildes y con espacios simples."""
    if pd.isna(texto):
        return ""
    texto = unicodedata.normalize("NFKD", str(texto)).encode("ascii", "ignore").decode()
    return re.sub(r"\s+", " ", texto.lower()).strip()


# Contenido: unidad del texto -> (unidad estándar, factor)
UNIDADES = {"kg": ("g", 1000), "kilo": ("g", 1000), "kilos": ("g", 1000),
            "g": ("g", 1), "gr": ("g", 1), "grs": ("g", 1), "gramo": ("g", 1), "gramos": ("g", 1),
            "lb": ("g", 453.6), "lbs": ("g", 453.6), "libra": ("g", 453.6), "libras": ("g", 453.6),
            "ml": ("ml", 1), "cc": ("ml", 1), "l": ("ml", 1000), "lt": ("ml", 1000),
            "lts": ("ml", 1000), "litro": ("ml", 1000), "litros": ("ml", 1000)}
NUM = r"(\d+(?:[.,]\d+)?)"
MEDIDA = r"(" + "|".join(sorted(UNIDADES, key=len, reverse=True)) + r")\b"
CONTEO = (r"(?:unidades|unidad|unds|und|unid|uds|ud|u|capsulas|capsula|caps|tabletas|tableta|tabs|"
          r"softgels|softgel|sobres|sobre|sticks|stick|sachets|sachet|barras|barra|geles|gel|"
          r"tarros|tarro|latas|lata|botellas|botella|frascos|frasco|paquetes|paquete)")
SUELTO = r"(?<![a-wyz0-9.,])(\d+)"   # número entero no pegado a una letra (salvo x12): evita leer C4 o B12
PACK_1 = re.compile(rf"{SUELTO}\s*({CONTEO})?\s*(?:x|de|por)\s*{NUM}\s*{MEDIDA}")    # 30 sticks x 6 g
PACK_2 = re.compile(rf"{NUM}\s*{MEDIDA}\s*(?:x|por)\s*(\d+)\s*{CONTEO}\b")           # 500 ml x 6 und
SIMPLE = re.compile(rf"{NUM}\s*{MEDIDA}")                                            # 910 gr, 2 lb
CUENTA = re.compile(rf"{SUELTO}\s*{CONTEO}\b")                                       # x 1 und


def a_numero(texto, factor):
    if factor == 1 and re.fullmatch(r"\d{1,3}[.,]\d{3}", texto):   # 1.000 g son mil gramos
        return float(texto[:-4] + texto[-3:])
    return float(texto.replace(",", "."))


def extraer_contenido(nombre, total_al_final=False):
    """(contenido total, unidad, unidades del empaque) en g, ml o und; None si no aparece.

    total_al_final: el comercio escribe "x 25 unidades x 500 g" con el total del empaque (Locatel)."""
    m = PACK_1.search(nombre)
    if m and m[2] and total_al_final:
        unidad, factor = UNIDADES[m[4]]
        return round(a_numero(m[3], factor) * factor, 1), unidad, int(m[1])
    if m and (m[2] or int(m[1]) <= 60):   # sin palabra de conteo, un número alto es del nombre (ISO 100)
        n, cantidad, medida = int(m[1]), m[3], m[4]
    elif m := PACK_2.search(nombre):
        n, cantidad, medida = int(m[3]), m[1], m[2]
    elif m := SIMPLE.findall(nombre):   # la última medida: "0.7 Lb 360 G" o "2.2 lb (1.000g)"
        n, (cantidad, medida) = 1, m[-1]
    elif m := CUENTA.search(nombre):
        return float(m[1]), "und", int(m[1])
    else:
        return None, None, None
    unidad, factor = UNIDADES[medida]
    return round(n * a_numero(cantidad, factor) * factor, 1), unidad, n


SABORES = {   # texto en el nombre -> sabor estándar
    "cookies and cream": "cookies and cream", "cookies & cream": "cookies and cream",
    "cookies n cream": "cookies and cream", "chocolate": "chocolate", "vainilla": "vainilla",
    "vanilla": "vainilla", "fresa": "fresa", "frutos rojos": "frutos rojos", "banano": "banano",
    "mango": "mango", "maracuya": "maracuyá", "limon": "limón", "naranja": "naranja",
    "mandarina": "mandarina", "mora": "mora", "uva": "uva", "cereza": "cereza", "sandia": "sandía",
    "pina": "piña", "coco": "coco", "manzana": "manzana", "green apple": "manzana", "kiwi": "kiwi",
    "guayaba": "guayaba", "granada": "granada", "tropical": "tropical", "fruit punch": "ponche de frutas",
    "cafe": "café", "capuchino": "capuchino", "cappuccino": "capuchino", "arequipe": "arequipe",
    "caramelo": "caramelo", "mani": "maní", "sin sabor": "sin sabor", "natural": "sin sabor",
}
ALT_SABORES = "|".join(sorted(map(re.escape, SABORES), key=len, reverse=True))
PATRON_SABOR = re.compile(rf"\b({ALT_SABORES})\b")


def extraer_sabor(nombre):
    return " y ".join(sorted({SABORES[s] for s in PATRON_SABOR.findall(nombre)})) or None


def sin_sabores(nombre):
    """Quita 'sabor X' y cookies and cream para que un sabor no se confunda con un alimento."""
    texto = re.sub(r"cookies ?(?:and|&|n'?|y) ?(?:cream|crema)", " ", nombre)
    return re.sub(rf"\bsabor(?:es)? (?:a )?(?:{ALT_SABORES}|\w+)", " ", texto)


MARCAS = [   # patrón en la marca o el nombre normalizados -> marca estándar
    (r"nutramerican|bipro|mega soy|crea ?stack|radical (?:power|nutrition)", "Nutramerican"),
    (r"megaplex", "Megaplex"),
    (r"smart ?muscle", "Smart Muscle"),
    (r"muscle ?tech", "MuscleTech"),
    (r"optimum|gold standard", "Optimum Nutrition"),
    (r"benet|nutresa", "Benet"),
    (r"\bquest\b", "Quest"),
    (r"sascha", "Sascha Fitness"),
    (r"dymatize", "Dymatize"),
    (r"healthy sports", "Healthy Sports"),
    (r"\bgoing\b", "Going"),
    (r"\bgu\b", "GU"),
]
GENERICAS = {"", "generico", "generica", "sin marca", "otros", "otras", "varios", "na", "n/a"}


def estandarizar_marca(marca, nombre):
    """Marca del diccionario si coincide con la marca del sitio o con el nombre; si no, la del sitio."""
    marca = normalizar(marca)
    textos = [nombre] if marca in GENERICAS else [marca, nombre]
    for texto in textos:
        for patron, estandar in MARCAS:
            if re.search(patron, texto):
                return estandar
    return None if marca in GENERICAS else marca.title()


# Delimitación de la planilla: proteínas, creatina, aminoácidos, preentrenos, geles, barras y bebidas.
# Incluye líneas de producto cuyo nombre no trae el tipo ("Bipro Classic", "Crea Stack", "Radical Nutramerican").
SUBCATEGORIAS = {
    "barra": r"\bbarra|\bbars?\b|barrita|fit ?bar",
    "gel": r"\bgu\b|\bgel(?:es)?\b.*(?:energ|deportiv|isotonic|carbohidrat|cafeina)|energy gel|gel (?:benet|going)",
    "ganador de peso": r"ganador|gainer|\bmass\b|hipercalori|creatine power",
    "preentreno": r"pre ?entreno|pre-?workout|pre ?work|\bc4\b|oxido nitrico|radical (?:power|nutramerican)",
    "bebida deportiva": (r"gatorade|powerade|sporade|isotonic|hidratante|electrolito|bebida deportiv|"
                         r"recuperador|recovery"),
    "creatina": r"creatin|crea ?stack",
    "aminoácidos": (r"bcaa|glutamin|gluta ?stack|aminoacid|\bamino\b|aminomix|\beaas?\b|leucin|arginin|citrulin|"
                    r"beta[ -]?alanin|\bhmb\b|\bbccas?\b"),
    "proteína": r"prote[ií]n|whey|isolat|\biso ?100\b|caseina|casein|suero de leche|bipro",
}
FUERTES = (r"creatin|crea ?stack|whey|isolat|caseina|casein|suero de leche|bipro|bcaa|glutamin|gluta ?stack|"
           r"barra de prote|barra proteic|protein bar|quest bar|fit ?bar|\bgu\b|"
           r"aminoacid|\bamino\b|aminomix|\beaas?\b|pre ?entreno|pre-?workout|ganador|gainer|gatorade|powerade|sporade|isotonic|"
           r"\bgel(?:es)?\b.*energ|energy gel|gel (?:benet|going)")
EXCLUSIONES_PREVIAS = [
    ("cuidado personal", r"shampoo|champu|acondicionador|capilar|cabello|keratina|peinar|locion|mascarilla|"
                         r"jabon|desodorante|bloqueador|protector solar|maquillaje|facial|corporal|\bpiel\b|"
                         r"(?<!y )(?<!&)(?<!con )\bcrema\b(?! de mani| agria)|intim|unas|esmalte|manicura|lampara|compresa|gafas|gorro"),
    ("suero de rehidratación de uso médico", r"\bsuero\b(?! de leche)|\belectrolit\b|hidraplus|pedialyte|"
                                             r"rehidratacion oral|sales de rehidratacion"),
    ("colágeno", r"colageno|collagen"),
    ("nutrición general o médica", r"ensure|pediasure|glucerna|nutren|enterex|\bboost\b|formula infantil|"
                                   r"prowhey|fresubin|frebini|diben|supportan|fortisip|fortini|nepro|pulmocare|"
                                   r"prosure|proklein|nutrione|ascenda|gerifood|espesante|similac"),
    ("quemador o termogénico", r"quemador|termogenic|burner|carnitina|\bcla\b|thermo|lipo ?6|movilizador de grasa"),
]
ACCESORIOS = r"shaker|mezclador|\btermo\b|tomatodo|botilito|cinturon|guante|toalla|bascula|pastillero"
EXCLUSIONES = [
    ("alimento general", r"brownie|galleta|cookie|cereal|ponque|pancake|waffle|wafer|granola|avena|"
                         r"mantequilla de mani|crema de mani|\bchips?\b|pudin|helado|\bleche\b|lacte|yogur|"
                         r"kumis|chocolatina|\btorta\b|muffin|\bpan\b|\bpasta\b|\bcafe\b|infusion|\bte\b|"
                         r"panela|\bmiel\b|arequipe"),
    ("vitaminas y minerales", r"vitamin|multivit|mineral|omega|calcio|magnesio|zinc|hierro|biotina|"
                              r"acido folico|complejo b|\bb12\b"),
    ("otro suplemento", r"aloe|ginseng|espirulina|spirulina|moringa|\bmaca\b|curcuma|ashwagandha|levadura|"
                        r"lecitina|aceite|propoleo|valeriana|melatonina|probiotic|fibra|glucosamina|"
                        r"condroitina|cartilago|resveratrol|coenzima|q10"),
]


def subcategoria(nombre):
    """Primero las formas (barra, gel...); entre creatina, aminoácidos y proteína, la que aparece primero."""
    for sub in ("barra", "gel", "ganador de peso", "preentreno", "bebida deportiva"):
        if re.search(SUBCATEGORIAS[sub], nombre):
            return sub
    posiciones = [(m.start(), sub) for sub in ("creatina", "aminoácidos", "proteína")
                  if (m := re.search(SUBCATEGORIAS[sub], nombre))]
    return min(posiciones)[1] if posiciones else None


def clasificar(nombre, comercio, categoria_sitio):
    """(en_categoria, subcategoria, motivo_exclusion) según la delimitación de la planilla."""
    if comercio == "Farmatodo" and not normalizar(categoria_sitio).startswith(normalizar(CATEGORIA_FT)):
        return 0, None, "fuera de Suplementos deportivos en Farmatodo"
    for motivo, patron in EXCLUSIONES_PREVIAS:
        if re.search(patron, nombre):
            return 0, None, motivo
    if re.search(FUERTES, nombre):
        return 1, subcategoria(nombre), None
    if re.search(ACCESORIOS, nombre):   # después de las palabras fuertes: un combo con termo sigue siendo suplemento
        return 0, None, "accesorio"
    texto = sin_sabores(nombre)
    for motivo, patron in EXCLUSIONES:
        if re.search(patron, texto):
            return 0, None, motivo
    sub = subcategoria(nombre)
    return (1, sub, None) if sub else (0, None, "sin palabra clave de nutrición deportiva")


VACIAS = set("""x por de del la las el los en con y para a al sabor sabores sin tarro frasco bolsa doypack
doy pack caja sobre sobres pote paquete botella lata envase und unds unid unidad unidades ud uds polvo
suplemento suplementos dietario dietetico deportivo deportiva nutricional alimento bebida proteina
proteinas protein proteico proteica hidratante isotonica isotonico energetico energetica nuevo nueva
new formula presentacion nutramericanpharma pharma c u g""".split())
SINONIMOS = {"creatine": "creatina", "glutamine": "glutamina", "monohydrate": "monohidratada",
             "monohidrato": "monohidratada", "aislada": "isolate", "aislado": "isolate",
             "isolated": "isolate", "bar": "barra", "bars": "barra", "barras": "barra", "geles": "gel"}


def nombre_estandar(nombre, marca_sitio, marca_std):
    """Nombre sin marca, sabor, contenido ni palabras de relleno, para comparar entre comercios."""
    texto = PATRON_SABOR.sub(" ", sin_sabores(nombre))
    for patron in (PACK_1, PACK_2, SIMPLE, CUENTA):
        texto = patron.sub(" ", texto)
    texto = re.sub(r"(\d+)\s*mg\b", r"\1mg", texto)   # la concentración queda como una palabra
    texto = re.sub(r"suero de leche", "whey", texto)
    texto = re.sub(r"pre ?-?(?:entreno|workout)", "preentreno", texto)
    marca = set((normalizar(marca_sitio) + " " + normalizar(marca_std)).split())
    palabras = [SINONIMOS.get(p, p) for p in re.findall(r"[a-z0-9]+", texto)]
    palabras = [p for p in palabras if p not in VACIAS and p not in marca and not p.isdigit()]
    return " ".join(dict.fromkeys(palabras)) or None


def limpiar(df):
    """Agrega las columnas estandarizadas sin tocar el texto original."""
    nombres = df["nombre"].map(normalizar)
    contenido = pd.DataFrame([extraer_contenido(n, c == "Locatel") for n, c in zip(nombres, df["comercio"])],
                             index=df.index,
                             columns=["contenido", "unidad", "unidades_empaque"])
    clases = pd.DataFrame([clasificar(n, c, s) for n, c, s in
                           zip(nombres, df["comercio"], df["categoria_sitio"])], index=df.index,
                          columns=["en_categoria", "subcategoria_std", "motivo_exclusion"])
    df = pd.concat([df, contenido, clases], axis=1)
    df["marca_std"] = [estandarizar_marca(m, n) for m, n in zip(df["marca"], nombres)]
    df["nombre_std"] = [nombre_estandar(n, m, s) for n, m, s in zip(nombres, df["marca"], df["marca_std"])]
    df["variante"] = [extraer_sabor(n) for n in nombres]
    df["con_descripcion"] = [int(normalizar(d) != "") for d in df["descripcion"]]
    return df


# ---------------- Base SQLite ----------------
ESQUEMA = """
CREATE TABLE IF NOT EXISTS comercios (
    id_comercio   INTEGER PRIMARY KEY,
    nombre        TEXT NOT NULL UNIQUE,
    url_categoria TEXT
);
INSERT OR IGNORE INTO comercios VALUES
    (1, 'Exito', 'https://tienda.exito.com/cuidado-personal/salud-y-medicamentos/vitaminas-proteinas-y-suplementos'),
    (2, 'Farmatodo', 'https://www.farmatodo.com.co/categorias/bienestar-y-nutricion-deportiva/suplementos-deportivos'),
    (3, 'Locatel', 'https://www.locatelcolombia.com/alimentos-nutricionales/suplementos');
CREATE TABLE IF NOT EXISTS ejecuciones (
    id_ejecucion  TEXT NOT NULL,
    comercio      TEXT NOT NULL REFERENCES comercios (nombre),
    inicio        TEXT NOT NULL,
    fin           TEXT,
    registros     INTEGER NOT NULL,
    en_categoria  INTEGER NOT NULL,
    total_sitio   INTEGER,
    estado        TEXT NOT NULL CHECK (estado IN ('completa', 'parcial', 'fallida')),
    carpeta_raw   TEXT,
    PRIMARY KEY (id_ejecucion, comercio)
);
CREATE TABLE IF NOT EXISTS productos (
    id_producto_comun TEXT PRIMARY KEY,
    nombre_comun      TEXT NOT NULL,
    marca_std         TEXT,
    subcategoria_std  TEXT,
    nombre_std        TEXT,
    variante          TEXT,
    contenido         REAL,
    unidad            TEXT,
    unidades_empaque  INTEGER,
    regla             TEXT
);
CREATE TABLE IF NOT EXISTS anuncios (
    id_anuncio           INTEGER PRIMARY KEY,
    comercio             TEXT NOT NULL REFERENCES comercios (nombre),
    id_producto_comercio TEXT NOT NULL,
    sku                  TEXT NOT NULL,
    ean                  TEXT,
    nombre               TEXT,
    texto_original       TEXT,
    categoria_sitio      TEXT,
    marca_sitio          TEXT,
    marca_std            TEXT,
    subcategoria_std     TEXT,
    nombre_std           TEXT,
    variante             TEXT,
    contenido            REAL,
    unidad               TEXT CHECK (unidad IN ('g', 'ml', 'und')),
    unidades_empaque     INTEGER,
    con_descripcion      INTEGER,
    en_categoria         INTEGER NOT NULL,
    motivo_exclusion     TEXT,
    url                  TEXT,
    id_producto_comun    TEXT REFERENCES productos (id_producto_comun),
    comparable           INTEGER NOT NULL DEFAULT 0,
    UNIQUE (comercio, id_producto_comercio, sku)
);
CREATE TABLE IF NOT EXISTS observaciones (
    id_observacion INTEGER PRIMARY KEY,
    id_anuncio     INTEGER NOT NULL REFERENCES anuncios (id_anuncio),
    id_ejecucion   TEXT NOT NULL,
    precio         REAL CHECK (precio > 0),
    precio_antes   REAL,
    promocion      INTEGER NOT NULL,
    disponible     INTEGER,
    vendedor       TEXT,
    fecha_consulta TEXT NOT NULL,
    UNIQUE (id_anuncio, fecha_consulta)
);
CREATE INDEX IF NOT EXISTS idx_observaciones_ejecucion ON observaciones (id_ejecucion);
"""

CAMPOS_ANUNCIO = ["comercio", "id_producto", "sku", "ean", "nombre", "texto_original", "categoria_sitio",
                  "marca", "marca_std", "subcategoria_std", "nombre_std", "variante", "contenido", "unidad",
                  "unidades_empaque", "con_descripcion", "en_categoria", "motivo_exclusion", "url"]
SQL_ANUNCIO = """
INSERT INTO anuncios (comercio, id_producto_comercio, sku, ean, nombre, texto_original, categoria_sitio,
    marca_sitio, marca_std, subcategoria_std, nombre_std, variante, contenido, unidad, unidades_empaque,
    con_descripcion, en_categoria, motivo_exclusion, url)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (comercio, id_producto_comercio, sku) DO UPDATE SET
    ean = excluded.ean, nombre = excluded.nombre, texto_original = excluded.texto_original,
    categoria_sitio = excluded.categoria_sitio, marca_sitio = excluded.marca_sitio,
    marca_std = excluded.marca_std, subcategoria_std = excluded.subcategoria_std,
    nombre_std = excluded.nombre_std, variante = excluded.variante, contenido = excluded.contenido,
    unidad = excluded.unidad, unidades_empaque = excluded.unidades_empaque,
    con_descripcion = excluded.con_descripcion, en_categoria = excluded.en_categoria,
    motivo_exclusion = excluded.motivo_exclusion, url = excluded.url
"""
SQL_OBSERVACION = """
INSERT OR IGNORE INTO observaciones (id_anuncio, id_ejecucion, precio, precio_antes, promocion,
    disponible, vendedor, fecha_consulta)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)
"""


def guardar(con, df, m, carpeta):
    """Registra la ejecución y agrega anuncios y observaciones; las observaciones previas no se tocan."""
    df = limpiar(df)
    df[["id_producto", "sku"]] = df[["id_producto", "sku"]].astype(str)
    df = df.astype(object).where(df.notna(), None)   # NaN -> NULL
    en_cat = int(sum(df["en_categoria"]))
    con.execute("INSERT OR REPLACE INTO ejecuciones VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                (m["id_ejecucion"], m["comercio"], m["fecha_consulta"], m.get("fecha_fin"), len(df),
                 en_cat, m.get("total_sitio"), m["estado"], Path(carpeta).as_posix()))
    con.executemany(SQL_ANUNCIO, df[CAMPOS_ANUNCIO].itertuples(index=False))
    ids = {(p, s): i for i, p, s in con.execute(
        "SELECT id_anuncio, id_producto_comercio, sku FROM anuncios WHERE comercio = ?", (m["comercio"],))}
    con.executemany(SQL_OBSERVACION, [
        (ids[(r.id_producto, r.sku)], r.id_ejecucion, r.precio, r.precio_antes, r.promocion,
         r.disponible, r.vendedor, r.fecha_consulta) for r in df.itertuples()])
    con.commit()
    print(f"{m['comercio']:10} {m['id_ejecucion']}  {len(df):5} registros, "
          f"{en_cat:4} en la categoría, {m['estado']}")
    return {"id_ejecucion": m["id_ejecucion"], "comercio": m["comercio"], "inicio": m["fecha_consulta"],
            "registros": len(df), "en_categoria": en_cat, "total_sitio": m.get("total_sitio"),
            "estado": m["estado"]}


RECOLECTORES = {"exito": recolectar_exito, "farmatodo": recolectar_farmatodo, "locatel": recolectar_locatel}
NOMBRES = {"exito": "Exito", "farmatodo": "Farmatodo", "locatel": "Locatel"}
COMERCIOS = tuple(NOMBRES.values())


def ejecutar_recoleccion(online=True, db=BASE):
    """Recolecta los tres comercios con un mismo id de ejecución (o reprocesa raw/) y carga SQLite."""
    con = sqlite3.connect(db)
    con.executescript(ESQUEMA)
    resumen = []
    if online:
        id_ej = nuevo_id()
        for clave, recolectar in RECOLECTORES.items():
            print(f"--- {NOMBRES[clave]} ---")
            inicio = ahora_iso()
            try:
                df, m = recolectar(online=True, id_ej=id_ej)
            except Exception as e:   # si un comercio falla, se registra y se sigue con los demás
                print(f"Falló {NOMBRES[clave]}: {e}")
                df = pd.DataFrame(columns=COLUMNAS)
                m = {"comercio": NOMBRES[clave], "id_ejecucion": id_ej, "fecha_consulta": inicio,
                     "fecha_fin": ahora_iso(), "estado": "fallida"}
            resumen.append(guardar(con, df, m, nueva_carpeta(clave, id_ej)))
    else:
        for clave, recolectar in RECOLECTORES.items():
            for manifiesto in sorted((RAW / clave).glob("*/*/manifiesto.json")):
                carpeta = manifiesto.parent   # raw/<comercio>/<fecha>/<id_ejecucion>
                df, m = recolectar(online=False, carpeta=carpeta)
                resumen.append(guardar(con, df, m, carpeta))
    con.close()
    return pd.DataFrame(resumen)


def ejecucion_referencia(db=BASE):
    """La ejecución más reciente con datos de los tres comercios."""
    con = sqlite3.connect(db)
    fila = con.execute("""SELECT id_ejecucion FROM ejecuciones WHERE estado != 'fallida'
                          GROUP BY id_ejecucion HAVING COUNT(DISTINCT comercio) = 3
                          ORDER BY id_ejecucion DESC LIMIT 1""").fetchone()
    con.close()
    return fila[0] if fila else None


# ---------------- Correspondencia entre comercios ----------------
def id_comun(r):
    partes = [r.marca_std, r.nombre_std, r.variante, f"{r.contenido:g}{r.unidad}" if pd.notna(r.contenido) else None]
    return re.sub(r"[^a-z0-9]+", "_", normalizar(" ".join(str(p) for p in partes if pd.notna(p)))).strip("_")


def proponer_correspondencia(id_ejecucion, db=BASE, quitar=(), agregar=()):
    """Grupos de la misma presentación en los tres comercios: mismo EAN (código de barras) en los tres.
    quitar: ids comunes a descartar; agregar: tuplas (sku Éxito, sku Farmatodo, sku Locatel)."""
    con = sqlite3.connect(db)
    a = pd.read_sql("""SELECT a.id_anuncio, a.comercio, a.sku, a.ean, a.nombre, a.marca_std, a.nombre_std,
                              a.variante, a.contenido, a.unidad, o.precio
                       FROM anuncios a JOIN observaciones o ON o.id_anuncio = a.id_anuncio
                       WHERE o.id_ejecucion = ? AND a.en_categoria = 1""", con, params=(id_ejecucion,))
    con.close()
    a["ean"] = a["ean"].str.lstrip("0")
    con_ean = a.dropna(subset=["ean"]).sort_values("precio", na_position="last")   # primero el que tiene precio
    ex, ft, lo = (con_ean[con_ean["comercio"] == c].drop_duplicates("ean").set_index("ean") for c in COMERCIOS)

    grupos = [["EAN", ex.loc[e], ft.loc[e], lo.loc[e]] for e in ft.index if e in ex.index and e in lo.index]
    fila = {(r.comercio, str(r.sku)): r for _, r in a.iterrows()}
    for skus in agregar:
        grupos.append(["manual", *[fila[(c, str(s))] for c, s in zip(COMERCIOS, skus)]])

    filas = []
    for regla, *miembros in grupos:
        clave = id_comun(miembros[1])
        if clave in quitar:
            continue
        for r in miembros:
            filas.append({"id_producto_comun": clave, "regla": regla, "comercio": r.comercio,
                          "id_anuncio": r.id_anuncio, "sku": r.sku, "nombre": r.nombre,
                          "variante": r.variante, "contenido": r.contenido, "unidad": r.unidad,
                          "precio": r.precio})
    return pd.DataFrame(filas).drop_duplicates(["id_producto_comun", "comercio"])


def guardar_correspondencia(tabla, db=BASE):
    """Reemplaza la correspondencia anterior: tabla productos y columnas id_producto_comun y comparable."""
    con = sqlite3.connect(db)
    con.execute("UPDATE anuncios SET id_producto_comun = NULL, comparable = 0")
    con.execute("DELETE FROM productos")
    for clave, g in tabla.groupby("id_producto_comun"):
        ref = int(g.loc[g["comercio"] == "Farmatodo", "id_anuncio"].iloc[0])
        marca, sub, nombre, variante, contenido, unidad, unidades = con.execute(
            """SELECT marca_std, subcategoria_std, nombre_std, variante, contenido, unidad, unidades_empaque
               FROM anuncios WHERE id_anuncio = ?""", (ref,)).fetchone()
        base = nombre.title() if nombre and len(nombre) > 2 else (sub or "").capitalize()   # sin nombre útil, la subcategoría
        comun = " ".join(str(p) for p in [base, variante,
                                          f"{contenido:g} {unidad}" if contenido else None] if p)
        con.execute("INSERT INTO productos VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    (clave, comun, marca, sub, nombre, variante, contenido, unidad, unidades,
                     g["regla"].iloc[0]))
        con.executemany("UPDATE anuncios SET id_producto_comun = ?, comparable = 1 WHERE id_anuncio = ?",
                        [(clave, int(i)) for i in g["id_anuncio"]])
    con.commit()
    con.close()
