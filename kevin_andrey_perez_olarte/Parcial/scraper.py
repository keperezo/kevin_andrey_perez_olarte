import os
import sqlite3
import requests
from bs4 import BeautifulSoup
from datetime import datetime, timezone, timedelta
import uuid
import time

# Configuración de zona horaria (Colombia UTC-5)
TZ_COL = timezone(timedelta(hours=-5))
DIRECTORIO_ACTUAL = os.path.dirname(os.path.abspath(__file__))
DB_PATH = os.path.join(DIRECTORIO_ACTUAL, "comparador_precios.sqlite")

def setup_database(db_name=DB_PATH):
    """Crea la estructura de la base de datos SQLite."""
    conn = sqlite3.connect(db_name)
    cursor = conn.cursor()
    
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS ejecuciones (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        id_ejecucion TEXT,
        fecha_inicio TEXT,
        comercio TEXT,
        registros_obtenidos INTEGER,
        estado TEXT
    )''')
    
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS observaciones (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        comercio TEXT,
        producto TEXT,
        marca_subcategoria TEXT,
        presentacion TEXT,
        precio REAL,
        promocion INTEGER,
        url_extraccion TEXT,
        texto_original TEXT,
        fecha_hora_consulta TEXT,
        comparable INTEGER DEFAULT 0,
        id_ejecucion TEXT
    )''')
    
    conn.commit()
    return conn

def save_raw_data(comercio, content, fecha_str):
    """Guarda copia cruda del HTML."""
    fecha_corta = fecha_str.split("T")[0]
    folder_path = os.path.join(DIRECTORIO_ACTUAL, "raw", comercio, fecha_corta)
    os.makedirs(folder_path, exist_ok=True)
    
    filename = f"pagina_{int(time.time())}.html"
    filepath = os.path.join(folder_path, filename)
    with open(filepath, 'wb') as f:
        f.write(content)
    return filepath

def extract_html_store(comercio, base_url, url_busqueda, db_conn, id_ejecucion, selectores):
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        "Accept-Language": "es-CO,es;q=0.9"
    }
    
    fecha_inicio = datetime.now(TZ_COL).isoformat()
    registros = 0
    estado = "completa"
    cursor = db_conn.cursor()
    
    print(f"Iniciando recolección en {comercio}...")
    
    try:
        response = requests.get(url_busqueda, headers=headers, timeout=15)
        
        if response.status_code == 200:
            save_raw_data(comercio, response.content, fecha_inicio)
            soup = BeautifulSoup(response.text, 'html.parser')
            productos = soup.select(selectores['contenedor'])
            
            if not productos:
                print(f"No se encontraron productos en {comercio}.")
            
            for item in productos:
                try:
                    elem_nombre = item.select_one(selectores['nombre'])
                    texto_original = elem_nombre.get_text(strip=True) if elem_nombre else "Sin nombre"
                    
                    elem_precio = item.select_one(selectores['precio'])
                    precio_texto = elem_precio.get_text(strip=True) if elem_precio else "0"
                    precio_limpio = float(''.join(filter(str.isdigit, precio_texto)))
                    
                    if precio_limpio == 0:
                        continue
                        
                    elem_url = item.select_one(selectores['enlace'])
                    url_producto = elem_url.get('href', "") if elem_url else ""
                    if url_producto.startswith("/"):
                        url_producto = base_url + url_producto
                        
                    fecha_hora_consulta = datetime.now(TZ_COL).isoformat()
                    
                    cursor.execute('''
                    INSERT INTO observaciones 
                    (comercio, marca_subcategoria, precio, promocion, url_extraccion, texto_original, fecha_hora_consulta, id_ejecucion)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (comercio, "Por clasificar", precio_limpio, 0, url_producto, texto_original, fecha_hora_consulta, id_ejecucion))
                    
                    registros += 1
                except Exception:
                    continue 
        else:
            print(f"Error HTTP {response.status_code} en {comercio}.")
            estado = "parcial o fallida"
            
    except Exception as e:
        print(f"Fallo de conexión en {comercio}: {e}")
        estado = "fallida"
        
    cursor.execute('''
    INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado)
    VALUES (?, ?, ?, ?, ?)
    ''', (id_ejecucion, fecha_inicio, comercio, registros, estado))
    
    db_conn.commit()
    print(f"Terminado {comercio}. Registros extraídos: {registros}")
    return registros
def scrape_uno_a(categoria, db_conn, id_ejecucion):
    """Descarga de Droguerías Uno A basado en la inspección de la imagen"""
    comercio = "Droguerias Uno A"
    
    # URL construida con la estructura de la imagen que proporcionaste
    url_busqueda = f"https://drogueriasunoa.com/search?q={categoria}&options%5Bprefix%5D=last"
    
    # Selectores deducidos a partir de tu captura de pantalla
    selectores = {
        'contenedor': 'div.product-details',
        'nombre': 'div.product-title h4 a',
        'precio': 'div.product-price h6',
        'enlace': 'div.product-title h4 a'
    }
    
    # Llamamos a la función genérica
    return extract_html_store("Droguerias Uno A", "https://drogueriasunoa.com", url_busqueda, db_conn, id_ejecucion, selectores)
def scrape_laeconomia(categoria, db_conn, id_ejecucion):
    """Descarga de Droguería La Economía (Reemplaza a Farmatodo)"""
    comercio = "La Economia"
    url = f"https://www.droguerialaeconomia.com/search?name={categoria}"
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    fecha_inicio = datetime.now(TZ_COL).isoformat()
    registros = 0
    estado = "completa"
    cursor = db_conn.cursor()
    
    print(f"Iniciando descarga en {comercio}...")
    try:
        res = requests.get(url, headers=headers, timeout=15)
        if res.status_code == 200:
            save_raw_data(comercio, res.content, fecha_inicio)
            soup = BeautifulSoup(res.text, 'html.parser')
            
            # Contenedor basado en tu fragmento HTML
            productos = soup.select('div.general__content')
            
            for item in productos:
                # Nombre
                nombre_elem = item.select_one('h3.prod__name')
                texto_original = nombre_elem.get_text(strip=True) if nombre_elem else "Sin nombre"
                
                # Precio
                precio_elem = item.select_one('p.base__price')
                precio_texto = precio_elem.get_text(strip=True) if precio_elem else "0"
                precio_limpio = float(''.join(filter(str.isdigit, precio_texto)) or 0)
                
                # Enlace: Como en tu HTML no se ve la etiqueta <a>, buscamos si el div está envuelto en un enlace
                enlace_elem = item.find_parent('a') or item.select_one('a')
                url_producto = enlace_elem['href'] if enlace_elem and 'href' in enlace_elem.attrs else url
                if url_producto.startswith("/"):
                    url_producto = "https://www.droguerialaeconomia.com" + url_producto
                
                if precio_limpio > 0:
                    fecha_hora = datetime.now(TZ_COL).isoformat()
                    cursor.execute('''
                    INSERT INTO observaciones (comercio, marca_subcategoria, precio, promocion, url_extraccion, texto_original, fecha_hora_consulta, id_ejecucion)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (comercio, "Por clasificar", precio_limpio, 0, url_producto, texto_original, fecha_hora, id_ejecucion))
                    registros += 1
        else:
            estado = "fallida"
    except Exception:
        estado = "fallida"

    cursor.execute('INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado) VALUES (?, ?, ?, ?, ?)', 
                   (id_ejecucion, fecha_inicio, comercio, registros, estado))
    db_conn.commit()
    print(f"Terminado {comercio}. Registros: {registros}")

def scrape_colsubsidio(categoria, db_conn, id_ejecucion):
    """Descarga de Colsubsidio (2 páginas)"""
    comercio = "Colsubsidio"
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    fecha_inicio = datetime.now(TZ_COL).isoformat()
    registros = 0
    estado = "completa"
    cursor = db_conn.cursor()
    
    print(f"Iniciando descarga en {comercio}...")
    for pagina in range(1, 3):
        url = f"https://www.drogueriascolsubsidio.com/{categoria}?_q={categoria}&map=ft" if pagina == 1 else f"https://www.drogueriascolsubsidio.com/{categoria}?page={pagina}&map=ft&order=OrderByScoreDESC"
        
        try:
            res = requests.get(url, headers=headers, timeout=15)
            if res.status_code == 200:
                save_raw_data(comercio, res.content, fecha_inicio)
                soup = BeautifulSoup(res.text, 'html.parser')
                productos = soup.select('div.ProductItemCard__info--wrap')
                
                if not productos:
                    break
                    
                for item in productos:
                    nombre_elem = item.select_one('div.ProductItemCard__info--wrap__title')
                    texto_original = nombre_elem.get_text(strip=True) if nombre_elem else "Sin nombre"
                    
                    precio_elem = item.select_one('div.ProductItemCard__info--wrap__prices--inner')
                    precio_texto = precio_elem.get_text(strip=True).split('Mililitro')[0] if precio_elem else "0"
                    precio_limpio = float(''.join(filter(str.isdigit, precio_texto)) or 0)
                    
                    url_producto = item.get('data-event-link-url', '')
                    if url_producto.startswith("/"):
                        url_producto = "https://www.drogueriascolsubsidio.com" + url_producto
                    
                    if precio_limpio > 0:
                        fecha_hora = datetime.now(TZ_COL).isoformat()
                        cursor.execute('''
                        INSERT INTO observaciones (comercio, marca_subcategoria, precio, promocion, url_extraccion, texto_original, fecha_hora_consulta, id_ejecucion)
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                        ''', (comercio, "Por clasificar", precio_limpio, 0, url_producto, texto_original, fecha_hora, id_ejecucion))
                        registros += 1
            else:
                estado = "parcial"
        except Exception:
            estado = "fallida"
        time.sleep(2)

    cursor.execute('INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado) VALUES (?, ?, ?, ?, ?)', 
                   (id_ejecucion, fecha_inicio, comercio, registros, estado))
    db_conn.commit()
    print(f"Terminado {comercio}. Registros: {registros}")

def scrape_locatel(categoria, db_conn, id_ejecucion):
    """Descarga de Locatel (VTEX - Captura los primeros 8 productos mostrados en el DOM)"""
    comercio = "Locatel"
    url = f"https://www.locatelcolombia.com/{categoria}?_q={categoria}&map=ft"
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    fecha_inicio = datetime.now(TZ_COL).isoformat()
    registros = 0
    estado = "completa"
    cursor = db_conn.cursor()
    
    print(f"Iniciando descarga en {comercio}...")
    try:
        res = requests.get(url, headers=headers, timeout=15)
        if res.status_code == 200:
            save_raw_data(comercio, res.content, fecha_inicio)
            soup = BeautifulSoup(res.text, 'html.parser')
            productos = soup.select('section.vtex-product-summary-2-x-container')
            
            for item in productos:
                nombre_elem = item.select_one('span.vtex-product-summary-2-x-productBrand')
                texto_original = nombre_elem.get_text(strip=True) if nombre_elem else "Sin nombre"
                
                precio_elem = item.select_one('span.vtex-product-price-1-x-sellingPriceValue')
                precio_texto = precio_elem.get_text(strip=True) if precio_elem else "0"
                precio_limpio = float(''.join(filter(str.isdigit, precio_texto)) or 0)
                
                enlace_elem = item.select_one('a.vtex-product-summary-2-x-clearLink')
                url_producto = enlace_elem['href'] if enlace_elem and 'href' in enlace_elem.attrs else ""
                if url_producto.startswith("/"):
                    url_producto = "https://www.locatelcolombia.com" + url_producto
                
                if precio_limpio > 0:
                    fecha_hora = datetime.now(TZ_COL).isoformat()
                    cursor.execute('''
                    INSERT INTO observaciones (comercio, marca_subcategoria, precio, promocion, url_extraccion, texto_original, fecha_hora_consulta, id_ejecucion)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (comercio, "Por clasificar", precio_limpio, 0, url_producto, texto_original, fecha_hora, id_ejecucion))
                    registros += 1
        else:
            estado = "fallida"
    except Exception:
        estado = "fallida"

    cursor.execute('INSERT INTO ejecuciones (id_ejecucion, fecha_inicio, comercio, registros_obtenidos, estado) VALUES (?, ?, ?, ?, ?)', 
                   (id_ejecucion, fecha_inicio, comercio, registros, estado))
    db_conn.commit()
    print(f"Terminado {comercio}. Registros: {registros} (Límite VTEX inicial alcanzado)")

if __name__ == "__main__":
    if os.path.exists(DB_PATH):
        os.remove(DB_PATH)
        
    conn = setup_database()
    id_ejecucion = str(uuid.uuid4())
    print(f"--- Arrancando Extracción General: {id_ejecucion} ---")
    
    # Aquí llamamos a la nueva tienda
    scrape_uno_a("repelente", conn, id_ejecucion)
    scrape_colsubsidio("repelente", conn, id_ejecucion)
    scrape_locatel("repelente", conn, id_ejecucion)
    
    conn.close()
    print("--- Proceso Completado. Base de datos actualizada. ---")