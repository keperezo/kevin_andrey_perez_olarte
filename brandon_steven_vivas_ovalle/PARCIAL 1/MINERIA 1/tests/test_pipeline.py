"""
Prueba de humo del pipeline con datos SINTÉTICOS (no son precios reales; solo validan el código).
Ejecutar:  python tests/test_pipeline.py
Simula la API VTEX (modo legacy) de tres comercios, corre la recolección, reprocesa en modo sin
conexión y verifica que ambas bases coincidan y que la correspondencia de productos funcione.
"""
import sqlite3
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import scraper as S  # noqa: E402
import limpieza as L  # noqa: E402

NOMBRES = [  # (nombre, marca, precio base, categoría)
    ("Pañales Huggies Natural Care Etapa 3/G X 50 Und", "Huggies", 69400, "/Bebé/Pañales/"),
    ("Pañales Huggies Natural Care Etapa 3/G X 100 Und", "Huggies", 125950, "/Bebé/Pañales/"),
    ("Pañales Huggies Active Sec Etapa 3 Paquete X 50 Und", "Huggies", 62950, "/Bebé/Pañales/"),
    ("Pañales Huggies Natural Care Etapa 5/XXG X 50 Und", "Huggies", 99950, "/Bebé/Pañales/"),
    ("Pañales Winny Ultratrim Sec Etapa 3 X 30 Und", "Winny", 31000, "/Bebé/Pañales/"),
    ("Pañales Babysec Ultraprotect Etapa 3 X 50 Unidades", "Babysec", 52000, "/Bebé/Pañales/"),
    ("Pañales Pampers Confortsec Talla G 44 Unidades", "Pampers", 78000, "/Bebé/Pañales/"),
    ("Toallitas Húmedas Huggies Puro y Natural 3 X 80 Und", "Huggies", 29900, "/Bebé/Toallitas/"),
    ("Toallitas Húmedas Huggies Puro y Natural X 80 Und", "Huggies", 11000, "/Bebé/Toallitas/"),
    ("Cambiador Desechable Winny X 10 Und", "Winny", 14500, "/Bebé/Cambiadores/"),
    ("Crema Antipañalitis Desitin Barrera 57 g", "Desitin", 26000, "/Bebé/Cremas/"),
    ("Crema Antipañalitis Desitin Barrera 113 g", "Desitin", 41000, "/Bebé/Cremas/"),
    # Para excluir:
    ("Pañales para Adulto Tena Slip M X 10 Und", "Tena", 40000, "/Salud/Adulto/"),
    ("Pañalera Bolso Chicco Gris", "Chicco", 150000, "/Bebé/Accesorios/"),
    ("Toallitas Desmaquillantes Nivea X 25", "Nivea", 12000, "/Belleza/"),
    ("Pañales Winny Etapa 3 X 200 + 40 Toallitas Húmedas", "Winny", 99000, "/Bebé/Pañales/"),
    ("Crema Nistatina Antimicótica Pañalitis 30 g", "Genfar", 9000, "/Salud/"),
]
FACTOR = {"exito": 1.0, "jumbo": 1.05, "olimpica": 0.97}
# Cada comercio omite un par de productos para probar que solo cuentan los comunes:
OMITE = {"exito": {9}, "jumbo": {6}, "olimpica": {3}}


def producto(i, nombre, marca, precio, cat, factor, tienda):
    lista = round(precio * factor)
    pagar = round(lista * (0.9 if i % 5 == 0 else 1))
    return {
        "productId": f"{tienda[:1]}{i}", "productName": nombre, "brand": marca, "categories": [cat],
        "link": f"/{nombre.lower().replace(' ', '-')}/p", "description": f"Descripción de {nombre}",
        "items": [{"itemId": f"{1000 + i}", "name": nombre, "nameComplete": nombre, "ean": f"77000{i:04d}",
                   "sellers": [{"commertialOffer": {"Price": pagar, "ListPrice": lista, "AvailableQuantity": 10}}]}],
    }


class ClienteFalso:
    def __init__(self):
        self.llamadas = 0

    def get_json(self, url, params=None):
        self.llamadas += 1
        tienda = next(k for k, c in S.COMERCIOS.items() if url.startswith(c.base_url))
        if "category/tree" in url:
            return [{"id": 1, "name": "Bebé", "children": [{"id": 11, "name": "Pañales", "children": []},
                    {"id": 12, "name": "Toallitas húmedas", "children": []}]}], {}, url
        lista = [producto(i, *n, FACTOR[tienda], tienda) for i, n in enumerate(NOMBRES) if i not in OMITE[tienda]]
        a, b = params["_from"], params["_to"]
        return lista[a:b + 1], {"resources": f"{a}-{min(b, len(lista) - 1)}/{len(lista)}"}, url + "?x"


def main():
    tmp = Path(tempfile.mkdtemp())
    db, raw = tmp / "t.sqlite", tmp / "raw"
    cli = ClienteFalso()
    cli.pausa = 0
    res = S.ejecutar_recoleccion(db, raw, cliente=cli, ejecucion_id="run_TEST1")
    print(res)
    con = sqlite3.connect(db)
    L.normalizar_anuncios(con)
    rev = L.marcar_comparables(con, "run_TEST1")
    print(rev[["producto_id", "nombre_std", "comercio"]].to_string())
    print(L.resumen_descartes(con).to_string())
    n_comp = con.execute("SELECT COUNT(*) FROM productos").fetchone()[0]
    obs_comp = con.execute("SELECT COUNT(*) FROM v_precios_comparables").fetchone()[0]
    assert n_comp >= 8, n_comp
    assert obs_comp == n_comp * 3, (obs_comp, n_comp)
    # Exclusiones esperadas
    desc = dict(con.execute("SELECT nombre_original, motivo_descarte FROM anuncios WHERE incluido=0").fetchall())
    assert any("Tena" in k for k in desc) and any("Pañalera" in k for k in desc)
    assert any("Nistatina" in k for k in desc) and any("+ 40" in k for k in desc)
    # Multipaquete 3 x 80 = 240
    assert con.execute("SELECT contenido_total FROM anuncios WHERE nombre_original LIKE '%3 X 80%' LIMIT 1").fetchone()[0] == 240
    # Segunda ejecución: no sobrescribe observaciones
    n_antes = con.execute("SELECT COUNT(*) FROM observaciones").fetchone()[0]
    con.close()
    import time; time.sleep(3.2)  # la unicidad es (anuncio, fecha_hora): garantiza otra marca de tiempo
    S.ejecutar_recoleccion(db, raw, cliente=cli, ejecucion_id="run_TEST2")
    con = sqlite3.connect(db)
    assert con.execute("SELECT COUNT(*) FROM observaciones").fetchone()[0] == 2 * n_antes
    con.close()
    # Modo sin conexión: reconstruye una base nueva solo con raw/
    db2 = tmp / "t2.sqlite"
    llamadas = cli.llamadas
    S.reprocesar_raw(db2, raw)
    assert cli.llamadas == llamadas, "el modo sin conexión no debe consultar sitios"
    c2 = sqlite3.connect(db2)
    for tabla in ("anuncios", "observaciones", "ejecuciones"):
        a = sqlite3.connect(db).execute(f"SELECT COUNT(*) FROM {tabla}").fetchone()[0]
        b = c2.execute(f"SELECT COUNT(*) FROM {tabla}").fetchone()[0]
        assert a == b, (tabla, a, b)
    print("OK: pipeline completo y modo sin conexión verificados")


if __name__ == "__main__":
    main()
