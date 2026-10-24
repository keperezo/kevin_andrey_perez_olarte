"""Pruebas de procesamiento.py. Uso:  python test_procesamiento.py [ruta_a_comparador_precios.sqlite]
- Funciones puras (precios, contenido, marca).
- Modo sin conexión con HTML sintético (mismos selectores que scraper.py).
- Pipeline completo sobre una COPIA de tu base real (si se indica/existe)."""
import shutil, sqlite3, sys, tempfile
from pathlib import Path
import procesamiento as P

def ok(cond, msg):
    print(("OK   " if cond else "FALLA"), msg)
    if not cond:
        ok.fallas += 1
ok.fallas = 0

# 1. Precios
ok(P.precio_colombiano("$ 12.500") == 12500, "precio '$ 12.500' -> 12500")
ok(P.precio_colombiano("12.500,00") == 12500, "precio '12.500,00' -> 12500 (no 1.250.000)")
ok(P.precio_colombiano("0") is None and P.precio_colombiano(None) is None, "precio vacío/0 -> None")
for v, esp in [(3200024000, (24000, 32000)), (2170016275361667, (16275, 21700)), (71005680, (5680, 7100)),
               (1297113200, (12971, 13200)), (1885015080, (15080, 18850))]:
    p, l, regla = P.separar_precios(v)
    ok((p, l) == esp and regla == "separado_en_dos", f"separar_precios({v}) -> {p}, {l}")
ok(P.separar_precios(42650) == (42650.0, None, "directo"), "precio normal queda directo")
ok(P.precios_desde_texto("$32.000$24.000$361.667 por Mililitro") == (24000.0, 32000.0), "texto de tarjeta con 2 precios")
ok(P.precios_desde_texto("$ 42.650") == (42650.0, None), "texto de tarjeta con 1 precio")

# 2. Contenido, marca, formato
c = lambda t: P.parse_contenido(t)["contenido_total"]
ok(c("Repelente Autan Crema Kids X 100 Ml") == 100, "contenido 'X 100 Ml'")
ok(c("Repelente Nopikex 50Grm") == 50, "contenido '50Grm' (gramos)")
ok(c("Fly Free Extreme Niños Sprayx120ml") == 120, "contenido 'Sprayx120ml'")
ok(c("Mosqoff Stcker Citronella X 6Und") == 6, "contenido '6Und' (unidades)")
ok(c("Repelente Stay Off Spray Adultos") is None, "sin contenido -> None (no se inventa)")
ok(P.parse_contenido("Repelente 2 x 60 ml")["contenido_total"] == 120, "paquete 2 x 60 ml = 120 ml")
ok(P.es_combo("Repelente Stay Off Adulto 120 ml + Crema 60 gr"), "combo detectado")
ok(P.inferir_marca("Repelente Stay Off Spray")[0] == "STAY OFF", "marca STAY OFF (antes que OFF)")
ok(P.inferir_marca("Repelente Off! Defense Gel")[0] == "OFF", "marca OFF")
ok(P.clasificar("Alcanfor Leon 35 tabletas")[0] == 0 and P.clasificar("Repelente Autan Crema")[0] == 1, "clasificar categoría")

# 3. Modo sin conexión con HTML sintético
tmp = Path(tempfile.mkdtemp())
def html(items): return "<html><body>" + "".join(items) + "</body></html>"
(tmp / "raw/Droguerias Uno A/2026-10-10").mkdir(parents=True)
(tmp / "raw/Colsubsidio/2026-10-10").mkdir(parents=True)
(tmp / "raw/Locatel/2026-10-10").mkdir(parents=True)
(tmp / "raw/Droguerias Uno A/2026-10-10/pagina_1760132597.html").write_text(html([
    '<div class="product-details"><div class="product-title"><h4><a href="/products/repelente-x-120-ml?_pos=1">Repelente X 120 ml</a></h4></div>'
    '<div class="product-price"><h6>$ 16.600</h6></div></div>']), encoding="utf-8")
(tmp / "raw/Colsubsidio/2026-10-10/pagina_1760132598.html").write_text(html([
    '<div class="ProductItemCard__info--wrap" data-event-link-url="/repelente-y-1/p"><div class="ProductItemCard__info--wrap__title">Repelente Y 100 ml</div>'
    '<div class="ProductItemCard__info--wrap__prices--inner">$32.000$24.000$240 por Mililitro</div></div>']), encoding="utf-8")
(tmp / "raw/Locatel/2026-10-10/pagina_1760132602.html").write_text(html([
    '<section class="vtex-product-summary-2-x-container"><a class="vtex-product-summary-2-x-clearLink" href="/z/p"></a>'
    '<span class="vtex-product-summary-2-x-productBrand">Repelente Z X 60Ml</span>'
    '<span class="vtex-product-price-1-x-sellingPriceValue">$ 12.900</span></section>']), encoding="utf-8")
regs = P.leer_raw(tmp / "raw")
ok(len(regs) == 3, f"leer_raw encontró {len(regs)} productos en el HTML sintético")
col = [r for r in regs if r["comercio"] == "Colsubsidio"][0]
ok((col["precio"], col["precio_lista"]) == (24000.0, 32000.0) and col["url"].endswith("/repelente-y-1/p"), "Colsubsidio: precio 24.000, lista 32.000, URL completa")
ok(col["fecha_hora"].endswith("-05:00"), "fecha ISO con -05:00 tomada del nombre del archivo")
db_off = tmp / "off.sqlite"
ok(P.cargar_raw_en_db(tmp / "raw", db_off) == 3, "cargar_raw_en_db inserta 3 observaciones")
cx = sqlite3.connect(db_off)
res = P.reconstruir_base(cx)
ok(res["anuncios"] == 3 and cx.execute("select count(*) from precios where precio is not null").fetchone()[0] == 3, "pipeline sobre la base offline")

# 4. Pipeline sobre tu base real (copia)
ruta = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("comparador_precios.sqlite")
if ruta.exists():
    cp = tmp / "copia.sqlite"; shutil.copy(ruta, cp)
    cx = sqlite3.connect(cp)
    n_obs = cx.execute("select count(*) from observaciones").fetchone()[0]
    res = P.reconstruir_base(cx)
    print("Resumen:", res)
    ok(res["observaciones"] == n_obs, "todas las observaciones se procesaron (o se excluyeron)")
    ok(cx.execute("select max(precio) from precios").fetchone()[0] < 500000, "ningún precio absurdo (> 500.000 COP)")
    ok(cx.execute("select count(*) from precios where precio is null").fetchone()[0] == res["precio_irrecuperable"], "sin precios NULL salvo irrecuperables")
    ok(cx.execute("select count(*) from anuncios a where texto_original is null").fetchone()[0] == 0, "texto original conservado")
    antes = cx.execute("select count(*), sum(precio) from observaciones").fetchone()
    ok(antes[0] == n_obs, "tabla observaciones intacta")
    nc = P.correspondencia(cx, [r[0] for r in cx.execute("select nombre from comercios order by nombre")])
    print("Productos comunes en todos los comercios:", nc, "| en ≥2:", cx.execute("select count(*) from productos_comunes where n_comercios>=2").fetchone()[0])
else:
    print("(sin base real: pasa la ruta como argumento para probarla)")
print("\nTODO OK" if ok.fallas == 0 else f"\n{ok.fallas} FALLAS")
sys.exit(1 if ok.fallas else 0)
