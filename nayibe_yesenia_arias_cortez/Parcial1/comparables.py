"""
comparables.py — Taller 1 Minería de Datos (UNAL): correspondencia de productos comparables.

Lee comparador_precios.sqlite (llenada por scraper.run_all) y, dentro de UNA misma ejecución, identifica las
presentaciones que están en TODOS los comercios con la regla documentada:

  R1. Mismo EAN/GTIN-13 normalizado (``anuncios.ean_norm``) en cada comercio. Es el identificador común.
  R2. Dentro de la delimitación de la categoría (``anuncios.incluido = 1``).
  R3. Con precio disponible en esa ejecución (``observaciones.disponible = 1`` y ``precio`` no nulo).
  R4. Verificación de presentación: el contenido declarado (mL o g) y el número de unidades deben coincidir entre
      comercios; si dos comercios declaran contenidos distintos, el EAN se descarta. Las diferencias de marca
      estandarizada solo se anotan (el EAN identifica al fabricante).
  R5. Si un comercio lista el mismo EAN en dos SKU, se usa el SKU cuyo contenido coincide con el de los demás
      comercios (y, en empate, el de menor código de SKU); el caso queda anotado.

Resultado en la base:
  * ``productos``: una fila por presentación comparable (id_producto_comun = EAN).
  * ``correspondencia``: qué SKU de cada comercio corresponde a cada producto, con la verificación aplicada.
  * ``anuncios`` y ``observaciones``: ``comparable = 1`` e ``id_producto_comun`` para esos SKU (todas sus
    observaciones); el resto queda en ``comparable = 0``. Volver a correrlo recalcula todo (es idempotente).

Uso:
    python comparables.py                 # usa la última ejecución con datos de todos los comercios
    python comparables.py --id 20261009T123410
    # o desde Python / Quarto:
    import comparables; tabla = comparables.marcar_comparables()
"""
from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

import scraper

REGLA = ("Mismo EAN/GTIN-13 normalizado en todos los comercios, producto dentro de la delimitación (incluido=1), "
         "precio disponible en la misma ejecución, y mismo contenido (mL/g) y número de unidades declarados.")
ORDEN_NOMBRE = ["locatel", "cafam", "larebaja"]  # comercio del que se toma el nombre de referencia
TOLERANCIA = 0.01  # 1 % de diferencia admitida en el contenido declarado

# Pregunta 5 (precio por mL entre tamaños): presentaciones del MISMO producto y formulación en distintos tamaños.
# Agrupación manual documentada: se verificó en los nombres de los tres comercios que solo cambia el tamaño.
# Un tamaño puede no estar en los tres comercios; en ese caso se compara solo donde exista (no es "comparable").
LINEAS = {
    "TIO NACHO ENGROSADOR SHAMPOO": {"650240035166": "415 mL", "650240036750": "1 L"},
    "PANTENE PRO-V MIRACLES COLAGENO NUTRE Y REVITALIZA SHAMPOO": {"7500435191425": "300 mL",
                                                                   "7500435191456": "510 mL"},
}


def ultima_ejecucion_completa(con) -> str:
    """Última ejecución en la que todos los comercios aportaron datos (estado distinto de 'fallida')."""
    fila = con.execute("""
        SELECT id_ejecucion FROM ejecuciones
        GROUP BY id_ejecucion
        HAVING SUM(estado <> 'fallida') = (SELECT COUNT(*) FROM comercios)
        ORDER BY id_ejecucion DESC LIMIT 1""").fetchone()
    if fila is None:
        raise RuntimeError("No hay ninguna ejecución con datos de todos los comercios.")
    return fila[0]


def _moda(s: pd.Series):
    s = s.dropna()
    return s.mode().iloc[0] if len(s) else None


def candidatos(con, id_ejecucion: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Aplica R1-R5. Devuelve (seleccion, descartes): una fila por (EAN, comercio) seleccionada y los EAN
    presentes en todos los comercios que no pasaron la verificación, con su motivo."""
    obs = pd.read_sql_query("""
        SELECT o.id_comercio, o.sku_comercio, o.precio, o.precio_antes_descuento, o.es_promocion, o.disponible,
               o.fecha_hora_consulta, o.url, a.ean_norm, a.nombre_original, a.marca_std, a.subcategoria_std,
               a.nombre_std, a.contenido, a.unidad, a.n_unidades
        FROM observaciones o
        JOIN anuncios a ON a.id_comercio = o.id_comercio AND a.sku_comercio = o.sku_comercio
        WHERE o.id_ejecucion = ? AND a.incluido = 1 AND a.ean_norm IS NOT NULL
          AND o.disponible = 1 AND o.precio IS NOT NULL""", con, params=(id_ejecucion,))
    n_comercios = con.execute("SELECT COUNT(*) FROM comercios").fetchone()[0]
    obs["n_unidades"] = obs["n_unidades"].fillna(1).astype(int)
    en_todos = obs.groupby("ean_norm")["id_comercio"].nunique()
    obs = obs[obs["ean_norm"].isin(en_todos[en_todos == n_comercios].index)]

    seleccion, descartes = [], []
    for ean, g in obs.groupby("ean_norm"):
        medibles = g[g["unidad"].isin(["ml", "g"]) & g["contenido"].notna()]
        ref = _moda(medibles["contenido"])
        notas = []
        # R5: un SKU por comercio
        filas = []
        for comercio, gc in g.groupby("id_comercio"):
            if len(gc) > 1:
                gc = gc.assign(_ok=(gc["contenido"] - (ref or 0)).abs() <= (ref or 0) * TOLERANCIA)
                gc = gc.sort_values(["_ok", "sku_comercio"], ascending=[False, True]).drop(columns="_ok")
                notas.append(f"{comercio} lista {len(gc)} SKU con este EAN; se usa {gc.iloc[0]['sku_comercio']}")
            filas.append(gc.iloc[0])
        g = pd.DataFrame(filas)
        # R4: contenido y unidades
        medibles = g[g["unidad"].isin(["ml", "g"]) & g["contenido"].notna()]
        motivo = None
        if len(medibles) >= 2 and (medibles["contenido"].max() - medibles["contenido"].min()) > \
                TOLERANCIA * medibles["contenido"].max():
            motivo = "contenido declarado distinto: " + ", ".join(
                f"{r.id_comercio}={r.contenido:g} {r.unidad}" for r in medibles.itertuples())
        elif g["n_unidades"].nunique() > 1:
            motivo = "número de unidades distinto: " + ", ".join(
                f"{r.id_comercio}={r.n_unidades}" for r in g.itertuples())
        if motivo:
            descartes.append({"id_producto_comun": ean, "nombre": g["nombre_original"].iloc[0], "motivo": motivo})
            continue
        if medibles["unidad"].nunique() > 1:
            notas.append("mismo contenido declarado en mL y en g según el comercio")
        if len(medibles) < len(g):
            sin = sorted(set(g["id_comercio"]) - set(medibles["id_comercio"]))
            notas.append(f"sin contenido en mL/g en: {', '.join(sin)}")
        if g["marca_std"].nunique() > 1:
            notas.append("marca estandarizada distinta: " + ", ".join(
                f"{r.id_comercio}={r.marca_std}" for r in g.itertuples()))
        g = g.assign(id_producto_comun=ean, verificacion="ok" if not notas else "ok; " + "; ".join(notas))
        seleccion.append(g)
    sel = pd.concat(seleccion, ignore_index=True) if seleccion else pd.DataFrame()
    return sel, pd.DataFrame(descartes, columns=["id_producto_comun", "nombre", "motivo"])


def _tabla_productos(sel: pd.DataFrame, id_ejecucion: str) -> pd.DataFrame:
    filas = []
    for ean, g in sel.groupby("id_producto_comun"):
        g = g.set_index("id_comercio")
        ref = next((g.loc[c] for c in ORDEN_NOMBRE if c in g.index), g.iloc[0])
        medibles = g[g["unidad"].isin(["ml", "g"])]
        filas.append({"id_producto_comun": ean, "marca_std": _moda(g["marca_std"]), "nombre_std": ref["nombre_std"],
                      "subcategoria_std": _moda(g["subcategoria_std"]), "contenido": _moda(medibles["contenido"]),
                      "unidad": _moda(medibles["unidad"]), "n_unidades": int(_moda(g["n_unidades"]) or 1),
                      "linea": None, "regla_correspondencia": REGLA, "id_ejecucion_verificacion": id_ejecucion})
    return pd.DataFrame(filas)


def marcar_comparables(db_path: Path = scraper.DB_PATH, id_ejecucion: str | None = None, minimo: int = 8,
                       mostrar: bool = True) -> pd.DataFrame:
    """Marca los comparables en la base y devuelve la tabla lado a lado (una fila por producto)."""
    con = scraper.conectar(db_path)
    try:
        id_ejecucion = id_ejecucion or ultima_ejecucion_completa(con)
        sel, descartes = candidatos(con, id_ejecucion)
        prod = _tabla_productos(sel, id_ejecucion) if len(sel) else pd.DataFrame()
        with con:
            # Recalcular desde cero: quitar marcas anteriores
            con.execute("UPDATE observaciones SET comparable = 0, id_producto_comun = NULL")
            con.execute("UPDATE anuncios SET comparable = 0, id_producto_comun = NULL")
            con.execute("DELETE FROM correspondencia")
            con.execute("DELETE FROM productos")
            con.execute("DELETE FROM lineas")
            con.executemany("INSERT INTO lineas VALUES (?,?,?)",
                            [(linea, scraper.normalizar_ean(e), tam) for linea, eans in LINEAS.items()
                             for e, tam in eans.items()])
            if len(prod):
                lin = {scraper.normalizar_ean(e): linea for linea, eans in LINEAS.items() for e in eans}
                prod["linea"] = prod["id_producto_comun"].map(lin)
            if len(sel):
                con.executemany(f"INSERT INTO productos ({','.join(prod.columns)}) "
                                f"VALUES ({','.join('?' * len(prod.columns))})",
                                [tuple(map(scraper._sql_valor, r)) for r in prod.itertuples(index=False)])
                claves = [(r.id_producto_comun, r.id_comercio, r.sku_comercio) for r in sel.itertuples()]
                con.executemany("UPDATE anuncios SET comparable = 1, id_producto_comun = ? "
                                "WHERE id_comercio = ? AND sku_comercio = ?", claves)
                con.executemany("UPDATE observaciones SET comparable = 1, id_producto_comun = ? "
                                "WHERE id_comercio = ? AND sku_comercio = ?", claves)
                con.executemany(
                    "INSERT INTO correspondencia VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
                    [tuple(map(scraper._sql_valor, (r.id_producto_comun, r.id_comercio, r.sku_comercio, id_ejecucion,
                                                    r.nombre_original, r.marca_std, r.contenido, r.unidad,
                                                    r.n_unidades, r.precio, REGLA, r.verificacion)))
                     for r in sel.itertuples()])
        n_obs = con.execute("SELECT COUNT(*) FROM observaciones WHERE comparable = 1 AND id_ejecucion = ?",
                            (id_ejecucion,)).fetchone()[0]
    finally:
        con.close()

    lado = _lado_a_lado(sel) if len(sel) else pd.DataFrame()
    if mostrar:
        _imprimir(sel, descartes, id_ejecucion, n_obs, minimo)
    return lado


def _lado_a_lado(sel: pd.DataFrame) -> pd.DataFrame:
    """Una fila por producto común; por comercio: nombre original, contenido, precio, disponible, es_promocion."""
    campos = ["nombre_original", "contenido", "unidad", "precio", "precio_antes_descuento", "disponible", "es_promocion"]
    ancho = sel.pivot(index="id_producto_comun", columns="id_comercio", values=campos)
    ancho.columns = [f"{c}_{campo}" for campo, c in ancho.columns]
    orden = sorted(ancho.columns, key=lambda c: (c.split("_")[0], campos.index(c.split("_", 1)[1])))
    return ancho[orden]


def _imprimir(sel, descartes, id_ejecucion, n_obs, minimo) -> None:
    n = sel["id_producto_comun"].nunique() if len(sel) else 0
    comercios = sorted(sel["id_comercio"].unique()) if len(sel) else []
    print(f"Ejecución {id_ejecucion}: {n} productos comparables, {n_obs} observaciones comparables "
          f"({len(comercios)} comercios).")
    if n < minimo:
        print(f"ATENCIÓN: se requieren al menos {minimo} productos ({minimo * max(len(comercios), 3)} observaciones).")
    for ean, g in sel.groupby("id_producto_comun"):
        print(f"\n[{ean}] verificación: {g['verificacion'].iloc[0]}")
        for r in g.sort_values("id_comercio").itertuples():
            cont = f"{r.contenido:g} {r.unidad}" if pd.notna(r.contenido) else "s/d"
            antes = f" (antes {r.precio_antes_descuento:,.0f})" if pd.notna(r.precio_antes_descuento) else ""
            print(f"  {r.id_comercio:9s} {r.nombre_original[:70]:70s} {cont:>9s}  ${r.precio:>9,.0f}{antes}"
                  f"  disp={r.disponible} promo={r.es_promocion}")
    if len(descartes):
        print(f"\nDescartados por la verificación ({len(descartes)}):")
        for r in descartes.itertuples():
            print(f"  [{r.id_producto_comun}] {r.nombre[:60]} -> {r.motivo}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Marca los productos comparables en comparador_precios.sqlite")
    ap.add_argument("--id", default=None, help="id de ejecución (por defecto, la última con todos los comercios)")
    ap.add_argument("--db", default=str(scraper.DB_PATH), help="ruta de la base SQLite")
    a = ap.parse_args()
    marcar_comparables(db_path=Path(a.db), id_ejecucion=a.id)
