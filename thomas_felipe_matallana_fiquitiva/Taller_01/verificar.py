"""Validación de integridad, cobertura, SQL, reconstrucción e idempotencia."""
import json
import re
import sqlite3
import tempfile
from contextlib import closing
from pathlib import Path
from unittest.mock import patch

from scraper import ROOT, SOURCES, run_collection, validate


def snapshot(path):
    with closing(sqlite3.connect(path)) as con:
        return {table: con.execute(f"SELECT * FROM {table} ORDER BY 1,2").fetchall()
                for table in ["comercios", "productos", "ejecuciones", "cobertura", "observaciones"]}


def main():
    reference = ROOT / "comparador_precios.sqlite"
    runs = sorted(set.intersection(*[
        {p.name for p in (ROOT / "raw" / s).iterdir() if (p / "manifest.json").exists()}
        for s in SOURCES]))
    with tempfile.TemporaryDirectory(prefix="higiene_validacion_") as tmp:
        db = Path(tmp) / "reconstruida.sqlite"
        # Cualquier acceso de red hace fallar esta prueba.
        with patch("urllib.request.urlopen", side_effect=AssertionError("Red usada en modo offline")):
            for run in runs:
                run_collection(offline=True, run_id=run, db=db)
            before = snapshot(db)
            for run in runs:
                run_collection(offline=True, run_id=run, db=db)
            assert snapshot(db) == before, "El reprocesamiento agregó o alteró datos"
        assert snapshot(reference) == before, "La base entregada difiere de las copias crudas"
        check = validate(db, runs[-1])
        assert check["cobertura_completa"], check
        assert check["observaciones_comparables"] == 24, check
        assert not check["faltantes_o_duplicados"], check
        with closing(sqlite3.connect(db)) as con:
            assert not con.execute("PRAGMA foreign_key_check").fetchall()
            sections = re.split(r"-- name: ([a-z_]+)\s*\n", (ROOT / "consultas.sql").read_text(encoding="utf-8"))
            for i in range(1, len(sections), 2):
                con.execute(sections[i+1], {"run": runs[-1]}).fetchall()
            assert con.execute("SELECT COUNT(*) FROM observaciones WHERE fecha_hora NOT LIKE '%-05:00'").fetchone()[0] == 0
        print(json.dumps({**check, "reconstruccion_identica": True,
                          "offline_sin_red": True, "idempotente": True,
                          "consultas_sql_ejecutadas": (len(sections)-1)//2}, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
