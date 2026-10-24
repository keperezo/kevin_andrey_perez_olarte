"""Clase 7: partes en Python de los ejercicios 3f, 4e y 5f.
Juan Felipe Rojas Casallas

Requiere pandas, numpy y scikit-learn. Los CSV deben estar en esta misma carpeta.
El archivo ej3_imputacion_r.csv lo genera el script de R (ejercicio 3f).
"""

import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import MinMaxScaler, OneHotEncoder, StandardScaler

# ── Ejercicio 3f ───────────────────────────────────────────────────────────────
# Estrategia 2 (mediana por pclass y sex) con groupby().transform("median") y
# verificación contra lo que calculó R.

titanic = pd.read_csv("titanic.csv")
imp_r = pd.read_csv("ej3_imputacion_r.csv")  # index, oculto, age_obs, imp_clase_sexo

# La llave es index: id se repite en el dataset (solo 916 valores distintos).
edades = titanic[titanic["age"].notna()].merge(imp_r, on="index", how="inner")
assert len(edades) == 1046

# age_obs ya trae NA en las edades ocultas, así que la mediana del grupo se calcula
# solo con las visibles: no hay fuga de información.
mediana_grupo = edades.groupby(["pclass", "sex"])["age_obs"].transform("median")
edades["imp_python"] = edades["age_obs"].fillna(mediana_grupo)

iguales = np.allclose(edades["imp_python"], edades["imp_clase_sexo"])
print("Ejercicio 3f")
print("  filas comparadas:", len(edades))
print("  edades ocultas  :", int(edades["oculto"].sum()))
print("  ¿R y Python dan lo mismo?:", iguales)

mae = (edades.loc[edades["oculto"], "imp_python"]
       - edades.loc[edades["oculto"], "age"]).abs().mean()
print(f"  MAE de la estrategia 2: {mae:.4f}")

# ── Ejercicio 4e ───────────────────────────────────────────────────────────────
# Partición 70/30, MinMaxScaler ajustado SOLO con entrenamiento.

moviles = pd.read_csv("mobile_data.csv")
X = moviles.drop(columns="price_range")

X_train, X_test = train_test_split(X, test_size=0.30, random_state=2024)

escalador = MinMaxScaler().fit(X_train)          # fit solo con entrenamiento
X_test_mm = escalador.transform(X_test)

# Tolerancia: sin ella se cuentan valores que se pasan de 1 por 1e-16, que son
# redondeo de punto flotante y no observaciones fuera del rango de entrenamiento.
TOL = 1e-9
fuera_de_rango = lambda m: (m < -TOL) | (m > 1 + TOL)

fuera = fuera_de_rango(X_test_mm).sum()
total = X_test_mm.size
print("\nEjercicio 4e")
print(f"  valores de prueba fuera de [0,1]: {fuera} de {total} ({fuera / total:.3%})")

por_variable = pd.Series(fuera_de_rango(X_test_mm).sum(axis=0), index=X.columns)
print("  variables con valores fuera de rango:")
print(por_variable[por_variable > 0].to_string())

# Escalar todo junto (lo incorrecto): por construcción nada queda fuera de [0,1]
X_todo = MinMaxScaler().fit_transform(X)
print("  fuera de [0,1] al escalar todo junto:", int(fuera_de_rango(X_todo).sum()))
# El porcentaje es parecido al de R (≈ 0.01 %). No es idéntico porque la partición
# aleatoria de train_test_split no es la misma que la de sample() en R.

# ── Ejercicio 5f ───────────────────────────────────────────────────────────────
# El mismo pipeline con Pipeline y ColumnTransformer.

adult = pd.read_csv("adult.csv")
adult = adult.replace(r"^\s*\?\s*$", np.nan, regex=True)

# Mismas decisiones que en R: bandera para el centinela, país agrupado y las
# columnas que no se usan se descartan.
adult["tope_capital"] = (adult["capital.gain"] == 99999).astype(int)
adult.loc[adult["capital.gain"] == 99999, "capital.gain"] = np.nan
adult["es_estados_unidos"] = (adult["native.country"] == "United-States").astype(int)
adult["income"] = (adult["income"] == ">50K").astype(int)

cols_num = ["age", "education.num", "capital.gain", "capital.loss", "hours.per.week"]
cols_cat = ["workclass", "marital.status", "sex"]
cols_pasan = ["tope_capital", "es_estados_unidos"]

preprocesador = ColumnTransformer([
    ("num", Pipeline([
        ("imputer", SimpleImputer(strategy="median")),
        ("scaler", StandardScaler()),
    ]), cols_num),
    ("cat", Pipeline([
        ("imputer", SimpleImputer(strategy="constant", fill_value="Desconocido")),
        ("ohe", OneHotEncoder(handle_unknown="ignore", sparse_output=False)),
    ]), cols_cat),
    ("passthrough", "passthrough", cols_pasan),
])

X = adult[cols_num + cols_cat + cols_pasan]
y = adult["income"]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.30, random_state=2024, stratify=y
)

preprocesador.fit(X_train)                       # fit solo con entrenamiento
X_train_listo = preprocesador.transform(X_train)
X_test_listo = preprocesador.transform(X_test)

print("\nEjercicio 5f")
print("  entrenamiento:", X_train_listo.shape, " prueba:", X_test_listo.shape)
print("  columnas:", list(preprocesador.get_feature_names_out())[:8], "...")
print(f"  media de age en entrenamiento: {X_train_listo[:, 0].mean():.6f}")
print(f"  media de age en prueba       : {X_test_listo[:, 0].mean():.6f}")
print("  NaN restantes:", int(np.isnan(X_train_listo.astype(float)).sum()))

# ¿Por qué fit solo con entrenamiento? Porque la mediana de la imputación, la media
# y la desviación del StandardScaler y los niveles del OneHotEncoder son parámetros
# estimados a partir de los datos. Si se calculan con el dataset completo, el modelo
# entrena con información de las filas que después se usan para evaluarlo (fuga de
# información) y la evaluación queda optimista.
#
# ¿Por qué handle_unknown="ignore"? Porque en datos nuevos puede aparecer una
# categoría que no estaba en entrenamiento (por ejemplo un workclass raro que quedó
# solo en prueba). Sin esa opción el transform lanza un error; con ella la fila se
# codifica con ceros en todas las dummies de esa variable, que es exactamente lo que
# hace la función codificar() del script de R al usar los niveles del entrenamiento.
