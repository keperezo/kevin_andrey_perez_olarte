import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer

# 1. Cargar datos
adult = pd.read_csv("adult.csv")
adult.replace("?", np.nan, inplace=True)

# 2. Definir variables
num_cols = ["age", "hours.per.week", "capital.gain", "capital.loss"]
cat_cols = ["workclass", "marital.status", "sex"]

X = adult[num_cols + cat_cols].copy()
y = (adult["income"].str.contains(">50K")).astype(int)

# 3. Partición 70/30
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.30, random_state=2024)

# 4. Pipeline con ColumnTransformer
# handle_unknown='ignore' evita errores si el test contiene categorías que no aparecieron en el train
preprocesador = ColumnTransformer(
    transformers=[
        ("num", Pipeline([
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler())
        ]), num_cols),
        ("cat", Pipeline([
            ("imputer", SimpleImputer(strategy="constant", fill_value="Desconocido")),
            ("ohe", OneHotEncoder(handle_unknown="ignore", sparse_output=False))
        ]), cat_cols)
    ]
)

# 5. Ajuste ÚNICAMENTE con entrenamiento
preprocesador.fit(X_train)

X_train_proc = preprocesador.transform(X_train)
X_test_proc  = preprocesador.transform(X_test)

print("=" * 55)
print("Pipeline de scikit-learn finalizado con éxito:")
print(f"Dimensiones X_train procesado: {X_train_proc.shape}")
print(f"Dimensiones X_test procesado:  {X_test_proc.shape}")
print("=" * 55)

# Justificación teórica:
# 'fit' se ejecuta exclusivamente sobre X_train para evitar fuga de información 
# (data leakage). 'handle_unknown="ignore"' es fundamental en producción porque si un nuevo
# cliente registra una categoría laboral nunca antes vista, el modelo genera un vector 
# de ceros en lugar de romper el pipeline con una excepción.