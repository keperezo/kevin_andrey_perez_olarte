import pandas as pd
import numpy as np

# 1. Cargar datos (asegúrate de haber subido 'titanic.csv' al entorno de Colab)
df = pd.read_csv("titanic.csv")

# 2. Filtrar pasajeros con edad conocida (1046 registros)
df_conocidos = df[df["age"].notna()].copy().reset_index(drop=True)

# 3. Ocultar el 20% de las edades al azar con semilla 2024
np.random.seed(2024)
n_total = len(df_conocidos)
n_ocultos = int(0.20 * n_total)
idx_ocultos = np.random.choice(df_conocidos.index, size=n_ocultos, replace=False)

df_conocidos["age_real"] = df_conocidos["age"]
df_conocidos["age_obs"] = df_conocidos["age"]
df_conocidos.loc[idx_ocultos, "age_obs"] = np.nan

# 4. ESTRATEGIA 2: Imputación por clase y sexo con groupby().transform()
# Se calcula la mediana usando ÚNICAMENTE las edades observadas (age_obs)
df_conocidos["mediana_cs"] = (
    df_conocidos.groupby(["pclass", "sex"])["age_obs"]
    .transform("median")
)

# Imputar valores faltantes
df_conocidos["age_imp_cs"] = df_conocidos["age_obs"].fillna(df_conocidos["mediana_cs"])

# 5. Evaluación del Error Absoluto Medio (MAE) sobre el 20% oculto
eval_ocultos = df_conocidos.loc[idx_ocultos]
mae_python = (eval_ocultos["age_real"] - eval_ocultos["age_imp_cs"]).abs().mean()
rmse_python = np.sqrt(((eval_ocultos["age_real"] - eval_ocultos["age_imp_cs"]) ** 2).mean())

print("=" * 50)
print(f"MAE en Python (Pclass + Sex):  {mae_python:.4f} años")
print(f"RMSE en Python (Pclass + Sex): {rmse_python:.4f} años")
print("=" * 50)

# 6. Verificación: Medianas calculadas por grupo (idénticas a las de R)
print("\nMedianas por grupo utilizadas para la imputación:")
tabla_medianas = (
    df_conocidos.dropna(subset=["age_obs"])
    .groupby(["pclass", "sex"])["age_obs"]
    .median()
    .reset_index(name="mediana_edad")
)
print(tabla_medianas)