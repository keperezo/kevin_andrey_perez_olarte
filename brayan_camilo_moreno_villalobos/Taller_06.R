
library(tidyverse)
#_____________________________________________
#PARTE 1: EJERCICIO HOUSE PRICES
#_____________________________________________
  

df_house <- read_csv("house_prices.csv")

#-----------------------------------------------------------------------------
# a. Porcentaje de NA y columnas que se perderían
#-----------------------------------------------------------------------------
  
  na_porcentaje <- df_house %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "pct_na")

columnas_a_eliminar <- na_porcentaje %>%
  filter(pct_na > 50) %>%
  pull(columna)

cat("--- HOUSE PRICES: Parte A ---\n")
cat("Columnas con más del 50% de NA:\n")
print(columnas_a_eliminar)

# #Explicación: Al eliminar PoolQC (que tiene muchos NA porque casi nadie tiene
# piscina), se pierde la información de calidad (excelente, buena, etc.) de las
#pocas viviendas que SÍ tienen piscina, afectando la valoración de esas propiedades.
# -----------------------------------------------------------------------------
#   b. Faltantes reales vs estructurales
# -----------------------------------------------------------------------------
  
  garage_cols <- df_house %>% select(starts_with("Garage") & where(is.character)) %>% names()
bsmt_cols <- df_house %>% select(starts_with("Bsmt") & where(is.character)) %>% names()

faltantes_reales_garage <- df_house %>%
  filter(if_any(all_of(garage_cols), is.na) & (GarageArea > 0 | is.na(GarageArea))) %>%
  nrow()

faltantes_reales_bsmt <- df_house %>%
  filter(if_any(all_of(bsmt_cols), is.na) & (TotalBsmtSF > 0 | is.na(TotalBsmtSF))) %>%
  nrow()

faltantes_reales_pool <- df_house %>%
  filter(is.na(PoolQC) & (PoolArea > 0 | is.na(PoolArea))) %>%
  nrow()

cat("\n--- HOUSE PRICES: Parte B ---\n")
cat("Faltantes reales - Garage:", faltantes_reales_garage, "\n")
cat("Faltantes reales - Bsmt:", faltantes_reales_bsmt, "\n")
cat("Faltantes reales - Pool:", faltantes_reales_pool, "\n")

# -----------------------------------------------------------------------------
# c. Función recodificar_ausencia con purrr
# -----------------------------------------------------------------------------
  
  recodificar_ausencia <- function(df, cols, area) {
    df %>%
      mutate(across(
        all_of(cols) & where(is.character),
        ~ if_else(is.na(.) & .data[[area]] == 0 & !is.na(.data[[area]]), "Ninguno", .)
      ))
  }

especificaciones <- list(
  list(cols = garage_cols, area = "GarageArea"),
  list(cols = bsmt_cols, area = "TotalBsmtSF"),
  list(cols = "PoolQC", area = "PoolArea")
)

df_recodificado <- reduce(especificaciones,
                          .f = ~ recodificar_ausencia(.x, .y$cols, .y$area),
                          .init = df_house)

# -----------------------------------------------------------------------------
#   d. Imputación de LotFrontage y comparación
# -----------------------------------------------------------------------------
  
  mediana_global <- median(df_recodificado$LotFrontage, na.rm = TRUE)

df_imputado <- df_recodificado %>%
  group_by(Neighborhood) %>%
  mutate(
    mediana_barrio = median(LotFrontage, na.rm = TRUE),
    LotFrontage_neigh = if_else(is.na(LotFrontage), mediana_barrio, LotFrontage)
  ) %>%
  ungroup() %>%
  mutate(
    LotFrontage_global = if_else(is.na(LotFrontage), mediana_global, LotFrontage),
    diferencia_imputacion = LotFrontage_neigh - LotFrontage_global
  )

# Gráfico de la diferencia media por barrio (usando print() para que renderice si es un script)

plot_imputacion <- df_imputado %>%
  group_by(Neighborhood) %>%
  summarise(dif_media = mean(abs(diferencia_imputacion), na.rm = TRUE)) %>%
  filter(dif_media > 0) %>%
  ggplot(aes(x = reorder(Neighborhood, dif_media), y = dif_media)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(title = "Diferencia Media Absoluta: Imputación por Barrio vs Global",
       x = "Barrio", y = "Diferencia") +
  theme_minimal()

print(plot_imputacion)
# ----------------------------------------------------------------------------
#   e. Valores imposibles en edad de vivienda
# -----------------------------------------------------------------------------
  
  df_limpio <- df_imputado %>%
  mutate(
    edad_vivienda_temp = YrSold - YearBuilt,
    dato_corregido = case_when(
      edad_vivienda_temp < 0 ~ "Edad negativa (eliminada)",
      GarageYrBlt > YrSold ~ "Año garaje > Año venta (corregido)",
      YrSold > 2010 | YearBuilt > 2010 | GarageYrBlt > 2010 ~ "Año > 2010 (convertido a NA)",
      TRUE ~ "Sin corrección"
    ),
    YrSold = if_else(YrSold > 2010, NA_real_, YrSold),
    YearBuilt = if_else(YearBuilt > 2010, NA_real_, YearBuilt),
    GarageYrBlt = if_else(GarageYrBlt > 2010, NA_real_, GarageYrBlt),
    GarageYrBlt = if_else(GarageYrBlt > YrSold, YrSold, GarageYrBlt),
    edad_vivienda = YrSold - YearBuilt,
    edad_vivienda = if_else(edad_vivienda < 0, NA_real_, edad_vivienda)
  ) %>%
  select(-edad_vivienda_temp)
# -----------------------------------------------------------------------------
#   f. Discretización de edad_vivienda: cut vs ntile
# -----------------------------------------------------------------------------
  
  df_final <- df_limpio %>%
  filter(!is.na(edad_vivienda)) %>%
  mutate(
    grupo_cut = cut(edad_vivienda,
                    breaks = quantile(edad_vivienda, probs = 0:4/4, na.rm = TRUE),
                    include.lowest = TRUE),
    grupo_ntile = factor(ntile(edad_vivienda, 4))
  )

precio_por_cut <- df_final %>%
  group_by(grupo_cut) %>%
  summarise(mediana_precio = median(SalePrice, na.rm = TRUE), n = n())

precio_por_ntile <- df_final %>%
  group_by(grupo_ntile) %>%
  summarise(mediana_precio = median(SalePrice, na.rm = TRUE), n = n())

cat("\n--- HOUSE PRICES: Parte F ---\n")
cat("\nResumen usando cut():\n")
print(precio_por_cut)
cat("\nResumen usando ntile():\n")
print(precio_por_ntile)

# Explicación: ntile() fuerza la creación de N grupos con el mismo número exacto
# de filas. Si hay empates (casas con la misma edad), ntile() las dividirá al azar
# en distintos grupos. cut() respeta el valor matemático, asignando siempre el
# mismo grupo a la misma edad, aunque los tamaños de los grupos queden desbalanceados.

#_____________________________________________
#PARTE 1: EJERCICIO TITANIC
#_____________________________________________

# -----------------------------------------------------------------------------
# a. Criterio IQR global sobre fare (k = 1.5 y k = 3)
# -----------------------------------------------------------------------------
# Calculamos los límites globales
q1_global <- quantile(df$fare, 0.25, na.rm = TRUE)
q3_global <- quantile(df$fare, 0.75, na.rm = TRUE)
iqr_global <- q3_global - q1_global

lim_sup_15 <- q3_global + 1.5 * iqr_global
lim_sup_30 <- q3_global + 3.0 * iqr_global

# Marcamos los outliers y calculamos los porcentajes
df <- df %>%
  mutate(
    outlier_global_15 = fare > lim_sup_15,
    outlier_global_30 = fare > lim_sup_30
  )

porcentaje_outliers_15 <- mean(df$outlier_global_15, na.rm = TRUE) * 100
porcentaje_outliers_30 <- mean(df$outlier_global_30, na.rm = TRUE) * 100

cat("Porcentaje de outliers (k=1.5):", round(porcentaje_outliers_15, 2), "%\n")
cat("Porcentaje de outliers (k=3.0):", round(porcentaje_outliers_30, 2), "%\n")

# -----------------------------------------------------------------------------
# b. Criterio IQR dentro de cada pclass
# -----------------------------------------------------------------------------
df <- df %>%
  group_by(pclass) %>%
  mutate(
    q1_clase = quantile(fare, 0.25, na.rm = TRUE),
    q3_clase = quantile(fare, 0.75, na.rm = TRUE),
    iqr_clase = q3_clase - q1_clase,
    lim_sup_clase_15 = q3_clase + 1.5 * iqr_clase,
    outlier_clase_15 = fare > lim_sup_clase_15
  ) %>%
  ungroup()

# ¿Cuántos pasajeros de primera clase dejan de ser outliers?
pasajeros_1ra_dejan_outliers <- df %>%
  filter(pclass == 1, outlier_global_15 == TRUE, outlier_clase_15 == FALSE) %>%
  nrow()

cat("Pasajeros de 1ra clase que dejan de ser outliers:", pasajeros_1ra_dejan_outliers, "\n")

# Explicación:
# Comparar una tarifa de primera clase con la distribución global es injusto porque 
# las clases agrupan poblaciones con niveles adquisitivos completamente distintos. 
# Una tarifa de £80 puede ser un outlier extremo si la comparas con los de tercera 
# clase (donde pagaban £8), pero es el precio normal y esperado para la primera clase.

# -----------------------------------------------------------------------------
# c. Implementación del z-score robusto
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    z_robusto = (fare - median(fare, na.rm = TRUE)) / mad(fare, na.rm = TRUE),
    outlier_z_robusto = abs(z_robusto) > 3.5
  )

# Explicación de la ventaja:
# El z-score clásico usa la media y la desviación estándar. Ambas medidas son muy 
# sensibles a valores extremos: un solo millonario en el barco infla la media y 
# la desviación, "escondiendo" otros posibles outliers (efecto de enmascaramiento). 
# El z-score robusto usa la mediana y la Desviación Absoluta de la Mediana (MAD), 
# que no se ven afectadas por valores extremos, detectándolos de forma más confiable.

# -----------------------------------------------------------------------------
# d. Tabla cruzada de los tres criterios
# -----------------------------------------------------------------------------
tabla_cruzada <- df %>%
  count(outlier_global_15, outlier_clase_15, outlier_z_robusto)

print("Tabla de contingencia de criterios de outliers:")
print(tabla_cruzada)

# Análisis de la tabla:
# Dependiendo de los datos, encontrarás combinaciones como pasajeros marcados 
# por la regla global y z-robusto, pero NO por la regla de clase (que suelen ser 
# los pasajeros normales de primera clase).

# -----------------------------------------------------------------------------
# e. Revisión de pasajeros con fare == 0
# -----------------------------------------------------------------------------
pasajeros_gratis <- df %>% filter(fare == 0)
# print(pasajeros_gratis) 

# Explicación y Propuesta:
# ¿Qué son?: Usualmente, los pasajeros con tarifa 0 en el Titanic eran empleados 
# de la naviera ("guarantee group", músicos, etc.) o personas con billetes de 
# cortesía. No son outliers ni errores en el sentido estricto, sino un caso especial 
# (o faltantes estructurales si simplemente no se registró el pago).
# Tratamiento: Depende del objetivo. Si el modelo intenta predecir el poder 
# adquisitivo o la probabilidad de supervivencia basada en lo que pagaron, el 0 
# distorsiona el análisis. Lo ideal es convertirlos a NA (faltantes) si queremos 
# analizar tarifas comerciales, o bien crear una variable categórica separada 
# (ej. "Pasajero de Cortesía / Empleado" = TRUE/FALSE) y dejarlos en 0.

df <- df %>%
  mutate(
    # Ejemplo de tratamiento: Convertir a NA
    fare_corregido = if_else(fare == 0, NA_real_, fare)
  )

# -----------------------------------------------------------------------------
# f. Transformación log1p y repetición del IQR
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    fare_log = log1p(fare), # log(1 + fare) para evitar error con log(0)
    q1_log = quantile(fare_log, 0.25, na.rm = TRUE),
    q3_log = quantile(fare_log, 0.75, na.rm = TRUE),
    iqr_log = q3_log - q1_log,
    outlier_log_15 = fare_log > (q3_log + 1.5 * iqr_log) | fare_log < (q1_log - 1.5 * iqr_log)
  )

cantidad_outliers_log <- sum(df$outlier_log_15, na.rm = TRUE)
cat("Outliers restantes tras log1p(fare):", cantidad_outliers_log, "\n")
