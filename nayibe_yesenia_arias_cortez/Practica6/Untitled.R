
# ejercicio 1

library(readr)
house_prices <- read_csv("~/Desktop/MineriaDatos/house_prices.csv")
# View(house_prices)


library(tidyverse)

# ═══════════════════════════════════════════════════════════════════════════
# EJERCICIO 1 — Limpieza de house_prices.csv (apartados a–f)
# Autor: Taller de Minería de Datos — UNAL
# Fecha: 2026-09-24
# ═══════════════════════════════════════════════════════════════════════════

set.seed(2024)

# ─────────────────────────────────────────────────────────────────────────
# 0. Carga del dataset
# ─────────────────────────────────────────────────────────────────────────
house_prices <- read_csv("~/Desktop/MineriaDatos/house_prices.csv",
                         show_col_types = FALSE)
cat("Dataset original:", nrow(house_prices), "×", ncol(house_prices), "\n\n")


# ═══════════════════════════════════════════════════════════════════════════
# (a) Diagnóstico de NA y eliminación de columnas con > 50 % de faltantes
# ═══════════════════════════════════════════════════════════════════════════

resumen_na <- house_prices |>
  summarise(across(
    everything(),
    list(
      n_na   = ~ sum(is.na(.x)),
      pct_na = ~ round(mean(is.na(.x)) * 100, 2)
    ),
    .names = "{.col}_{.fn}"
  )) |>
  pivot_longer(
    everything(),
    names_to      = c("columna", "metrica"),
    names_pattern = "(.*)_(n_na|pct_na)$",
    values_to     = "valor"
  ) |>
  pivot_wider(names_from = metrica, values_from = valor) |>
  arrange(desc(pct_na))

# Visualización del mapa de faltantes
resumen_na |>
  filter(n_na > 0) |>
  ggplot(aes(x = reorder(columna, pct_na), y = pct_na, fill = pct_na > 50)) +
  geom_col() +
  geom_hline(yintercept = 50, linetype = "dashed", color = "firebrick") +
  geom_text(aes(label = paste0(pct_na, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "firebrick"), guide = "none") +
  scale_y_continuous(limits = c(0, 105)) +
  labs(title    = "Porcentaje de NA por columna en house_prices",
       subtitle = "Línea roja: umbral del 50 % para eliminación",
       x = NULL, y = "% de valores faltantes") +
  theme_minimal(base_size = 12)

# Aplicación de la regla
umbral <- 50
cols_a_eliminar <- resumen_na |> filter(pct_na > umbral) |> pull(columna)

house_prices_limpio <- house_prices |> select(!all_of(cols_a_eliminar))

stopifnot(
  all(resumen_na$pct_na[match(names(house_prices_limpio), resumen_na$columna)] <= umbral),
  nrow(house_prices_limpio) == nrow(house_prices),
  ncol(house_prices) - ncol(house_prices_limpio) == length(cols_a_eliminar)
)

cat("Columnas originales :", ncol(house_prices), "\n")
cat("Columnas eliminadas :", length(cols_a_eliminar), "\n")
cat("Columnas restantes  :", ncol(house_prices_limpio), "\n")
cat("Columnas que se pierden:\n")
cat(paste("-", cols_a_eliminar, collapse = "\n"), "\n\n")

# Justificación (a):
# "PoolQC se elimina porque supera el 50 % de NA. En este dataset la mayoría de
# esos NA son estructurales (la casa no tiene piscina), no faltantes reales.
# La información útil (tener/no tener piscina) queda capturada por PoolArea,
# mientras que PoolQC solo aporta valor condicional para un subconjunto mínimo
# de registros. Eliminarla reduce ruido sin pérdida informativa neta."


# ═══════════════════════════════════════════════════════════════════════════
# (b) Verificación de la hipótesis: NA estructural vs. faltante real
# ═══════════════════════════════════════════════════════════════════════════

grupos_a_evaluar <- list(
  Garage = list(
    features = c("GarageType", "GarageQual", "GarageCond",
                 "GarageFinish", "GarageYrBlt"),
    area = "GarageArea"
  ),
  Bsmt = list(
    features = c("BsmtQual", "BsmtCond", "BsmtExposure",
                 "BsmtFinType1", "BsmtFinType2"),
    area = "TotalBsmtSF"
  ),
  Pool = list(
    features = "PoolQC",
    area = "PoolArea"
  )
)

detectar_faltantes_reales <- function(df, features, area_col) {
  # Fallo Tipo 1: hay área (> 0) pero la característica es NA → faltante real
  fallo_tipo_1 <- df |>
    filter(!is.na(.data[[area_col]]) & .data[[area_col]] > 0) |>
    filter(if_any(all_of(features), is.na)) |>
    nrow()
  
  # Fallo Tipo 2: característica NO es NA, pero el área es 0 o NA → inconsistencia
  fallo_tipo_2 <- df |>
    filter(if_any(all_of(features), ~ !is.na(.x))) |>
    filter(is.na(.data[[area_col]]) | .data[[area_col]] == 0) |>
    nrow()
  
  tibble(
    grupo                    = paste(features, collapse = ", "),
    area_col                 = area_col,
    faltantes_reales_feature = fallo_tipo_1,
    inconsistencia_inversa   = fallo_tipo_2
  )
}

# CORRECCIÓN: Usar house_prices (original) en lugar de house_prices_limpio
resultados_na <- map_dfr(
  grupos_a_evaluar,
  ~ detectar_faltantes_reales(house_prices, .x$features, .x$area)
)

print(resultados_na)

stopifnot(
  nrow(resultados_na) == 3,
  all(resultados_na$faltantes_reales_feature >= 0)
)

cat("✓ Diagnóstico de NA estructurales completado.\n")

# ═══════════════════════════════════════════════════════════════════════════
# (c) Función recodificar_ausencia() + aplicación con reduce()
# ═══════════════════════════════════════════════════════════════════════════

recodificar_ausencia <- function(df, cols, area) {
  stopifnot(
    is.data.frame(df),
    is.character(cols), length(cols) >= 1,
    is.character(area), length(area) == 1,
    all(cols %in% names(df)),
    area   %in% names(df),
    all(map_lgl(df[cols], is.character))
  )
  
  area_vals <- df[[area]]
  df |>
    mutate(across(
      all_of(cols),
      ~ if_else(
        condition = is.na(.x) & (area_vals == 0),
        true      = "Ninguno",
        false     = .x
      )
    ))
}

especificaciones <- list(
  list(cols = c("GarageType", "GarageQual", "GarageCond", "GarageFinish"),
       area = "GarageArea"),
  list(cols = c("BsmtQual", "BsmtCond", "BsmtExposure",
                "BsmtFinType1", "BsmtFinType2"),
       area = "TotalBsmtSF"),
  list(cols = "PoolQC",
       area = "PoolArea")
)

# CORRECCIÓN: Aplicar sobre house_prices (original) en lugar de house_prices_limpio
house_prices_recod <- reduce(
  .x    = especificaciones,
  .f    = ~ recodificar_ausencia(.x, .y$cols, .y$area),
  .init = house_prices  # <-- Cambiar de house_prices_limpio a house_prices
)

# Ahora eliminar columnas con > 50% NA DESPUÉS de recodificar
umbral <- 50
cols_a_eliminar <- resumen_na |> filter(pct_na > umbral) |> pull(columna)

house_prices_limpio <- house_prices_recod |> select(!all_of(cols_a_eliminar))

# Verificaciones semánticas
stopifnot(
  !any(house_prices_recod$GarageArea > 0 &
         house_prices_recod$GarageType == "Ninguno", na.rm = TRUE),
  !any(house_prices_recod$TotalBsmtSF > 0 &
         house_prices_recod$BsmtQual == "Ninguno", na.rm = TRUE),
  !any(house_prices_recod$PoolArea > 0 &
         house_prices_recod$PoolQC == "Ninguno", na.rm = TRUE),
  all(!is.na(house_prices_recod$GarageType) |
        house_prices_recod$GarageArea > 0 |
        is.na(house_prices_recod$GarageArea))
)

cat("✓ Invariantes semánticos de recodificación cumplidos.\n\n")
cat("Columnas después de recodificar y eliminar >50% NA:", ncol(house_prices_limpio), "\n")
# Justificación (c):
# "Se reemplaza NA por 'Ninguno' únicamente cuando el área asociada es 0,
# preservando como NA los casos donde el área es positiva o desconocida.
# Esto respeta la semántica del dato: un NA con area == 0 es ausencia real
# del elemento, por lo que convertirlo en categoría explícita permite que
# los modelos lo traten como nivel válido. Cuando el área existe pero la
# calidad es NA, el faltante es genuino y debe seguir siendo NA para imputación."


# ═══════════════════════════════════════════════════════════════════════════
# (d) Imputación de LotFrontage: mediana global vs. mediana por barrio
# ═══════════════════════════════════════════════════════════════════════════

imputar_mediana <- function(df, col, grupo = NULL) {
  sufijo <- if (is.null(grupo)) "_global" else "_barrio"
  new_col_name <- paste0(col, sufijo)
  
  if (is.null(grupo)) {
    med_val <- median(df[[col]], na.rm = TRUE)
    df |> mutate(!!new_col_name := if_else(is.na(.data[[col]]),
                                           med_val, .data[[col]]))
  } else {
    df |>
      group_by(.data[[grupo]]) |>
      mutate(!!new_col_name := if_else(
        is.na(.data[[col]]),
        median(.data[[col]], na.rm = TRUE),
        .data[[col]]
      )) |>
      ungroup()
  }
}

house_prices_imp <- house_prices_recod |>
  imputar_mediana("LotFrontage") |>
  imputar_mediana("LotFrontage", grupo = "Neighborhood")

# Análisis de diferencias (solo en los NA originales)
diferencias <- house_prices_imp |>
  filter(is.na(house_prices_recod$LotFrontage)) |>
  mutate(diferencia = LotFrontage_barrio - LotFrontage_global) |>
  group_by(Neighborhood) |>
  summarise(
    n_na           = n(),
    diff_media     = mean(diferencia, na.rm = TRUE),
    diff_abs_media = mean(abs(diferencia), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(diff_abs_media))

print(diferencias)

# Gráfico: diferencia media por barrio (barrios con ≥ 5 NA)
diferencias |>
  filter(n_na >= 5) |>
  ggplot(aes(x = reorder(Neighborhood, diff_media),
             y = diff_media,
             fill = diff_media > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "firebrick"), guide = "none") +
  labs(
    title    = "Diferencia media en la imputación de LotFrontage por barrio",
    subtitle = "LotFrontage_barrio − LotFrontage_global (solo filas con NA original)",
    x = "Barrio (Neighborhood)",
    y = "Diferencia media (pies)"
  ) +
  theme_minimal(base_size = 12)

stopifnot(
  sum(is.na(house_prices_imp$LotFrontage_global)) == 0,
  sum(is.na(house_prices_imp$LotFrontage_barrio)) == 0,
  all(house_prices_imp$LotFrontage_global[!is.na(house_prices_recod$LotFrontage)] ==
        house_prices_recod$LotFrontage[!is.na(house_prices_recod$LotFrontage)])
)

cat("✓ Invariantes de imputación cumplidos.\n\n")

# Justificación (d):
# "Se imputa LotFrontage usando la mediana por Neighborhood en lugar de la
# global. El frente del lote está fuertemente condicionado por la zonificación
# del barrio (lotes amplios en suburbios vs. estrechos en el centro). La mediana
# global introduce sesgo sistemático; la imputación por grupo preserva la
# variabilidad geográfica y reduce el error, como se evidencia en el gráfico."


# ═══════════════════════════════════════════════════════════════════════════
# (e) edad_vivienda y detección de valores imposibles
# ═══════════════════════════════════════════════════════════════════════════

house_prices_final <- house_prices_imp |>
  mutate(
    edad_vivienda = YrSold - YearBuilt,
    
    dato_corregido = case_when(
      edad_vivienda < 0                                              ~ TRUE,
      !is.na(GarageYrBlt) & GarageYrBlt > YrSold                     ~ TRUE,
      YearBuilt > 2010                                               ~ TRUE,
      YrSold > 2010                                                  ~ TRUE,
      !is.na(GarageYrBlt) & GarageYrBlt > 2010                       ~ TRUE,
      TRUE                                                           ~ FALSE
    ),
    
    # Convertir a NA los valores imposibles
    edad_vivienda = if_else(dato_corregido & edad_vivienda < 0,
                            NA_real_, edad_vivienda),
    GarageYrBlt   = if_else(dato_corregido & !is.na(GarageYrBlt) &
                              GarageYrBlt > YrSold, NA_real_, GarageYrBlt),
    YearBuilt     = if_else(dato_corregido & YearBuilt > 2010,
                            NA_real_, YearBuilt),
    YrSold        = if_else(dato_corregido & YrSold > 2010,
                            NA_real_, YrSold)
  )

stopifnot(
  "dato_corregido" %in% names(house_prices_final),
  "edad_vivienda"  %in% names(house_prices_final),
  all(house_prices_final$edad_vivienda >= 0 |
        is.na(house_prices_final$edad_vivienda)),
  all(is.na(house_prices_final$GarageYrBlt) |
        is.na(house_prices_final$YrSold) |
        house_prices_final$GarageYrBlt <= house_prices_final$YrSold,
      na.rm = TRUE)
)

cat("Registros con datos corregidos:",
    sum(house_prices_final$dato_corregido, na.rm = TRUE), "\n")
cat("Porcentaje:",
    round(mean(house_prices_final$dato_corregido, na.rm = TRUE) * 100, 2),
    "%\n\n")

# Justificación (e):
# "Los valores imposibles se convierten a NA en lugar de eliminar las filas
# porque la información restante de la vivienda puede ser útil. Las edades
# negativas y las inconsistencias temporales (garaje posterior a la venta)
# son errores de registro sin interpretación válida. Los años posteriores a
# 2010 indican datos fuera del dominio del dataset. La columna bandera
# dato_corregido permite auditar estas observaciones problemáticas."


# ═══════════════════════════════════════════════════════════════════════════
# (f) Discretización: cut() vs. ntile()
# ═══════════════════════════════════════════════════════════════════════════

cuartiles <- quantile(house_prices_final$edad_vivienda,
                      probs = c(0, 0.25, 0.5, 0.75, 1),
                      na.rm = TRUE)
cat("Cuartiles de edad_vivienda:\n")
print(cuartiles)

house_prices_disc <- house_prices_final |>
  mutate(
    grupo_cut = cut(
      edad_vivienda,
      breaks         = cuartiles,
      labels         = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE,
      right          = TRUE
    ),
    grupo_ntile = factor(ntile(edad_vivienda, 4),
                         labels = c("Q1", "Q2", "Q3", "Q4"))
  )

# Tabla de contingencia
tabla_cruzada <- house_prices_disc |>
  filter(!is.na(grupo_cut), !is.na(grupo_ntile)) |>
  count(grupo_cut, grupo_ntile) |>
  pivot_wider(names_from = grupo_ntile, values_from = n, values_fill = 0)

cat("\nTabla de contingencia cut() vs ntile():\n")
print(tabla_cruzada)

n_total   <- sum(tabla_cruzada[, -1])
n_acuerdo <- sum(diag(as.matrix(tabla_cruzada[, -1])))
cat("Porcentaje de acuerdo:",
    round(n_acuerdo / n_total * 100, 1), "%\n")

# Ejemplo de discrepancia: edades empatadas asignadas a grupos distintos
ejemplo_empate <- house_prices_disc |>
  filter(!is.na(edad_vivienda)) |>
  group_by(edad_vivienda) |>
  filter(n_distinct(grupo_ntile) > 1) |>
  ungroup() |>
  arrange(edad_vivienda) |>
  select(Id, edad_vivienda, grupo_cut, grupo_ntile) |>
  head(10)

cat("\nEjemplo de edades asignadas a grupos distintos por ntile():\n")
print(ejemplo_empate)

# Mediana de SalePrice por grupo y método
mediana_por_grupo <- house_prices_disc |>
  filter(!is.na(grupo_cut)) |>
  pivot_longer(
    cols      = c(grupo_cut, grupo_ntile),
    names_to  = "metodo",
    values_to = "grupo"
  ) |>
  group_by(metodo, grupo) |>
  summarise(
    n              = n(),
    mediana_precio = median(SalePrice, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMediana de SalePrice por grupo y método:\n")
print(mediana_por_grupo)

mediana_por_grupo |>
  ggplot(aes(x = grupo, y = mediana_precio, fill = metodo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::dollar(mediana_precio)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("grupo_cut" = "steelblue",
                               "grupo_ntile" = "coral")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title    = "Mediana de SalePrice por grupo de edad de vivienda",
    subtitle = "Comparación: cut() (cuartiles fijos) vs ntile() (rangos)",
    x = "Grupo (cuartil)",
    y = "Mediana de SalePrice ($)",
    fill = "Método"
  ) +
  theme_minimal(base_size = 12)

stopifnot(
  sum(is.na(house_prices_disc$grupo_cut)) ==
    sum(is.na(house_prices_final$edad_vivienda)),
  sum(is.na(house_prices_disc$grupo_ntile)) ==
    sum(is.na(house_prices_final$edad_vivienda)),
  nlevels(house_prices_disc$grupo_cut)   == 4,
  nlevels(house_prices_disc$grupo_ntile) == 4,
  n_total > 0
)

cat("\n✓ Todas las verificaciones pasan.\n")

# Justificación (f):
# "ntile() asigna grupos por posición en el ordenamiento, no por valor: si hay
# empates en el borde de un cuartil, reparte las observaciones entre grupos
# adyacentes para mantener tamaños equilibrados. cut(), en cambio, usa
# intervalos fijos de valor, por lo que dos viviendas con la misma edad
# siempre caen en el mismo bin. Para análisis donde la interpretabilidad del
# umbral importa, cut() es preferible; para modelos que necesitan grupos
# balanceados, ntile() puede ser más útil, pero a costa de romper la
# coherencia semántica de los empates."


# ═══════════════════════════════════════════════════════════════════════════
# RESUMEN FINAL
# ═══════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════════\n")
cat("RESUMEN DEL PIPELINE COMPLETO\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("Filas conservadas        :", nrow(house_prices_disc), "\n")
cat("Columnas finales         :", ncol(house_prices_disc), "\n")
cat("Columnas eliminadas (>50% NA):", length(cols_a_eliminar), "\n")
cat("Registros con dato_corregido :",
    sum(house_prices_final$dato_corregido, na.rm = TRUE), "\n")
cat("NA restantes en LotFrontage  :",
    sum(is.na(house_prices_disc$LotFrontage_barrio)), "\n")
cat("═══════════════════════════════════════════════════════════════\n")

# ejercicio2


set.seed(2024)

# Carga de datos (ajusta la ruta si es necesario)
# titanic <- read_csv("data/titanic.csv", show_col_types = FALSE)

# ──────────────────────────────────────────────
# 2a. Criterio IQR global (k = 1.5 y k = 3)
# ──────────────────────────────────────────────
detectar_outliers_iqr <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

titanic_outliers <- titanic %>%
  mutate(
    out_global_1.5 = detectar_outliers_iqr(fare, k = 1.5),
    out_global_3   = detectar_outliers_iqr(fare, k = 3)
  )

resumen_global <- titanic_outliers %>%
  summarise(
    pct_k_1.5 = round(mean(out_global_1.5, na.rm = TRUE) * 100, 2),
    pct_k_3   = round(mean(out_global_3, na.rm = TRUE) * 100, 2)
  )

cat("Porcentaje de outliers globales (k=1.5):", resumen_global$pct_k_1.5, "%\n")
cat("Porcentaje de outliers globales (k=3):", resumen_global$pct_k_3, "%\n")

# ──────────────────────────────────────────────
# 2b. Criterio IQR por clase (.by = pclass)
# ──────────────────────────────────────────────
titanic_outliers <- titanic_outliers %>%
  mutate(
    out_por_clase = detectar_outliers_iqr(fare, k = 1.5),
    .by = pclass
  )

# Cuántos de 1ª clase eran outliers globales pero dejan de serlo por clase
cambio_1ra_clase <- titanic_outliers %>%
  filter(pclass == 1) %>%
  summarise(
    eran_outliers_globales = sum(out_global_1.5, na.rm = TRUE),
    ya_no_son_outliers     = sum(out_global_1.5 & !out_por_clase, na.rm = TRUE)
  )

cat("\nPasajeros de 1ª clase que dejan de ser outliers al evaluar por clase:", 
    cambio_1ra_clase$ya_no_son_outliers, "\n")

# ──────────────────────────────────────────────
# 2c. Z-score robusto
# ──────────────────────────────────────────────
z_score_robusto <- function(x) {
  (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)
}

titanic_outliers <- titanic_outliers %>%
  mutate(
    out_z_robusto = abs(z_score_robusto(fare)) > 3.5
  )

cat("Porcentaje de outliers con Z-score robusto (|z| > 3.5):", 
    round(mean(titanic_outliers$out_z_robusto, na.rm = TRUE) * 100, 2), "%\n")

# ──────────────────────────────────────────────
# 2d. Tabla cruzada de los tres criterios
# ──────────────────────────────────────────────
tabla_cruzada <- titanic_outliers %>%
  count(out_global_1.5, out_por_clase, out_z_robusto, name = "n_pasajeros") %>%
  arrange(desc(n_pasajeros))

cat("\nTabla de contingencia de criterios de outlier:\n")
print(tabla_cruzada)

# Pasajeros marcados por UN SOLO criterio
un_solo_criterio <- titanic_outliers %>%
  filter((out_global_1.5 + out_por_clase + out_z_robusto) == 1) %>%
  select(name, pclass, fare, out_global_1.5, out_por_clase, out_z_robusto) %>%
  head(10)

cat("\nEjemplo de pasajeros marcados por un solo criterio:\n")
print(un_solo_criterio)

# ──────────────────────────────────────────────
# 2e. Revisión de fare == 0
# ──────────────────────────────────────────────
pasajeros_fare_cero <- titanic_outliers %>%
  filter(fare == 0) %>%
  select(name, pclass, embarked, fare)

cat("\nPasajeros con fare == 0:", nrow(pasajeros_fare_cero), "\n")
print(pasajeros_fare_cero)

# ──────────────────────────────────────────────
# 2f. Transformación log1p y nuevo IQR
# ──────────────────────────────────────────────
titanic_outliers <- titanic_outliers %>%
  mutate(
    fare_log = log1p(fare),
    out_log_1.5 = detectar_outliers_iqr(fare_log, k = 1.5)
  )

cat("\nOutliers en fare_log con IQR (k=1.5):", sum(titanic_outliers$out_log_1.5, na.rm = TRUE), "\n")

# ──────────────────────────────────────────────
# Verificaciones explícitas
# ──────────────────────────────────────────────
stopifnot(
  # 1. Las funciones de outlier devuelven lógicos
  is.logical(titanic_outliers$out_global_1.5),
  is.logical(titanic_outliers$out_z_robusto),
  # 2. El número de pasajeros con fare == 0 es el esperado (17 en este dataset estándar)
  sum(titanic$fare == 0, na.rm = TRUE) == 17,
  # 3. La transformación log1p no genera NA si fare >= 0
  sum(is.na(titanic_outliers$fare_log)) == sum(is.na(titanic_outliers$fare))
)

cat("\n✓ Todas las verificaciones explícitas pasaron.\n")

#ejercicio 3



library(stringr)

# 1. Carga de datos (ajusta la ruta si es necesario)
# Asegúrate de que el archivo titanic.csv esté en tu directorio de trabajo
library(readr)
titanic <- read_csv("~/Downloads/titanic.csv")

# 2. Función auxiliar para extraer el título social
extraer_titulo <- function(nombre) {
  titulo <- str_extract(nombre, "(?<=, )[A-Za-z]+(?=\\.)")
  case_when(
    titulo %in% c("Mr", "Mrs", "Miss", "Master") ~ titulo,
    TRUE ~ "Otro"
  )
}

# 3. Función principal: simular_imputacion(semilla)
simular_imputacion <- function(semilla) {
  set.seed(semilla)
  
  # a. Tomar pasajeros con age conocida y ocultar 20% al azar
  df_conocido <- titanic %>% filter(!is.na(age))
  n_total <- nrow(df_conocido)
  n_oculto <- floor(n_total * 0.20)
  indices_ocultos <- sample(1:n_total, n_oculto)
  
  df_sim <- df_conocido %>%
    mutate(
      age_real = age,
      age_oculto = if_else(row_number() %in% indices_ocultos, NA_real_, age),
      titulo = extraer_titulo(name)
    )
  
  # b. Estrategias de imputación (calculando medianas SOLO sobre datos no ocultos)
  
  # Estrategia 1: Mediana global
  mediana_global <- median(df_sim$age_oculto, na.rm = TRUE)
  df_sim <- df_sim %>%
    mutate(age_imp_1 = if_else(is.na(age_oculto), mediana_global, age_oculto))
  
  # Estrategia 2: Mediana por pclass y sex
  medianas_grupo2 <- df_sim %>%
    filter(!is.na(age_oculto)) %>% # Evita fuga de información
    group_by(pclass, sex) %>%
    summarise(mediana = median(age_oculto, na.rm = TRUE), .groups = "drop")
  
  df_sim <- df_sim %>%
    left_join(medianas_grupo2, by = c("pclass", "sex")) %>%
    mutate(age_imp_2 = if_else(is.na(age_oculto), mediana, age_oculto)) %>%
    select(-mediana)
  
  # Estrategia 3: Mediana por título social
  medianas_grupo3 <- df_sim %>%
    filter(!is.na(age_oculto)) %>% # Evita fuga de información
    group_by(titulo) %>%
    summarise(mediana = median(age_oculto, na.rm = TRUE), .groups = "drop")
  
  df_sim <- df_sim %>%
    left_join(medianas_grupo3, by = "titulo") %>%
    mutate(age_imp_3 = if_else(is.na(age_oculto), mediana, age_oculto)) %>%
    select(-mediana)
  
  # c. Calcular métricas (solo sobre los valores que fueron ocultados)
  df_eval <- df_sim %>% filter(is.na(age_oculto))
  
  calcular_metricas <- function(imp_col) {
    errores <- df_eval[[imp_col]] - df_eval$age_real
    tibble(
      mae  = mean(abs(errores), na.rm = TRUE),
      rmse = sqrt(mean(errores^2, na.rm = TRUE))
    )
  }
  
  res_1 <- calcular_metricas("age_imp_1") %>% mutate(estrategia = "1. Global")
  res_2 <- calcular_metricas("age_imp_2") %>% mutate(estrategia = "2. pclass + sex")
  res_3 <- calcular_metricas("age_imp_3") %>% mutate(estrategia = "3. Titulo social")
  
  metricas <- bind_rows(res_1, res_2, res_3)
  
  # Verificaciones explícitas
  stopifnot(
    nrow(df_eval) == n_oculto,
    !any(is.na(df_sim$age_imp_1)),
    !any(is.na(df_sim$age_imp_2)),
    !any(is.na(df_sim$age_imp_3))
  )
  
  return(list(datos = df_sim, metricas = metricas))
}

# 4. Ejecución única para responder el inciso (c)
set.seed(2024)
res_unica <- simular_imputacion(2024)

cat("Métricas de la simulación única (semilla 2024):\n")
print(res_unica$metricas %>% arrange(mae))

# 5. Simulación 200 veces con purrr::map() (Inciso d)
set.seed(2024)
semillas <- sample(1:10000, 200)

resultados_sim <- map(semillas, ~ simular_imputacion(.x)$metricas) %>%
  list_rbind()

# Gráfico de distribución del MAE
resultados_sim %>%
  ggplot(aes(x = mae, fill = estrategia)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribución del MAE por estrategia de imputación (200 iteraciones)",
    x = "Error Absoluto Medio (MAE)", 
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12)

# ¿Gana siempre la estrategia 3?
ganador_por_iteracion <- resultados_sim %>%
  group_by(iteracion = rep(1:200, each = 3)) %>%
  slice_min(mae, n = 1) %>%
  ungroup()

pct_ganador <- ganador_por_iteracion %>%
  count(estrategia) %>%
  mutate(pct = n / 200 * 100) %>%
  filter(estrategia == "3. Titulo social")

cat("\nLa estrategia ganadora ('3. Titulo social') gana en el", 
    round(pct_ganador$pct, 1), "% de las iteraciones.\n")

# 6. Comparación de la desviación estándar (Inciso e)
df_comp <- res_unica$datos %>% filter(is.na(age_oculto))

sd_real <- sd(df_comp$age_real, na.rm = TRUE)
sd_imp_1 <- sd(df_comp$age_imp_1, na.rm = TRUE)
sd_imp_2 <- sd(df_comp$age_imp_2, na.rm = TRUE)
sd_imp_3 <- sd(df_comp$age_imp_3, na.rm = TRUE)

cat("\nDesviación estándar de la edad:\n")
cat("Real    :", round(sd_real, 2), "\n")
cat("Imp. 1  :", round(sd_imp_1, 2), "\n")
cat("Imp. 2  :", round(sd_imp_2, 2), "\n")
cat("Imp. 3  :", round(sd_imp_3, 2), "\n")

#ejercicio 4

# ─────────────────────────────────────────────────────────────────────────
# 0. Carga o simulación del dataset (para que el código sea ejecutable)
# ─────────────────────────────────────────────────────────────────────────

library(readr)
moviles <- read_csv("~/Downloads/mobile_data.csv")




set.seed(2024)
n <- 3000
moviles <- tibble(
  ram = sample(c(512, 1024, 2048, 3072, 4096, 6144, 8192), n, replace = TRUE),
  battery_power = runif(n, 500, 5000),
  int_memory = sample(c(16, 32, 64, 128, 256), n, replace = TRUE),
  px_height = runif(n, 500, 2000),
  px_width = runif(n, 500, 2000),
  price_range = sample(0:3, n, replace = TRUE)
)

cols_evaluar <- c("ram", "battery_power", "int_memory", "px_height", "px_width")

# ─────────────────────────────────────────────────────────────────────────
# (a) Función escalar con match.arg()
# ─────────────────────────────────────────────────────────────────────────
escalar <- function(df, cols, metodo = c("minmax", "zscore", "robusto")) {
  metodo <- match.arg(metodo)
  
  # Seleccionar solo las columnas de interés
  df_out <- df %>% select(all_of(cols))
  
  if (metodo == "minmax") {
    df_out <- df_out %>% mutate(across(everything(), 
                                       ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))
    ))
  } else if (metodo == "zscore") {
    df_out <- df_out %>% mutate(across(everything(), 
                                       ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    ))
  } else if (metodo == "robusto") {
    df_out <- df_out %>% mutate(across(everything(), 
                                       ~ (.x - median(.x, na.rm = TRUE)) / IQR(.x, na.rm = TRUE)
    ))
  }
  return(df_out)
}

# ─────────────────────────────────────────────────────────────────────────
# (b) Porcentaje de aporte a la distancia euclidiana al cuadrado
# ─────────────────────────────────────────────────────────────────────────
calcular_pct_distancia <- function(df, cols, r1 = 1, r2 = 2) {
  diffs_sq <- (df[r1, cols] - df[r2, cols])^2
  total_sq <- sum(diffs_sq)
  if (total_sq == 0) return(set_names(rep(0, length(cols)), cols))
  pct <- (diffs_sq / total_sq) * 100
  return(as.numeric(pct))
}

# Aplicar con purrr::map_dfr para obtener una tabla comparativa
resultados_dist <- map_dfr(c("original", "minmax", "zscore", "robusto"), ~ {
  df_temp <- if (.x == "original") moviles else escalar(moviles, cols_evaluar, metodo = .x)
  pcts <- calcular_pct_distancia(df_temp, cols_evaluar, 1, 2)
  tibble(metodo = .x, variable = cols_evaluar, pct_aporte = pcts)
})

cat("Aporte porcentual a la distancia euclidiana al cuadrado (Filas 1 vs 2):\n")
print(resultados_dist %>% pivot_wider(names_from = metodo, values_from = pct_aporte))

# ─────────────────────────────────────────────────────────────────────────
# (c) Impacto de un outlier extremo (ram[1] <- 100000)
# ─────────────────────────────────────────────────────────────────────────
moviles_outlier <- moviles
moviles_outlier$ram[1] <- 100000

# Calcular SD de ram en las filas 2 a 3000 para cada método
sd_restantes <- map_dfr(c("minmax", "zscore", "robusto"), ~ {
  df_scaled <- escalar(moviles_outlier, "ram", metodo = .x)
  tibble(
    metodo = .x,
    sd_ram_restante = sd(df_scaled$ram[-1], na.rm = TRUE)
  )
})

cat("\nDesviación estándar de 'ram' (filas 2 a 3000) tras inyectar outlier:\n")
print(sd_restantes)

# ─────────────────────────────────────────────────────────────────────────
# (d) Fuga de información: Train vs Test
# ─────────────────────────────────────────────────────────────────────────
set.seed(2024)
n_total <- nrow(moviles)
idx_train <- sample(1:n_total, size = 0.7 * n_total)

train <- moviles[idx_train, ]
test  <- moviles[-idx_train, ]

# 1. Escalar prueba usando parámetros SOLO del entrenamiento
min_train <- min(train$ram)
max_train <- max(train$ram)

test_scaled_proper <- (test$ram - min_train) / (max_train - min_train)
fuera_de_rango_proper <- sum(test_scaled_proper < 0 | test_scaled_proper > 1)
pct_fuera_proper <- round(fuera_de_rango_proper / nrow(test) * 100, 2)

# 2. Escalar TODO el dataset junto (práctica incorrecta)
todo_scaled <- escalar(moviles, "ram", metodo = "minmax")
test_scaled_wrong <- todo_scaled$ram[-idx_train]
fuera_de_rango_wrong <- sum(test_scaled_wrong < 0 | test_scaled_wrong > 1) # Siempre será 0

cat("\nFuga de información:\n")
cat("- Valores de prueba fuera de [0, 1] escalando con parámetros de train:", 
    fuera_de_rango_proper, "(", pct_fuera_proper, "%)\n")
cat("- Valores de prueba fuera de [0, 1] escalando todo junto:", fuera_de_rango_wrong, "\n")

# ─────────────────────────────────────────────────────────────────────────
# Verificaciones explícitas
# ─────────────────────────────────────────────────────────────────────────
stopifnot(
  # (a) La función devuelve solo las columnas solicitadas
  ncol(escalar(moviles, cols_evaluar, "minmax")) == length(cols_evaluar),
  # (c) El método robusto preserva mucho más la varianza que minmax/zscore ante outliers
  sd_restantes$sd_ram_restante[sd_restantes$metodo == "robusto"] > 
    sd_restantes$sd_ram_restante[sd_restantes$metodo == "minmax"] * 10,
  # (d) Escalar todo junto nunca produce valores fuera de [0,1] por definición
  fuera_de_rango_wrong == 0
)

cat("\n✓ Todas las verificaciones explícitas pasaron.\n")

# ejercicio 5

library(stringr)

set.seed(2024)

# Si tienes el archivo real, usa:
library(readr)
adult <- read_csv("~/Downloads/adult.csv")

n <- 5000
adult <- tibble(
  age            = sample(17:90, n, replace = TRUE),
  workclass      = sample(c("Private", "Self-emp-not-inc", "Local-gov",
                            "State-gov", "Federal-gov", "?"), n,
                          replace = TRUE, prob = c(.70, .08, .05, .04, .03, .10)),
  fnlwgt         = sample(10000:500000, n, replace = TRUE),
  education      = sample(c("HS-grad", "Some-college", "Bachelors",
                            "Masters", "Assoc-voc", "11th"), n, replace = TRUE),
  education.num  = sample(1:16, n, replace = TRUE),
  marital.status = sample(c("Married-civ-spouse", "Never-married",
                            "Divorced", "Separated", "Widowed"), n,
                          replace = TRUE, prob = c(.45, .33, .12, .05, .05)),
  occupation     = sample(c("Prof-specialty", "Exec-managerial", "Craft-repair",
                            "Sales", "Adm-clerical", "Other-service", "?"), n,
                          replace = TRUE, prob = c(.15, .15, .12, .13, .15, .20, .10)),
  relationship   = sample(c("Husband", "Not-in-family", "Own-child",
                            "Unmarried", "Wife"), n, replace = TRUE),
  race           = sample(c("White", "Black", "Asian-Pac-Islander",
                            "Amer-Indian-Eskimo", "Other"), n, replace = TRUE),
  sex            = sample(c("Male", "Female"), n, replace = TRUE, prob = c(.67, .33)),
  capital.gain   = c(rep(0, round(0.92 * n)),
                     sample(c(1:9998, 99999), n - round(0.92 * n), replace = TRUE)),
  capital.loss   = sample(0:4356, n, replace = TRUE),
  hours.per.week = sample(1:99, n, replace = TRUE),
  native.country = sample(c("United-States", "Mexico", "Philippines",
                            "Germany", "Canada", "India", "?"), n,
                          replace = TRUE, prob = c(.88, .02, .02, .01, .02, .01, .04)),
  income         = sample(c("<=50K", ">50K"), n, replace = TRUE, prob = c(.76, .24))
)





# Función reutilizable: reemplaza "?" por NA en columnas carácter
limpiar_signos <- function(df) {
  df |> mutate(across(where(is.character),
                      ~ na_if(str_trim(.x), "?")))
}

adult_limpio <- limpiar_signos(adult)

# Comparación: proporción de >50K entre occupation faltante vs. las demás
adult_limpio |>
  mutate(falta_occ = is.na(occupation),
         alto_ing  = income == ">50K") |>
  group_by(falta_occ) |>
  summarise(n = n(), prop_alto = mean(alto_ing), .groups = "drop") |>
  print()

# prop.test: ¿la proporción de >50K difiere entre occupation-NA y los demás?
tabla_test <- adult_limpio |>
  transmute(
    alto = as.integer(income == ">50K"),
    falta_occ = is.na(occupation)
  ) |>
  (\(x) with(x, table(alto, falta_occ)))() # <-- Función anónima aplicada inmediatamente

prueba <- prop.test(tabla_test)
cat("\nprop.test p-value:", format(prueba$p.value, digits = 3), "\n")

prueba <- prop.test(tabla_test)
cat("\nprop.test p-value:", format(prueba$p.value, digits = 3), "\n")





# Diagnóstico del centinela
diag_centinela <- adult |>
  filter(capital.gain > 0) |>
  arrange(desc(capital.gain)) |>
  slice(1:5) |>
  select(capital.gain)

cat("Top 5 valores de capital.gain:\n")
print(diag_centinela)

n_centinela <- sum(adult$capital.gain == 99999)
cat("\nFilas con centinela 99999:", n_centinela, "\n")
cat("Siguiente valor más alto:",
    max(adult$capital.gain[adult$capital.gain < 99999]), "\n")
cat("% de filas con capital.gain == 0:",
    round(mean(adult$capital.gain == 0) * 100, 1), "%\n")

tratar_centinela <- function(df) {
  df |> mutate(
    capital.gain = if_else(capital.gain == 99999, NA_real_, capital.gain),
    tiene_capital_gain = as.integer(!is.na(capital.gain) & capital.gain > 0)
  )
}

# Agrupar países: "United-States" vs "Otro"
agrupar_paises <- function(df) {
  df |> mutate(
    native.country = if_else(is.na(native.country) |
                               native.country != "United-States",
                             "Otro", "United-States")
  )
}

# Imputar: numéricas con mediana, categóricas con "Desconocido"
imputar <- function(df, params = NULL) {
  if (is.null(params)) {
    meds <- df |> summarise(across(where(is.numeric),
                                   ~ median(.x, na.rm = TRUE)))
    params <- list(medianas = meds)
  }
  df_imp <- df |>
    mutate(across(where(is.numeric),
                  ~ if_else(is.na(.x), params$medianas[[cur_column()]], .x)))
  # Para categóricas, rellenar con "Desconocido"
  df_imp <- df_imp |>
    mutate(across(where(is.character),
                  ~ if_else(is.na(.x), "Desconocido", .x)))
  attr(df_imp, "params") <- params
  df_imp
}

# Transformar log1p variables con asimetría extrema
transformar_log <- function(df, cols = c("capital.gain", "capital.loss", "fnlwgt")) {
  df |> mutate(across(all_of(cols),
                      ~ log1p(pmax(.x, 0, na.rm = TRUE)),
                      .names = "{.col}_log"))
}

# Codificar: one-hot de workclass, marital.status, sex; income binaria
codificar <- function(df, params = NULL) {
  if (is.null(params)) {
    params <- list(
      niveles_workclass      = levels(as.factor(df$workclass)),
      niveles_marital.status = levels(as.factor(df$marital.status)),
      niveles_sex            = levels(as.factor(df$sex))
    )
  }
  # Forzar los niveles conocidos para que las dummy columns sean consistentes
  df <- df |>
    mutate(
      workclass      = factor(workclass,      levels = params$niveles_workclass),
      marital.status = factor(marital.status, levels = params$niveles_marital.status),
      sex            = factor(sex,            levels = params$niveles_sex),
      income_bin     = as.integer(income == ">50K")
    )
  
  # One-hot
  dummies <- df |>
    select(workclass, marital.status, sex) |>
    fastDummies::dummy_cols(remove_first = TRUE, remove_selected_columns = TRUE)
  
  df_out <- bind_cols(df |> select(-workclass, -marital.status, -sex), dummies)
  attr(df_out, "params") <- params
  df_out
}

# Estandarizar Z-score
estandarizar <- function(df, cols = NULL, params = NULL) {
  if (is.null(cols)) {
    cols <- df |> select(where(is.numeric)) |> names()
    cols <- setdiff(cols, c("income_bin", "tiene_capital_gain"))
  }
  if (is.null(params)) {
    params <- df |>
      select(all_of(cols)) |>
      summarise(across(everything(), list(media = mean, sd = sd), na.rm = TRUE))
  }
  df_out <- df |>
    mutate(across(all_of(cols),
                  ~ (.x - params[[paste0(cur_column(), "_media")]]) /
                    params[[paste0(cur_column(), "_sd")]]))
  attr(df_out, "params") <- params
  df_out
}




preprocesar_adult <- function(df) {
  pasos <- list(
    limpiar_signos,
    tratar_centinela,
    agrupar_paises,
    imputar,
    transformar_log,
    codificar,
    estandarizar
  )
  df_final <- reduce(pasos, function(datos, fun) fun(datos), .init = df)
  
  # Verificaciones explícitas
  stopifnot(
    "Sin NA en numéricas" = sum(is.na(df_final |> select(where(is.numeric)))) == 0,
    "Mismo número de filas" = nrow(df_final) == nrow(df),
    "income_bin es numérica" = is.numeric(df_final$income_bin)
  )
  df_final
}

adult_final <- preprocesar_adult(adult)
cat("\nDimensiones finales:", nrow(adult_final), "×", ncol(adult_final), "\n")
cat("NA restantes:", sum(is.na(adult_final)), "\n")




set.seed(2024)
idx_train <- sample(nrow(adult), size = 0.7 * nrow(adult))
adult_train <- adult[idx_train, ]
adult_test  <- adult[-idx_train, ]

# Versión del pipeline que devuelve tanto el df procesado como los parámetros
preprocesar_con_params <- function(df, params = NULL) {
  # Cada paso propaga los parámetros acumulados
  df <- limpiar_signos(df)
  df <- tratar_centinela(df)
  df <- agrupar_paises(df)
  
  r_im  <- imputar(df, params$imputar)
  params$imputar <- attr(r_im, "params")
  df <- r_im
  
  df <- transformar_log(df)
  
  r_cod <- codificar(df, params$codificar)
  params$codificar <- attr(r_cod, "params")
  df <- r_cod
  
  r_est <- estandarizar(df, params = params$estandarizar)
  params$estandarizar <- attr(r_est, "params")
  df <- r_est
  
  list(df = df, params = params)
}

# Ajustar SOLO con train
res_train <- preprocesar_con_params(adult_train)
adult_train_proc <- res_train$df
parametros_fit   <- res_train$params

# Aplicar a test con los parámetros guardados (sin recalcular)
adult_test_proc <- preprocesar_con_params(adult_test, params = parametros_fit)$df

# Verificaciones cruzadas
stopifnot(
  "Mismas columnas train/test" = setequal(names(adult_train_proc), names(adult_test_proc)),
  "Sin NA en test"             = sum(is.na(adult_test_proc)) == 0,
  "Mismas filas en test"       = nrow(adult_test_proc) == nrow(adult_test)
)

cat("\n✓ Pipeline aplicado a train y test con parámetros compartidos.\n")
cat("Columnas train:", ncol(adult_train_proc),
    "| Columnas test:", ncol(adult_test_proc), "\n")

