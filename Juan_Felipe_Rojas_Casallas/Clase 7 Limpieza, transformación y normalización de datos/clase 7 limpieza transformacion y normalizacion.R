# Clase 7: Limpieza, transformación y normalización de datos
# Juan Felipe Rojas Casallas
#
# Los CSV deben estar en la misma carpeta que este script:
# titanic.csv, house_prices.csv, mobile_data.csv, adult.csv
#
# El ejercicio 3 genera ej3_imputacion_r.csv, que usa el archivo
# "clase 7 equivalentes python.py" para verificar que R y Python dan lo mismo.
# Todo muestreo va precedido de set.seed(2024), como pide el enunciado.

paquetes  <- c("dplyr", "readr", "tidyr", "ggplot2", "purrr", "stringr", "tibble")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tibble)

titanic <- read_csv("titanic.csv", show_col_types = FALSE)

# Funciones de apoyo reutilizadas en varios ejercicios (sección 4.1 de la clase)
detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

zscore_robusto <- function(x) (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)


############################## EJERCICIO 1 ##################################
# Faltantes que no son faltantes (house_prices.csv)

precios <- read_csv("house_prices.csv", show_col_types = FALSE)

# a. Porcentaje de NA por columna y regla del 50 %
na_por_columna <- precios |>
  map_df(~ mean(is.na(.x))) |>
  pivot_longer(everything(), names_to = "columna", values_to = "pct_na") |>
  mutate(pct_na = round(pct_na * 100, 1)) |>
  filter(pct_na > 0) |>
  arrange(desc(pct_na))
print(na_por_columna, n = 20)

cols_eliminadas <- na_por_columna |> filter(pct_na > 50) |> pull(columna)
cat("Columnas que se perderían con la regla del 50 %:", cols_eliminadas, "\n")
# Resultado: PoolQC (99.7 %), MiscFeature (96.4 %), Alley (93.2 %) y Fence (80.4 %).
# Justificación: aplicar la regla a ciegas aquí es un error. En PoolQC el NA no es
# "no sabemos la calidad de la piscina" sino "la casa no tiene piscina": son 2906
# viviendas sin piscina y solo 13 con piscina, de las cuales 10 sí tienen calidad
# registrada. Al eliminar la columna se pierde la distinción entre una piscina
# excelente (Ex) y una en mal estado (Fa), que es justo lo que puede explicar
# diferencias de precio entre las pocas casas que sí la tienen.

# b. NA estructurales vs. faltantes reales
cols_garage <- precios |> select(starts_with("Garage")) |> select(where(is.character)) |> names()
cols_bsmt   <- precios |> select(starts_with("Bsmt"))   |> select(where(is.character)) |> names()

grupos_ausencia <- list(
  list(nombre = "Garage", cols = cols_garage,  area = "GarageArea"),
  list(nombre = "Bsmt",   cols = cols_bsmt,    area = "TotalBsmtSF"),
  list(nombre = "Pool",   cols = "PoolQC",     area = "PoolArea")
)

# Un NA es estructural si el área correspondiente vale 0; si el área es > 0 (o es
# NA y por tanto no confirma la ausencia) se trata de un faltante real.
contar_faltantes <- function(df, cols, area, nombre) {
  sin_elemento <- coalesce(df[[area]] == 0, FALSE)
  df |>
    summarise(
      grupo         = nombre,
      na_total      = sum(if_any(all_of(cols), is.na)),
      estructurales = sum(if_any(all_of(cols), is.na) &  sin_elemento),
      reales        = sum(if_any(all_of(cols), is.na) & !sin_elemento),
      area_na       = sum(if_any(all_of(cols), is.na) & is.na(.data[[area]]))
    )
}

resumen_ausencias <- grupos_ausencia |>
  map(~ contar_faltantes(precios, .x$cols, .x$area, .x$nombre)) |>
  list_rbind()
resumen_ausencias
# Resultado: Garage 2 faltantes reales, Bsmt 10 y Pool 3.
# En Garage uno tiene GarageArea > 0 con calidad sin registrar y el otro tiene la
# propia área en NA; en Bsmt hay 9 sótanos con área > 0 a los que les falta algún
# atributo y 1 con TotalBsmtSF en NA; en Pool son 3 casas con piscina (PoolArea > 0)
# sin calidad registrada.

# c. Recodificar solo los NA estructurales
recodificar_ausencia <- function(df, cols, area) {
  sin_elemento <- coalesce(df[[area]] == 0, FALSE)
  df |>
    mutate(across(all_of(cols) & where(is.character),
                  ~ if_else(is.na(.x) & sin_elemento, "Ninguno", .x)))
}

precios_rec <- grupos_ausencia |>
  reduce(\(df, spec) recodificar_ausencia(df, spec$cols, spec$area), .init = precios)

# Verificación: los NA que quedan son exactamente los faltantes reales
verificacion <- grupos_ausencia |>
  map(~ contar_faltantes(precios_rec, .x$cols, .x$area, .x$nombre)) |>
  list_rbind()
verificacion
stopifnot(verificacion$na_total == resumen_ausencias$reales, verificacion$estructurales == 0)
# Nota: GarageYrBlt es numérica, así que la función no la toca (solo opera sobre
# columnas de texto). Sus 159 NA siguen significando "sin garaje" y no deben
# imputarse con una mediana de años.

# d. LotFrontage: mediana por barrio vs. mediana global
mediana_global_lf <- median(precios_rec$LotFrontage, na.rm = TRUE)

precios_rec <- precios_rec |>
  mutate(
    lf_global = if_else(is.na(LotFrontage), mediana_global_lf, LotFrontage),
    mediana_barrio = median(LotFrontage, na.rm = TRUE),
    mediana_barrio = coalesce(mediana_barrio, mediana_global_lf),
    lf_barrio = if_else(is.na(LotFrontage), mediana_barrio, LotFrontage),
    .by = Neighborhood
  )

dif_barrios <- precios_rec |>
  summarise(
    n_imputados = sum(is.na(LotFrontage)),
    mediana_barrio = first(mediana_barrio),
    diferencia = first(mediana_barrio) - mediana_global_lf,
    .by = Neighborhood
  ) |>
  arrange(desc(abs(diferencia)))
print(dif_barrios, n = 10)

dif_barrios |>
  ggplot(aes(x = reorder(Neighborhood, diferencia), y = diferencia,
             fill = diferencia > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "firebrick"), guide = "none") +
  labs(title = "LotFrontage: mediana del barrio menos mediana global",
       subtitle = paste("Mediana global =", mediana_global_lf, "pies"),
       x = NULL, y = "Diferencia (pies)") +
  theme_minimal(base_size = 12)
# Los barrios donde más difieren son los de lotes grandes (Timber, NoRidge) y los
# de casas en hilera (Blueste, MeadowV, BrDale), donde la mediana global
# sobreestima el frente del lote. Imputar por barrio respeta esa estructura.

# e. edad_vivienda y valores imposibles
precios_rec <- precios_rec |>
  mutate(
    edad_bruta = YrSold - YearBuilt,
    problema = case_when(
      GarageYrBlt > 2010              ~ "GarageYrBlt imposible",
      edad_bruta < 0                  ~ "venta anterior a la construccion",
      GarageYrBlt > YrSold            ~ "garaje posterior a la venta",
      YearRemodAdd > YrSold           ~ "remodelacion posterior a la venta",
      YearRemodAdd < YearBuilt        ~ "remodelacion anterior a la construccion",
      .default = NA_character_
    )
  )

precios_rec |>
  filter(!is.na(problema)) |>
  select(Id, problema, YearBuilt, YearRemodAdd, GarageYrBlt, YrSold, SaleType, SaleCondition)

# Decisiones:
# - Id 2593 tiene GarageYrBlt = 2207 en una casa de 2006 remodelada en 2007: es un
#   error de digitación (2207 en vez de 2007), así que se corrige con YearRemodAdd.
# - Id 2550 se construyó en 2008 y se vendió en 2007 (edad = -1). Es una preventa
#   (SaleType = New, SaleCondition = Partial), pero como "edad" el valor es
#   imposible y arrastra también un garaje posterior a la venta, así que se elimina
#   la fila: es 1 de 2919 y no vale la pena inventarle una edad.
# - Las remodelaciones posteriores a la venta (Id 524, 2296) y la anterior a la
#   construcción (Id 1877) se conservan: no afectan edad_vivienda y lo más probable
#   es que sean diferencias entre la fecha del contrato y la de terminación de obra.
precios_limpio <- precios_rec |>
  mutate(
    dato_corregido = coalesce(problema == "GarageYrBlt imposible", FALSE),
    GarageYrBlt = if_else(dato_corregido, YearRemodAdd, GarageYrBlt)
  ) |>
  filter(coalesce(problema, "") != "venta anterior a la construccion") |>
  mutate(edad_vivienda = YrSold - YearBuilt)

stopifnot(
  all(precios_limpio$edad_vivienda >= 0),
  all(precios_limpio$GarageYrBlt <= 2010, na.rm = TRUE),
  nrow(precios_limpio) == nrow(precios) - 1,
  sum(precios_limpio$dato_corregido) == 1
)

# f. Discretización con cut() y con ntile()
cortes <- quantile(precios_limpio$edad_vivienda, probs = seq(0, 1, 0.25), na.rm = TRUE)

precios_limpio <- precios_limpio |>
  mutate(
    edad_cut   = cut(edad_vivienda, breaks = cortes, labels = paste0("Q", 1:4),
                     include.lowest = TRUE),
    edad_ntile = factor(ntile(edad_vivienda, 4), labels = paste0("Q", 1:4))
  )

table(cut = precios_limpio$edad_cut, ntile = precios_limpio$edad_ntile)

precios_limpio |>
  filter(edad_cut != edad_ntile) |>
  count(edad_vivienda, edad_cut, edad_ntile, sort = TRUE) |>
  head(10)
# Los grupos no coinciden. cut() parte el rango en los valores de los cuartiles, así
# que todas las viviendas con la misma edad caen en el mismo grupo, pero los grupos
# quedan de tamaños distintos. ntile() hace lo contrario: reparte las filas en 4
# grupos del mismo tamaño y, cuando hay empates en el borde (por ejemplo muchas
# casas con la misma edad), tiene que partir ese empate y manda unas a un grupo y
# otras al siguiente aunque tengan exactamente la misma edad.

precios_limpio |>
  summarise(
    n = n(),
    edad_min = min(edad_vivienda),
    edad_max = max(edad_vivienda),
    mediana_precio = median(SalePrice, na.rm = TRUE),
    .by = edad_cut
  ) |>
  arrange(edad_cut)


############################## EJERCICIO 2 ##################################
# ¿Qué es un outlier en fare? (titanic.csv)

# a. IQR global con k = 1.5 y k = 3
# El único pasajero sin fare queda con bandera NA y no entra en los porcentajes.
outliers_fare <- titanic |>
  mutate(
    iqr_global_15 = detectar_outliers_iqr(fare, k = 1.5),
    iqr_global_3  = detectar_outliers_iqr(fare, k = 3)
  )

outliers_fare |>
  summarise(
    pct_k_1.5 = round(mean(iqr_global_15, na.rm = TRUE) * 100, 1),
    pct_k_3   = round(mean(iqr_global_3,  na.rm = TRUE) * 100, 1)
  )
# Resultado: k = 1.5 marca 13.1 % de los pasajeros y k = 3 marca 6.4 %.

# b. IQR dentro de cada pclass
outliers_fare <- outliers_fare |>
  mutate(iqr_clase = detectar_outliers_iqr(fare, k = 1.5), .by = pclass)

outliers_fare |>
  filter(!is.na(fare)) |>
  count(pclass, iqr_global_15, iqr_clase)

n_primera_rescatados <- outliers_fare |>
  filter(pclass == 1, iqr_global_15, !iqr_clase) |>
  nrow()
cat("Pasajeros de 1a clase que dejan de ser outliers:", n_primera_rescatados, "\n")
# Resultado: 128 pasajeros de primera clase dejan de ser outliers.
# Justificación: la tarifa depende de la clase, así que la distribución global
# mezcla tres poblaciones distintas. Casi toda la muestra es de 3a clase (tarifa
# mediana ≈ 8), así que un tiquete normal de 1a (mediana ≈ 61) parece extremo sin
# serlo. Al comparar dentro de la clase, además, aparecen outliers en 2a y 3a
# (5 y 54) que el criterio global no veía porque quedaban tapados por la 1a.

# c. Z-score robusto: (x - mediana) / MAD, outlier si |z| > 3.5
outliers_fare <- outliers_fare |>
  mutate(
    z_robusto        = zscore_robusto(fare),
    outlier_z_robust = abs(z_robusto) > 3.5
  )

cat("Outliers por z-score robusto:", sum(outliers_fare$outlier_z_robust, na.rm = TRUE), "\n")
# Justificación: el z-score clásico usa media y desviación estándar, que se
# inflan con los propios valores extremos (las tarifas de 512 suben la sd a 51.8)
# y así los extremos se "esconden". La mediana y la MAD casi no se mueven con
# ellos, así que la escala de referencia es la del pasajero típico.

# d. Cruce de los tres criterios
outliers_fare |>
  filter(!is.na(fare)) |>
  count(iqr_global_15, iqr_clase, outlier_z_robust)

# ¿A quién marca un solo criterio?
outliers_fare |>
  filter(!is.na(fare)) |>
  mutate(
    criterio_unico = case_when(
      iqr_global_15  & !iqr_clase & !outlier_z_robust ~ "solo global",
      !iqr_global_15 &  iqr_clase & !outlier_z_robust ~ "solo por clase",
      !iqr_global_15 & !iqr_clase &  outlier_z_robust ~ "solo z robusto",
      .default = NA_character_
    )
  ) |>
  filter(!is.na(criterio_unico)) |>
  summarise(
    n         = n(),
    fare_min  = min(fare),
    fare_max  = max(fare),
    clases    = paste(sort(unique(pclass)), collapse = ", "),
    .by = criterio_unico
  )
# Lectura: "solo por clase" son tarifas altas de 2a y 3a (caras para su clase,
# normales para el barco). "Solo z robusto" son tarifas intermedias (≈ 50-65):
# como la MAD es pequeña (10.2), el z robusto es más estricto que el IQR global.
# Ningún pasajero queda marcado únicamente por el IQR global.

# e. Los 17 pasajeros con fare == 0
outliers_fare |>
  filter(fare == 0) |>
  select(name, pclass, ticket, embarked, home.dest)
# Todos son hombres, embarcaron en Southampton y viajaban con tiquetes de grupo:
# "LINE" (empleados de la American Line) y 112050-112059 / 239853-239856
# (personal de Harland & Wolff y White Star, p. ej. Thomas Andrews y J. Bruce Ismay).
#
# Decisión: convertir a NA e imputar con la mediana de su pclass.
# Justificación: no son errores de digitación sino pasajes de cortesía, pero si
# fare se usa como proxy del nivel socioeconómico, un 0 en primera clase (Ismay)
# es engañoso. Por eso se tratan como faltantes disfrazados y se imputa por clase,
# y se deja la bandera tarifa_cortesia para no perder el dato de que no pagaron.
# La mediana se calcula sin los ceros; el pasajero que ya tenía fare NA también
# se imputa así.
titanic_fare <- titanic |>
  mutate(
    tarifa_cortesia = fare == 0 & !is.na(fare),
    fare_limpia     = if_else(fare == 0, NA_real_, fare),
    fare_limpia     = if_else(is.na(fare_limpia),
                              median(fare_limpia, na.rm = TRUE),
                              fare_limpia),
    .by = pclass
  )

stopifnot(
  sum(is.na(titanic_fare$fare_limpia)) == 0,
  sum(titanic_fare$fare_limpia == 0) == 0,
  sum(titanic_fare$tarifa_cortesia) == 17
)

titanic_fare |>
  filter(tarifa_cortesia) |>
  count(pclass, fare_limpia)

# f. IQR sobre log1p(fare)
outliers_log <- titanic_fare |>
  mutate(
    iqr_log_original = detectar_outliers_iqr(log1p(fare)),
    iqr_log_limpia   = detectar_outliers_iqr(log1p(fare_limpia))
  )

outliers_log |>
  summarise(
    outliers_fare_original = sum(detectar_outliers_iqr(fare), na.rm = TRUE),
    outliers_log_original  = sum(iqr_log_original, na.rm = TRUE),
    de_ellos_fare_cero     = sum(iqr_log_original & tarifa_cortesia, na.rm = TRUE),
    outliers_log_limpia    = sum(iqr_log_limpia)
  )
# Resultado: con fare original los outliers IQR bajan de 171 a 42, pero 17 de esos
# 42 son justamente los fare = 0: log1p(0) = 0 queda muy por debajo del resto (la
# tarifa positiva más baja, 3.17, pasa a 1.4), así que la transformación crea
# outliers nuevos en el extremo bajo. Sobre fare_limpia (ya sin ceros) quedan menos.
#
# Discusión: para modelar sí lo resuelve. El log comprime la cola derecha, reduce
# la asimetría y evita que una tarifa de 512 pese como 60 tarifas típicas en
# modelos sensibles a la escala. Pero no reemplaza la revisión de los datos: no
# dice si un valor es un error, y los ceros hay que tratarlos antes (como en el
# punto e), porque si no la transformación los convierte en el problema contrario.


############################## EJERCICIO 3 ##################################
# ¿Qué estrategia de imputación se equivoca menos? (titanic.csv)

# Título social con la regla de la sección 5.3 de la clase.
# "the Countess" no coincide con la regex (tiene espacio) y cae en "Otro", igual
# que el resto de títulos poco frecuentes.
titanic_edad <- titanic |>
  filter(!is.na(age)) |>
  mutate(
    titulo = str_extract(name, "(?<=, )[A-Za-z]+(?=\\.)"),
    titulo_grupo = case_when(
      titulo %in% c("Mr")                ~ "Mr",
      titulo %in% c("Mrs", "Mme")        ~ "Mrs",
      titulo %in% c("Miss", "Ms", "Mlle") ~ "Miss",
      titulo == "Master"                 ~ "Master",
      .default = "Otro"
    )
  )

stopifnot(nrow(titanic_edad) == 1046)

# Imputa las edades ocultas (age_obs = NA) con la mediana del grupo calculada
# solo con las edades visibles. Si un grupo quedara sin edades visibles se usa
# la mediana global, para no dejar NA.
imputar_mediana <- function(df, ...) {
  mediana_global <- median(df$age_obs, na.rm = TRUE)
  df |>
    mutate(
      mediana_grupo = median(age_obs, na.rm = TRUE),
      mediana_grupo = coalesce(mediana_grupo, mediana_global),
      age_imp       = if_else(is.na(age_obs), mediana_grupo, age_obs),
      .by = c(...)
    ) |>
    pull(age_imp)
}

# a. y b. Ocultar el 20 % e imputar con las tres estrategias
ocultar_e_imputar <- function(df, semilla, prop = 0.20) {
  set.seed(semilla)
  ocultos <- sample(nrow(df), size = round(prop * nrow(df)))

  df |>
    mutate(
      age_real = age,
      oculto   = row_number() %in% ocultos,
      age_obs  = if_else(oculto, NA_real_, age)
    ) |>
    mutate(
      imp_global      = imputar_mediana(pick(everything())),
      imp_clase_sexo  = imputar_mediana(pick(everything()), pclass, sex),
      imp_titulo      = imputar_mediana(pick(everything()), titulo_grupo)
    )
}

# c. MAE y RMSE sobre las edades ocultas
calcular_errores <- function(df_imp) {
  df_imp |>
    filter(oculto) |>
    pivot_longer(starts_with("imp_"), names_to = "estrategia", values_to = "age_imp") |>
    summarise(
      mae  = mean(abs(age_imp - age_real)),
      rmse = sqrt(mean((age_imp - age_real)^2)),
      .by = estrategia
    )
}

titanic_sim <- ocultar_e_imputar(titanic_edad, semilla = 2024)

# Verificaciones: 20 % oculto y ningún NA después de imputar
stopifnot(
  sum(titanic_sim$oculto) == round(0.20 * 1046),
  sum(is.na(titanic_sim$age_obs)) == sum(titanic_sim$oculto),
  all(!is.na(select(titanic_sim, starts_with("imp_"))))
)

errores_2024 <- calcular_errores(titanic_sim) |> arrange(mae)
errores_2024
ganadora_2024 <- errores_2024$estrategia[1]
cat("Estrategia ganadora con semilla 2024:", ganadora_2024, "\n")

# d. Repetir 200 veces
simular_imputacion <- function(semilla) {
  ocultar_e_imputar(titanic_edad, semilla) |>
    calcular_errores() |>
    mutate(semilla = semilla)
}

set.seed(2024)
semillas <- sample.int(1e6, 200)

resultados_sim <- semillas |>
  map(simular_imputacion) |>
  list_rbind()

resultados_sim |>
  ggplot(aes(x = estrategia, y = mae, fill = estrategia)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title = "MAE de la imputación de edad en 200 simulaciones",
       x = NULL, y = "MAE (años)") +
  theme_minimal(base_size = 13)

pct_victorias <- resultados_sim |>
  slice_min(mae, n = 1, with_ties = FALSE, by = semilla) |>
  count(estrategia, name = "victorias") |>
  mutate(pct = round(victorias / length(semillas) * 100, 1))
pct_victorias

resultados_sim |>
  summarise(mae_medio = mean(mae), rmse_medio = mean(rmse), .by = estrategia) |>
  arrange(mae_medio)

# e. Variabilidad después de imputar
titanic_sim |>
  summarise(
    sd_real         = sd(age_real),
    sd_global       = sd(imp_global),
    sd_clase_sexo   = sd(imp_clase_sexo),
    sd_titulo       = sd(imp_titulo),
    # solo sobre las 209 edades ocultas
    sd_real_ocultas       = sd(age_real[oculto]),
    sd_global_ocultas     = sd(imp_global[oculto]),
    sd_clase_sexo_ocultas = sd(imp_clase_sexo[oculto]),
    sd_titulo_ocultas     = sd(imp_titulo[oculto])
  ) |>
  pivot_longer(everything(), names_to = "medida", values_to = "sd") |>
  mutate(sd = round(sd, 2))
# Justificación: imputar con una mediana asigna el mismo valor a todos los de un
# grupo, así que la varianza de la variable baja (con la mediana global las
# edades imputadas tienen sd = 0). Un modelo que use age verá una relación más
# débil de lo que es y subestimará la incertidumbre: los errores estándar salen
# demasiado pequeños porque trata valores inventados como observados.

# f. Insumo para verificar en Python: edades imputadas por pclass y sex (semilla 2024).
# La llave es index, no id: id se repite (solo 916 valores distintos en 1309 filas).
stopifnot(n_distinct(titanic$index) == nrow(titanic))

titanic_sim |>
  select(index, oculto, age_obs, imp_clase_sexo) |>
  write_csv("ej3_imputacion_r.csv")


############################## EJERCICIO 4 ##################################
# Escalado, outliers y fuga de información (mobile_data.csv)

moviles <- read_csv("mobile_data.csv", show_col_types = FALSE)
cols_mov <- c("ram", "battery_power", "int_memory", "px_height", "px_width")

# a. Función de escalado con match.arg
escalar <- function(df, cols, metodo = c("minmax", "zscore", "robusto")) {
  metodo <- match.arg(metodo)
  f <- switch(
    metodo,
    minmax  = \(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    zscore  = \(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
    robusto = \(x) (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
  )
  df |>
    select(all_of(cols)) |>
    mutate(across(everything(), f))
}

# b. Aporte de cada variable a la distancia euclidiana entre los teléfonos 1 y 2
aporte_distancia <- function(df, cols, etiqueta) {
  d2 <- (df[1, cols] - df[2, cols])^2
  d2 |>
    pivot_longer(everything(), names_to = "variable", values_to = "d2") |>
    mutate(escala = etiqueta, pct = round(d2 / sum(d2) * 100, 1)) |>
    select(escala, variable, pct)
}

aportes <- list(
  aporte_distancia(moviles, cols_mov, "original"),
  aporte_distancia(escalar(moviles, cols_mov, "minmax"),  cols_mov, "minmax"),
  aporte_distancia(escalar(moviles, cols_mov, "zscore"),  cols_mov, "zscore"),
  aporte_distancia(escalar(moviles, cols_mov, "robusto"), cols_mov, "robusto")
) |>
  list_rbind() |>
  pivot_wider(names_from = escala, values_from = pct)
aportes
# Sin escalar la distancia la deciden los píxeles: px_width aporta 64.8 % y
# px_height 33.4 %, mientras que battery_power (1.4 %), ram (0.3 %) e int_memory
# (0.1 %) son casi invisibles. No es porque importen menos, sino porque los píxeles
# se miden en miles y int_memory en decenas: manda la variable con el rango más
# grande, no la más informativa. Al escalar, int_memory pasa a ser la segunda o
# tercera en aporte (31-38 % según el método) y los píxeles bajan a la mitad. ram
# sigue aportando casi 0, pero ahora por una razón real: estos dos teléfonos tienen
# casi la misma RAM (2549 y 2631).

# c. Efecto de un outlier extremo
moviles_outlier <- moviles |> mutate(ram = replace(ram, 1, 100000))

sd_sin_outlier <- c("minmax", "zscore", "robusto") |>
  set_names() |>
  map_dbl(\(m) sd(escalar(moviles_outlier, cols_mov, m)$ram[-1]))

tibble(
  metodo = names(sd_sin_outlier),
  sd_ram_resto = round(sd_sin_outlier, 4),
  sd_sin_contaminar = round(c("minmax", "zscore", "robusto") |>
                              map_dbl(\(m) sd(escalar(moviles, cols_mov, m)$ram[-1])), 4)
)
# Min-Max es el que colapsa: el máximo pasa de 3998 a 100000, así que las 2999
# observaciones restantes quedan comprimidas en la franja [0, 0.04] y su sd cae de
# 0.290 a 0.011 (26 veces menos). Z-score también se contrae, de 1.00 a 0.519,
# porque el outlier infla la sd que se usa como divisor. El método robusto no se
# mueve (0.586 en ambos casos): la mediana y el IQR dependen del orden de los
# datos, no del valor del extremo.

# d. Fuga de información
predictores <- setdiff(names(moviles), "price_range")

set.seed(2024)
idx_train  <- sample(nrow(moviles), size = round(0.7 * nrow(moviles)))
train      <- moviles |> slice(idx_train)
test       <- moviles |> slice(-idx_train)

minimos <- map_dbl(train[predictores], min, na.rm = TRUE)
maximos <- map_dbl(train[predictores], max, na.rm = TRUE)

test_escalado <- test |>
  select(all_of(predictores)) |>
  imap(\(x, nombre) (x - minimos[[nombre]]) / (maximos[[nombre]] - minimos[[nombre]])) |>
  as_tibble()

# Tolerancia: sin ella se contarían valores que se pasan de 1 por 1e-16, que son
# redondeo de punto flotante y no observaciones fuera del rango de entrenamiento.
tol <- 1e-9
fuera_de_rango <- function(x) x < -tol | x > 1 + tol

fuera_rango <- test_escalado |>
  summarise(across(everything(), ~ sum(fuera_de_rango(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_fuera") |>
  filter(n_fuera > 0)
fuera_rango

cat("Valores de prueba fuera de [0,1]:", sum(fuera_rango$n_fuera),
    "de", nrow(test) * length(predictores),
    paste0("(", round(sum(fuera_rango$n_fuera) / (nrow(test) * length(predictores)) * 100, 3), " %)"), "\n")

# Escalar todo junto: por construcción nada queda fuera de [0,1]
todo_escalado <- escalar(moviles, predictores, "minmax")
cat("Fuera de [0,1] al escalar todo junto:",
    sum(map_dbl(todo_escalado, ~ sum(fuera_de_rango(.x)))), "\n")
# Justificación: escalar con el dataset completo es fuga de información aunque
# nunca se toque price_range, porque el mínimo y el máximo de cada variable son
# estadísticos calculados con datos de prueba. El modelo entrena con valores que
# ya "saben" hasta dónde llegan los teléfonos que va a tener que predecir, así que
# la evaluación queda optimista: en producción los datos nuevos sí se salen del
# rango (como pasa arriba con los pocos valores fuera de [0,1]) y el modelo nunca
# vio ese caso. Lo correcto es estimar los parámetros solo con entrenamiento y
# aplicarlos tal cual a prueba, aceptando que algunos valores queden fuera.


############################## EJERCICIO 5 ##################################
# Pipeline funcional y reutilizable (adult.csv)

adult <- read_csv("adult.csv", show_col_types = FALSE)

# a. "?" a NA y prueba de si el faltante es aleatorio
adult_signos <- adult |>
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "?")))

tabla_occ <- adult_signos |>
  mutate(occupation_na = is.na(occupation)) |>
  summarise(
    n = n(),
    ricos = sum(income == ">50K"),
    pct_mas_50k = round(mean(income == ">50K") * 100, 1),
    .by = occupation_na
  ) |>
  arrange(desc(occupation_na))
tabla_occ

prueba <- prop.test(tabla_occ$ricos, tabla_occ$n)
prueba
# Resultado: 10.4 % de >50K entre quienes no tienen occupation, frente a 24.9 %
# entre el resto; p < 2.2e-16.
# Justificación: el faltante no es completamente aleatorio (no es MCAR), depende
# de la variable que queremos predecir. Por eso se crea la categoría "Desconocido"
# en vez de imputar con la moda: el hecho de no tener ocupación registrada es en sí
# mismo información (casi todos son también "?" en workclass, es decir, personas
# fuera del mercado laboral). Imputar con la moda (Prof-specialty) los mezclaría
# con un grupo cuya tasa de ingresos altos es el doble y borraría esa señal.

# b. Valor centinela en capital.gain
adult_signos |>
  summarise(
    n_99999 = sum(capital.gain == 99999),
    siguiente_mayor = max(capital.gain[capital.gain < 99999]),
    pct_ceros = round(mean(capital.gain == 0) * 100, 1)
  )

adult_signos |> filter(capital.gain == 99999) |> count(income)
# Las 159 filas con 99999 son todas de income > 50K y el siguiente valor real es
# 41310: 99999 es un tope de codificación (censura), no una ganancia real.
# Decisión: bandera tope_capital + NA + imputación con la mediana de quienes
# tienen ganancia positiva, y luego log1p.
# Justificación: la bandera conserva la señal (ese tope predice >50K casi perfecto)
# sin afirmar que esas personas ganaron exactamente 99999 ni 41310, que sería
# inventar un dato. El log1p se aplica porque más del 90 % son ceros y el resto se
# reparte en una cola larguísima: sin transformar, la variable estandarizada queda
# dominada por unos pocos valores enormes.

# c. Una función por paso. Todas reciben los parámetros estimados con entrenamiento.
limpiar_signos <- function(df, params = NULL) {
  df |> mutate(across(where(is.character), ~ na_if(str_trim(.x), "?")))
}

tratar_centinela <- function(df, params = NULL) {
  df |>
    mutate(
      tope_capital = as.integer(capital.gain == 99999),
      capital.gain = if_else(capital.gain == 99999, NA_real_, capital.gain)
    )
}

agrupar_paises <- function(df, params = NULL) {
  df |>
    mutate(
      es_estados_unidos = as.integer(coalesce(native.country == "United-States", FALSE)),
      native.country = NULL
    )
}

imputar <- function(df, params) {
  df |>
    mutate(
      capital.gain = coalesce(capital.gain, params$mediana_gain_positiva),
      across(where(is.character), ~ coalesce(.x, "Desconocido")),
      across(where(is.numeric), ~ coalesce(.x, params$medianas[[cur_column()]]))
    )
}

transformar_log <- function(df, params = NULL) {
  df |> mutate(across(c(capital.gain, capital.loss), log1p))
}

# One-hot con los niveles del entrenamiento: una categoría nueva en datos futuros
# queda en ceros (equivale a handle_unknown="ignore" de scikit-learn).
codificar <- function(df, params) {
  dummies <- params$niveles |>
    imap(\(niveles, col) {
      niveles |>
        set_names(paste0(col, "_", str_replace_all(niveles, "[^A-Za-z0-9]+", "_"))) |>
        map(\(nivel) as.integer(df[[col]] == nivel)) |>
        as_tibble()
    }) |>
    unname() |>
    list_cbind()

  df |>
    mutate(income = as.integer(income == ">50K")) |>
    select(-all_of(names(params$niveles)), -any_of(params$cols_descartadas)) |>
    bind_cols(dummies)
}

estandarizar <- function(df, params) {
  df |>
    mutate(across(all_of(params$cols_continuas),
                  ~ (.x - params$medias[[cur_column()]]) / params$sd[[cur_column()]]))
}

pasos_adult <- list(limpiar_signos, tratar_centinela, agrupar_paises,
                    imputar, transformar_log, codificar, estandarizar)

# e. Parámetros estimados SOLO con los datos de entrenamiento
ajustar_parametros <- function(df) {
  base <- df |> limpiar_signos() |> tratar_centinela() |> agrupar_paises()

  params <- list(
    mediana_gain_positiva = median(base$capital.gain[base$capital.gain > 0], na.rm = TRUE),
    medianas = base |> select(where(is.numeric)) |> map(median, na.rm = TRUE),
    cols_descartadas = c("fnlwgt", "education", "occupation", "relationship", "race"),
    cols_continuas = c("age", "education.num", "capital.gain", "capital.loss",
                       "hours.per.week")
  )

  base_log <- base |> imputar(params) |> transformar_log()

  params$niveles <- list(
    workclass      = sort(unique(coalesce(base_log$workclass, "Desconocido"))),
    marital.status = sort(unique(coalesce(base_log$marital.status, "Desconocido"))),
    sex            = sort(unique(base_log$sex))
  )
  params$medias <- base_log |> select(all_of(params$cols_continuas)) |> map(mean)
  params$sd     <- base_log |> select(all_of(params$cols_continuas)) |> map(sd)
  params
}

# d. Composición del pipeline con reduce()
preprocesar_adult <- function(df, params = NULL) {
  if (is.null(params)) params <- ajustar_parametros(df)

  resultado <- reduce(pasos_adult, \(datos, paso) paso(datos, params), .init = df)

  stopifnot(
    sum(is.na(resultado)) == 0,
    all(map_lgl(resultado, is.numeric)),
    nrow(resultado) == nrow(df)
  )
  attr(resultado, "parametros") <- params
  resultado
}

adult_listo <- preprocesar_adult(adult)
cat("adult preprocesado:", nrow(adult_listo), "×", ncol(adult_listo), "\n")
glimpse(adult_listo)

# Prueba con partición 70/30 usando solo los parámetros del entrenamiento
set.seed(2024)
idx_adult    <- sample(nrow(adult), size = round(0.7 * nrow(adult)))
adult_train  <- adult |> slice(idx_adult)
adult_test   <- adult |> slice(-idx_adult)

params_train <- ajustar_parametros(adult_train)
train_listo  <- preprocesar_adult(adult_train, params_train)
test_listo   <- preprocesar_adult(adult_test,  params_train)

stopifnot(identical(names(train_listo), names(test_listo)))

tibble(
  particion = c("entrenamiento", "prueba"),
  media_age = c(mean(train_listo$age), mean(test_listo$age)),
  sd_age    = c(sd(train_listo$age),   sd(test_listo$age))
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))
# En entrenamiento la media es 0 y la sd 1 por construcción; en prueba quedan
# cerca pero no exactamente, que es justo la señal de que no hubo fuga: los
# parámetros vienen del entrenamiento y a prueba solo se le aplican.
#
# Parámetros que hay que guardar para datos nuevos: la mediana de capital.gain
# positivo, las medianas de cada numérica, los niveles de workclass,
# marital.status y sex (para que las columnas dummy sean siempre las mismas) y las
# medias y desviaciones de las continuas, calculadas después del log.
