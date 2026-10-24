library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)

set.seed(2024)
setwd("C:/Users/Estudiante/Documents/david_esteban_marquez_bayona/david_esteban_marquez_bayona/Taller_06")
titanic <- read_csv("titanic.csv",      show_col_types = FALSE)
adult   <- read_csv("adult.csv",        show_col_types = FALSE)
precios <- read_csv("house_prices.csv", show_col_types = FALSE)
moviles <- read_csv("mobile_data.csv",  show_col_types = FALSE)

detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}


#### Ejercicio 1 #################

# 1.a
resumen_na_precios <- precios |>
  map_df(~ sum(is.na(.x))) |>
  pivot_longer(everything(), names_to = "columna", values_to = "n_na") |>
  mutate(pct_na = round(n_na / nrow(precios) * 100, 1)) |>
  filter(n_na > 0) |>
  arrange(desc(pct_na))
print(resumen_na_precios, n = Inf)

umbral_cols <- 0.50
precios_sin_na <- precios |>
  select(where(~ mean(is.na(.x)) <= umbral_cols))
cols_perdidas <- setdiff(names(precios), names(precios_sin_na))
cat("Columnas eliminadas:", cols_perdidas, "\n")

precios |>
  count(PoolQC, tiene_piscina = PoolArea > 0)

precios |>
  filter(!is.na(PoolQC)) |>
  summarise(mediana_con_piscina = median(SalePrice), n = n()) |>
  bind_cols(precios |>
              filter(is.na(PoolQC)) |>
              summarise(mediana_sin_piscina = median(SalePrice)))

# 1.b
especificaciones <- list(
  garage  = list(cols = c("GarageType", "GarageYrBlt", "GarageFinish",
                          "GarageQual", "GarageCond"),
                 area = "GarageArea"),
  sotano  = list(cols = c("BsmtQual", "BsmtCond", "BsmtExposure",
                          "BsmtFinType1", "BsmtFinType2"),
                 area = "TotalBsmtSF"),
  piscina = list(cols = "PoolQC",
                 area = "PoolArea")
)

cruzar_ausencia <- function(df, cols, area) {
  map(cols, \(col) tibble(
    columna    = col,
    col_na     = is.na(df[[col]]),
    area_cero  = df[[area]] == 0
  ) |>
    count(columna, col_na, area_cero)) |>
    list_rbind()
}

contar_faltantes_reales <- function(df, cols, area) {
  area_cero <- coalesce(df[[area]] == 0, FALSE)
  tibble(
    columna        = cols,
    na_reales      = map_int(cols, \(col) sum(is.na(df[[col]]) & !area_cero)),
    inconsistentes = map_int(cols, \(col) sum(!is.na(df[[col]]) & area_cero))
  )
}

especificaciones |>
  map(\(e) cruzar_ausencia(precios, e$cols, e$area)) |>
  list_rbind(names_to = "grupo") |>
  print(n = Inf)

faltantes_reales <- especificaciones |>
  map(\(e) contar_faltantes_reales(precios, e$cols, e$area)) |>
  list_rbind(names_to = "grupo")
print(faltantes_reales, n = Inf)

especificaciones |>
  map_int(\(e) precios |>
            filter(if_any(all_of(e$cols), is.na),
                   !coalesce(.data[[e$area]] == 0, FALSE)) |>
            nrow())

# 1.c
recodificar_ausencia <- function(df, cols, area) {
  area_cero <- coalesce(df[[area]] == 0, FALSE)
  df |>
    mutate(across(all_of(cols) & where(is.character),
                  ~ if_else(is.na(.x) & area_cero, "Ninguno", .x)))
}

precios_rec <- reduce(
  especificaciones,
  \(df, e) recodificar_ausencia(df, e$cols, e$area),
  .init = precios
)

verificacion_rec <- especificaciones |>
  map(\(e) {
    cols_chr <- precios |> select(all_of(e$cols) & where(is.character)) |> names()
    tibble(
      columna      = cols_chr,
      na_despues   = map_int(cols_chr, \(col) sum(is.na(precios_rec[[col]]))),
      n_ninguno    = map_int(cols_chr, \(col) sum(precios_rec[[col]] == "Ninguno", na.rm = TRUE))
    )
  }) |>
  list_rbind(names_to = "grupo") |>
  left_join(faltantes_reales, by = c("grupo", "columna"))
print(verificacion_rec, n = Inf)

stopifnot(
  all(verificacion_rec$na_despues == verificacion_rec$na_reales),
  sum(is.na(precios_rec$GarageYrBlt)) == sum(is.na(precios$GarageYrBlt)),
  nrow(precios_rec) == nrow(precios)
)

# 1.d
mediana_global_lf <- median(precios_rec$LotFrontage, na.rm = TRUE)

precios_lf <- precios_rec |>
  mutate(lf_global = if_else(is.na(LotFrontage), mediana_global_lf, LotFrontage)) |>
  mutate(lf_barrio = if_else(is.na(LotFrontage),
                             median(LotFrontage, na.rm = TRUE),
                             LotFrontage),
         .by = Neighborhood) |>
  mutate(lf_barrio = coalesce(lf_barrio, lf_global))

stopifnot(!anyNA(precios_lf$lf_global), !anyNA(precios_lf$lf_barrio))

diferencia_barrio <- precios_rec |>
  summarise(
    mediana_barrio = median(LotFrontage, na.rm = TRUE),
    n_imputados    = sum(is.na(LotFrontage)),
    .by = Neighborhood
  ) |>
  mutate(
    mediana_global = mediana_global_lf,
    diferencia     = mediana_barrio - mediana_global
  ) |>
  arrange(desc(abs(diferencia)))
print(diferencia_barrio, n = Inf)

grafico_lf <- diferencia_barrio |>
  ggplot(aes(x = reorder(Neighborhood, diferencia), y = diferencia,
             fill = diferencia > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("firebrick", "steelblue"), guide = "none") +
  labs(title = "Mediana de LotFrontage por barrio menos mediana global",
       x = NULL, y = "Diferencia (pies)") +
  theme_minimal(base_size = 12)
print(grafico_lf)

# 1.e
precios_edad <- precios_lf |>
  mutate(
    edad_vivienda  = YrSold - YearBuilt,
    flag_edad      = edad_vivienda < 0,
    flag_garage    = coalesce(GarageYrBlt > YrSold | GarageYrBlt > 2010, FALSE),
    flag_anio      = YearBuilt > 2010 | YearRemodAdd > 2010 | YrSold > 2010
  )

precios_edad |>
  filter(if_any(c(flag_edad, flag_garage, flag_anio), ~ .x)) |>
  select(Id, YearBuilt, YearRemodAdd, GarageYrBlt, YrSold, edad_vivienda,
         flag_edad, flag_garage, flag_anio) |>
  print(n = Inf)

precios_edad |>
  summarise(across(c(flag_edad, flag_garage, flag_anio), sum))

precios_edad <- precios_edad |>
  filter(!flag_anio) |>
  mutate(
    dato_corregido = flag_edad | flag_garage,
    edad_vivienda  = case_when(
      edad_vivienda < 0 ~ 0,
      .default          = edad_vivienda
    ),
    GarageYrBlt    = case_when(
      GarageYrBlt > 2010   ~ NA_real_,
      GarageYrBlt > YrSold ~ YrSold,
      .default             = GarageYrBlt
    )
  ) |>
  select(-starts_with("flag_"))

stopifnot(
  all(precios_edad$edad_vivienda >= 0),
  all(precios_edad$GarageYrBlt <= precios_edad$YrSold, na.rm = TRUE),
  all(precios_edad$YearBuilt <= 2010)
)
count(precios_edad, dato_corregido)

# 1.f
cortes_edad <- quantile(precios_edad$edad_vivienda, probs = seq(0, 1, 0.25))
cortes_edad

precios_edad <- precios_edad |>
  mutate(
    grupo_cut   = cut(edad_vivienda, breaks = unique(cortes_edad),
                      include.lowest = TRUE),
    grupo_ntile = ntile(edad_vivienda, 4)
  )

precios_edad |>
  count(grupo_cut, grupo_ntile) |>
  print(n = Inf)

precios_edad |>
  summarise(n_grupos_ntile = n_distinct(grupo_ntile),
            n_grupos_cut   = n_distinct(grupo_cut),
            n              = n(),
            .by = edad_vivienda) |>
  filter(n_grupos_ntile > 1)

precios_edad |>
  summarise(n = n(), mediana_precio = median(SalePrice), .by = grupo_cut) |>
  arrange(grupo_cut)

precios_edad |>
  summarise(n = n(), mediana_precio = median(SalePrice), .by = grupo_ntile) |>
  arrange(grupo_ntile)


########## Ejercicio 2 ##############

# 2.a
titanic_out <- titanic |>
  mutate(
    out_global_15 = detectar_outliers_iqr(fare, k = 1.5),
    out_global_3  = detectar_outliers_iqr(fare, k = 3)
  )

titanic_out |>
  summarise(
    n_k15   = sum(out_global_15, na.rm = TRUE),
    pct_k15 = round(mean(out_global_15, na.rm = TRUE) * 100, 2),
    n_k3    = sum(out_global_3, na.rm = TRUE),
    pct_k3  = round(mean(out_global_3, na.rm = TRUE) * 100, 2)
  )

# 2.b
titanic_out <- titanic_out |>
  mutate(out_clase = detectar_outliers_iqr(fare), .by = pclass)

titanic_out |>
  summarise(
    n_out_global   = sum(out_global_15, na.rm = TRUE),
    n_out_clase    = sum(out_clase, na.rm = TRUE),
    dejan_de_serlo = sum(out_global_15 & !out_clase, na.rm = TRUE),
    nuevos         = sum(!out_global_15 & out_clase, na.rm = TRUE),
    .by = pclass
  ) |>
  arrange(pclass)

titanic_out |>
  summarise(
    q1 = quantile(fare, 0.25, na.rm = TRUE),
    mediana = median(fare, na.rm = TRUE),
    q3 = quantile(fare, 0.75, na.rm = TRUE),
    .by = pclass
  ) |>
  arrange(pclass)

# 2.c
zscore_robusto <- function(x) (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)

titanic_out <- titanic_out |>
  mutate(
    z_fare         = zscore(fare),
    z_rob_fare     = zscore_robusto(fare),
    out_z_clasico  = abs(z_fare) > 3,
    out_robusto    = abs(z_rob_fare) > 3.5
  )

titanic_out |>
  summarise(
    n_out_z_clasico = sum(out_z_clasico, na.rm = TRUE),
    n_out_robusto   = sum(out_robusto, na.rm = TRUE)
  )

titanic_out |>
  summarise(media = mean(fare, na.rm = TRUE), sd = sd(fare, na.rm = TRUE),
            mediana = median(fare, na.rm = TRUE), mad = mad(fare, na.rm = TRUE))

# 2.d
titanic_out |>
  filter(!is.na(fare)) |>
  count(out_global_15, out_clase, out_robusto)

titanic_out |>
  filter(!is.na(fare)) |>
  mutate(n_criterios = out_global_15 + out_clase + out_robusto) |>
  filter(n_criterios %in% c(1, 2)) |>
  count(pclass, out_global_15, out_clase, out_robusto)

titanic_out |>
  filter(out_clase, !out_global_15, !out_robusto) |>
  select(name, pclass, fare) |>
  arrange(pclass, desc(fare)) |>
  print(n = Inf)

titanic_out |>
  filter(out_robusto, !out_global_15) |>
  select(name, pclass, fare) |>
  arrange(desc(fare)) |>
  print(n = Inf)

# 2.e
titanic |>
  filter(fare == 0) |>
  select(name, pclass, age, sex, ticket, embarked, survived) |>
  print(n = Inf)

titanic |>
  filter(fare == 0) |>
  count(pclass, ticket)

titanic_fare <- titanic |>
  mutate(fare_tratada = if_else(fare == 0, NA_real_, fare)) |>
  mutate(fare_tratada = if_else(is.na(fare_tratada),
                                median(fare_tratada, na.rm = TRUE),
                                fare_tratada),
         .by = pclass) |>
  mutate(fare_imputada = is.na(fare) | fare == 0)

stopifnot(
  !anyNA(titanic_fare$fare_tratada),
  all(titanic_fare$fare_tratada > 0),
  sum(titanic_fare$fare_imputada) == sum(titanic$fare == 0, na.rm = TRUE) + sum(is.na(titanic$fare))
)

titanic_fare |>
  filter(fare_imputada) |>
  count(pclass, fare_tratada)

# 2.f
titanic_out <- titanic_out |>
  mutate(
    fare_log    = log1p(fare),
    out_log     = detectar_outliers_iqr(fare_log)
  )

limites_log <- quantile(titanic_out$fare_log, c(0.25, 0.75), na.rm = TRUE)
iqr_log     <- diff(limites_log)

titanic_out |>
  filter(!is.na(fare)) |>
  summarise(
    n_out_original = sum(out_global_15),
    n_out_log      = sum(out_log),
    out_log_bajos  = sum(fare_log < limites_log[1] - 1.5 * iqr_log),
    out_log_altos  = sum(fare_log > limites_log[2] + 1.5 * iqr_log)
  )

titanic_out |>
  filter(out_log) |>
  count(fare, pclass)

titanic_out |>
  filter(!is.na(fare)) |>
  summarise(correlacion_rangos = cor(fare, fare_log, method = "spearman"))


########## Ejercicio 3 ##############

extraer_titulo <- function(nombre) {
  titulo <- str_extract(nombre, "(?<=, )[A-Za-z]+(?=\\.)")
  case_when(
    titulo %in% c("Mr")                ~ "Mr",
    titulo %in% c("Mrs", "Mme")        ~ "Mrs",
    titulo %in% c("Miss", "Ms", "Mlle") ~ "Miss",
    titulo == "Master"                 ~ "Master",
    .default                           = "Otro"
  )
}

titanic_edad <- titanic |>
  filter(!is.na(age)) |>
  mutate(titulo_grupo = extraer_titulo(name))

stopifnot(nrow(titanic_edad) == 1046)
count(titanic_edad, titulo_grupo, sort = TRUE)

ocultar_edades <- function(df, semilla, prop = 0.20) {
  set.seed(semilla)
  ocultos <- sample(nrow(df), round(prop * nrow(df)))
  df |>
    mutate(
      age_real = age,
      oculto   = row_number() %in% ocultos,
      age      = if_else(oculto, NA_real_, age)
    )
}

imputar_estrategias <- function(df) {
  df |>
    mutate(imp_global = if_else(is.na(age), median(age, na.rm = TRUE), age)) |>
    mutate(imp_clase_sexo = if_else(is.na(age), median(age, na.rm = TRUE), age),
           .by = c(pclass, sex)) |>
    mutate(imp_titulo = if_else(is.na(age), median(age, na.rm = TRUE), age),
           .by = titulo_grupo)
}

calcular_errores <- function(df) {
  df |>
    filter(oculto) |>
    pivot_longer(starts_with("imp_"), names_to = "estrategia", values_to = "age_imp") |>
    summarise(
      MAE  = mean(abs(age_imp - age_real)),
      RMSE = sqrt(mean((age_imp - age_real)^2)),
      .by = estrategia
    )
}

# 3.a
titanic_sim <- ocultar_edades(titanic_edad, 2024)
stopifnot(sum(titanic_sim$oculto) == round(0.2 * 1046),
          all(titanic_sim$age_real == titanic_edad$age))

# 3.b
titanic_sim <- imputar_estrategias(titanic_sim)
stopifnot(!anyNA(select(titanic_sim, starts_with("imp_"))),
          all(titanic_sim$imp_global[!titanic_sim$oculto] ==
                titanic_sim$age_real[!titanic_sim$oculto]))

titanic_sim |>
  filter(!oculto) |>
  summarise(mediana = median(age), n = n(), .by = titulo_grupo)

# 3.c
errores_2024 <- calcular_errores(titanic_sim) |> arrange(MAE)
errores_2024
ganadora_2024 <- errores_2024$estrategia[1]

# 3.d
simular_imputacion <- function(semilla) {
  titanic_edad |>
    ocultar_edades(semilla) |>
    imputar_estrategias() |>
    calcular_errores() |>
    mutate(semilla = semilla)
}

set.seed(2024)
semillas <- sample.int(1e6, 200)

resultados_sim <- semillas |>
  map(simular_imputacion) |>
  list_rbind()

stopifnot(nrow(resultados_sim) == 200 * 3)

resultados_sim |>
  summarise(
    MAE_medio  = mean(MAE),
    MAE_sd     = sd(MAE),
    RMSE_medio = mean(RMSE),
    .by = estrategia
  ) |>
  arrange(MAE_medio)

grafico_mae <- resultados_sim |>
  ggplot(aes(x = MAE, fill = estrategia)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Distribución del MAE en 200 simulaciones",
       x = "MAE (años)", y = "Densidad", fill = "Estrategia") +
  theme_minimal(base_size = 12)
print(grafico_mae)

ganadoras <- resultados_sim |>
  slice_min(MAE, n = 1, with_ties = FALSE, by = semilla) |>
  count(estrategia) |>
  mutate(pct = round(n / sum(n) * 100, 1))
ganadoras

ganadoras |> filter(estrategia == ganadora_2024)

# 3.e
titanic_sim |>
  summarise(
    sd_real        = sd(age_real),
    sd_global      = sd(imp_global),
    sd_clase_sexo  = sd(imp_clase_sexo),
    sd_titulo      = sd(imp_titulo)
  )

bind_rows(
  tibble(valor = titanic_sim$age_real,       tipo = "Real"),
  tibble(valor = titanic_sim$imp_global,     tipo = "Mediana global"),
  tibble(valor = titanic_sim$imp_clase_sexo, tipo = "Clase y sexo"),
  tibble(valor = titanic_sim$imp_titulo,     tipo = "Título")
) |>
  ggplot(aes(x = valor, fill = tipo)) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Distribución de edad: real vs. imputada",
       x = "Edad", y = "Densidad", fill = NULL) +
  theme_minimal(base_size = 12)


########### Ejercicio 4 ###############

cols_mov <- c("ram", "battery_power", "int_memory", "px_height", "px_width")

# 4.a
escalar <- function(df, cols, metodo = c("minmax", "zscore", "robusto")) {
  metodo <- match.arg(metodo)
  robusto <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    (x - median(x, na.rm = TRUE)) / (q3 - q1)
  }
  f <- switch(metodo,
              minmax  = min_max,
              zscore  = zscore,
              robusto = robusto)
  df |>
    select(all_of(cols)) |>
    mutate(across(everything(), f))
}

metodos <- c("minmax", "zscore", "robusto")

escalados <- metodos |>
  set_names() |>
  map(\(m) escalar(moviles, cols_mov, m))

escalados |>
  map(\(d) d |> map_df(~ tibble(min = min(.x), max = max(.x),
                                media = mean(.x), mediana = median(.x),
                                sd = sd(.x)),
                       .id = "variable")) |>
  list_rbind(names_to = "metodo") |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = Inf)

stopifnot(
  all(map_lgl(escalados$minmax, ~ min(.x) == 0 & max(.x) == 1)),
  all(map_lgl(escalados$zscore, ~ abs(mean(.x)) < 1e-10 & abs(sd(.x) - 1) < 1e-10)),
  all(map_lgl(escalados$robusto, ~ median(.x) == 0))
)

# 4.b
contribucion_distancia <- function(df) {
  d2 <- map_dbl(df, \(x) (x[1] - x[2])^2)
  tibble(variable = names(d2), pct = round(d2 / sum(d2) * 100, 2))
}

versiones <- c(list(original = select(moviles, all_of(cols_mov))), escalados)

contribuciones <- versiones |>
  map(contribucion_distancia) |>
  list_rbind(names_to = "metodo") |>
  pivot_wider(names_from = metodo, values_from = pct)
contribuciones

stopifnot(all(abs(colSums(select(contribuciones, -variable)) - 100) < 0.05))

moviles |> slice(1:2) |> select(all_of(cols_mov))

# 4.c
moviles_mod <- moviles
moviles_mod$ram[1] <- 100000

sd_ram <- tibble(
  metodo       = metodos,
  sd_original  = map_dbl(metodos, \(m) sd(escalar(moviles, "ram", m)$ram[-1])),
  sd_con_error = map_dbl(metodos, \(m) sd(escalar(moviles_mod, "ram", m)$ram[-1]))
) |>
  mutate(razon = round(sd_con_error / sd_original, 4))
sd_ram

metodos |>
  set_names() |>
  map_df(\(m) {
    x <- escalar(moviles_mod, "ram", m)$ram[-1]
    tibble(min = min(x), max = max(x), rango = max(x) - min(x))
  }, .id = "metodo")

# 4.d
cols_pred <- setdiff(names(moviles), "price_range")

set.seed(2024)
idx_train   <- sample(nrow(moviles), round(0.7 * nrow(moviles)))
mov_train   <- moviles[idx_train, ]
mov_test    <- moviles[-idx_train, ]

stopifnot(nrow(mov_train) + nrow(mov_test) == nrow(moviles),
          length(intersect(idx_train, setdiff(seq_len(nrow(moviles)), idx_train))) == 0)

param_minmax <- tibble(
  variable = cols_pred,
  minimo   = map_dbl(cols_pred, \(col) min(mov_train[[col]])),
  maximo   = map_dbl(cols_pred, \(col) max(mov_train[[col]]))
)

mov_test_esc <- pmap(
  list(mov_test[cols_pred], param_minmax$minimo, param_minmax$maximo),
  \(x, mn, mx) (x - mn) / (mx - mn)
) |>
  as_tibble()

fuera_rango <- mov_test_esc |>
  map_df(~ tibble(n_fuera = sum(.x < 0 | .x > 1)), .id = "variable") |>
  filter(n_fuera > 0)
fuera_rango

total_fuera <- sum(map_int(mov_test_esc, ~ sum(.x < 0 | .x > 1)))
pct_fuera   <- total_fuera / (nrow(mov_test_esc) * ncol(mov_test_esc)) * 100
cat("Valores de prueba fuera de [0, 1]:", total_fuera,
    "(", round(pct_fuera, 3), "% )\n")

mov_todo_esc <- escalar(moviles, cols_pred, "minmax")
mov_test_fuga <- mov_todo_esc[-idx_train, ]
cat("Valores de prueba fuera de [0, 1] escalando todo junto:",
    sum(map_int(mov_test_fuga, ~ sum(.x < 0 | .x > 1))), "\n")

param_minmax |>
  mutate(
    minimo_total = map_dbl(variable, \(col) min(moviles[[col]])),
    maximo_total = map_dbl(variable, \(col) max(moviles[[col]]))
  ) |>
  filter(minimo != minimo_total | maximo != maximo_total)


################## Ejercicio 5 #################

# 5.a
adult_na <- adult |>
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "?")))

adult |>
  summarise(across(where(is.character), ~ sum(str_trim(.x) == "?", na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "columna", values_to = "n_interrogacion") |>
  left_join(
    adult_na |>
      map_df(~ sum(is.na(.x))) |>
      pivot_longer(everything(), names_to = "columna", values_to = "n_na"),
    by = "columna"
  ) |>
  filter(n_na > 0)

tabla_occ <- adult_na |>
  mutate(occupation_faltante = is.na(occupation)) |>
  summarise(
    n        = n(),
    n_altos  = sum(income == ">50K"),
    prop_50k = round(mean(income == ">50K"), 4),
    .by = occupation_faltante
  )
tabla_occ

prop.test(x = tabla_occ$n_altos, n = tabla_occ$n)

adult_na |>
  filter(is.na(occupation)) |>
  count(workclass_faltante = is.na(workclass))

# 5.b
adult_na |>
  summarise(
    n_99999          = sum(capital.gain == 99999),
    siguiente_maximo = max(capital.gain[capital.gain < 99999]),
    pct_ceros        = round(mean(capital.gain == 0) * 100, 2),
    pct_positivos    = round(mean(capital.gain > 0 & capital.gain < 99999) * 100, 2)
  )

adult_na |>
  filter(capital.gain > 0) |>
  count(capital.gain, sort = TRUE) |>
  slice_max(capital.gain, n = 5)

adult_na |>
  mutate(grupo_cg = case_when(
    capital.gain == 0     ~ "cero",
    capital.gain == 99999 ~ "99999",
    .default              = "positivo"
  )) |>
  summarise(n = n(), prop_50k = round(mean(income == ">50K"), 3), .by = grupo_cg)

# 5.c
cols_onehot  <- c("workclass", "marital.status", "sex",
                  "occupation", "relationship", "race")
cols_escalar <- c("age", "fnlwgt", "education.num", "capital.gain",
                  "capital.loss", "hours.per.week")

limpiar_signos <- function(df) {
  df |>
    mutate(across(where(is.character), ~ na_if(str_trim(.x), "?")))
}

tratar_centinela <- function(df, techo) {
  df |>
    mutate(
      cg_centinela = as.integer(capital.gain == 99999),
      cg_positiva  = as.integer(capital.gain > 0),
      capital.gain = if_else(capital.gain == 99999, techo, capital.gain)
    )
}

agrupar_paises <- function(df) {
  df |>
    mutate(native.country = if_else(native.country == "United-States",
                                    "Estados Unidos", "Otro"))
}

imputar <- function(df, params) {
  df <- df |>
    mutate(across(c(workclass, occupation), ~ replace_na(.x, "Desconocido")))
  cols_med  <- names(params$medianas)
  cols_moda <- names(params$modas)
  df[cols_med]  <- map2(df[cols_med],  params$medianas, replace_na)
  df[cols_moda] <- map2(df[cols_moda], params$modas,    replace_na)
  df
}

transformar_log <- function(df) {
  df |>
    mutate(across(c(capital.gain, capital.loss), log1p))
}

codificar <- function(df, niveles) {
  dummies <- map2(niveles, names(niveles), \(lv, col) {
    map(lv, \(l) as.integer(df[[col]] == l)) |>
      set_names(paste0(col, "_", lv)) |>
      as_tibble()
  }) |>
    unname() |>
    list_cbind() |>
    rename_with(~ str_replace_all(.x, "[^A-Za-z0-9_]", "_"))
  df |>
    select(-all_of(names(niveles)), -education) |>
    mutate(
      native.country = as.integer(native.country == "Estados Unidos"),
      income         = as.integer(income == ">50K")
    ) |>
    bind_cols(dummies)
}

estandarizar <- function(df, params) {
  cols <- names(params$medias)
  df[cols] <- pmap(list(df[cols], params$medias, params$sds),
                   \(x, m, s) (x - m) / s)
  df
}

ajustar_parametros <- function(df) {
  d1 <- limpiar_signos(df)
  techo <- max(d1$capital.gain[d1$capital.gain < 99999])
  d2 <- d1 |>
    tratar_centinela(techo) |>
    agrupar_paises()
  medianas <- d2 |>
    summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) |>
    as.list()
  modas <- d2 |>
    summarise(across(where(is.character) & !income, moda)) |>
    as.list()
  d3 <- d2 |>
    imputar(list(medianas = medianas, modas = modas)) |>
    transformar_log()
  niveles <- d3 |>
    select(all_of(cols_onehot)) |>
    map(~ sort(unique(.x)))
  medias <- d3 |> summarise(across(all_of(cols_escalar), mean)) |> as.list()
  sds    <- d3 |> summarise(across(all_of(cols_escalar), sd))   |> as.list()
  list(techo = techo, medianas = medianas, modas = modas,
       niveles = niveles, medias = medias, sds = sds)
}

# 5.d
preprocesar_adult <- function(df, params = ajustar_parametros(df)) {
  pasos <- list(
    limpiar_signos,
    \(d) tratar_centinela(d, params$techo),
    agrupar_paises,
    \(d) imputar(d, params),
    transformar_log,
    \(d) codificar(d, params$niveles),
    \(d) estandarizar(d, params)
  )
  salida <- reduce(pasos, \(d, f) f(d), .init = df)
  stopifnot(
    !anyNA(salida),
    all(map_lgl(salida, is.numeric)),
    nrow(salida) == nrow(df)
  )
  salida
}

adult_proc <- preprocesar_adult(adult)
dim(adult_proc)
glimpse(adult_proc)

adult_proc |>
  select(all_of(cols_escalar)) |>
  map_df(~ tibble(media = round(mean(.x), 4), sd = round(sd(.x), 4)), .id = "variable")

stopifnot(
  all(map_lgl(select(adult_proc, all_of(cols_escalar)), ~ abs(mean(.x)) < 1e-8)),
  sum(adult_proc$cg_centinela) == sum(adult$capital.gain == 99999),
  all(adult_proc$income %in% c(0, 1))
)

# 5.e
set.seed(2024)
idx_adult    <- sample(nrow(adult), round(0.7 * nrow(adult)))
adult_train  <- adult[idx_adult, ]
adult_test   <- adult[-idx_adult, ]

params_train <- ajustar_parametros(adult_train)
str(params_train[c("techo", "medianas", "modas", "medias", "sds")])
params_train$niveles

adult_train_proc <- preprocesar_adult(adult_train, params_train)
adult_test_proc  <- preprocesar_adult(adult_test,  params_train)

stopifnot(
  identical(names(adult_train_proc), names(adult_test_proc)),
  nrow(adult_train_proc) == nrow(adult_train),
  nrow(adult_test_proc)  == nrow(adult_test)
)

bind_rows(
  adult_train_proc |> select(all_of(cols_escalar)) |>
    map_df(~ tibble(media = mean(.x), sd = sd(.x)), .id = "variable") |>
    mutate(conjunto = "entrenamiento"),
  adult_test_proc |> select(all_of(cols_escalar)) |>
    map_df(~ tibble(media = mean(.x), sd = sd(.x)), .id = "variable") |>
    mutate(conjunto = "prueba")
) |>
  mutate(across(c(media, sd), ~ round(.x, 4))) |>
  pivot_wider(names_from = conjunto, values_from = c(media, sd))

