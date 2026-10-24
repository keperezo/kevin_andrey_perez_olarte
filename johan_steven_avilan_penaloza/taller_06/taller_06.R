library(tidyverse)

detectar_outliers_iqr <- function(x, k = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - k * iqr) | x > (q3 + k * iqr)
}

moda <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

#Ejercicio 1
precios <- read_csv("johan_steven_avilan_penaloza/data/house_prices.csv", show_col_types = FALSE)

resumen_na <- precios |>
  map_df(~ sum(is.na(.x))) |>
  pivot_longer(everything(), names_to = "columna", values_to = "n_na") |>
  mutate(pct_na = round(n_na / nrow(precios) * 100, 1)) |>
  arrange(desc(pct_na))

umbral_cols    <- 0.50
precios_limpio <- precios |>
  select(where(~ mean(is.na(.x)) <= umbral_cols))

cols_perdidas <- setdiff(names(precios), names(precios_limpio))
cols_perdidas

grupos <- list(
  garaje  = list(cols = c("GarageType", "GarageFinish", "GarageQual",
                          "GarageCond", "GarageYrBlt"),
                 area = "GarageArea"),
  sotano  = list(cols = c("BsmtQual", "BsmtCond", "BsmtExposure",
                          "BsmtFinType1", "BsmtFinType2"),
                 area = "TotalBsmtSF"),
  piscina = list(cols = "PoolQC", area = "PoolArea")
)

faltantes_grupo <- function(g) {
  sin_area <- precios[[g$area]] == 0
  falla <- map(g$cols, ~ is.na(sin_area) | is.na(precios[[.x]]) != sin_area)
  sum(reduce(falla, `|`))
}

faltantes_reales <- map_int(grupos, faltantes_grupo)
faltantes_reales

recodificar_ausencia <- function(df, cols, area) {
  cols_chr       <- cols[map_lgl(cols, ~ is.character(df[[.x]]))]
  es_estructural <- !is.na(df[[area]]) & df[[area]] == 0
  df |>
    mutate(across(all_of(cols_chr),
                  ~ if_else(is.na(.x) & es_estructural, "Ninguno", .x)))
}

precios_rec <- reduce(
  grupos,
  ~ recodificar_ausencia(.x, .y$cols, .y$area),
  .init = precios
)

na_despues <- precios_rec |>
  summarise(across(c(GarageType, BsmtQual, PoolQC), ~ sum(is.na(.x))))
na_despues

precios_lf <- precios |>
  mutate(lf_global = if_else(is.na(LotFrontage),
                             median(LotFrontage, na.rm = TRUE), LotFrontage)) |>
  mutate(lf_barrio = if_else(is.na(LotFrontage),
                             median(LotFrontage, na.rm = TRUE), LotFrontage),
         .by = Neighborhood)

dif_barrio <- precios_lf |>
  filter(is.na(LotFrontage)) |>
  summarise(mediana_barrio = first(lf_barrio),
            mediana_global = first(lf_global),
            .by = Neighborhood) |>
  mutate(diferencia = mediana_barrio - mediana_global) |>
  arrange(desc(abs(diferencia)))
dif_barrio

grafico_dif <- dif_barrio |>
  ggplot(aes(x = reorder(Neighborhood, diferencia), y = diferencia)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Mediana por barrio menos mediana global (LotFrontage)",
       x = NULL, y = "Diferencia") +
  theme_minimal(base_size = 13)
grafico_dif

precios_edad <- precios |>
  filter(YearBuilt <= 2010, YearRemodAdd <= 2010) |>
  mutate(
    edad_vivienda  = YrSold - YearBuilt,
    dato_corregido = edad_vivienda < 0 | coalesce(GarageYrBlt > YrSold, FALSE),
    edad_vivienda  = case_when(edad_vivienda < 0 ~ 0, .default = edad_vivienda),
    GarageYrBlt    = case_when(GarageYrBlt > YrSold ~ NA_real_,
                               .default = GarageYrBlt)
  )

stopifnot(all(precios_edad$edad_vivienda >= 0),
          all(precios_edad$GarageYrBlt <= precios_edad$YrSold, na.rm = TRUE))
count(precios_edad, dato_corregido)

etiquetas <- c("Q1", "Q2", "Q3", "Q4")
cortes    <- quantile(precios_edad$edad_vivienda, probs = seq(0, 1, 0.25))

precios_edad <- precios_edad |>
  mutate(grupo_cut   = cut(edad_vivienda, breaks = cortes, labels = etiquetas,
                           include.lowest = TRUE),
         grupo_ntile = factor(ntile(edad_vivienda, 4), labels = etiquetas))

comparacion_grupos <- count(precios_edad, grupo_cut, grupo_ntile)
comparacion_grupos

mismos_datos_distinto_grupo <- precios_edad |>
  summarise(n_ntile = n_distinct(grupo_ntile), .by = edad_vivienda) |>
  filter(n_ntile > 1)
mismos_datos_distinto_grupo

mediana_precio_grupo <- precios_edad |>
  summarise(mediana_precio = median(SalePrice, na.rm = TRUE), .by = grupo_cut) |>
  arrange(grupo_cut)
mediana_precio_grupo

#Ejercicio 2
titanic <- read_csv("johan_steven_avilan_penaloza/data/titanic.csv", show_col_types = FALSE)

pct_outliers <- titanic |>
  summarise(k_1.5 = mean(detectar_outliers_iqr(fare, 1.5), na.rm = TRUE) * 100,
            k_3   = mean(detectar_outliers_iqr(fare, 3),   na.rm = TRUE) * 100)
pct_outliers

zscore_robusto <- function(x) (x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)

titanic_out <- titanic |>
  mutate(out_global = detectar_outliers_iqr(fare)) |>
  mutate(out_clase = detectar_outliers_iqr(fare), .by = pclass) |>
  mutate(out_zrob = abs(zscore_robusto(fare)) > 3.5) |>
  mutate(across(c(out_global, out_clase, out_zrob), ~ coalesce(.x, FALSE)))

outliers_por_clase <- titanic_out |>
  summarise(global = sum(out_global), por_clase = sum(out_clase),
            z_robusto = sum(out_zrob), .by = pclass) |>
  arrange(pclass)
outliers_por_clase

primera_clase <- titanic_out |>
  filter(pclass == 1) |>
  summarise(dejan_de_ser_outliers = sum(out_global & !out_clase))
primera_clase

tabla_criterios <- count(titanic_out, out_global, out_clase, out_zrob)
tabla_criterios

discordantes <- titanic_out |>
  filter((out_global + out_clase + out_zrob) %in% 1:2) |>
  select(name, pclass, fare, out_global, out_clase, out_zrob) |>
  arrange(desc(fare))
discordantes

tarifa_cero <- titanic_out |>
  filter(fare == 0) |>
  select(name, pclass, age, embarked, out_global, out_clase, out_zrob)
tarifa_cero

titanic_fare <- titanic |>
  mutate(fare_cero = coalesce(fare == 0, FALSE),
         fare = na_if(fare, 0)) |>
  mutate(fare = if_else(is.na(fare), median(fare, na.rm = TRUE), fare),
         .by = pclass)

outliers_log <- tibble(
  escala     = c("fare", "log1p(fare)"),
  n_outliers = c(sum(detectar_outliers_iqr(titanic$fare), na.rm = TRUE),
                 sum(detectar_outliers_iqr(log1p(titanic$fare)), na.rm = TRUE))
)
outliers_log

#Ejercicio 3
titanic_t <- titanic |>
  mutate(
    titulo = str_extract(name, "(?<=, )[A-Za-z]+(?=\\.)"),
    titulo_grupo = case_when(
      titulo == "Mr"                      ~ "Mr",
      titulo %in% c("Mrs", "Mme")         ~ "Mrs",
      titulo %in% c("Miss", "Ms", "Mlle") ~ "Miss",
      titulo == "Master"                  ~ "Master",
      .default = "Otro"
    )
  )

conocidos <- titanic_t |> filter(!is.na(age))
stopifnot(nrow(conocidos) == 1046)

simular_imputacion <- function(semilla) {
  set.seed(semilla)
  ocultos <- sample(nrow(conocidos), size = round(nrow(conocidos) * 0.20))
  d <- conocidos |>
    mutate(age_real = age,
           oculta   = row_number() %in% ocultos,
           age_obs  = if_else(oculta, NA_real_, age_real))
  med_global <- median(d$age_obs, na.rm = TRUE)
  d |>
    mutate(med_cs  = median(age_obs, na.rm = TRUE), .by = c(pclass, sex)) |>
    mutate(med_tit = median(age_obs, na.rm = TRUE), .by = titulo_grupo) |>
    mutate(imp_global     = if_else(oculta, med_global, age_obs),
           imp_clase_sexo = if_else(oculta, coalesce(med_cs, med_global), age_obs),
           imp_titulo     = if_else(oculta, coalesce(med_tit, med_global), age_obs)) |>
    pivot_longer(starts_with("imp_"), names_to = "estrategia",
                 values_to = "imputada") |>
    summarise(MAE  = mean(abs(imputada - age_real)[oculta]),
              RMSE = sqrt(mean((imputada - age_real)[oculta]^2)),
              sd_imputada = sd(imputada),
              sd_real     = sd(age_real),
              .by = estrategia)
}

resultado_2024 <- simular_imputacion(2024)
resultado_2024

resultados_200 <- map_df(1:200, ~ simular_imputacion(.x) |> mutate(semilla = .x))

grafico_mae <- resultados_200 |>
  ggplot(aes(x = estrategia, y = MAE, fill = estrategia)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title = "MAE por estrategia en 200 repeticiones", x = NULL, y = "MAE") +
  theme_minimal(base_size = 13)
grafico_mae

victorias <- resultados_200 |>
  slice_min(MAE, n = 1, by = semilla) |>
  count(estrategia) |>
  mutate(pct = round(n / sum(n) * 100, 1))
victorias

comparacion_sd <- resultados_200 |>
  summarise(across(c(sd_imputada, sd_real), mean), .by = estrategia)
comparacion_sd

#Ejercicio 4
moviles  <- read_csv("johan_steven_avilan_penaloza/data/mobile_data.csv", show_col_types = FALSE)
cols_num <- c("ram", "battery_power", "int_memory", "px_height", "px_width")

escalar <- function(df, cols, metodo = c("minmax", "zscore", "robusto")) {
  metodo <- match.arg(metodo)
  f <- switch(
    metodo,
    minmax  = function(x) (x - min(x, na.rm = TRUE)) /
                          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    zscore  = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE),
    robusto = function(x) (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
  )
  df |> select(all_of(cols)) |> mutate(across(everything(), f))
}

aporte_distancia <- function(df) {
  d2 <- (unlist(df[1, ]) - unlist(df[2, ]))^2
  as_tibble_row(round(d2 / sum(d2) * 100, 1))
}

versiones <- list(
  original = moviles |> select(all_of(cols_num)),
  minmax   = escalar(moviles, cols_num, "minmax"),
  zscore   = escalar(moviles, cols_num, "zscore"),
  robusto  = escalar(moviles, cols_num, "robusto")
)

aportes <- map_df(versiones, aporte_distancia, .id = "escala")
aportes

moviles_out <- moviles
moviles_out$ram[1] <- 100000

sd_ram <- map_df(
  set_names(c("minmax", "zscore", "robusto")),
  ~ tibble(sin_outlier = sd(escalar(moviles, "ram", .x)$ram[-1]),
           con_outlier = sd(escalar(moviles_out, "ram", .x)$ram[-1])),
  .id = "metodo"
)
sd_ram

set.seed(2024)
idx_train <- sample(nrow(moviles), size = floor(0.7 * nrow(moviles)))
train <- moviles[idx_train, ]
test  <- moviles[-idx_train, ]

minimos <- map_dbl(train[cols_num], min)
maximos <- map_dbl(train[cols_num], max)

test_esc <- test |>
  select(all_of(cols_num)) |>
  mutate(across(everything(),
                ~ (.x - minimos[[cur_column()]]) /
                  (maximos[[cur_column()]] - minimos[[cur_column()]])))

fuera_rango <- test_esc |>
  summarise(across(everything(), ~ sum(.x < 0 | .x > 1)))
fuera_rango

pct_fuera <- sum(fuera_rango) / (nrow(test) * length(cols_num)) * 100
pct_fuera

todo_junto <- escalar(moviles, cols_num, "minmax")
stopifnot(all(todo_junto >= 0 & todo_junto <= 1))

#Ejercicio 5
adult <- read_csv("johan_steven_avilan_penaloza/data/adult.csv", show_col_types = FALSE)

limpiar_signos <- function(df) {
  df |> mutate(across(where(is.character), ~ na_if(str_trim(.x), "?")))
}

adult_limpio <- limpiar_signos(adult)

comp_occupation <- adult_limpio |>
  mutate(occupation_falta = is.na(occupation)) |>
  summarise(n = n(), n_alto = sum(income == ">50K"),
            prop_alto = n_alto / n, .by = occupation_falta)
comp_occupation

test_prop <- prop.test(comp_occupation$n_alto, comp_occupation$n)
test_prop

centinela <- adult_limpio |>
  count(capital.gain, sort = TRUE) |>
  arrange(desc(capital.gain)) |>
  slice_head(n = 3)
centinela

pct_cero <- mean(adult_limpio$capital.gain == 0) * 100
pct_cero

cols_onehot <- c("workclass", "marital.status", "sex", "native.country")
cols_cont   <- c("age", "fnlwgt", "education.num", "capital.gain",
                 "capital.loss", "hours.per.week")

tratar_centinela <- function(df) {
  df |>
    mutate(ganancia_tope = as.integer(capital.gain == 99999),
           capital.gain  = na_if(capital.gain, 99999))
}

agrupar_paises <- function(df) {
  df |>
    mutate(native.country = case_when(
      is.na(native.country)             ~ NA_character_,
      native.country == "United-States" ~ "Estados Unidos",
      .default = "Otro"
    ))
}

imputar <- function(df, p) {
  df |>
    mutate(across(all_of(names(p$medianas)),
                  ~ if_else(is.na(.x), p$medianas[[cur_column()]], .x)),
           across(all_of(names(p$modas)),
                  ~ if_else(is.na(.x), p$modas[[cur_column()]], .x)))
}

transformar_log <- function(df) {
  df |> mutate(across(c(capital.gain, capital.loss), log1p))
}

codificar <- function(df, niveles) {
  dummies <- imap(niveles, function(niv, col) {
    niv |>
      set_names(paste0(col, "_", niv)) |>
      map(~ as.integer(df[[col]] == .x)) |>
      as_tibble()
  })
  df |>
    mutate(income = as.integer(income == ">50K")) |>
    select(where(is.numeric)) |>
    bind_cols(dummies)
}

estandarizar <- function(df, p) {
  df |>
    mutate(across(all_of(cols_cont),
                  ~ (.x - p$medias[[cur_column()]]) / p$sds[[cur_column()]]))
}

ajustar_adult <- function(train) {
  d <- train |> limpiar_signos() |> tratar_centinela() |> agrupar_paises()
  p <- list(
    medianas = d |> select(where(is.numeric)) |> map(~ median(.x, na.rm = TRUE)),
    modas    = d |> select(where(is.character), -income) |> map(moda)
  )
  d <- d |> imputar(p) |> transformar_log()
  p$niveles <- d |> select(all_of(cols_onehot)) |> map(~ sort(unique(.x)))
  d <- codificar(d, p$niveles)
  p$medias <- d |> select(all_of(cols_cont)) |> map(mean)
  p$sds    <- d |> select(all_of(cols_cont)) |> map(sd)
  p
}

preprocesar_adult <- function(df, p) {
  pasos <- list(
    limpiar_signos,
    tratar_centinela,
    agrupar_paises,
    \(d) imputar(d, p),
    transformar_log,
    \(d) codificar(d, p$niveles),
    \(d) estandarizar(d, p)
  )
  res <- reduce(pasos, \(acc, f) f(acc), .init = df)
  stopifnot(!anyNA(res),
            all(map_lgl(res, is.numeric)),
            nrow(res) == nrow(df))
  res
}

set.seed(2024)
idx_adult   <- sample(nrow(adult), size = floor(0.7 * nrow(adult)))
adult_train <- adult[idx_adult, ]
adult_test  <- adult[-idx_adult, ]

params_adult <- ajustar_adult(adult_train)
train_listo  <- preprocesar_adult(adult_train, params_adult)
test_listo   <- preprocesar_adult(adult_test,  params_adult)

stopifnot(identical(names(train_listo), names(test_listo)))

resumen_train <- train_listo |>
  summarise(across(all_of(cols_cont), list(media = mean, sd = sd)))
resumen_train

resumen_test <- test_listo |>
  summarise(across(all_of(cols_cont), list(media = mean, sd = sd)))
resumen_test

#Ejercicio 6
netflix <- read_csv("johan_steven_avilan_penaloza/data/netflix_titles.csv", show_col_types = FALSE)

netflix <- netflix |>
  mutate(mal_ubicado = coalesce(is.na(duration) & str_detect(rating, "\\d+ min"), FALSE),
         duration    = if_else(mal_ubicado, rating, duration),
         rating      = if_else(mal_ubicado, NA_character_, rating))

filas_corregidas <- netflix |> filter(mal_ubicado) |> select(title, duration, rating)
filas_corregidas

netflix <- netflix |>
  mutate(duracion_valor  = as.numeric(str_extract(duration, "\\d+")),
         duracion_unidad = case_when(str_detect(duration, "min")    ~ "min",
                                     str_detect(duration, "Season") ~ "temporadas",
                                     .default = NA_character_))

duracion_promedio <- netflix |>
  summarise(promedio = mean(duracion_valor, na.rm = TRUE),
            .by = c(type, duracion_unidad))
duracion_promedio

netflix <- netflix |>
  mutate(fecha_agregada = parse_date(str_trim(date_added), "%B %d, %Y",
                                     locale = locale("en")),
         rezago = year(fecha_agregada) - release_year)

resumen_rezago <- summary(netflix$rezago)
resumen_rezago

rezagos_negativos <- netflix |>
  filter(rezago < 0) |>
  select(title, release_year, fecha_agregada, rezago)
rezagos_negativos

generos <- netflix |>
  separate_longer_delim(listed_in, delim = ",") |>
  mutate(listed_in = str_trim(listed_in))

filas_antes_despues <- c(antes = nrow(netflix), despues = nrow(generos))
filas_antes_despues

top10_generos <- generos |>
  count(type, listed_in, name = "titulos") |>
  slice_max(titulos, n = 10, by = type, with_ties = FALSE) |>
  arrange(type, desc(titulos))
top10_generos

netflix <- netflix |>
  mutate(n_paises     = if_else(is.na(country), NA_integer_,
                                str_count(country, ",") + 1L),
         coproduccion = n_paises > 1)

paises <- count(netflix, n_paises)
paises
