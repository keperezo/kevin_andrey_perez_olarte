# =============================================================================
# Minería de Datos - Clase 8: Valores faltantes
# Solución de los ejercicios de práctica
# =============================================================================
# Paquetes: base R (Ejercicio 1) + tidymodels e ipred (Ejercicio 2)
# install.packages(c("tidymodels", "ipred"))
# Opcional: install.packages(c("naniar", "mice"))
# =============================================================================


# =============================================================================
# EJERCICIO 1: Diagnóstico de valores faltantes
# =============================================================================

set.seed(2024); n <- 400
ciudad  <- sample(c("Bogotá","Medellín","Cali","Barranquilla"),
                  n, TRUE, c(.35,.25,.25,.15))
edad    <- pmax(18, round(rnorm(n, 38, 12)))
ingreso <- rnorm(n, 2.5e6 + 4e4*edad, 6e5)
gasto   <- ingreso * runif(n, 0.3, 0.7)
compra  <- rbinom(n, 1, plogis(scale(gasto)[,1]*0.8 - 0.5))
riesgo  <- ciudad == "Cali" & edad > 42

# Se guardan los valores reales ANTES de enmascarar (solo para medir sesgo en 1b y 1c).
# No altera el generador aleatorio: la secuencia de números es idéntica.
ingreso_real <- ingreso
gasto_real   <- gasto

ingreso[rbinom(n, 1, ifelse(riesgo, .55, .06)) == 1] <- NA
gasto[rbinom(n, 1, plogis(scale(gasto)[,1]*1.5 - 1)) == 1] <- NA
datos <- data.frame(ciudad, edad, ingreso, gasto, compra)

# -----------------------------------------------------------------------------
# Diagnóstico general
# -----------------------------------------------------------------------------
colSums(is.na(datos))
round(colMeans(is.na(datos)) * 100, 1)
sum(!complete.cases(datos))

# -----------------------------------------------------------------------------
# 1a. ¿La ausencia de ingreso es MAR y no MCAR? ¿De qué depende?
# -----------------------------------------------------------------------------
# Si fuera MCAR, la tasa de NA en ingreso sería la misma en todos los subgrupos
# definidos por variables observadas. Lo contrastamos contra ciudad y edad.

datos$na_ingreso <- as.integer(is.na(datos$ingreso))

# (i) Tasa de faltantes por ciudad + prueba chi-cuadrado
tab_ciudad <- table(datos$ciudad, datos$na_ingreso)
round(prop.table(tab_ciudad, 1) * 100, 1)
chisq.test(tab_ciudad)

# (ii) Edad de quienes tienen y no tienen ingreso
aggregate(edad ~ na_ingreso, data = datos, FUN = mean)
t.test(edad ~ na_ingreso, data = datos)

# (iii) Cruce ciudad x grupo de edad: aquí aparece la estructura
datos$grupo_edad <- cut(datos$edad, breaks = c(17, 30, 42, 55, Inf),
                        labels = c("18-30", "31-42", "43-55", "56+"))
tasa_cruce <- tapply(datos$na_ingreso, list(datos$ciudad, datos$grupo_edad), mean)
round(tasa_cruce * 100, 1)
table(datos$ciudad, datos$grupo_edad)     # tamaños de celda

# (iv) Modelo logístico de la indicadora de ausencia
mod_na <- glm(na_ingreso ~ ciudad * edad, data = datos, family = binomial)
summary(mod_na)
# Contraste global: ¿ciudad y edad explican la ausencia? (vs. modelo nulo)
anova(glm(na_ingreso ~ 1, data = datos, family = binomial), mod_na, test = "Chisq")

# (v) Opcional: prueba de Little para MCAR
if (requireNamespace("naniar", quietly = TRUE)) {
  print(naniar::mcar_test(datos[, c("edad", "ingreso", "gasto", "compra")]))
}

# CONCLUSIÓN 1a:
# - La tasa de NA de ingreso NO es homogénea: es mucho mayor en Cali que en las
#   demás ciudades (chi-cuadrado significativo), y quienes no reportan ingreso
#   son en promedio mayores.
# - El cruce ciudad x edad muestra que el efecto se concentra en Cali a partir
#   de los ~43 años (tasas cercanas al 50%), mientras que en el resto de celdas
#   la tasa es baja (~5-10%). El término de interacción del glm lo confirma.
# - Como la probabilidad de faltar depende de variables OBSERVADAS (ciudad y
#   edad, en interacción), se rechaza MCAR y el patrón es consistente con MAR.
# - Nota: con datos observados nunca se puede "probar" MAR frente a MNAR; lo que
#   sí se puede mostrar es que la ausencia está explicada por ciudad y edad.

# -----------------------------------------------------------------------------
# 1b. Media global vs. media condicional (ciudad y edad). Sesgo en el subgrupo
#     con más faltantes.
# -----------------------------------------------------------------------------
# Subgrupo con más faltantes según la tabla anterior: Cali, 43 años o más
sub <- datos$ciudad == "Cali" & datos$edad >= 43
falta <- is.na(datos$ingreso)
cat("NA en el subgrupo:", sum(sub & falta), "de", sum(sub),
    sprintf("(%.1f%%)\n", 100 * mean(falta[sub])))

# (A) Imputación con la media global
imp_global <- datos$ingreso
imp_global[falta] <- mean(datos$ingreso, na.rm = TRUE)

# (B) Media condicional por regresión: ingreso ~ ciudad + edad
#     (lm se ajusta solo con las filas con ingreso observado)
mod_ing <- lm(ingreso ~ ciudad + edad, data = datos)
summary(mod_ing)
imp_reg <- datos$ingreso
imp_reg[falta] <- predict(mod_ing, newdata = datos[falta, ])

# (C) Variante: media por celda ciudad x grupo de edad (más ruidosa: celdas pequeñas)
media_celda <- ave(datos$ingreso, datos$ciudad, datos$grupo_edad,
                   FUN = function(x) mean(x, na.rm = TRUE))
imp_celda <- datos$ingreso
imp_celda[falta] <- media_celda[falta]

# Métricas contra la verdad (conocida porque tenemos la semilla).
# Como solo hay ~18 imputados en el subgrupo, el sesgo contra el valor real
# mezcla sesgo sistemático + ruido individual (sd = 6e5). Por eso también se
# mide contra la esperanza real E[ingreso | edad] = 2.5e6 + 4e4 * edad.
esperanza <- 2.5e6 + 4e4 * datos$edad
idx <- sub & falta

evaluar <- function(imp, nombre) {
  data.frame(
    metodo            = nombre,
    sesgo_vs_real     = mean(imp[idx] - ingreso_real[idx]),
    sesgo_vs_esperanz = mean(imp[idx] - esperanza[idx]),   # sesgo sistemático
    rmse_imputados    = sqrt(mean((imp[idx] - ingreso_real[idx])^2)),
    sesgo_media_subgr = mean(imp[sub]) - mean(ingreso_real[sub]),
    sd_subgr_imp      = sd(imp[sub]),
    sd_subgr_real     = sd(ingreso_real[sub])
  )
}

res_1b <- rbind(
  evaluar(imp_global, "Media global"),
  evaluar(imp_reg,    "Regresión ciudad + edad"),
  evaluar(imp_celda,  "Media ciudad x grupo edad")
)
res_1b[, -1] <- round(res_1b[, -1])
print(res_1b)
cat("Ruido de esta muestra (real - esperanza) en los imputados:",
    round(mean(ingreso_real[idx] - esperanza[idx])), "\n")
cat("Media del subgrupo con casos completos:",
    round(mean(datos$ingreso[sub], na.rm = TRUE)),
    "| media real del subgrupo:", round(mean(ingreso_real[sub])), "\n")

# Monte Carlo: se repite el mecanismo de simulación 500 veces para ver el
# sesgo ESPERADO de cada método (elimina la suerte de una sola muestra)
sim_sesgo <- function(semilla, n = 400) {
  set.seed(semilla)
  ci <- sample(c("Bogotá","Medellín","Cali","Barranquilla"), n, TRUE, c(.35,.25,.25,.15))
  ed <- pmax(18, round(rnorm(n, 38, 12)))
  ir <- rnorm(n, 2.5e6 + 4e4*ed, 6e5)
  ing <- ir
  ing[rbinom(n, 1, ifelse(ci == "Cali" & ed > 42, .55, .06)) == 1] <- NA
  f <- is.na(ing); s <- ci == "Cali" & ed >= 43; i <- f & s
  d <- data.frame(ci, ed, ing)
  p <- predict(lm(ing ~ ci + ed, data = d), newdata = d[i, ])
  c(global = mean(mean(ing, na.rm = TRUE) - ir[i]),
    regresion = mean(p - ir[i]))
}
mc <- t(sapply(1:500, sim_sesgo))
round(rbind(sesgo_medio = colMeans(mc),
            error_estandar = apply(mc, 2, sd) / sqrt(nrow(mc))))

# CONCLUSIÓN 1b:
# - En el subgrupo Cali, 43+ años falta ~50% del ingreso. Como ingreso crece
#   con la edad (≈ 40.000 por año) y este subgrupo es de mayor edad, la media
#   global imputa un valor demasiado bajo: sesgo sistemático ≈ -540.000 frente
#   a la esperanza real (≈ -12%), y en Monte Carlo el sesgo esperado es
#   igualmente negativo y grande. Además aplana la varianza del subgrupo.
# - La media condicional (regresión ciudad + edad) usa las variables de las que
#   depende la ausencia (MAR): su sesgo sistemático es cercano a 0 y en Monte
#   Carlo su sesgo esperado es ≈ 0. En esta muestra particular el sesgo contra
#   el valor real aparece positivo solo porque los 18 individuos que faltaron
#   quedaron, por azar, ~200.000 por debajo de su esperanza (ruido).
# - La media por celda es insesgada en teoría, pero con celdas de 3-15 casos
#   observados es muy ruidosa: la regresión aprovecha mejor la información.
# - Bajo MAR, condicionar en las variables que explican la ausencia elimina el
#   sesgo; aun así, la imputación determinística subestima la varianza, por lo
#   que para inferencia se prefiere imputación múltiple (mice).


# Opcional: imputación múltiple con mice (pmm) para comparar
if (requireNamespace("mice", quietly = TRUE)) {
  d_mice <- datos[, c("ciudad", "edad", "ingreso", "gasto", "compra")]
  d_mice$ciudad <- factor(d_mice$ciudad)
  imp_m <- mice::mice(d_mice, m = 5, method = "pmm", seed = 123, printFlag = FALSE)
  medias_sub <- sapply(1:5, function(k) mean(mice::complete(imp_m, k)$ingreso[sub]))
  cat("mice - media del subgrupo (promedio 5 imputaciones):", round(mean(medias_sub)),
      "| sesgo:", round(mean(medias_sub) - mean(ingreso_real[sub])), "\n")
}

# -----------------------------------------------------------------------------
# 1c. gasto es MNAR: falta más cuando su valor real es alto
# -----------------------------------------------------------------------------
falta_g <- is.na(datos$gasto)

# Evidencia (con la verdad conocida): los gastos que faltan son más altos
c(media_gasto_observado = mean(gasto_real[!falta_g]),
  media_gasto_faltante  = mean(gasto_real[falta_g]))

# Evidencia observable indirecta: la ausencia de gasto se asocia con ingreso y
# con compra (ambas correlacionadas con el gasto real)
summary(glm(is.na(gasto) ~ ingreso + edad + ciudad + compra,
            data = datos, family = binomial))

# Imputación condicional "buena" (regresión con ingreso, edad, ciudad, compra)
mod_g <- lm(gasto ~ ingreso + edad + ciudad + compra, data = datos)
idx_g <- falta_g & !is.na(datos$ingreso)
pred_g <- predict(mod_g, newdata = datos[idx_g, ])
c(sesgo_imputacion_regresion = mean(pred_g - gasto_real[idx_g]),
  sesgo_media_global         = mean(mean(datos$gasto, na.rm = TRUE) - gasto_real[falta_g]))

# Análisis de sensibilidad tipo "delta adjustment" (patrón-mezcla):
# se supone que los faltantes están un delta% por encima de lo que predice
# el modelo MAR, y se revisa cuánto cambian las conclusiones.
deltas <- c(0, 0.05, 0.10, 0.20, 0.30)
sens <- sapply(deltas, function(d) {
  g <- datos$gasto
  g[idx_g] <- pred_g * (1 + d)
  c(media_gasto = mean(g, na.rm = TRUE),
    coef_compra_x_millon = unname(coef(glm(compra ~ g, family = binomial))[2]) * 1e6)
})
colnames(sens) <- paste0("delta_", deltas * 100, "%")
round(sens, 3)
cat("Media real de gasto (verdad):", round(mean(gasto_real)), "\n")

# CONCLUSIÓN 1c:
# - En MNAR la probabilidad de faltar depende del propio valor no observado.
#   Todo método de imputación (media, regresión, KNN, bosques, mice) aprende la
#   relación gasto ~ X con los casos OBSERVADOS y asume que
#   P(gasto | X, falta) = P(gasto | X, observado). En MNAR eso es falso: los
#   faltantes son sistemáticamente más altos incluso condicionando en X, y los
#   datos no contienen información para corregir ese desplazamiento (no es
#   identificable). En esta muestra: el gasto real de los faltantes promedia
#   ≈ 2,50 M vs ≈ 1,78 M de los observados; la imputación por regresión (aun
#   usando ingreso, que está muy correlacionado) queda con sesgo ≈ -490.000 y
#   la media global ≈ -730.000. El sesgo se reduce, pero no desaparece.
# - El análisis de sensibilidad muestra que habría que suponer un +25-30% sobre
#   la predicción MAR para recuperar la media real, y que el coeficiente de
#   gasto sobre compra cambia según ese supuesto: la conclusión depende de un
#   supuesto que los datos no pueden verificar.
# - Qué haría en su lugar:
#   1) Crear un indicador de ausencia (gasto_NA) y usarlo como predictor: el
#      hecho de faltar es informativo (señala gasto alto y más probabilidad de
#      compra).
#   2) Modelar explícitamente el mecanismo: análisis de sensibilidad
#      (delta-adjustment / pattern-mixture, p. ej. mice con post-procesamiento)
#      o modelos de selección tipo Heckman, y reportar el rango de resultados.
#   3) Recuperar información externa o de negocio: fuentes auxiliares (p. ej.
#      transacciones, extractos), variables proxy fuertemente asociadas, o
#      re-contactar una submuestra de no respondientes para estimar el sesgo.
#   4) Documentar el supuesto y no presentar el valor imputado como "verdad".


# =============================================================================
# EJERCICIO 2: Depurar un pipeline con recipes
# =============================================================================
library(tidymodels)

datos_modelo <- data.frame(ciudad, edad, ingreso, gasto, compra)
datos_modelo$ciudad <- factor(datos_modelo$ciudad)
datos_modelo$compra <- factor(datos_modelo$compra, levels = c(1, 0),
                              labels = c("si", "no"))   # clasificación binaria

set.seed(7)
split <- initial_split(datos_modelo, prop = 0.8, strata = compra)
train <- training(split)
test  <- testing(split)

# -----------------------------------------------------------------------------
# ERRORES DEL PIPELINE ORIGINAL
# -----------------------------------------------------------------------------
# Error 1 (fuga de información): recipe(..., data = bind_rows(train, test)).
#   Con prep() las medianas y modas se estiman usando también el test, así que
#   el conjunto de prueba "contamina" el preprocesamiento y la evaluación queda
#   optimista. El recipe debe estimarse SOLO con train.
#
# Error 2 (orden de los pasos): step_dummy() va antes de las imputaciones.
#   - step_dummy convierte ciudad en columnas numéricas 0/1; si ciudad tuviera
#     NA, las dummies quedan en NA (o se pierde la fila en la codificación) y
#     step_impute_mode(all_nominal_predictors()) ya no encuentra ninguna
#     columna nominal: la imputación de categóricas nunca ocurre.
#   - Además step_impute_median(all_numeric_predictors()) terminaría
#     imputando las dummies con la mediana (0 o 1), lo cual no es una
#     imputación coherente de la categoría.
#   Orden correcto: primero imputar, luego codificar (dummies).

# -----------------------------------------------------------------------------
# PIPELINE CORREGIDO
# -----------------------------------------------------------------------------
receta <- recipe(compra ~ ., data = train) |>          # solo train
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>              # dummies al final
  prep()

train_proc <- bake(receta, new_data = NULL)
test_proc  <- bake(receta, new_data = test)

colSums(is.na(train_proc))   # todo en 0
colSums(is.na(test_proc))    # todo en 0
tidy(receta, number = 1)     # medianas estimadas solo con train

# -----------------------------------------------------------------------------
# PIPELINE EXTENDIDO: indicador de ausencia + step_impute_bag para ingreso
# -----------------------------------------------------------------------------
receta_ext <- recipe(compra ~ ., data = train) |>
  # 1. Indicadores de ausencia: se crean ANTES de imputar, porque después ya no
  #    queda ningún NA y la columna saldría toda en 0. Se agrega también para
  #    gasto porque su ausencia es informativa (MNAR).
  step_indicate_na(ingreso, gasto) |>
  # 2. Imputación de ingreso con bagged trees, usando las variables de las que
  #    depende la ausencia (ciudad, edad) y gasto (muy correlacionado).
  #    Se hace antes de las dummies para que ciudad entre como factor, y se
  #    excluyen los indicadores na_ind_* (en train son constantes = 0 entre los
  #    casos observados, no aportan nada al modelo imputador).
  step_impute_bag(ingreso,
                  impute_with = imp_vars(ciudad, edad, gasto),
                  trees = 50, seed_val = 7) |>
  # 3. El resto de numéricas (gasto) con mediana y nominales con moda
  step_impute_median(gasto) |>
  step_impute_mode(all_nominal_predictors()) |>
  # 4. Codificación y escalado al final, cuando ya no hay NA
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors(), -starts_with("na_ind"),
                 -starts_with("ciudad_"))

prep_ext <- prep(receta_ext)
train_ext <- bake(prep_ext, new_data = NULL)
test_ext  <- bake(prep_ext, new_data = test)
glimpse(train_ext)
colSums(is.na(test_ext))
table(train_ext$na_ind_ingreso)

# Modelo en un workflow (el recipe se re-estima dentro del fit, solo con train)
wf_base <- workflow() |> add_recipe(recipe(compra ~ ., data = train) |>
                                      step_impute_median(all_numeric_predictors()) |>
                                      step_impute_mode(all_nominal_predictors()) |>
                                      step_dummy(all_nominal_predictors())) |>
  add_model(logistic_reg())
wf_ext  <- workflow() |> add_recipe(receta_ext) |> add_model(logistic_reg())

fit_base <- fit(wf_base, data = train)
fit_ext  <- fit(wf_ext,  data = train)

evaluar_wf <- function(fit_obj, nombre) {
  pred <- augment(fit_obj, new_data = test)
  bind_rows(
    roc_auc(pred, truth = compra, .pred_si),
    accuracy(pred, truth = compra, .pred_class)
  ) |> mutate(modelo = nombre)
}
bind_rows(evaluar_wf(fit_base, "Corregido (mediana/moda)"),
          evaluar_wf(fit_ext,  "Extendido (indicadores + bag)"))

tidy(fit_ext)   # revisar el coeficiente de na_ind_gasto (ausencia informativa)

# JUSTIFICACIÓN DEL ORDEN FINAL:
# indicate_na -> impute_bag(ingreso) -> impute_median(gasto) -> impute_mode
#   -> dummy -> normalize
# - indicate_na primero: necesita ver los NA originales.
# - impute_bag antes de dummy: el árbol usa ciudad como factor y no ve dummies
#   con NA. Se imputa ingreso antes que gasto para que el bagging use el gasto
#   observado (los árboles de ipred/rpart toleran NA en predictores) y no un
#   gasto ya "aplanado" por la mediana.
# - dummy y normalize al final: operan sobre datos completos; normalizar antes
#   de imputar haría que las medias/desviaciones se calculen con huecos.
# - Todo se estima con train dentro del workflow: no hay fuga hacia el test.


# =============================================================================
# EJERCICIO 3: Razonamiento conceptual (crédito, 5.000 solicitudes)
# =============================================================================
# 3a. ¿MAR o MCAR? ¿Por qué no se descarta MNAR?
# - No es MCAR: la ausencia de ingreso_reportado se asocia fuertemente con una
#   variable observada (estrato, r ≈ 0.52) y con el resultado (incumplimiento
#   31% entre faltantes vs 9% entre observados). Bajo MCAR los faltantes serían
#   una submuestra aleatoria y ambas tasas serían similares.
# - La evidencia es consistente con MAR: la ausencia se explica por estrato
#   (y no por edad, r ≈ 0.03).
# - No se puede descartar MNAR porque:
#   * MAR vs MNAR no es verificable con datos observados: nunca vemos el
#     ingreso de quien no lo reporta.
#   * Estrato es un proxy imperfecto del ingreso; una r de 0.52 deja mucha
#     variación sin explicar. Es plausible que, dentro de un mismo estrato,
#     quien tiene ingresos más bajos o informales (o inestables) sea quien no lo
#     reporta, y eso también explicaría el mayor incumplimiento.
#   * La brecha 31% vs 9% puede persistir aun condicionando por estrato: si
#     persiste, la ausencia está ligada a algo no observado (probablemente el
#     propio ingreso).
#
# 3b. Imputar con la mediana global: consecuencia práctica
# - Los solicitantes sin ingreso, que se concentran en estratos bajos y tienen
#   más riesgo, reciben el ingreso "típico" de toda la cartera: parecen más
#   solventes de lo que son. El modelo subestima su riesgo y tenderá a
#   APROBAR créditos a perfiles con mayor probabilidad de incumplir (más
#   pérdida esperada y provisiones mal calculadas).
# - A la vez, se aplana la relación ingreso-riesgo (muchos valores idénticos
#   en la mediana), el coeficiente del ingreso se atenúa y el modelo
#   discrimina peor entre clientes con ingreso real conocido.
# - Se pierde la señal más fuerte disponible: "no reportó ingreso" tiene 31% de
#   incumplimiento. Sin indicador, esa información se borra.
# - Efecto distributivo: el error no es aleatorio sino concentrado por estrato,
#   por lo que las decisiones erradas (aprobaciones riesgosas o, si se corrige
#   con reglas ad hoc, rechazos masivos) recaen sobre un grupo socioeconómico
#   específico: problema de equidad, de cumplimiento regulatorio y de
#   explicabilidad ante el comité.
#
# 3c. Estrategia defendible en 3 pasos (sin eliminar el 22%)
# 1) Diagnóstico documentado del mecanismo: modelar P(falta ingreso) con
#    estrato, edad, canal, tipo de empleo, etc.; revisar si la brecha de
#    incumplimiento persiste DENTRO de cada estrato (indicio de MNAR). Todo
#    sobre el conjunto de entrenamiento.
# 2) Imputación condicional dentro del pipeline + indicador de ausencia:
#    imputación múltiple (mice/pmm) o bagged trees condicionada en estrato y
#    demás covariables, ajustada solo con train (recipes/workflows), y una
#    variable ingreso_faltante como predictor. Validar AUC, calibración y
#    tasas de aprobación/incumplimiento por estrato y por grupo faltante.
# 3) Sensibilidad y gobierno: escenarios MNAR (desplazar el ingreso imputado
#    -10%, -20%, -30%) y medir cuánto cambian las aprobaciones y la pérdida
#    esperada; si las decisiones son sensibles, aplicar una política de
#    mitigación (verificación del ingreso con fuentes externas como centrales
#    de riesgo o aportes a seguridad social, o límites/cupos más conservadores
#    para el grupo sin ingreso). Documentar supuestos y monitorear en producción.
# =============================================================================