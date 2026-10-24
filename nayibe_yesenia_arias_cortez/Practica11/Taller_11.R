# ============================================================
# Ejercicio de práctica - Clase 12
# Inferencia en tablas de contingencia
# Evaluación docente: presencial vs virtual
# ============================================================
# 400 evaluaciones = 200 docentes evaluados en ambas modalidades.
# Datos ilustrativos (el enunciado no trae la tabla).
# Si tienes datos reales, cambia solo la matriz 'pares'.


# ---------- Datos ----------
# Tabla cuadrada 3x3: cada celda cuenta DOCENTES
# Filas = calificación presencial, columnas = calificación virtual
pares <- matrix(
  c(50, 20,  8,
    10, 55, 17,
    3,  9, 28),
  nrow = 3, byrow = TRUE,
  dimnames = list(
    Presencial = c("Excelente", "Bueno", "Regular"),
    Virtual    = c("Excelente", "Bueno", "Regular")
  )
)
addmargins(pares)

# Vista "ingenua": 400 evaluaciones como si fueran independientes (2x3)
ingenua <- rbind(Presencial = rowSums(pares),
                 Virtual    = colSums(pares))
addmargins(ingenua)
round(prop.table(ingenua, 1), 3)


# ============================================================
# TAREA 1: Esquema de muestreo
# ============================================================
# - La tabla 2x3 parece producto-multinomial (200 por modalidad fijos).
# - Pero el mismo docente está en ambas filas -> NO son muestras
#   independientes. La unidad es el docente (200 pares).
# - Esquema correcto: MULTINOMIAL con n = 200 sobre la tabla 3x3.


# ============================================================
# TAREA 2: ¿Difiere la distribución de calificaciones?
# ============================================================

# --- 2a. INCORRECTO (solo para comparar): Chi-cuadrado sobre 2x3 ---
chisq.test(ingenua, correct = FALSE)
# p ≈ 0.18 -> "no hay diferencia" (conclusión errónea: ignora el pareo)

# --- 2b. CORRECTO: Stuart-Maxwell (homogeneidad marginal) ---
fil <- rowSums(pares)
col <- colSums(pares)
d   <- (fil - col)[1:2]          # se usan I - 1 = 2 categorías

V <- matrix(0, 2, 2)
for (i in 1:2) {
  for (j in 1:2) {
    if (i == j) {
      V[i, j] <- fil[i] + col[i] - 2 * pares[i, i]
    } else {
      V[i, j] <- -(pares[i, j] + pares[j, i])
    }
  }
}

X2_SM <- as.numeric(t(d) %*% solve(V) %*% d)
gl_SM <- nrow(pares) - 1
p_SM  <- pchisq(X2_SM, df = gl_SM, lower.tail = FALSE)

cat("d =", d, "\n")
cat("Stuart-Maxwell: X2 =", round(X2_SM, 3),
    " gl =", gl_SM, " p =", round(p_SM, 4), "\n")
# X2 ≈ 7.85, gl = 2, p ≈ 0.020 -> SÍ hay diferencia entre modalidades

# Con el paquete del curso (mismo resultado):
# install.packages("DescTools")
library(DescTools)
StuartMaxwellTest(pares)

# --- 2c. Complemento: simetría de Bowker ---
# mcnemar.test() sobre una tabla IxI calcula la prueba de Bowker
mcnemar.test(pares, correct = FALSE)
# X2 ≈ 8.07, gl = 3, p ≈ 0.045 -> los cambios no son simétricos


# ============================================================
# TAREA 3: Docente como unidad de análisis
# ============================================================
# Tabla cuadrada 3x3 de datos pareados:
# - Homogeneidad marginal: Stuart-Maxwell (gl = I - 1 = 2)
# - Simetría: Bowker (gl = I(I-1)/2 = 3)
# - Si se dicotomiza: McNemar (gl = 1)
# NO usar Chi-cuadrado de independencia/homogeneidad.


# ============================================================
# TAREA 4: Kappa con Aprueba / No aprueba
# ============================================================
# Aprueba = Excelente o Bueno ; No aprueba = Regular
grupo <- c(Excelente = "Aprueba", Bueno = "Aprueba", Regular = "No aprueba")

binaria <- matrix(0, 2, 2,
                  dimnames = list(Presencial = c("Aprueba", "No aprueba"),
                                  Virtual    = c("Aprueba", "No aprueba")))
for (i in rownames(pares)) {
  for (j in colnames(pares)) {
    binaria[grupo[i], grupo[j]] <- binaria[grupo[i], grupo[j]] + pares[i, j]
  }
}
addmargins(binaria)

n     <- sum(binaria)
Po    <- sum(diag(binaria)) / n
Pe    <- sum(rowSums(binaria) * colSums(binaria)) / n^2
kappa <- (Po - Pe) / (1 - Pe)
se    <- sqrt(Pe / (n * (1 - Pe)^2))
ic    <- kappa + c(-1, 1) * qnorm(0.975) * se

cat("Po =", round(Po, 3), " Pe =", round(Pe, 3),
    " Kappa =", round(kappa, 3), "\n")
cat("IC 95%: [", round(ic[1], 3), ",", round(ic[2], 3), "]\n")
# Kappa ≈ 0.485 -> concordancia MODERADA (Landis y Koch)

# Con paquete (opcional):
CohenKappa(binaria, conf.level = 0.95)

# Complemento: ¿cambia la tasa de aprobación? -> McNemar
mcnemar.test(binaria, correct = FALSE)
# X2 ≈ 4.57, p ≈ 0.033 -> menos docentes aprueban en virtual (73.5% vs 80%)

