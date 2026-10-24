# =====================================================================
# Minería de Datos - Clase 12: Inferencia y estructura en tablas de contingencia
# Ejercicio de práctica: evaluación docente presencial vs. virtual
# ---------------------------------------------------------------------
# La diapositiva no trae la tabla de datos. Se usan DATOS ILUSTRATIVOS
# coherentes con el enunciado: 200 docentes, cada uno evaluado una vez en
# cada modalidad (200 x 2 = 400 evaluaciones).
# Si tiene la tabla real, reemplace la matriz `pareada` y vuelva a correr.
# =====================================================================

niveles <- c("Excelente", "Bueno", "Regular")

# Tabla cuadrada: cada celda cuenta DOCENTES (calificación presencial x virtual)
pareada <- matrix(c(50, 22,  6,
                    10, 58, 16,
                     2,  7, 29),
                  nrow = 3, byrow = TRUE,
                  dimnames = list(Presencial = niveles, Virtual = niveles))
addmargins(pareada)


# ---------------------------------------------------------------------
# Tarea 1. Esquema de muestreo
# ---------------------------------------------------------------------
# Vista "evaluaciones" (tabla 2x3 de 400 evaluaciones): los totales de fila
# (200 presencial, 200 virtual) están fijos por diseño -> producto-multinomial
# en apariencia, PERO las dos filas son los mismos docentes: no son muestras
# independientes, así que el supuesto del producto-multinomial no se cumple.
# Vista "docente" (tabla 3x3 de 200 docentes): n = 200 fijo y cada docente cae
# en una de 9 celdas -> multinomial, datos pareados en tabla cuadrada.
evaluaciones <- rbind(Presencial = rowSums(pareada),
                      Virtual    = colSums(pareada))
names(dimnames(evaluaciones)) <- c("Modalidad", "Calificacion")
addmargins(evaluaciones)


# ---------------------------------------------------------------------
# Tarea 2. ¿Difiere la distribución de calificaciones entre modalidades?
# Chi-cuadrado de homogeneidad sobre la tabla 2x3 (trata las 400
# evaluaciones como independientes; ver Tarea 3).
# ---------------------------------------------------------------------
homog <- chisq.test(evaluaciones, correct = FALSE)
homog
round(prop.table(evaluaciones, margin = 1), 3)
round(homog$expected, 1)
round(homog$stdres, 2)

# Razón de verosimilitudes G2 (mismos gl)
G2 <- 2 * sum(evaluaciones * log(evaluaciones / homog$expected))
c(G2 = round(G2, 3), p_valor = round(pchisq(G2, df = 2, lower.tail = FALSE), 4))


# ---------------------------------------------------------------------
# Tarea 3. Docente como unidad de análisis -> tabla cuadrada pareada
# Prueba apropiada: Stuart-Maxwell (homogeneidad marginal), gl = I - 1
# ---------------------------------------------------------------------
stuart_maxwell <- function(tab) {
  I  <- nrow(tab)
  fi <- rowSums(tab); ci <- colSums(tab)
  d  <- (fi - ci)[-I]                       # se descarta la última categoría
  V  <- -(tab + t(tab))                     # V_ij = -(n_ij + n_ji)
  diag(V) <- fi + ci - 2 * diag(tab)        # V_ii = n_i+ + n_+i - 2 n_ii
  V  <- V[-I, -I]
  X2 <- as.numeric(t(d) %*% solve(V) %*% d)
  list(d = d, V = V, X2 = round(X2, 3), gl = I - 1,
       p_valor = round(pchisq(X2, df = I - 1, lower.tail = FALSE), 4))
}
stuart_maxwell(pareada)
# Equivalente con paquete:  DescTools::StuartMaxwellTest(pareada)

# Complemento: simetría de Bowker (generalización de McNemar), gl = I(I-1)/2
mcnemar.test(pareada)

# Complemento: cuasi-simetría y homogeneidad marginal condicional
df_p <- as.data.frame(as.table(pareada))
i <- as.integer(df_p$Presencial); j <- as.integer(df_p$Virtual)
df_p$Par <- factor(paste(pmin(i, j), pmax(i, j)))
m_qs <- glm(Freq ~ Presencial + Virtual + Par, family = poisson, data = df_p)
m_s  <- glm(Freq ~ Par,                        family = poisson, data = df_p)
c(G2_QS = round(deviance(m_qs), 3), gl = df.residual(m_qs),
  p = round(pchisq(deviance(m_qs), df.residual(m_qs), lower.tail = FALSE), 4))
anova(m_s, m_qs, test = "Chisq")   # HM dado QS: G2(S) - G2(QS), gl = I - 1

# OJO: chisq.test(pareada) responde otra pregunta (¿se asocian la nota
# presencial y la virtual del mismo docente?), no si las distribuciones difieren.
chisq.test(pareada)


# ---------------------------------------------------------------------
# Tarea 4. Kappa con calificación dicotomizada
# Aprueba = Excelente o Bueno ; No aprueba = Regular
# ---------------------------------------------------------------------
ap <- c("Aprueba", "No aprueba")
tab2 <- matrix(c(sum(pareada[1:2, 1:2]), sum(pareada[1:2, 3]),
                 sum(pareada[3, 1:2]),   pareada[3, 3]),
               nrow = 2, byrow = TRUE,
               dimnames = list(Presencial = ap, Virtual = ap))
addmargins(tab2)

n     <- sum(tab2)
Po    <- sum(diag(tab2)) / n
Pe    <- sum(rowSums(tab2) * colSums(tab2)) / n^2
kappa <- (Po - Pe) / (1 - Pe)
se_k  <- sqrt(Po * (1 - Po) / (n * (1 - Pe)^2))      # EE asintótico (Cohen, 1960)
ic    <- kappa + c(-1, 1) * qnorm(0.975) * se_k
cat("Po:", round(Po, 3), " Pe:", round(Pe, 3), " Kappa:", round(kappa, 3),
    " IC95%: [", round(ic[1], 3), ",", round(ic[2], 3), "]\n")

# Aproximación de la diapositiva: SE ~ sqrt(Pe / (n (1 - Pe)^2)) (más conservadora)
se_clase <- sqrt(Pe / (n * (1 - Pe)^2))
cat("SE (fórmula de clase):", round(se_clase, 3), " z:", round(kappa / se_clase, 2), "\n")

# Kappa mide acuerdo; McNemar mide si hay cambio sistemático en la aprobación
round(prop.table(rowSums(tab2)), 3)   # aprobación presencial
round(prop.table(colSums(tab2)), 3)   # aprobación virtual
mcnemar.test(tab2, correct = FALSE)
