# Cargar librerías necesarias
install.packages(c("irr", "coin")) # Ejecutar si no las tienes instaladas
library(irr)
library(coin)

# Simulación de la estructura de datos (400 docentes evaluados en ambas modalidades)
set.seed(123) # Para reproducibilidad

evaluaciones <- data.frame(
  docente_id = 1:400,
  presencial = factor(sample(c("Excelente", "Bueno", "Regular"), 400, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
                      levels = c("Regular", "Bueno", "Excelente"), ordered = TRUE),
  virtual = factor(sample(c("Excelente", "Bueno", "Regular"), 400, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
                   levels = c("Regular", "Bueno", "Excelente"), ordered = TRUE)
)

# Mostrar los primeros registros
head(evaluaciones)


# Crear la tabla de contingencia pareada
tabla_modalidades <- table(evaluaciones$presencial, evaluaciones$virtual)
print(tabla_modalidades)

# Prueba de McNemar-Bowker / Homogeneidad marginal
mcnemar.test(tabla_modalidades)

# Convertir niveles ordinales a valores numéricos (Regular = 1, Bueno = 2, Excelente = 3)
presencial_num <- as.numeric(evaluaciones$presencial)
virtual_num    <- as.numeric(evaluaciones$virtual)

# Prueba de Rangos con Signo de Wilcoxon para muestras pareadas
wilcox.test(presencial_num, virtual_num, paired = TRUE)

# Recodificación a variable binaria
evaluaciones$presencial_bin <- ifelse(evaluaciones$presencial %in% c("Excelente", "Bueno"), "Aprueba", "No aprueba")
evaluaciones$virtual_bin    <- ifelse(evaluaciones$virtual %in% c("Excelente", "Bueno"), "Aprueba", "No aprueba")

# Matriz 2x2 para Kappa
datos_kappa <- evaluaciones[, c("presencial_bin", "virtual_bin")]

# Cálculo del coeficiente Kappa de Cohen
kappa_resultado <- kappa2(datos_kappa, weight = "unweighted")
print(kappa_resultado)