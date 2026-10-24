# Paso 0: genera saber11.csv (datos simulados, solo R base)
set.seed(1111); n <- 1500
dp <- c("Bogotá", "Antioquia", "Valle del Cauca", "Boyacá", "Chocó")
departamento <- sample(dp, n, TRUE, c(.30, .25, .20, .15, .10))
p_rural <- c(.05, .25, .25, .40, .50)[match(departamento, dp)]
zona <- ifelse(runif(n) < p_rural, "Rural", "Urbana")
estrato <- ifelse(zona == "Rural", sample(1:3, n, TRUE, c(.6, .3, .1)),
                  sample(1:6, n, TRUE, c(.15, .30, .28, .15, .07, .05)))
naturaleza <- ifelse(runif(n) < plogis(-3.2 + .75 * estrato),
                     "Privado", "Oficial")
horas_estudio <- round(pmax(0, rnorm(n, 5 + .5 * estrato, 2.5)), 1)
puntaje <- round(200 + 14 * estrato + 12 * (naturaleza == "Privado") +
                 4 * horas_estudio - 15 * (zona == "Rural") -
                 20 * (departamento == "Chocó") + rnorm(n, 0, 30))
horas_estudio[sample(n, 60)] <- NA  # encuesta sin responder
write.csv(data.frame(departamento, zona, estrato, naturaleza,
                     horas_estudio, puntaje), "saber11.csv", row.names = FALSE)
