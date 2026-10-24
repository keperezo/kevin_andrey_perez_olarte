library(tidyverse)

options(scipen = 999)

set.seed(909); n <- 1500
dias <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), "day")

pedidos <- tibble(
  fecha        = sample(dias, n, replace = TRUE),
  canal        = sample(c("App", "Web", "Tienda"), n, TRUE, prob = c(.45, .35, .20)),
  categoria    = sample(c("Tecnología", "Hogar", "Moda"), n, replace = TRUE),
  valor        = round(rlnorm(n, 11.5 + 0.6 * (categoria == "Tecnología"), 0.6), -2),
  dias_entrega = rpois(n, lambda = 3) + 1,
  calificacion = sample(1:5, n, TRUE, prob = c(1, 2, 3, 7, 7))
)

# Ejercicio 1: Tablas de frecuencia y análisis

# 1. Distribución de frecuencia de 'calificacion'
tabla_calif <- pedidos %>%
  count(calificacion) %>%
  mutate(
    pct = round(100 * n / sum(n), 2),
    acum = round(cumsum(pct), 2)
  )

cat("--- TABLA 1: Distribución de Frecuencia de Calificación ---\n")
print(tabla_calif)

# 2. Distribución de frecuencia de 'valor' (cortes propuestos)
tabla_valor <- pedidos %>%
  mutate(rango = cut(
    valor, 
    breaks = c(0, 100000, 200000, 400000, Inf), 
    labels = c("< $100K", "$100K - $200K", "$200K - $400K", "> $400K"),
    right = FALSE
  )) %>%
  count(rango) %>%
  mutate(pct = round(100 * n / sum(n), 2))

cat("\n--- TABLA 2: Distribución de Valor (Cortes de la Diapositiva) ---\n")
print(tabla_valor)

# Tabla de cortes de igual ancho

# 1. Calculamos los 5 cortes de igual amplitud
cortes <- seq(min(pedidos$valor), max(pedidos$valor), length.out = 6)

# 2. Creamos etiquetas claras con formato de moneda y separador de miles
etiquetas_moneda <- paste0(
  "$", format(round(head(cortes, -1)), big.mark = ","),
  " a $", format(round(tail(cortes, -1)), big.mark = ",")
)

# 3. Construimos la tabla de frecuencia completa
tabla_igual_ancho <- pedidos %>%
  mutate(rango_valor = cut(
    valor, 
    breaks = cortes, 
    include.lowest = TRUE, 
    labels = etiquetas_moneda
  )) %>%
  count(rango_valor, name = "pedidos") %>%
  mutate(
    pct      = round(100 * pedidos / sum(pedidos), 2),
    acum_n   = cumsum(pedidos),
    acum_pct = round(cumsum(pct), 2)
  )

print(tabla_igual_ancho)


# Pregunta 1
pct_4_mas <- tabla_calif %>% 
  filter(calificacion >= 4) %>% 
  summarise(total_pct = sum(pct)) %>% 
  pull(total_pct)

cat(sprintf("1. ¿Qué porcentaje de pedidos tiene calificación de 4 o más?\n"))
cat(sprintf("   RTA: El %.2f%% de los pedidos (34.00%% con 4 estrellas + 35.73%% con 5 estrellas).\n\n", pct_4_mas))

# Pregunta 2
cat("2. ¿Por qué la acumulada tiene sentido para 'calificacion' y no para 'canal'?\n")
cat("   RTA: 'calificacion' es una variable cualitativa ORDINAL (1 < 2 < 3 < 4 < 5),\n")
cat("   por lo que acumular responde a umbrales con significado lógico (ej. 'clientes con a lo sumo 3 estrellas').\n")
cat("   En contraste, 'canal' (App, Web, Tienda) es NOMINAL; no hay jerarquía natural entre ellos,\n")
cat("   por lo que acumular 'App + Web' carece de interpretación matemática.\n\n")

# Pregunta 3
n_vacios <- sum(tail(tabla_igual_ancho$n, 2))
pct_vacios <- sum(tail(tabla_igual_ancho$pct, 2))

cat("3. Con cortes de igual ancho, ¿qué intervalo queda casi vacío y por qué?\n")
cat(sprintf("   RTA: Los intervalos 4 y 5 quedan casi vacíos (apenas %d pedidos de 1500, el %.2f%% del total).\n", n_vacios, pct_vacios))
cat("   Esto se debe a que la variable 'valor' proviene de una distribución log-normal (asimétrica positiva).\n")
cat("   El 87.7% de los datos se agrupa en el primer tramo (< $268,000), pero unos pocos valores atípicos muy altos\n")
cat("   (superiores a $1,000,000) estiran el ancho de los bins, dejando los últimos dos casi desiertos.\n")
