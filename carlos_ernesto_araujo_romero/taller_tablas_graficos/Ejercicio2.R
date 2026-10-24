library(tidyverse)
library(lubridate)
library(scales)

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

# 1. ¿Qué canal genera más pedidos?
# Tipo de variables: 1 Categórica nominal (canal) -> Conteo de observaciones.
# Gráfico elegido: Gráfico de barras (geom_col / geom_bar).
# ------------------------------------------------------------------------------
# Justificación: Un gráfico de barras es el más adecuado para comparar frecuencias 
# entre categorías discretas porque el ojo humano juzga longitudes alineadas con máxima precisión.

pedidos %>%
  count(canal) %>%
  ggplot(aes(x = reorder(canal, -n), y = n, fill = canal)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(n)), vjust = -0.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "1. Cantidad de Pedidos por Canal de Venta",
    subtitle = "La App lidera ampliamente el volumen de transacciones",
    x = "Canal",
    y = "Número de pedidos"
  ) +
  theme_minimal(base_size = 12)

# 2. ¿Cómo se distribuyen los días de entrega?
# Tipo de variables: 1 Numérica discreta/continua (días).
# Gráfico elegido: Histograma / Diagrama de barras discretas.
# ------------------------------------------------------------------------------
# Justificación: Un histograma es el gráfico por excelencia para examinar la forma, 
# el centro, la dispersión y la asimetría de una variable cuantitativa individual.

pedidos %>%
  ggplot(aes(x = dias_entrega)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = min(pedidos$dias_entrega):max(pedidos$dias_entrega)) +
  labs(
    title = "2. Distribución de los Días de Entrega",
    subtitle = "Distribución tipo Poisson centrada alrededor de 3-4 días de entrega",
    x = "Días de entrega",
    y = "Frecuencia de pedidos"
  ) +
  theme_minimal(base_size = 12)

# 3. ¿El valor del pedido cambia según la categoría?
# Tipo de variables: 1 Categórica (categoría) vs 1 Numérica continua (valor).
# Gráfico elegido: Diagrama de cajas (Boxplot).
# ------------------------------------------------------------------------------
# Justificación: Un boxplot permite comparar de forma compacta y simultánea la mediana, 
# el rango intercuartílico y los valores atípicos entre múltiples grupos.

pedidos %>%
  ggplot(aes(x = categoria, y = valor, fill = categoria)) +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  scale_y_log10(labels = label_dollar(prefix = "$", scale = 1e-3, suffix = "K")) +
  labs(
    title = "3. Valor del Pedido según Categoría (Escala Log)",
    subtitle = "Tecnología presenta una mediana y dispersión sustancialmente superiores",
    x = "Categoría",
    y = "Valor del pedido (COP - Escala Logarítmica)"
  ) +
  theme_minimal(base_size = 12)

# 4. ¿Cómo evolucionaron las ventas mensuales por canal?
# Tipo de variables: 1 Temporal (mes) vs 1 Numérica (ventas) segmentada por 1 Categórica (canal).
# Gráfico elegido: Gráfico de líneas temporales (geom_line).
# ------------------------------------------------------------------------------
# Justificación: Un gráfico de líneas es el estándar para representar series de tiempo, 
# facilitando la identificación de tendencias continuas y estacionalidades por canal.

ventas_mes <- pedidos %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes, canal) %>%
  summarise(ventas = sum(valor), .groups = "drop")

ventas_mes %>%
  ggplot(aes(x = mes, y = ventas, color = canal, group = canal)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = label_dollar(prefix = "$", scale = 1e-6, suffix = "M")) +
  labs(
    title = "4. Evolución de Ventas Mensuales por Canal (2025)",
    subtitle = "Comparativa mensual de ingresos brutos entre App, Web y Tienda",
    x = "Mes",
    y = "Ventas totales (Millones COP)",
    color = "Canal"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
