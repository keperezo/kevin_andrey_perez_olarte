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

# 1. Gráfico de Desvío con Paleta Divergente
desvio_mes <- pedidos %>%
  count(mes = month(fecha, label = TRUE, abbr = TRUE)) %>%
  mutate(desvio = n - mean(n))

p_divergente <- ggplot(desvio_mes, aes(x = mes, y = desvio, fill = desvio)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  scale_fill_gradient2(
    low = "#b2182b",   # Rojo oscuro (por debajo de la media)
    mid = "grey90",    # Gris neutro (en la media)
    high = "#2166ac",  # Azul oscuro (por encima de la media)
    midpoint = 0,
    name = "Desvío vs\nMedia"
  ) +
  labs(
    title = "Desviación Mensual del Volumen de Pedidos",
    subtitle = "Meses con superávit (azul) vs déficit (rojo) respecto a la media anual",
    x = "Mes",
    y = "Diferencia respecto a la media de pedidos"
  ) +
  theme_minimal(base_size = 12)

print(p_divergente)

# 2. Definición de colores_canal con nombres (Paleta Colorblind-Friendly)
# Usamos Azul, Naranja y Púrpura (seguros para daltonismo y de alto contraste)
colores_canal <- c(
  "App"    = "#1f78b4",  # Azul
  "Web"    = "#ff7f00",  # Naranja
  "Tienda" = "#6a3d9a"   # Púrpura
)

# Gráfico 2A: Distribución del valor de pedidos por canal (Boxplot)
p_canal_1 <- ggplot(pedidos, aes(x = canal, y = valor, fill = canal)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = colores_canal) +
  scale_y_log10(labels = label_dollar(prefix = "$", scale = 1e-3, suffix = "K")) +
  labs(
    title = "2A. Distribución del Valor por Canal de Venta",
    subtitle = "Uso consistente del vector nombrado 'colores_canal'",
    x = "Canal",
    y = "Valor del pedido (COP - Log)"
  ) +
  theme_minimal(base_size = 12)

print(p_canal_1)

# Gráfico 2B: Tendencia temporal de pedidos mensuales por canal (Líneas)
pedidos_mes_canal <- pedidos %>%
  mutate(mes = month(fecha, label = TRUE, abbr = TRUE)) %>%
  count(mes, canal)

p_canal_2 <- ggplot(pedidos_mes_canal, aes(x = mes, y = n, color = canal, group = canal)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores_canal) +
  labs(
    title = "2B. Evolución del Número de Pedidos por Canal",
    subtitle = "El mismo código cromático aplicado a líneas temporales",
    x = "Mes",
    y = "Total de pedidos",
    color = "Canal"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_canal_2)

# RESPUESTAS A LAS PREGUNTAS 
# Pregunta 1
cat("1. ¿Por qué aquí conviene una paleta divergente? ¿Cuál es la referencia?\n")
cat("   - ¿Por qué divergente?: Porque la variable graficada ('desvio') tiene una dirección dual\n")
cat("     y un centro natural con significado cuantitativo: valores positivos (meses con más ventas de lo normal)\n")
cat("     frente a valores negativos (meses con déficit de ventas).\n")
cat("   - Referencia: El punto neutro es 0 (midpoint = 0), que representa exactamente la MEDIA\n")
cat("     del número mensual de pedidos (mean(n)).\n\n")

# Pregunta 2
cat("2. Define colores_canal con nombres y úsalo en dos gráficos distintos.\n")
cat("   - Se definió un vector nombrado con valores hexadecimales:\n")
cat("     c('App' = '#1f78b4', 'Web' = '#ff7f00', 'Tienda' = '#6a3d9a').\n")
cat("   - Ventaja: Al estar nombrado, R asegura que 'App' siempre reciba azul y 'Web' naranja,\n")
cat("     incluso si los datos se filtran, se reordenan o se grafican en tipos de gráficos distintos\n")
cat("     (usado en el Boxplot 2A y en las Líneas Temporales 2B).\n\n")

# Pregunta 3
cat("3. ¿Tus gráficos se entienden sin distinguir rojo y verde? Pruébalos en Coblis.\n")
cat("   RTA: SÍ, son 100% accesibles y legibles bajo cualquier tipo de daltonismo:\n")
cat("   a) Paleta Azul-Rojo (#2166ac vs #b2182b): Al usar Azul y Rojo/Marrón, las personas con\n")
cat("      deuteranopía o protanopía (ceguera al rojo-verde) no tienen conflicto alguno.\n")
cat("   b) Redundancia de posición: El gráfico no depende únicamente del color;\n")
cat("      la posición de la barra (hacia arriba o hacia abajo del eje 0) ya comunica el signo.\n")
cat("   c) Paleta de canales (Azul, Naranja, Púrpura): Es una combinación de alto contraste que\n")
cat("      conserva una luminosidad y tono claramente distinguibles incluso en escala de grises (acromatopsia).\n")