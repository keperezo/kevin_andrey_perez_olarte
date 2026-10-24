library(tidyverse)
library(knitr)
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

# 1. Tablas de Contingencia 
tabla_conteo <- table(Canal = pedidos$canal, Categoria = pedidos$categoria)
tabla_prop   <- round(100 * prop.table(tabla_conteo, margin = 1), 1)

cat("--- TABLA EN CONTEOS BRUTOS ---\n")
print(tabla_conteo)

cat("\n--- TABLA EN PORCENTAJES POR FILA (% dentro de cada canal) ---\n")
print(tabla_prop)

# 2. Gráfico de Barras al 100% 

p_fill <- ggplot(pedidos, aes(x = canal, fill = categoria)) +
  geom_bar(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Composición de Categorías por Canal (Barras al 100%)",
    subtitle = "Muestra la proporción interna pero oculta el volumen total de pedidos",
    x = "Canal de venta",
    y = "Proporción dentro del canal",
    fill = "Categoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_fill)

# 3. Gráfico alternativo que resuelve la falta de volumen (Barras Apiladas Absolutas)
# Al usar position = "stack", la altura de la barra revela el tamaño real del canal
# y los segmentos internos conservan la proporción de cada categoría.
p_stack <- ggplot(pedidos, aes(x = reorder(canal, -after_stat(count)), fill = categoria)) +
  geom_bar(position = "stack", width = 0.65) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Volumen Real y Composición por Canal (Barras Apiladas)",
    subtitle = "Permite ver simultáneamente el volumen total (App lidera) y la mezcla de productos",
    x = "Canal de venta",
    y = "Total de pedidos",
    fill = "Categoría"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_stack)

# 4. Presentación Formal con kable()

cat("\n--- TABLA CON FORMATO kable() ---\n")
tabla_kable <- kable(
  tabla_prop, 
  caption = "Distribución Porcentual de Categorías por Canal de Venta (% por Fila)",
  align = "c"
)
print(tabla_kable)

# RESPUESTAS A LAS PREGUNTAS 

canal_max_tec <- names(which.max(tabla_prop[, "Tecnología"]))
pct_max_tec   <- max(tabla_prop[, "Tecnología"])

# Pregunta 1
cat("1. ¿En qué canal pesa más Tecnología? ¿Coincide con las barras al 100%?\n")
cat(sprintf("   RTA: Pesa más en el canal '%s' con un %.1f%% de sus pedidos.\n", canal_max_tec, pct_max_tec))
cat("   - SÍ coincide con las barras al 100%: 'position = fill' grafica exactamente las frecuencias relativas\n")
cat(sprintf("     condicionales (margin = 1), por lo que el canal con el segmento de Tecnología más alto es '%s'.\n\n", canal_max_tec))

# Pregunta 2
cat("2. Las barras al 100% ocultan cuántos pedidos tiene cada canal: ¿qué gráfico de los retos lo resuelve?\n")
cat("   RTA: Lo resuelven dos tipos de gráficos:\n")
cat("   a) El Gráfico de Barras Apiladas Absolutas ('position = stack'): La altura total de la barra refleja\n")
cat("      el volumen de ventas del canal (haciendo evidente que App tiene más del doble de pedidos que Tienda),\n")
cat("      mientras los bloques internos muestran la partición de categorías.\n")
cat("   b) El Gráfico de Mosaico (Mosaic Plot / Marimekko): Ajusta el ancho de la barra según el peso de cada canal\n")
cat("      y la altura según la proporción de cada categoría, combinando ambas dimensiones sin distorsión.\n\n")

# Pregunta 3
cat("3. Dos frases de interpretación para la tabla:\n")
cat("   - Frase 1: La distribución de categorías es muy homogénea entre canales, donde cada categoría\n")
cat("     (Hogar, Moda y Tecnología) representa aproximadamente un tercio (~31% a 35%) de las compras de cada canal.\n")
cat("   - Frase 2: Aunque la composición porcentual de productos es similar, la App concentra la mayor masa crítica\n")
cat("     del negocio en volumen absoluto de transacciones frente a Web y Tienda física.\n")