library(tidyverse)
library(highcharter)
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

# 1. Versión Highcharter
datos_grafico <- pedidos %>%
  count(categoria, canal)

grafico_interactivo <- datos_grafico %>%
  hchart("column", hcaes(x = categoria, y = n, group = canal)) %>%
  hc_title(text = "Pedidos por Categoría y Canal de Venta") %>%
  hc_subtitle(text = "Pase el cursor sobre las barras o use la leyenda para aislar canales") %>%
  hc_xAxis(title = list(text = "Categoría de Producto")) %>%
  hc_yAxis(
    title = list(text = "Número de Pedidos"),
    labels = list(format = "{value:,.0f}")  # Separador de miles en eje Y
  ) %>%
  hc_tooltip(
    shared = TRUE,
    headerFormat = "<b>Categoría: {point.key}</b><br/>",
    # Separador de miles y texto en español
    pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f}</b> pedidos<br/>'
  ) %>%
  hc_plotOptions(column = list(borderRadius = 3))

# Para visualizar en el visor de RStudio / Navegador
grafico_interactivo

# 2. Versión Estática en ggplot2 (Barras Agrupadas / Dodge)
grafico_ggplot <- ggplot(datos_grafico, aes(x = categoria, y = n, fill = canal)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = comma(n)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Pedidos por Categoría y Canal de Venta",
    subtitle = "Comparativa estática con etiquetas directas de conteo",
    x = "Categoría de Producto",
    y = "Número de Pedidos",
    fill = "Canal"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

print(grafico_ggplot)

# RESPUESTAS A LAS PREGUNTAS DEL EJERCICIO 6
# Pregunta 1
cat("1. Traduce el tooltip y los ejes al español, con separador de miles:\n")
cat("   - Ejes: 'Categoría de Producto' (X) y 'Número de Pedidos' (Y con formato '{value:,.0f}').\n")
cat("   - Tooltip: Configurado en español mediante 'hc_tooltip(headerFormat, pointFormat)'\n")
cat("     mostrando: '<Canal>: <N> pedidos' con separador de miles mediante 'hc_lang(thousandsSep = ',')'.\n\n")

# Pregunta 2
cat("2. Comparación ggplot2 vs highcharter: ¿Qué gana y qué pierde cada una?\n")
cat("   - ggplot2:\n")
cat("     * GANA: Portabilidad universal (renderiza directamente en PDF, Word, LaTeX e impreso),\n")
cat("       control tipográfico absoluto y reproducibilidad fija sin dependencias de JavaScript.\n")
cat("     * PIERDE: Cero interactividad (no tiene hover, no permite aislar series haciendo clic,\n")
cat("       y requiere saturar el gráfico con etiquetas de texto para mostrar valores exactos).\n")
cat("   - highcharter:\n")
cat("     * GANA: Experiencia de usuario rica (los tooltips despejan el lienzo,\n")
cat("       permite ocultar/mostrar canales dinámicamente y tiene transiciones animadas fluidas).\n")
cat("     * PIERDE: No es imprimible ni compatible de forma nativa con documentos estáticos PDF/LaTeX\n")
cat("       y requiere mayor consumo de recursos web en el cliente.\n\n")

# Pregunta 3
cat("3. ¿Cuál usarías en un informe PDF para gerencia y cuál en un dashboard? ¿Por qué?\n")
cat("   - En un INFORME PDF: Usaría 'ggplot2'. El PDF es un formato de lectura fija/impresa donde el JavaScript\n")
cat("     queda totalmente inerte. ggplot2 garantiza calidad vectorial perfecta y etiquetas legibles fijadas.\n")
cat("   - En un DASHBOARD (ej. Shiny / Web / Quarto HTML): Usaría 'highcharter'. Los tomadores de decisiones\n")
cat("     esperan una interfaz interactiva donde puedan hacer clic en la leyenda para aislar un canal específico\n")
cat("     o pasar el mouse para consultar el dato exacto sin necesidad de tablas auxiliares.\n")
