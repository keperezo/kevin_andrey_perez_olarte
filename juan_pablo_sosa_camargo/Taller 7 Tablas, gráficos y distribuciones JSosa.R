# ============================================================
# Análisis exploratorio de pedidos 2025 (datos simulados)
# ============================================================

library(tidyverse)   # dplyr, ggplot2, tibble, forcats, lubridate...
library(knitr)
library(highcharter)

theme_set(theme_minimal(base_size = 12))

# Formato colombiano: punto para miles, coma para decimales
fmt_miles <- scales::label_number(big.mark = ".", decimal.mark = ",")

# Paletas fijas (una por variable) para que el color siempre signifique lo mismo
colores_canal <- c(App = "#0072B2", Web = "#E69F00", Tienda = "#009E73")
colores_cat   <- c(Tecnología = "#CC79A7", Hogar = "#56B4E9", Moda = "#F0E442")

# ------------------------------------------------------------
# Datos
# ------------------------------------------------------------
set.seed(909)
n <- 1500

pedidos <- tibble(
  fecha        = sample(seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
                        n, replace = TRUE),
  canal        = factor(sample(names(colores_canal), n, replace = TRUE,
                               prob = c(.45, .35, .20)),
                        levels = names(colores_canal)),
  categoria    = sample(c("Tecnología", "Hogar", "Moda"), n, replace = TRUE),
  valor        = round(rlnorm(n, 11.5 + 0.6 * (categoria == "Tecnología"), 0.6), -2),
  dias_entrega = rpois(n, lambda = 3) + 1,
  calificacion = sample(1:5, n, replace = TRUE, prob = c(1, 2, 3, 7, 7))
)

# Ventas mensuales por canal (millones de COP); se usa en 2.4 y en el Ejercicio 4
ventas_mes <- pedidos |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes, canal) |>
  summarise(ventas = sum(valor) / 1e6, .groups = "drop")


# ============================================================
# Ejercicio 1: tablas de frecuencia
# ============================================================

# 1.1 Porcentaje de pedidos con calificación 4 o 5
pedidos |>
  summarise(pct_4_o_5 = round(100 * mean(calificacion >= 4), 1))

# 1.2 Distribución de calificaciones (frecuencia, % y % acumulado)
pedidos |>
  count(calificacion) |>
  mutate(pct  = 100 * n / sum(n),
         acum = cumsum(pct),
         across(c(pct, acum), \(x) round(x, 1)))

# 1.3 Pedidos por rango de valor
# El último intervalo llega hasta Inf: con un tope de 5e5 los pedidos más
# caros quedaban como NA y desaparecían de la tabla.
cortes    <- c(0, 1e5, 2e5, 3e5, 4e5, 5e5, Inf)
etiquetas <- c("< 100 mil", "100–200 mil", "200–300 mil",
               "300–400 mil", "400–500 mil", "≥ 500 mil")

pedidos |>
  mutate(rango = cut(valor, cortes, labels = etiquetas, right = FALSE)) |>
  count(rango, .drop = FALSE) |>
  mutate(pct = round(100 * n / sum(n), 1))


# ============================================================
# Ejercicio 2: elige el gráfico según la pregunta
# ============================================================

# 2.1 ¿Qué canal genera más pedidos?
# Barras: comparar cantidades entre categorías; se ordenan de mayor a menor.
pedidos |>
  count(canal) |>
  ggplot(aes(x = fct_reorder(canal, n, .desc = TRUE), y = n)) +
  geom_col(fill = "#006DAE") +
  geom_text(aes(label = fmt_miles(n)), vjust = -0.4) +
  labs(x = "Canal", y = "Número de pedidos")

# 2.2 ¿Cómo se distribuyen los días de entrega?
# dias_entrega es discreta (enteros), así que cada valor tiene su propia barra.
# Un histograma con bins = 10 deja huecos y barras de ancho arbitrario.
ggplot(pedidos, aes(x = dias_entrega)) +
  geom_bar(fill = "#006DAE") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(x = "Días de entrega", y = "Número de pedidos")

# 2.3 ¿El valor del pedido cambia según la categoría?
# Caja: compara la distribución (mediana, dispersión, atípicos) entre grupos.
# El valor es muy asimétrico, así que se usa escala logarítmica.
ggplot(pedidos, aes(x = fct_reorder(categoria, valor, .fun = median),
                    y = valor)) +
  geom_boxplot(fill = "#9ecae1") +
  scale_y_log10(labels = fmt_miles) +
  labs(x = "Categoría", y = "Valor del pedido (COP, escala log)")

# 2.4 ¿Cómo evolucionaron las ventas mensuales por canal?
# Líneas: evolución en el tiempo, una línea por canal.
ggplot(ventas_mes, aes(x = mes, y = ventas, color = canal)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = colores_canal) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Mes", y = "Ventas (millones de COP)", color = "Canal")


# ============================================================
# Ejercicio 3: la forma del valor del pedido
# ============================================================

# Gráfico base reutilizable
hist_valor <- function(bins, log = FALSE) {
  p <- ggplot(pedidos, aes(x = valor / 1000)) +
    geom_histogram(bins = bins, fill = "#006DAE", color = "white") +
    labs(x = "Valor del pedido (miles COP)", y = "Pedidos")
  if (log) p <- p + scale_x_log10(labels = fmt_miles)
  p
}

# 1. Forma, centro y cola
hist_valor(30) +
  geom_vline(xintercept = median(pedidos$valor) / 1000, linewidth = 0.8) +
  geom_vline(xintercept = mean(pedidos$valor)   / 1000, linewidth = 0.8,
             linetype = "dashed", color = "#D55E00")

pedidos |>
  summarise(media = mean(valor), mediana = median(valor),
            media_sobre_mediana = round(media / mediana, 2))
# Interpretación: la media (línea discontinua) es mayor que la mediana, y
# la cola se extiende a la derecha: distribución asimétrica positiva.
# Unos pocos pedidos muy caros "jalan" la media hacia arriba, por lo que la
# mediana describe mejor el pedido típico.

# 2. Número de bins
hist_valor(8)     # muy pocos: se pierde la forma
hist_valor(120)   # demasiados: ruido, picos espurios
# 30 es un punto intermedio razonable para n = 1.500.

# 3. Escala logarítmica
hist_valor(30, log = TRUE)
# En log10 la distribución se ve casi simétrica (forma de campana):
# es coherente con datos que crecen de forma multiplicativa (lognormal).

# 4. Por categoría
hist_valor(30) + facet_wrap(~categoria, ncol = 1)
# Tecnología está desplazada hacia valores más altos que Hogar y Moda.


# ============================================================
# Ejercicio 4: color con propósito
# ============================================================

# 1. Paleta divergente: hay un punto de referencia con significado (desvío = 0)
# Ejemplo: desvío de las ventas mensuales de cada canal frente a su promedio.
ventas_mes |>
  group_by(canal) |>
  mutate(desvio = ventas - mean(ventas)) |>
  ungroup() |>
  ggplot(aes(x = mes, y = canal, fill = desvio)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                       midpoint = 0, name = "Desvío\n(millones COP)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Mes", y = NULL)
# Rojo = por debajo del promedio, azul = por encima, blanco = en el promedio.

# 2. Color categórico consistente para "canal"
# La leyenda sobra cuando el color repite lo que ya dice el eje x.
ggplot(pedidos, aes(x = canal, fill = canal)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = colores_canal) +
  labs(x = "Canal", y = "Número de pedidos")

# Histogramas apilados dificultan comparar canales; se separa por panel.
ggplot(pedidos, aes(x = valor / 1000, fill = canal)) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  scale_fill_manual(values = colores_canal) +
  facet_wrap(~canal, ncol = 1, scales = "free_y") +
  labs(x = "Valor del pedido (miles COP)", y = "Pedidos")

# 3. Verificar daltonismo (en código, sin depender de una web externa)
if (requireNamespace("colorspace", quietly = TRUE)) {
  colorspace::swatchplot(
    original = colores_canal,
    deutan   = colorspace::deutan(colores_canal),
    protan   = colorspace::protan(colores_canal),
    tritan   = colorspace::tritan(colores_canal)
  )
}
# Coblis (o esta simulación) sirve para comprobar que las categorías siguen
# siendo distinguibles. Además del color, conviene usar etiquetas, posición
# o forma, y no depender solo de rojo/verde.


# ============================================================
# Ejercicio 5: tabla de contingencia y su gráfico
# ============================================================

# 1. Porcentaje de categorías dentro de cada canal (porcentajes por fila)
tabla <- table(canal = pedidos$canal, categoria = pedidos$categoria)
prop  <- round(100 * prop.table(tabla, margin = 1), 1)
prop

# Canal con mayor porcentaje de Tecnología
names(which.max(prop[, "Tecnología"]))

# ¿La diferencia es real o ruido muestral?
chisq.test(tabla)
# En estos datos, categoria se generó independiente de canal, así que se
# espera un p-valor alto: las diferencias entre filas serían solo azar.
# Concluir "el peso de Tecnología depende del canal" sin esta prueba sería
# sobreinterpretar.

# 2. Gráfico
# La pregunta es sobre proporciones y los canales tienen tamaños muy distintos
# (Tienda ~20 %, App ~45 %), así que las barras al 100 % son la opción correcta.
pedidos |>
  count(canal, categoria) |>
  group_by(canal) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = canal, y = pct, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colores_cat) +
  labs(x = "Canal", y = "% de pedidos del canal", fill = "Categoría")

# Barras agrupadas: útiles para cantidades absolutas, no para comparar proporciones.
ggplot(pedidos, aes(x = canal, fill = categoria)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = colores_cat) +
  labs(x = "Canal", y = "Número de pedidos", fill = "Categoría")

# 3. Tabla presentable
kable(prop, caption = "Porcentaje de pedidos por categoría dentro de cada canal")

# Interpretación:
# 1. Los porcentajes de Tecnología varían entre canales, pero la prueba
#    chi-cuadrado indica si esa variación supera lo esperable por azar.
# 2. Las barras al 100 % comparan proporciones, pero ocultan el volumen total
#    de cada canal; conviene acompañarlas con la tabla de conteos (`tabla`).


# ============================================================
# Ejercicio 6: interactividad con criterio
# ============================================================

# 1. Highcharter en español y con separador de miles
lang_hc <- getOption("highcharter.lang")
lang_hc$decimalPoint <- ","
lang_hc$thousandsSep <- "."
lang_hc$months       <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                          "julio", "agosto", "septiembre", "octubre",
                          "noviembre", "diciembre")
lang_hc$shortMonths  <- c("ene", "feb", "mar", "abr", "may", "jun",
                          "jul", "ago", "sep", "oct", "nov", "dic")
lang_hc$weekdays     <- c("domingo", "lunes", "martes", "miércoles",
                          "jueves", "viernes", "sábado")
options(highcharter.lang = lang_hc)

pedidos |>
  count(categoria, canal) |>
  hchart("column", hcaes(x = categoria, y = n, group = canal)) |>
  hc_colors(unname(colores_canal)) |>   # mismo color por canal que en ggplot2
  hc_title(text = "Pedidos por categoría y canal") |>
  hc_xAxis(title = list(text = "Categoría")) |>
  hc_yAxis(title  = list(text = "Número de pedidos"),
           labels = list(format = "{value:,.0f}")) |>
  hc_tooltip(shared = TRUE,
             pointFormat = "<b>{series.name}</b>: {point.y:,.0f} pedidos<br>")
# Nota: hc_colors asigna colores en el orden de las series (App, Web, Tienda,
# el orden de los niveles de `canal`); verifica en el gráfico que coincidan.

# 2. Versión en ggplot2
pedidos |>
  count(categoria, canal) |>
  ggplot(aes(x = categoria, y = n, fill = canal)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colores_canal) +
  scale_y_continuous(labels = fmt_miles) +
  labs(title = "Pedidos por categoría y canal",
       x = "Categoría", y = "Número de pedidos", fill = "Canal")

# 3. ¿Cuál usar y cuándo?
# - PDF para gerencia: ggplot2. El PDF es estático, así que la interactividad
#   no aporta; ggplot2 da control total sobre tipografía, tamaño y estilo, y
#   el mensaje debe verse de un vistazo (título, colores y etiquetas claros).
# - Dashboard: highcharter. Tooltips, zoom y filtros permiten explorar los
#   datos en pantalla sin generar un gráfico nuevo por cada pregunta.
# - Matiz: la interactividad solo ayuda si alguien explora; si el mensaje
#   principal solo se ve al pasar el mouse, el gráfico está mal diseñado.
#   Y ggplot2 también puede volverse interactivo (plotly, ggiraph) si hace falta.