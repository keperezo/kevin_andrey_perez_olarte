# 0. CONFIGURACIÓN ------------------------------------------------------------

paquetes <- c("tidyverse", "patchwork", "plotly", "highcharter",
              "knitr", "gt", "scales")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)     # incluye ggplot2, dplyr, tidyr y lubridate
library(patchwork)
library(plotly)
library(highcharter)
library(knitr)
library(gt)
library(scales)

# Paleta con nombre para canal (Ejercicio 4b). Okabe-Ito: azul, naranja y
# púrpura rojizo; se distinguen también con daltonismo rojo-verde.
colores_canal <- c(App = "#0072B2", Web = "#E69F00", Tienda = "#CC79A7")


# DATASET ---------------------------------------------------------------------

set.seed(909); n <- 1500
dias <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), "day")
pedidos <- tibble(
  fecha        = sample(dias, n, replace = TRUE),
  canal        = sample(c("App", "Web", "Tienda"), n, TRUE,
                        prob = c(.45, .35, .20)),
  categoria    = sample(c("Tecnología", "Hogar", "Moda"),
                        n, replace = TRUE),
  valor        = round(rlnorm(n, 11.5 + 0.6 *
                                (categoria == "Tecnología"), 0.6), -2),
  dias_entrega = rpois(n, lambda = 3) + 1,
  calificacion = sample(1:5, n, TRUE, prob = c(1, 2, 3, 7, 7))
) |>
  # canal como factor con orden fijo: así los colores siempre coinciden
  mutate(canal = factor(canal, levels = names(colores_canal)))

glimpse(pedidos)


# =============================================================================
# EJERCICIO 1 · Tablas de frecuencia
# =============================================================================

# 1a. Distribución de frecuencia de calificacion (absoluta, relativa, acumulada)
tab_calif <- pedidos |>
  count(calificacion) |>
  mutate(pct = 100 * n / sum(n), acum = cumsum(pct))

kable(tab_calif, digits = 1,
      col.names = c("Calificación", "n", "%", "% acumulado"),
      caption = "Distribución de frecuencia de la calificación")

# ¿Qué porcentaje de pedidos tiene calificación de 4 o más?
pct_4_o_mas <- sum(tab_calif$pct[tab_calif$calificacion >= 4])
cat(sprintf("\n>> %.1f%% de los pedidos tiene calificación >= 4.\n",
            pct_4_o_mas))

# ¿Por qué la acumulada tiene sentido para calificacion y no para canal?
# Porque calificacion es ORDINAL (1 < 2 < ... < 5): "hasta 3 estrellas" tiene
# significado. canal es NOMINAL (App, Web, Tienda no tienen orden): el orden en
# que se listen es arbitrario, así que acumular no significa nada.

# Gráfico interactivo (highcharter): % por calificación + % acumulado
g1a <- highchart() |>
  hc_add_series(tab_calif, "column", hcaes(x = calificacion, y = pct),
                name = "% de pedidos") |>
  hc_add_series(tab_calif, "line", hcaes(x = calificacion, y = acum),
                name = "% acumulado") |>
  hc_title(text = "Distribución de la calificación") |>
  hc_xAxis(title = list(text = "Calificación (estrellas)")) |>
  hc_yAxis(title = list(text = "Porcentaje de pedidos"), max = 100) |>
  hc_tooltip(shared = TRUE, valueDecimals = 1, valueSuffix = " %")
g1a

# 1b. Distribución agrupada del valor del pedido (cortes del enunciado)
tab_valor <- pedidos |>
  mutate(rango = cut(valor, c(0, 1e5, 2e5, 4e5, Inf), right = FALSE)) |>
  count(rango) |>
  mutate(pct = 100 * n / sum(n), acum = cumsum(pct))
kable(tab_valor, digits = 1, caption = "Valor del pedido (cortes del enunciado)")

# Con cortes de IGUAL ANCHO (cada 100.000 COP) y mostrando intervalos vacíos
tope <- ceiling(max(pedidos$valor) / 1e5) * 1e5
tab_valor_igual <- pedidos |>
  mutate(rango = cut(valor, breaks = seq(0, tope, by = 1e5),
                     right = FALSE, include.lowest = TRUE,
                     dig.lab = 7)) |>
  count(rango, .drop = FALSE) |>
  mutate(pct = 100 * n / sum(n), acum = cumsum(pct))
kable(tab_valor_igual, digits = 1,
      caption = "Valor del pedido, cortes de igual ancho (100.000 COP)")

# ¿Qué intervalo queda casi vacío y por qué?
# Los últimos intervalos (valores más altos): el valor sigue una lognormal, con
# cola larga a la derecha; pocos pedidos son muy caros, así que con ancho fijo
# los intervalos de la cola quedan con 0 o 1 pedidos.

g1b <- hchart(tab_valor_igual, "column", hcaes(x = rango, y = n),
              name = "Pedidos") |>
  hc_title(text = "Valor del pedido: cortes de igual ancho") |>
  hc_xAxis(title = list(text = "Rango de valor (COP)")) |>
  hc_yAxis(title = list(text = "Pedidos")) |>
  hc_colors("#006DAE") |>
  hc_tooltip(pointFormat = "<b>{point.y}</b> pedidos")
g1b


# =============================================================================
# EJERCICIO 2 · Elige el gráfico según la pregunta (mapa de decisión)
# =============================================================================

ventas_mes <- pedidos |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes, canal) |>
  summarise(ventas = sum(valor), .groups = "drop")

# 2a. ¿Qué canal genera más pedidos?
#     Una categórica -> BARRAS, ordenadas de mayor a menor (comparar categorías).
p2a <- pedidos |>
  count(canal) |>
  ggplot(aes(x = reorder(canal, -n), y = n, fill = canal,
             text = paste0(canal, ": ", n, " pedidos"))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4) +
  scale_fill_manual(values = colores_canal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Pedidos por canal", x = NULL, y = "Pedidos") +
  theme_minimal(base_size = 14)
ggplotly(p2a, tooltip = "text")

# 2b. ¿Cómo se distribuyen los días de entrega?
#     Una numérica (discreta) -> HISTOGRAMA / barras de frecuencia (distribución).
p2b <- ggplot(pedidos, aes(x = dias_entrega)) +
  geom_bar(fill = "#006DAE", color = "white") +
  geom_vline(xintercept = mean(pedidos$dias_entrega),
             color = "#b91c1c", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(pedidos$dias_entrega),
                                  max(pedidos$dias_entrega), by = 1)) +
  labs(title = "Distribución de los días de entrega",
       subtitle = "Línea roja: media",
       x = "Días de entrega", y = "Pedidos") +
  theme_minimal(base_size = 14)
ggplotly(p2b)

# 2c. ¿El valor del pedido cambia según la categoría?
#     Categórica + numérica -> BOXPLOT por grupo (+ puntos: n = 1500 es moderado).
#     plot_ly nativo, como en la clase.
g2c <- plot_ly(pedidos, x = ~categoria, y = ~valor / 1000, color = ~categoria,
               type = "box", boxpoints = "all", jitter = 0.35, pointpos = 0,
               marker = list(opacity = 0.25, size = 4)) |>
  layout(title = "Valor del pedido por categoría",
         xaxis = list(title = ""),
         yaxis = list(title = "Valor del pedido (miles COP)"),
         showlegend = FALSE)
g2c

# 2d. ¿Cómo evolucionaron las ventas mensuales por canal?
#     Variable en el tiempo -> LÍNEAS (una por canal).
p2d <- ggplot(ventas_mes, aes(x = mes, y = ventas / 1e6, color = canal,
                              group = canal)) +
  geom_line(linewidth = 1) +
  geom_point(aes(text = paste0(canal, "<br>", format(mes, "%b %Y"),
                               "<br>Ventas: $",
                               comma(ventas / 1e6, accuracy = 0.1,
                                     big.mark = ".", decimal.mark = ","),
                               " M")), size = 2) +
  scale_color_manual(values = colores_canal) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Ventas mensuales por canal", x = NULL,
       y = "Ventas (millones COP)", color = "Canal") +
  theme_minimal(base_size = 14)
ggplotly(p2d, tooltip = "text")

# Versión highcharter de 2d (eje datetime, como en la clase)
g2d_hc <- hchart(ventas_mes, "line",
                 hcaes(x = mes, y = ventas / 1e6, group = canal)) |>
  hc_xAxis(type = "datetime") |>
  hc_yAxis(title = list(text = "Ventas (millones COP)")) |>
  hc_colors(unname(colores_canal)) |>
  hc_title(text = "Ventas mensuales por canal") |>
  hc_tooltip(shared = TRUE, valueDecimals = 1)
g2d_hc


# =============================================================================
# EJERCICIO 3 · La forma del valor del pedido
# =============================================================================

media_val   <- mean(pedidos$valor)
mediana_val <- median(pedidos$valor)
cat(sprintf("\n>> Media = %s | Mediana = %s | Media > mediana: %s\n",
            comma(media_val, big.mark = "."),
            comma(mediana_val, big.mark = "."),
            media_val > mediana_val))

# Función que repite el histograma del enunciado con distintos bins / escala
hist_valor <- function(bins, log = FALSE) {
  p <- ggplot(pedidos, aes(x = valor / 1000)) +
    geom_histogram(bins = bins, fill = "#006DAE", color = "white") +
    geom_vline(xintercept = mediana_val / 1000, color = "#b91c1c",
               linetype = "dashed") +
    labs(x = paste0("Valor (miles COP) · ", bins, " bins",
                    if (log) " · escala log" else ""),
         y = "Pedidos") +
    theme_minimal(base_size = 12)
  if (log) p <- p + scale_x_log10(labels = label_number(big.mark = "."))
  p
}

# 3a. bins = 8, 30 (enunciado) y 120 lado a lado (interactivo)
g3 <- subplot(ggplotly(hist_valor(8)),
              ggplotly(hist_valor(30)),
              ggplotly(hist_valor(120)),
              nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE,
              margin = 0.04) |>
  layout(title = "Valor del pedido con 8, 30 y 120 bins (línea roja: mediana)")
g3

# 3b. Con scale_x_log10()
g3_log <- subplot(ggplotly(hist_valor(30)),
                  ggplotly(hist_valor(30, log = TRUE)),
                  nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE,
                  margin = 0.05) |>
  layout(title = "Escala lineal vs. escala logarítmica")
g3_log

# 3c. Repetir por categoría (escala log, un panel por categoría)
p3c <- ggplot(pedidos, aes(x = valor / 1000, fill = categoria)) +
  geom_histogram(bins = 30, color = "white", show.legend = FALSE) +
  facet_wrap(~categoria, ncol = 1, scales = "free_y") +
  scale_x_log10(labels = label_number(big.mark = ".")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Valor del pedido por categoría (escala log)",
       x = "Valor del pedido (miles COP)", y = "Pedidos") +
  theme_minimal(base_size = 12)
ggplotly(p3c)

# Resumen por categoría para respaldar la lectura
pedidos |>
  group_by(categoria) |>
  summarise(n = n(), media = mean(valor), mediana = median(valor),
            .groups = "drop") |>
  kable(digits = 0, format.args = list(big.mark = "."),
        caption = "Valor del pedido por categoría (COP)")

# RESPUESTAS (se apoyan en los gráficos y en las cifras impresas arriba):
# - Forma: asimétrica, con sesgo a la derecha (cola larga de pedidos caros).
#   Centro: la mediana queda en la zona más densa; la media es MAYOR que la
#   mediana porque los valores extremos la arrastran hacia la cola.
# - Bins: 8 oculta la forma (todo se agrupa); 120 es ruido (barras con 1 o 2
#   pedidos); ~30 muestra bien el pico y la cola. Punto de partida: sqrt(n) ~ 39.
# - scale_x_log10(): la distribución se ve casi simétrica (compatible con una
#   lognormal) y la cola desaparece. A gerencia: reportar la MEDIANA (o la
#   mediana con percentiles), no la media, que sobreestima el pedido típico.
# - Por categoría: Tecnología aparece desplazada hacia valores más altos que
#   Hogar y Moda. El histograma global mezclaba dos poblaciones con centros
#   distintos, lo que alarga la cola derecha del global.


# =============================================================================
# EJERCICIO 4 · Color con propósito
# =============================================================================

meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

desvio_mes <- pedidos |>
  count(mes = month(fecha)) |>
  mutate(desvio = n - mean(n),
         mes_txt = factor(mes, levels = 1:12, labels = meses_es))

# 4a. Paleta DIVERGENTE: la variable es una desviación respecto a una
#     referencia con significado (el promedio mensual de pedidos, desvío = 0).
#     Azul = por encima del promedio; rojo = por debajo; gris = en el promedio.
p4a <- ggplot(desvio_mes, aes(mes_txt, desvio, fill = desvio,
                              text = paste0(mes_txt, ": ", n, " pedidos<br>",
                                            "Desvío vs. promedio: ",
                                            round(desvio, 1)))) +
  geom_col() +
  scale_fill_gradient2(low = "#b2182b", mid = "grey90", high = "#2166ac",
                       midpoint = 0, name = "Desvío") +
  labs(title = "Pedidos por mes: desvío respecto al promedio mensual",
       x = NULL, y = "Desvío (pedidos)") +
  theme_minimal(base_size = 14)
ggplotly(p4a, tooltip = "text")

# 4b. colores_canal (definido en la configuración) usado en dos gráficos
#     distintos: pedidos por canal (p2a) y ventas mensuales por canal (p2d).
#     Cada canal conserva el mismo color en ambos.
p2a + p2d

# Los colores se asignan por NOMBRE (no por orden de niveles), así que no
# cambian aunque cambie el orden de las barras o las categorías presentes.

# 4c. Daltonismo: toma una captura de estos gráficos y pruébala en
#     https://www.color-blindness.com/coblis-color-blindness-simulator/
# - Azul / naranja / púrpura rojizo (Okabe-Ito) se distinguen con deuteranopía.
# - El divergente azul-rojo (#2166ac / #b2182b) también funciona porque difiere
#   en luminosidad; además el signo del desvío se lee por la dirección de la
#   barra (redundancia).


# =============================================================================
# EJERCICIO 5 · Tabla de contingencia y su gráfico
# =============================================================================

tabla <- table(pedidos$canal, pedidos$categoria)
prop_fila <- prop.table(tabla, margin = 1)
round(100 * prop_fila, 1)                         # % por fila

# ¿En qué canal pesa más Tecnología?
canal_max_tec <- names(which.max(prop_fila[, "Tecnología"]))
cat(sprintf("\n>> Tecnología pesa más en el canal %s (%.1f%% de sus pedidos).\n",
            canal_max_tec, 100 * max(prop_fila[, "Tecnología"])))

# Barras al 100% (interactivas): coinciden con la tabla porcentual por fila
p5 <- ggplot(pedidos, aes(x = canal, fill = categoria)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +   # cualitativa: categorías sin orden
  labs(title = "Composición por categoría dentro de cada canal",
       x = NULL, y = "Proporción dentro del canal", fill = "Categoría") +
  theme_minimal(base_size = 14)
ggplotly(p5)

# 5b. Las barras al 100% ocultan cuántos pedidos tiene cada canal.
#     Lo resuelve el gráfico de MARIMEKKO: ancho = peso del canal (P(canal)),
#     alto = mezcla de categorías dentro del canal (P(categoría | canal)),
#     área = proporción conjunta.
mm <- pedidos |>
  count(canal, categoria) |>
  group_by(canal) |>
  mutate(n_canal = sum(n), prop = n / n_canal) |>
  ungroup()

anchos <- mm |>
  distinct(canal, n_canal) |>
  arrange(canal) |>
  mutate(xmax = cumsum(n_canal) / sum(n_canal),
         xmin = xmax - n_canal / sum(n_canal),
         xmid = (xmin + xmax) / 2)

mm <- mm |>
  left_join(select(anchos, canal, xmin, xmax), by = "canal") |>
  arrange(canal, categoria) |>
  group_by(canal) |>
  mutate(ymax = cumsum(prop), ymin = ymax - prop) |>
  ungroup() |>
  mutate(xmid_celda = (xmin + xmax) / 2,
         ymid_celda = (ymin + ymax) / 2,
         texto = paste0(canal, " · ", categoria,
                        "<br>Pedidos: ", n,
                        "<br>% del canal: ", round(100 * prop, 1), " %",
                        "<br>% del total: ", round(100 * n / sum(n), 1), " %"))

p5b <- ggplot(mm) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = categoria, text = texto), color = "white") +
  geom_text(aes(x = xmid_celda, y = ymid_celda,
                label = percent(prop, accuracy = 1)), size = 3.5) +
  scale_x_continuous(breaks = anchos$xmid,
                     labels = paste0(anchos$canal, "\n(n = ",
                                     anchos$n_canal, ")"),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Marimekko: canal (ancho) y categoría (alto)",
       x = NULL, y = "Proporción dentro del canal", fill = "Categoría") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())
ggplotly(p5b, tooltip = "text")

# 5c. Tabla con kable() + interpretación
tabla_pct <- as.data.frame.matrix(round(100 * prop_fila, 1))
kable(tabla_pct, caption = "Categoría por canal (% por fila)")

# Versión con gt (también vista en clase)
tabla_pct |>
  rownames_to_column("Canal") |>
  gt() |>
  fmt_number(columns = -Canal, decimals = 1, dec_mark = ",") |>
  tab_header(title = "Categoría de producto por canal",
             subtitle = "Porcentaje por fila (cada canal suma 100 %)") |>
  tab_style(style = cell_fill(color = "#dbeafe"),
            locations = cells_body(columns = Tecnología,
                                   rows = Tecnología == max(Tecnología)))

# Dos frases de interpretación (se completan con las cifras calculadas):
cat(sprintf(paste0(
  "\nInterpretación:\n",
  "1) Tecnología pesa más en el canal %s (%.1f%% de sus pedidos), aunque la ",
  "diferencia con los demás canales es pequeña.\n",
  "2) Con n = %d pedidos y canales de distinto tamaño, esas diferencias pueden ",
  "ser ruido muestral; hace falta una prueba (p. ej. chi-cuadrado) antes de ",
  "concluir que el canal cambia la mezcla de categorías.\n"),
  canal_max_tec, 100 * max(prop_fila[, "Tecnología"]), nrow(pedidos)))

# Referencia opcional: prueba de independencia
chisq.test(tabla)


# =============================================================================
# EJERCICIO 6 · Interactividad con criterio
# =============================================================================

# 6a. highcharter en español: tooltip, ejes y separador de miles
lang <- getOption("highcharter.lang")
lang$thousandsSep <- "."
lang$decimalPoint <- ","
options(highcharter.lang = lang)

g6 <- pedidos |>
  count(categoria, canal) |>
  hchart("column", hcaes(x = categoria, y = n, group = canal)) |>
  hc_colors(unname(colores_canal)) |>
  hc_title(text = "Pedidos por categoría y canal") |>
  hc_xAxis(title = list(text = "Categoría de producto")) |>
  hc_yAxis(title = list(text = "Número de pedidos")) |>
  hc_legend(title = list(text = "Canal")) |>
  hc_tooltip(
    shared = TRUE,
    useHTML = TRUE,
    headerFormat = "<b>{point.key}</b><br/>",
    pointFormat = paste0('<span style="color:{point.color}">\u25CF</span> ',
                         "{series.name}: <b>{point.y:,.0f}</b> pedidos<br/>")
  )
g6

# 6b. Versión en ggplot2 (estática, para el informe PDF)
p6 <- pedidos |>
  count(categoria, canal) |>
  ggplot(aes(x = categoria, y = n, fill = canal)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = colores_canal) +
  scale_y_continuous(labels = label_number(big.mark = "."),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Pedidos por categoría y canal",
       x = "Categoría de producto", y = "Número de pedidos", fill = "Canal") +
  theme_minimal(base_size = 14)
p6

# 6c. ¿Qué gana y qué pierde cada una? ¿Cuál usar y dónde?
# - highcharter: GANA hover con el detalle exacto, tooltip compartido por
#   categoría, leyenda clicable para ocultar canales y estética cuidada por
#   defecto. PIERDE en impresión (no sirve en PDF), en peso/dependencias y en
#   licencia: Highcharts exige licencia comercial para uso empresarial.
# - ggplot2: GANA control total, reproducibilidad, salida PNG/SVG liviana y
#   perfecta para PDF/Word; las etiquetas sobre las barras cubren el "hover".
#   PIERDE interacción: todo debe leerse en la imagen.
# - Informe PDF para gerencia -> ggplot2 (estático, con valores rotulados).
# - Dashboard o portal web -> highcharter (o plotly si se quiere código libre),
#   porque el usuario explora, filtra y consulta valores exactos.
