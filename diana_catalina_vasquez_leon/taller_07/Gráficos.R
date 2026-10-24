# ============================================================
# CLASE 11 - EJERCICIOS DE PRÁCTICA (pedidos en línea)
# Las respuestas están escritas como comentarios (RESPUESTA)
# ============================================================
install.packages("tidyverse")
install.packages("lubridate")
install.packages("highcharter")
install.packages("knitr")# ---- 0. Paquetes y datos -----------------------------------
library(tidyverse)   # tibble, dplyr, ggplot2, lubridate
library(lubridate)
library(highcharter)
library(knitr)       # kable()

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
)

# Colores fijos por canal (se usan en varios ejercicios)
colores_canal <- c(App = "#006DAE", Web = "#E69F00", Tienda = "#7F7F7F")


# ============================================================
# EJERCICIO 1: tablas de frecuencia
# ============================================================
pedidos |>
  count(calificacion) |>
  mutate(pct = 100 * n / sum(n), acum = cumsum(pct))

pedidos |>
  mutate(rango = cut(valor, c(0, 1e5, 2e5, 4e5, Inf),
                     right = FALSE)) |>
  count(rango)

# Cortes de IGUAL ancho (200 mil)
pedidos |>
  mutate(rango = cut(valor, seq(0, 1.4e6, by = 2e5), right = FALSE)) |>
  count(rango)

# RESPUESTA 1a: 4 o más = 34.0 % + 35.7 % = 69.7 % de los pedidos
#   (equivale a 100 - 30.3, la acumulada en la calificación 3).
# RESPUESTA 1b: calificacion es ordinal (1 < 2 < ... < 5), por eso
#   "hasta 3 estrellas" tiene sentido. canal es nominal, sin orden:
#   sumar App + Web no significa nada.
# RESPUESTA 1c: con cortes de igual ancho quedan casi vacíos los
#   intervalos altos (desde ~600 mil hasta 1.4 millones), porque el
#   valor tiene una cola larga a la derecha: muchos pedidos
#   pequeños y muy pocos enormes.


# ============================================================
# EJERCICIO 2: elige el gráfico según la pregunta
# ============================================================
ventas_mes <- pedidos |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes, canal) |>
  summarise(ventas = sum(valor), .groups = "drop")

# 2a. ¿Qué canal genera más pedidos? -> barras
ggplot(pedidos, aes(canal, fill = canal)) +
  geom_bar() +
  scale_fill_manual(values = colores_canal) +
  labs(x = "Canal", y = "Pedidos") +
  theme(legend.position = "none")
# RESPUESTA 2a: App (~700 pedidos, casi 47 %), luego Web (~515)
#   y Tienda (~275).
#   Justificación: canal es categórica y comparo conteos -> barras.

# 2b. ¿Cómo se distribuyen los días de entrega? -> histograma / barras
ggplot(pedidos, aes(dias_entrega)) +
  geom_bar(fill = "#006DAE") +
  labs(x = "Días de entrega", y = "Pedidos")
# RESPUESTA 2b: asimétrica a la derecha. Lo más común es entregar en
#   3 o 4 días (~320 pedidos cada uno); casi todo cae entre 2 y 5 días
#   y hay una cola pequeña hasta 13 días.
#   Justificación: variable numérica discreta y me interesa la forma
#   de la distribución -> histograma / barras por valor.

# 2c. ¿El valor cambia según la categoría? -> boxplot
ggplot(pedidos, aes(categoria, valor / 1000)) +
  geom_boxplot() +
  labs(x = "Categoría", y = "Valor del pedido (miles COP)")
# RESPUESTA 2c: Sí. Tecnología tiene mediana ~190 mil, casi el doble
#   de Hogar y Moda (~100 mil, muy parecidas entre sí), y más
#   dispersión y valores extremos (hasta más de 1.2 millones).
#   Justificación: comparo una numérica entre grupos; el boxplot
#   muestra mediana, dispersión y atípicos a la vez.

# 2d. ¿Cómo evolucionaron las ventas mensuales por canal? -> líneas
ggplot(ventas_mes, aes(mes, ventas / 1e6, color = canal)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = colores_canal) +
  labs(x = NULL, y = "Ventas (millones COP)")
# RESPUESTA 2d: App lidera casi todo el año (9 a 11 millones/mes) con
#   caídas en abril, junio y diciembre. Web se mueve entre 5 y 7.5
#   millones y solo supera a App en abril (en diciembre quedan a la
#   par). Tienda es el más bajo y estable (2 a 4.5 millones).
#   Justificación: es una serie de tiempo; las líneas muestran la
#   tendencia y permiten comparar canales.


# ============================================================
# EJERCICIO 3: la forma del valor del pedido
# ============================================================
p <- ggplot(pedidos, aes(x = valor / 1000)) +
  labs(x = "Valor del pedido (miles COP)", y = "Pedidos")

p + geom_histogram(bins = 30, fill = "#006DAE", color = "white") +
  geom_vline(xintercept = median(pedidos$valor) / 1000)

p + geom_histogram(bins = 8,   fill = "#006DAE", color = "white")
p + geom_histogram(bins = 120, fill = "#006DAE", color = "white")
p + geom_histogram(bins = 30,  fill = "#006DAE", color = "white") +
  scale_x_log10()

mean(pedidos$valor); median(pedidos$valor)

# Repetir por categoría
p + geom_histogram(bins = 30, fill = "#006DAE", color = "white") +
  facet_wrap(~categoria, ncol = 1)

# RESPUESTA 3a: forma asimétrica a la derecha (sesgo positivo).
#   Centro: mediana ~120 mil (línea vertical); la moda está más a la
#   izquierda, entre 50 y 100 mil. Cola larga hasta más de 1 millón
#   con muy pocos pedidos. La media es mayor que la mediana porque la
#   cola derecha la "jala" hacia arriba (verifícalo con la salida).
# RESPUESTA 3b: 30 bins muestra mejor la forma. Con 8 se pierde el
#   detalle (todo parece un bloque) y con 120 aparece ruido.
# RESPUESTA 3c: con escala log10 la distribución se ve casi simétrica
#   (el valor es aproximadamente lognormal) y la cola deja de dominar.
#   A gerencia le reportaría la MEDIANA, porque la media se infla con
#   los pedidos extremos.
# RESPUESTA 3d: sí. Tecnología está desplazada claramente a la derecha
#   (valores más altos); Hogar y Moda son parecidas. El histograma
#   global mezclaba tres distribuciones y ocultaba esa diferencia.


# ============================================================
# EJERCICIO 4: color con propósito
# ============================================================
desvio_mes <- pedidos |>
  count(mes = month(fecha)) |>
  mutate(desvio = n - mean(n))

ggplot(desvio_mes, aes(factor(mes), desvio, fill = desvio)) +
  geom_col() +
  scale_fill_gradient2(low = "#b2182b", mid = "grey90",
                       high = "#2166ac", midpoint = 0) +
  labs(x = "Mes", y = "Desvío vs. promedio mensual (pedidos)")

# colores_canal ya está definido arriba; segundo uso en otro gráfico:
ggplot(pedidos, aes(categoria, fill = canal)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = colores_canal) +
  labs(x = "Categoría", y = "Pedidos")

# RESPUESTA 4a: conviene una paleta divergente porque hay un punto de
#   referencia con significado: el promedio mensual de pedidos
#   (1500/12 = 125), donde el desvío es 0. Azul = meses sobre el
#   promedio, rojo = meses bajo el promedio. Marzo (+15) es el más
#   alto y junio (-13) el más bajo.
# RESPUESTA 4b: colores_canal es un vector con nombres (App, Web,
#   Tienda) usado con scale_fill_manual / scale_color_manual, así cada
#   canal tiene el mismo color en todos los gráficos.
# RESPUESTA 4c: la paleta divergente rojo-azul y la de canales
#   (azul, naranja, gris) se distinguen sin depender de rojo vs verde.
#   Compruébalo subiendo una captura a Coblis.


# ============================================================
# EJERCICIO 5: tabla de contingencia y su gráfico
# ============================================================
tabla <- table(pedidos$canal, pedidos$categoria)
round(100 * prop.table(tabla, margin = 1), 1)   # % por fila

ggplot(pedidos, aes(x = canal, fill = categoria)) +
  geom_bar(position = "fill") +
  labs(y = "Proporción dentro del canal")

# Barras apiladas con conteos: muestran el TAMAÑO de cada canal
ggplot(pedidos, aes(x = canal, fill = categoria)) +
  geom_bar() +
  labs(y = "Pedidos")

# Tabla con kable
kable(round(100 * prop.table(tabla, margin = 1), 1),
      caption = "% de cada categoría dentro del canal")

# RESPUESTA 5a: Tecnología pesa más en App (34.3 %) y menos en Tienda
#   (28.1 %). Sí coincide con las barras al 100 %: el segmento azul
#   de App es el más grande.
# RESPUESTA 5b: las barras al 100 % ocultan cuántos pedidos tiene cada
#   canal. Lo resuelve un gráfico de mosaico (ancho proporcional al
#   tamaño del canal) o, más simple, barras apiladas/agrupadas con
#   conteos como las de arriba.
# RESPUESTA 5c (interpretación): "Tecnología pesa más en App (34.3 %)
#   y menos en Tienda (28.1 %). En Tienda predomina Moda (40.5 %),
#   mientras que App y Web tienen una mezcla de categorías más pareja."


# ============================================================
# EJERCICIO 6: interactividad con criterio
# ============================================================
options(highcharter.lang = list(thousandsSep = "."))

pedidos |>
  count(categoria, canal) |>
  hchart("column", hcaes(x = categoria, y = n, group = canal)) |>
  hc_colors(unname(colores_canal[c("App", "Tienda", "Web")])) |>
  hc_tooltip(shared = TRUE,
             headerFormat = "<b>{point.key}</b><br/>",
             pointFormat  = "{series.name}: <b>{point.y:,.0f}</b> pedidos<br/>") |>
  hc_xAxis(title = list(text = "Categoría")) |>
  hc_yAxis(title = list(text = "Número de pedidos")) |>
  hc_title(text = "Pedidos por categoría y canal")

# Versión en ggplot2
pedidos |>
  count(categoria, canal) |>
  ggplot(aes(categoria, n, fill = canal)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colores_canal) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Categoría", y = "Número de pedidos",
       title = "Pedidos por categoría y canal")

# RESPUESTA 6a: hc_tooltip(headerFormat, pointFormat) con
#   {point.y:,.0f} para el separador de miles, hc_xAxis / hc_yAxis
#   con title para los ejes, y options(highcharter.lang) con
#   thousandsSep = "." para el formato en español.
#   Lectura: App tiene más pedidos en las tres categorías y Tienda
#   es el canal con menos.
# RESPUESTA 6b: ggplot2 gana en simplicidad, control total del diseño
#   y se incrusta bien en informes estáticos; pierde la interacción
#   (tooltip, zoom, ocultar series). highcharter gana en interacción
#   y pulido web; pierde en que no sirve en papel/PDF y requiere
#   licencia para uso comercial.
# RESPUESTA 6c: informe PDF para gerencia -> ggplot2 (un PDF no puede
#   ser interactivo y se ve igual en cualquier lugar). Dashboard ->
#   highcharter, porque ahí el usuario explora y la interacción aporta.

