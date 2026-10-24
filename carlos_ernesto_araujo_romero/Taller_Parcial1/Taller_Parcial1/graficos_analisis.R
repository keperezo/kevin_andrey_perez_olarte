library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(highcharter)

con <- dbConnect(SQLite(), "comparador_precios.sqlite")

df_obs <- dbGetQuery(con, "
  SELECT 
    comercio,
    id_producto_comun,
    texto_original,
    precio_cop,
    contenido_valor,
    contenido_unidad,
    precio_por_unidad
  FROM observaciones
  WHERE es_comparable = 1;
")

dbDisconnect(con)

# Paleta corporativa consistente para las 3 tiendas
colores_tiendas <- c(
  "Carulla" = "#27AE60",  # Verde
  "Éxito"   = "#F1C40F",  # Amarillo
  "Jumbo"   = "#2980B9"   # Azul
)

# Diccionario de nombres comerciales legibles
nombres_canasta <- c(
  "CONF_01_KINDER_LECHE_100G"    = "Kinder Chocolate 100g",
  "CONF_02_MYM_LECHE_48G"        = "M&M con Leche 48g",
  "CONF_03_MYM_MANI_49G"         = "M&M con Maní 49g",
  "CONF_04_CHOCOBREAK_150G"      = "Chocobreak Surtido 150g",
  "CONF_05_CHOCOBREAK_250G"      = "Chocobreak Surtido 250g",
  "CONF_06_CHOCOLATE_CORONA_TRAD" = "Choc. Corona Tradicional",
  "CONF_07_JUMBO_MANI"           = "Chocolatina Jumbo Maní 100g",
  "CONF_09_FERRERO_ROCHER"       = "Ferrero Rocher Bombones x8"
)

# GRÁFICO 1: COMPARACIÓN DE PRECIOS DE PRESENTACIONES IDÉNTICAS ENTRE TIENDAS

df_g1 <- df_obs %>%
  filter(!is.na(id_producto_comun), id_producto_comun %in% names(nombres_canasta)) %>%
  mutate(producto_nombre = recode(id_producto_comun, !!!nombres_canasta)) %>%
  group_by(producto_nombre, comercio) %>%
  summarise(precio_cop = min(precio_cop), .groups = "drop") %>%
  arrange(producto_nombre, comercio)

grafico_1 <- hchart(
  df_g1, 
  type = "column", 
  hcaes(x = producto_nombre, y = precio_cop, group = comercio)
) %>%
  hc_colors(unname(colores_tiendas[c("Carulla", "Éxito", "Jumbo")])) %>%
  hc_title(text = "1. Comparación de Precios de Confitería: Las 3 Tiendas") %>%
  hc_subtitle(text = "Precios observados para 8 presentaciones idénticas en Éxito, Carulla y Jumbo (COP)") %>%
  hc_xAxis(
    title = list(text = "Producto / Presentación"),
    labels = list(style = list(fontSize = "11px"))
  ) %>%
  hc_yAxis(
    title = list(text = "Precio en Góndola (COP)"),
    labels = list(format = "${value:,.0f}")
  ) %>%
  hc_tooltip(
    shared = TRUE,
    headerFormat = "<b>{point.key}</b><br/>",
    pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>${point.y:,.0f} COP</b><br/>'
  ) %>%
  hc_plotOptions(column = list(
    borderRadius = 4, 
    grouping = TRUE
  )) %>%
  hc_exporting(enabled = TRUE)

# Renderizar Gráfico 1
grafico_1

# INTERPRETACIÓN Y LIMITACIONES - GRÁFICO 1
cat("          ANÁLISIS GRÁFICO 1         \n")
cat("• INTERPRETACIÓN:\n",
    "  - Éxito ofrece sistemáticamente el precio más económico en golosinas individuales e importadas:\n",
    "    Kinder Chocolate ($12,000 vs $12,600 en Carulla y $12,490 en Jumbo) y M&M Maní ($5,091 vs $5,601 y $6,590).\n",
    "  - Jumbo ofrece el precio más bajo en chocolates tradicionales de mesa como Corona 450g ($15,600).\n",
    "  - Carulla es competitivo en paquetes medianos como Chocobreak 150g ($10,500 vs $11,650 en Éxito),\n",
    "    y empata exactamente en Chocobreak 250g ($15,700 en Éxito y Carulla).\n",
    "• LIMITACIONES:\n",
    "  - Compara exclusivamente el precio de lista observado en la fecha de consulta.\n",
    "  - No contempla cargos de envío a domicilio, beneficios por programas de fidelización (Puntos Colombia),\n",
    "    ni montos mínimos de compra para despacho virtual.\n\n")

# GRÁFICO 2: PRECIO POR UNIDAD DE CONTENIDO (COP / GRAMO) - ECONOMÍA DE ESCALA

df_g2 <- df_obs %>%
  filter(id_producto_comun %in% c("CONF_04_CHOCOBREAK_150G", "CONF_05_CHOCOBREAK_250G", "CONF_06_CHOCOLATE_CORONA_TRAD")) %>%
  mutate(
    presentacion_label = case_when(
      str_detect(id_producto_comun, "CORONA") ~ paste0("Corona (", contenido_valor, "g) - ", comercio),
      str_detect(id_producto_comun, "CHOCOBREAK") ~ paste0("Chocobreak (", contenido_valor, "g) - ", comercio),
      TRUE ~ paste0(contenido_valor, "g - ", comercio)
    )
  ) %>%
  # Agrupamos limpiamente para asegurar una sola etiqueta por barra (cero texto montado)
  group_by(presentacion_label, comercio, contenido_valor) %>%
  summarise(
    precio_promedio = mean(precio_cop),
    cop_por_gramo   = round(mean(precio_por_unidad), 1),
    .groups = "drop"
  ) %>%
  arrange(cop_por_gramo)

grafico_2 <- hchart(
  df_g2, 
  type = "bar", 
  hcaes(x = presentacion_label, y = cop_por_gramo, color = comercio)
) %>%
  hc_colors(unname(colores_tiendas[c("Carulla", "Éxito", "Jumbo")])) %>%
  hc_title(text = "2. Eficiencia de Contenido: Precio por Gramo (COP / g)") %>%
  hc_subtitle(text = "Economía de escala: empaques pequeños vs. empaques familiares (Barras limpias)") %>%
  hc_xAxis(title = list(text = "Presentación y Comercio")) %>%
  hc_yAxis(
    title = list(text = "Costo por Gramo (COP/g)"),
    labels = list(format = "${value:,.0f}/g")
  ) %>%
  hc_tooltip(
    pointFormat = '<b>{point.comercio}</b><br/>Costo unitario: <b>${point.y:.1f} COP/g</b><br/>Contenido neto: <b>{point.contenido_valor} g</b><br/>Precio de empaque: <b>${point.precio_promedio:,.0f} COP</b>'
  ) %>%
  hc_plotOptions(bar = list(
    borderRadius = 4,
    dataLabels = list(
      enabled = TRUE, 
      format = "${point.y:.1f}/g",
      style = list(fontWeight = "bold", fontSize = "11px")
    )
  )) %>%
  hc_exporting(enabled = TRUE)

# Renderizar Gráfico 2
grafico_2

# INTERPRETACIÓN Y LIMITACIONES - GRÁFICO 2
cat("          ANÁLISIS GRÁFICO 2 \n")
cat("• INTERPRETACIÓN:\n",
    "  - Demuestra con claridad la ventaja de volumen: los empaques familiares reducen el costo unitario.\n",
    "  - El Chocolate Corona familiar (450g-500g) en Jumbo ofrece el costo más bajo del mercado ($33.2 a $34.7 COP/g),\n",
    "    mientras que presentaciones pequeñas de 200g suben a $41.5 COP/g (un 25% más costoso por gramo).\n",
    "  - En Chocobreak ocurre lo mismo: el empaque de 250g sale a $62.8 COP/g, mientras que el paquete de 150g\n",
    "    cuesta entre $70.0 y $77.7 COP/g (un 15% a 24% más costoso por gramo).\n",
    "• LIMITACIONES:\n",
    "  - El beneficio económico requiere que el consumidor tenga la liquidez inicial para adquirir el empaque grande\n",
    "    y que la tasa de consumo evite el vencimiento o deterioro del alimento.\n\n")

# GRÁFICO 3: OPORTUNIDADES DE AHORRO POTENCIAL (% Y DIFERENCIA EN COP)

con <- dbConnect(SQLite(), "comparador_precios.sqlite")
df_g3 <- dbGetQuery(con, "
  SELECT 
    id_producto_comun,
    MIN(precio_cop) AS precio_min,
    MAX(precio_cop) AS precio_max,
    ROUND(MAX(precio_cop) - MIN(precio_cop), 0) AS ahorro_absoluto_cop,
    ROUND(100.0 * (MAX(precio_cop) - MIN(precio_cop)) / MAX(precio_cop), 1) AS ahorro_porcentual
  FROM observaciones
  WHERE es_comparable = 1
  GROUP BY id_producto_comun
  HAVING COUNT(DISTINCT comercio) > 1
  ORDER BY ahorro_porcentual DESC;
")
dbDisconnect(con)

# Mapear a nombres comerciales reales
df_g3 <- df_g3 %>%
  filter(id_producto_comun %in% names(nombres_canasta)) %>%
  mutate(producto_comercial = recode(id_producto_comun, !!!nombres_canasta))

grafico_3 <- hchart(
  df_g3, 
  type = "lollipop", 
  hcaes(x = producto_comercial, y = ahorro_porcentual)
) %>%
  hc_colors("#E74C3C") %>%
  hc_title(text = "3. Oportunidades de Ahorro Máximo por Producto (%)") %>%
  hc_subtitle(text = "Brecha porcentual entre el comercio más costoso y el más económico para el mismo producto") %>%
  hc_xAxis(title = list(text = "Producto")) %>%
  hc_yAxis(
    title = list(text = "Ahorro Potencial (%)"),
    labels = list(format = "{value}%"),
    max = 30
  ) %>%
  hc_tooltip(
    pointFormat = 'Ahorro porcentual: <b>{point.y:.1f}%</b><br/>Ahorro en pesos: <b>${point.ahorro_absoluto_cop:,.0f} COP</b><br/>Precio mínimo: <b>${point.precio_min:,.0f}</b> | Máximo: <b>${point.precio_max:,.0f}</b>'
  ) %>%
  hc_plotOptions(lollipop = list(
    dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%", style = list(fontWeight = "bold"))
  )) %>%
  hc_exporting(enabled = TRUE)

# Renderizar Gráfico 3
grafico_3

# INTERPRETACIÓN Y LIMITACIONES - GRÁFICO 3
cat("          ANÁLISIS GRÁFICO 3     \n")
cat("• INTERPRETACIÓN:\n",
    "  - La mayor brecha relativa se presenta en M&M con Maní (22.7% de diferencia; $1,499 COP de ahorro)\n",
    "    y en Chocobreak 150g (19.8% de diferencia; $2,550 COP de ahorro al elegir Carulla sobre Éxito).\n",
    "  - En chocolates finos o tradicionales (Ferrero Rocher, Corona 450g, Chocobreak 250g), las brechas de precio\n",
    "    son reducidas (0.0% a 5.1%), lo que indica alta competencia de precios en marcas de alta rotación.\n",
    "• LIMITACIONES:\n",
    "  - El porcentaje de ahorro puede fluctuar en cuestión de días por promociones relámpago de comercio electrónico.\n\n")

# CONCLUSIÓN GENERAL Y ORIENTACIÓN AL CONSUMIDOR 
cat("             CONCLUSIONES Y RECOMENDACIONES AL CONSUMIDOR           \n")
cat("1. DÓNDE CONVIENE COMPRAR:\n",
    "   • Para golosinas individuales e importadas (Kinder, M&M, Jet): El comercio más económico es ÉXITO.\n",
    "   • Para chocolate tradicional de mesa y cocina (Corona): El comercio más conveniente es JUMBO.\n",
    "   • Para bolsas de gomitas y caramelos medianos (Chocobreak 150g): CARULLA ofrece el precio más bajo.\n",
    "   • En empaques grandes (Chocobreak 250g): Existe empate técnico entre ÉXITO y CARULLA ($15,700).\n\n",
    "2. DESEMBOLSO TOTAL VS. AHORRO POR UNIDAD:\n",
    "   • Un desembolso menor en caja no garantiza un menor costo real: comprar empaques pequeños de 150g-200g\n",
    "     exige menos dinero inmediato, pero resulta entre un 15% y un 25% más costoso por gramo consumido.\n",
    "   • Para consumo recurrente del hogar, se recomienda priorizar presentaciones de 450g a 500g.\n\n",
    "3. CONDICIONES DE COMPRA:\n",
    "   • Para compras de bajo monto ($5,000 - $15,000), el costo del domicilio puede anular cualquier ahorro;\n",
    "     conviene consolidar la compra dentro de la canasta familiar mensual.\n")
