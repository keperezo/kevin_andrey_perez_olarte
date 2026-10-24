# =============================================================================
# Minería de Datos — Clase 6: Tipos de datos, estructuras y alta dimensionalidad
# Solución de los ejercicios 1 a 5
# =============================================================================

library(jsonlite)
library(dplyr)
library(tidyr)
library(tibble)
library(Matrix)
library(ggplot2)
library(igraph)

# Ajustar la ruta al archivo del Titanic
setwd("C:/Users/david/OneDrive/Escritorio/Mineria/david_esteban_marquez_bayona/david_esteban_marquez_bayona/Taller_05")
titanic <- read.csv("titanic.csv")
colnames(titanic) <- tolower(colnames(titanic))



# =============================================================================
# EJERCICIO 1: Clasificación de tipos de datos
# =============================================================================

# 1) Microdatos GEIH del DANE (.csv de 300.000 filas)
#    (a) Estructurado.
#    (b) Esquema fijo: cada fila es una persona/hogar y cada columna una
#        variable con tipo definido según el diccionario de datos del DANE.
#    (c) Leer con readr::read_csv() / read_csv2() indicando delimitador (el DANE
#        suele usar ";") y codificación (locale(encoding = "latin1")); fijar
#        tipos de columna, convertir códigos a factores con el diccionario y, si
#        se usan varios módulos, unirlos por DIRECTORIO, SECUENCIA_P y ORDEN.
#
# 2) Respuesta JSON de la API de datos.gov.co (calidad del aire)
#    (a) Semi-estructurado.
#    (b) Tiene pares clave-valor y jerarquía, pero no un esquema tabular rígido:
#        puede haber campos ausentes, objetos anidados (p. ej. coordenadas) o
#        listas de longitud variable entre registros.
#    (c) Parsear con jsonlite::fromJSON(), aplanar los sub-objetos
#        (flatten = TRUE o tidyr::unnest_wider()), convertir tipos (en Socrata
#        casi todo llega como texto: fechas a Date, concentraciones a numeric)
#        y manejar los NA de los campos faltantes.
#
# 3) Grabaciones de audio de audiencias judiciales
#    (a) No estructurado.
#    (b) Es una señal continua sin campos ni esquema; la información (quién
#        habla, qué se dice) no está disponible como variables.
#    (c) Feature engineering: transcripción automática (ASR) para pasar a texto
#        y luego representación numérica (TF-IDF, embeddings), o extracción de
#        características acústicas (MFCC). El tibble queda con una fila por
#        audiencia o segmento: id, duración, texto, features...
#
# 4) Factura electrónica DIAN (XML)
#    (a) Semi-estructurado.
#    (b) Estructura jerárquica con etiquetas y un estándar (UBL 2.1 con
#        namespaces cac:/cbc:), pero anidada: una factura tiene emisor,
#        adquiriente y un número variable de ítems.
#    (c) Parsear con xml2::read_xml(), navegar con XPath (xml_find_all())
#        teniendo en cuenta los namespaces, extraer los campos (NIT, fecha,
#        total, líneas) y armar un tibble con una fila por factura o por ítem.
#
# 5) Tabla HTML de Wikipedia scrapeada con rvest
#    (a) Semi-estructurado.
#    (b) El HTML es un lenguaje de marcado: la información está en etiquetas
#        (<table>, <tr>, <td>) mezclada con contenido irrelevante de la página.
#        La tabla tiene forma tabular, pero hay que extraerla del marcado.
#    (c) rvest::read_html() -> html_element("table") -> html_table(); luego
#        limpieza: nombres de columnas (janitor::clean_names()), quitar notas
#        al pie como [1] y separadores de miles, convertir a numérico y
#        resolver celdas combinadas (rowspan/colspan).


# =============================================================================
# EJERCICIO 2: JSON anidado a DataFrame
# =============================================================================

url <- "https://jsonplaceholder.typicode.com/users"
raw <- fromJSON(url)

# --- 2.1 Exploración de la estructura ---------------------------------------
str(raw, max.level = 3)

# fromJSON() devuelve un data.frame de 10 filas en el que 'address' y 'company'
# NO son columnas atómicas sino data.frames anidados. Dentro de 'address' hay
# un segundo nivel de anidación: 'address$geo' (con lat y lng). El resto
# (id, name, username, email, phone, website) son vectores simples.

# --- 2.2 Aplanado a un tibble (una fila por usuario) ------------------------

# Opción A: extracción explícita con $ (como en clase)
usuarios <- raw |>
  as_tibble() |>
  mutate(
    city         = address$city,
    lat          = as.numeric(address$geo$lat),
    lng          = as.numeric(address$geo$lng),
    company_name = company$name
  ) |>
  select(name, email, city, lat, lng, company_name)

print(usuarios)

# Opción B: aplanado automático con flatten()
usuarios_b <- raw |>
  flatten() |>   # genera address.city, address.geo.lat, company.name, ...
  as_tibble() |>
  select(name, email, address.city, address.geo.lat, address.geo.lng, company.name) |>
  mutate(across(c(address.geo.lat, address.geo.lng), as.numeric))

print(usuarios_b)

# Nota: lat y lng vienen como TEXTO en el JSON; hay que pasarlas a numeric.

# --- 2.3 ¿Qué tipo de dato es y por qué no sale un tibble plano? ------------
# Es un dato SEMI-ESTRUCTURADO: tiene claves con nombre y jerarquía, pero no un
# esquema tabular. fromJSON() no produce un tibble plano porque respeta la
# jerarquía del JSON: un objeto anidado ("address": {...}) no cabe como un
# único valor de celda, así que lo convierte en un data.frame dentro de la
# columna (y 'geo' en un data.frame dentro de ese). La tabla bidimensional no
# puede expresar esa anidación sin decidir cómo nombrar y expandir los niveles;
# esa decisión es el paso de APLANADO (flattening) que le toca al analista. Si
# hubiera listas de longitud variable (p. ej. varios teléfonos) habría además
# que elegir entre ensanchar (unnest_wider) o alargar (unnest_longer).


# =============================================================================
# EJERCICIO 3: Matriz de diseño
# =============================================================================

datos3 <- titanic |>
  select(age, fare, sibsp, parch) |>
  na.omit()

# --- 3.1 Matriz centrada y escalada -----------------------------------------
X <- datos3 |> as.matrix() |> scale()

dim(X)                              # 1045 x 4
print(object.size(X), units = "Kb") # ~ 100 Kb
round(colMeans(X), 10)              # medias = 0
apply(X, 2, sd)                     # desviaciones = 1

# X tiene 1045 pasajeros sin NA x 4 predictores y ocupa ~100 Kb (8 bytes x 4180
# valores = ~33 Kb de datos, más nombres y los atributos 'scaled:center' y
# 'scaled:scale' que agrega scale()).

# --- 3.2 X'X ------------------------------------------------------------------
XtX <- crossprod(X)
round(XtX, 1)

# Dividiendo por n - 1 se obtiene la matriz de correlaciones
round(XtX / (nrow(X) - 1), 3)
all.equal(XtX / (nrow(X) - 1), cor(datos3), check.attributes = FALSE)

# Con X centrada y escalada, cada elemento de la diagonal es
# sum(z_ij^2) = (n-1) * s_j^2 = n - 1 = 1044: la suma de cuadrados de una
# variable estandarizada (n-1 veces su varianza, que vale 1). Fuera de la
# diagonal están (n-1) * r_jk, así que X'X / (n-1) = R, la matriz de
# correlaciones, que es la que descompone PCA con variables escaladas.
# Ej.: correlación positiva sibsp-parch (0.37) y negativa age-sibsp (-0.24).

# --- 3.3 Versión dispersa -----------------------------------------------------
X_sparse <- Matrix(X, sparse = TRUE)

print(object.size(X), units = "Kb")        # densa:    ~ 99.9 Kb
print(object.size(X_sparse), units = "Kb") # dispersa: ~116.1 Kb

mean(X == 0)                          # proporción de ceros en X escalada: 0
round(mean(as.matrix(datos3) == 0), 3) # proporción de ceros sin escalar: 0.349

# NO tiene sentido usar formato disperso aquí. Al centrar, los ceros de sibsp
# y parch se vuelven valores negativos: la matriz escalada tiene 0 % de ceros.
# El formato CSC guarda por cada valor no nulo el valor (8 bytes) más su índice
# de fila (4 bytes) y los punteros de columna; con densidad del 100 % ocupa MÁS
# que la versión densa y además pierde el acceso O(1). Incluso sin escalar solo
# ~35 % de las entradas son cero, lejos de la densidad < 1 % donde el formato
# disperso compensa (DTM, usuario-ítem). Además, centrar una matriz dispersa la
# vuelve densa; por eso en datos realmente dispersos se escala sin centrar.

# --- 3.4 Estructura para las relaciones familiares --------------------------
# Usaría un GRAFO G = (V, E): cada pasajero es un nodo y cada vínculo familiar
# una arista. Conviene tipar las aristas: no dirigidas para hermanos/cónyuges
# (sibsp) y dirigidas padre -> hijo (parch), o un multigrafo con atributo
# 'tipo'. sibsp y parch son solo CONTEOS (el grado del nodo por tipo de
# relación): dicen cuántos familiares tenía cada persona, pero no quiénes.
# La información relevante (qué pasajeros viajaban juntos) es relacional y una
# tabla plana la pierde. Las aristas se reconstruyen cruzando apellido y
# tiquete; con igraph se obtienen las familias (componentes conexas), su tamaño
# y se puede analizar si la supervivencia se correlaciona dentro de cada una.

# Esbozo: familias = pasajeros con el mismo apellido y el mismo tiquete
pasajeros <- titanic |>
  mutate(id = row_number(), apellido = sub(",.*", "", name)) |>
  filter(sibsp + parch > 0)

aristas <- pasajeros |>
  inner_join(pasajeros, by = c("apellido", "ticket"), relationship = "many-to-many") |>
  filter(id.x < id.y) |>
  select(from = id.x, to = id.y)

G_familias <- graph_from_data_frame(aristas, directed = FALSE,
                                    vertices = pasajeros |> select(id, name, survived))
table(components(G_familias)$csize)   # distribución del tamaño de las familias


# =============================================================================
# EJERCICIO 4: Concentración de distancias
# =============================================================================

# --- 4.1 y 4.4: distancia media y CV (euclidiana y coseno) ------------------
# Mismo esquema de la clase: 500 puntos y distancias entre los pares (i, i+200),
# i = 1..200. Ambas métricas se calculan en el mismo recorrido para compararlas
# sobre los MISMOS puntos (las columnas cos_* responden el punto 4).

set.seed(42)
dimensiones <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

dist_coseno <- function(a, b) 1 - sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

resumen <- lapply(dimensiones, function(p) {
  puntos <- matrix(runif(500 * p), nrow = 500, ncol = p)
  d_euc <- sapply(1:200, function(i) sqrt(sum((puntos[i, ] - puntos[i + 200, ])^2)))
  d_cos <- sapply(1:200, function(i) dist_coseno(puntos[i, ], puntos[i + 200, ]))
  tibble(
    p         = p,
    euc_media = mean(d_euc),
    euc_cv    = 100 * sd(d_euc) / mean(d_euc),
    cos_media = mean(d_cos),
    cos_cv    = 100 * sd(d_cos) / mean(d_cos)
  )
}) |> bind_rows()

print(resumen |> mutate(across(-p, \(x) round(x, 4))))

#      p euc_media euc_cv cos_media  cos_cv
#      1    0.3360  69.42    0.0000     NaN
#      2    0.5175  47.81    0.1706  123.70
#      5    0.8949  27.96    0.2317   60.38
#     10    1.2650  18.88    0.2424   40.79
#     50    2.9058   8.82    0.2554   19.70
#    100    4.0450   6.05    0.2439   12.58
#    500    9.0956   2.48    0.2484    5.44
#   1000   12.8877   1.86    0.2493    3.84
#   5000   28.8918   0.75    0.2503    1.68

# --- 4.2 Gráfico: distancia media y CV vs p (escala log en x) ---------------
g_ej4 <- resumen |>
  select(p, `Distancia media` = euc_media, `CV (%)` = euc_cv) |>
  pivot_longer(-p, names_to = "medida", values_to = "valor") |>
  ggplot(aes(p, valor)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(data = tibble(medida = "CV (%)", y = 5),
             aes(yintercept = y), linetype = "dashed", color = "firebrick") +
  facet_wrap(~ medida, scales = "free_y") +
  scale_x_log10(breaks = dimensiones, labels = scales::label_comma()) +
  labs(title = "Concentración de la distancia euclidiana en [0,1]^p",
       subtitle = "Línea punteada: CV = 5 %", x = "p (escala log)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_ej4)

# La distancia media crece como sqrt(p/6), pues E[(x_j - y_j)^2] = 1/6 para
# uniformes independientes (en p = 5000: sqrt(5000/6) = 28.9), mientras que el
# CV cae aproximadamente como 1/sqrt(p).

# --- 4.3 ¿Desde qué p el CV cae por debajo del 5 %? -------------------------
resumen |> filter(euc_cv < 5) |> slice_min(p, n = 1) |> select(p, euc_cv)

# En la grilla, el CV es 6.05 % en p = 100 y 2.48 % en p = 500, que es el
# primer valor bajo el 5 %. Como el CV decrece como c/sqrt(p) (c ~ 60), el
# cruce real ocurre alrededor de p ~ 150.
#
# Implicación para K-NN: con CV < 5 %, la distancia al vecino "más cercano" y
# al "más lejano" difieren muy poco en términos relativos
# ((d_max - d_min) / d_min -> 0). El ranking de vecinos queda dominado por
# ruido, el vecindario local pierde sentido y K-NN pierde poder discriminativo
# (se parece a elegir vecinos al azar). Con unas pocas centenas de variables ya
# conviene reducir dimensión (PCA), seleccionar variables o cambiar de métrica
# o de modelo antes de aplicar K-NN.

# --- 4.4 Distancia coseno -----------------------------------------------------
print(resumen |> select(p, euc_cv, cos_media, cos_cv) |> mutate(across(-p, \(x) round(x, 2))))

g_ej4_cos <- resumen |>
  filter(p > 1) |>
  select(p, Euclidiana = euc_cv, Coseno = cos_cv) |>
  pivot_longer(-p, names_to = "metrica", values_to = "cv") |>
  ggplot(aes(p, cv, color = metrica)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "grey40") +
  scale_x_log10(breaks = dimensiones[-1], labels = scales::label_comma()) +
  scale_y_log10() +
  labs(title = "CV de las distancias: euclidiana vs. coseno",
       x = "p (escala log)", y = "CV (%, escala log)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_ej4_cos)

# La distancia coseno TAMBIÉN se concentra, pero parte de un CV más alto y
# cruza el 5 % más tarde: 5.44 % en p = 500 (vs 2.48 % de la euclidiana) y
# solo baja del 5 % en p = 1000. Su CV es aproximadamente el doble para todo p,
# pero decae a la misma tasa (proporcional a 1/sqrt(p), por ley de grandes
# números sobre el producto interno y las normas). La distancia media coseno
# converge a una constante, 1 - (1/4)/(1/3) = 0.25, en lugar de crecer con p.
#
# Observaciones:
#  (i)  En p = 1 la distancia coseno es siempre 0 (todos los puntos de [0,1]
#       apuntan en la misma dirección), por eso el CV es NaN.
#  (ii) La ventaja del coseno aparece sobre todo en datos DISPERSOS (texto,
#       usuario-ítem), donde compara patrones de coocurrencia y no magnitudes.
#       Con datos uniformes densos no evita la maldición de la dimensionalidad:
#       solo la retrasa.


# =============================================================================
# EJERCICIO 5: PCA y reducción de dimensionalidad
# =============================================================================

datos5 <- titanic |>
  select(survived, age, fare, sibsp, parch, pclass) |>
  na.omit()

X5 <- datos5 |> select(-survived)
dim(X5)   # 1045 x 5

# --- 5.1 PCA y número de componentes ----------------------------------------
pca <- prcomp(X5, scale. = TRUE)
summary(pca)

var_expl <- tibble(
  componente = seq_along(pca$sdev),
  varianza   = pca$sdev^2 / sum(pca$sdev^2),
  acumulada  = cumsum(varianza)
)
print(var_expl)

var_expl |> filter(acumulada >= 0.80) |> slice(1) |> pull(componente)   # 3
var_expl |> filter(acumulada >= 0.95) |> slice(1) |> pull(componente)   # 5

# - >= 80 % de la varianza: 3 componentes (PC1-PC3 acumulan 80.6 %).
# - >= 95 % de la varianza: 5 componentes (con 4 se llega a 92.8 %), o sea todas.
# A diferencia de iris, la reducción es modesta: las correlaciones entre las 5
# variables son débiles o moderadas, así que la varianza está repartida.

# --- 5.2 Scree plot (varianza acumulada vs componente) ----------------------
g_scree <- ggplot(var_expl, aes(componente, acumulada)) +
  geom_col(aes(y = varianza), fill = "grey80", width = 0.6) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = c(0.80, 0.95), linetype = "dashed",
             color = c("darkorange", "firebrick")) +
  annotate("text", x = 1, y = c(0.83, 0.98), label = c("80 %", "95 %"),
           color = c("darkorange", "firebrick"), hjust = 0) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1.02)) +
  labs(title = "Scree plot - PCA Titanic",
       subtitle = "Barras: varianza de cada componente | Línea: varianza acumulada",
       x = "Componente principal", y = "Proporción de varianza") +
  theme_minimal(base_size = 12)

print(g_scree)

# --- 5.3 Loadings de PC1 y PC2 ------------------------------------------------
loadings <- pca$rotation[, 1:2] |>
  as.data.frame() |>
  rownames_to_column("variable")

print(loadings |> mutate(across(c(PC1, PC2), \(x) round(x, 3))))

#   variable    PC1    PC2
#   age       0.492 -0.312
#   fare      0.563  0.330
#   sibsp    -0.088  0.631
#   parch     0.005  0.628
#   pclass   -0.658 -0.030

g_loadings <- loadings |>
  pivot_longer(c(PC1, PC2), names_to = "PC", values_to = "loading") |>
  ggplot(aes(loading, reorder(variable, loading), fill = loading > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ PC) +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(x = "Loading", y = NULL, title = "Loadings de las dos primeras componentes") +
  theme_minimal(base_size = 12)

print(g_loadings)

# (El signo global de cada componente es arbitrario; importa el signo relativo.)
#
# PC1 (35.9 %) — "estatus socioeconómico": dominada por pclass (-0.66),
#   fare (+0.56) y age (+0.49). Valores altos = pasajeros de primera clase
#   (pclass = 1), con tarifas altas y mayores; valores bajos = pasajeros
#   jóvenes de tercera clase con tarifas bajas.
#
# PC2 (31.3 %) — "tamaño del grupo familiar": dominada por sibsp (+0.63) y
#   parch (+0.63), con aportes menores de fare (+0.33; el tiquete familiar
#   cuesta más) y age (-0.31; las familias numerosas incluyen niños). Valores
#   altos = quienes viajaban con muchos familiares.
#
# Las dos primeras componentes separan dos ejes casi independientes:
# clase/riqueza/edad y familia.

# --- 5.4 Reto: K-Means (k = 2) sobre X original vs 2 primeros PCs -----------
# Se incluyen tres representaciones para aislar el efecto de la escala.
# Como las etiquetas de K-Means son arbitrarias, la coincidencia con survived
# se mide con la mejor asignación grupo <-> clase y con el índice de Rand
# ajustado (ARI; 0 = azar, 1 = coincidencia perfecta).

set.seed(42)
km_orig  <- kmeans(X5,           centers = 2, nstart = 25)
km_escal <- kmeans(scale(X5),    centers = 2, nstart = 25)
km_pcs   <- kmeans(pca$x[, 1:2], centers = 2, nstart = 25)

coincidencia <- function(grupo, y) {
  t <- table(grupo, y)
  max(t[1, 1] + t[2, 2], t[1, 2] + t[2, 1]) / length(y)
}

ari <- function(a, b) {
  t   <- table(a, b); n <- sum(t)
  s   <- sum(choose(t, 2))
  sa  <- sum(choose(rowSums(t), 2)); sb <- sum(choose(colSums(t), 2))
  esp <- sa * sb / choose(n, 2)
  (s - esp) / ((sa + sb) / 2 - esp)
}

comparacion <- tibble(
  representacion = c("X original (sin escalar)", "X escalada", "2 primeros PCs"),
  coincidencia   = c(coincidencia(km_orig$cluster,  datos5$survived),
                     coincidencia(km_escal$cluster, datos5$survived),
                     coincidencia(km_pcs$cluster,   datos5$survived)),
  ARI            = c(ari(km_orig$cluster,  datos5$survived),
                     ari(km_escal$cluster, datos5$survived),
                     ari(km_pcs$cluster,   datos5$survived))
)
print(comparacion |> mutate(across(-representacion, \(x) round(x, 3))))

#   representacion           coincidencia   ARI
#   X original (sin escalar)        0.621 0.034
#   X escalada                      0.649 0.083
#   2 primeros PCs                  0.648 0.082

table(grupo = km_orig$cluster, survived = datos5$survived)
table(grupo = km_pcs$cluster,  survived = datos5$survived)

# Perfil de los grupos
perfil <- function(km, nombre) {
  datos5 |>
    mutate(grupo = km$cluster) |>
    group_by(grupo) |>
    summarise(n = n(), tasa_superv = mean(survived), across(age:pclass, mean)) |>
    mutate(representacion = nombre, .before = 1)
}

print(bind_rows(perfil(km_orig, "X original"), perfil(km_pcs, "2 PCs")) |>
        mutate(across(where(is.double), \(x) round(x, 2))))

g_km <- tibble(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
               grupo = factor(km_pcs$cluster),
               survived = factor(datos5$survived,
                                 labels = c("No sobrevivió", "Sobrevivió"))) |>
  ggplot(aes(PC1, PC2, color = grupo, shape = survived)) +
  geom_point(alpha = 0.6) +
  scale_shape_manual(values = c(4, 16)) +
  labs(title = "K-Means (k = 2) sobre los 2 primeros PCs",
       color = "Grupo K-Means", shape = NULL) +
  theme_minimal(base_size = 12)

print(g_km)

# Resultados e interpretación:
#
# - Sobre X original SIN escalar la coincidencia es la más baja (~62 %,
#   ARI ~ 0.03). 'fare' tiene una varianza órdenes de magnitud mayor que las
#   otras variables y domina la distancia euclidiana: K-Means corta por tarifa
#   y aísla un grupo pequeño (67 pasajeros) de tarifas altísimas (media ~211),
#   dejando al 94 % de las personas en el otro grupo. Es un ejemplo directo de
#   por qué hay que escalar antes de usar algoritmos basados en distancias.
#
# - Sobre los 2 primeros PCs la coincidencia sube a ~65 % (ARI ~ 0.08) y los
#   grupos son interpretables: uno de pasajeros mayores, de primera clase y
#   tarifa alta (supervivencia ~59 %) y otro de pasajeros jóvenes de segunda/
#   tercera clase (~33 %). El clustering recupera el eje PC1 de "estatus",
#   que sí está asociado con sobrevivir.
#
# - X escalada y 2 PCs dan prácticamente los mismos grupos (64.9 % vs 64.8 %),
#   porque usan la misma geometría estandarizada y PC1-PC2 retienen el 67 % de
#   la varianza.
#
# Conclusión: los grupos coinciden mejor con la supervivencia en la
# representación de los PCs (o en X escalada) que en X original. Aun así la
# coincidencia es modesta: K-Means es no supervisado y busca estructura de
# varianza, no la variable respuesta, y además la variable más predictiva de
# supervivencia (sex, "mujeres y niños primero") no está entre las usadas.
# Para predecir 'survived' sería más adecuado un modelo supervisado (p. ej.
# regresión logística) que incluya el sexo.