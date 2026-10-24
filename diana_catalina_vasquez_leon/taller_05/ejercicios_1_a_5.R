# ============================================================
# Ejercicio 1: clasificación de tipos de datos

# ============================================================

# --- 1. Microdatos GEIH del DANE (.csv, 300.000 filas) ---
# (a) Estructurada
# (b) Es un CSV con filas y columnas fijas (variables como edad, ingreso,
#     ocupación), con un esquema tabular consistente en todas las filas.
# (c) Preprocesamiento minimo: leer el archivo y tipar las columnas.
# geih <- read_csv("microdatos_geih.csv") |>
#   as_tibble() |>
#   mutate(across(where(is.character), as.factor))


# --- 2. Respuesta JSON de la API de datos.gov.co (calidad del aire) ---
# (a) Semi-estructurada
# (b) El JSON tiene un esquema (llaves/valores), pero puede tener
#     anidamiento o campos que no aparecen en todos los registros,
#     a diferencia de un CSV plano.
# (c) Preprocesamiento minimo: aplanar (flatten) el JSON a formato tabular.
# aire <- fromJSON("https://www.datos.gov.co/resource/xxxx.json") |>
#   as_tibble() |>
#   unnest_wider(alguna_columna_anidada)


# --- 3. Grabaciones de audio de audiencias judiciales ---
# (a) No estructurada
# (b) Es una señal de audio continua, sin ningun esquema de filas/columnas
#     ni etiquetas.
# (c) Preprocesamiento minimo: transcripcion de voz a texto (speech-to-text)
#     y luego estructurar esa salida en un tibble.
# audiencias <- tibble(
#   archivo = "audiencia_001.mp3",
#   texto   = transcripcion$texto,
#   minuto  = transcripcion$timestamps
# )


# --- 4. Factura electronica DIAN (XML) ---
# (a) Semi-estructurada
# (b) El XML tiene jerarquia definida por etiquetas (emisor, receptor,
#     items, impuestos), pero es anidado y con secciones repetibles,
#     no una tabla plana.
# (c) Preprocesamiento minimo: parsear el XML y extraer los nodos
#     relevantes a columnas de un tibble.
# items <- read_xml("factura.xml") |>
#   xml_find_all(".//cac:InvoiceLine") |>
#   purrr::map_df(~ tibble(
#     descripcion = xml_text(xml_find_first(.x, ".//cbc:Description")),
#     cantidad    = as.numeric(xml_text(xml_find_first(.x, ".//cbc:InvoicedQuantity"))),
#     valor       = as.numeric(xml_text(xml_find_first(.x, ".//cbc:LineExtensionAmount")))
#   ))


# --- 5. Tabla HTML de Wikipedia scrapeada con rvest ---
# (a) Estructurada (una vez extraida); la pagina HTML completa de origen
#     es semi-estructurada.
# (b) El HTML completo mezcla contenido, marcado y estilos, pero la
#     tabla <table> que se extrae ya tiene filas y columnas bien definidas.
# (c) Preprocesamiento minimo: extraer la tabla y convertirla a tibble.
# tabla <- read_html("https://es.wikipedia.org/wiki/...") |>
#   html_element("table.wikitable") |>
#   html_table() |>
#   as_tibble()
# ============================================================
# Ejercicios 2 a 5 - Curso de Ciencia de Datos
# ============================================================
# Antes de correr esto, instala los paquetes que falten (una sola vez):
# install.packages(c("jsonlite","dplyr","tibble","tidyr",
#                     "Matrix","ggplot2","tidyverse"))

# IMPORTANTE: cambia esta ruta a la carpeta donde tengas titanic.csv
# setwd("ruta/a/tu/carpeta")

library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(Matrix)
library(ggplot2)


# ============================================================
# EJERCICIO 2: JSON anidado a DataFrame
# ============================================================

url <- "https://jsonplaceholder.typicode.com/users"
raw <- fromJSON(url)

# --- 1. Explorar estructura ---
str(raw)
# address y company son data.frames anidados dentro de 'raw' (columnas-lista)

# --- 2. Aplanar a un tibble con una fila por usuario ---
usuarios <- raw |>
  as_tibble() |>
  unnest_wider(address, names_sep = "_") |>   # despliega address.*
  unnest_wider(company, names_sep = "_") |>   # despliega company.*
  unnest_wider(address_geo, names_sep = "_") |> # despliega geo.lat / geo.lng
  select(
    name,
    email,
    address.city = address_city,
    address.geo.lat = address_geo_lat,
    address.geo.lng = address_geo_lng,
    company.name = company_name
  )

print(usuarios)

# --- 3. Tipo de dato y por qué no sale plano ---
cat("
Respuesta 3:
Es una fuente SEMI-ESTRUCTURADA: el JSON tiene un esquema (llaves
consistentes), pero con anidamiento (objetos dentro de objetos: address
dentro de cada usuario, geo dentro de address). fromJSON() convierte el
JSON a listas/data.frames anidados porque un data.frame de R es
inherentemente plano (2 dimensiones); no puede representar de forma
nativa una columna cuyo valor es a su vez otra tabla, así que jsonlite
las deja como columnas-lista (o data.frames anidados) y es tarea nuestra
'aplanarlas' con unnest_wider().
")


# ============================================================
# EJERCICIO 3: matriz de diseño
# ============================================================

titanic <- read.csv("p/Universidad/MinDatos/titanic.csv")
library(readxl)
View(titanic)
vars <- titanic |>
  select(age, fare, sibsp, parch) |>
  na.omit()

# --- 1. X centrada y escalada ---
X <- scale(as.matrix(vars))

dim(X)
print(object.size(X), units = "Kb")

# --- 2. X^T X ---
XtX <- t(X) %*% X
print(XtX)
cat("
Respuesta 2:
Cuando X está escalada (media 0, varianza 1 por columna), X^T X / (n-1)
es la MATRIZ DE CORRELACIÓN de las variables. La diagonal de X^T X
equivale a (n-1) veces la varianza de cada variable escalada, que es 1,
por lo que la diagonal de X^T X es simplemente (n-1) en cada entrada
(la suma de cuadrados de una variable estandarizada).
")

# --- 3. Versión dispersa ---
X_sparse <- Matrix(X, sparse = TRUE)
cat("Tamaño denso: ", format(object.size(X), units = "Kb"), "\n")
cat("Tamaño disperso:", format(object.size(X_sparse), units = "Kb"), "\n")
cat("
Respuesta 3:
NO tiene sentido usar formato disperso aquí: al escalar (centrar y
dividir por la desviación estándar) casi ningún valor queda en cero,
así que la matriz queda densa. El formato disperso solo ahorra memoria
cuando la mayoría de las entradas SON cero; en este caso probablemente
ocupe igual o más memoria que la versión densa (por el overhead de
guardar índices de fila/columna).
")

# --- 4. Estructura para relaciones familiares ---
cat("
Respuesta 4:
Para modelar relaciones familiares entre pasajeros (sibsp, parch) lo
natural es un GRAFO (o una lista de adyacencia / matriz de adyacencia
dispersa), no una matriz numérica densa como X. Cada pasajero es un
nodo y cada relación familiar (hermano/cónyuge, padre/hijo) es una
arista. Como la mayoría de los pasajeros NO tienen relación familiar
entre sí, la matriz de adyacencia sería muy dispersa (mayormente ceros),
así que ahí sí conviene una matriz dispersa (Matrix::sparseMatrix) o
una estructura de grafo con el paquete igraph.
")


# ============================================================
# EJERCICIO 4: concentración de distancias
# ============================================================

set.seed(42)

p_values <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

resultados <- data.frame()

for (p in p_values) {
  # 500 puntos uniformes en [0,1]^p
  puntos <- matrix(runif(500 * p), nrow = 500, ncol = p)

  # 200 distancias euclidianas entre pares aleatorios
  pares <- t(sapply(1:200, function(k) sample(1:500, 2)))
  pares <- pares[pares[,1] != pares[,2], ][1:200, ]

  dist_euclid <- sapply(1:nrow(pares), function(i) {
    sqrt(sum((puntos[pares[i,1], ] - puntos[pares[i,2], ])^2))
  })

  # 200 distancias coseno entre los mismos pares
  dist_coseno <- sapply(1:nrow(pares), function(i) {
    a <- puntos[pares[i,1], ]
    b <- puntos[pares[i,2], ]
    1 - sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  })

  resultados <- rbind(resultados, data.frame(
    p = p,
    dist_media_euclid = mean(dist_euclid),
    cv_euclid = sd(dist_euclid) / mean(dist_euclid) * 100,
    dist_media_coseno = mean(dist_coseno),
    cv_coseno = sd(dist_coseno) / mean(dist_coseno) * 100
  ))
}

print(resultados)

# --- 2. Graficar distancia media y CV vs p (escala log en x) ---
p1 <- ggplot(resultados, aes(x = p)) +
  geom_line(aes(y = dist_media_euclid), color = "steelblue") +
  geom_point(aes(y = dist_media_euclid), color = "steelblue") +
  scale_x_log10() +
  labs(title = "Distancia media euclidiana vs. p",
       x = "p (escala log)", y = "Distancia media") +
  theme_minimal()

p2 <- ggplot(resultados, aes(x = p)) +
  geom_line(aes(y = cv_euclid), color = "firebrick") +
  geom_point(aes(y = cv_euclid), color = "firebrick") +
  geom_hline(yintercept = 5, linetype = "dashed") +
  scale_x_log10() +
  labs(title = "CV de la distancia euclidiana vs. p",
       x = "p (escala log)", y = "CV (%)") +
  theme_minimal()

print(p1)
print(p2)

# --- 3. ¿A partir de qué p el CV cae bajo 5%? ---
p_bajo_5 <- resultados$p[resultados$cv_euclid < 5]
cat("p donde CV < 5%:", if (length(p_bajo_5) > 0) min(p_bajo_5) else "ninguno en el rango probado", "\n")
cat("
Respuesta 3:
Cuando el CV de las distancias cae mucho (tiende a 0), todos los puntos
quedan aproximadamente equidistantes entre sí. Esto es grave para K-NN
porque el algoritmo depende de que existan vecinos claramente 'más
cercanos' que otros; si todas las distancias son casi iguales, el
concepto de 'vecino más cercano' pierde sentido y K-NN deja de ser
informativo (maldición de la dimensionalidad).
")

# --- 4. Repetir con distancia coseno ---
p3 <- ggplot(resultados, aes(x = p)) +
  geom_line(aes(y = cv_coseno), color = "darkgreen") +
  geom_point(aes(y = cv_coseno), color = "darkgreen") +
  scale_x_log10() +
  labs(title = "CV de la distancia coseno vs. p",
       x = "p (escala log)", y = "CV (%)") +
  theme_minimal()
print(p3)

cat("
Respuesta 4:
Compara las columnas cv_euclid y cv_coseno en 'resultados': típicamente
la distancia coseno se concentra igual de rápido (o incluso más rápido)
que la euclidiana a medida que p crece, porque ambas dependen de sumas
de muchas coordenadas independientes que, por la ley de los grandes
números, convergen a un valor esperado con varianza relativa decreciente.
Revisa los números concretos que te dio tu semilla para confirmar la
tendencia en tus resultados.
")


# ============================================================
# EJERCICIO 5: PCA y reducción de dimensionalidad
# ============================================================
titanic5 <- read.csv("p/Universidad/MinDatos/titanic.csv") |>
  select(age, fare, sibsp, parch, pclass, survived) |>
  na.omit()


  select(age, fare, sibsp, parch, pclass, survived) |>
  na.omit()

X5 <- titanic5 |> select(age, fare, sibsp, parch, pclass)

# --- 1. PCA ---
pca <- prcomp(X5, scale. = TRUE)
summary(pca)

varianza <- pca$sdev^2 / sum(pca$sdev^2)
varianza_acum <- cumsum(varianza)
print(round(varianza_acum, 3))

n_80 <- which(varianza_acum >= 0.80)[1]
n_95 <- which(varianza_acum >= 0.95)[1]
cat("Componentes para >=80% varianza:", n_80, "\n")
cat("Componentes para >=95% varianza:", n_95, "\n")

# --- 2. Scree plot (varianza acumulada) ---
scree_df <- data.frame(
  componente = 1:length(varianza_acum),
  varianza_acumulada = varianza_acum
)

p_scree <- ggplot(scree_df, aes(x = componente, y = varianza_acumulada)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = c(0.80, 0.95), linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = scree_df$componente) +
  labs(title = "Scree plot - varianza acumulada",
       x = "Número de componente", y = "Varianza acumulada") +
  theme_minimal()

print(p_scree)

# --- 3. Loadings de PC1 y PC2 ---
print(pca$rotation[, 1:2])
cat("
Respuesta 3:
Observa los valores absolutos más grandes en cada columna de rotation.
En el Titanic, típicamente:
- PC1 suele estar dominado por 'fare' y 'pclass' (con signos opuestos,
  ya que tarifa alta se asocia a clase baja en número = 1ra clase), lo
  que se puede interpretar como un eje de 'estatus socioeconómico'.
- PC2 suele estar dominado por 'sibsp' y 'parch', interpretable como un
  eje de 'tamaño del grupo familiar a bordo'.
Verifica los signos y magnitudes exactos con tu resultado de 'rotation'.
")

# --- 4. K-Means sobre X original vs. sobre las 2 primeras PCs ---
X5_scaled <- scale(X5)

set.seed(42)
km_original <- kmeans(X5_scaled, centers = 2, nstart = 25)

pcs_2 <- pca$x[, 1:2]
set.seed(42)
km_pca <- kmeans(pcs_2, centers = 2, nstart = 25)

tabla_original <- table(km_original$cluster, titanic5$survived)
tabla_pca <- table(km_pca$cluster, titanic5$survived)

cat("Cruce cluster (X original) vs. survived:\n")
print(tabla_original)

cat("\nCruce cluster (2 PCs) vs. survived:\n")
print(tabla_pca)

cat("
Respuesta 4:
Compara qué tan 'puro' es cada cluster respecto a survived en las dos
tablas de arriba (idealmente cada cluster debería concentrar mayormente
sobrevivientes O mayormente no-sobrevivientes). En la práctica, con
estas variables numéricas del Titanic, ninguna de las dos representaciones
suele separar perfectamente la supervivencia (que depende mucho de 'sex'
y 'class', variables que aquí solo entran parcialmente vía pclass), pero
generalmente los resultados sobre las 2 PCs son muy similares a los de
X original, ya que con solo 5 variables las 2 primeras PCs ya retienen
la mayor parte de la varianza. Revisa tus tablas concretas para confirmar
cuál de las dos coincide mejor con 'survived' en tu caso.
")

