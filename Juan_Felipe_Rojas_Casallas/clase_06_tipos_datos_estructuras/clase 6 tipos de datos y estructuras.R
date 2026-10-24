# Clase 6: Tipos de Datos y Estructuras
# Juan Felipe Rojas Casallas

paquetes  <- c("jsonlite", "dplyr", "tibble", "tidyr", "ggplot2", "Matrix", "readr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(Matrix)
library(readr)


############################## EJERCICIO 1 ##################################
# Clasificación de tipos de datos

Clasificacion_tipos_datos <- tibble(
  fuente = c(
    "Microdatos GEIH del DANE (.csv, 300.000 filas)",
    "Respuesta JSON de la API de datos.gov.co (calidad del aire)",
    "Grabaciones de audio de audiencias judiciales",
    "Factura electrónica DIAN (XML)",
    "Tabla HTML de Wikipedia (scrapeada con rvest)"
  ),
  tipo = c(
    "Estructurado",
    "Semi-estructurado",
    "No estructurado",
    "Semi-estructurado",
    "Semi-estructurado"
  ),
  razon = c(
    "Esquema fijo: columnas con nombre y tipo definidos, se puede consultar directamente con SQL y cargar sin transformación intermedia.",
    "Tiene claves y jerarquía (tipo lista/objeto), pero no una tabla rígida; distintos registros pueden traer distintos campos según el indicador reportado.",
    "No tiene ningún esquema tabular: es una señal continua (forma de onda), no columnas con significado directo.",
    "Usa etiquetas anidadas (jerarquía tipo árbol) como JSON/HTML; el esquema es fijo por norma de la DIAN, pero el almacenamiento es de marcado, no tabular.",
    "Proviene de marcado HTML (etiquetas <table>, <tr>, <td>); el contenido ya se ve tabular en la página, pero la fuente cruda es marcado, no una tabla lista para analizar."
  ),
  preprocesamiento_minimo = c(
    "Leer con readr::read_csv(): ya es tabular, basta con verificar los tipos de columna al importar.",
    "Parsear con jsonlite::fromJSON() y aplanar (unnest) los campos anidados a columnas simples antes de construir el tibble.",
    "Extraer características numéricas del audio (feature engineering, p. ej. MFCC) o transcribir a texto; no puede pasar directo a un tibble.",
    "Parsear el XML (xml2::read_xml()), extraer los nodos relevantes y aplanarlos en columnas de un tibble.",
    "Extraer la tabla con rvest::html_table(); al ser un <table> HTML el aplanado es casi inmediato, solo falta ajustar tipos de columna."
  )
)

print(Clasificacion_tipos_datos, width = Inf)


############################## EJERCICIO 2 ##################################
# JSON anidado a tibble

url_usuarios  <- "https://jsonplaceholder.typicode.com/users"
raw_usuarios  <- fromJSON(url_usuarios)

# 1. Exploramos la estructura: address y company aparecen como columnas que
#    en realidad son data.frames anidados (listas de sub-columnas), no
#    vectores simples; geo queda anidado un nivel más adentro de address.
str(raw_usuarios)

# 2. Aplanamos a un tibble con una fila por usuario
Usuarios_tbl <- tibble(
  name            = raw_usuarios$name,
  email           = raw_usuarios$email,
  address.city    = raw_usuarios$address$city,
  address.geo.lat = as.numeric(raw_usuarios$address$geo$lat),
  address.geo.lng = as.numeric(raw_usuarios$address$geo$lng),
  company.name    = raw_usuarios$company$name
)

Usuarios_tbl

# 3. Interpretación:
# Es una fuente semi-estructurada (JSON): tiene claves y jerarquía, pero no
# un esquema tabular plano. fromJSON() no entrega directamente un tibble
# plano porque el JSON original anida objetos dentro de cada usuario
# (address contiene a su vez geo); R representa esos niveles como
# data.frames dentro del data.frame principal (columnas-lista), y hay que
# "bajar" manualmente a cada nivel (raw_usuarios$address$geo$lat) para
# aplanarlos en columnas simples de primer nivel.


############################## EJERCICIO 3 ##################################
# Matriz de diseño
# NOTA: requiere el archivo "titanic.csv" en esta misma carpeta.

titanic <- read_csv("titanic.csv")
names(titanic) <- tolower(names(titanic))

titanic_ex3 <- titanic %>%
  select(age, fare, sibsp, parch) %>%
  drop_na()

# 1. Matriz X centrada y escalada
X <- scale(as.matrix(titanic_ex3))

dim(X)
object.size(X)

# 2. X^T X
XtX <- t(X) %*% X
XtX

# Interpretación: como cada columna de X está centrada y escalada
# (media 0, varianza 1), la diagonal de X^T X es la suma de cuadrados de
# cada variable estandarizada, que equivale a (n - 1) * varianza = n - 1
# para las cuatro variables (el mismo valor en las cuatro posiciones). Los
# elementos fuera de la diagonal son covarianzas (escaladas por n - 1)
# entre pares de variables.

# 3. Versión dispersa
X_sparse <- Matrix(X, sparse = TRUE)

object.size(X)
object.size(X_sparse)

# Interpretación: X_sparse ocupa igual o más memoria que X, porque al estar
# centrada y escalada la matriz casi no tiene ceros exactos (todos los
# valores son continuos); el formato disperso solo ahorra memoria cuando la
# mayoría de las posiciones son cero, lo cual no es el caso aquí. No tiene
# sentido usar formato disperso para esta matriz.

# 4. Para modelar relaciones familiares (sibsp, parch) entre pasajeros:
# Un grafo G = (V, E), donde V = pasajeros y E = relaciones familiares
# (hermano/cónyuge o padre/hijo). sibsp y parch por sí solos solo dan un
# conteo de familiares por pasajero, pero no dicen con cuál otro pasajero
# existe esa relación; un grafo sí permite representar explícitamente esas
# conexiones entre observaciones, incluso como grafo ponderado o con
# distintos tipos de arista (hermano-cónyuge vs. padre-hijo).


############################## EJERCICIO 4 ##################################
# Concentración de distancias

calcular_concentracion <- function(p, n = 500, n_pares = 200, metodo = "euclidiana") {
  set.seed(42)
  X_sim <- matrix(runif(n * p), nrow = n, ncol = p)

  idx   <- sample(1:n, 2 * n_pares)
  i_idx <- idx[1:n_pares]
  j_idx <- idx[(n_pares + 1):(2 * n_pares)]

  if (metodo == "euclidiana") {
    dif         <- X_sim[i_idx, , drop = FALSE] - X_sim[j_idx, , drop = FALSE]
    distancias  <- sqrt(rowSums(dif^2))
  } else { # coseno
    A               <- X_sim[i_idx, , drop = FALSE]
    B               <- X_sim[j_idx, , drop = FALSE]
    producto_punto  <- rowSums(A * B)
    norma_a         <- sqrt(rowSums(A^2))
    norma_b         <- sqrt(rowSums(B^2))
    distancias      <- 1 - (producto_punto / (norma_a * norma_b))
  }

  data.frame(
    p = p,
    distancia_media = mean(distancias),
    cv = sd(distancias) / mean(distancias) * 100
  )
}

valores_p <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

# 1. Distancia euclidiana
Concentracion_euclidiana <- do.call(
  rbind,
  lapply(valores_p, calcular_concentracion, metodo = "euclidiana")
)

Concentracion_euclidiana

# 2. Gráfico distancia media y CV vs. p (escala log en x)
ggplot(Concentracion_euclidiana, aes(x = p)) +
  geom_line(aes(y = distancia_media, color = "Distancia media")) +
  geom_point(aes(y = distancia_media, color = "Distancia media")) +
  geom_line(aes(y = cv, color = "CV (%)")) +
  geom_point(aes(y = cv, color = "CV (%)")) +
  scale_x_log10() +
  labs(
    x = "p (dimensiones, escala log)", y = "Valor", color = "Métrica",
    title = "Concentración de distancias euclidianas al aumentar p"
  ) +
  theme_minimal()

# 3. ¿A partir de qué p el CV cae por debajo del 5%?
p_umbral_5pct <- Concentracion_euclidiana$p[which(Concentracion_euclidiana$cv < 5)][1]
cat("El CV (euclidiana) cae por debajo del 5% a partir de p =", p_umbral_5pct, "\n")

# Interpretación: a medida que p crece, la distancia media entre puntos
# aumenta, pero el coeficiente de variación (CV) cae hacia cero: todas las
# distancias entre pares se vuelven parecidas entre sí ("concentración de
# distancias"). Para K-NN esto es un problema porque el algoritmo depende
# de que existan vecinos claramente más cercanos que otros; si todas las
# distancias se parecen, el concepto de "vecino más cercano" pierde
# significado y K-NN deja de ser confiable en dimensiones muy altas.

# 4. Repetimos usando distancia coseno
Concentracion_coseno <- do.call(
  rbind,
  lapply(valores_p, calcular_concentracion, metodo = "coseno")
)

Concentracion_coseno

# Nota: para p = 1 todos los valores son positivos (runif entre 0 y 1), por
# lo que la distancia coseno da 0 para los 200 pares (mismo sentido en una
# sola dimensión) y el CV resulta en NaN (0/0); es un caso esperado, no un
# error de código.

p_umbral_5pct_coseno <- Concentracion_coseno$p[which(Concentracion_coseno$cv < 5)][1]
cat("El CV (coseno) cae por debajo del 5% a partir de p =", p_umbral_5pct_coseno, "\n")

# Interpretación: la distancia coseno también se concentra al aumentar p,
# pero lo hace más lentamente que la euclidiana (el CV tarda más en bajar
# del 5%). Esto ocurre porque el coseno normaliza por la norma de cada
# vector y depende del ángulo entre ellos, no de la magnitud absoluta, por
# lo que es algo más robusto (aunque no inmune) frente a la concentración
# de distancias en alta dimensionalidad.


############################## EJERCICIO 5 ##################################
# PCA y reducción de dimensionalidad
# NOTA: requiere el archivo "titanic.csv" en esta misma carpeta (reutiliza
# el objeto "titanic" ya leído y normalizado en el Ejercicio 3).

titanic_ex5 <- titanic %>%
  select(survived, age, fare, sibsp, parch, pclass) %>%
  drop_na()

# 1. PCA con escalado
pca_titanic <- prcomp(
  titanic_ex5 %>% select(age, fare, sibsp, parch, pclass),
  scale. = TRUE
)

varianza_acumulada <- summary(pca_titanic)$importance["Cumulative Proportion", ]
varianza_acumulada

n_componentes_80 <- which(varianza_acumulada >= 0.80)[1]
n_componentes_95 <- which(varianza_acumulada >= 0.95)[1]

cat("Componentes necesarios para >= 80% de varianza:", n_componentes_80, "\n")
cat("Componentes necesarios para >= 95% de varianza:", n_componentes_95, "\n")

# 2. Scree plot (varianza acumulada vs. número de componente)
Scree_df <- data.frame(
  componente         = seq_along(varianza_acumulada),
  varianza_acumulada = varianza_acumulada
)

ggplot(Scree_df, aes(x = componente, y = varianza_acumulada)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  labs(
    x = "Número de componente", y = "Varianza acumulada",
    title = "Scree plot - PCA Titanic"
  ) +
  theme_minimal()

# 3. Loadings de PC1 y PC2
pca_titanic$rotation[, c("PC1", "PC2")]

# Interpretación (verificada con los datos reales de titanic.csv):
# PC1 queda dominado por pclass, fare y age (los tres con mayor valor
# absoluto), con pclass y fare de signo opuesto entre sí: una tarifa más
# alta se asocia a una clase más baja en número (1ª clase), y a pasajeros
# de mayor edad. Se puede interpretar PC1 como un eje de "estatus
# socioeconómico / edad" del pasajero. PC2 queda dominado casi exclusiva-
# mente por sibsp y parch (ambos con signo igual y magnitud similar,
# claramente por encima de age y fare), interpretable como un eje de
# "tamaño del grupo familiar a bordo". Revisa el signo exacto en tu propia
# tabla, ya que prcomp() puede invertir el signo de un componente sin que
# eso cambie su interpretación.

# 4. Reto: K-Means (k = 2) sobre X original vs. sobre los 2 primeros PC
set.seed(42)
X_ex5 <- scale(as.matrix(titanic_ex5 %>% select(age, fare, sibsp, parch, pclass)))

km_original <- kmeans(X_ex5, centers = 2)
km_pca      <- kmeans(pca_titanic$x[, 1:2], centers = 2)

cat("Clusters sobre las variables originales vs. survived:\n")
table(cluster = km_original$cluster, survived = titanic_ex5$survived)

cat("Clusters sobre los 2 primeros componentes vs. survived:\n")
table(cluster = km_pca$cluster, survived = titanic_ex5$survived)

# Interpretación (verificada con los datos reales de titanic.csv):
# compara las dos tablas de contingencia impresas arriba: en cuál de ellas
# los sobrevivientes (survived = 1) quedan más concentrados en un solo
# cluster y los no sobrevivientes en el otro. Con estas 5 variables, ambas
# representaciones separan la supervivencia con una calidad similar y más
# bien baja (ninguna de las dos logra clusters puros); esto tiene sentido
# porque age, fare, sibsp, parch y pclass no incluyen la variable más
# asociada a la supervivencia en el Titanic, que es sex, así que ni el
# espacio original ni sus 2 primeras componentes principales pueden separar
# bien algo que sus propias variables no explican. Reducir a 2 componentes
# aquí no pierde información relevante para este resultado porque esa
# información (sex) nunca estuvo entre las variables usadas.
