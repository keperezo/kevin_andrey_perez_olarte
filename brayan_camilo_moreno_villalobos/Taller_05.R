# ==============================================================================
# CARGA DE LIBRERÍAS Y CONFIGURACIÓN INICIAL
# ==============================================================================
library(jsonlite)
library(dplyr)
library(tibble)
library(Matrix)
library(ggplot2)
library(purrr)
library(tidyr)

set.seed(42)

# ==============================================================================
# EJERCICIO 2: JSON anidado a DataFrame
# ==============================================================================
url_json <- "https://jsonplaceholder.typicode.com/users"
raw_json <- fromJSON(url_json)

# 1. Explorar estructura del JSON
str(raw_json)

# 2. Aplanar a tibble con las columnas requeridas
df_json_plano <- jsonlite::flatten(raw_json) %>%
  as_tibble() %>%
  select(
    name,
    email,
    address.city,
    address.geo.lat,
    address.geo.lng,
    company.name
  )

print("--- EJERCICIO 2: TIBBLE PLANO ---")
print(df_json_plano)

# ==============================================================================
# EJERCICIO 3: Matriz de diseño
# ==============================================================================
# Carga de datos de Titanic (asegúrate de tener 'titanic.csv' en el working directory)
titanic <- read.csv("titanic.csv")

df_ex3 <- titanic %>% 
  select(age, fare, sibsp, parch) %>% 
  na.omit()

# 1. Matriz X centrada y escalada + Reporte de memoria
X_mat <- scale(as.matrix(df_ex3))
cat("\n--- EJERCICIO 3 ---\n")
cat("Dimensiones de X:", dim(X_mat), "\n")
cat("Memoria ocupada por X:", object.size(X_mat), "bytes\n")

# 2. Cálculo de X^T X
XtX <- t(X_mat) %*% X_mat
print("Matriz X^T X:")
print(XtX)

# 3. Versión dispersa y comparación de memoria
X_sparse <- Matrix(X_mat, sparse = TRUE)
cat("Memoria de X dispersa:", object.size(X_sparse), "bytes\n")

# ==============================================================================
# EJERCICIO 4: Concentración de distancias
# ==============================================================================
p_vec <- c(1, 2, 5, 10, 50, 100, 500, 1000, 5000)

# 1. Función para simular distancias euclidianas
simular_euclidiana <- function(p, n_puntos = 500, n_pares = 200) {
  X <- matrix(runif(n_puntos * p), nrow = n_puntos, ncol = p)
  i <- sample(1:n_puntos, n_pares, replace = TRUE)
  j <- sample(1:n_puntos, n_pares, replace = TRUE)
  
  dists <- sqrt(rowSums((X[i, , drop = FALSE] - X[j, , drop = FALSE])^2))
  tibble(p = p, media = mean(dists), cv = sd(dists) / mean(dists))
}

res_euc <- map_dfr(p_vec, simular_euclidiana)
print("\n--- EJERCICIO 4: DISTANCIA EUCLIDIANA ---")
print(res_euc)

# 2. Gráfico con ggplot2
res_euc_long <- res_euc %>%
  pivot_longer(cols = c(media, cv), names_to = "metrica", values_to = "valor")

p_euc_plot <- ggplot(res_euc_long, aes(x = p, y = valor, color = metrica)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10() +
  facet_wrap(~metrica, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Concentración de Distancias Euclidianas",
    x = "Dimensión p (escala log10)",
    y = "Valor"
  )

print(p_euc_plot)

# 4. Simulación con Distancia Coseno
simular_coseno <- function(p, n_puntos = 500, n_pares = 200) {
  X <- matrix(runif(n_puntos * p), nrow = n_puntos, ncol = p)
  i <- sample(1:n_puntos, n_pares, replace = TRUE)
  j <- sample(1:n_puntos, n_pares, replace = TRUE)
  
  Xi <- X[i, , drop = FALSE]
  Xj <- X[j, , drop = FALSE]
  
  prod_punto <- rowSums(Xi * Xj)
  norma_i <- sqrt(rowSums(Xi^2))
  norma_j <- sqrt(rowSums(Xj^2))
  
  d_cos <- 1 - (prod_punto / (norma_i * norma_j))
  
  m_val <- mean(d_cos, na.rm = TRUE)
  sd_val <- sd(d_cos, na.rm = TRUE)
  cv_val <- if (is.na(sd_val) || m_val == 0) NA else sd_val / m_val
  
  tibble(p = p, media = m_val, cv = cv_val)
}

res_cos <- map_dfr(p_vec, simular_coseno)
print("\n--- EJERCICIO 4: DISTANCIA COSENO ---")
print(res_cos)

# ==============================================================================
# EJERCICIO 5: PCA y reducción de dimensionalidad
# ==============================================================================
df_ex5 <- titanic %>%
  select(age, fare, sibsp, parch, pclass, survived) %>%
  na.omit()

X_pca_data <- df_ex5 %>% select(age, fare, sibsp, parch, pclass)

# 1. Aplicación de PCA
pca_res <- prcomp(X_pca_data, scale. = TRUE)
summary_pca <- summary(pca_res)

print("\n--- EJERCICIO 5: RESUMEN PCA ---")
print(summary_pca)

# 2. Scree plot con ggplot2
var_acum <- summary_pca$importance["Cumulative Proportion", ]
df_scree <- tibble(
  PC = factor(names(var_acum), levels = names(var_acum)),
  var_acum = as.numeric(var_acum)
)

p_scree_plot <- ggplot(df_scree, aes(x = PC, y = var_acum, group = 1)) +
  geom_line(color = "#2b5c8f", linewidth = 1) +
  geom_point(color = "#1a365d", size = 3) +
  geom_hline(yintercept = c(0.80, 0.95), linetype = "dashed", color = "firebrick") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Scree Plot: Varianza Acumulada Explicada por PC",
    x = "Componentes Principales",
    y = "Varianza Acumulada"
  ) +
  theme_minimal()

print(p_scree_plot)

# 3. Loadings de PC1 y PC2
print("Loadings (Rotación) PC1 y PC2:")
print(pca_res$rotation[, c("PC1", "PC2")])

# 4. Reto K-Means (k = 2) en X original vs 2 primeros PCs
set.seed(42)
X_pca_scaled <- scale(X_pca_data)

km_orig <- kmeans(X_pca_scaled, centers = 2, nstart = 25)
km_pca  <- kmeans(pca_res$x[, 1:2], centers = 2, nstart = 25)

# Medición de coincidencia máxima con la variable 'survived'
mould_acc <- function(cluster, target) {
  max(mean(cluster == target), mean((3 - cluster) == target))
}

cat("\nCoincidencia K-Means (X original escalado):", round(mould_acc(km_orig$cluster, df_ex5$survived) * 100, 2), "%\n")
cat("Coincidencia K-Means (2 Primeros PCs):", round(mould_acc(km_pca$cluster, df_ex5$survived) * 100, 2), "%\n")