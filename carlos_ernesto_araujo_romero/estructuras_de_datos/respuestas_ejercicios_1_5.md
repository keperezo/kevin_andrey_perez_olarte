# Respuestas Ejercicios 1 al 5

## Ejercicio 1 Clasificacion de tipos de datos

### 1 Microdatos GEIH del DANE archivo csv
- Tipo Estructurado
- Razon Archivo tabular con esquema fijo filas como observaciones y columnas como variables
- Preprocesamiento Leer con read_csv revisar tipos y eliminar duplicados si existen

### 2 Respuesta JSON de datos gov co
- Tipo Semi estructurado
- Razon Estructura jerarquica con objetos anidados y campos variables sin formato tabular rigido
- Preprocesamiento Parsear con fromJSON y aplanar o desanidar campos con unnest

### 3 Grabaciones de audio de audiencias judiciales
- Tipo No estructurado
- Razon Señal acustica continua sin esquema tabular o filas y columnas definidas
- Preprocesamiento Extraer caracteristicas como MFCC o espectrogramas con tuneR y convertir a matriz tabular

### 4 Factura electronica de la DIAN formato XML
- Tipo Semi estructurado
- Razon Uso de etiquetas jerarquicas con esquema definido pero carente de tablas relacionales rigidas
- Preprocesamiento Parsear con xml2 extraer nodos relevantes y aplanar a tibble

### 5 Tabla HTML de Wikipedia scrapeada con rvest
- Tipo Semi estructurado
- Razon Estructura definida por etiquetas HTML que puede presentar celdas vacias o variaciones por fila
- Preprocesamiento Leer con read_html extraer tabla con html_table y convertir a tibble

## Ejercicio 2 JSON anidado a DataFrame

### 1 Estructura con str raw
El objeto es un data frame donde address y company son columnas que contienen listas anidadas o sub tablas

### 2 Aplanar a tibble una fila por usuario
```r
library(jsonlite)
library(dplyr)
library(tibble)
url <- "https://jsonplaceholder.typicode.com/users"
raw <- fromJSON(url)
datos_tbl <- as_tibble(raw) |>
  mutate(
    address_city = address$city,
    address_geo_lat = address$geo$lat,
    address_geo_lng = address$geo$lng,
    company_name = company$name
  ) |>
  select(id, name, email, address_city, address_geo_lat, address_geo_lng, company_name)
```

### 3 Tipo de dato y comportamiento de fromJSON
Es semi estructurado porque posee objetos anidados variables que impiden un formato plano directo

## Ejercicio 3 Matriz de diseno

### 1 Matriz X centrada y escalada
```r
library(dplyr)
datos_modelo <- titanic |>
  select(survived, pclass, age, fare, sibsp, parch) |>
  na.omit()
y <- datos_modelo$survived
X <- datos_modelo |> select(-survived) |> scale() |> as.matrix()
```
Dimensiones 1045 filas y 5 columnas con un consumo minimo de memoria

### 2 XtX y significado de la diagonal
Cada valor diagonal equivale a n menos 1 porque la funcion scale estandariza las columnas con media cero y varianza uno

### 3 Matriz dispersa
No es recomendable usar matrices dispersas aqui porque la densidad de datos es alta y el almacenamiento de indices incrementa innecesariamente el uso de memoria

### 4 Estructura para relaciones familiares
Corresponde a un grafo mediante igraph ya que sibsp y parch definen conexiones topologicas entre pasajeros y no atributos aislados

## Ejercicio 4 Concentracion de distancias

### 1 Replicacion y resultados
La media de distancia aumenta con la dimension mientras el coeficiente de variacion disminuye progresivamente

### 2 Grafico CV versus dimensionalidad
```r
library(ggplot2)
df <- data.frame(p = dimensiones, media = resumen["media", ], cv = resumen["cv", ] * 100)
ggplot(df, aes(x = p, y = cv)) + geom_line() + geom_point() + scale_x_log10()
```

### 3 Umbral de cinco por ciento
El coeficiente de variacion desciende por debajo del cinco por ciento alrededor de doscientas dimensiones afectando la utilidad de metodos basados en cercania como KNN

### 4 Distancia coseno
Presenta un comportamiento similar de concentracion en espacios densos aunque muestra mayor robustez frente a la dimensionalidad en contextos con alta dispersion

## Ejercicio 5 PCA y reduccion de dimensionalidad

### 1 Aplicacion de PCA
```r
library(dplyr)
datos_num <- titanic |> select(age, fare, sibsp, parch, pclass) |> na.omit()
pca <- prcomp(datos_num, scale. = TRUE)
var_acum <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
```
Se requieren tres componentes para superar el ochenta por ciento y cuatro componentes para el noventa y cinco por ciento de varianza explicada

### 2 Scree plot
```r
scree_df <- data.frame(componente = factor(1:length(var_acum)), varianza_acum = var_acum * 100)
ggplot(scree_df, aes(x = componente, y = varianza_acum, group = 1)) + geom_line() + geom_point()
```

### 3 Loadings de PC1 y PC2
PC1 agrupa estatus socioeconomico mediante tarifa y clase mientras PC2 resume composicion familiar y edad

### 4 K Means sobre datos originales versus componentes principales
Los grupos generados sobre los componentes principales reflejan mejor la supervivencia al eliminar ruido y correlaciones redundantes del espacio original