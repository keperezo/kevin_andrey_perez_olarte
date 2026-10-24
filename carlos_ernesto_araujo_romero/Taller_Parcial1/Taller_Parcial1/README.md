# Taller Parcial: Comparador de Precios en Línea (Confitería)

* **Curso:** Minería de Datos
* **Estudiante:** Carlos Ernesto Araujo Romero
* **Universidad Nacional de Colombia — Sede Bogotá**
* **Lenguaje:** R (versión 4.3.3 sobre Linux x86_64)
* **Categoría:** Confitería (Chocolates, dulces, gomitas y caramelos)
* **Comercios:** Éxito, Carulla, Tiendas Jumbo

---

## 1. Estructura del Repositorio

```text
.
├── raw/                             # Copias crudas del HTML renderizado 
│   ├── exito/YYYY-MM-DD/
│   ├── carulla/YYYY-MM-DD/
│   └── jumbo/YYYY-MM-DD/
├── scraper.R                        # Módulo independiente de extracción 
├── comparador_precios.sqlite        # Base de datos relacional SQLite 
├── taller_1.Rmd                     # Documento maestro reproducible 
├── taller_1.html                    # Reporte compilado interactivo
└── README.md                        # Documentación e instrucciones de ejecución
```

---

## 2. Requisitos y Dependencias

El entorno requiere **R (≥ 4.3.0)** y **Chromium / Brave Browser** instalado en el sistema (`/usr/bin/brave-browser`).

Para instalar las dependencias necesarias en R, ejecute en la consola:
```r
paquetes <- c(
  "rvest", "chromote", "DBI", "RSQLite", 
  "dplyr", "stringr", "tidyr", "lubridate", 
  "highcharter", "knitr", "scales"
)
install.packages(paquetes)
```

---

## 3. Instrucciones de Ejecución

### Modo 1: Compilación Reproducible sin Conexión (Recomendado para Evaluación)
El informe `taller_1.Rmd` viene configurado por defecto en **modo sin conexión (`offline = TRUE`)**. Procesa en segundos las copias crudas almacenadas en `raw/` y consulta la base `comparador_precios.sqlite` sin requerir conexión a internet ni riesgo de bloqueos de IP:

* **Desde RStudio:** Abra `taller_1.Rmd` y presione el botón **Knit**.
* **Desde la consola de R:**
  ```r
  rmarkdown::render("taller_1.Rmd")
  ```

### Modo 2: Extracción en Vivo desde la Web (Online)
Para ejecutar un nuevo ciclo de recolección en vivo contra los servidores de Éxito, Carulla y Jumbo mediante Brave Browser:
```r
source("scraper.R")
ejecutar_recoleccion_completa(offline = FALSE, ruta_db = "comparador_precios.sqlite")
```

---

## 4. Esquema de la Base de Datos (`comparador_precios.sqlite`)

La base relacional cuenta con dos tablas normalizadas:

1. **`ejecuciones`**: Registra la auditoría de cada corrida (`id_ejecucion`, `fecha_inicio`, `fecha_fin`, `total_registros`, `estado`).
2. **`observaciones`**: Registra cada producto extraído con:
   * Identificador común de canasta (`id_producto_comun`) y bandera (`es_comparable = 1`).
   * Nombre original del anuncio (`texto_original`) y código de tienda (`sku_comercio`).
   * Precios en pesos (`precio_cop`) y costo por gramo (`precio_por_unidad`).
   * Contenido neto (`contenido_valor`) y unidad (`contenido_unidad = 'g'`).
   * Fecha y hora en formato ISO 8601 con zona horaria de Colombia.
   * Restricción de unicidad: `UNIQUE(comercio, sku_comercio, fecha_consulta)` para evitar duplicados.

---

## 5. Resumen de Hallazgos

* **Cobertura de la canasta:** Se consolidaron **24 observaciones comparables exactas** ($8 \text{ productos} \times 3 \text{ comercios}$) en una misma ejecución, cumpliendo con la Sección 2.4.
* **Economía de escala:** El empaque familiar de 250g de Chocobreak reduce el costo por gramo a **$62.8 COP/g**, representando un ahorro del **24%** frente a la presentación de 150g ($77.7 COP/g).
* **Mejor opción por cadena:** **Éxito** ofrece los precios más bajos en golosinas individuales e importadas (Kinder, M&M), **Jumbo** en chocolates de mesa familiares (Corona), y **Carulla** empata en paquetes medianos de caramelos blandos.

