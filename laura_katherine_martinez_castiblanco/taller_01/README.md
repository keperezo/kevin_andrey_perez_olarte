# Taller 1: Comparador de Precios de Bebidas en Línea
**Asignatura:** Minería de Datos (Código: 2016325)  
**Institución:** Universidad Nacional de Colombia — Departamento de Estadística  
**Estudiante:** Laura Katherine Martínez Castiblanco  
**Marco Metodológico:** KDD (Knowledge Discovery in Databases)

---

## 1. Visión General del Proyecto

Este repositorio contiene la solución completa al **Taller 1: Comparador de Precios en Línea**, cuyo objetivo es recolectar, estandarizar, almacenar y analizar la oferta de bebidas de consumo masivo en cadenas minoristas de Colombia (Éxito, Carulla, PriceSmart y Supermercado Comunal). 

El sistema implementa un pipeline end-to-end que abarca desde la adquisición automatizada web (*web scraping* con renderizado dinámico en Chrome), la carga relacional transaccional en SQLite con prevención de duplicados, la creación de vistas de comparación analítica, la evaluación mediante 8 consultas SQL, hasta la generación de un informe web interactivo y ejecutable en R Markdown.

---

## 2. Estructura y Navegación del Repositorio

Todos los componentes del proyecto se encuentran organizados dentro de la carpeta individual de entrega (`laura_katherine_martinez_castiblanco/taller_01/`):

```text
taller_01/
├── [enlace sospechoso eliminado]                      # [Este archivo] Guía de navegación y ejecución
├── taller_1.Rmd                   # Informe principal reproducible en R Markdown
├── taller_1.html                  # Reporte web final autocontenido (con pestañas y tablas)
├── scraper.R                      # Módulo de extracción e inserción independiente
├── helpers.R                      # Funciones de apoyo (vistas SQL, tablas DT y componentes UI)
├── consultas.sql                  # Sentencias SQL de las 8 preguntas (visor de código)
├── comparador_precios.sqlite      # Base de datos relacional SQLite
├── df_resultado_final.rds         # Objeto R comprimido para tejido offline instantáneo
│
├── raw/                           # Respaldos inmutables en HTML crudo por comercio
│   ├── exito/                     # Capturas HTML de Éxito
│   ├── carulla/                   # Capturas HTML de Carulla
│   ├── pricesmart/                # Capturas HTML de PriceSmart
│   └── supercomunal/              # Capturas HTML de Supermercado Comunal
│
├── evidencias/                    # Capturas de pantalla de la ejecución y scraping
├── renv.lock                      # Matriz de dependencias congeladas del entorno R
├── UNAL_logo.png                  # Emblema institucional para el informe HTML
├── style.css                      # Hoja de estilos (temas, modo nocturno, responsive)
├── scripts.js                     # Motor JavaScript (visor de código, atajos y panel lateral)
└── header.html                    # Inclusión de cabecera HTML
```

---

## 3. Arquitectura de Módulos del Sistema KDD

| Módulo / Archivo | Rol en el Pipeline KDD | Descripción Técnica |
| :--- | :--- | :--- |
| **`scraper.R`** | **1. Adquisición y Extracción** | Script desacoplado que coordina el scraping en vivo o la relectura desde `raw/`. Maneja bypass SSL, scroll automático con `chromote`/`rvest`, extracción de metadatos, cálculo de MD5 para SKUs e inserción en SQLite vía tabla de paso (`temp_staging`). |
| **`comparador_precios.sqlite`** | **2. Almacenamiento Relacional** | Base de datos relacional normalizada. Contiene la tabla `ejecuciones` (metadatos de auditoría) y la tabla `observaciones` (esquema DDL de 22 variables con clave primaria compuesta). |
| **`helpers.R`** | **3. Preparación y Vistas SQL** | Conecta a la base y construye dinámicamente las vistas `v_categoria` (surtido general corregido) y `v_comparables` (canasta común homologada de la corrida más reciente). Proveedor de componentes gráficos e interactivos. |
| **`consultas.sql`** | **4. Evaluación SQL** | Archivo en texto plano con las 8 consultas estandarizadas que alimentan el visor de código del informe y responden las preguntas de negocio. |
| **`taller_1.Rmd`** | **5. Interpretación e Informe** | Documento fuente ejecutable organizado en pestañas interactivas (`Introducción`, `Fuentes`, `Preparación`, `Base SQLite`, `Consultas SQL`, `Visualizaciones`, `Conclusiones`, `Entrega`). |
| **`renv.lock`** | **6. Reproducibilidad** | Registro JSON congelado con las versiones exactas de las librerías R empleadas (`tidyverse`, `RSQLite`, `chromote`, `DT`, `plotly`, `rmarkdown`). |

---

## 4. Instrucciones de Reproducción y Ejecución

El proyecto está diseñado para funcionar tanto en entornos con acceso completo a internet como en auditorías **offline** o con restricciones de red.

### A. Restauración del Entorno R (Recomendado)
Antes de ejecutar los scripts, restaura las versiones fijadas en el proyecto abriendo R en el directorio del taller:
```r
renv::restore()
```

### B. Modo Evaluador / Tejido del Informe (`taller_1.Rmd` → `taller_1.html`)
Para tejer el informe final sin necesidad de re-ejecutar la extracción web viva de 30 minutos, el documento lee automáticamente el archivo auditado `df_resultado_final.rds` o se conecta a `comparador_precios.sqlite`:
* **Desde RStudio:** Abre `taller_1.Rmd` y haz clic en el botón **Knit**.
* **Desde la Consola de R:**
  ```r
  rmarkdown::render("taller_1.Rmd")
  ```
El resultado genera `taller_1.html`, un reporte autocontenido con gráficos interactivos, tablas con filtrado dinámico y visor de código.

### C. Modo Extracción Completa (Ejecución del Scraper Independiente)
Si deseas ejecutar la recolección de datos desde cero, ejecuta el módulo `scraper.R` de forma desacoplada:
* **Desde la Terminal:**
  ```bash
  Rscript scraper.R
  ```
* **Comportamiento:** `scraper.R` detecta los comercios objetivos, genera las copias crudas en `raw/`, inserta los registros en `comparador_precios.sqlite` mediante `INSERT OR IGNORE` y actualiza la tabla `ejecuciones`.

---

## 5. Resumen de la Base de Datos y Resultados Clave

* **Comercios Analizados:** 4 cadenas (Éxito, Carulla, PriceSmart y Supermercado Comunal).
* **Volumen Recolectado:** 768 observaciones totales únicas en la ejecución de referencia.
* **Canasta Homologada:** 36 presentaciones comparables comunes identificadas simultáneamente (`comparable = 1`).
* **Calidad de Datos:** 100% de efectividad en precios válidos (> \$0) y 0% de nulos en variables estructurales.
* **Hallazgo Principal:** Se identificaron brechas de precio relativas de hasta **99%** entre comercios para productos idénticos. La opción de mayor eficiencia en costo unitario corresponde a **Agua Cristal 6000 ml en Carulla** con **\$94 COP/Litro**.

---

## 6. Declaración de Cumplimiento de Entrega

- [x] **Inmutabilidad y Aislamiento:** El Pull Request modifica exclusivamente archivos dentro de la carpeta individual `laura_katherine_martinez_castiblanco/taller_01/`.
- [x] **Sin Restricciones de Tamaño:** Ningún archivo del repositorio excede los 100 MB establecidos por GitHub.
- [x] **Seguridad y Credenciales:** No se incluyen contraseñas, claves ni tokens de acceso en el código.
- [x] **Archivos Autocontenidos:** El HTML resultante incluye hoja de estilo, librerías JS y recursos gráficos para visualización offline.


