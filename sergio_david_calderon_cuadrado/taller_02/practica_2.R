# paquetes -------------------------------------
paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr", "chromote"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  tryCatch(
    install.packages(pendientes),
    error = function(e) stop(
      "No fue posible instalar: ", paste(pendientes, collapse = ", "),
      ". Solicite apoyo al docente. Detalle: ", conditionMessage(e)
    )
  )
}

# library("chromote")
if (requireNamespace("rvest", quietly = TRUE) && packageVersion("rvest") < "1.0.5") {
  install.packages("rvest")
}

invisible(lapply(paquetes, library, character.only = TRUE))

diagnostico <- tibble(
  elemento = c("Sistema operativo", "Versión de R", "Versión de rvest", "Versión de chromote"),
  valor = c(
    Sys.info()[["sysname"]],
    R.version.string,
    as.character(packageVersion("rvest")),
    as.character(packageVersion("chromote"))
  )
)

# mostrar_tabla(diagnostico, n = nrow(diagnostico), caption = "Entorno de ejecución")

if (packageVersion("rvest") < "1.0.5") {
  stop("Se necesita rvest 1.0.5 o posterior. Ejecute install.packages('rvest') y reinicie RStudio.")
}

Sys.setenv(CHROMOTE_CHROME = "/usr/bin/brave-browser")
chromote::chromote_info()


chrome_disponible <- tryCatch({
  chromote::chromote_info()
  TRUE
}, error = function(e) FALSE)

chrome_disponible

# solo una consulta -------------------------------------

# pagina inicio
# https://www.exito.com/s?q=cerveza+michelob&sort=score_desc&page=0
# https://www.carulla.com/s?q=cerveza&sort=score_desc&page=0

termino_busqueda <- c("cerveza")
urls_cerveza <- paste0(
  # "https://www.exito.com/s?q=",termino_busqueda,"&sort=score_desc&page=",0)
  # "https://www.carulla.com/s?q=",termino_busqueda,"&sort=score_desc&page=",0)
  "https://www.jumbocolombia.com/search?page=",1,"&query=",termino_busqueda,"&type=term")

# Primer intento: leer la página directamente
pagina_simple <- tryCatch(
  read_html_live(urls_cerveza),
  error = function(e) NULL
)
if (is.null(pagina_simple)) {
  "La lectura directa falló, pero el taller puede continuar."
} else {
  pagina_simple
}

# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# selector

# productos = nodos
productos <- pagina_simple %>%
  # html_elements("div.product-grid_fs-product-grid___qKN2 > ul > li")
  html_elements("div.tiendasjumboqaio-custom-search-0-x-products__container > div.tiendasjumboqaio-custom-search-0-x-product__container")
length(productos)

# (EXITO Y CARULLA)
# nombres <- productos %>%
#   html_element("article div.productCard_productInfo__yn2lK a h3") %>%
#   html_text2()

# precios <- productos %>%
#   html_element("article div.productCard_productInfo__yn2lK div.ProductPrice_container__JKbri") %>%
# html_text2()

# (JUMBO)
nombres <- productos %>%
  html_attr("data-cnstrc-item-name")

precios <- productos %>%
  html_attr("data-cnstrc-item-price")

tibble(
  nombres = nombres,
  precios = precios
)

# Mostramos el objeto HTML obtenido
pagina_simple

# Extraer campos básicos ---------------------------------------------------------
# Mantiene la estructura esperada cuando una página no devuelve productos.
tabla_productos_vacia <- function() {
  tibble(
    nombres = character(),
    precios = integer()
  )
}

# Definimos una función que recibe el nodo HTML de un producto ---------------------------------

# (EXITO Y CARULLA)
# extraer_producto_cerveza <- function(nodo) {
#   nombres <- nodo %>%
#     html_element("article div.productCard_productInfo__yn2lK a h3") %>%
#     html_text2()
#   precios <- nodo %>%
#     html_element("article div.productCard_productInfo__yn2lK div.ProductPrice_container__JKbri") %>%
#     html_text2()
#   tibble(
#     nombres = nombres,
#     precios = precios
#   )
# }

# (JUMBO)
extraer_producto_cerveza <- function(nodo) {
  nombres <- nodo %>% html_attr("data-cnstrc-item-name")
  precios <- nodo %>% html_attr("data-cnstrc-item-price")
  tibble(
    nombres = nombres,
    precios = precios
  )
}

# Construir una tabla de resultados ------------------------------------------------------
tabla_productos <- if (length(productos) == 0) {
  tabla_productos_vacia() %>% select(-pagina)
} else {
  map_dfr(productos, extraer_producto_cerveza) %>% clean_names()
}

if (nrow(tabla_productos) == 0) {
  message("No se encontraron productos. Revise el estado HTTP, un posible bloqueo o los selectores.")
}
tabla_productos

# Limpieza de datos ------------------------------------------------
tabla_productos_limpia <- tabla_productos %>%
  mutate(
    # Tipo
    tipo = str_extract(nombres, "^[^ ]+"),
    
    # Cantidad
    cantidad = as.numeric(
      str_match(nombres, "\\(([0-9]+)\\s+[^)]+\\)")[, 2]
    ),
    
    # Unidad
    unidad = str_match(
      nombres,
      "\\([0-9]+\\s+([^)]+)\\)"
    )[, 2],
    
    # Texto sin tipo ni cantidad/unidad
    texto = nombres %>%
      str_remove("^\\S+\\s+") %>%
      str_remove("\\s*\\([^)]*\\)\\s*$"),
    
    # Presentación: última parte del texto
    presentacion = str_extract(
      texto,
      "(?i)(six\\s*pack|internacional|botella|Ultra\\s+Nrb\\s+X6|Ultra\\s+Nrb\\s+Und)$"
    ),
    
    # Nombre: lo que queda después de quitar la presentación
    nombre = str_trim(
      str_remove(
        texto,
        regex(
          paste0(
            "\\s*",
            str_replace_all(
              presentacion,
              "([.|()\\[\\]{}*+?^$\\\\])",
              "\\\\\\1"
            ),
            "$"
          ),
          ignore_case = TRUE
        )
      )
    ),
    
    # Precio
    precio = as.numeric(
      str_remove_all(precios, "[.$,]")
    )
  ) %>%
  select(tipo, nombre, presentacion, cantidad, unidad, precio) %>%
  filter(!is.na(nombre), nombre != "")
tabla_productos_limpia

# resumen de NA -------------------------
resumen_na <- tibble(
  variable = names(tabla_productos_limpia),
  n_na = sapply(tabla_productos_limpia, function(x) sum(is.na(x)))
)

# funcion de creacion de url ---------------------------------
construir_url_cerveza <- function(termino, pagina = 0) {
  paste0(
    # "https://www.exito.com/s?q=",termino,"&sort=score_desc&page=",pagina)
    # "https://www.carulla.com/s?q=",termino_busqueda,"&sort=score_desc&page=",pagina)
    "https://www.jumbocolombia.com/search?page=",pagina,"&query=",termino_busqueda,"&type=term")
}

construir_url_cerveza("cerveza", 0)
construir_url_cerveza("cerveza", 1)


# -----------------------------------------------------------------------
# (EXITO Y CARULLA)
# Abrir una página dinámica
if (!chrome_disponible) {
  stop("Chrome no fue detectado. Revise la sección de solución de problemas.")
}

Sys.getenv("CHROMOTE_CHROME")
file.exists(Sys.getenv("CHROMOTE_CHROME"))

url_busqueda <- construir_url_cerveza("cerveza", 0)
pagina_viva <- rvest::read_html_live(url_busqueda)
pagina_viva$view()

nodos_renderizados <- pagina_viva %>%
  html_elements("div.product-grid_fs-product-grid___qKN2 > ul > li")

length(nodos_renderizados)

# Scroll dinámico
for (i in 1:5) {
  pagina_viva$scroll_by(top = 1200)
  Sys.sleep(2)
}

nodos_scroll <- pagina_viva %>%
  html_elements("div.product-grid_fs-product-grid___qKN2 > ul > li")

productos_scroll <- if (length(nodos_scroll) == 0) {
  tabla_productos_vacia() %>% select(-pagina)
} else {
  map_dfr(nodos_scroll, extraer_producto_cerveza)
}
productos_scroll

# Paginación con navegador dinámico ------------------------------------------
# (EXITO Y CARULLA)
scrapear_pagina_live <- function(termino, pagina = 0, pausa = 4) {
  sesion <- rvest::read_html_live(construir_url_cerveza(termino, pagina))
  on.exit(sesion$session$close(), add = TRUE)
  
  Sys.sleep(pausa)
  sesion$scroll_by(top = 1200)
  Sys.sleep(2)
  
  nodos <- sesion %>%
    html_elements("div.product-grid_fs-product-grid___qKN2 > ul > li")
  
  if (length(nodos) == 0) {
    return(tabla_productos_vacia())
  }
  
  map_dfr(nodos, extraer_producto_cerveza) %>% mutate(pagina = pagina)
}

exito_dinamico <- map_dfr(0:10, ~ scrapear_pagina_live("cerveza", pagina = .x))
exito_dinamico
dim(exito_dinamico)

# Manejo de errores ------------------------------------------
scrapear_pagina_segura <- purrr::possibly(
  scrapear_pagina_live,
  otherwise = tabla_productos_vacia()
)

resultado_seguro <- 
  map_dfr(0:10, ~scrapear_pagina_segura("cerveza", .x, pausa = 2))
dim(resultado_seguro)

# -------------------------------------------------------
## definitivo}

# (JUMBO)
termino_busqueda <- c("cerveza")
construir_url_cerveza <- function(termino, pagina = 0) {
  paste0(
    # "https://www.exito.com/s?q=",termino,"&sort=score_desc&page=",pagina)
    # "https://www.carulla.com/s?q=",termino_busqueda,"&sort=score_desc&page=",pagina)
    "https://www.jumbocolombia.com/search?page=",pagina,"&query=",termino_busqueda,"&type=term")
}

extraer_producto_cerveza <- function(nodo) {
  nombres <- nodo %>% html_attr("data-cnstrc-item-name")
  precios <- nodo %>% html_attr("data-cnstrc-item-price")
  tibble(
    nombres = nombres,
    precios = precios
  )
}

if (!chrome_disponible) {
  stop("Chrome no fue detectado. Revise la sección de solución de problemas.")
}
Sys.getenv("CHROMOTE_CHROME")
file.exists(Sys.getenv("CHROMOTE_CHROME"))

# (EXITO Y CARULLA)
# productos <- pagina_simple %>%
#   html_elements("div.product-grid_fs-product-grid___qKN2 > ul > li")
# length(productos)
#
# nombres <- productos %>%
#   html_element("article div.productCard_productInfo__yn2lK a h3") %>%
#   html_text2()
#
# precios <- productos %>%
#   html_element("article div.productCard_productInfo__yn2lK div.ProductPrice_container__JKbri") %>%
# html_text2()

# (JUMBO)
productos <- pagina_simple %>%
  html_elements("div.tiendasjumboqaio-custom-search-0-x-products__container > div.tiendasjumboqaio-custom-search-0-x-product__container")
length(productos)

nombres <- productos %>%
  html_attr("data-cnstrc-item-name")

precios <- productos %>%
  html_attr("data-cnstrc-item-price")

# (EXITO Y CARULLA)
scrapear_pagina_live <- function(termino, pagina = 0, pausa = 4) {
  sesion <- rvest::read_html_live(construir_url_cerveza(termino, pagina))
  on.exit(sesion$session$close(), add = TRUE)
  
  Sys.sleep(pausa)
  sesion$scroll_by(top = 1200)
  Sys.sleep(2)
  
  nodos <- sesion %>%
    html_elements("div.product-grid_fs-product-grid___qKN2 > ul > li")
    # html_elements("div.tiendasjumboqaio-custom-search-0-x-products__container > div.tiendasjumboqaio-custom-search-0-x-product__container")
  
  if (length(nodos) == 0) {
    return(tabla_productos_vacia())
  }
  
  if (length(nodos) == 0) {
    datos <- tabla_productos_vacia()
  } else {
    datos <- map_dfr(nodos, extraer_producto_cerveza) %>%mutate(pagina = pagina)
  }
  
  boton_siguiente <- sesion %>%
    html_elements('button[aria-label="Próxima Pagina"]')

  boton_siguiente <- sesion %>%
    html_elements("select.tiendasjumboqaio-custom-search-0-x-pagination__selector")

  if (length(boton_siguiente) == 0) { hay_siguiente <- FALSE} else {
    # Revisamos si el botón está deshabilitado
    disabled <- html_attr(boton_siguiente,"disabled")
    hay_siguiente <- is.na(disabled)
  }
  list(datos = datos, hay_siguiente = hay_siguiente)
}

scrapear_pagina_segura <- purrr::possibly(
  scrapear_pagina_live,
  otherwise = list(datos = tabla_productos_vacia()) # , hay_siguiente = FALSE
)

scrapear_todas_las_paginas <- function(termino, pausa = 2, max_paginas = 100) {
  pagina <- 0
  resultados <- list()
  repeat {
    if (pagina >= max_paginas) {
      warning("Se alcanzó el límite de ", max_paginas, " páginas.")
      break
    }
    resultado <- scrapear_pagina_segura(
      termino = termino,
      pagina = pagina,
      pausa = pausa
    )
 
    if (nrow(resultado$datos) > 0) {
      resultados[[length(resultados) + 1]] <- resultado$datos }
    
    if (!resultado$hay_siguiente) {
      cat("\nÚltima página alcanzada:", pagina,"\n")
      break
    }
    pagina <- pagina + 1
  }

  if (length(resultados) == 0) { # UNIR TODAS LAS PÁGINAS
    return( tabla_productos_vacia() )
  }
  bind_rows(resultados)
}

resultado_seguro <- scrapear_todas_las_paginas(termino = "cerveza",pausa = 2)
dim(resultado_seguro)
head(resultado_seguro)
table(resultado_seguro$pagina) # saber cuantas cervezas hay en cada pagina

# getwd()
setwd("/media/dave7177/Nuevo vol/1 Full Dave/SUBJECTS/X-CURRENT-2026-2/Mineria/Codigo/Practica_2")
# Guardar resultados
# dir.create("resultados", showWarnings = FALSE)
# ruta_salida <- file.path("resultados", "exito_cerveza_resultados.csv")
# ruta_salida <- file.path("resultados", "carulla_cerveza_resultados.csv")
ruta_salida <- file.path("resultados", "jumbo_cerveza_resultados.csv")
write_csv(resultado_seguro, ruta_salida)
ruta_salida

# (JUMBO)
# la estructura de la pagina de Jumbo es distinta por eso se debe hacer esta seccion
scrapear_pagina_live <- function(termino, pagina = 0, pausa = 4) {
  sesion <- rvest::read_html_live(construir_url_cerveza(termino, pagina))
  on.exit(sesion$session$close(), add = TRUE)
  
  Sys.sleep(pausa)
  sesion$scroll_by(top = 1200)
  Sys.sleep(2)
  
  nodos <- sesion %>%
    html_elements(
      "div.tiendasjumboqaio-custom-search-0-x-products__container > div.tiendasjumboqaio-custom-search-0-x-product__container"
    )
  
  if (length(nodos) == 0) {
    datos <- tabla_productos_vacia()
  } else {
    datos <- map_dfr(nodos, extraer_producto_cerveza) %>% mutate(pagina = pagina + 1)
  }
  
  # El <select> contiene todas las páginas disponibles.
  selector_paginas <- sesion %>% html_element('select[aria-label="Seleccionar página"]')
  
  if (length(selector_paginas) == 0 || is.na(selector_paginas)) {
    total_paginas <- 1
  } else {
    paginas <- selector_paginas %>%
      html_elements("option") %>%
      html_attr("value") %>%
      as.numeric()
    
    total_paginas <- max(paginas, na.rm = TRUE)
  }
  
  list(
    datos = datos,
    total_paginas = total_paginas
  )
}

scrapear_pagina_segura <- purrr::possibly(
  scrapear_pagina_live,
  otherwise = list(
    datos = tabla_productos_vacia(),
    total_paginas = NA_integer_
  )
)

scrapear_todas_las_paginas <- function(termino, pausa = 2, max_paginas = 100) {
  pagina <- 0
  resultados <- list()
  total_paginas <- NULL
  
  repeat {
    if (pagina >= max_paginas) {
      warning("Se alcanzó el límite de ", max_paginas, " páginas.")
      break
    }
    
    cat("\nScrapeando página", pagina + 1, "...\n")
    
    resultado <- scrapear_pagina_segura(
      termino = termino,
      pagina = pagina,
      pausa = pausa
    )
    
    # Guardamos el total de páginas encontrado
    if (!is.na(resultado$total_paginas)) {
      total_paginas <- resultado$total_paginas
    }
    
    # Guardamos los productos
    if (nrow(resultado$datos) > 0) {resultados[[length(resultados) + 1]] <- resultado$datos}
    
    # Si ya llegamos a la última página, terminamos
    if (!is.null(total_paginas) && pagina + 1 >= total_paginas) {
      cat("\nÚltima página alcanzada:", pagina + 1, "de",  total_paginas,"\n")
      break
    }
    pagina <- pagina + 1
  }
  if (length(resultados) == 0) { return(tabla_productos_vacia()) }
  bind_rows(resultados)
}

resultado_seguro <- scrapear_todas_las_paginas( termino = "cerveza", pausa = 2)
dim(resultado_seguro)
head(resultado_seguro)
table(resultado_seguro$pagina) 

### CONSOLIDADA BASE ------------------------------------------------------
# consolidar toda la informacion en una sola tabla
library(readr)
carulla <- read_csv("resultados/carulla_cerveza_resultados.csv")
exito <- read_csv("resultados/exito_cerveza_resultados.csv")
jumbo <- read_csv("resultados/jumbo_cerveza_resultados.csv")
names(carulla)

# normalizar nombres
carulla <- within(carulla,{
  nombres <- str_to_lower(nombres)
  nombres <- str_to_title(nombres)
})

exito <- within(exito,{
  nombres <- str_to_lower(nombres)
  nombres <- str_to_title(nombres)
})

jumbo <- within(jumbo,{
  nombres <- str_to_lower(nombres)
  nombres <- str_to_title(nombres)
})

# buscar las palabras clave
marcas <- c("Michelob","Stella Artois", "Club Colombia")

exito <- exito %>% filter(str_detect(nombres, paste(marcas, collapse = "|"))) %>% mutate(tienda="Exito")
carulla <- carulla %>% filter(str_detect(nombres, paste(marcas, collapse = "|"))) %>% mutate(tienda="Carulla")
jumbo <- jumbo %>% filter(str_detect(nombres, paste(marcas, collapse = "|"))) %>% mutate(tienda="Jumbo")

exito$precios <- exito$precios %>% str_remove_all("[^0-9]") %>% as.numeric()
carulla$precios <- carulla$precios %>% str_remove_all("[^0-9]") %>% as.numeric()
jumbo$precios <- jumbo$precios %>% as.numeric()

base <- rbind(exito, carulla, jumbo) # concadenar base

library(dplyr)
library(stringr)

# exito %>% filter(str_detect(nombres, "Club Colombia")) 

# Ejercicios

# 1
resultado <- base %>% group_by(tienda) %>% #exito %>% 
  mutate(
    marca = case_when(
      str_detect(nombres, regex("Michelob", ignore_case = TRUE)) ~ "Michelob",
      str_detect(nombres, regex("Stella Artois", ignore_case = TRUE)) ~ "Stella Artois",
      str_detect(nombres, regex("Club Colombia", ignore_case = TRUE)) ~ "Club Colombia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(marca)) %>%
  group_by(tienda, marca) %>%
  summarise(
    precio_promedio = mean(precios, na.rm = TRUE),
    n_productos = n(),
    .groups = "drop"
  ) %>% 
  arrange((precio_promedio))
resultado

# 2
base <- base %>%
  mutate(
    
    # ---------------------------------------------
    # LIMPIAR NOMBRE
    # ---------------------------------------------
    nombre_limpio = nombres %>%
      str_replace_all("[()]", " ") %>%
      str_squish(),
    
    
    # ---------------------------------------------
    # ML POR UNIDAD
    # ---------------------------------------------
    ml_unidad = as.numeric(
      str_extract(
        nombre_limpio,
        regex("\\d+(?=\\s*ml\\b)", ignore_case = TRUE)
      )
    ),
    
    
    # ---------------------------------------------
    # CANTIDAD DE UNIDADES
    # ---------------------------------------------
    unidades = case_when(
      
      # -------------------------------------------
      # SIXPACK / SIX PACK
      # -------------------------------------------
      str_detect(
        nombre_limpio,
        regex("\\bsix\\s*-?\\s*pack\\b", ignore_case = TRUE)
      ) ~ 6,
      
      
      # -------------------------------------------
      # X6 / X 6
      # X6und / X 6 unds / X6 unidades
      # -------------------------------------------
      str_detect(
        nombre_limpio,
        regex(
          "\\bx\\s*([2-9]|[12][0-9]|3[0-6])\\s*(?:und(?:s)?|unidades?)?\\b",
          ignore_case = TRUE
        )
      ) ~ as.numeric(
        str_match(
          nombre_limpio,
          regex(
            "\\bx\\s*([2-9]|[12][0-9]|3[0-6])\\s*(?:und(?:s)?|unidades?)?\\b",
            ignore_case = TRUE
          )
        )[, 2]
      ),
      
      
      # -------------------------------------------
      # 6PACK / 6 PACK / 6-PACK
      # -------------------------------------------
      str_detect(
        nombre_limpio,
        regex(
          "\\b([2-9]|[12][0-9]|3[0-6])\\s*-?\\s*pack\\b",
          ignore_case = TRUE
        )
      ) ~ as.numeric(
        str_match(
          nombre_limpio,
          regex(
            "\\b([2-9]|[12][0-9]|3[0-6])\\s*-?\\s*pack\\b",
            ignore_case = TRUE
          )
        )[, 2]
      ),
      
      
      # -------------------------------------------
      # "6 UND", "6 UNDS", "6 UNIDADES"
      # -------------------------------------------
      str_detect(
        nombre_limpio,
        regex(
          "\\b([2-9]|[12][0-9]|3[0-6])\\s*(?:und(?:s)?|unidades?)\\b",
          ignore_case = TRUE
        )
      ) ~ as.numeric(
        str_match(
          nombre_limpio,
          regex(
            "\\b([2-9]|[12][0-9]|3[0-6])\\s*(?:und(?:s)?|unidades?)\\b",
            ignore_case = TRUE
          )
        )[, 2]
      ),
      
      
      # -------------------------------------------
      # CUALQUIER OTRO CASO = 1 UNIDAD
      # -------------------------------------------
      TRUE ~ 1
    ),
    
    
    # ---------------------------------------------
    # ML TOTAL
    # ---------------------------------------------
    ml_total = unidades * ml_unidad,
    
    
    # ---------------------------------------------
    # PRECIO POR ML
    # ---------------------------------------------
    precio_ml = precios / ml_total
  )

# 3
resultado_2 <- base %>% select(-nombre_limpio) %>% group_by(tienda) %>% #exito %>% 
  group_by(tienda) %>%
  summarise(
    precio_promedio_ml = mean(precio_ml, na.rm = TRUE),
    n_productos = n(),
    .groups = "drop"
  ) %>% 
  arrange((precio_promedio_ml))
resultado_2

# 4
resultado_3 <- base %>% group_by(tienda) %>% #exito %>% 
  mutate(
    marca = case_when(
      str_detect(nombres, regex("Michelob", ignore_case = TRUE)) ~ "Michelob",
      str_detect(nombres, regex("Stella Artois", ignore_case = TRUE)) ~ "Stella Artois",
      str_detect(nombres, regex("Club Colombia", ignore_case = TRUE)) ~ "Club Colombia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(marca)) %>%
  group_by(tienda, marca) %>%
  summarise(
    precio_promedio_ml = mean(precio_ml, na.rm = TRUE),
    n_productos = n(),
    .groups = "drop"
  ) %>% 
  arrange((precio_promedio_ml))
resultado_3

# 5
base_comparacion <- base %>%
  mutate(
    tipo_envase = case_when(
      str_detect(nombres, regex("lata", ignore_case = TRUE)) ~ "Lata",
      str_detect(nombres, regex("botella", ignore_case = TRUE)) ~ "Botella",
      TRUE ~ "Otro"
    )
  )

base_comparacion2 <- base_comparacion %>%
  mutate(
    presentacion = case_when(
      tipo_envase == "Lata" & unidades == 1 ~ "Lata individual",
      tipo_envase == "Botella" & unidades == 1 ~ "Botella individual",
      unidades == 6 ~ "Sixpack",
      TRUE ~ "Otra"
    )
  )

resultado_4 <- base_comparacion2 %>% group_by(tienda) %>% #exito %>% 
  mutate(
    marca = case_when(
      str_detect(nombres, regex("Michelob", ignore_case = TRUE)) ~ "Michelob",
      str_detect(nombres, regex("Stella Artois", ignore_case = TRUE)) ~ "Stella Artois",
      str_detect(nombres, regex("Club Colombia", ignore_case = TRUE)) ~ "Club Colombia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(marca)) %>%
  group_by(marca, tipo_envase, presentacion, tienda) %>%
  summarise(
    precio_promedio_ml = mean(precio_ml, na.rm = TRUE),
    n_productos = n(),
    .groups = "drop"
  ) %>% 
  arrange(precio_promedio_ml) # tipo_envase, presentacion, marca, tienda, 
resultado_4
