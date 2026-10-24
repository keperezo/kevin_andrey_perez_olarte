paquetes <- c("httr2", "rvest", "dplyr", "stringr", "purrr", "readr", "tibble")

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

invisible(lapply(paquetes, library, character.only = TRUE))

# 2. Armamos las URLs de cada mes (2024 y 2025) --------------
# AccuWeather tiene una página por mes con el histórico, así
# que solo hay que ir cambiando el nombre del mes y el año en
# la URL.
nombres_meses <- c("january", "february", "march", "april", "may", "june",
                   "july", "august", "september", "october", "november", "december")

tabla_urls <- expand.grid(num_mes = 1:12, anio = c(2024, 2025)) %>%
  arrange(anio, num_mes) %>%
  mutate(
    nombre_mes = nombres_meses[num_mes],
    link = sprintf(
      "https://www.accuweather.com/es/co/bogota/107487/%s-weather/107487?year=%d",
      nombre_mes, anio
    )
  )

# 3. Función para sacar los días de UN mes ya descargado -----
# Ojo: el calendario mensual de AccuWeather mete días "de
# relleno" del mes anterior y del siguiente para completar la
# grilla (ej. últimos días de mayo aparecen en la vista de
# junio). Para no confundirnos, seguimos la secuencia de
# números de día: cuando el número "se devuelve" (de 31 pasa
# a 1), ahí empieza un bloque de mes nuevo. Nos quedamos con
# el bloque que tenga 25 días o más, que es el mes real.
sacar_dias_del_mes <- function(html_pagina, num_mes, anio, link_origen) {
  paneles_dia <- html_pagina %>% html_elements(".monthly-daypanel")
  if (length(paneles_dia) == 0) return(NULL)
  
  info_bruta <- map_dfr(seq_along(paneles_dia), function(i) {
    panel <- paneles_dia[[i]]
    
    texto_dia <- panel %>% html_element(".date") %>% html_text2() %>% str_squish()
    numero_dia <- suppressWarnings(as.integer(texto_dia))
    
    temp_max <- panel %>% html_element(".high") %>% html_text2() %>%
      str_replace_all("[^0-9\\-]", "") %>% as.numeric()
    temp_min <- panel %>% html_element(".low") %>% html_text2() %>%
      str_replace_all("[^0-9\\-]", "") %>% as.numeric()
    
    tibble(posicion = i, dia = numero_dia, high = temp_max, low = temp_min)
  })
  
  if (all(is.na(info_bruta$dia))) return(NULL)
  
  info_bruta <- info_bruta %>%
    mutate(
      diferencia   = dia - lag(dia, default = dplyr::first(dia)),
      empieza_mes  = diferencia < 0
    ) %>%
    mutate(bloque = cumsum(empieza_mes) + 1)
  
  bloque_bueno <- info_bruta %>%
    count(bloque) %>%
    filter(n >= 25) %>%
    pull(bloque)
  
  info_bruta %>%
    filter(bloque %in% bloque_bueno) %>%
    mutate(
      # 4. Pasamos la fecha a tipo Date y las temps a numérico
      fecha       = as.Date(sprintf("%d-%02d-%02d", anio, num_mes, dia)),
      high        = as.numeric(high),
      low         = as.numeric(low),
      # 5. Guardamos la URL de origen por si toca verificar
      url_origen  = link_origen
    ) %>%
    select(fecha, high, low, url_origen)
}

# 6. Función para descargar y procesar un mes completo -------
descargar_mes <- function(link, num_mes, anio) {
  
  # Pausa entre solicitudes pa' no saturar el servidor
  Sys.sleep(1.5)
  
  respuesta <- tryCatch(
    request(link) %>%
      req_headers(
        `User-Agent` = paste(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "(KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36"
        ),
        `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "es-CO,es;q=0.9,en;q=0.8"
      ) %>%
      req_error(is_error = function(resp) FALSE) %>%  # no queremos que truene solo, lo revisamos abajo
      req_perform(),
    error = function(e) e
  )
  
  if (inherits(respuesta, "error")) {
    message("  [ERROR de conexión] ", link, " -> ", conditionMessage(respuesta))
    return(NULL)
  }
  
  if (resp_status(respuesta) != 200) {
    message("  [HTTP ", resp_status(respuesta), "] ", link)
    return(NULL)
  }
  
  html_pagina <- read_html(resp_body_string(respuesta))
  datos_mes   <- sacar_dias_del_mes(html_pagina, num_mes, anio, link)
  
  if (is.null(datos_mes)) {
    message("  [SIN DATOS] No encontré paneles de días en: ", link)
  }
  
  datos_mes
}

# 7. Corremos el scraping para los 24 meses, controlando errores
cat("Descargando", nrow(tabla_urls), "meses de AccuWeather Bogotá...\n")

descargar_mes_seguro <- safely(descargar_mes)

lista_resultados <- pmap(
  list(tabla_urls$link, tabla_urls$num_mes, tabla_urls$anio),
  function(link, num_mes, anio) {
    cat("Procesando:", anio, "-", num_mes, "\n")
    descargar_mes_seguro(link, num_mes, anio)
  }
)

# Separamos lo que sí funcionó de lo que falló ----------------
meses_ok      <- lista_resultados %>% map("result") %>% compact()
meses_fallidos <- sum(map_lgl(lista_resultados, ~ is.null(.x$result)))

if (meses_fallidos > 0) {
  cat("\n⚠️  OJO:", meses_fallidos, "de", nrow(tabla_urls),
      "meses no se pudieron descargar o no tenían datos.\n",
      "Revisa los mensajes de arriba antes de seguir.\n\n")
}

if (length(meses_ok) == 0) {
  stop(
    "No se pudo sacar NINGÚN dato real de AccuWeather. ",
    "No se genera el CSV con datos inventados: revisa la conexión, los headers, ",
    "o si la página cambió de estructura antes de volver a intentar."
  )
}

datos_completos <- list_rbind(meses_ok) %>%
  arrange(fecha)

# 8. Revisiones de calidad de los datos ------------------------
rango_fechas_esperado <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
fechas_que_faltan     <- setdiff(rango_fechas_esperado, datos_completos$fecha) %>% as.Date(origin = "1970-01-01")

cat("\n--- RESUMEN DE VALIDACIÓN ---\n")
resumen <- datos_completos %>%
  summarise(
    total_filas      = n(),
    fechas_unicas    = n_distinct(fecha),
    duplicados       = sum(duplicated(fecha)),
    faltantes_high   = sum(is.na(high)),
    faltantes_low    = sum(is.na(low)),
    fecha_min        = min(fecha),
    fecha_max        = max(fecha),
    dias_esperados   = length(rango_fechas_esperado),
    dias_faltantes   = length(fechas_que_faltan),
    cobertura_pct    = round(100 * total_filas / length(rango_fechas_esperado), 1)
  )

print(resumen)

if (length(fechas_que_faltan) > 0) {
  cat("\nFechas sin datos (las primeras 10):\n")
  print(head(fechas_que_faltan, 10))
}

if (any(duplicated(datos_completos$fecha))) {
  cat("\n⚠️  Hay fechas duplicadas. Revisando...\n")
  print(datos_completos %>% filter(duplicated(fecha) | duplicated(fecha, fromLast = TRUE)))
}

# 9. Exportamos la tabla final a CSV ---------------------------
tabla_final <- datos_completos %>%
  select(fecha, high, low) %>%
  arrange(fecha)

write_csv(tabla_final, "temperaturas_bogota_2024_2025.csv")
cat("\n✅ Listo, guardé 'temperaturas_bogota_2024_2025.csv'.\n")
cat("   (", nrow(tabla_final), "filas de", length(rango_fechas_esperado), "días esperados,",
    resumen$cobertura_pct, "% de cobertura )\n")

# También guardamos la versión con la URL de origen, por si hay
# que verificar algún dato después
write_csv(datos_completos, "temperaturas_bogota_2024_2025_con_origen.csv")

# Vista previa rápida
mostrar_tabla(tabla_final, n=10, caption = "Tabla de temperaturas")
view(tabla_final)
