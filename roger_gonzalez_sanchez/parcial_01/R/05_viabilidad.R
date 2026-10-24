# 05_viabilidad.R  -  FASE 1: evaluación y selección de comercios
# Escaneo preliminar por comercio:
#   (a) 1 página de su categoría respiratoria principal -> ofertados vs.
#       extraíbles y completitud de campos clave;
#   (b) 1 página de búsqueda por cada producto comparable -> presencia de los 8
#       productos con la regla estricta (marca + variante + contenido).
# Luego evalúa todas las combinaciones de 3 comercios y recomienda la mejor.
# Las respuestas quedan en raw/viabilidad/{fecha}/{comercio}/ (trazabilidad) y
# el resultado completo en raw/viabilidad/{fecha}/resultado_VIAB_*.rds, que el
# informe puede releer con leer_ultima_viabilidad() sin repetir peticiones.

pct <- function(x) round(100 * mean(x, na.rm = TRUE), 1)

diagnosticar_comercio <- function(clave, id) {
  fn <- FUNCIONES_SCRAPE[[clave]]
  nombre <- COMERCIOS[[clave]]$nombre

  # (a) Una página de la categoría principal
  ctx_cat <- crear_contexto(clave, test_mode = TRUE, id_ejecucion = id, modo_raw = "viabilidad")
  pag <- fn(ctx = ctx_cat)
  res_pag <- attr(pag, "resumen_peticiones")
  ofertados <- res_pag$total_reportado[1]

  # (b) Búsquedas dirigidas de los 8 comparables (1 página por término)
  ctx_bus <- crear_contexto(clave, id_ejecucion = id, modo_raw = "viabilidad")
  bus <- fn(ctx = ctx_bus, categorias = character(0), terminos = TERMINOS_COMPARABLES)
  bus_f <- filtrar_categoria_respiratorios(bus, verbose = FALSE)
  cand <- candidatos_comparables(bus_f)
  cand <- if (nrow(cand)) filter(cand, exacto, !is.na(marca_estandarizada)) else cand

  diag <- tibble(
    clave = clave, comercio = nombre, rol = COMERCIOS[[clave]]$rol,
    plataforma = COMERCIOS[[clave]]$plataforma,
    estado = if (nrow(pag) > 0) "OK" else "SIN DATOS",
    ofertados_categoria = ofertados,
    filas_pagina_1 = nrow(pag),
    extraibles_pagina_1 = sum(pag$aux_extraible),
    pct_extraibles = pct(pag$aux_extraible),
    pct_sku = pct(!is.na(pag$sku_comercio)),
    pct_marca = pct(!is.na(pag$marca_estandarizada)),
    pct_texto_original = pct(!is.na(pag$producto_nombre_original) & str_trim(pag$producto_nombre_original) != ""),
    pct_descripcion = pct(!is.na(pag$descripcion_original) | !is.na(pag$ficha_tecnica_original)),
    pct_contenido_unidad = pct(!is.na(pag$contenido_num) & !is.na(pag$unidad_medida)),
    comparables_presentes = n_distinct(cand$id_producto_comun)
  )
  list(diag = diag, cand = cand)
}

evaluar_viabilidad_comercios <- function(comercios = names(COMERCIOS), n_seleccion = 3,
                                         imprimir = TRUE) {
  id <- nuevo_id_ejecucion("VIAB")
  op <- options(width = max(getOption("width"), 200)); on.exit(options(op), add = TRUE)
  log_msg("===== FASE 1: evaluación de viabilidad (", id, ") =====")
  resultados <- map(set_names(comercios), function(cl) {
    tryCatch(diagnosticar_comercio(cl, id), error = function(e) {
      log_msg("  ! ", cl, ": ", conditionMessage(e))
      list(diag = tibble(clave = cl, comercio = COMERCIOS[[cl]]$nombre,
                         rol = COMERCIOS[[cl]]$rol, plataforma = COMERCIOS[[cl]]$plataforma,
                         estado = paste("ERROR:", conditionMessage(e))),
           cand = tibble())
    })
  })
  diag <- map(resultados, "diag") |> list_rbind()
  cand <- map(resultados, "cand") |> list_rbind()

  # Matriz de presencia producto x comercio (con la marca encontrada)
  presencia <- expand.grid(id_producto_comun = IDS_COMPARABLES, comercio = diag$comercio,
                           stringsAsFactors = FALSE) |> as_tibble()
  if (nrow(cand) > 0) {
    hallados <- cand |> group_by(id_producto_comun, comercio) |>
      summarise(marcas = paste(sort(unique(marca_estandarizada)), collapse = "/"), .groups = "drop")
    presencia <- presencia |> left_join(hallados, by = c("id_producto_comun", "comercio"))
  } else presencia$marcas <- NA_character_
  matriz <- presencia |>
    mutate(valor = if_else(is.na(marcas), "--", paste0("SI (", marcas, ")"))) |>
    select(-marcas) |>
    tidyr_pivot(id_producto_comun, comercio, valor)

  # Evaluación de combinaciones de n_seleccion comercios
  ok <- diag$comercio[diag$estado == "OK"]
  combinaciones <- tibble()
  if (length(ok) >= n_seleccion) {
    combinaciones <- combn(ok, n_seleccion, simplify = FALSE) |>
      map(function(trio) {
        sel <- seleccionar_comparables(cand, trio)
        d <- filter(diag, comercio %in% trio)
        tibble(comercios = paste(trio, collapse = " + "),
               productos_simultaneos = sum(sel$resumen$completo),
               obs_comparables = sum(sel$resumen$completo) * n_seleccion,
               n_respaldo = sum(d$rol == "respaldo"),
               completitud_media = round(mean(c(d$pct_sku, d$pct_marca, d$pct_texto_original,
                                                d$pct_descripcion, d$pct_contenido_unidad)), 1),
               pct_extraibles_medio = round(mean(d$pct_extraibles), 1),
               ofertados_total = sum(d$ofertados_categoria, na.rm = TRUE))
      }) |> list_rbind() |>

      arrange(desc(productos_simultaneos), n_respaldo, desc(completitud_media), desc(pct_extraibles_medio))
  }
  recomendados <- if (nrow(combinaciones)) str_split(combinaciones$comercios[1], " \\+ ")[[1]] else character(0)
  claves_rec <- diag$clave[match(recomendados, diag$comercio)]

  resultado <- list(id_viabilidad = id, fecha_hora = ahora_iso_co(), entorno = entorno_actual(),
                    diagnostico = diag, presencia = matriz, combinaciones = combinaciones,
                    recomendados = claves_rec, candidatos = cand)
  dir_res <- file.path(DIR_RAW_VIAB, fecha_hoy_co())
  dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)
  saveRDS(resultado, file.path(dir_res, paste0("resultado_", id, ".rds")))
  if (!imprimir) return(invisible(resultado))

  cat("\n================ FASE 1 - DIAGNÓSTICO POR COMERCIO (1 página) ================\n")
  print(as.data.frame(select(diag, -clave, -plataforma)), row.names = FALSE)
  cat("\n=========== PRESENCIA DE LOS 8 PRODUCTOS COMPARABLES (regla estricta) ===========\n")
  print(as.data.frame(matriz), row.names = FALSE)
  if (nrow(combinaciones)) {
    cat("\n================ COMBINACIONES DE", n_seleccion, "COMERCIOS (mejor primero) ================\n")
    print(as.data.frame(combinaciones), row.names = FALSE)
    cat("\n>>> Comercios recomendados:", paste(recomendados, collapse = ", "),
        sprintf("(%d/8 productos presentes simultáneamente; %d observaciones comparables)\n",
                combinaciones$productos_simultaneos[1], combinaciones$obs_comparables[1]))
    cat(">>> Para usarlos: COMERCIOS_SELECCIONADOS <- c(", paste0('"', claves_rec, '"', collapse = ", "), ")\n\n")
  }
  invisible(resultado)
}

# Último resultado guardado de la Fase 1 (NULL si nunca se ha ejecutado).
leer_ultima_viabilidad <- function() {
  rds <- list.files(DIR_RAW_VIAB, pattern = "^resultado_VIAB_.*\\.rds$",
                    recursive = TRUE, full.names = TRUE)
  if (length(rds) == 0) return(NULL)
  readRDS(rds[order(basename(rds))][length(rds)])
}

# Pivot mínimo (evita depender de tidyr): filas = id, columnas = comercio.
tidyr_pivot <- function(df, fila, columna, valor) {
  f <- rlang::as_name(rlang::enquo(fila)); c <- rlang::as_name(rlang::enquo(columna))
  v <- rlang::as_name(rlang::enquo(valor))
  cols <- unique(df[[c]])
  out <- tibble(!!f := unique(df[[f]]))
  for (cc in cols) {
    sub <- df[df[[c]] == cc, ]
    out[[cc]] <- sub[[v]][match(out[[f]], sub[[f]])]
  }
  out
}
