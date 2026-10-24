# 04_comparables.R
# Motor de emparejamiento de los 8 productos comparables (lo usan la Fase 1
# para verificar presencia simultánea y la Fase 4 para marcar la base).

# Devuelve todas las observaciones candidatas por producto/alternativa.
#   familia = cumple patrones de producto y variante (cualquier contenido)
#   exacto  = además coincide contenido_num y unidad_medida de la regla
candidatos_comparables <- function(obs) {
  if (nrow(obs) == 0) return(tibble())
  if (!"id_observacion" %in% names(obs)) obs$id_observacion <- seq_len(nrow(obs))
  if (!"disponible" %in% names(obs)) obs$disponible <- NA_integer_
  nombre_n <- normalizar_texto(obs$producto_nombre_original)

  map(REGLAS_COMPARABLES, function(regla) {
    imap(regla$alternativas, function(alt, k) {
      ok <- rep(TRUE, nrow(obs))
      for (p in alt$incluye) ok <- ok & str_detect(nombre_n, regex(p))
      if (nzchar(alt$excluye)) ok <- ok & !str_detect(nombre_n, regex(alt$excluye))
      if (!any(ok)) return(NULL)
      obs[ok, ] |>
        transmute(id_producto_comun = regla$id, descripcion_producto = regla$descripcion,
                  prioridad = k, alternativa = alt$etiqueta,
                  id_observacion, comercio, sku_comercio, producto_nombre_original,
                  marca_estandarizada, contenido_num, unidad_medida, precio_cop, disponible,
                  unidad_canonica = alt$unidades[1],
                  unidades_validas = paste(alt$unidades, collapse = "|"),
                  contenido_objetivo = alt$contenido,
                  exacto = !is.na(contenido_num) & contenido_num == alt$contenido &
                    unidad_medida %in% alt$unidades,
                  orden_marca = coalesce(match(marca_estandarizada, alt$marcas_preferidas), 99L))
    }) |> list_rbind()
  }) |> list_rbind()
}

# Elige, para cada producto, la alternativa + marca presente en TODOS los
# comercios objetivo (prioridad: alternativa -> marca preferida -> cobertura).
# En cada comercio se toma un único SKU: se prefiere uno disponible (no
# agotado) y, entre ellos, el de menor precio (la oferta que elegiría un
# consumidor). Empates de precio -> menor sku_comercio (determinístico).
seleccionar_comparables <- function(cand, comercios_objetivo) {
  n_obj <- length(comercios_objetivo)
  vacio <- list(elegidos = tibble(), familias = tibble(), resumen = tibble())
  if (nrow(cand) == 0) return(vacio)

  res <- map(IDS_COMPARABLES, function(id) {
    ex <- cand |> filter(id_producto_comun == id, exacto, !is.na(marca_estandarizada),
                         comercio %in% comercios_objetivo)
    if (nrow(ex) == 0) {
      return(list(eleccion = tibble(id_producto_comun = id, prioridad = NA_integer_, alternativa = NA_character_,
                                    marca_estandarizada = NA_character_, cobertura = 0L)))
    }

    opciones <- ex |>
      mutate(con_stock = coalesce(as.integer(disponible), 1L) == 1L) |>
      group_by(id_producto_comun, prioridad, alternativa, marca_estandarizada, orden_marca) |>
      summarise(cobertura = n_distinct(comercio),
                comercios_disponibles = n_distinct(comercio[con_stock]), .groups = "drop") |>
      mutate(completa = cobertura >= n_obj) |>
      arrange(desc(completa), prioridad, desc(comercios_disponibles), orden_marca, desc(cobertura))
    eleccion <- opciones |> slice(1)
    elegidos <- ex |>
      semi_join(eleccion, by = c("prioridad", "marca_estandarizada")) |>
      arrange(comercio, coalesce(as.integer(disponible), 1L) == 0L, precio_cop, sku_comercio) |>
      group_by(comercio) |> slice(1) |> ungroup()
    familias <- cand |>
      filter(id_producto_comun == id, comercio %in% comercios_objetivo) |>
      semi_join(eleccion, by = c("prioridad", "marca_estandarizada"))
    list(eleccion = select(eleccion, id_producto_comun, prioridad, alternativa, marca_estandarizada, cobertura),
         elegidos = elegidos, familias = familias)
  })

  resumen <- map(res, "eleccion") |> list_rbind() |>
    mutate(comercios_requeridos = n_obj, completo = cobertura >= n_obj)
  list(elegidos = map(res, "elegidos") |> compact() |> list_rbind(),
       familias = map(res, "familias") |> compact() |> list_rbind(),
       resumen = resumen)
}
