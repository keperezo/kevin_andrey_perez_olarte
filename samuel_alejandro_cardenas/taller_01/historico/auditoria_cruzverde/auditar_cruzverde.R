# Auditoria independiente de cobertura de Cruz Verde - Taller Mineria de Datos
# No altera scraper.R ni comparador_precios.sqlite.
# Requiere httr2, rvest, stringr, dplyr, tibble, xml2 (ya usados en el taller).

suppressPackageStartupMessages({
  library(httr2); library(rvest); library(stringr); library(dplyr)
  library(tibble); library(xml2)
})

fecha_co_aud <- function() {
  s <- format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z', tz='America/Bogota')
  sub('([+-][0-9]{2})([0-9]{2})$', '\\1:\\2', s)
}
normal_aud <- function(x) {
  x[is.na(x)] <- ''
  stringr::str_squish(tolower(iconv(x, to='ASCII//TRANSLIT', sub='')))
}
clasifica_aud <- function(x) {
  z <- normal_aud(x)
  dplyr::case_when(
    str_detect(z, 'condon|preservativ') ~ 'Preservativos',
    str_detect(z, 'lubricante|gel k-y|gel ky|gel intimo estimulante') ~ 'Lubricantes sexuales',
    str_detect(z, '(embarazo|gestacion).*(prueba|test)|(?:prueba|test).*(embarazo|gestacion)') ~ 'Pruebas de embarazo',
    str_detect(z, '(?:prueba|test).*ovulacion|ovulacion.*(?:test|digital)|clearblue.*ovulacion') ~ 'Pruebas de ovulacion',
    TRUE ~ NA_character_)
}
excluido_aud <- function(x) str_detect(normal_aud(x),
 'postday|postinor|levonorgestrel|anticonceptivo oral|pastilla anticonceptiva|inyeccion anticonceptiva|implante anticonceptivo|vibrador|jabon intimo|hidratante vaginal|copa menstrual|ducha vaginal')

# URLs comprobadas como secciones publicas; NO se asume que su primera pagina sea completa.
secciones_cv <- c(
  preservativos = 'https://beta1.cruzverde.com.co/salud-sexual/preservativos-y-accesorios/',
  pruebas = 'https://beta1.cruzverde.com.co/salud-sexual/test-de-diagnostico/',
  lubricantes = 'https://beta1.cruzverde.com.co/falabella/medicamentos/saludsexualmujeres/lubricanteehigiene/'
)

leer_gz_seguro <- function(path) {
  con <- gzfile(path, 'rt'); on.exit(close(con), add=TRUE)
  paste(readLines(con, warn=FALSE, encoding='UTF-8'),collapse='\n')
}
guardar_gz_seguro <- function(x,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  con <- gzfile(path,'wt'); on.exit(close(con),add=TRUE)
  writeLines(x,con,useBytes=TRUE)
}

# Extrae pares del JSON escapado presente en atributos HTML. El campo price
# puede representar un precio de referencia: NO se interpreta como precio final
# de compra sin contrastar oferta, Club y descuentos de la ficha.
extraer_pares <- function(html,seccion,fuente,captura) {
  z <- gsub('&quot;', '"', html, fixed=TRUE)
  z <- gsub('&#34;', '"', z, fixed=TRUE)
  patron <- '"name"\\s*:\\s*"([^"<>]+)"\\s*,\\s*"price"\\s*:\\s*([0-9]+)(?:\\s*,\\s*"id"\\s*:\\s*"([^"]+)")?'
  m <- str_match_all(z,patron)[[1]]
  if (!nrow(m)) return(tibble())
  dat <- tibble(seccion_fuente=seccion, comercio='Cruz Verde',
                producto_original=m[,2], precio_html=as.numeric(m[,3]),
                sku=m[,4], url_fuente=fuente, fecha_consulta=captura)
  dat$subcategoria <- clasifica_aud(dat$producto_original)
  dat |> filter(!is.na(subcategoria), !excluido_aud(producto_original),
                is.finite(precio_html),precio_html>0) |>
    distinct(producto_original,precio_html,sku,.keep_all=TRUE)
}

# Indicadores de paginacion: no se inventan URLs por ?page=.
examinar_paginacion <- function(html,url) {
  doc <- read_html(html)
  a <- html_elements(doc,'a[href]')
  href <- html_attr(a,'href'); texto <- trimws(html_text2(a))
  idx <- which(grepl('page|pagina|pagin|offset|start|from|cursor', href, ignore.case=TRUE) |
               grepl('^(2|3|4|siguiente|next)$',texto,ignore.case=TRUE))
  enlaces <- if(length(idx)) unique(xml2::url_absolute(href[idx],url)) else character()
  enlaces <- enlaces[grepl('^https?://',enlaces)]
  enlaces
}

# Ejecuta SOLO esta funcion. Descarga las 3 primeras paginas de seccion conocidas
# y genera archivos CSV nuevos, sin borrar ni insertar observaciones existentes.
auditar_cruzverde <- function() {
  ejec <- format(Sys.time(),'%Y%m%d_%H%M%S',tz='America/Bogota')
  carpeta <- file.path('auditoria_cruzverde',ejec)
  dir.create(carpeta,recursive=TRUE,showWarnings=FALSE)
  piezas <- list(); resumen <- list(); links <- list()
  for (s in names(secciones_cv)) {
    u <- unname(secciones_cv[[s]])
    cat('Consultando:',s,'\n')
    res <- tryCatch({
      resp <- httr2::request(u) |>
        httr2::req_user_agent('Proyecto universitario de mineria de datos - solicitud moderada') |>
        httr2::req_timeout(30) |>
        httr2::req_perform()
      httr2::resp_body_string(resp)
    },error=function(e) {message('No se pudo consultar ',s,': ',conditionMessage(e));NULL})
    if (is.null(res)) {
      resumen[[s]] <- tibble(seccion=s, registros=0L, precio_html=0L,
                             estado='ERROR HTTP - NO COMPLETA',links_paginacion=0L)
      next
    }
    ruta <- file.path(carpeta,paste0(s,'.html.gz'))
    guardar_gz_seguro(res,ruta)
    dat <- extraer_pares(res,s,u,fecha_co_aud())
    pag <- tryCatch(examinar_paginacion(res,u), error=function(e) character())
    piezas[[s]] <- dat
    if(length(pag)) links[[s]] <- tibble(seccion=s,enlace=pag)
    resumen[[s]] <- tibble(seccion=s,registros=nrow(dat),
      precio_html=if(nrow(dat)) sum(!is.na(dat$precio_html)) else 0L,
      estado='PARCIAL - PAGINACION/CATÁLOGO PENDIENTE',links_paginacion=length(pag))
    Sys.sleep(1)
  }
  detalle <- bind_rows(piezas)
  if(nrow(detalle)) {
    detalle <- detalle |>
      arrange(desc(!is.na(sku)),seccion_fuente) |>
      distinct(producto_original,precio_html,.keep_all=TRUE)
  }
  informe <- bind_rows(resumen)
  write.csv2(informe,file.path(carpeta,'resumen.csv'),row.names=FALSE,na='')
  write.csv2(detalle,file.path(carpeta,'productos_detectados.csv'),row.names=FALSE,na='')
  write.csv2(bind_rows(links),file.path(carpeta,'enlaces_paginacion.csv'),row.names=FALSE,na='')
  cat('\nRESULTADO DE AUDITORIA (NO equivale a categoria completa):\n')
  print(informe)
  cat('Productos distintos detectados:',nrow(detalle),'\n')
  cat('Archivos de auditoria:',normalizePath(carpeta,winslash='/'),'\n')
  invisible(list(resumen=informe,productos=detalle,carpeta=carpeta))
}

resultado_auditoria <- auditar_cruzverde()
