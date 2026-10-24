# Una sola comprobacion offline: no consulta internet ni modifica la base principal.
if (!file.exists('scraper.R') || !dir.exists('raw')) {
  stop('Abre RStudio en la carpeta Taller_Mineria_R_Reinicio. No encuento scraper.R y raw/.')
}
source('scraper.R')
cat('1/4 Cargando respuestas VTEX historicas...\n')
id <- '20261009_205507'
leer_vtex_historico <- function(tienda) {
  archivos <- sort(list.files(file.path('raw',tienda,id),pattern='[.]gz$',full.names=TRUE))
  if (!length(archivos)) stop('Faltan copias historicas: ',tienda)
  piezas <- lapply(archivos,function(p) {
    texto <- leer_gzip(p)
    ps <- productos_vtex(texto)
    if (is.null(ps)) stop('JSON VTEX invalido: ',p)
    list(brutos=length(ps),datos=extraer_vtex(texto,tienda,id,'historico:',p,fecha_historica(id)))
  })
  list(brutos=sum(vapply(piezas,function(x)x$brutos,integer(1))),
       datos=unir_filas(lapply(piezas,function(x)x$datos)))
}
ca <- leer_vtex_historico('Cafam')
lo <- leer_vtex_historico('Locatel')
cat('Cafam:',ca$brutos,'brutos,',nrow(ca$datos),'clasificados\n')
cat('Locatel:',lo$brutos,'brutos,',nrow(lo$datos),'clasificados\n')
stopifnot(ca$brutos==60L,lo$brutos==91L,
          nrow(ca$datos)>0,nrow(lo$datos)>0,
          all(is.na(ca$datos$precio) | ca$datos$precio>0),
          all(is.na(lo$datos$precio) | lo$datos$precio>0))
cat('2/4 Cargando HTML historico de Cruz Verde...\n')
carpeta <- 'historico/auditoria_cruzverde/20261009_221615'
prueba_cv <- list()
for (sec in c('preservativos','pruebas','lubricantes')) {
  archivo <- file.path(carpeta,paste0(sec,'.html.gz'))
  if (!file.exists(archivo)) stop('Falta HTML historico: ',archivo)
  txt <- leer_gzip(archivo)
  meta <- metadatos_cv(txt)
  datos <- extraer_cruzverde(txt,'20261009_221615',sec,
     unname(fuentes_cruzverde[[sec]]),archivo,'2026-10-09T22:16:15-05:00')
  stopifnot(is.finite(meta$total), meta$tamanio>0, nrow(datos)>0,
            all(is.finite(datos$precio)),all(datos$precio>0))
  prueba_cv[[sec]] <- datos
  cat(sec,':',nrow(datos),'validos;',meta$total,'publicados en esa seccion\n')
}
cat('3/4 Probando SQLite en un archivo TEMPORAL...\n')
tmp <- tempfile(fileext='.sqlite')
con <- abrir_base(tmp)
res <- list(datos=ca$datos[seq_len(min(3L,nrow(ca$datos))),,drop=FALSE],
 cobertura=resultado_cobertura('Cafam','prueba',60L,3L,1L,'parcial','Prueba local'))
guardar_resultado(con,id,'Cafam',res,fecha_historica(id))
query <- DBI::dbGetQuery(con,'SELECT COUNT(*) AS n FROM observaciones')
stopifnot(query$n[[1L]]==nrow(res$datos))
DBI::dbDisconnect(con)
unlink(tmp)
cat('4/4 Verificando la base historica SIN MODIFICARLA...\n')
if (!file.exists('comparador_precios.sqlite')) stop('Falta la base historica')
con <- DBI::dbConnect(RSQLite::SQLite(),'comparador_precios.sqlite')
integridad <- DBI::dbGetQuery(con,'PRAGMA integrity_check')
DBI::dbDisconnect(con)
stopifnot(identical(integridad[[1L]][[1L]],'ok'))
cat('\n=== PRUEBA OFFLINE FINALIZADA SIN ERRORES ===\n')
cat('IMPORTANTE: esto no certifica una extraccion en vivo ni 8 productos simultaneos.\n')
