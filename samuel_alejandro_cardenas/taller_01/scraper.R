# MINERIA DE DATOS - SALUD SEXUAL NO FARMACOLOGICA
# Modulo de funciones: cargar con source('scraper.R') 

paquetes <- c('jsonlite','rvest','xml2','httr2','DBI','RSQLite')
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)]
if (length(faltantes)) stop('Instala estos paquetes: ', paste(faltantes, collapse=', '))

`%o%` <- function(x, default) {
  if (is.null(x) || length(x) == 0L) default else x
}
texto1 <- function(x) {
  if (is.null(x) || length(x) == 0L || is.list(x)) return(NA_character_)
  z <- as.character(x[[1L]])
  if (is.na(z) || !nzchar(trimws(z))) NA_character_ else z
}
numero1 <- function(x) {
  if (is.null(x) || !length(x) || is.list(x)) return(NA_real_)
  suppressWarnings(as.numeric(x[[1L]]))
}
fecha_colombia <- function() {
  raw <- format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z', tz='America/Bogota')
  sub('([+-][0-9]{2})([0-9]{2})$', '\\1:\\2', raw)
}
normalizar <- function(x) {
  if (is.na(x) || !nzchar(x)) return('')
  z <- iconv(tolower(x), to='ASCII//TRANSLIT', sub='')
  if (is.na(z)) return('')
  trimws(gsub(' +', ' ', z))
}
subcategoria_producto <- function(nombre) {
  x <- normalizar(nombre)
  if (!nzchar(x)) return(NA_character_)
  excluir <- 'postday|postinor|levonorgestrel|anticonceptiv|etonogestrel|desogestrel|vibrador|juguete|mascara|mascarilla|copa menstrual|ducha vaginal|jabon intimo|hidratante vaginal|sildenafil|potenciador sexual'
  if (grepl(excluir, x)) return(NA_character_)
  if (grepl('ovulacion|ovulation', x)) return('Pruebas de ovulacion')
  if (grepl('embarazo|gestacion|pregnancy', x) && grepl('prueba|test|kit|detector|clearblue',x)) return('Pruebas de embarazo')

  if (grepl('preservativ|condon', x)) return('Preservativos')
  if (grepl('lubricante|gel k-y|gel ky|durex play|lubricacion sexual', x)) return('Lubricantes sexuales')
  if (grepl('triple pleasure|real sensation|long action|real feel|placer prolongado|extra seguro|punto g|invisible',x)) return('Preservativos')
  NA_character_
}
marca_producto <- function(nombre, marca_sitio=NA_character_) {
  x <- normalizar(nombre)
  patrones <- c(today='today',durex='durex',duo='\\bduo\\b',uniq='uniq|unique',
               'K-Y'='k-y|\\bky\\b|k.y',clearblue='clearblue',profamilia='profamilia',
               unitest='unitest',piel='\\bpiel\\b',elixir='elixir',prudence='prudence')
  for (i in seq_along(patrones)) {
    if (grepl(patrones[[i]], x, perl=TRUE)) return(names(patrones)[[i]])
  }
  texto1(marca_sitio)
}
contenido_producto <- function(nombre, subcat) {
  x <- normalizar(nombre)
  if (is.na(subcat)) return(list(cantidad=NA_real_,unidad=NA_character_))
  if (subcat == 'Lubricantes sexuales') {
    m <- regmatches(x, regexec('(\\d+(?:[.,]\\d+)?)\\s*(ml|mililitros|mg|g|gr|gramos)\\b', x, perl=TRUE))[[1]]
    if (length(m) < 3L) return(list(cantidad=NA_real_,unidad=NA_character_))
    n <- as.numeric(sub(',', '.', m[[2]], fixed=TRUE))
    un <- switch(m[[3]], ml='mL', mililitros='mL', mg='mg', g='g',gr='g',gramos='g')
    return(list(cantidad=n, unidad=un))
  }
  m <- regmatches(x, regexec('pague\\s*\\d+\\s*lleve\\s*(\\d+)',x,perl=TRUE))[[1]]
  if (length(m)>1L) return(list(cantidad=as.numeric(m[[2]]),unidad='unidad'))
  patrones <- c('(?:x|por|caja(?:\\s+con)?|paquete(?:\\s+de)?|blister(?:\\s+x)?|bli(?:\\s+por)?)\\s*(\\d{1,3})',
               '(\\d{1,3})\\s*(?:unds?|unidades|sobres?|condones|pruebas|tests?)\\b')
  for (pat in patrones) {
    m <- regmatches(x,regexec(pat,x,perl=TRUE))[[1]]
    if (length(m)>1L) return(list(cantidad=as.numeric(m[[2]]),unidad='unidad'))
  }
  list(cantidad=NA_real_,unidad='unidad')
}
columnas <- c('comercio','sku','producto_original','descripcion_original','marca',
 'subcategoria','precio','precio_anterior','promocion','url','fecha_consulta',
 'id_ejecucion','cantidad','unidad','id_comun','comparable','precio_club','seccion_fuente','origen_raw')
fila_vacia <- function() {
  data.frame(comercio=character(),sku=character(),producto_original=character(),
   descripcion_original=character(),marca=character(),subcategoria=character(),
   precio=double(),precio_anterior=double(),promocion=integer(),url=character(),
   fecha_consulta=character(),id_ejecucion=character(),cantidad=double(),unidad=character(),
   id_comun=character(),comparable=integer(),precio_club=double(),
   seccion_fuente=character(),origen_raw=character(),stringsAsFactors=FALSE)
}
unir_filas <- function(partes) {
  partes <- Filter(function(x) is.data.frame(x) && nrow(x)>0L, partes)
  if (!length(partes)) return(fila_vacia())
  out <- do.call(rbind, partes)
  rownames(out) <- NULL
  out
}
leer_gzip <- function(archivo) {
  con <- gzfile(archivo,open='rt',encoding='UTF-8')
  on.exit(close(con),add=TRUE)
  paste(readLines(con,warn=FALSE),collapse='\n')
}
guardar_gzip <- function(texto,archivo) {
  dir.create(dirname(archivo),recursive=TRUE,showWarnings=FALSE)
  con <- gzfile(archivo,open='wt',encoding='UTF-8')
  on.exit(close(con),add=TRUE)
  writeLines(texto,con,useBytes=TRUE)
  invisible(archivo)
}
obtener_texto <- function(url,archivo,offline=FALSE) {
  if (offline) {
    if (!file.exists(archivo)) stop('Falta archivo raw: ',archivo)
    return(leer_gzip(archivo))
  }
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req,'Ejercicio universitario de precios - consultas espaciadas')
  req <- httr2::req_timeout(req,35)
  req <- httr2::req_retry(req,max_tries=2)
  txt <- httr2::resp_body_string(httr2::req_perform(req))
  guardar_gzip(txt,archivo)
  Sys.sleep(0.8)
  txt
}


productos_vtex <- function(texto) {
  x <- tryCatch(jsonlite::fromJSON(texto,simplifyVector=FALSE),error=function(e) NULL)
  if (!is.list(x) || !is.null(names(x))) return(NULL)
  if (length(x) && !all(vapply(x,function(p) is.list(p) && !is.null(p$productName) && is.list(p$items),logical(1)))) return(NULL)
  x
}
extraer_vtex <- function(texto,comercio,ejecucion,url_fuente,archivo,momento) {
  prods <- productos_vtex(texto)
  if (is.null(prods)) stop('La respuesta no es una lista VTEX de productos')
  filas <- list()
  for (p in prods) {
    nombre <- texto1(p$productName)
    if (is.na(nombre)) next
    subcat <- subcategoria_producto(nombre)
    if (is.na(subcat)) next
    sku_items <- p$items
    for (art in sku_items) {
      if (!is.list(art)) next
      sku <- texto1(art$itemId)
      if (is.na(sku)) next
      vendedores <- art$sellers %o% list()
      oferta <- if (is.list(vendedores) && length(vendedores) && is.list(vendedores[[1]])) vendedores[[1]]$commertialOffer else NULL
      if (!is.list(oferta)) oferta <- list()
      precio <- numero1(oferta$Price)
      anterior <- numero1(oferta$ListPrice)
      if (!is.finite(precio) || precio<=0) precio <- NA_real_
      if (!is.finite(anterior) || is.na(precio) || anterior<=precio) anterior <- NA_real_
      contenido <- contenido_producto(paste(nombre,texto1(art$name)),subcat)
      url_p <- texto1(p$link)
      if (is.na(url_p)) url_p <- url_fuente
      descripcion <- texto1(p$description)
      if (is.na(descripcion)) descripcion <- nombre
      filas[[length(filas)+1L]] <- data.frame(comercio=comercio,sku=sku,
        producto_original=nombre,descripcion_original=descripcion,
        marca=marca_producto(nombre,p$brand),subcategoria=subcat,
        precio=precio,precio_anterior=anterior,promocion=as.integer(!is.na(anterior)),
        url=url_p,fecha_consulta=momento,id_ejecucion=ejecucion,
        cantidad=contenido$cantidad,unidad=contenido$unidad,
        id_comun=NA_character_,comparable=0L,precio_club=NA_real_,
        seccion_fuente='catalogo_vtex',origen_raw=archivo,stringsAsFactors=FALSE)
    }
  }
  z <- unir_filas(filas)
  if (nrow(z)) z <- z[!duplicated(z$sku),,drop=FALSE]
  z
}


seleccionar_texto <- function(nodo,css) {
  a <- rvest::html_element(nodo,css)
  if (inherits(a,'xml_missing') || !length(a)) return(NA_character_)
  texto1(rvest::html_text2(a))
}
precio_cop <- function(x) {
  if (is.na(x)) return(NA_real_)
  v <- gsub('[^0-9]','',x)
  if (!nzchar(v)) return(NA_real_)
  as.numeric(v)
}
extraer_cruzverde <- function(texto,ejecucion,seccion,url_fuente,archivo,momento) {
  doc <- rvest::read_html(texto)
  tarjetas <- rvest::html_elements(doc,'.product-tile')
  filas <- list()
  for (card in tarjetas) {
    gtm <- rvest::html_attr(card,'data-gtm')
    if (is.na(gtm)) next
    info <- tryCatch(jsonlite::fromJSON(gtm,simplifyVector=FALSE),error=function(e) NULL)
    if (!is.list(info) || !is.list(info$ecommerce) || !is.list(info$ecommerce$impressions)) next
    item <- info$ecommerce$impressions
    nombre <- texto1(item$name); sku <- texto1(item$id)
    if (is.na(nombre) || is.na(sku)) next
    subcat <- subcategoria_producto(nombre)
    if (is.na(subcat)) next
    precio_gtm <- numero1(item$price)
    sales <- precio_cop(seleccionar_texto(card,'.price .sales .value'))
    precio_lista <- rvest::html_element(card,'.price .list .value')
    anterior <- if (inherits(precio_lista,'xml_missing')) NA_real_ else numero1(rvest::html_attr(precio_lista,'content'))
    club <- length(rvest::html_elements(card,'.promo-price-club'))>0L
    if (club) {
      precio <- if (is.finite(precio_gtm) && precio_gtm>0) precio_gtm else anterior
      precio_club <- sales
      anterior_valido <- NA_real_
    } else {
      precio <- if (is.finite(sales) && sales>0) sales else precio_gtm
      precio_club <- NA_real_
      anterior_valido <- if (is.finite(anterior) && is.finite(precio) && anterior>precio) anterior else NA_real_
    }
    if (!is.finite(precio) || precio<=0) next
    link <- rvest::html_element(card,'a[href]')
    href <- if (inherits(link,'xml_missing')) NA_character_ else rvest::html_attr(link,'href')
    enlace <- if (is.na(href)) url_fuente else xml2::url_absolute(href,url_fuente)
    contenido <- contenido_producto(nombre,subcat)
    filas[[length(filas)+1L]] <- data.frame(comercio='Cruz Verde',sku=sku,
      producto_original=nombre,descripcion_original=rvest::html_text2(card),
      marca=marca_producto(nombre),subcategoria=subcat,precio=precio,
      precio_anterior=anterior_valido,promocion=as.integer(!is.na(anterior_valido)),
      url=enlace,fecha_consulta=momento,id_ejecucion=ejecucion,
      cantidad=contenido$cantidad,unidad=contenido$unidad,
      id_comun=NA_character_,comparable=0L,precio_club=precio_club,
      seccion_fuente=seccion,origen_raw=archivo,stringsAsFactors=FALSE)
  }
  z <- unir_filas(filas)
  if (nrow(z)) z <- z[!duplicated(z$sku),,drop=FALSE]
  z
}
metadatos_cv <- function(texto) {
  doc <- rvest::read_html(texto)
  nodo <- rvest::html_element(doc,'.js-pagination-data')
  if (inherits(nodo,'xml_missing')) stop('No aparece .js-pagination-data en Cruz Verde')
  total <- suppressWarnings(as.integer(as.numeric(rvest::html_attr(nodo,'data-count'))))
  tam <- suppressWarnings(as.integer(as.numeric(rvest::html_attr(nodo,'data-page-size'))))
  endpoint <- rvest::html_attr(nodo,'data-default-url')
  if (!is.finite(total) || !is.finite(tam) || tam<1 || is.na(endpoint)) stop('Paginacion de Cruz Verde incompleta')
  list(total=total,tamanio=tam,endpoint=endpoint)
}
url_pagina_cv <- function(endpoint,inicio,tamanio) {
  base <- sub('\\?.*','',endpoint)
  cgid <- sub('.*[?&]cgid=([^&]+).*','\\1',endpoint)
  if (identical(cgid,endpoint) || !nzchar(cgid)) stop('No se encontro cgid de Cruz Verde')
  paste0(base,'?cgid=',cgid,'&start=',inicio,'&sz=',tamanio)
}


fuentes_vtex <- list(
 Cafam=list(host='https://www.drogueriascafam.com.co',ruta='cuidado-personal/salud-sexual'),
 Locatel=list(host='https://www.locatelcolombia.com',ruta='cuidado-personal/bienestar-intimo'))
fuentes_cruzverde <- c(
 preservativos='https://beta1.cruzverde.com.co/salud-sexual/preservativos-y-accesorios/',
 pruebas='https://beta1.cruzverde.com.co/salud-sexual/test-de-diagnostico/',
 lubricantes='https://beta1.cruzverde.com.co/falabella/medicamentos/saludsexualmujeres/lubricanteehigiene/')
resultado_cobertura <- function(comercio,seccion,esperados,capturados,paginas,estado,detalle) {
  data.frame(comercio=comercio,seccion=seccion,esperados=as.integer(esperados),
   capturados=as.integer(capturados),paginas=as.integer(paginas),
   estado=estado,detalle=detalle,stringsAsFactors=FALSE)
}

extraer_tienda_vtex <- function(comercio,ejecucion,offline=FALSE,max_paginas=20L) {
  conf <- fuentes_vtex[[comercio]]
  if (is.null(conf)) stop('Comercio VTEX desconocido')
  partes <- list(); ids_paginas <- character(); visitadas <- 0L
  estado <- 'parcial'; detalle <- 'No se pudo confirmar final del catalogo'; total_bruto <- 0L
  for (i in seq_len(max_paginas)) {
    archivo <- file.path('raw',comercio,ejecucion,sprintf('pagina_%03d.txt.gz',i))
    if (offline && !file.exists(archivo)) {detalle <- paste('No existe',archivo);break}
    desde <- (i-1L)*50L
    url <- paste0(conf$host,'/api/catalog_system/pub/products/search/',conf$ruta,
                  '?_from=',desde,'&_to=',desde+49L)
    txt <- tryCatch(obtener_texto(url,archivo,offline),error=function(e) {detalle <<- conditionMessage(e);NULL})
    if (is.null(txt)) break
    productos <- productos_vtex(txt)
    if (is.null(productos)) {detalle <- paste('Respuesta no valida en pagina',i);break}
    if (!length(productos)) {estado <- 'ruta_recorrida';detalle <- 'Respuesta vacia final';break}
    firma <- paste(vapply(productos,function(p) texto1(p$productId),character(1)),collapse='|')
    if (firma %in% ids_paginas) {detalle <- paste('Pagina repetida',i);break}
    ids_paginas <- c(ids_paginas,firma);visitadas <- visitadas+1L
    total_bruto <- total_bruto+length(productos)
    ahora <- if (offline) fecha_historica(ejecucion) else fecha_colombia()
    partes[[length(partes)+1L]] <- extraer_vtex(txt,comercio,ejecucion,url,archivo,ahora)
    if (length(productos)<50L) {estado <- 'ruta_recorrida';detalle <- 'Ultima pagina con menos de 50 productos';break}
  }
  dat <- unir_filas(partes)
  if (nrow(dat)) dat <- dat[!duplicated(dat$sku),,drop=FALSE]
  cobertura <- resultado_cobertura(comercio,'categoria_vtex',total_bruto,nrow(dat),visitadas,estado,
    paste0(detalle,'; bruto=',total_bruto,'; filtrados=',total_bruto-nrow(dat)))
  list(datos=dat,cobertura=cobertura)
}
fecha_historica <- function(ejecucion) {
  if (!grepl('^[0-9]{8}_[0-9]{6}$',ejecucion)) return(NA_character_)
  paste0(substr(ejecucion,1,4),'-',substr(ejecucion,5,6),'-',substr(ejecucion,7,8),'T',
         substr(ejecucion,10,11),':',substr(ejecucion,12,13),':',substr(ejecucion,14,15),'-05:00')
}
extraer_tienda_cruzverde <- function(ejecucion,offline=FALSE,max_paginas=20L) {
  partes <- list(); resumen <- list()
  for (seccion in names(fuentes_cruzverde)) {
    inicio_url <- unname(fuentes_cruzverde[[seccion]])
    archivo <- file.path('raw','CruzVerde',ejecucion,paste0(seccion,'_001.txt.gz'))
    detalle <- 'Sin respuesta'; visitadas <- 0L; brutos <- 0L; cat_datos <- list()
    primera <- tryCatch(obtener_texto(inicio_url,archivo,offline),error=function(e) {detalle <<- conditionMessage(e);NULL})
    if (is.null(primera)) {
      resumen[[seccion]] <- resultado_cobertura('Cruz Verde',seccion,NA_integer_,0L,0L,'fallida',detalle)
      next
    }
    meta <- tryCatch(metadatos_cv(primera),error=function(e) {detalle <<- conditionMessage(e);NULL})
    if (is.null(meta)) {
      resumen[[seccion]] <- resultado_cobertura('Cruz Verde',seccion,NA_integer_,0L,0L,'parcial',detalle)
      next
    }
    cuantos <- max(1L,as.integer(ceiling(meta$total/meta$tamanio)))
    firmas <- character(); detalle <- 'Fin de paginas esperado'
    for (i in seq_len(min(cuantos,max_paginas))) {
      nomarchivo <- file.path('raw','CruzVerde',ejecucion,sprintf('%s_%03d.txt.gz',seccion,i))
      url <- if (i==1L) inicio_url else url_pagina_cv(meta$endpoint,(i-1L)*meta$tamanio,meta$tamanio)
      contenido <- if (i==1L) primera else tryCatch(obtener_texto(url,nomarchivo,offline),
                 error=function(e) {detalle <<- conditionMessage(e);NULL})
      if (is.null(contenido)) break
      doc <- rvest::read_html(contenido)
      tarjetas <- rvest::html_elements(doc,'.product-tile')
      lista_skus <- vapply(tarjetas,function(c) {
        texto1(rvest::html_attr(c,'data-gtm'))
      },character(1))
      firma <- paste(lista_skus,collapse='|')
      if (firma %in% firmas) {detalle <- paste('Pagina repetida',i);break}
      firmas <- c(firmas,firma);brutos <- brutos+length(tarjetas); visitadas <- visitadas+1L
      ahora <- if (offline) fecha_historica(ejecucion) else fecha_colombia()
      cat_datos[[length(cat_datos)+1L]] <- extraer_cruzverde(contenido,ejecucion,seccion,url,nomarchivo,ahora)
    }
    filtrado <- unir_filas(cat_datos)
    if (nrow(filtrado)) filtrado <- filtrado[!duplicated(filtrado$sku),,drop=FALSE]
    partes[[seccion]] <- filtrado
    estado <- if (brutos>=meta$total && visitadas>=cuantos && identical(detalle,'Fin de paginas esperado')) 'ruta_recorrida' else 'parcial'
    resumen[[seccion]] <- resultado_cobertura('Cruz Verde',seccion,meta$total,nrow(filtrado),visitadas,estado,
                     paste0(detalle,'; tarjetas=',brutos,'; filtrados=',brutos-nrow(filtrado)))
  }
  dat <- unir_filas(partes)
  if (nrow(dat)) dat <- dat[!duplicated(dat$sku),,drop=FALSE]
  list(datos=dat,cobertura=unir_filas(resumen))
}


abrir_base <- function(archivo='comparador_precios.sqlite') {
  con <- DBI::dbConnect(RSQLite::SQLite(),archivo)
  DBI::dbExecute(con,'CREATE TABLE IF NOT EXISTS ejecuciones (id_ejecucion TEXT, comercio TEXT, inicio TEXT, registros INTEGER, estado TEXT, detalle TEXT, PRIMARY KEY(id_ejecucion,comercio))')
  DBI::dbExecute(con,'CREATE TABLE IF NOT EXISTS observaciones (id INTEGER PRIMARY KEY AUTOINCREMENT, comercio TEXT, sku TEXT, producto_original TEXT, descripcion_original TEXT, marca TEXT, subcategoria TEXT, precio REAL, precio_anterior REAL, promocion INTEGER, url TEXT, fecha_consulta TEXT, id_ejecucion TEXT, cantidad REAL, unidad TEXT, id_comun TEXT, comparable INTEGER, precio_club REAL, seccion_fuente TEXT, origen_raw TEXT, UNIQUE(id_ejecucion,comercio,sku))')
  DBI::dbExecute(con,'CREATE TABLE IF NOT EXISTS cobertura (id_ejecucion TEXT, comercio TEXT, seccion TEXT, esperados INTEGER, capturados INTEGER, paginas INTEGER, estado TEXT, detalle TEXT, PRIMARY KEY(id_ejecucion,comercio,seccion))')
  con
}
mapa_equivalencias <- function() {
  p <- 'config/equivalencias_revisar.csv'
  if (!file.exists(p)) return(data.frame(comercio=character(),sku=character(),id_comun=character()))
  z <- read.csv(p,stringsAsFactors=FALSE,colClasses='character')
  z[,c('comercio','sku','id_comun'),drop=FALSE]
}
guardar_resultado <- function(con,ejecucion,comercio,resultado,momento) {
  datos <- resultado$datos
  if (nrow(datos)) {
    mapa <- mapa_equivalencias()
    ids <- match(paste(datos$comercio,datos$sku),paste(mapa$comercio,mapa$sku))
    datos$id_comun <- mapa$id_comun[ids]
    datos$comparable <- 0L # SOLO se activa tras revision formal de equivalencias.
  }
  sql_obs <- paste0('INSERT OR IGNORE INTO observaciones (',paste(columnas,collapse=','),
    ') VALUES (',paste(rep('?',length(columnas)),collapse=','),')')
  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(datos))) {
      fila <- unname(as.list(datos[i,columnas,drop=FALSE]))
      DBI::dbExecute(con,sql_obs,params=fila)
    }
    cov <- resultado$cobertura
    for (i in seq_len(nrow(cov))) {
      fila <- list(ejecucion,cov$comercio[[i]],cov$seccion[[i]],cov$esperados[[i]],
                   cov$capturados[[i]],cov$paginas[[i]],cov$estado[[i]],cov$detalle[[i]])
      DBI::dbExecute(con,'INSERT OR REPLACE INTO cobertura VALUES (?,?,?,?,?,?,?,?)',params=unname(fila))
    }
    estado <- if (!nrow(datos)) 'fallida' else if (nrow(cov)>0L && all(cov$estado=='ruta_recorrida')) 'rutas_recorridas' else 'parcial'
    DBI::dbExecute(con,'INSERT OR REPLACE INTO ejecuciones VALUES (?,?,?,?,?,?)',
       params=unname(list(ejecucion,comercio,momento,nrow(datos),estado,
         paste(paste(cov$seccion,cov$estado,sep=':'),collapse='; '))))
  })
  invisible(nrow(datos))
}
recolectar_todo <- function(db='comparador_precios.sqlite') {
  id <- format(Sys.time(),'%Y%m%d_%H%M%S',tz='America/Bogota')
  con <- abrir_base(db)
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  nombres <- c('Cafam','Locatel','Cruz Verde')
  for (tienda in nombres) {
    momento <- fecha_colombia()
    cat('Consultando',tienda,'...\n')
    resultado <- tryCatch(if(tienda=='Cruz Verde') extraer_tienda_cruzverde(id) else extraer_tienda_vtex(tienda,id),
       error=function(e) {
         msg <- conditionMessage(e)
         list(datos=fila_vacia(),cobertura=resultado_cobertura(tienda,'error',NA_integer_,0L,0L,'fallida',msg))
       })
    guardar_resultado(con,id,tienda,resultado,momento)
    cat(tienda,':',nrow(resultado$datos),'registros; cobertura',
       paste(resultado$cobertura$estado,collapse=','),'\n')
  }
  id
}


# -----------------------------------------------------------------------------
# PROCEDIMIENTO EN VIVO (antes: extraer_en_vivo.R)
# -----------------------------------------------------------------------------
extraer_en_vivo <- function(db='comparador_precios.sqlite') {
  if (!file.exists(db)) stop('No existe la base SQLite. Revise el directorio de trabajo.')
  dir.create('respaldos',recursive=TRUE,showWarnings=FALSE)
  respaldo <- file.path('respaldos',paste0('sqlite_antes_',
    format(Sys.time(),'%Y%m%d_%H%M%S'),'.sqlite'))
  if (!file.copy(db,respaldo,overwrite=FALSE)) {
    stop('No se pudo respaldar SQLite; no se inicia el scraping.')
  }
  cat('Respaldo previo:',respaldo,'\n')
  id <- recolectar_todo(db=db)
  con <- DBI::dbConnect(RSQLite::SQLite(),db)
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  cat('\n=== EJECUCIONES ===\n')
  ejecuciones <- DBI::dbGetQuery(con,
    'SELECT * FROM ejecuciones WHERE id_ejecucion=?', params=list(id))
  print(ejecuciones,row.names=FALSE)
  cat('\n=== COBERTURA POR FUENTE ===\n')
  cobertura <- DBI::dbGetQuery(con,
    'SELECT * FROM cobertura WHERE id_ejecucion=?',params=list(id))
  print(cobertura,row.names=FALSE)
  cat('\n=== CANDIDATOS COMUNES (PENDIENTES DE VALIDACION) ===\n')
  candidatos <- DBI::dbGetQuery(con,
    paste('SELECT id_comun,COUNT(DISTINCT comercio) AS comercios',
          'FROM observaciones WHERE id_ejecucion=?',
          'AND id_comun IS NOT NULL AND precio>0',
          'GROUP BY id_comun ORDER BY comercios DESC,id_comun'),
    params=list(id))
  print(candidatos,row.names=FALSE)
  cat('\nLa coincidencia por SKU es candidata hasta validar marca, variante y contenido.\n')
  invisible(list(id_ejecucion=id,ejecuciones=ejecuciones,cobertura=cobertura,
                 candidatos=candidatos,respaldo=respaldo))
}


validar_ultima_ejecucion <- function(db = 'comparador_precios.sqlite') {
  if (!file.exists(db)) stop('No se encuentra la base SQLite en esta carpeta')
  ruta_mapa <- 'config/equivalencias_revisar.csv'
  if (!file.exists(ruta_mapa)) stop('Falta el mapa de SKU: ',ruta_mapa)
  if (!requireNamespace('DBI', quietly=TRUE) || !requireNamespace('RSQLite', quietly=TRUE)) {
    stop('Necesitas DBI y RSQLite')
  }
  mapa <- read.csv(ruta_mapa, stringsAsFactors=FALSE, colClasses='character')
  if (!identical(sort(names(mapa)),sort(c('comercio','sku','id_comun'))) ||
      nrow(mapa)!=24L || anyDuplicated(mapa[c('comercio','sku')])) {
    stop('El mapa debe contener exactamente 24 parejas (comercio, SKU) distintas')
  }
  esperado <- c(
    durex_placer_prolongado_3=3, durex_real_feel_3=3,
    ky_lubricante_100_g=100, ky_lubricante_50_g=50,
    today_long_action_6=6, today_real_sensation_6=6,
    today_triple_pleasure_6=6, today_triple_pleasure_8=8
  )
  if (!setequal(unique(mapa$id_comun),names(esperado))) stop('ID comunes desconocidos')
  # Identificar la ultima ejecucion y cerrar la conexion antes del respaldo.
  con <- DBI::dbConnect(RSQLite::SQLite(),db)
  ultima <- tryCatch(
    DBI::dbGetQuery(con,'SELECT MAX(id_ejecucion) AS id FROM ejecuciones')$id[[1]],
    finally=DBI::dbDisconnect(con)
  )
  if (is.na(ultima)) stop('No hay ninguna ejecucion registrada')
  message('Validando ejecucion: ',ultima)
  backup_dir <- 'respaldos'
  dir.create(backup_dir,showWarnings=FALSE)
  destino <- file.path(backup_dir,paste0('antes_validar_',ultima,'_',format(Sys.time(),'%Y%m%d_%H%M%S'),'.sqlite'))
  if (!file.copy(db,destino,overwrite=FALSE)) stop('No se pudo realizar el respaldo: validacion cancelada')
  con <- DBI::dbConnect(RSQLite::SQLite(),db)
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  notas <- c(
    durex_placer_prolongado_3='Durex Placer Prolongado; preservativos caja x3; lubricante especial no es subcategoria.',
    durex_real_feel_3='Durex Real Feel; preservativos caja x3.',
    ky_lubricante_100_g='Gel lubricante K-Y; presentacion de 100 g.',
    ky_lubricante_50_g='Gel lubricante K-Y; presentacion de 50 g.',
    today_long_action_6='Preservativos Today Long Action; 6 unidades.',
    today_real_sensation_6='Preservativos Today Real Sensation; 6 unidades; error de escritura Sentation en Cafam.',
    today_triple_pleasure_6='Preservativos Today Triple Pleasure/Triple Placer; 6 unidades.',
    today_triple_pleasure_8='Today Triple Pleasure pague 6 lleve 8; contenido efectivo 8 unidades.'
  )
  DBI::dbWithTransaction(con, {
    # Corregir unicamente DOS campos normalizados de dos referencias,
    # comprobadas por su nombre y SKU. Precio, URL, texto y fecha no cambian.
    for (sku in c('COCV_573040','COCV_43163')) {
      DBI::dbExecute(con,
        "UPDATE observaciones SET subcategoria='Preservativos', cantidad=3, unidad='unidad' WHERE id_ejecucion=? AND comercio='Cruz Verde' AND sku=?",
        params=list(ultima,sku))
    }
    obs <- DBI::dbGetQuery(con,
      'SELECT comercio,sku,id_comun,marca,cantidad,unidad,precio FROM observaciones WHERE id_ejecucion=?',
      params=list(ultima))
    pares <- merge(mapa,obs,by=c('comercio','sku'),all.x=TRUE)
    if (nrow(pares)!=24L || anyNA(pares$precio) || any(pares$precio<=0)) {
      stop('No existen las 24 observaciones con precio en esta ejecucion')
    }
    if (anyNA(pares$id_comun.y) || any(pares$id_comun.x != pares$id_comun.y)) {
      stop('Los SKU de esta captura no coinciden con la correspondencia documentada')
    }
    esperado_cantidad <- unname(esperado[pares$id_comun.x])
    esperado_unidad <- ifelse(grepl('^ky_',pares$id_comun.x),'g','unidad')
    esperado_marca <- ifelse(grepl('^ky_',pares$id_comun.x),'k-y',
                       ifelse(grepl('^durex_',pares$id_comun.x),'durex','today'))
    if (anyNA(pares$cantidad) || any(pares$cantidad!=esperado_cantidad) ||
        anyNA(pares$unidad) || any(pares$unidad!=esperado_unidad) ||
        anyNA(pares$marca) || any(tolower(pares$marca)!=esperado_marca)) {
      stop('Marca, contenido o unidad no coinciden: revisar antes de validar')
    }
    frecuencia <- table(pares$id_comun.x)
    if (length(frecuencia)!=8L || any(frecuencia!=3L)) stop('Faltan ternas de comercio')
    DBI::dbExecute(con,
      'CREATE TABLE IF NOT EXISTS validaciones_comparables (id_ejecucion TEXT NOT NULL,id_comun TEXT NOT NULL,metodo TEXT NOT NULL,justificacion TEXT NOT NULL,fecha_revision TEXT NOT NULL,PRIMARY KEY (id_ejecucion,id_comun))')
    for (id in names(esperado)) {
      DBI::dbExecute(con,'INSERT OR REPLACE INTO validaciones_comparables VALUES (?,?,?,?,?)',
        params=list(ultima,id,'Revision marca, variante, SKU y contenido',unname(notas[id]),
                    format(Sys.Date(),'%Y-%m-%d')))
    }
    for (i in seq_len(nrow(mapa))) {
      DBI::dbExecute(con,
        'UPDATE observaciones SET comparable=1 WHERE id_ejecucion=? AND comercio=? AND sku=? AND id_comun=?',
        params=list(ultima,mapa$comercio[[i]],mapa$sku[[i]],mapa$id_comun[[i]]))
    }
  })
  resumen <- DBI::dbGetQuery(con,
    'SELECT COUNT(*) AS observaciones_validas,COUNT(DISTINCT id_comun) AS presentaciones,COUNT(DISTINCT comercio) AS comercios FROM observaciones WHERE id_ejecucion=? AND comparable=1',
    params=list(ultima))
  print(resumen,row.names=FALSE)
  message('Archivo de respaldo: ',destino)
  invisible(resumen)
}
