# prueba.R — revisar cada comercio por separado antes de la corrida completa
source("scraper.R")

corrida <- nueva_corrida()

locatel <- scrape_locatel(corrida)
nrow(locatel)                         # ~237 (incluye agotados)
attr(locatel, "verificacion")
View(clasificar_productos(locatel))

farmatodo <- scrape_farmatodo(corrida)
nrow(farmatodo)                       # ~803 en "Cuidado de la vista"
attr(farmatodo, "verificacion")

colsubsidio <- scrape_colsubsidio(corrida)
nrow(colsubsidio)                     # ~239 en "Salud visual"
attr(colsubsidio, "verificacion")

# Modo sin conexión: vuelve a leer lo guardado en raw/
identical(nrow(locatel),     nrow(scrape_locatel(corrida, offline = TRUE)))
identical(nrow(farmatodo),   nrow(scrape_farmatodo(corrida, offline = TRUE)))
identical(nrow(colsubsidio), nrow(scrape_colsubsidio(corrida, offline = TRUE)))

# Carga en SQLite de ESA misma corrida (desde raw/, sin volver a consultar)
ejecutar_recoleccion(corrida = corrida, offline = TRUE)

# Productos comunes a los tres comercios
con <- conectar_db()
DBI::dbGetQuery(con, "SELECT id_producto, nombre_std, linea, regla_correspondencia FROM productos")
DBI::dbDisconnect(con)

