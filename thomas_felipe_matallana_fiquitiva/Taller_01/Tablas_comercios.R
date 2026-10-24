# Instalar paquetes solo si faltan
paquetes <- c("DBI", "RSQLite")
faltantes <- setdiff(paquetes, rownames(installed.packages()))

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

# Selecciona el archivo comparador_precios.sqlite
ruta <- file.choose()

# Conectar en modo de solo lectura
conexion <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = ruta,
  flags = RSQLite::SQLITE_RO
)

# Mostrar nombres de tablas y vistas
DBI::dbListTables(conexion)

# Cargar los datos en R
observaciones <- DBI::dbReadTable(conexion, "observaciones")
categoria     <- DBI::dbReadTable(conexion, "categoria")
comparables   <- DBI::dbReadTable(conexion, "comparables")
productos     <- DBI::dbReadTable(conexion, "productos")
comercios     <- DBI::dbReadTable(conexion, "comercios")
ejecuciones   <- DBI::dbReadTable(conexion, "ejecuciones")
cobertura     <- DBI::dbReadTable(conexion, "cobertura")

# Cargar únicamente los productos comparables de la última captura
comparables_ultima <- DBI::dbGetQuery(conexion, "
  SELECT *
  FROM comparables
  WHERE ejecucion = (SELECT MAX(id) FROM ejecuciones)
")

# Cerrar la conexión: las tablas ya quedan cargadas en R
DBI::dbDisconnect(conexion)

# Abrir las tablas en el visor de RStudio
View

############

# Seleccionar la última ejecución
ultima_ejecucion <- max(ejecuciones$id)

datos_ultima <- subset(
  observaciones,
  ejecucion == ultima_ejecucion
)

# Una tabla por comercio
tabla_colsubsidio <- subset(
  datos_ultima, comercio == "colsubsidio"
)

tabla_cafam <- subset(
  datos_ultima, comercio == "cafam"
)

tabla_la_rebaja <- subset(
  datos_ultima, comercio == "larebaja"
)

# Abrir cada tabla en RStudio
View(tabla_colsubsidio)
View(tabla_cafam)
View(tabla_la_rebaja)

# Cantidad de registros extraídos
data.frame(
  comercio = c("Colsubsidio", "Cafam", "La Rebaja"),
  registros = c(
    nrow(tabla_colsubsidio),
    nrow(tabla_cafam),
    nrow(tabla_la_rebaja)
  )
)
