################ PREVIO ###############
url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

# Instalamos los paquetes necesarios
paquetes  <- c("DBI", "RSQLite", "dplyr", "knitr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)
library(dplyr)

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

dbListTables(con)

## MOSTRAR TABLA FUNCIÓN

mostrar_tabla <- function(x, n = 10, caption = NULL) {
  x_mostrar <- head(x, n)
  
  tabla_html <- knitr::kable(
    x_mostrar,
    format = "html",
    caption = caption,
    table.attr = 'class="table table-striped table-hover table-condensed"'
  )
  
  knitr::asis_output(
    paste0(
      '<div class="table-scroll">',
      as.character(tabla_html),
      '</div>'
    )
  )
}

################ EJERCICIO 1 ###############
#Con la base de datos Chinook, responde las siguientes preguntas usando 
#sentencias SQL.
#1) ¿Cuántas pistas hay en total en la base de datos?

Total_pistas_1 <- dbGetQuery(con, "
  SELECT DISTINCT Name,COUNT(TrackId) AS n_pistas
  FROM Track
  GROUP BY Name
  ORDER BY n_pistas;
")

#2) ¿Cuántos géneros distintos existen?

generos <- dbGetQuery(con, "
  SELECT   DISTINCT g.Name        AS genero,
           COUNT(t.TrackId) AS n_pistas
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")

generos_SOLOT <- dbGetQuery(con, "
  SELECT   DISTINCT GenreId, COUNT(TrackId) AS n_pistas
  FROM     Track
  GROUP BY GenreId
  ORDER BY n_pistas DESC;
")

#Obtén el nombre del género, el número de pistas y la duración promedio 
#(en minutos) para cada género. Ordena de mayor a menor número de pistas.

Gen_pis_prom <- dbGetQuery(con, "
  SELECT   DISTINCT g.Name        AS genero,
           COUNT(t.TrackId) AS n_pistas,
           AVG(t.Milliseconds) AS prom
  FROM     Track  t
  JOIN     Genre  g ON t.GenreId = g.GenreId
  GROUP BY g.Name
  ORDER BY n_pistas DESC;
")


#¿Cuáles son las 5 pistas más largas? Muestra el nombre, el álbum y la duración 
#en minutos.

pistas_top <- dbGetQuery(con, "
  SELECT   DISTINCT Name, AlbumId,Milliseconds
  FROM     Track
  ORDER BY Milliseconds DESC
  LIMIT 5;
")

#¿Cuántas pistas tienen un UnitPrice superior a $0.99? ¿Qué porcentaje 
#representan del total?



#Calcula la media, mínimo, máximo y varianza (en SQL) de la duración en 
#milisegundos de todas las pistas.

