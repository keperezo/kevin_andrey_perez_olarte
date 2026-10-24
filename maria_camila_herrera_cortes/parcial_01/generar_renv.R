# generar_renv.R -- crea renv.lock con las versiones instaladas EN TU COMPUTADOR.
# Ejecutar UNA vez, desde la carpeta del proyecto (la de la entrega).

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# 1) Asegura que los paquetes estén instalados
paquetes <- c("httr2", "jsonlite", "DBI", "RSQLite", "ggplot2", "scales", "knitr", "rmarkdown")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)

# 2) Inicia renv sin reinstalar nada (conserva lo ya instalado) y detecta dependencias
renv::init(bare = FALSE, restart = FALSE)

# 3) Registra las versiones en renv.lock
renv::snapshot(type = "implicit", prompt = FALSE)

# 4) Verificación: debe decir que el proyecto está sincronizado
renv::status()
