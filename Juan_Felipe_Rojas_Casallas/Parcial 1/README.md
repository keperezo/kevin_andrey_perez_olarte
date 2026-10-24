# Parcial 1 - Comparador de Precios

Juan Felipe Rojas Casallas - Mineria de Datos (2016325)

Categoria 42: Repelentes y control de insectos

## Tiendas

- La Rebaja Virtual (drogueria)
- Exito (supermercado)
- Jumbo Colombia (supermercado)

Las 3 usan VTEX, asi que use la API publica de catalogo para sacar los productos.

## Archivos

- `taller_1.R` - Todo el codigo (scraping, limpieza, consultas SQL, graficos)
- `comparador_precios.sqlite` - Base de datos con los productos
- `raw/` - Los JSON crudos que devuelve la API
- `renv.lock` - Versiones de los paquetes

## Como correr

Abrir `taller_1.R` en RStudio y darle source, o desde la terminal:

```
Rscript taller_1.R
```

Si ya existe `comparador_precios.sqlite` no vuelve a scrapear.
Para volver a scrapear hay que borrar la base primero.

## Paquetes necesarios

httr, jsonlite, DBI, RSQLite, dplyr, ggplot2
