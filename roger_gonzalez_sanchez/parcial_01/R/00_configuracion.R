# Paquetes, constantes globales, comercios, listas negra/blanca y reglas de
# productos comparables. Todo lo "configurable" del taller vive aquí.


paquetes_requeridos <- c("dplyr", "stringr", "purrr", "tibble", "httr2",
                         "jsonlite", "DBI", "RSQLite")
faltantes <- paquetes_requeridos[!vapply(paquetes_requeridos, requireNamespace,
                                         logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  stop("Faltan paquetes. Ejecute: install.packages(c(",
       paste0('"', faltantes, '"', collapse = ", "), "))", call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(httr2)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
})


# Rutas y parámetros generales

if (!exists("DIR_PROYECTO")) DIR_PROYECTO <- getwd()

DIR_RAW        <- file.path(DIR_PROYECTO, "raw")
DIR_RAW_TEST   <- file.path(DIR_RAW, "test")
DIR_RAW_VIAB   <- file.path(DIR_RAW, "viabilidad")
DB_PATH        <- file.path(DIR_PROYECTO, "comparador_precios.sqlite")
DB_TEST_PATH   <- file.path(DIR_PROYECTO, "comparador_precios_test.sqlite")

TZ_COLOMBIA     <- "America/Bogota"   # Colombia no tiene horario de verano
OFFSET_COLOMBIA <- "-05:00"

USER_AGENT <- "Mozilla/5.0 (compatible; Taller1-MineriaDatos-UNAL/1.0; uso academico)"
PAUSA_ENTRE_PETICIONES <- 1.2   # segundos entre peticiones (cortesía con el servidor)


EXIGIR_DISPONIBILIDAD <- FALSE
TIMEOUT_SEG            <- 45
MAX_REINTENTOS         <- 3

# Los 3 comercios seleccionados tras la Fase 1 (evaluar_viabilidad_comercios()).
COMERCIOS_SELECCIONADOS <- c("farmatodo", "cruzverde", "larebaja")

SUBCATEGORIAS_VALIDAS <- c("Antihistamínicos",
                           "Antigripales / Descongestionantes",
                           "Antitusivos / Expectorantes",
                           "Enfermedades Respiratorias")
UNIDADES_VALIDAS <- c("mL", "g", "mg", "unidad", "sobres", "dosis")

# Estructura uniforme de salida (las 13 columnas enumeradas en el enunciado).
COLUMNAS_ESTANDAR <- c("comercio", "sku_comercio", "producto_nombre_original",
                       "marca_estandarizada", "subcategoria_estandar",
                       "contenido_num", "unidad_medida", "precio_cop",
                       "precio_regular_cop", "es_promocion", "url_producto",
                       "fecha_hora_consulta", "id_ejecucion")


COLUMNAS_TRAZABILIDAD <- c("presentacion_original", "descripcion_original",
                           "ficha_tecnica_original", "categoria_origen",
                           "url_consulta", "archivo_raw")


COLUMNAS_EXTRA <- c(COLUMNAS_TRAZABILIDAD, "disponible")


REGEX_SPECS_EXCLUIDAS <- paste0("^ML_|^tituloContenido$|^DescripcionContenido$|^Ciudades Exentas$|",
                                "^Filtros$|^Marcadores$|^Restricciones$|^idkit$|^Kit$|^Pum$|^PUM$|^Impuesto$")

# Comercios candidatos

COMERCIOS <- list(
  farmatodo = list(
    nombre = "Farmatodo",
    rol = "principal",
    plataforma = "Algolia Search API (REST/JSON) del front-end",
    url_base = "https://www.farmatodo.com.co",
   
    algolia_app = "VCOJEYD2PO",
    algolia_indice = "products-colombia",
    filtro_categorias = c(
      "OTC SIST RESPIRATORIO" = 'rms_deparment:"OTC SIST RESPIRATORIO"',
      "RX SIST RESPIRATORIO"  = 'rms_deparment:"RX SIST RESPIRATORIO"'
    ),
    tam_pagina = 100,
    tam_pagina_busqueda = 50
  ),
  cruzverde = list(
    nombre = "Cruz Verde",
    rol = "principal",
    plataforma = "Salesforce Commerce Cloud (API REST/JSON)",
    url_base = "https://www.cruzverde.com.co",
    api = "https://api.cruzverde.com.co",
    categorias = c("medicamento-gripaytos", "medicamento-sisresp",
                   "medicamento-alergias"),
    tam_pagina = 48
  ),
  larebaja = list(
    nombre = "La Rebaja",
    rol = "principal",
    plataforma = "VTEX Catalog API (REST/JSON)",
    url_base = "https://www.larebajavirtual.com",
    api = "https://www.larebajavirtual.com/api/catalog_system/pub/products/search",
    campo_nombre = "productName",
    categorias = c(
      "drogueria/gripa-y-tos",
      "drogueria/antialergicos-y-descongestionantes",
      "drogueria/sistema-respiratorio",
      "drogueria/medicamentos-con-prescripcion-medica/aparato-respiratorio",
      "drogueria/medicamentos-con-prescripcion-medica/tos-y-mucoliticos",
      "drogueria/medicamentos-genericos-con-prescripcion-medica/antiasmaticos",
      "drogueria/medicamentos-genericos-con-prescripcion-medica/antihistaminicos",
      "drogueria/medicamentos-genericos-con-prescripcion-medica/gripa-y-tos",
      "drogueria/medicamentos-genericos-de-venta-libre/tos"
    ),
    tam_pagina = 50
  ),
  pasteur = list(
    nombre = "Farmacias Pasteur",
    rol = "respaldo",
    plataforma = "VTEX Catalog API (REST/JSON)",
    url_base = "https://www.farmaciaspasteur.com.co",
    api = "https://www.farmaciaspasteur.com.co/api/catalog_system/pub/products/search",
    campo_nombre = "nameComplete",
    categorias = c("salud-y-medicamentos/tos-gripa-y-alergias"),
    tam_pagina = 50
  ),
  exito = list(
    nombre = "Éxito",
    rol = "respaldo",
    plataforma = "VTEX Catalog API (REST/JSON, prefijo /io)",
    url_base = "https://www.exito.com",
    api = "https://www.exito.com/io/api/catalog_system/pub/products/search",
    campo_nombre = "nameComplete",
    categorias = c("cuidado-personal/salud-y-medicamentos/gripa-y-tos"),
    tam_pagina = 50
  )
)


# Lista NEGRA del taller (descarte obligatorio). 

LISTA_NEGRA_TALLER <- c(
  "Amoxicilina"     = "amoxicilina",
  "Azitromicina"    = "azitromicina",
  "Ciprofloxacina"  = "ciprofloxacin",
  "Cephalexina"     = "cephalexina|cefalexina",
  "Claritromicina"  = "claritromicina",
  "Ampicilina"      = "ampicilina",
  "Levofloxacina"   = "levofloxacin",
  "Oseltamivir"     = "oseltamivir",
  "Tamiflu"         = "tamiflu",
  "Aciclovir"       = "aciclovir|acyclovir",
  "Fluconazol"      = "fluconazol",
  "Naproxeno"       = "naproxen",
  "Omeprazol"       = "omeprazol",
  "Loperamida"      = "loperamida",
  "Vitamina C"      = "vitamina\\s*c\\b|\\bvit\\.?\\s*c\\b",
  "Multivitamínico" = "multivitamin",
  "Shampoo"         = "shampoo|champu",
  "Jabón"           = "\\bjabon",
  "Cremas"          = "\\bcremas?\\b"
)
REGEX_IBUPROFENO <- "ibuprofeno"

# Exclusiones complementarias (no medicamentos que sí podrían contener una
# palabra de la lista blanca, p. ej. "Humidificador Vick").
LISTA_NEGRA_COMPLEMENTARIA <- c(
  "Dispositivo / accesorio" = paste0("humidificador|vaporizador|nebulizador|inhalocamara|",
                                     "aerocamara|camara espaciadora|termometro|aspirador|",
                                     "tapabocas|mascarilla|panuelos"),
  "Uso oftálmico / ótico"   = "oftalm|ophtha|ocular|colirio|\\bojos?\\b|\\boticas?\\b|\\boidos?\\b",
  "Budesonida de uso digestivo" = "cortiment|entocort|budenofalk|\\bmmx\\b|budesonida(?=.*\\b(tabletas?|capsulas?|comprimidos?)\\b)",
  "Obsequio / sin precio real" = "obsequio|\\bregalo\\b|muestra gratis|muestra medica",
  "Uso veterinario"         = "mascota|veterinar|\\bperros?\\b|\\bgatos?\\b",
  "Uso tópico dermatológico" = "\\blocion\\b|dermatolog",
  "Higiene nasal (no medicamento)" = "agua de mar|isotonic|hipertonic|solucion salina|suero fisiologico|marimer|sterimar",
  "Suplemento"              = "redoxon|cebion|probiotic|colageno|omega\\s*3",
  "Aromaterapia"            = "aromaterapia|aceites? esencial",
  "Antiséptico bucal (no respiratorio)" = "solucion bucal|uso bucal|spray bucal|enjuague|bucofaring|colutorio"
)


# Lista BLANCA del taller (inclusión).

LISTA_BLANCA_TALLER <- c(
  "Loratadina"     = "(?<!des)loratadina",
  "Desloratadina"  = "desloratadina",
  "Cetirizina"     = "(?<!levo)ceti?ri[zc]+in+a",   # incluye erratas: cetrizina, cetirizinna
  "Levocetirizina" = "levoceti?ri[zc]+in+a",     # incluye errata: levocetiricina
  "Fexofenadina"   = "fexofenadina",
  "Allegra"        = "allegra",
  "Alercet"        = "alercet",
  "Dolex Gripa"    = "dolex\\s*gripa",
  "Pax"            = "\\bpax\\b",
  "Noraver"        = "noraver",
  "Sinutab"        = "sinutab",
  "Vick"           = "\\bvicks?\\b",
  "Afrin"          = "afrin",
  "Nasivin"        = "nasivin",
  "Oximetazolina"  = "oximetazolina",
  "Fluimucil"      = "fluimucil",
  "Acetilcisteína" = "acetilcisteina",
  "Ambroxol"       = "ambroxol",
  "Bisolvon"       = "bisolvon",
  "Abrilar"        = "abrilar",
  "Hedera Helix"   = "hedera\\s*helix",
  "Guaifenesina"   = "guaifenesina|guayacolato",  # sinónimo DCI: guayacolato de glicerilo
  "Robitussin"     = "robitussin",
  "Avamys"         = "avamys",
  "Flixonase"      = "flixonase",
  "Fluticasona"    = "fluticasona",
  "Ventolin"       = "ventolin",
  "Salbutamol"     = "salbutamol",
  "Budesonida"     = "budesonida",
  "Montelukast"    = "montelukast"
)

# Complemento de la lista blanca: principios activos y términos funcionales de
# los mismos grupos terapéuticos que la categoría declara incluir.
LISTA_BLANCA_COMPLEMENTARIA <- c(
  "Antihistamínico (otro)" = paste0("bilastina|rupatadina|ebastina|azelastina|ketotifeno|",
                                    "dexclorfeniramina|clorfeniramina|difenhidramina|hidroxizina|ciproheptadina|",
                                    "antihistaminic|antialergic"),
  "Antigripal / descongestionante (otro)" = paste0("antigripal|\\bgripa\\b|resfriado|fenilefrina|fenilerfina|",
                                                   "pseudoefedrina|xilometazolina|descongestionante"),
  "Antitusivo / expectorante (otro)" = paste0("dextrometorfano|bromhexina|carbocisteina|carbo?x?i?metilcisteina|",
                                              "carbximetilcisteina|erdosteina|dropropizina|cloperastina|dihidrocodeina|",
                                              "acebrofilina|antitusiv|expectorante|mucolitic|\\btos\\b"),
  "Respiratorio (otro)" = paste0("ipratropio|tiotropio|formoterol|salmeterol|vilanterol|fenoterol|",
                                 "umeclidinio|indacaterol|glicopirronio|olodaterol|doxofilina|mepolizumab|",
                                 "omalizumab|benralizumab|teofilina|aminofilina|terbutalina|broncodilatador|\\basma\\b|",
                                 "(beclometasona|mometasona|ciclesonida|triamcinolona|olopatadina)",
                                 "(?=.*(nasal|inhal|aerosol|spray|mcg|dosis|aplicaciones))"),
  # Marcas cuyo nombre comercial no menciona el principio activo, pero éste
  # pertenece a la categoría (p. ej. Lordinex = loratadina, Fexy = fexofenadina).
  "Marca con principio activo de la categoría" = paste0(
    "clarityne|lordinex|loreze|\\bfexy\\b|alermed|glencet|deslodex|aerius|zyrtec|xyzal|bilaxten|",
    "azeflu|congestex|descongel|noxpirin|dolicox grip|fluzetrin|theraflu|teraflu|\\bzas\\b|",
    "muxol|mucosolvan|tukol|neumolex|nasonex|singulair|clarigrip|tabcin|gripac|coricidin|",
    "flumixol|mucosina|periactin")
)


# Subcategoría estándar (el ORDEN importa: la primera coincidencia gana).

REGEX_SUBCATEGORIA <- c(
  "Enfermedades Respiratorias" = paste0(
    "salbutamol|ventolin|budesonida|montelukast|fluticasona|avamys|flixonase|",
    "beclometasona|mometasona|ciclesonida|ipratropio|tiotropio|formoterol|salmeterol|",
    "vilanterol|fenoterol|umeclidinio|indacaterol|glicopirronio|olodaterol|doxofilina|mepolizumab|omalizumab|",
    "benralizumab|triamcinolona|olopatadina|teofilina|aminofilina|terbutalina|inhalador|\\basma\\b|\\bepoc\\b|",
    "broncodilat|singulair|seretide|symbicort|pulmicort|budecort|nasonex"),
  "Antigripales / Descongestionantes" = paste0(
    "dolex\\s*gripa|\\bpax\\b|noraver|sinutab|\\bvicks?\\b|afrin|nasivin|oximetazolina|",
    "xilometazolina|fenilefrina|fenilerfina|pseudoefedrina|antigripal|\\bgripa\\b|resfriado|",
    "descongest|congestex|noxpirin|theraflu|teraflu|\\bzas\\b|fluzetrin|dolicox grip|clarigrip|tabcin|gripac|coricidin"),
  "Antitusivos / Expectorantes" = paste0(
    "fluimucil|acetilcisteina|ambroxol|bisolvon|abrilar|hedera|guaifenesina|guayacolato|robitussin|",
    "dextrometorfano|bromhexina|carbocisteina|erdosteina|dropropizina|antitusiv|",
    "expectorante|mucolitic|\\btos\\b|muxol|mucosolvan|tukol|neumolex|flumixol|mucosina|cloperastina|",
    "dihidrocodeina|acebrofilina|carbo?x?i?metilcisteina|carbximetilcisteina"),
  "Antihistamínicos" = paste0(
    "loratadina|ceti?ri[zc]+in+a|fexofenadina|allegra|alercet|clarityne|aerius|zyrtec|xyzal|ciproheptadina|periactin|",
    "bilastina|rupatadina|ebastina|azelastina|ketotifeno|clorfeniramina|difenhidramina|hidroxizina|",
    "lordinex|loreze|\\bfexy\\b|alermed|glencet|deslodex|bilaxten|azeflu|",
    "antihistamin|antialergic|alergi")
)
.
REGEX_COMPONENTE_RESPIRATORIO <- paste(REGEX_SUBCATEGORIA, collapse = "|")

# Términos de búsqueda dirigida: la lista blanca + los 8 comparables.
TERMINOS_COMPARABLES <- c("dolex gripa", "pax noche", "fluimucil", "abrilar",
                          "afrin", "loratadina", "allegra", "avamys", "ventolin")
TERMINOS_BUSQUEDA <- unique(c(tolower(names(LISTA_BLANCA_TALLER)), TERMINOS_COMPARABLES))
TERMINOS_BUSQUEDA <- stringi::stri_trans_general(TERMINOS_BUSQUEDA, "Latin-ASCII")


# Diccionarios de marca

MARCAS_COMERCIALES <- c(
  "Dolex" = "\\bdolex\\b", "Pax" = "\\bpax\\b", "Noraver" = "noraver",
  "Sinutab" = "sinutab", "Vick" = "\\bvicks?\\b", "Afrin" = "\\bafrin\\b",
  "Nasivin" = "nasivin", "Fluimucil" = "fluimucil", "Bisolvon" = "bisolvon",
  "Abrilar" = "abrilar", "Robitussin" = "robitussin", "Avamys" = "avamys",
  "Flixonase" = "flixonase", "Ventolin" = "ventolin", "Allegra" = "allegra",
  "Alercet" = "alercet", "Clarityne" = "clarityne", "Aerius" = "aerius",
  "Zyrtec" = "zyrtec", "Xyzal" = "xyzal", "Descongel" = "descongel",
  "Noxpirin" = "noxpirin", "Advil" = "\\badvil\\b", "Congestex" = "congestex",
  "Lordinex" = "lordinex", "Loreze" = "loreze", "Mucosolvan" = "mucosolvan",
  "Broncoxmaxx" = "broncox", "Benadryl" = "benadryl", "Theraflu" = "theraflu|teraflu",
  "Zas" = "\\bzas\\b", "Neumolex" = "neumolex", "Singulair" = "singulair",
  "Seretide" = "seretide", "Symbicort" = "symbicort", "Pulmicort" = "pulmicort",
  "Nasonex" = "nasonex", "Umquan" = "umquan", "Glencet" = "glencet",
  "Alermed" = "alermed", "Deslodex" = "deslodex", "Mieltertos" = "mieltertos",
  "Lemovit" = "lemovit", "Bronxol" = "bronxol", "Budecort" = "budecort",
  "Fexy" = "\\bfexy\\b", "Bilaxten" = "bilaxten", "Azeflu" = "azeflu", "Fluzetrin" = "fluzetrin",
  "Muxol" = "muxol", "Tukol" = "tukol", "Dolicox" = "dolicox", "Geniol" = "geniol",
  "Next" = "\\bnext\\b", "Ventilan" = "ventilan", "Openxy" = "openxy", "Berodual" = "berodual",
  "Iprasynt" = "iprasynt", "Sacrusyt" = "sacrusyt", "Salbumed" = "salbumed", "Rinaid" = "rinaid",
  "Expectos" = "expectos", "Strepsils" = "strepsils", "Benzirin" = "benzirin"
)

LABORATORIOS <- c(
  "Genfar" = "genfar", "MK" = "\\bmk\\b|tecnoquimica", "La Santé" = "la sante",
  "AG" = "\\bag\\b|laboratorios ag", "Coaspharma" = "coaspharma",
  "Laproff" = "laproff", "Memphis" = "memphis", "Expofarma" = "expofarma",
  "Procaps" = "procaps", "Lafrancol" = "lafrancol", "Bussié" = "bussie",
  "Vitalis" = "vitalis", "Siegfried" = "siegfried", "Chalver" = "chalver",
  "Novamed" = "novamed", "Ecar" = "\\becar\\b", "Winthrop" = "winthrop",
  "Sandoz" = "sandoz", "American Generics" = "american generics",
  "Colmed" = "colmed", "Anglopharma" = "anglopharma", "Biochem" = "biochem",
  "Faes Farma" = "faes farma"
)


# Reglas de los 8 productos comparables.
# Cada producto tiene alternativas en orden de prioridad. Para cada alternativa
# se exige: todos los patrones de `incluye`, ninguno de `excluye`, el mismo
# contenido (número y unidad) y la MISMA marca_estandarizada en los 3 comercios
# (para genéricos, el mismo laboratorio). `marcas_preferidas` sólo ordena.

REGLAS_COMPARABLES <- list(
  list(id = "COMP_DOLEX_GRIPA", descripcion = "Dolex Gripa x 12 tabletas",
       alternativas = list(
         list(etiqueta = "Dolex Gripa tabletas x 12",
              incluye = c("dolex\\s*gripa"),
              excluye = "noche|nino|jarabe|suspension|\\bmax\\b|forte|plus|extra",
              contenido = 12, unidades = c("unidad"), marcas_preferidas = c("Dolex")))),
  list(id = "COMP_PAX_NOCHE", descripcion = "Pax Noche sobres x 6",
       alternativas = list(
         list(etiqueta = "Pax Noche Panela-Limón x 6 sobres",
              incluye = c("\\bpax\\b", "noche", "panela"),
              excluye = "\\bdia\\b|jarabe|tableta|capsula",
              contenido = 6, unidades = c("sobres", "unidad"), marcas_preferidas = c("Pax")),
         list(etiqueta = "Pax Noche Limón x 6 sobres",
              incluye = c("\\bpax\\b", "noche", "limon"),
              excluye = "panela|\\bdia\\b|jarabe|tableta|capsula",
              contenido = 6, unidades = c("sobres", "unidad"), marcas_preferidas = c("Pax")))),
  list(id = "COMP_FLUIMUCIL_600", descripcion = "Fluimucil 600 mg sobres x 10",
       alternativas = list(
         list(etiqueta = "Fluimucil 600 mg granulado x 10 sobres",
              incluye = c("fluimucil|acetilcisteina", "(?<![0-9.,])600\\s*mg|\\b600\\b"),
              excluye = "efervesc|tableta|\\btab\\b|comprimido|jarabe|nino|pediatr",
              contenido = 10, unidades = c("sobres", "unidad"), marcas_preferidas = c("Fluimucil")))),
  list(id = "COMP_ABRILAR_JARABE", descripcion = "Abrilar jarabe 100 mL",
       alternativas = list(
         list(etiqueta = "Abrilar jarabe 100 mL (sin mentol)",
              incluye = c("abrilar"),
              excluye = "mentol|tableta|efervesc|sobre|pastilla|goma",
              contenido = 100, unidades = c("mL"), marcas_preferidas = c("Abrilar")))),
  list(id = "COMP_AFRIN_ADULTOS", descripcion = "Afrin adultos 0,05 % 15 mL",
       alternativas = list(
         list(etiqueta = "Afrin adultos 0,05 % frasco 15 mL",
              incluye = c("\\bafrin\\b"),
              excluye = "pediatr|nino|infantil|0[.,]\\s?025|hidratante|humect|eucalipt|\\bmenta\\b",
              contenido = 15, unidades = c("mL"), marcas_preferidas = c("Afrin")))),
  list(id = "COMP_LORATADINA_10MG", descripcion = "Loratadina 10 mg x 10 tabletas (mismo laboratorio)",
       alternativas = list(
         list(etiqueta = "Loratadina 10 mg x 10 tabletas",
              incluye = c("(?<!des)loratadina", "(?<![0-9.,])10\\s*mg"),
              excluye = "gripa|fenilefrina|pseudoefedrina|acetaminofen|descongel|jarabe|jbe|suspension|solucion|\\bml\\b|nino|pediatr",
              contenido = 10, unidades = c("unidad"),
              marcas_preferidas = c("Genfar", "MK", "La Santé", "AG", "Coaspharma", "Clarityne")))),
  list(id = "COMP_ALLEGRA_120", descripcion = "Allegra / Fexofenadina 120 mg x 10 tabletas",
       alternativas = list(
         list(etiqueta = "Allegra (fexofenadina) 120 mg x 10 tabletas",
              incluye = c("allegra|fexofenadina", "(?<![0-9.,])120\\s*mg"),
              excluye = "pediatr|suspension|jarabe|\\bml\\b|allegra\\s*d\\b|pseudoefedrina|180",
              contenido = 10, unidades = c("unidad"), marcas_preferidas = c("Allegra")))),
  list(id = "COMP_AVAMYS_SPRAY", descripcion = "Avamys (fluticasona) spray 120 dosis / Ventolin",
       alternativas = list(
         list(etiqueta = "Avamys fluticasona spray nasal 120 dosis",
              incluye = c("avamys|fluticasona\\s*furoato"),
              excluye = "",
              contenido = 120, unidades = c("dosis"), marcas_preferidas = c("Avamys")),
         list(etiqueta = "Ventolin (salbutamol) inhalador 200 dosis",
              incluye = c("ventolin|salbutamol", "inhal|aerosol|spray|dosis|mcg"),
              excluye = "jarabe|tableta|nebuliz|solucion para",
              contenido = 200, unidades = c("dosis"), marcas_preferidas = c("Ventolin"))))
)
IDS_COMPARABLES <- map_chr(REGLAS_COMPARABLES, "id")
