# =============================================================
# Taller 11 - Saber 11: ¿colegio o estrato?
# Explorador con filtros (Etapa 1) + modelo e historial (Etapa 2)
# Requiere saber11.csv en la misma carpeta (lo crea generar_saber11.R)
# =============================================================

library(shiny)
library(bslib)
library(DT)

# ---- Datos -------------------------------------------------------------
saber <- read.csv("saber11.csv")
saber$naturaleza <- factor(saber$naturaleza, levels = c("Oficial", "Privado"))
saber$zona       <- factor(saber$zona,       levels = c("Rural", "Urbana"))
saber$departamento <- factor(saber$departamento)

dptos <- c("Bogotá", "Antioquia", "Valle del Cauca", "Boyacá", "Chocó")

# Nombre de la columna en los datos para cada casilla de predictor
predictores_disp <- c("estrato" = "estrato",
                      "naturaleza" = "naturaleza",
                      "horas" = "horas_estudio",
                      "zona" = "zona")
categoricos <- c("naturaleza", "zona", "departamento")

# Historial vacío (plantilla para rbind)
historial_vacio <- data.frame(
  escenario   = integer(0),
  predictores = character(0),
  filtro      = character(0),
  privado     = numeric(0),
  r2          = numeric(0),
  n           = integer(0),
  stringsAsFactors = FALSE
)

# ---- UI ----------------------------------------------------------------
ui <- page_sidebar(
  title = "Saber 11: ¿colegio o estrato?",                       # (1)
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(                                             # (2)
    width = 280,
    selectInput("departamento", "Departamento",
                choices = c("Todos", dptos), selected = "Todos"),
    checkboxGroupInput("naturaleza", "Naturaleza del colegio",
                       choices = c("Oficial", "Privado"),
                       selected = c("Oficial", "Privado")),
    sliderInput("estrato", "Estrato", min = 1, max = 6,
                value = c(1, 6), step = 1),
    radioButtons("zona", "Zona",
                 choices = c("Todas", "Urbana", "Rural"),
                 selected = "Todas")
  ),

  navset_card_tab(                                               # (6)
    nav_panel(
      "Explorar",
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Resumen del puntaje"),
             tableOutput("resumen")),
        card(card_header("Puntaje por naturaleza"),
             plotOutput("boxplot", height = "320px"))
      ),
      card(card_header("Datos filtrados"),
           DTOutput("tabla"))
    ),

    nav_panel(
      "Modelo",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Especificación"),
          checkboxGroupInput("predictores", "Predictores (lm sobre puntaje)", # (7)
                             choices = names(predictores_disp),
                             selected = c("estrato", "naturaleza")),
          actionButton("ajustar", "Ajustar", class = "btn-primary"),
          tags$hr(),
          actionButton("guardar", "Guardar en historial",          # (10)
                       class = "btn-outline-secondary"),
          tags$small(class = "text-muted",
                     "El modelo solo cambia al pulsar Ajustar.")
        ),
        card(
          card_header("Resultado"),
          textOutput("formula_txt"),
          tableOutput("coeficientes"),                          # (8)
          textOutput("ajuste_txt")
        )
      ),
      card(
        card_header("Historial de escenarios"),
        DTOutput("historial"),
        downloadButton("descargar", "Descargar CSV")
      )
    )
  )
)

# ---- Server ------------------------------------------------------------
server <- function(input, output, session) {

  # (3) + (5): un solo reactive con el filtro, validado
  datos_filtrados <- reactive({
    validate(need(length(input$naturaleza) > 0,
                  "Selecciona al menos un tipo de colegio (Oficial o Privado)."))
    d <- saber
    if (input$departamento != "Todos")
      d <- d[d$departamento == input$departamento, ]
    d <- d[d$naturaleza %in% input$naturaleza, ]
    d <- d[d$estrato >= input$estrato[1] & d$estrato <= input$estrato[2], ]
    if (input$zona != "Todas")
      d <- d[d$zona == input$zona, ]
    validate(need(nrow(d) > 0,
                  "No hay estudiantes con esta combinación de filtros (p. ej., no hay colegios rurales de estrato 5 o 6)."))
    droplevels(d)
  })

  # Texto del filtro activo (para el historial)
  filtro_txt <- reactive({
    paste0("Depto: ", input$departamento,
           " | Nat: ", paste(input$naturaleza, collapse = "+"),
           " | Estrato ", input$estrato[1], "-", input$estrato[2],
           " | Zona: ", input$zona)
  })

  # (4) Salidas de la etapa 1 ------------------------------------------
  output$resumen <- renderTable({
    d <- datos_filtrados()
    fila <- function(nombre, x)
      data.frame(Grupo = nombre, n = length(x),
                 Media = mean(x), Mediana = median(x))
    filas <- list(fila("Total", d$puntaje))
    for (niv in levels(d$naturaleza))
      filas[[length(filas) + 1]] <- fila(niv, d$puntaje[d$naturaleza == niv])
    do.call(rbind, filas)
  }, digits = 1)

  output$boxplot <- renderPlot({
    d <- datos_filtrados()
    boxplot(puntaje ~ naturaleza, data = d,
            col = c(Oficial = "#8FB8DE", Privado = "#F2A65A")[levels(d$naturaleza)],
            xlab = "Naturaleza", ylab = "Puntaje global",
            main = paste0("n = ", nrow(d)))
  })

  output$tabla <- renderDT({
    datatable(datos_filtrados(), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # (7) Modelo solo al pulsar Ajustar -----------------------------------
  modelo <- eventReactive(input$ajustar, {
    d <- datos_filtrados()
    preds <- input$predictores
    validate(need(length(preds) > 0, "Marca al menos un predictor."))

    vars <- unname(predictores_disp[preds])

    # Quitar categóricos con un solo nivel en los datos filtrados
    un_nivel <- vars[vars %in% categoricos &
                       sapply(vars, function(v) length(unique(d[[v]])) < 2)]
    if (length(un_nivel) > 0) {
      showNotification(
        paste0("Se quitó del modelo ", paste(un_nivel, collapse = ", "),
               ": con el filtro actual tiene un solo nivel."),
        type = "warning", duration = 6)
      vars <- setdiff(vars, un_nivel)
    }
    validate(need(length(vars) > 0,
                  "Ningún predictor tiene variación con este filtro."))

    f <- reformulate(vars, response = "puntaje")
    m <- lm(f, data = d)

    # (9) Aviso de filas descartadas por NA
    descartadas <- nrow(d) - nobs(m)
    if (descartadas > 0)
      showNotification(
        paste0("lm() descartó ", descartadas,
               " filas sin datos (NA en horas de estudio)."),
        type = "message", duration = 6)

    list(modelo = m, formula = f, vars = vars,
         filtro = filtro_txt(), descartadas = descartadas)
  })

  output$formula_txt <- renderText({
    paste("Fórmula:", deparse(modelo()$formula))
  })

  output$coeficientes <- renderTable({
    s <- summary(modelo()$modelo)$coefficients
    data.frame(Termino = rownames(s),
               Coef = s[, 1], EE = s[, 2], p_valor = s[, 4])
  }, digits = 3)

  output$ajuste_txt <- renderText({
    m <- modelo()$modelo
    paste0("R² = ", round(summary(m)$r.squared, 3),
           "   |   n usado = ", nobs(m),
           if (modelo()$descartadas > 0)
             paste0("   |   filas descartadas = ", modelo()$descartadas) else "")
  })

  # (10) Historial con reactiveValues ------------------------------------
  rv <- reactiveValues(historial = historial_vacio)

  observeEvent(input$guardar, {
    if (input$ajustar == 0) {
      showNotification("Primero ajusta un modelo.", type = "warning")
      return()
    }
    res <- modelo()
    m <- res$modelo
    b <- coef(m)
    nueva <- data.frame(
      escenario   = nrow(rv$historial) + 1L,
      predictores = paste(res$vars, collapse = " + "),
      filtro      = res$filtro,
      privado     = if ("naturalezaPrivado" %in% names(b))
                      round(unname(b["naturalezaPrivado"]), 1) else NA,
      r2          = round(summary(m)$r.squared, 3),
      n           = nobs(m),
      stringsAsFactors = FALSE
    )
    rv$historial <- rbind(rv$historial, nueva)
    showNotification("Escenario guardado.", type = "message", duration = 3)
  })

  output$historial <- renderDT({
    datatable(rv$historial, rownames = FALSE,
              options = list(dom = "t", pageLength = 50))
  })

  output$descargar <- downloadHandler(
    filename = function() "historial_escenarios.csv",
    content  = function(file) write.csv(rv$historial, file, row.names = FALSE)
  )
}

shinyApp(ui, server)
