# Taller 11 - Saber 11: ¿colegio o estrato?
# App Shiny: explorador con filtros (Etapa 1) + modelo e historial (Etapa 2)

library(shiny)
library(bslib)
library(DT)

# Datos: si no existe el CSV, se genera con el script del paso 0
if (!file.exists("saber11.csv")) source("generar_datos.R")
saber <- read.csv("saber11.csv")

deptos <- sort(unique(saber$departamento))

# ---------------------------------------------------------------- UI ----
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Saber 11: ¿colegio o estrato?"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h5("Filtros"),
      selectInput("depto", "Departamento:", choices = c("Todos", deptos)),
      checkboxGroupInput("naturaleza", "Naturaleza del colegio:",
                         choices = c("Oficial", "Privado"),
                         selected = c("Oficial", "Privado")),
      sliderInput("estrato", "Estrato:", min = 1, max = 6,
                  value = c(1, 6), step = 1),
      radioButtons("zona", "Zona:", choices = c("Todas", "Urbana", "Rural"),
                   selected = "Todas", inline = TRUE)
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # ------------------------------------------ Etapa 1: Explorar --
        tabPanel("Explorar",
          br(),
          fluidRow(
            column(5, h5("Resumen del puntaje"), tableOutput("resumen")),
            column(7, plotOutput("boxplot", height = "320px"))
          ),
          hr(),
          DTOutput("tabla")
        ),

        # -------------------------------------------- Etapa 2: Modelo --
        tabPanel("Modelo",
          br(),
          fluidRow(
            column(4,
              checkboxGroupInput("predictores", "Predictores:",
                choices = c("Estrato" = "estrato",
                            "Naturaleza" = "naturaleza",
                            "Horas de estudio" = "horas_estudio",
                            "Zona" = "zona"),
                selected = c("estrato", "naturaleza")),
              actionButton("ajustar", "Ajustar", class = "btn-primary"),
              actionButton("guardar", "Guardar", class = "btn-success")
            ),
            column(8,
              h5("Modelo: lm(puntaje ~ ...)"),
              textOutput("info_modelo"),
              br(),
              tableOutput("coeficientes")
            )
          ),
          hr(),
          h5("Historial de escenarios"),
          tableOutput("historial"),
          downloadButton("descargar", "Descargar CSV")
        )
      )
    )
  )
)

# ------------------------------------------------------------ SERVER ----
server <- function(input, output, session) {

  # Un solo filtro que alimenta todas las salidas
  datos_filtrados <- reactive({
    validate(need(length(input$naturaleza) > 0,
                  "Selecciona al menos un tipo de colegio (Oficial o Privado)."))
    d <- saber
    if (input$depto != "Todos") d <- d[d$departamento == input$depto, ]
    d <- d[d$naturaleza %in% input$naturaleza, ]
    d <- d[d$estrato >= input$estrato[1] & d$estrato <= input$estrato[2], ]
    if (input$zona != "Todas") d <- d[d$zona == input$zona, ]
    validate(need(nrow(d) > 0,
                  "No hay estudiantes con esa combinación de filtros (por ejemplo, no hay colegios rurales de estrato 5 o 6)."))
    d
  })

  # Texto del filtro activo (para el historial)
  filtro_txt <- reactive({
    paste0(input$depto, " | ", paste(input$naturaleza, collapse = "+"),
           " | estrato ", input$estrato[1], "-", input$estrato[2],
           " | zona ", input$zona)
  })

  # ---- Etapa 1: salidas ----
  output$resumen <- renderTable({
    d <- datos_filtrados()
    fila <- function(nombre, x) data.frame(Grupo = nombre, n = length(x),
                                           Media = mean(x), Mediana = median(x))
    res <- fila("Total", d$puntaje)
    for (k in sort(unique(d$naturaleza)))
      res <- rbind(res, fila(k, d$puntaje[d$naturaleza == k]))
    res
  }, digits = 1, striped = TRUE)

  output$boxplot <- renderPlot({
    d <- datos_filtrados()
    boxplot(puntaje ~ naturaleza, data = d,
            col = c(Oficial = "#93c5fd", Privado = "#fca5a5")[sort(unique(d$naturaleza))],
            xlab = "", ylab = "Puntaje global",
            main = "Puntaje por naturaleza del colegio")
  })

  output$tabla <- renderDT({
    datatable(datos_filtrados(), rownames = FALSE,
              options = list(pageLength = 10))
  })

  # ---- Etapa 2: modelo bajo demanda ----
  modelo <- eventReactive(input$ajustar, {
    validate(need(length(input$predictores) > 0,
                  "Selecciona al menos un predictor."))
    d <- datos_filtrados()
    # Un factor con un solo nivel hace fallar lm()
    for (v in intersect(input$predictores, c("naturaleza", "zona"))) {
      validate(need(length(unique(d[[v]])) >= 2,
        paste0("Con el filtro actual '", v, "' tiene un solo nivel; ",
               "quítala de los predictores o amplía el filtro.")))
    }
    m <- lm(reformulate(input$predictores, response = "puntaje"), data = d)
    list(modelo = m,
         predictores = paste(input$predictores, collapse = " + "),
         filtro = filtro_txt(),
         descartadas = nrow(d) - nobs(m))
  })

  # Aviso de filas descartadas por NA
  observeEvent(modelo(), {
    if (modelo()$descartadas > 0)
      showNotification(paste(modelo()$descartadas,
                             "filas sin datos de horas_estudio fueron descartadas por lm()."),
                       type = "warning", duration = 6)
  })

  output$info_modelo <- renderText({
    res <- modelo()
    paste0("R² = ", round(summary(res$modelo)$r.squared, 3),
           "   |   n usado = ", nobs(res$modelo),
           "   |   filas descartadas = ", res$descartadas)
  })

  output$coeficientes <- renderTable({
    s <- summary(modelo()$modelo)$coefficients
    data.frame("Término" = rownames(s), "Coeficiente" = s[, 1],
               "Error est." = s[, 2], "p-valor" = s[, 4], check.names = FALSE)
  }, digits = 3, striped = TRUE)

  # ---- Historial de escenarios ----
  rv <- reactiveValues(
    historial = data.frame(Escenario = integer(), Predictores = character(),
                           Filtro = character(), Privado = numeric(),
                           R2 = numeric(), n = integer())
  )

  observeEvent(input$guardar, {
    res <- modelo()
    b <- coef(res$modelo)
    nueva <- data.frame(
      Escenario   = as.integer(nrow(rv$historial) + 1),
      Predictores = res$predictores,
      Filtro      = res$filtro,
      Privado     = if ("naturalezaPrivado" %in% names(b)) round(b[["naturalezaPrivado"]], 1) else NA,
      R2          = round(summary(res$modelo)$r.squared, 3),
      n           = nobs(res$modelo)
    )
    rv$historial <- rbind(rv$historial, nueva)
    showNotification("Escenario guardado en el historial.", type = "message")
  })

  output$historial <- renderTable({
    h <- rv$historial
    h$Privado <- ifelse(is.na(h$Privado), "—", sprintf("%+.1f", h$Privado))
    h$R2 <- sprintf("%.3f", h$R2)
    h
  }, striped = TRUE)

  output$descargar <- downloadHandler(
    filename = function() paste0("historial_escenarios_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(rv$historial, file, row.names = FALSE)
  )
}

shinyApp(ui, server)
