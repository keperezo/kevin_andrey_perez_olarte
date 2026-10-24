set.seed(1111); n <- 1500
dp <- c("Bogotá", "Antioquia", "Valle del Cauca", "Boyacá", "Chocó")
departamento <- sample(dp, n, TRUE, c(.30, .25, .20, .15, .10))
p_rural <- c(.05, .25, .25, .40, .50)[match(departamento, dp)]
zona <- ifelse(runif(n) < p_rural, "Rural", "Urbana")
estrato <- ifelse(zona == "Rural", sample(1:3, n, TRUE, c(.6, .3, .1)),
                  sample(1:6, n, TRUE, c(.15, .30, .28, .15, .07, .05)))
naturaleza <- ifelse(runif(n) < plogis(-3.2 + .75 * estrato),
                     "Privado", "Oficial")
horas_estudio <- round(pmax(0, rnorm(n, 5 + .5 * estrato, 2.5)), 1)
puntaje <- round(200 + 14 * estrato + 12 * (naturaleza == "Privado") +
                   4 * horas_estudio - 15 * (zona == "Rural") -
                   20 * (departamento == "Chocó") + rnorm(n, 0, 30))
horas_estudio[sample(n, 60)] <- NA  # 60 valores faltantes

df <- data.frame(departamento, zona, estrato, naturaleza, horas_estudio, puntaje)
write.csv(df, "saber11.csv", row.names = FALSE)


library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# Cargar datos
saber11 <- read.csv("saber11.csv")
saber11$naturaleza <- as.factor(saber11$naturaleza)
saber11$zona <- as.factor(saber11$zona)
saber11$departamento <- as.factor(saber11$departamento)

ui <- page_navbar(
  title = "Saber 11: ¿colegio o estrato?",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar global con filtros generales
  sidebar = sidebar(
    title = "Filtros globales",
    selectInput("depto", "Departamento:",
                choices = c("Todos", levels(saber11$departamento))),
    checkboxGroupInput("nat", "Naturaleza:",
                       choices = c("Oficial", "Privado"),
                       selected = c("Oficial", "Privado")),
    checkboxGroupInput("estrato", "Estrato:",
                       choices = 1:6,
                       selected = 1:6),
    radioButtons("zona", "Zona:",
                 choices = c("Todas", "Urbana", "Rural"),
                 selected = "Todas")
  ),
  
  # Pestaña 1: Explorar
  nav_panel(
    title = "Explorar",
    layout_columns(
      value_box(title = "N° Estudiantes (n)", value = textOutput("n_count")),
      value_box(title = "Media Puntaje", value = textOutput("media_puntaje")),
      value_box(title = "Mediana Puntaje", value = textOutput("mediana_puntaje"))
    ),
    card(
      card_header("Distribución por Naturaleza de Colegio"),
      plotOutput("boxplot_nat")
    ),
    card(
      card_header("Tabla de Datos Filtrados"),
      DTOutput("tabla_datos")
    )
  ),
  
  # Pestaña 2: Modelo e Historial
  nav_panel(
    title = "Modelo",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Configuración del Modelo"),
        checkboxGroupInput("predictores", "Seleccionar Predictores:",
                           choices = c("estrato", "naturaleza", "horas_estudio", "zona"),
                           selected = c("estrato", "naturaleza")),
        actionButton("btn_ajustar", "Ajustar Modelo", class = "btn-primary"),
        br(), br(),
        actionButton("btn_guardar", "Guardar en Historial", class = "btn-success"),
        uiOutput("aviso_na")
      ),
      card(
        card_header("Resultados del Modelo e Historial"),
        tableOutput("coef_tabla"),
        hr(),
        h5("Historial de Escenarios"),
        tableOutput("historial_tabla"),
        downloadButton("download_historial", "Descargar CSV")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Reactive con validaciones para evitar errores rojos (Prueba de estrés)
  datos_filtrados <- reactive({
    req(input$nat, input$estrato)
    
    # Validar selecciones vacías de checkbox
    validate(
      need(length(input$nat) > 0, "Por favor seleccione al menos una Naturaleza."),
      need(length(input$estrato) > 0, "Por favor seleccione al menos un Estrato.")
    )
    
    df <- saber11
    if (input$depto != "Todos") {
      df <- df[df$departamento == input$depto, ]
    }
    df <- df[df$naturaleza %in% input$nat, ]
    df <- df[df$estrato %in% as.numeric(input$estrato), ]
    if (input$zona != "Todas") {
      df <- df[df$zona == input$zona, ]
    }
    
    # Validar que queden datos
    validate(
      need(nrow(df) > 0, "No hay datos disponibles para los filtros seleccionados (Cero filas).")
    )
    
    df
  })
  
  # Salidas de la pestaña Explorar
  output$n_count <- renderText({ nrow(datos_filtrados()) })
  output$media_puntaje <- renderText({ round(mean(datos_filtrados()$puntaje, na.rm = TRUE), 1) })
  output$mediana_puntaje <- renderText({ round(median(datos_filtrados()$puntaje, na.rm = TRUE), 1) })
  
  output$boxplot_nat <- renderPlot({
    df <- datos_filtrados()
    ggplot(df, aes(x = naturaleza, y = puntaje, fill = naturaleza)) +
      geom_boxplot() +
      theme_minimal() +
      labs(x = "Naturaleza", y = "Puntaje Saber 11")
  })
  
  output$tabla_datos <- renderDT({
    datatable(datos_filtrados(), options = list(pageLength = 5))
  })
  
  # 2. Ajuste del modelo reactivo solo con botón (eventReactive)
  modelo_fit <- eventReactive(input$btn_ajustar, {
    req(input$predictores)
    df <- datos_filtrados()
    
    # Validación: verificar que cada predictor categórico tenga al menos 2 niveles en los datos filtrados
    for (pred in input$predictores) {
      if (is.factor(df[[pred]]) || is.character(df[[pred]])) {
        validate(
          need(length(unique(na.omit(df[[pred]]))) > 1,
               paste("El predictor", pred, "tiene un solo nivel en la muestra filtrada. No se puede ajustar lm()."))
        )
      }
    }
    
    formula <- as.formula(paste("puntaje ~", paste(input$predictores, collapse = " + ")))
    fit <- lm(formula, data = df)
    
    # Conteo de filas omitidas por NA
    n_inicial <- nrow(df)
    n_usado <- length(fit$residuals)
    n_omitidos <- n_inicial - n_usado
    
    list(fit = fit, n_usado = n_usado, n_omitidos = n_omitidos)
  })
  
  output$aviso_na <- renderUI({
    res <- modelo_fit()
    if (res$n_omitidos > 0) {
      div(class = "alert alert-warning mt-2",
          paste("Aviso:", res$n_omitidos, "filas con datos faltantes (NA) fueron descartadas."))
    }
  })
  
  output$coef_tabla <- renderTable({
    res <- modelo_fit()
    s <- summary(res$fit)
    coef_df <- as.data.frame(s$coefficients)
    coef_df$Término <- rownames(coef_df)
    
    # Formatear la tabla con coeficientes y métricas
    resumen_df <- data.frame(
      Término = c(coef_df$Término, "R² Ajustado", "n Usado"),
      Coeficiente = c(round(coef_df$Estimate, 2), round(s$adj.r.squared, 3), res$n_usado)
    )
    resumen_df
  })
  
  # 3. Historial de escenarios con reactiveValues
  historial <- reactiveValues(df = data.frame(
    Escenario = character(),
    Privado = numeric(),
    R2 = numeric(),
    n = integer(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$btn_guardar, {
    res <- modelo_fit()
    s <- summary(res$fit)
    
    coefs <- coef(res$fit)
    val_privado <- if ("naturalezaPrivado" %in% names(coefs)) round(coefs["naturalezaPrivado"], 2) else NA
    
    nuevo_escenario <- data.frame(
      Escenario = paste("Escenario", nrow(historial$df) + 1),
      Privado = val_privado,
      R2 = round(s$adj.r.squared, 3),
      n = res$n_usado
    )
    
    historial$df <- rbind(historial$df, nuevo_escenario)
  })
  
  output$historial_tabla <- renderTable({
    historial$df
  })
  
  output$download_historial <- downloadHandler(
    filename = function() { paste("historial_escenarios-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(historial$df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

