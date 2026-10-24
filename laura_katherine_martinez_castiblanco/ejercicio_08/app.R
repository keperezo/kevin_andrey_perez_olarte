

library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# Tema visual Bootstrap 5 (bslib)
mi_tema <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1d4ed8",
  secondary = "#64748b"
)

# ------------------------------------------------------------------------------
# 2. INTERFAZ DE USUARIO (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = mi_tema,
  titlePanel(
    div(
      h2("Saber 11: ¿Efecto Colegio o Efecto Estrato?", class = "fw-bold text-primary mb-1"),
      p("Herramienta interactiva para la exploración y modelado econométrico bajo la metodología KDD.", class = "text-muted lead")
    )
  ),
  
  tabsetPanel(
    id = "pestañas_principales",
    type = "pills",
    
    # --------------------------------------------------------------------------
    # PESTAÑA 1: EXPLORAR DATOS
    # --------------------------------------------------------------------------
    tabPanel(
      title = "1. Explorar Datos",
      icon = icon("search"),
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h5("Filtros de Exploración", class = "fw-bold"),
          hr(),
          selectInput("filtro_depto", "Departamento:", 
                      choices = c("Todos", unique(datos_saber11$departamento)), 
                      selected = "Todos"),
          selectInput("filtro_zona", "Zona:", 
                      choices = c("Todas", "Urbana", "Rural"), 
                      selected = "Todas"),
          checkboxGroupInput("filtro_naturaleza", "Naturaleza del Colegio:",
                             choices = c("Oficial", "Privado"),
                             selected = c("Oficial", "Privado")),
          sliderInput("filtro_estrato", "Rango de Estrato:",
                      min = 1, max = 6, value = c(1, 6), step = 1),
          hr(),
          actionButton("btn_reset_filtros", "Restablecer Filtros", 
                       class = "btn-outline-secondary btn-sm w-100")
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(12,
                   div(class = "alert alert-info py-2 shadow-sm mb-3",
                       uiOutput("resumen_metricas_texto")
                   )
            )
          ),
          fluidRow(
            column(6,
                   h6("Distribución de Puntajes por Naturaleza", class = "fw-bold text-secondary"),
                   plotOutput("grafico_boxplot", height = "320px")
            ),
            column(6,
                   h6("Resumen Estadístico por Naturaleza", class = "fw-bold text-secondary"),
                   tableOutput("tabla_resumen_grupo")
            )
          ),
          hr(),
          h6("Vista Previa de la Muestra Filtrada", class = "fw-bold text-secondary"),
          DT::dataTableOutput("tabla_vista_datos")
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # PESTAÑA 2: MODELO E HISTORIAL
    # --------------------------------------------------------------------------
    tabPanel(
      title = "2. Modelo e Historial",
      icon = icon("calculator"),
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h5("Especificación del Modelo (lm)", class = "fw-bold"),
          hr(),
          p(strong("Ecuación Base:"), " puntaje ~ naturaleza"),
          p(class = "text-muted small", 
            "Nota: La variable 'naturaleza' siempre forma parte de la ecuación principal."),
          hr(),
          checkboxGroupInput(
            "covariables", "Agregar Covariables al Modelo:",
            choices = c(
              "Estrato Socioeconómico" = "estrato",
              "Horas de Estudio"       = "horas_estudio",
              "Zona (Urbana/Rural)"    = "zona",
              "Departamento"           = "departamento"
            ),
            selected = NULL
          ),
          hr(),
          actionButton("btn_ajustar", "Ajustar Modelo", class = "btn-primary w-100 fw-bold shadow-sm"),
          br(), br(),
          downloadButton("btn_descargar_historial", "Descargar Historial (CSV)", 
                         class = "btn-success btn-sm w-100"),
          br(), br(),
          actionButton("btn_limpiar_historial", "Limpiar Historial", 
                       class = "btn-outline-danger btn-sm w-100")
        ),
        mainPanel(
          width = 9,
          uiOutput("alerta_missing_obs"),
          h5("Salida Detallada de la Regresión Lineal (summary)", class = "fw-bold text-primary"),
          verbatimTextOutput("salida_resumen_lm"),
          hr(),
          h5("Historial Acumulado de Escenarios", class = "fw-bold text-success"),
          DT::dataTableOutput("tabla_historial_escenarios")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # A. Reactive centralizado con validaciones informativas (sin gráficos vacíos)
  datos_filtrados <- reactive({
    # Validación A1: Desmarcar ambas naturalezas (Evita gráfico vacío)
    validate(
      need(
        !is.null(input$filtro_naturaleza) && length(input$filtro_naturaleza) > 0,
        "⚠️ No se seleccionó ninguna categoría en 'Naturaleza del Colegio'.\nPor favor marque 'Oficial', 'Privado' o ambas en el panel lateral para desplegar los datos y gráficos."
      )
    )
    
    df <- datos_saber11 %>%
      filter(
        naturaleza %in% input$filtro_naturaleza,
        estrato >= input$filtro_estrato,
        estrato <= input$filtro_estrato
      )
    
    if (input$filtro_depto != "Todos") {
      df <- df %>% filter(departamento == input$filtro_depto)
    }
    
    if (input$filtro_zona != "Todas") {
      df <- df %>% filter(zona == input$filtro_zona)
    }
    
    # Validación A2: Muestra vacía por combinación de filtros (Ej: Zona Rural + Estrato 5 a 6)
    validate(
      need(
        nrow(df) > 0,
        "⚠️ Muestra vacía: La combinación actual de filtros (departamento, zona, estrato, naturaleza) no retornó ningún estudiante en la base de datos."
      )
    )
    
    return(df)
  })
  
  # Reset de filtros
  observeEvent(input$btn_reset_filtros, {
    updateSelectInput(session, "filtro_depto", selected = "Todos")
    updateSelectInput(session, "filtro_zona", selected = "Todas")
    updateCheckboxGroupInput(session, "filtro_naturaleza", selected = c("Oficial", "Privado"))
    updateSliderInput(session, "filtro_estrato", value = c(1, 6))
  })
  
  # Outputs de la pestaña 1 (Explorar)
  output$resumen_metricas_texto <- renderUI({
    df <- datos_filtrados()
    n_tot <- nrow(df)
    mediana_p <- median(df$puntaje, na.rm = TRUE)
    media_p <- round(mean(df$puntaje, na.rm = TRUE), 1)
    
    HTML(paste0(
      "<strong>Muestra Filtrada:</strong> ", n_tot, " observaciones | ",
      "<strong>Puntaje Mediano:</strong> ", mediana_p, " puntos | ",
      "<strong>Puntaje Promedio:</strong> ", media_p, " puntos"
    ))
  })
  
  output$grafico_boxplot <- renderPlot({
    df <- datos_filtrados()
    ggplot(df, aes(x = naturaleza, y = puntaje, fill = naturaleza)) +
      geom_boxplot(alpha = 0.75, outlier.colour = "red") +
      theme_minimal(base_size = 13) +
      scale_fill_manual(values = c("Oficial" = "#2563eb", "Privado" = "#16a34a")) +
      labs(x = "Naturaleza del Colegio", y = "Puntaje Global Saber 11") +
      theme(legend.position = "none")
  })
  
  output$tabla_resumen_grupo <- renderTable({
    df <- datos_filtrados()
    df %>%
      group_by(Naturaleza = naturaleza) %>%
      summarise(
        n = n(),
        Media = round(mean(puntaje, na.rm = TRUE), 1),
        Mediana = median(puntaje, na.rm = TRUE),
        Desv_Std = round(sd(puntaje, na.rm = TRUE), 1),
        .groups = "drop"
      )
  }, digits = 1)
  
  output$tabla_vista_datos <- DT::renderDataTable({
    DT::datatable(
      datos_filtrados(),
      options = list(pageLength = 5, autoWidth = TRUE, dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # B. ReactiveValues para el estado del historial
  historial_rv <- reactiveValues(
    df = data.frame(
      Escenario = character(),
      Filtro_Departamento = character(),
      Coef_Privado = numeric(),
      R2_Ajustado = numeric(),
      n_obs = integer(),
      Variables_Ajustadas = character(),
      stringsAsFactors = FALSE
    ),
    contador = 0,
    filas_descartadas = 0
  )
  
  # C. EventReactive bajo demanda con validación explícita de niveles
  modelo_event <- eventReactive(input$btn_ajustar, {
    df <- datos_filtrados()
    covars_sel <- input$covariables
    depto_sel <- input$filtro_depto
    
    # Validación B1: Verificar que la muestra contenga EXACTAMENTE los 2 niveles (Oficial y Privado)
    validate(
      need(
        length(unique(df$naturaleza)) == 2,
        paste0(
          "❌ IMPOSIBLE EJECUTAR EL MODELO lm()\n\n",
          "La muestra filtrada contiene únicamente la categoría '", unique(df$naturaleza), "'.\n\n",
          "Causa Técnica: Para estimar el coeficiente del predictor 'naturaleza' (puntaje ~ naturaleza + ...), ",
          "el algoritmo de regresión lineal requiere obligatoriamente que la muestra contenga colegios 'Oficial' y 'Privado' al mismo tiempo.\n\n",
          "Solución: Vaya a la pestaña '1. Explorar Datos', marque ambas casillas ('Oficial' y 'Privado') en el filtro de Naturaleza y presione nuevamente 'Ajustar Modelo'."
        )
      )
    )
    
    # Construcción dinámica de la fórmula
    formula_vars <- c("naturaleza", covars_sel)
    formula_fit <- as.formula(paste("puntaje ~", paste(formula_vars, collapse = " + ")))
    
    # Ajuste MCO
    fit <- lm(formula_fit, data = df)
    resumen <- summary(fit)
    
    # Cómputo del n exacto
    n_usado <- nobs(fit)
    n_descartado <- nrow(df) - n_usado
    
    # Extracción del coeficiente del nivel "Privado"
    coef_priv <- if ("naturalezaPrivado" %in% rownames(resumen$coefficients)) {
      round(resumen$coefficients["naturalezaPrivado", "Estimate"], 2)
    } else {
      NA_real_
    }
    
    # Actualizar contador e historial
    historial_rv$contador <- historial_rv$contador + 1
    historial_rv$filas_descartadas <- n_descartado
    
    nuevo_registro <- data.frame(
      Escenario = paste("Escenario", historial_rv$contador),
      Filtro_Departamento = depto_sel,
      Coef_Privado = coef_priv,
      R2_Ajustado = round(resumen$adj.r.squared, 4),
      n_obs = as.integer(n_usado),
      Variables_Ajustadas = paste(formula_vars, collapse = " + "),
      stringsAsFactors = FALSE
    )
    
    historial_rv$df <- rbind(historial_rv$df, nuevo_registro)
    
    return(fit)
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE ejecuta el Escenario 1 automático al cargar la app
  
  # Reset del Historial
  observeEvent(input$btn_limpiar_historial, {
    historial_rv$df <- data.frame(
      Escenario = character(),
      Filtro_Departamento = character(),
      Coef_Privado = numeric(),
      R2_Ajustado = numeric(),
      n_obs = integer(),
      Variables_Ajustadas = character(),
      stringsAsFactors = FALSE
    )
    historial_rv$contador <- 0
    historial_rv$filas_descartadas <- 0
  })
  
  # D. Outputs de la pestaña 2 (Modelo e Historial)
  output$alerta_missing_obs <- renderUI({
    fit <- tryCatch(modelo_event(), error = function(e) NULL)
    req(fit)
    n_desc <- historial_rv$filas_descartadas
    
    if (n_desc > 0) {
      div(class = "alert alert-warning py-2 mb-3 shadow-sm",
          HTML(paste0("<strong>Aviso de Limpieza (Missingness):</strong> <code>lm()</code> descartó <strong>", 
                      n_desc, " observaciones</strong> por valores faltantes (NA) en las covariables seleccionadas (p. ej. en <code>horas_estudio</code>).")))
    } else {
      div(class = "alert alert-success py-2 mb-3 shadow-sm",
          HTML("<strong>Calidad de Datos:</strong> El modelo utilizó el 100% de los registros de la muestra filtrada (0 datos faltantes)."))
    }
  })
  
  output$salida_resumen_lm <- renderPrint({
    summary(modelo_event())
  })
  
  output$tabla_historial_escenarios <- DT::renderDataTable({
    DT::datatable(
      historial_rv$df,
      options = list(dom = 't', ordering = FALSE, pageLength = 100),
      rownames = FALSE
    )
  })
  
  output$btn_descargar_historial <- downloadHandler(
    filename = function() {
      paste0("historial_escenarios_saber11_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(historial_rv$df, file, row.names = FALSE)
    }
  )
}

# ------------------------------------------------------------------------------
# 4. EJECUCIÓN
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


