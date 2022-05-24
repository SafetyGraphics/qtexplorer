
#' QT central tendancy dule - UI
#'
#' @param id module id
#'
#' @return returns shiny module UI
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#'
#' @export
#'

QT_central_tendancy_ui <- function(id) {
    ns <- NS(id)
    sidebar <- sidebarPanel(
        uiOutput(ns("selectMeasures")),
        selectizeInput(inputId = ns("plot_what"), label = "Select Y-Axis Variable", choices = c("Observed", "Change"), selected = "Observed")
    )

    main <- mainPanel(
       plotlyOutput(ns("QT_meanPlot"), height = 800)
    )
    
    ui <- fluidPage(
        sidebarLayout(
            sidebar,
            main,
            position = c("right"),
            fluid = TRUE
        )
    )
    return(ui)
}


#' QT Central Tendancy Module - Server
#'
#' @param input module input
#' @param output module output
#' @param session module session
#' @param params parameters object with `data` and `settings` options.
#'
#' @return returns shiny module Server function
#'
#' @import shiny
#' @importFrom plotly renderPlotly ggplotly
#'
#' @export

QT_central_tendancy_server <- function(input, output, session, params) {
    ns <- session$ns

    rv <- reactiveValues() # filtered_data = params()$data$ecg )

    observe({
        rv$measure_col <- params()$settings$ecg$measure_col
        rv$measures <- unique(params()$data$ecg[[rv$measure_col]])

        rv$base_col <- params()$settings$ecg$base_col
        rv$change_col <- params()$settings$ecg$change_col
        rv$value_col <- params()$settings$ecg$value_col

     })

    output$selectMeasures <- renderUI({
        req(rv$measures)
        selectizeInput(
            ns("measures"),
            "Select Measures",
            multiple = FALSE,
            choices = rv$measures,
            selected = "QTcF"
        )
    })

    

    # derive change from baseline

    ecg_data <- reactive({
        data_baseline <- params()$data$ecg %>%
            filter(.data[[params()$settings$ecg$baseline_flag_col]] == params()$settings$ecg$baseline_flag_values)

        vars_remove <- c("BASE", "BL", "CHG", "CHANGE")
        ecg <- params()$data$ecg %>% select(-one_of(vars_remove)) # remove CHG and BASE from ecg, if any, to avoid conflict

        ecg_new <- data_baseline %>%
            mutate(BASE = .data[[params()$settings$ecg$value_col]]) %>%
            select(.data$BASE, params()$settings$ecg$id_col, params()$settings$ecg$measure_col) %>%
            right_join(
                ecg,
                by = c(params()$settings$ecg$id_col, params()$settings$ecg$measure_col)
            ) %>%
            mutate(CHG = .data[[params()$settings$ecg$value_col]] - .data$BASE)
    })

    
    # Populate control with measures and select all by default

    # customize selected measures based on input
    settingsR <- reactive({
        settings <- params()$settings$ecg
        settings$measure_values <- input$measures
        settings$group_col <- params()$settings$dm$group_col
        settings$plot_what <- input$plot_what
          return(settings)
    })

    # central tendency
    output$QT_meanPlot <- renderPlotly({
        req(input$measures)
        QT_Central_Tendency(ecg_data(), settingsR())
    })
}