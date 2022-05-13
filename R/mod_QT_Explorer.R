
#' QT Explorer Module - UI
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

QT_Explorer_ui <- function(id) {
    ns <- NS(id)
    sidebar <- sidebarPanel(
        uiOutput(ns("selectMeasures")),
        selectizeInput(inputId = ns("plot_what"), label = "Plot observed or change?", choices = c("Observed", "Change"), selected = "Observed"),
        uiOutput(ns("selectSubgroups"))
    )
    main <- mainPanel(
        tabsetPanel(
            tabPanel("QT Central Tendency", plotlyOutput(ns("QT_meanPlot"), height = 800)),
            tabPanel("QT Vis",
			  fluidRow(uiOutput(ns("selectOutlierX")), "Click a point for individual data and line plots"),
			  fluidRow(plotlyOutput(ns("QT_OutlierExplorer"), height = 800)),
              fluidRow(tableOutput(ns("QT_OutlierExplorer_DrillDownTable"))),			  
			  fluidRow(plotlyOutput(ns("QT_OutlierExplorer_DrillDown")))			
			),          
            tabPanel("QT Data Info", verbatimTextOutput(ns("info")))
        )
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


#' QT Explorer Module - UI
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

QT_Explorer_server <- function(input, output, session, params) {
    ns <- session$ns

    rv <- reactiveValues() # filtered_data = params()$data$ecg )

    observe({
        rv$measure_col <- params()$settings$ecg$measure_col
        rv$measures <- unique(params()$data$ecg[[rv$measure_col]])
		
		rv$base_col <- params()$settings$ecg$base_col
		rv$change_col <- params()$settings$ecg$change_col
		rv$value_col <- params()$settings$ecg$value_col		

        rv$sex_col <- params()$settings$dm$sex_col
        rv$age_col <- params()$settings$dm$age_col
        rv$race_col <- params()$settings$dm$race_col

        rv$sex_vals <- unique(params()$data$dm[[rv$sex_col]])
        rv$race_vals <- unique(params()$data$dm[[rv$race_col]])
        rv$age_min <- min(params()$data$dm[[rv$age_col]], na.rm = TRUE)
        rv$age_max <- max(params()$data$dm[[rv$age_col]], na.rm = TRUE)
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

    # subgroup dropdowns
    output$selectSubgroups <- renderUI({
        tagList(
            selectizeInput(
                ns("sex"),
                "Sex",
                multiple = TRUE,
                choices = rv$sex_vals,
                selected = rv$sex_vals
            ),
            selectizeInput(
                ns("race"),
                "Race",
                multiple = TRUE,
                choices = rv$race_vals,
                selected = rv$race_vals
            ),
            sliderInput(
                ns("age"),
                "Age",
                min = rv$age_min,
                max = rv$age_max,
                step = 1,
                value = c(rv$age_min, rv$age_max)
            )
        )
    })


    # derive change from baseline

    ecg_data <- reactive({
 
        data_baseline <- params()$data$ecg %>%
            filter(.data[[params()$settings$ecg$baseline_flag_col]] == params()$settings$ecg$baseline_flag_values)
        
        vars_remove <- c("BASE", "BL", "CHG", "CHANGE")
        ecg <- params()$data$ecg %>% select( -one_of(vars_remove) ) # remove CHG and BASE from ecg, if any, to avoid conflict

        ecg_new <- data_baseline %>%
            mutate(BASE = .data[[params()$settings$ecg$value_col]]) %>%
            select(.data$BASE, params()$settings$ecg$id_col, params()$settings$ecg$measure_col) %>%
            right_join(
                ecg, 
                by = c(params()$settings$ecg$id_col, params()$settings$ecg$measure_col)
            ) %>%
            mutate(CHG = .data[[params()$settings$ecg$value_col]] - .data$BASE)
    })

	# Allow user to choose x axis variable (base/aval) for the outlier plot
	output$selectOutlierX <- renderUI({
      selectizeInput(
          inputId = ns("OutlierX"),
          "Select X-Axis Variable: ",
          multiple = FALSE,
          choices = list(rv$base_col, rv$value_col),
          selected = rv$base_col
      )
    })	

    # Populate control with measures and select all by default

    # customize selected measures based on input
    settingsR <- reactive({
        settings <- params()$settings$ecg
        settings$measure_values <- input$measures
        settings$group_col <- params()$settings$dm$group_col
        settings$plot_what <- input$plot_what
		settings$Outlier_X_var <- input$OutlierX		
        return(settings)
    })

    # data info

    output$info <- renderPrint({
        params()$data$ecg %>%
            count(.data[[params()$settings$ecg$visit_col]], .data[[params()$settings$ecg$tpt_col]], sort = FALSE) %>%
            data.frame()
    })


    observe({
        req(input$age, input$sex, input$race)

        dm_subset <- params()$data$dm %>%
            select(one_of(
                params()$settings$dm$id_col,
                params()$settings$dm$sex_col,
                params()$settings$dm$race_col,
                params()$settings$dm$age_col,
                params()$settings$dm$group_col
            ))

        rv$filter_ecg_data <- ecg_data() %>%
            select( # remove these variables if they exist in ecg, to avoid merge conflict
                -one_of(
                    params()$settings$dm$sex_col,
                    params()$settings$dm$race_col,
                    params()$settings$dm$age_col,
                    params()$settings$dm$group_col
                )
            ) %>%
            left_join(dm_subset, by = params()$settings$dm$id_col) %>%
            filter(
                .data[[rv$sex_col]] %in% input$sex,
                .data[[rv$race_col]] %in% input$race,
                .data[[rv$age_col]] >= input$age[1] & .data[[rv$age_col]] <= input$age[2]
            )
    })

    # outlier explorer chart
    output$QT_OutlierExplorer <- renderPlotly({
        req(input$measures)
        QT_Outlier_Explorer(rv$filter_ecg_data, settingsR())
    })
	
	# drill down plot for outlier explorer when point clicked on
	output$QT_OutlierExplorer_DrillDown <- renderPlotly({ 
        req(input$measures)	
		QT_Outlier_Explorer_Drill_Down(rv$filter_ecg_data, settingsR())
	})
	
	# drill down table for outlier explorer when point clicked on
	output$QT_OutlierExplorer_DrillDownTable <- renderTable({ 
        req(input$measures)	
		QT_Outlier_Explorer_Drill_Down_Table(params()$data$dm, settingsR())
	}, bordered=TRUE)		

    # central tendency
    output$QT_meanPlot <- renderPlotly({
        req(input$measures)       
        QT_Central_Tendency(rv$filter_ecg_data, settingsR())
    })
}