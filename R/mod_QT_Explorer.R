
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
        selectizeInput(inputId = ns("plot_what"), label = "Select Y-Axis Variable", choices = c("Observed", "Change"), selected = "Observed"),
		uiOutput(ns("selectOutlierX")),
        uiOutput(ns("RefLines"))
    )
    main <- mainPanel(
        tabsetPanel(
            tabPanel("All Observations",
			  fluidRow("Click a point for individual data and line plots"),
			  fluidRow(plotlyOutput(ns("QT_OutlierExplorer_Overall"), height = 800)),
              fluidRow(tableOutput(ns("QT_OutlierExplorer_DrillDownTable_Overall"))),	  
			  fluidRow(plotlyOutput(ns("QT_OutlierExplorer_DrillDown_Overall")))			
			), 
            tabPanel("Observations Over Time",
			  fluidRow("Click a point for individual data and line plots"),			  
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
	
	#allow user to remove reference lines (conditional on y axis variable)
	Reflines_choices <- reactive({
	  if(input$plot_what == "Observed"){
	      c("QTc Interval > 450",
          "QTc Interval > 480",
          "QTc Interval > 500")
		  }
	  else if(input$plot_what == "Change"){
	      c("QTc Change from Baseline > 30",
          "QTc Change from Baseline > 60",
		  "QTc Change from Baseline x=y Line")
		  }
	})	
	output$RefLines <- renderUI({
		#add checkboxes for user to remove reference lines
		checkboxGroupInput(inputId = ns("RefLines"), label= "Select Reference Lines to Display:", 
		                   choices= Reflines_choices(),
					       selected = Reflines_choices())
	})

    # Populate control with measures and select all by default

    # customize selected measures based on input
    settingsR <- reactive({
        settings <- params()$settings$ecg
        settings$measure_values <- input$measures
        settings$group_col <- params()$settings$dm$group_col
        settings$plot_what <- input$plot_what
		settings$Outlier_X_var <- input$OutlierX		
		settings$RefLines <- input$RefLines			
        return(settings)
    })

    # data info

    output$info <- renderPrint({
        params()$data$ecg %>%
            count(.data[[params()$settings$ecg$visit_col]], .data[[params()$settings$ecg$tpt_col]], sort = FALSE) %>%
            data.frame()
    })


    observe({

        dm_subset <- params()$data$dm %>%
            select(one_of(
                params()$settings$dm$id_col,
                params()$settings$dm$group_col
            ))

        rv$filter_ecg_data <- ecg_data() %>%
            select( # remove these variables if they exist in ecg, to avoid merge conflict
                -one_of(
                    params()$settings$dm$group_col
                )
            ) %>%
            left_join(dm_subset, by = params()$settings$dm$id_col)
    })

    # outlier explorer chart - overall 
    output$QT_OutlierExplorer_Overall <- renderPlotly({
        req(input$measures)
        QT_Outlier_Explorer_Overall(rv$filter_ecg_data, settingsR())
    })

    # outlier explorer chart - animation slider over time
    output$QT_OutlierExplorer <- renderPlotly({
        req(input$measures)
        QT_Outlier_Explorer(rv$filter_ecg_data, settingsR())
    })
	
	# drill down plot for outlier explorer when point clicked on - for overall plot
	output$QT_OutlierExplorer_DrillDown_Overall <- renderPlotly({ 
        req(input$measures)	
		QT_Outlier_Explorer_Drill_Down(rv$filter_ecg_data, settingsR())
	})	
	
	# drill down plot for outlier explorer when point clicked on
	output$QT_OutlierExplorer_DrillDown <- renderPlotly({ 
        req(input$measures)	
		QT_Outlier_Explorer_Drill_Down(rv$filter_ecg_data, settingsR())
	})

	# drill down table for outlier explorer when point clicked on - for overall plot
	output$QT_OutlierExplorer_DrillDownTable_Overall <- renderTable({ 
        req(input$measures)	
		QT_Outlier_Explorer_Drill_Down_Table(params()$data$dm, settingsR())
	}, bordered=TRUE)	
	
	# drill down table for outlier explorer when point clicked on
	output$QT_OutlierExplorer_DrillDownTable <- renderTable({ 
        req(input$measures)	
		QT_Outlier_Explorer_Drill_Down_Table(params()$data$dm, settingsR())
	}, bordered=TRUE)		

}