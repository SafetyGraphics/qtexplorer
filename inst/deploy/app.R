library(tidyverse)
library(safetyGraphics) # need new safetyGraphics and safetyCharts with makeMeta()
# library(safetyCharts)
#library(qtexplorer)
library(shinyjs)
library(shiny)


devtools::load_all()

# test qtexplorer
qtCharts <- makeChartConfig(#packages = "qtexplorer") %>%
     dirs = "./inst/config/", packages = NULL, packageLocation = NULL)  %>% 
    purrr::map(function(chart) {
        chart$order <- 1
        return(chart)
    })


#qtCharts <- makeChartConfig(packages = "qtexplorer") %>%
#    # dirs = "./inst/config/", packages = NULL, packageLocation = NULL
#    purrr::map(function(chart) {
#        chart$order <- 1
#        return(chart)
#    })

length(qtCharts)

mapping_TQT <- yaml::read_yaml(
    text = "
ecg:
  id_col: USUBJID
  value_col: AVAL
  measure_col: PARAM
  measure_values:
    QT: QT
    QTcF: QTcF
    QTcB: ''
    RR: RR
    QRS: QRS
  normal_col_low: ''
  normal_col_high: ''
  studyday_col: ADY
  visit_col: ATPT
  visitn_col: ATPTN
  tpt_col: ATPT
  tptn_col: ATPTN
  period_col: APERIOD
  unit_col: ''
  baseline_flag_col: ABLFL
  baseline_flag_values: 'Y'
  treatment_col: TRTA
  analysis_flag_col: ''
  analysis_flag_values: ''
dm:
  id_col: USUBJID
  group_col: ARM
  group_values:
  sex_col: SEX
  race_col: RACE
  age_col: AGE
"
)


meta1 <- makeMeta(qtCharts)

# safetyGraphicsApp(
#    charts = qtCharts,
#    domainData = list(ecg = qtexplorer::adeg, dm = qtexplorer::adsl),
#    # meta = list(qtexplorer::meta_ecg, qtexplorer::meta_dm), #new makeMeta function()
#    mapping = mappingsTQT
# )


# eg_ph2 data
mapping_ph2 <- yaml::read_yaml(
    text = "
ecg:
  id_col: ID
  value_col: VALUE
  measure_col: PARAM
  measure_values:
    QT: ''
    QTcF: QTcF
    QTcB: ''
    RR: HR
    QRS: ''
  normal_col_low: ''
  normal_col_high: ''
  studyday_col: DAY
  visit_col: VISIT
  visitn_col: DAY
  tpt_col: TIME
  tptn_col: TIME
  period_col: ''
  unit_col: ''
  baseline_flag_col: 'BASEFL'
  baseline_flag_values: '1'
  treatment_col: TREAT
  analysis_flag_col: ''
  analysis_flag_values: ''
dm:
  id_col: ID
  group_col: TREAT
  group_values:
    group1: A
    group2: B
  sex_col: SEX
  race_col: RACE
  age_col: AGE
"
)

eg_ph2_new <- qtexplorer::eg_ph2 %>%
    mutate(BASEFL = if_else(DAY == 1, 1, 0)) %>%
    mutate(PARAM = if_else(grepl("qtcf", PARAM, ignore.case = TRUE), "QTcF", PARAM))  %>% 
    left_join(qtexplorer::dm_ph2  %>% select(-TREAT) %>% rename(age=AGE, sex=SEX, race=RACE), by = "ID" )

 safetyGraphicsApp(
    charts = qtCharts,
    domainData = list(ecg = eg_ph2_new, dm = qtexplorer::dm_ph2),
    # meta = safetyCharts::meta_ecg,
    mapping = mapping_ph2
    , runNow = FALSE
 )
#
#
#
#
#ui <- fluidPage(
#    sidebarLayout(
#        position = "left",
#        sidebarPanel(
#            width = 2,
#            h4("Data Loader"),
#            # all_domains %>% map(~loadDataUI(.x, domain=.x)),
#            selectInput("dataSel", "Select Data", choices = c("ADaM Sample", "Ph2b Sample"), selected = "Ph2b Sample"),
#            hr(),
#            actionButton("runApp", "Run App", class = "btn-block"),
#            actionButton("reset", "Reset", class = "btn-block")
#        ),
#        mainPanel(
#            width = 10,
#            uiOutput("sg")
#        )
#    )
#)
#
#
#
#
#server <- function(input, output, session) {
#    ns <- session$ns
#
#
#    rv <- reactiveValues()
#
#
#    observeEvent(input$reset, {
#        rv$config <- NULL
#        rv$reset <- 1
#    })
#
#
#    observeEvent(
#        {
#            input$runApp
#            #!input$reset
#        },
#        {
#            
#            domainData <- reactive({
#                req(input$dataSel)
#                if (input$dataSel == "ADaM Sample") {
#                    d <- list(ecg = qtexplorer::adeg, dm = qtexplorer::adsl)
#                } else {
#                    d <- list(ecg = eg_ph2_new, dm = qtexplorer::dm_ph2)
#                }
#                d
#            })
#
#
#            qtMapping <- reactive({
#                req(input$dataSel)
#                if (input$dataSel == "ADaM Sample") {
#                    mapping <- mapping_TQT
#                } else {
#                    mapping <- mapping_ph2
#                }
#                mapping
#            })
#
#            rv$config <-
#                app_startup(
#                    domainData = domainData() %>% keep(~ !is.null(.x)),
#                    meta = NULL, # meta1,
#                    charts = qtCharts,
#                    mapping = qtMapping(),
#                    # autoMapping = TRUE,
#                    filterDomain = "dm",
#                    autoMapping = FALSE
#                    # chartSettingsPaths = NULL
#                )
#
#            # observe(print(config))
#            # cf <<- config
#
#            output$sg <- renderUI({
#                req(rv$config)
#                safetyGraphicsUI(
#                    "sg",
#                    rv$config$meta,
#                    rv$config$domainData,
#                    rv$config$mapping,
#                    rv$config$standards
#                )
#            })
#
#            delayTime <- 1000
#
#            shinyjs::delay(
#                delayTime,
#                callModule(
#                    safetyGraphicsServer,
#                    "sg",
#                    rv$config$meta,
#                    rv$config$mapping,
#                    rv$config$domainData,
#                    rv$config$charts,
#                    rv$config$filterDomain
#                )
#            )
#        }
#    )
#}
#
# shinyApp(ui = ui, server = server)

# TODO safetyGraphicsInit()

# charts_init <- qtCharts
# all_domains <- charts_init %>%
#    map(~ .x$domain) %>%
#    unlist() %>%
#    unique()
#
#
# css_path <- system.file("www", "index.css", package = "safetyGraphics")
# app_css <- HTML(readLines(css_path))
#
# ui<-fluidPage(
#    useShinyjs(),
#    tags$head(tags$style(app_css)),
#    div(
#      id="init",
#      titlePanel("qtexplorer Initializer"),
#      sidebarLayout(
#        position="right",
#        sidebarPanel(
#          h4("Data Loader"),
#          all_domains %>% map(~loadDataUI(.x, domain=.x)),
#          textOutput("dataSummary"),
#          hr(),
#          shinyjs::disabled(
#            actionButton("runApp","Run App",class = "btn-block")
#          )
#        ),
#        mainPanel(
#          p(
#            icon("info-circle"),
#            "First, select charts by dragging items between the lists below. Next, load the required data domains using the controls on the right. Finally, click Run App to start the safetyGraphics Shiny App. Reload the webpage to select new charts/data.",
#            class="info"
#          ),
#          loadChartsUI("load-charts", charts=charts_init),
#        )
#      ),
#    ),
#    shinyjs::hidden(
#      div(
#        id="sg-app",
#        uiOutput("sg")
#      )
#    )
#  )
#
#
# server <- function(input, output, session) {
#    # initialize the chart selection moduls
#    charts <- callModule(loadCharts, "load-charts", charts = charts_init)
#    domainDataR <- all_domains %>% map(~ callModule(loadData, .x, domain = .x))
#    names(domainDataR) <- all_domains
#    domainData <- reactive({
#        domainDataR %>% map(~ .x())
#    })
#
#
#    current_domains <- reactive({
#        charts() %>%
#            map(~ .x$domain) %>%
#            unlist() %>%
#            unique()
#    })
#
#    observe({
#        for (domain in all_domains) {
#            if (domain %in% current_domains()) {
#                shinyjs::show(id = paste0(domain, "-wrap"))
#            } else {
#                shinyjs::hide(id = paste0(domain, "-wrap"))
#            }
#        }
#    })
#
#    initStatus <- reactive({
#        currentData <- domainData()
#        chartCount <- length(charts())
#        domainCount <- length(current_domains())
#        loadCount <- sum(currentData %>% map_lgl(~ !is.null(.x)))
#        notAllLoaded <- sum(currentData %>% map_lgl(~ !is.null(.x))) < domainCount
#        ready <- FALSE
#        if (domainCount == 0) {
#            status <- paste("No charts selected. Select one or more charts and then load domain data to initilize app.")
#        } else if (notAllLoaded) {
#            status <- paste(chartCount, " charts selected. ", loadCount, " of ", domainCount, " data domains loaded. Load remaining data domains to initialize app.")
#        } else {
#            status <- paste("Loaded ", loadCount, " data domains for ", chartCount, " charts. Click 'Run App' button to initialize app.")
#            ready <- TRUE
#        }
#        return(
#            list(
#                status = status,
#                ready = ready
#            )
#        )
#    })
#
#    output$dataSummary <- renderText({
#        initStatus()$status
#    })
#    observe({
#        if (initStatus()$ready) {
#            shinyjs::enable(id = "runApp")
#        } else {
#            shinyjs::disable(id = "runApp")
#        }
#    })
#
#    observeEvent(input$runApp, {
#        shinyjs::hide(id = "init")
#        shinyjs::show(id = "sg-app")
#        config <- app_startup(
#            domainData = domainData() %>% keep(~ !is.null(.x)),
#            meta = NULL,
#            charts = charts(),
#            # mapping=NULL,
#            filterDomain = "dm",
#            autoMapping = TRUE,
#            # chartSettingsPaths = NULL
#        )
#
#        output$sg <- renderUI({
#            safetyGraphicsUI(
#                "sg",
#                config$meta,
#                config$domainData,
#                config$mapping,
#                config$standards
#            )
#        })
#
#        # delay is needed to get the appendTab in mod_chartsNav to trigger properly
#        shinyjs::delay(
#            delayTime,
#            callModule(
#                safetyGraphicsServer,
#                "sg",
#                config$meta,
#                config$mapping,
#                config$domainData,
#                config$charts,
#                config$filterDomain
#            )
#        )
#    })
# }
#
# app <- shinyApp(ui = ui, server = server)
#
# runApp(app, launch.browser = TRUE)
