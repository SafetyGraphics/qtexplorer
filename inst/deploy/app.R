library(tidyverse)
library(safetyGraphics) # need new safetyGraphics and safetyCharts with makeMeta()
library(shinyjs)
library(shiny)

library(qtexplorer)
# devtools::load_all()

# test qtexplorer
qtCharts <- safetyGraphics::makeChartConfig(packages = "qtexplorer") %>%
  purrr::map(function(chart) {
    chart$order <- 1
    return(chart)
  })

length(qtCharts)

# meta1 <- makeMeta(qtCharts)


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
  sex_col: SEX
  race_col: RACE
  age_col: AGE
"
)

eg_ph2_new <- qtexplorer::eg_ph2 %>%
  mutate(BASEFL = if_else(DAY == 1, 1, 0)) %>%
  mutate(PARAM = if_else(grepl("qtcf", PARAM, ignore.case = TRUE), "QTcF", PARAM))



# TQT ADaM exmaple
adeg_new <- qtexplorer::adeg %>%
  mutate(ANRHI = 0, ANRLO = 0)


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
  sex_col: SEX
  race_col: RACE
  age_col: AGE
"
)


ui <- fluidPage(
  useShinyjs(),
  tags$div(
    id = ("div_dataloader"),
    h4("Data Loader"),
    selectInput("dataSel", "Select Data", choices = c("TQT ADaM Example", "Non TQT Ph2"), selected = "Non TQT Ph2"),
    actionButton("runApp", "Run App", icon("paper-plane"),
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    tags$hr(),
    wellPanel(
      uiOutput("dataDescription"),
      uiOutput("DataPreview")
    )
  ),
  shinyjs::hidden(
    tags$div(
      id = ("div_sg_app"),
      tags$div(
        actionLink(("back2dataloader"), label = "Reload Data", icon("undo"), class = "header-btn")
      ),
      uiOutput("sg_cond")
    )
  )
)



server <- function(input, output, session) {
  output$dataDescription <- renderUI({
    if (input$dataSel == "Non TQT Ph2") {
      tagList(
        tags$a("Link to Ph2b Example data description",
          href = "https://github.com/SafetyGraphics/qtexplorer/blob/dev/data-raw/Phase2_Example_Description.docx?raw=true",
          target = "_blank"
        ),
        tags$br(),
        tags$hr()
      )
    } else if (input$dataSel == "TQT ADaM Example") {
      # Sample ADEG data
      # https://physionet.org/content/ecgcipa/1.0.0/
      # https://physionet.org/content/ecgcipa/1.0.0/adeg.csv
      tagList(
        h3("CiPA ECG Validation Study: FDA Thorough QT Example"),
        tags$a("LINK to Study Description and Data", href = "https://physionet.org/content/ecgcipa/1.0.0/", target = "_blank"),
        tags$br(),
        tags$hr()
      )
    }
  })


  output$DataPreview <- renderUI({
    req(v$list_dm, v$list_ecg)
    tagList(
      tabsetPanel(
        tabPanel(title = "DM", DT::DTOutput("dmDT")),
        tabPanel(title = "ECG", DT::DTOutput("ecgDT"))
      )
    )
  })

  output$dmDT <- DT::renderDT(v$list_dm[[1]])
  output$ecgDT <- DT::renderDT(v$list_ecg[[1]])

  shinyjs::disable("runApp")


  charts_init <- qtCharts
  delayTime <- 3000
  maxFileSize <- NULL

  v <- reactiveValues(
    list_from_dataloader = NULL,
    list_dm = NULL,
    list_ecg = NULL,
    mapping = NULL
  )


  observe({
    if (input$dataSel == "Non TQT Ph2") {
      v$list_dm <- list(dm = qtexplorer::dm_ph2)
      v$list_ecg <- list(ecg = eg_ph2_new)
      v$mapping <- mapping_ph2
    } else if (input$dataSel == "TQT ADaM Example") {
      v$list_dm <- list(dm = qtexplorer::adsl)
      v$list_ecg <- list(ecg = adeg_new)
      v$mapping <- mapping_TQT
    }

    v$list_from_dataloader <- c(v$list_dm, v$list_ecg)
    sum_length_list <- sum(unlist(lapply(v$list_from_dataloader, length)))
    if (sum_length_list > 0) {
      shinyjs::enable("runApp")
    }
  })



  initStatus <- reactive({
    ready <- FALSE
    length_dD_list <- sum(do.call(rbind, lapply(v$list_from_dataloader, function(x) length(x))))
    if (length_dD_list > 0) {
      ready <- TRUE
    }
    return(list(ready = ready))
  })


  observe({
    if (initStatus()$ready) {
      shinyjs::enable(id = "runApp")
    } else {
      shinyjs::disable(id = "runApp")
    }
  })





  observeEvent(input$runApp, {
    print(v$list_from_dataloader)
    req(v$list_from_dataloader)

    shinyjs::hide(id = "div_dataloader")
    shinyjs::show(id = "div_sg_app")

    config <- app_startup(
      domainData = v$list_from_dataloader %>% keep(~ !is.null(.x)),
      meta = NULL,
      charts = charts_init,
      filterDomain = "dm",
      mapping = v$mapping,
      autoMapping = FALSE
    )



    output$sg_cond <- renderUI({
      if (input$dataSel == "Non TQT Ph2") {
        safetyGraphicsUI(
          "sg1",
          config$meta,
          config$domainData,
          config$mapping,
          config$standards
        )
      } else if (input$dataSel == "TQT ADaM Example") {
        safetyGraphicsUI(
          "sg2",
          config$meta,
          config$domainData,
          config$mapping,
          config$standards
        )
      }
    })


    # delay is needed to get the appendTab in mod_chartsNav to trigger properly

    if (input$dataSel == "Non TQT Ph2") {
      shinyjs::delay(
        delayTime,
        callModule(
          safetyGraphicsServer,
          "sg1",
          config$meta,
          config$mapping,
          config$domainData,
          config$charts,
          config$filterDomain
        )
      )
    } else if (input$dataSel == "TQT ADaM Example") {
      shinyjs::delay(
        delayTime,
        callModule(
          safetyGraphicsServer,
          "sg2",
          config$meta,
          config$mapping,
          config$domainData,
          config$charts,
          config$filterDomain
        )
      )
    }
  })

  observeEvent(input$back2dataloader, {
    shinyjs::hide(id = "div_sg_app")
    shinyjs::show(id = "div_dataloader")
    v$mapping <- NULL
    v$list_from_dataloader <- NULL
  })
}

app <- shinyApp(ui = ui, server = server)
app