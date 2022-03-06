library(shiny)
library(qtexplorer)
library(dplyr)

ui <- tagList(
    fluidPage(
        h2("Example 1: Outlier explorer module - all values"),
        safetyOutlierExplorer_ui("example1"),
    )
)

adeg <- readr::read_csv("https://physionet.org/files/ecgcipa/1.0.0/adeg.csv?download") %>%
  mutate(ATPTFCT = forcats::fct_reorder(ATPT, .x = ATPTN, .fun = min)) %>%
  mutate(ANRHI=0, ANRLO=0)

settings <- list(
    visit_col='ATPT',
    visitn_col='ATPTN',
    measure_col = "PARAM",
    studyday_col = "ADY",
    value_col = "AVAL",
    id_col = "USUBJID"
)

params <- reactive({
    list(data = adeg, settings = settings)
})

server <- function(input, output, session) {
    callModule(safetyOutlierExplorer_server, "example1", params)
}


shinyApp(ui, server)
