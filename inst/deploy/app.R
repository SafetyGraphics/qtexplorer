library(tidyverse)
library(safetyGraphics) # need new safetyGraphics and safetyCharts with makeMeta()
library(shinyjs)
library(shiny)
devtools::load_all()

# test qtexplorer
qtCharts <- makeChartConfig(
     dirs = "./inst/config/", packages = NULL, packageLocation = NULL)  %>% 
    purrr::map(function(chart) {
        chart$order <- 1
        return(chart)
    })

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
