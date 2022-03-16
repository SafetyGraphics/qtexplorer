library(tidyverse)
library(safetyGraphics) # need new safetyGraphics and safetyCharts with makeMeta()
# library(safetyCharts)
library(qtexplorer)


# test qtexplorer
qt1 <- makeChartConfig( # packages = "qtexplorer"
  dirs = "./inst/config/", packages = NULL, packageLocation = NULL
) %>%
  purrr::map(function(chart) {
    chart$order <- 1
    return(chart)
  })

length(qt1)

mappingsQT <- list(
  ecg = list(
    "visit_col" = "ATPT",
    "visitn_col" = "ATPTN"
  ),
  dm = list(
    "treatment_col" = "ARM"
  )
)

meta1 <- makeMeta(qt1)

safetyGraphicsApp(
  charts = qt1,
  domainData = list(ecg = qtexplorer::adeg, dm = qtexplorer::adsl),
  # meta = list(qtexplorer::meta_ecg, qtexplorer::meta_dm), #new makeMeta function()
  mapping = mappingsQT
)


# eg_ph2 data
mapping_ph2 <- yaml::read_yaml(
  text =
    "
ecg:
  id_col: ID
  value_col: VALUE
  measure_col: PARAM
  measure_values:
    QT: ''
    QTcF: QTCF
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
  treatment_col: TREAT
  treatment_values:
    group1: A
    group2: B
  sex_col: SEX
  race_col: RACE
  age_col: AGE
"
)

eg_ph2_new <- qtexplorer::eg_ph2 %>%
  mutate(BASEFL = if_else(DAY==1, 1, 0))  %>% 
  mutate(PARAM = if_else( grepl("qtcf", PARAM, ignore.case = TRUE), "QTcF", PARAM) )

safetyGraphicsApp(
  charts = qt1,
  domainData = list(ecg = eg_ph2_new, dm = qtexplorer::dm_ph2),
  # meta = safetyCharts::meta_ecg,
  mapping = mapping_ph2
)
