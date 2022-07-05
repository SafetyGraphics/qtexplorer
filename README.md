
<!-- badges: start -->
[![R build status](https://github.com/SafetyGraphics/qtexplorer/workflows/R-CMD-check/badge.svg)](https://github.com/SafetyGraphics/qtexplorer/actions)
<!-- badges: end -->

# `qtexplorer` QT/ECG Data Explorer  <img src="https://raw.githubusercontent.com/SafetyGraphics/qtexplorer/dev/inst/hex/hex-qtexplorer.png" width = "175" height = "200" align="right" />

The `{qtexplorer}` R package contains a set of charts for investigating QT/ECG cardiac safety, extended from [{safetyCharts}](https://github.com/SafetyGraphics/safetyCharts) pkg, developed by the [ISG](https://safetygraphics.github.io/). These charts are optimized for usage with the [{safetyGraphics} package](https://safetygraphics.github.io/safetyGraphics/) and shiny application, but can also easily be used as standalone displays. 


## Quick start

A demo of the app using sample data is available [here](https://xiao-ni.shinyapps.io/safetygraphics_qt/) or can be initialized as follows:

```r
# Install from github
remotes::install_github("SafetyGraphics/qtexplorer@dev")

library(safetyCharts)
library(safetyGraphics)
library(dplyr)
library(yaml)


qtCharts <- safetyGraphics::makeChartConfig(packages = "qtexplorer") 

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
  visitn_col: VISIT
  tpt_col: TIME
  tptn_col: TIME2
  period_col: ''
  unit_col: 'UNIT'
  baseline_flag_col: 'BL_FLAG'
  baseline_flag_values: 'Y'
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

meta <- rbind(qtexplorer::meta_dm, qtexplorer::meta_ecg)

# Launch qt explorer
safetyGraphics::safetyGraphicsApp(
  charts = qtCharts,
  meta=meta,
  domainData = list(ecg = qtexplorer::eg_ph2, dm = qtexplorer::dm_ph2),
  mapping = mapping_ph2
)

```

## Quick introduction to QT/ECG safety 

- [ICH E14 Guidance for Industry E14 Clinical Evaluation of QT/QTc Interval Prolongation and Proarrhythmic Potential for Non-Antiarrhythmic Drugs](https://www.fda.gov/media/71372/download)
- [CTSPEDIA ECG Clinical Questions](https://www.ctspedia.org/do/view/CTSpedia/ListingsECGVetted), contributed by Rich Anziano

## Sample ECG data

The `qtexplorer` package includes 3 sample ECG data sets

- Thorough QT: [FDA CiPA ECG Validation Study](https://physionet.org/content/ecgcipa/1.0.0/)
- De-identified Phase 2b study (non TQT). 
    + [Link to data description](https://github.com/SafetyGraphics/qtexplorer/blob/dev/data-raw/Phase2_Example_Description.docx?raw=true)

# qtexplorer Charts

- Central Tendency 
- Outlier explorer (ICH E14)
- safetyCharts lab widgets
    + Outlier explorer 
    + Boxplot
    + Histogram
    + Shiftplot
    + Paneled outlier plot
    + delta delta