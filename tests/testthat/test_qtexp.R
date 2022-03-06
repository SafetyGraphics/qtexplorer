library(tidyverse)
library(safetyGraphics)
library(safetyCharts)
library(qtexplorer)


# test safetyCharts
adeg <- readr::read_csv("https://physionet.org/files/ecgcipa/1.0.0/adeg.csv?download") %>%
  mutate(ATPTFCT = forcats::fct_reorder(ATPT, .x = ATPTN, .fun = min)) %>%
  mutate(ANRHI=0, ANRLO=0)

qt0 <-  makeChartConfig()  %>% 
  purrr::keep(~ (grepl("qt", .x$label, ignore.case = TRUE))) %>%
  purrr::map(function(chart){
    chart$order <- 1
    return(chart)
})  

length(qt0)

mappingsQT <- list(
  ecg=list(
    'visit_col'='ATPT',
    'visitn_col'='ATPTN'
  )
)

safetyGraphicsApp(
    charts=qt0, 
    domainData = list(ecg=adeg), 
    mapping = mappingsQT
)


# test qtexplorer
qt1 <-  makeChartConfig(#packages = "qtexplorer" 
 dirs = "./inst/config/", packages = NULL, packageLocation = NULL
)  %>% 
  purrr::map(function(chart){
    chart$order <- 1
    return(chart)
})  

length(qt1)

mappingsQT <- list(
  ecg=list(
    'visit_col'='ATPT',
    'visitn_col'='ATPTN'
  )
)

safetyGraphicsApp(
    charts=qt1, 
    domainData = list(ecg=adeg), 
    #meta = qtexplorer::meta_ecg, 
    mapping = mappingsQT
)


