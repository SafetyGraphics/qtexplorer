# Note: expected to be run from the root package directory
library(tidyverse)
library(usethis)

 # brackets after read_csv to remove spec_tbl_df class per https://www.tidyverse.org/blog/2018/12/readr-1-3-1/

#Copy metadata to /data
#ecg
meta_ecg<-read_csv("data-raw/meta_ecg.csv")[]
usethis::use_data(meta_ecg, overwrite = TRUE)
