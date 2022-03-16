

library(tidyverse)
library(usethis)

# brackets after read_csv to remove spec_tbl_df class per https://www.tidyverse.org/blog/2018/12/readr-1-3-1/


# reuse safetyCharts dm
#dm
meta_dm<-read_csv("data-raw/meta_dm.csv")[]

# Copy metadata to /data
# ecg
meta_ecg<-read_csv("data-raw/meta_ecg.csv")[]


# Ph1 dummy data
dm_ph1 <- read_csv("./data-raw/SAD_MAD_Example_SUBJ_Data.csv")[]
eg_ph1 <- read_csv("./data-raw/SAD_MAD_Example_ECG_Data.csv")[] %>%
    left_join(dm_ph1 %>% select(ID, TREAT)) # merge TREAT var to eg data

# Ph2 dummy data
dm_ph2 <- read_csv("./data-raw/Phase2_Example_SUBJ_Data.csv")[]
eg_ph2 <- read_csv("./data-raw/Phase2_Example_ECG_Data.csv")[] %>%
    left_join(dm_ph2 %>% select(ID, TREAT)) # merge TREAT var to eg data

# sample ADaM ECT data 
# https://physionet.org/content/ecgcipa/1.0.0/

adsl <- readr::read_csv("https://physionet.org/files/ecgcipa/1.0.0/adsl.csv?download")[]
adeg <- readr::read_csv("https://physionet.org/files/ecgcipa/1.0.0/adeg.csv?download")[] %>%
    mutate(ATPTFCT = forcats::fct_reorder(ATPT, .x = ATPTN, .fun = min))


# export to pkg
usethis::use_data(meta_dm, meta_ecg, adsl, adeg, eg_ph1, dm_ph1, eg_ph2, dm_ph2, overwrite = TRUE)
