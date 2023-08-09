## code to prepare `schedule_covercrop_template.csv` dataset as .rda goes here

library(data.table)
covercrop_schedule_template = fread('data-raw/schedule_covercrop_template.csv')

usethis::use_data(covercrop_schedule_template, overwrite = TRUE)
