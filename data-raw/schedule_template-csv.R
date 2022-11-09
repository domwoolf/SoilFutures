## code to prepare `schedule_template.csv` dataset as .rda goes here

library(data.table)
schedule_template = fread('data-raw/schedule_template.csv')

usethis::use_data(schedule_template, overwrite = TRUE)
