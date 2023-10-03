## code to prepare `schedule_covercrop_template.csv` dataset as .rda goes here

library(data.table)
path           = '/home/shelby/Documents/projects/SoilFutures'
covercrop_schedule_template = fread(paste(path, 'data-raw/schedule_covercrop_template.csv', sep = '/'))

usethis::use_data(covercrop_schedule_template, overwrite = TRUE)
