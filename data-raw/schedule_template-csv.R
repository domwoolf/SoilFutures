## code to prepare `schedule_template.csv` dataset as .rda goes here

library(data.table)
path           = '/home/shelby/Documents/projects/SoilFutures'
schedule_template = fread(paste(path, 'data-raw/schedule_template.csv', sep = '/'))

usethis::use_data(schedule_template, overwrite = TRUE)
