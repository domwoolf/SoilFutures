## code to prepare `EQ_extracted_sch.csv` dataset as .rda goes here

library(data.table)
EQ_sch_data = fread('data-raw/EQ_extracted_sch.csv')

usethis::use_data(EQ_sch_data, overwrite = TRUE)
