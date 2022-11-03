## code to prepare `cell_data_extracted_site100.csv` dataset as .rda goes here

library(data.table)
cell_data_site = fread('cell_data_extracted_site100.csv')

usethis::use_data(cell_data_site, overwrite = TRUE)
