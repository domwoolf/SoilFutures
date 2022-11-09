## code to prepare `cell_data_table_csu.csv` dataset as multiple .rda goes here

library(data.table)
cell_data = fread('data-raw/cell_data_table_csu.csv')

usethis::use_data(cell_data, overwrite = TRUE)
