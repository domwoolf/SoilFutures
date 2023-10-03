## code to prepare `cell_data_extracted_site100.csv` dataset as .rda goes here

library(data.table)
path           = '/home/shelby/Documents/projects/SoilFutures'
cell_data_site = fread(paste(path, 'data-raw/main_table_extracted_site100.csv', sep = '/'))

# OPTIONAL - remove gridid (< 1 ha)

usethis::use_data(cell_data_site, overwrite = TRUE)
