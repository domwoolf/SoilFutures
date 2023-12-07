## code to prepare `cell_data_extracted_site100.csv` dataset as .rda goes here

library(data.table)
path           = '/home/shelby/Documents/projects/SoilFutures'
cell_data_site = fread(paste(path, 'data-raw/main_table_extracted_site100.csv', sep = '/'))
#---------------------------------------------------------------------------------------------------
# Filter by >= 1 ha cutoff
#---------------------------------------------------------------------------------------------------
load(paste(path, 'data-raw/input_table_by_gridid_crop_irr_05Dec23_1ha-filtered.RData', sep = '/'))
gridid_f       = unique(main_table[,gridid])

cell_data_site = cell_data_site[gridid %in% gridid_f,]
fwrite(cell_data_site, paste(path, 'data-raw/main_table_extracted_site100_1ha-filtered.csv', sep = '/'))
save(cell_data_site, file = paste(path, 'data-raw/main_table_extracted_site100_1ha-filtered.RData', sep = '/'))
#---------------------------------------------------------------------------------------------------
usethis::use_data(cell_data_site, overwrite = TRUE)
