## code to prepare input dataset for analyses

library(data.table)
path       = '/home/shelby/Documents/projects/SoilFutures'
date       = '08December23'
load(paste0(path,'/data-raw/cell_data_table_',date,'.RData'))

main_table = main_table[scenario %in% 'conv' & ssp %in% 'historical']
save(main_table, file = paste(path,'data-raw/input_table_by_gridid_crop_irr.RData', sep = '/'))
