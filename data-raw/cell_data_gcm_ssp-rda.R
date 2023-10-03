# NOT TO BE USED IN MAIN PACKAGE #

library(data.table)
path      = getwd()
load(paste(path, '/data-raw/cell_data_table_03Oct23.RData', sep = '')) # most recent main table file

# split into gcm x ssp combinations
gcm       = fread(paste(path,'/data-raw/cmip6_calendars.csv', sep = ''))
gcm       = gcm[, calendar := NULL]
gcm       = gcm[gcm != 'historical',]

ssp       = c('ssp126', 'ssp370')

save_path = '/home/shelby/Documents/projects/SoilFutures/data-raw'
setwd(save_path)

for (.ssp in ssp) {
  for (.gcm in gcm$gcm) {
    print(paste('Creating ', .ssp, ' ', .gcm, ' cell data rda file.', sep = ''))
    gcm_ssp_data = main_table[gcm %in% .gcm & ssp %in% .ssp,]
    save(gcm_ssp_data, file = paste(.gcm, .ssp, 'cell_data.rda', sep = '_'))
  }
}
# historical case
gcm_ssp_data = main_table[gcm %in% 'historical' & ssp %in% 'historical',]
save(gcm_ssp_data, file = 'historical_historical_cell_data.rda')

# N.B. the data table name is gcm_ssp_data when loaded


