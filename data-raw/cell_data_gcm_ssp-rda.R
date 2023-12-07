# NOT TO BE USED IN MAIN PACKAGE #
#------------------------------------------------------------------------------------------------
library(data.table)
library(terra)
#------------------------------------------------------------------------------------------------
path      = getwd()
save_path = paste(path, 'data-raw/', sep = '/')
date      = '05December23'
#------------------------------------------------------------------------------------------------
load(paste0(path, '/data-raw/cell_data_table_', date, '.RData'))
#------------------------------------------------------------------------------------------------
# MISSING CHECK AND FILTER
#------------------------------------------------------------------------------------------------
# October 2023
crop_area_r      = rast(paste(pkg.env$gis_path, 'all-cropland-rf-ir-area.tif', sep = '/'))
crop_area_dt     = as.data.table(terra::as.data.frame(crop_area_r, xy = TRUE, cells = TRUE))
gridid_all       = unique(main_table[, gridid])
crop_area_dt_f   = crop_area_dt[cell %in% gridid_all,]
crop_area_dt_f   = crop_area_dt_f[wht_irr_area_ha >= 1 | wht_rain_area_ha >= 1 | maiz_rain_area_ha >= 1 |
                 maiz_irr_area_ha >= 1 | soyb_rain_area_ha >=1 | soyb_irr_area_ha >= 1,]
gridid_f         = unique(crop_area_dt_f[,cell])

# REMOVE < 1ha
main_table_old = main_table[gcm %in% 'historical' & gridid %in% gridid_f,]
gridid_old     = unique(main_table_old[,gridid])

# December 2023
crop_area_r      = rast(paste(pkg.env$gis_path, 'msw-cropland-rf-ir-area.tif', sep = '/'))
crop_area_dt     = as.data.table(terra::as.data.frame(crop_area_r, xy = TRUE, cells = TRUE))
gridid_all       = unique(main_table[, gridid])
crop_area_dt_f   = crop_area_dt[cell %in% gridid_all,]
crop_area_dt_f   = crop_area_dt_f[wheat_irrigated_2015 >= 1 | wheat_rainfed_2015 >= 1 | maize_rainfed_2015 >= 1 |
                                    maize_irrigated_2015 >= 1 | soybean_rainfed_2015 >= 1 | soybean_irrigated_2015 >= 1,]
gridid_f         = unique(crop_area_dt_f[,cell])

# REMOVE < 1ha
main_table_new = main_table[gcm %in% 'historical' & gridid %in% gridid_f,]
gridid_new     = unique(main_table_new[,gridid])

# Compute difference
gridid_check   = gridid_new[gridid_new %in% gridid_old]
# Data to remove based on 1 ha inclusion
gridid_remove  = gridid_old[!gridid_old %in% gridid_check]
remove_dt      = data.table(gridid = gridid_remove)
fwrite(remove_dt, paste0(save_path, 'gridid_removal_03Oct23.csv')) # posthoc correction

# Additional gridid runs based on revised raster
gridid_add     = gridid_new[!gridid_new %in% gridid_old]
add_dt         = data.table(gridid = gridid_add)
fwrite(add_dt, paste0(save_path, 'gridid_addition_03Oct23.csv')) # new simulations
#------------------------------------------------------------------------------------------------
# 05 December Simulations (ALL)
#------------------------------------------------------------------------------------------------
main_table = main_table[gridid %in% gridid_f,]
save(main_table, file = paste0(save_path,'/','cell_data_table_', date, '_1ha-filtered.RData'))
#------------------------------------------------------------------------------------------------
# 05 December Simulations (Missing ONLY)
#------------------------------------------------------------------------------------------------
main_table = main_table[gridid %in% gridid_add,]
save(main_table, file = paste0(save_path,'/','cell_data_table_', date, '_missing.RData'))
#------------------------------------------------------------------------------------------------
# RDA SPLIT
#------------------------------------------------------------------------------------------------
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


