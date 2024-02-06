# NOT TO BE USED IN MAIN PACKAGE #
#------------------------------------------------------------------------------------------------
library(data.table)
library(terra)
#------------------------------------------------------------------------------------------------
path      = getwd()
save_path = paste(path, 'data-raw/', sep = '/')
date      = '05February24'
#-----------------------------------------------------------------------------------------------
# MUST RUN zzz.R BEFORE THIS SCRIPT
#-----------------------------------------------------------------------------------------------
load(paste0(path, '/data-raw/cell_data_table_', date, '.RData'))
gridid_all     = unique(main_table[,gridid])
#------------------------------------------------------------------------------------------------
# MISSING CHECK AND FILTER
#------------------------------------------------------------------------------------------------
# DEPRECATED
# main_table_new = main_table
# rm(main_table)
#
# gridid_all     = unique(main_table_new[,gridid])

# October 2023 Run
# date_oct   = '03Oct23'
# load(paste0(path, '/data-raw/cell_data_table_', date_oct, '.RData'))
#
# gridid_oct = unique(main_table[,gridid])
# rm(main_table)
#
# gridid_missing = gridid_all[!gridid_all %in% gridid_oct]
#
# # rename main_table
# main_table = main_table_new
# rm(main_table_new)
# gc()
#------------------------------------------------------------------------------------------------
# CREATE TABLE WITH ACTUAL CROP AREA GRIDID NUMBERS
#------------------------------------------------------------------------------------------------
crop_area_r      = rast(paste(pkg.env$gis_path, 'msw-cropland-rf-ir-area.tif', sep = '/'))
crop_area_dt     = as.data.table(terra::as.data.frame(crop_area_r, xy = TRUE, cells = TRUE, na.rm = FALSE))

crop_area_gr_dt  = copy(crop_area_dt)
# remove NA
crop_area_gr_dt  = crop_area_gr_dt[!is.na(maize_rainfed_2015)]
crop_area_gr_dt  = crop_area_gr_dt[!is.na(maize_irrigated_2015)]
crop_area_gr_dt  = crop_area_gr_dt[!is.na(soybean_rainfed_2015)]
crop_area_gr_dt  = crop_area_gr_dt[!is.na(soybean_irrigated_2015)]
crop_area_gr_dt  = crop_area_gr_dt[!is.na(wheat_rainfed_2015)]
crop_area_gr_dt  = crop_area_gr_dt[!is.na(wheat_irrigated_2015)]
# greater than 0
crop_area_gr_dt  = crop_area_gr_dt[wheat_irrigated_2015 > 0 | wheat_rainfed_2015 > 0 | maize_rainfed_2015 > 0 |
                                     maize_irrigated_2015 > 0 | soybean_rainfed_2015 > 0 | soybean_irrigated_2015 > 0,]
# check sum
crop_area_gr_dt[, crop_sum := (maize_rainfed_2015 + maize_irrigated_2015 + soybean_rainfed_2015 +
                                soybean_irrigated_2015 + wheat_rainfed_2015 + wheat_irrigated_2015)]
sum(crop_area_gr_dt[, crop_sum])

# GRIDID WITH CROPLAND BUT NO SITE FILE
crop_area_gr_dt  = crop_area_gr_dt[!cell %in% gridid_all,]
crop_area_gr_dt  = crop_area_gr_dt[wheat_irrigated_2015 >= 1 | wheat_rainfed_2015 >= 1 | maize_rainfed_2015 >= 1 |
                                    maize_irrigated_2015 >= 1 | soybean_rainfed_2015 >= 1 | soybean_irrigated_2015 >= 1,]
sum(crop_area_gr_dt[,crop_sum]) # 31,525,875
maize_rf_m_gr    = crop_area_gr_dt[maize_rainfed_2015 >= 1, cell] # 5068
maize_ir_m_gr    = crop_area_gr_dt[maize_irrigated_2015 >= 1, cell] # 1362
soyb_rf_m_gr     = crop_area_gr_dt[soybean_rainfed_2015 >= 1, cell] # 2840
soyb_ir_m_gr     = crop_area_gr_dt[soybean_irrigated_2015 >= 1, cell] # 732
wheat_rf_m_gr    = crop_area_gr_dt[wheat_rainfed_2015 >= 1, cell] # 5032
wheat_ir_m_gr    = crop_area_gr_dt[wheat_irrigated_2015 >= 1, cell] # 1066
#------------------------------------------------------------------------------------------------
# FILTER BY CROP RASTER BY MAIN TABLE
#------------------------------------------------------------------------------------------------
crop_area_dt_f   = crop_area_dt[cell %in% gridid_all,]

# CHECK CROP AREA
crop_area_dt_f[, crop_sum := (maize_rainfed_2015 + maize_irrigated_2015 + soybean_rainfed_2015 +
                           soybean_irrigated_2015 + wheat_rainfed_2015 + wheat_irrigated_2015)]

sum(crop_area_dt_f[, crop_sum], na.rm = TRUE) # 493,834,741 BEFORE REMOVAL
area_missing_dt  = crop_area_dt_f[is.na(crop_sum)]


crop_area_dt_f   = crop_area_dt_f[wheat_irrigated_2015 >= 1 | wheat_rainfed_2015 >= 1 | maize_rainfed_2015 >= 1 |
                                    maize_irrigated_2015 >= 1 | soybean_rainfed_2015 >= 1 | soybean_irrigated_2015 >= 1,]
gridid_f         = unique(crop_area_dt_f[,cell])

# DEPRECATED

# REMOVE < 1ha
# main_table_new = main_table[gcm %in% 'historical' & gridid %in% gridid_f,]
# gridid_new     = unique(main_table_new[,gridid])

# CHECK CROP AREA

# Additional gridid runs based on revised raster
# gridid_add     = gridid_new[gridid_new %in% gridid_missing]
# add_dt         = data.table(gridid = gridid_add)
# fwrite(add_dt, paste0(save_path, 'gridid_additions_after_03Oct23.csv')) # new simulations
#------------------------------------------------------------------------------------------------
# December 2023 / February 2024 Simulations (ALL)
#------------------------------------------------------------------------------------------------
# DEPRECATED
# main_table_new = main_table[gridid %in% gridid_f,]
# save(main_table_new, file = paste0(save_path,'/','cell_data_table_', date, '_1ha-filtered.RData'))
#------------------------------------------------------------------------------------------------
# 05 December Simulations (Missing ONLY)
#------------------------------------------------------------------------------------------------
# DEPRECATED
# main_table = main_table[gridid %in% gridid_add,]
# save(main_table, file = paste0(save_path,'/','cell_data_table_', date, '_missing_1ha-filtered.RData'))
#------------------------------------------------------------------------------------------------
# RDA SPLIT
#------------------------------------------------------------------------------------------------
# split into gcm x ssp combinations
main_table= main_table[gridid %in% gridid_f,]
gc()

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


