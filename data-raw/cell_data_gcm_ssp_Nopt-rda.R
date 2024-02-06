# NOT TO BE USED IN MAIN PACKAGE #
#------------------------------------------------------------------------------------------------
library(data.table)
library(terra)
#------------------------------------------------------------------------------------------------
path      = getwd()
save_path = paste(path, 'data-raw/', sep = '/')
date      = '08December23'
#------------------------------------------------------------------------------------------------
load(paste0(path, '/data-raw/cell_data_table_', date, '.RData'))
crop_area_r      = rast(paste(pkg.env$gis_path, 'msw-cropland-rf-ir-area.tif', sep = '/'))
crop_area_dt     = as.data.table(terra::as.data.frame(crop_area_r, xy = TRUE, cells = TRUE, na.rm = FALSE))
#------------------------------------------------------------------------------------------------
# FILTER CROP RASTER BY MAIN TABLE
#------------------------------------------------------------------------------------------------
gridid_all       = unique(main_table[,gridid])
crop_area_dt_f   = crop_area_dt[cell %in% gridid_all,]

# CHECK CROP AREA
crop_area_dt_f[, crop_sum := (maize_rainfed_2015 + maize_irrigated_2015 + soybean_rainfed_2015 +
                           soybean_irrigated_2015 + wheat_rainfed_2015 + wheat_irrigated_2015)]

sum(crop_area_dt_f[, crop_sum], na.rm = TRUE) # 493,834,741 BEFORE REMOVAL
area_missing_dt  = crop_area_dt_f[is.na(crop_sum)]


crop_area_dt_f   = crop_area_dt_f[wheat_irrigated_2015 >= 1 | wheat_rainfed_2015 >= 1 | maize_rainfed_2015 >= 1 |
                                    maize_irrigated_2015 >= 1 | soybean_rainfed_2015 >= 1 | soybean_irrigated_2015 >= 1,]
gridid_f         = unique(crop_area_dt_f[,cell])
main_table       = main_table[gridid %in% gridid_f,]
#------------------------------------------------------------------------------------------------
# RDA SPLIT
#------------------------------------------------------------------------------------------------
# split into gcm x ssp combinations
gcm       = fread(paste(path,'/data-raw/cmip6_calendars.csv', sep = ''))
gcm       = gcm[, calendar := NULL]
gcm       = gcm[gcm != 'historical',]

ssp       = c('ssp126', 'ssp370')

save_path = '/home/shelby/Documents/projects/SoilFutures/data-raw/N-opt'
setwd(save_path)

for (.ssp in ssp) {
  for (.gcm in gcm$gcm) {
    print(paste('Creating ', .ssp, ' ', .gcm, ' cell data rda file.', sep = ''))
    gcm_ssp_data = main_table[gcm %in% .gcm & ssp %in% .ssp,]
    # reduce scenarios to BMP and BAU
    gcm_ssp_data = gcm_ssp_data[scenario %in% c('conv', 'ccg-ntill', 'ccl-ntill')]
    gc()
    # add nitrogen yield response curve levels
    n_scenario.col = gcm_ssp_data[, .('n_scenario' = rep(c('actual', 'none', 'n_p05', 'n_p10', 'n_p15', 'n_p20', 'n_p25', 'n_p30', 'n_p35',
                                                        'n_r05', 'n_r10', 'n_r15', 'n_r20', 'n_r25', 'n_r30', 'n_r35'))), by = scenario]
    gcm_ssp_data      = unique(gcm_ssp_data[n_scenario.col, on = .(scenario = scenario), by = .EACHI, allow.cartesian = TRUE])
    gc()
    # order
    setcolorder(gcm_ssp_data, c('gridid', 'gridid.rotated', 'x', 'x.rotated','y','regionid', 'ssp', 'gcm','crop', 'irr','scenario', 'n_scenario','pset_id'))
    setorder(gcm_ssp_data, gridid, ssp, gcm, crop, irr, scenario, n_scenario)
    gc()
    # adjust nitrogen levels
    gcm_ssp_data[n_scenario %in% 'none', fertN.amt := 0]
    gcm_ssp_data[n_scenario %in% 'n_p05', fertN.amt := fertN.amt + 5L]
    gcm_ssp_data[n_scenario %in% 'n_p10', fertN.amt := fertN.amt + 10L]
    gcm_ssp_data[n_scenario %in% 'n_p15', fertN.amt := fertN.amt + 15L]
    gcm_ssp_data[n_scenario %in% 'n_p20', fertN.amt := fertN.amt + 20L]
    gcm_ssp_data[n_scenario %in% 'n_p25', fertN.amt := fertN.amt + 25L]
    gcm_ssp_data[n_scenario %in% 'n_p30', fertN.amt := fertN.amt + 30L]
    gcm_ssp_data[n_scenario %in% 'n_p35', fertN.amt := fertN.amt + 35L]
    gcm_ssp_data[n_scenario %in% 'n_r05', fertN.amt := fertN.amt - 5L]
    gcm_ssp_data[n_scenario %in% 'n_r10', fertN.amt := fertN.amt - 10L]
    gcm_ssp_data[n_scenario %in% 'n_r15', fertN.amt := fertN.amt - 15L]
    gcm_ssp_data[n_scenario %in% 'n_r20', fertN.amt := fertN.amt - 20L]
    gcm_ssp_data[n_scenario %in% 'n_r25', fertN.amt := fertN.amt - 25L]
    gcm_ssp_data[n_scenario %in% 'n_r30', fertN.amt := fertN.amt - 30L]
    gcm_ssp_data[n_scenario %in% 'n_r35', fertN.amt := fertN.amt - 35L]
    # save
    save(gcm_ssp_data, file = paste(.gcm, .ssp, 'cell_data_Nopt.rda', sep = '_'))
    gc()
  }
}
# historical case
gcm_ssp_data = main_table[gcm %in% 'historical' & ssp %in% 'historical',]
# reduce scenarios to BMP and BAU
gcm_ssp_data = gcm_ssp_data[scenario %in% c('conv', 'ccg-ntill', 'ccl-ntill')]
gc()
# add nitrogen yield response curve levels
n_scenario.col = gcm_ssp_data[, .('n_scenario' = rep(c('actual', 'none', 'n_p05', 'n_p10', 'n_p15', 'n_p20', 'n_p25', 'n_p30', 'n_p35',
                                                       'n_r05', 'n_r10', 'n_r15', 'n_r20', 'n_r25', 'n_r30', 'n_r35'))), by = scenario]
gcm_ssp_data      = unique(gcm_ssp_data[n_scenario.col, on = .(scenario = scenario), by = .EACHI, allow.cartesian = TRUE])
gc()
# order
setcolorder(gcm_ssp_data, c('gridid', 'gridid.rotated', 'x', 'x.rotated','y','regionid', 'ssp', 'gcm','crop', 'irr','scenario', 'n_scenario','pset_id'))
setorder(gcm_ssp_data, gridid, ssp, gcm, crop, irr, scenario, n_scenario)
gc()
# adjust nitrogen levels
gcm_ssp_data[n_scenario %in% 'none', fertN.amt := 0]
gcm_ssp_data[n_scenario %in% 'n_p05', fertN.amt := fertN.amt + 5L]
gcm_ssp_data[n_scenario %in% 'n_p10', fertN.amt := fertN.amt + 10L]
gcm_ssp_data[n_scenario %in% 'n_p15', fertN.amt := fertN.amt + 15L]
gcm_ssp_data[n_scenario %in% 'n_p20', fertN.amt := fertN.amt + 20L]
gcm_ssp_data[n_scenario %in% 'n_p25', fertN.amt := fertN.amt + 25L]
gcm_ssp_data[n_scenario %in% 'n_p30', fertN.amt := fertN.amt + 30L]
gcm_ssp_data[n_scenario %in% 'n_p35', fertN.amt := fertN.amt + 35L]
gcm_ssp_data[n_scenario %in% 'n_r05', fertN.amt := fertN.amt - 5L]
gcm_ssp_data[n_scenario %in% 'n_r10', fertN.amt := fertN.amt - 10L]
gcm_ssp_data[n_scenario %in% 'n_r15', fertN.amt := fertN.amt - 15L]
gcm_ssp_data[n_scenario %in% 'n_r20', fertN.amt := fertN.amt - 20L]
gcm_ssp_data[n_scenario %in% 'n_r25', fertN.amt := fertN.amt - 25L]
gcm_ssp_data[n_scenario %in% 'n_r30', fertN.amt := fertN.amt - 30L]
gcm_ssp_data[n_scenario %in% 'n_r35', fertN.amt := fertN.amt - 35L]
save(gcm_ssp_data, file = 'historical_historical_cell_data.rda')

# N.B. the data table name is gcm_ssp_data when loaded

