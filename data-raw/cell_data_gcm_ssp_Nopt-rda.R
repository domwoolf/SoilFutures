# NOT TO BE USED IN MAIN PACKAGE #
#------------------------------------------------------------------------------------------------
library(data.table)
library(terra)
#------------------------------------------------------------------------------------------------
path      = getwd()
save_path = paste(path, 'data-raw/', sep = '/')
date      = '05February24'
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

# REMOVE < 1 HA BY CROP AND IRR
maiz_0_gr        = crop_area_dt_f[, .(cell, x, y, maize_rainfed_2015)]
maiz_0_gr        = maiz_0_gr[maize_rainfed_2015 < 1, cell]

maiz_1_gr        = crop_area_dt_f[, .(cell, x, y, maize_irrigated_2015)]
maiz_1_gr        = maiz_1_gr[maize_irrigated_2015 < 1, cell]

soyb_0_gr        = crop_area_dt_f[, .(cell, x, y, soybean_rainfed_2015)]
soyb_0_gr        = soyb_0_gr[soybean_rainfed_2015 < 1, cell]

soyb_1_gr        = crop_area_dt_f[, .(cell, x, y, soybean_irrigated_2015)]
soyb_1_gr        = soyb_1_gr[soybean_irrigated_2015 < 1, cell]

wht_0_gr         = crop_area_dt_f[, .(cell, x, y, wheat_rainfed_2015)]
wht_0_gr         = wht_0_gr[wheat_rainfed_2015 < 1, cell]

wht_1_gr         = crop_area_dt_f[, .(cell, x, y, wheat_irrigated_2015)]
wht_1_gr         = wht_1_gr[wheat_irrigated_2015 < 1, cell]

  # FILTER
main_table       = main_table[!crop %in% 'maiz' | !irr == 0 | !gridid %in% maiz_0_gr,]
gc()
main_table       = main_table[!crop %in% 'maiz' | !irr == 1 | !gridid %in% maiz_1_gr,]
gc()
main_table       = main_table[!crop %in% 'soyb' | !irr == 0 | !gridid %in% soyb_0_gr,]
gc()
main_table       = main_table[!crop %in% 'soyb' | !irr == 1 | !gridid %in% soyb_1_gr,]
gc()
main_table       = main_table[!crop %in% c('swht, wwht') | !irr == 0 | !gridid %in% wht_0_gr,]
gc()
main_table       = main_table[!crop %in% c('swht, wwht') | !irr == 1 | !gridid %in% wht_1_gr,]
gc()
#------------------------------------------------------------------------------------------------
# RDA SPLIT
#------------------------------------------------------------------------------------------------
# remove non-used scenarios
scenarios = c('res', 'ntill', 'ccg', 'ccl', 'ccg-res', 'ccl-res')

main_table = main_table[!scenario %in% scenarios]
gc()

# save input dt
main_table_input = main_table[scenario %in% 'conv' & ssp %in% 'historical']
main_table_input[, scenario := NULL]
main_table_input[, gcm      := NULL]
main_table_input[, ssp      := NULL]

save_path = '/home/shelby/Documents/projects/SoilFutures/data-raw/N-opt'
setwd(save_path)
save(main_table_input, file = paste(save_path,'input_table_Nopt_gridid_crop_irr.RData', sep = '/'))

# split into gcm x ssp combinations
gcm       = fread(paste(path,'/data-raw/cmip6_calendars.csv', sep = ''))
# N.B. not all cmip6 gcm, ssp have associated weather files because missing from NEX-GDDP download
gcm       = gcm[, calendar := NULL]
gcm       = gcm[gcm != 'historical',]

ssp       = c('ssp370') # add ssp126 if desired

for (.ssp in ssp) {
  for (.gcm in gcm$gcm) {
    print(paste('Creating ', .ssp, ' ', .gcm, ' cell data rda file.', sep = ''))
    gcm_ssp_data = main_table[gcm %in% .gcm & ssp %in% .ssp,]
    gc()
    # add nitrogen yield response curve levels by crop type
    n_scenario.col = gcm_ssp_data[, .('n_scenario' = rep(c('actual', 'none', 'n_1', 'n_2', 'n_3',
                                                                          'n_4', 'n_5', 'n_6', 'n_7', 'n_8'))), by = scenario]
    gcm_ssp_data      = unique(gcm_ssp_data[n_scenario.col, on = .(scenario = scenario), by = .EACHI, allow.cartesian = TRUE])
    gc()
    # order
    setcolorder(gcm_ssp_data, c('gridid', 'gridid.rotated', 'x', 'x.rotated','y','regionid', 'ssp', 'gcm','crop', 'irr','scenario', 'n_scenario','pset_id'))
    setorder(gcm_ssp_data, gridid, ssp, gcm, crop, irr, scenario, n_scenario)
    gc()
    # adjust nitrogen levels, 8 increments, maize
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'none', fertN.amt := 0]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_1', fertN.amt := 4.38]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_2', fertN.amt := 8.75]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_3', fertN.amt := 13.13]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_4', fertN.amt := 17.5]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_5', fertN.amt := 21.88]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_6', fertN.amt := 26.25]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_7', fertN.amt := 30.63]
    gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_8', fertN.amt := 35L]

    # adjust nitrogen levels, 8 increments, soybean
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'none', fertN.amt := 0]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_1', fertN.amt := 1.25]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_2', fertN.amt := 2.5]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_3', fertN.amt := 3.75]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_4', fertN.amt := 5]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_5', fertN.amt := 6.25]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_6', fertN.amt := 7.5]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_7', fertN.amt := 8.75]
    gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_8', fertN.amt := 10]

    # adjust nitrogen levels, 8 increments, wheat
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'none', fertN.amt := 0]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_1', fertN.amt := 2.5]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_2', fertN.amt := 5L]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_3', fertN.amt := 7.5]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_4', fertN.amt := 10L]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_5', fertN.amt := 12.5]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_6', fertN.amt := 15L]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_7', fertN.amt := 17.5]
    gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_8', fertN.amt := 20L]
    # save
    save(gcm_ssp_data, file = paste(.gcm, .ssp, 'cell_data_Nopt.rda', sep = '_'))
    gc()
  }
}
# historical case
# gcm_ssp_data = main_table[gcm %in% 'historical' & ssp %in% 'historical',]
# # add nitrogen yield response curve levels by crop type
# n_scenario.col = gcm_ssp_data[, .('n_scenario' = rep(c('actual', 'none', 'n_1', 'n_2', 'n_3',
#                                                        'n_4', 'n_5', 'n_6', 'n_7', 'n_8'))), by = scenario]
# gcm_ssp_data      = unique(gcm_ssp_data[n_scenario.col, on = .(scenario = scenario), by = .EACHI, allow.cartesian = TRUE])
# gc()
# # order
# setcolorder(gcm_ssp_data, c('gridid', 'gridid.rotated', 'x', 'x.rotated','y','regionid', 'ssp', 'gcm','crop', 'irr','scenario', 'n_scenario','pset_id'))
# setorder(gcm_ssp_data, gridid, ssp, gcm, crop, irr, scenario, n_scenario)
# gc()
# # adjust nitrogen levels, 8 increments, maize
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'none', fertN.amt := 0]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_1', fertN.amt := 4.38]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_2', fertN.amt := 8.75]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_3', fertN.amt := 13.13]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_4', fertN.amt := 17.5]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_5', fertN.amt := 21.88]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_6', fertN.amt := 26.25]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_7', fertN.amt := 30.63]
# gcm_ssp_data[crop %in% 'maiz' & n_scenario %in% 'n_8', fertN.amt := 35L]
#
# # adjust nitrogen levels, 8 increments, soybean
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'none', fertN.amt := 0]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_1', fertN.amt := 1.25]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_2', fertN.amt := 2.5]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_3', fertN.amt := 3.75]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_4', fertN.amt := 5]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_5', fertN.amt := 6.25]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_6', fertN.amt := 7.5]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_7', fertN.amt := 8.75]
# gcm_ssp_data[crop %in% 'soyb' & n_scenario %in% 'n_8', fertN.amt := 10]
#
# # adjust nitrogen levels, 8 increments, wheat
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'none', fertN.amt := 0]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_1', fertN.amt := 2.5]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_2', fertN.amt := 5L]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_3', fertN.amt := 7.5]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_4', fertN.amt := 10L]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_5', fertN.amt := 12.5]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_6', fertN.amt := 15L]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_7', fertN.amt := 17.5]
# gcm_ssp_data[crop %in% c('swht', 'wwht') & n_scenario %in% 'n_8', fertN.amt := 20L]
# # save
# save(gcm_ssp_data, file = 'historical_historical_cell_data.rda')

# N.B. the data table name is gcm_ssp_data when loaded

