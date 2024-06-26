# Create a data.table containing all variables required to run a DayCent simulation

# Generates a data.table with rows for every combination of scenario, crop and ssp
# and the raster cell numbers corresponding to locations where that crop is grown globally.
# Data values are automatically populated using spatial crop calendar, crop management, and soil data.
#-----------------------------------------------------------------------------------------------
library(terra)
library(data.table)
library(sf)
#-----------------------------------------------------------------------------------------------
# MUST RUN zzz.R BEFORE THIS SCRIPT
#-----------------------------------------------------------------------------------------------
# paths
save_path = paste(getwd(), 'data-raw', sep = '/')
#-----------------------------------------------------------------------------------------------
# variables
date = '05February24'
#-----------------------------------------------------------------------------------------------
  # create rasters
  crop_calendar  = rast(paste(pkg.env$gis_path, 'crop_calendar_agg.tif',             sep='/'))
  min_N          = rast(paste(pkg.env$gis_path, 'fertilizer-n_by_crop_focal.tif',    sep='/'))
  org_N          = rast(paste(pkg.env$gis_path, 'manure-n_cropland_focal.tif',       sep='/'))
  org_CN         = rast(paste(pkg.env$gis_path, 'global_manure_weighted_CN_agg.tif', sep='/'))
  residue_return = rast(paste(pkg.env$gis_path, 'wirsenius_global_res_rtrn_agg.tif', sep='/'))
  ipcc_clim      = rast(paste(pkg.env$gis_path, 'ipcc_climate_agg.tif',              sep='/'))
  cmip6_calendars= fread(paste(pkg.env$gis_path,'cmip6_calendars.csv',               sep='/'))
#-----------------------------------------------------------------------------------------------
# tidy variables and names
  crop_calendar        = crop_calendar[[names(crop_calendar) %like% 'plant_' | names(crop_calendar) %like% 'harvest_']]
  names(crop_calendar) = gsub('plant_', 'plant.date_', names(crop_calendar))
  names(crop_calendar) = gsub('harvest_', 'harvest.date_', names(crop_calendar))

  min_N                 = min_N[[names(min_N) %like% '2015']]
  min_N                 = min_N/10 # g m-2
  names(min_N)          = gsub('2015', 'fertN.amt', names(min_N))

  org_N                 = org_N[[165]]
  org_N                 = org_N/10 # g m-2
  names(org_N)          = 'orgN.amt'

  names(org_CN)         = 'orgCN.ratio'

  names(residue_return) = 'res.rtrn.amt'
#-----------------------------------------------------------------------------------------------
  # MAIZE
  gridid.dt       = fread(paste(pkg.env$gis_path, 'maiz_lu_cohort_id.csv',          sep='/'))

  # create a vector of rotated grid cell numbers needed for weather file script
  gridid.xy       = setDT(as.data.frame(xyFromCell(org_CN, gridid.dt[,gridid])))
  gridid.xy.rot   = gridid.xy
  gridid.xy.rot$x = (gridid.xy.rot$x + 360) %% 360 # http://www.idlcoyote.com/map_tips/lonconvert.html
  org_CN.rot      = rotate(org_CN, left = FALSE)

  rotated.gridid  = cellFromXY(org_CN.rot, gridid.xy.rot)

  # add rotated.gridid to gridid.dt
  gridid.dt[, gridid.rotated := rotated.gridid]
  gridid.dt[, x.rotated      := gridid.xy.rot$x]

  # add pset info
  pset.dt         = fread(paste(pkg.env$gis_path, 'pset_values_maiz_00s.csv',       sep='/'))
  pset.dt         = pset.dt[, pset_seq := NULL]
  gridid.dt       = gridid.dt[pset.dt, on = .(pset_id = pset_id)]
  gridid.dt       = gridid.dt[!is.na(gridid),]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'crop', 'irr', 'pset_id'))

  # add ssp and gcm
  gcm.col        = gridid.dt[, .(gcm = rep(cmip6_calendars$gcm)), by = .(gridid, irr)]
  gcm.ssp.col    = gcm.col[gcm != 'historical', .(ssp = rep(c('ssp126', 'ssp370'))), by = .(gridid, irr, gcm)]
  hist.hist.col  = gcm.col[gcm %in% 'historical', .(ssp = rep(c('historical'))), by = .(gridid, irr, gcm)]
  gcm.ssp.hist   = rbind(gcm.ssp.col, hist.hist.col)
  setorder(gcm.ssp.hist, gridid)

  # add scenario column, filter, and order
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ntill-res','ccg', 'ccl', 'ccg-res', 'ccl-res',
                                                    'ccg-ntill', 'ccl-ntill'))), by = irr]
  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  setorder(gridid.dt, gridid, ssp, gcm, irr)

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar[[1:2]], min_N[[1]], org_N, org_CN, residue_return, ipcc_clim)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code Na to 0
  raster_table[is.na((fertN.amt_mai)), fertN.amt_mai := 0][is.na(orgN.amt), orgN.amt := 0][is.na(orgCN.ratio), orgCN.ratio := 0][is.na(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date_Maize := round(plant.date_Maize, digits = 0)][, harvest.date_Maize := round(harvest.date_Maize, digits = 0)]

  # rename
  names(raster_table)[4:6] = c('plant.date', 'harvest.date', 'fertN.amt')
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]
  raster_table[, res.rtrn.amt := round(res.rtrn.amt, digits = 2)]

  # join DT
  main_table   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table, c('gridid', 'gridid.rotated', 'x', 'x.rotated', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  length(unique(main_table[,gridid])) # check = 30268
#-----------------------------------------------------------------------------------------------
  # SOYBEAN
  gridid.dt       = fread(paste(pkg.env$gis_path, 'soy_lu_cohort_id.csv',          sep='/'))

  # create a vector of rotated grid cell numbers needed for weather file script
  gridid.xy       = setDT(as.data.frame(xyFromCell(org_CN, gridid.dt[,gridid])))
  gridid.xy.rot   = gridid.xy
  gridid.xy.rot$x = (gridid.xy.rot$x + 360) %% 360 # http://www.idlcoyote.com/map_tips/lonconvert.html
  org_CN.rot      = rotate(org_CN, left = FALSE)

  rotated.gridid  = cellFromXY(org_CN.rot, gridid.xy.rot)

  # add rotated.gridid to gridid.dt
  gridid.dt[, gridid.rotated := rotated.gridid]
  gridid.dt[, x.rotated      := gridid.xy.rot$x]

  # add pset info
  # N.B. number of cols may differ from maize
  pset.dt         = fread(paste(pkg.env$gis_path, 'pset_values_soy_00s.csv',       sep='/'))
  pset.dt         = pset.dt[, pset_seq := NULL]
  gridid.dt       = gridid.dt[pset.dt, on = .(pset_id = pset_id)]
  gridid.dt       = gridid.dt[!is.na(gridid),]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'crop', 'irr', 'pset_id'))

  # add ssp and gcm
  gcm.col        = gridid.dt[, .(gcm = rep(cmip6_calendars$gcm)), by = .(gridid, irr)]
  gcm.ssp.col    = gcm.col[gcm != 'historical', .(ssp = rep(c('ssp126', 'ssp370'))), by = .(gridid, irr, gcm)]
  hist.hist.col  = gcm.col[gcm %in% 'historical', .(ssp = rep(c('historical'))), by = .(gridid, irr, gcm)]
  gcm.ssp.hist   = rbind(gcm.ssp.col, hist.hist.col)
  setorder(gcm.ssp.hist, gridid)

  # add scenario column, filter, and order
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ntill-res','ccg', 'ccl', 'ccg-res', 'ccl-res',
                                                    'ccg-ntill', 'ccl-ntill'))), by = irr]

  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  setorder(gridid.dt, gridid, ssp, gcm, irr)

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar[[3:4]], min_N[[2]], org_N, org_CN, residue_return, ipcc_clim)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code Na to 0
  raster_table[is.na((fertN.amt_soy)), fertN.amt_soy := 0][is.na(orgN.amt), orgN.amt := 0][is.na(orgCN.ratio), orgCN.ratio := 0][is.na(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date_Soybeans := round(plant.date_Soybeans, digits = 0)][, harvest.date_Soybeans := round(harvest.date_Soybeans, digits = 0)]

  # rename
  names(raster_table)[4:6] = c('plant.date', 'harvest.date', 'fertN.amt')
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]
  raster_table[, res.rtrn.amt := round(res.rtrn.amt, digits = 2)]

  # join DT
  main_table.soy   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table.soy, c('gridid', 'gridid.rotated', 'x', 'x.rotated', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  length(unique(main_table.soy[,gridid])) # check = 19094
#-----------------------------------------------------------------------------------------------
  # WWHEAT
  gridid.dt       = fread(paste(pkg.env$gis_path, 'wwheat_lu_cohort_id.csv',          sep='/'))

  # create a vector of rotated grid cell numbers needed for weather file script
  gridid.xy       = setDT(as.data.frame(xyFromCell(org_CN, gridid.dt[,gridid])))
  gridid.xy.rot   = gridid.xy
  gridid.xy.rot$x = (gridid.xy.rot$x + 360) %% 360 # http://www.idlcoyote.com/map_tips/lonconvert.html
  org_CN.rot      = rotate(org_CN, left = FALSE)

  rotated.gridid  = cellFromXY(org_CN.rot, gridid.xy.rot)

  # add rotated.gridid to gridid.dt
  gridid.dt[, gridid.rotated := rotated.gridid]
  gridid.dt[, x.rotated      := gridid.xy.rot$x]

  # add pset info
  # N.B. number of cols may differ from maize
  pset.dt         = fread(paste(pkg.env$gis_path, 'pset_values_wwheat_00s.csv',       sep='/'))
  pset.dt         = pset.dt[, pset_seq := NULL]
  gridid.dt       = gridid.dt[pset.dt, on = .(pset_id = pset_id)]
  gridid.dt       = gridid.dt[!is.na(gridid),]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'crop', 'irr', 'pset_id'))

  # add ssp and gcm
  gcm.col        = gridid.dt[, .(gcm = rep(cmip6_calendars$gcm)), by = .(gridid, irr)]
  gcm.ssp.col    = gcm.col[gcm != 'historical', .(ssp = rep(c('ssp126', 'ssp370'))), by = .(gridid, irr, gcm)]
  hist.hist.col  = gcm.col[gcm %in% 'historical', .(ssp = rep(c('historical'))), by = .(gridid, irr, gcm)]
  gcm.ssp.hist   = rbind(gcm.ssp.col, hist.hist.col)
  setorder(gcm.ssp.hist, gridid)

  # add scenario column, filter, and order
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ntill-res'))), by = irr]

  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  setorder(gridid.dt, gridid, ssp, gcm, irr)

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar[[7:8]], min_N[[3]], org_N, org_CN, residue_return, ipcc_clim)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code Na to 0 or mean value
  orgCN.wwheat_mean = mean(raster_table[,orgCN.ratio], na.rm = TRUE)

  raster_table[is.na((fertN.amt_whe)), fertN.amt_whe := 0][is.na(orgN.amt), orgN.amt := 0][is.na(orgCN.ratio), orgCN.ratio := orgCN.wwheat_mean][is.na(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date_WWheat := round(plant.date_WWheat, digits = 0)][, harvest.date_WWheat := round(harvest.date_WWheat, digits = 0)]

  # rename
  names(raster_table)[4:6] = c('plant.date', 'harvest.date', 'fertN.amt')
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]
  raster_table[, res.rtrn.amt := round(res.rtrn.amt, digits = 2)]

  # join DT
  main_table.wwh   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table.wwh, c('gridid', 'gridid.rotated', 'x', 'x.rotated', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  gc()

  length(unique(main_table.wwh[,gridid])) # check = 7563
#-----------------------------------------------------------------------------------------------
  # SWHEAT
  # N.B. number of cols may differ from maize

  gridid.dt       = fread(paste(pkg.env$gis_path, 'swheat_lu_cohort_id.csv',          sep='/'))

  # create a vector of rotated grid cell numbers needed for weather file script
  gridid.xy       = setDT(as.data.frame(xyFromCell(org_CN, gridid.dt[,gridid])))
  gridid.xy.rot   = gridid.xy
  gridid.xy.rot$x = (gridid.xy.rot$x + 360) %% 360 # http://www.idlcoyote.com/map_tips/lonconvert.html
  org_CN.rot      = rotate(org_CN, left = FALSE)

  rotated.gridid  = cellFromXY(org_CN.rot, gridid.xy.rot)

  # add rotated.gridid to gridid.dt
  gridid.dt[, gridid.rotated := rotated.gridid]
  gridid.dt[, x.rotated      := gridid.xy.rot$x]

  # add pset info
  # N.B. number of cols may differ from maize
  pset.dt         = fread(paste(pkg.env$gis_path, 'pset_values_swheat_00s.csv',       sep='/'))
  pset.dt         = pset.dt[, pset_seq := NULL]
  gridid.dt       = gridid.dt[pset.dt, on = .(pset_id = pset_id)]
  gridid.dt       = gridid.dt[!is.na(gridid),]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'crop', 'irr', 'pset_id'))

  # add ssp and gcm
  gcm.col        = gridid.dt[, .(gcm = rep(cmip6_calendars$gcm)), by = .(gridid, irr)]
  gcm.ssp.col    = gcm.col[gcm != 'historical', .(ssp = rep(c('ssp126', 'ssp370'))), by = .(gridid, irr, gcm)]
  hist.hist.col  = gcm.col[gcm %in% 'historical', .(ssp = rep(c('historical'))), by = .(gridid, irr, gcm)]
  gcm.ssp.hist   = rbind(gcm.ssp.col, hist.hist.col)
  setorder(gcm.ssp.hist, gridid)

  # add scenario column, filter, and order
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ntill-res','ccg', 'ccl', 'ccg-res', 'ccl-res',
                                                    'ccg-ntill', 'ccl-ntill'))), by = irr]
  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  setorder(gridid.dt, gridid, ssp, gcm, irr)

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar[[5:6]], min_N[[3]], org_N, org_CN, residue_return, ipcc_clim)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code Na to 0 or mean value
  orgCN.swheat_mean = mean(raster_table[,orgCN.ratio], na.rm = TRUE)

  raster_table[is.na((fertN.amt_whe)), fertN.amt_whe := 0][is.na(orgN.amt), orgN.amt := 0][is.na(orgCN.ratio), orgCN.ratio := orgCN.swheat_mean][is.na(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date_SWheat := round(plant.date_SWheat, digits = 0)][, harvest.date_SWheat := round(harvest.date_SWheat, digits = 0)]

  # rename
  names(raster_table)[4:6] = c('plant.date', 'harvest.date', 'fertN.amt')
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]
  raster_table[, res.rtrn.amt := round(res.rtrn.amt, digits = 2)]

  # join DT
  main_table.swh   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table.swh, c('gridid', 'gridid.rotated', 'x','x.rotated','y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  length(unique(main_table.swh[,gridid])) # check = 9256
  gc()
#-----------------------------------------------------------------------------------------------
  main_table = rbind(main_table, main_table.soy, fill = TRUE)
  rm(main_table.soy)
  gc()
  main_table = rbind(main_table, main_table.wwh, fill = TRUE)
  rm(main_table.wwh)
  gc()
  main_table = rbind(main_table, main_table.swh, fill = TRUE)
  rm(main_table.swh)
  gc()

  setorder(main_table, gridid)
  gc()

  length(unique(main_table[, gridid])) # 33316
#-----------------------------------------------------------------------------------------------
  #DEPRECATED
  # ADD EQ File Name
  # eq.file.lookup = fread(paste(pkg.env$gis_path, 'eq_file_assign_by-gridid.csv',       sep='/'))
  # main_table     = main_table[eq.file.lookup, on = .(gridid = gridid, regionid = regionid)]
  # main_table     = main_table[!is.na(gridid.rotated),]
  # gc()
  # length(unique(main_table[, gridid])) # 33316
#-----------------------------------------------------------------------------------------------
  # ADD Country, Region Name
  country.sf  = st_read(paste(pkg.env$gis_path, 'WB_countries_Admin0_10m.shp', sep = '/'))
  country.sf_dt = setDT(as.data.frame(country.sf))

  # CREATE raster
  shp_r       = rast(ext(country.sf), nrow = 360, ncol = 720)

  # CREATE shp as raster
  country_r   = terra::rasterize(country.sf, shp_r, fun = 'sum', "OBJECTID")

  # MATCH resolution of simulation data, dimensions the same
  target.r    = rast(nrow = 360, ncol = 720, resolution = 0.5)
  country_r   = resample(country_r, target.r, method = "near")
  country_r   = focal(country_r, w=9, fun = "modal", na.policy = "only", na.rm = TRUE) # needed to capture all gridid
  names(country_r) = "OBJECTID"

  # CREATE data.frame, merge
  country_r.dt    = as.data.frame(country_r, cells=TRUE, xy=TRUE)
  country_r.dt    = setDT(country_r.dt)

  country_n = data.table(WB_NAME = country.sf_dt$WB_NAME, ID = country.sf_dt$OBJECTID,
                         WB_REGION = country.sf_dt$WB_REGION)

  # BIND to cell numbers
  country_r.dt = country_r.dt[country_n, on = .(OBJECTID = ID)]

  main_table = main_table[country_r.dt[, .(cell, WB_NAME, WB_REGION)], on = .(gridid = cell)]
  gc()
  setorder(main_table, gridid)
  gc()
  main_table = main_table[!is.na(crop),]
  gc()

  length(unique(main_table[, gridid])) # 33316
#-----------------------------------------------------------------------------------------------
  # ADD fertilizer fraction by country
  n_fert_ifa = fread(paste(pkg.env$gis_path, 'n-fertilizer-fraction-by-country.csv', sep = '/'))
  n_fert_ifa[Country %in% 'U.S.A.', Country := 'United States of America']
  n_fert_ifa[Country %in% 'Czechia', Country := 'Czech Republic']
  n_fert_ifa[Country %in% 'Kyrgyzstan', Country := 'Kyrgyz Republic']
  n_fert_ifa[Country %in% 'Congo, Rep. Of', Country := 'Congo, Rep. of']
  n_fert_ifa[Country %in% 'Korea DPR', Country := "Korea, Democratic People's Republic of"]
  n_fert_ifa[Country %in% 'Korea Republic', Country := "Korea, Republic of"]
  n_fert_ifa[Country %in% 'Moldova Republic of', Country := 'Moldova']
  n_fert_ifa[Country %in% 'Iran', Country := 'Iran, Islamic Republic of']
  n_fert_ifa[Country %in% 'Syria', Country := 'Syrian Arab Republic']
  n_fert_ifa[Country %in% 'Egypt', Country := 'Egypt, Arab Republic of']
  n_fert_ifa[Country %in% 'Venezuela', Country := 'Venezuela, Republica Bolivariana de']
  n_fert_ifa[Country %in% 'Cote dIvoire', Country := "Côte d'Ivoire"]
  n_fert_ifa[Country %in% 'Viet Nam', Country := 'Vietnam']
  n_fert_ifa = n_fert_ifa[complete.cases(n_fert_ifa)]
  setorder(n_fert_ifa, Country)
  n_fert_ifa[, WB_REGION := NULL]

  main_table = main_table[n_fert_ifa, on = .(WB_NAME = Country),
                          by = .EACHI, allow.cartesian = TRUE]
  length(unique(main_table[, gridid])) # 33316
  gc()

  main_table = main_table[!is.na(WB_REGION)]
  setorder(main_table, gridid)
  length(unique(main_table[, gridid])) # 33316

  setcolorder(main_table, c('gridid', 'gridid.rotated', 'x', 'y', 'WB_NAME', 'WB_REGION','regionid', 'ssp', 'gcm',
                            'crop', 'irr','scenario','pset_id', 'clim_ipcc'))
  gc()

  main_table = unique(main_table)
  gc()
  length(unique(main_table[, gridid])) # 33316
#-----------------------------------------------------------------------------------------------
save(main_table, file = paste0(save_path,'/','cell_data_table_', date, '.RData'))
#-----------------------------------------------------------------------------------------------
maiz.gridid = unique(main_table[crop %in% 'maiz', c('gridid','irr','crop')])
fwrite(maiz.gridid, paste0(save_path,'/','cell_data_table_maiz_gridid.csv'))
soyb.gridid = unique(main_table[crop %in% 'soyb', c('gridid','irr','crop')])
fwrite(soyb.gridid, paste0(save_path,'/','cell_data_table_soyb_gridid.csv'))
wwh.gridid = unique(main_table[crop %in% 'wwht', c('gridid','irr','crop')])
fwrite(wwh.gridid, paste0(save_path,'/','cell_data_table_wwht_gridid.csv'))
swh.gridid = unique(main_table[crop %in% 'swht', c('gridid','irr','crop')])
fwrite(swh.gridid, paste0(save_path,'/','cell_data_table_swht_gridid.csv'))
