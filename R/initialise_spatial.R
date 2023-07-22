#' Create a data.table containing all variables required to run a DayCent simulation
#'
#' Returns a large table of all data required to run global simulations with CSU spin-ups.
#'
#' This function generates a data.table with rows for every combination of scenario, crop and ssp
#' and the raster cell numbers corresponding to locations where that crop is grown globally.
#' Data values are automatically populated using spatial crop calendar, crop management, and soil data.
#'
#' @import data.table
#' @import terra
#' @import sf
#' @export
create_cell_data_csu_table = function(cmip6_calendars) {
  # create rasters
  crop_calendar  = rast(paste(pkg.env$gis_path, 'crop_calendar.tif',                 sep='/'))
  min_N          = rast(paste(pkg.env$gis_path, 'fertilizer-n_by_crop.tif',          sep='/'))
  org_N          = rast(paste(pkg.env$gis_path, 'manure-n_cropland.tif',             sep='/'))
  org_CN         = rast(paste(pkg.env$gis_path, 'global_manure_weighted_CN_agg.tif', sep='/'))
  residue_return = rast(paste(pkg.env$gis_path, 'wirsenius_global_res_rtrn_agg.tif', sep='/'))
  ipcc_clim      = rast(paste(pkg.env$gis_path, 'ipcc_climate_agg.tif',              sep='/'))

  # tidy variables, names
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

  # load gridid, irrigation table for each crop
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
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ccg', 'ccl', 'ccg-ntill', 'ccl-ntill','rewild'))), by = irr]
  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  # gridid.dt      = gridid.dt[irr != 1 | scenario != 'rewild',]
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

  raster_table = raster_table[complete.cases(raster_table)]

  # join DT
  main_table   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table, c('gridid', 'gridid.rotated', 'x', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  # remove any plant/harv dates == 0
  main_table = main_table[plant.date !=0,]
  main_table = main_table[harvest.date !=0,]

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
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ccg', 'ccl', 'ccg-ntill', 'ccl-ntill','rewild'))), by = irr]
  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  # gridid.dt      = gridid.dt[irr != 1 | scenario != 'rewild',]
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

  raster_table = raster_table[complete.cases(raster_table)]

  # join DT
  main_table.soy   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table.soy, c('gridid', 'gridid.rotated', 'x', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  # remove any plant/harv dates == 0
  main_table.soy = main_table.soy[plant.date !=0,]
  main_table.soy = main_table.soy[harvest.date !=0,]

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
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv','res','ntill','rewild'))), by = irr]
  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  # gridid.dt      = gridid.dt[irr != 1 | scenario != 'rewild',]
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  setorder(gridid.dt, gridid, ssp, gcm, irr)

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar[[7:8]], min_N[[3]], org_N, org_CN, residue_return, ipcc_clim)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code Na to 0
  raster_table[is.na((fertN.amt_whe)), fertN.amt_whe := 0][is.na(orgN.amt), orgN.amt := 0][is.na(orgCN.ratio), orgCN.ratio := 0][is.na(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date_WWheat := round(plant.date_WWheat, digits = 0)][, harvest.date_WWheat := round(harvest.date_WWheat, digits = 0)]

  # rename
  names(raster_table)[4:6] = c('plant.date', 'harvest.date', 'fertN.amt')
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]
  raster_table[, res.rtrn.amt := round(res.rtrn.amt, digits = 2)]

  raster_table = raster_table[complete.cases(raster_table)]

  # join DT
  main_table.wwh   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table.wwh, c('gridid', 'gridid.rotated', 'x', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  # remove any plant/harv dates == 0
  main_table.wwh = main_table.wwh[plant.date !=0,]
  main_table.wwh = main_table.wwh[harvest.date !=0,]

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
  scenario.col   = gridid.dt[, .('scenario' = rep(c('conv', 'res', 'ntill', 'ccg', 'ccl', 'ccg-ntill', 'ccl-ntill','rewild'))), by = irr]
  gridid.dt      = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  gridid.dt      = gridid.dt[gcm.ssp.hist, on = .(gridid = gridid, irr = irr), by = .EACHI, allow.cartesian = TRUE]
  setorder(gridid.dt, gridid)
  # gridid.dt      = gridid.dt[irr != 1 | scenario != 'rewild',]
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))
  setorder(gridid.dt, gridid, ssp, gcm, irr)

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar[[5:6]], min_N[[3]], org_N, org_CN, residue_return, ipcc_clim)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code Na to 0
  raster_table[is.na((fertN.amt_whe)), fertN.amt_whe := 0][is.na(orgN.amt), orgN.amt := 0][is.na(orgCN.ratio), orgCN.ratio := 0][is.na(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date_SWheat := round(plant.date_SWheat, digits = 0)][, harvest.date_SWheat := round(harvest.date_SWheat, digits = 0)]

  # rename
  names(raster_table)[4:6] = c('plant.date', 'harvest.date', 'fertN.amt')
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]
  raster_table[, res.rtrn.amt := round(res.rtrn.amt, digits = 2)]

  raster_table = raster_table[complete.cases(raster_table)]

  # join DT
  main_table.swh   = gridid.dt[raster_table, on = .(gridid = cell)]
  setcolorder(main_table.swh, c('gridid', 'gridid.rotated', 'x', 'y','regionid', 'ssp', 'gcm','crop', 'irr','scenario','pset_id'))

  # remove any plant/harv dates == 0
  main_table.swh = main_table.swh[plant.date !=0,]
  main_table.swh = main_table.swh[harvest.date !=0,]
  gc()

  main_table = rbind(main_table, main_table.soy, fill = TRUE)
  main_table = rbind(main_table, main_table.wwh, fill = TRUE)
  main_table = rbind(main_table, main_table.swh, fill = TRUE)

  setorder(main_table, gridid)
  gc()

  # ADD EQ File Name
  eq.file.lookup = fread(paste(pkg.env$gis_path, 'eq_file_assign_by-gridid.csv',       sep='/'))
  main_table     = main_table[eq.file.lookup, on = .(gridid = gridid, regionid = regionid)]
  main_table     = main_table[!is.na(gridid.rotated),]
  gc()

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
  main_table = main_table[!is.na(crop),]
  gc()

  # ADD fertilizer fraction by country
  n_fert_ifa = fread(paste(pkg.env$gis_path, 'n-fertilizer-fraction-by-country.csv', sep = '/'))
  n_fert_ifa[Country %in% 'U.S.A.', Country := 'United States of America']
  n_fert_ifa[Country %in% 'Czechia', Country := 'Czech Republic']
  n_fert_ifa[Country %in% 'Moldova Republic of', Country := 'Moldova']
  n_fert_ifa[Country %in% 'Kyrgyzstan', Country := 'Kyrgyz Republic']
  n_fert_ifa[Country %in% 'Korea DPR', Country := "Korea, Democratic People's Republic of"]
  n_fert_ifa[Country %in% 'Iran', Country := 'Iran, Islamic Republic of']
  n_fert_ifa[Country %in% 'Syria', Country := 'Syrian Arab Republic']
  n_fert_ifa[Country %in% 'Egypt', Country := 'Egypt, Arab Republic of']
  n_fert_ifa[Country %in% 'Venezuela', Country := 'Venezuela, Republica Bolivariana de']
  n_fert_ifa[Country %in% 'Cote dIvoire', Country := "Côte d'Ivoire"]
  n_fert_ifa[Country %in% 'Viet Nam', Country := 'Vietnam']
  n_fert_ifa = n_fert_ifa[complete.cases(n_fert_ifa)]
  setorder(n_fert_ifa, Country)

  main_table = main_table[n_fert_ifa, on = .(WB_NAME = Country, WB_REGION = WB_REGION),
                          by = .EACHI, allow.cartesian = TRUE]
  gc()
  main_table = main_table[!is.na(gridid)]
  setorder(main_table, gridid)
  setcolorder(main_table, c('gridid', 'gridid.rotated', 'x', 'y', 'WB_NAME', 'WB_REGION','regionid', 'ssp', 'gcm',
                            'crop', 'irr','scenario','pset_id', 'clim_ipcc'))
  # ADD Cropping Area
  crop_area_r      = rast(paste(pkg.env$gis_path, 'all-cropland-rf-ir-area.tif', sep = '/'))
  crop_area_dt     = as.data.table(terra::as.data.frame(crop_area_r, xy = TRUE, cells = TRUE))
  gridid_all       = unique(main_table[, gridid])
  crop_area_dt_f   = crop_area_dt[cell %in% gridid_all,]
  crop_area_dt_f   = crop_area_dt_f[wht_irr_area_ha >= 1 | wht_rain_area_ha >= 1 | maiz_rain_area_ha >= 1 |
                   maiz_irr_area_ha >= 1 | soyb_rain_area_ha >=1 | soyb_irr_area_ha >= 1,]
  gridid_f         = unique(crop_area_dt_f[,cell])

  # REMOVE < 1ha
  main_table = main_table[gridid %in% gridid_f,]
  gc()

  fwrite(main_table, paste("/home/shelby/Documents/projects/SoilFutures/data-raw","cell_data_table_21July23.csv", sep = "/")) #csv
  save(main_table, file = paste("/home/shelby/Documents/projects/SoilFutures/data-raw","cell_data_table_21July23.RData", sep = "/")) #rda
  return(main_table)
}
