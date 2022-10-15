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
#' @export
create_cell_data_csu_table = function() {
  # create rasters
  crop_calendar  = rast(paste(pkg.env$gis_path, 'crop_calendar.tif',                 sep='/'))
  min_N          = rast(paste(pkg.env$gis_path, 'fertilizer-n_by_crop.tif',          sep='/'))
  org_N          = rast(paste(pkg.env$gis_path, 'manure-n_cropland.tif',             sep='/'))
  org_CN         = rast(paste(pkg.env$gis_path, 'global_manure_weighted_CN_agg.tif', sep='/'))
  residue_return = rast(paste(pkg.env$gis_path, 'wirsenius_global_res_rtrn_agg.tif', sep='/'))

  # tidy variables, names, and subset for maize-only
  crop_calendar        = crop_calendar['Maize']
  crop_calendar        = crop_calendar[[c(3,7)]]
  names(crop_calendar) = c('plant.date','harvest.date')

  min_N                 = min_N['mai']
  min_N                 = min_N[[165]]
  min_N                 = min_N/10 # g m-2
  names(min_N)          = 'fertN.amt'

  org_N                 = org_N[[165]]
  names(org_N)          = 'orgN.amt'

  names(org_CN)         = 'orgCN.ratio'

  names(residue_return) = 'res.rtrn.amt'

  # load gridid, irrigation table
  gridid.dt       = fread(paste(pkg.env$gis_path, 'run_seq_with_pset.csv',          sep='/'))
  gridid.dt       = gridid.dt[, run_seq := NULL]

  # create a vector of rotated grid cell numbers needed for weather file script
  gridid.xy       = setDT(as.data.frame(xyFromCell(org_CN, gridid.dt[,gridid])))
  gridid.xy.rot   = gridid.xy
  gridid.xy.rot$x = (gridid.xy.rot$x + 360) %% 360 # http://www.idlcoyote.com/map_tips/lonconvert.html
  org_CN.rot      = rotate(org_CN, left = FALSE)

  rotated.gridid  = cellFromXY(org_CN.rot, gridid.xy.rot)

  # add rotated.gridid to gridid.dt
  gridid.dt[, gridid.rotated := rotated.gridid]

  # add scenario column, filter, and order
  scenario.col = gridid.dt[, .('scenario' = rep(c('conv','res','ntill','ccg','ccl','rewild'))), by = irr]
  gridid.dt = unique(gridid.dt[scenario.col, on = .(irr = irr), by = .EACHI, allow.cartesian = TRUE])
  setorder(gridid.dt, gridid)
  gridid.dt[irr != 1 | scenario != 'rewild',]
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'pset_id', 'crop', 'irr'))

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar, min_N, org_N, org_CN, residue_return)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # code NaN to 0
  raster_table[is.nan((fertN.amt)), fertN.amt := 0][is.nan(orgN.amt), orgN.amt := 0][is.nan(orgCN.ratio), orgCN.ratio := 0][is.nan(res.rtrn.amt), res.rtrn.amt := 0]
  raster_table[, plant.date := round(plant.date, digits = 0)][, harvest.date := round(harvest.date, digits = 0)]
  raster_table[, fertN.amt := round(fertN.amt, digits = 1)][, orgN.amt := round(orgN.amt, digits = 1)][, orgCN.ratio := round(orgCN.ratio, digits = 1)]

  # what to do about plant/harv with NaN? remove? or use focal?
  # raster_table_var  = names(raster_table)
  NaNdt = raster_table[is.nan(plant.date),] # loss of 539 grid cells; 1.6% of grid cells

  # join DT
  main_table = gridid.dt[raster_table, on = .(gridid = cell)]

  return(main_table)
  fwrite(main_table, paste(pkg.env$out_path,"cell_data_table_csu.csv", sep = "/")) #csv
  save(main_table, file = paste(pkg.env$out_path,"cell_data_table_csu.RData", sep = "/")) #rda
}

#' Create a data.table containing all variables required to run a DayCent simulation
#'
#' Returns a large table of all data required to run global simulations with SPAM crop area.
#'
#' This function generates a data.table with rows for every combination of scenario, crop and ssp
#' and the raster cell numbers corresponding to locations where that crop is grown globally.
#' Data values are automatically populated using spatial crop calendar, crop management, and soil data.
#'
#' @import data.table
#' @import terra
#' @export
create_cell_data_table = function() {
  # create rasters
  crop_calendar  = rast(paste(pkg.env$gis_path, 'crop_calendar.tif',                 sep='/'))
  min_N          = rast(paste(pkg.env$gis_path, 'fertilizer-n_by_crop.tif',          sep='/'))
  org_N          = rast(paste(pkg.env$gis_path, 'manure-n_cropland.tif',             sep='/'))
  org_CN         = rast(paste(pkg.env$gis_path, 'global_manure_weighted_CN_agg.tif', sep='/'))
  residue_return = rast(paste(pkg.env$gis_path, 'wirsenius_global_res_rtrn_agg.tif', sep='/'))
  bdod           = rast(paste(pkg.env$gis_path, 'bdod_0-100cm_mean_agg.tif',         sep='/'))
  clay           = rast(paste(pkg.env$gis_path, 'clay_0-100cm_mean_agg.tif',         sep='/'))
  phh2o          = rast(paste(pkg.env$gis_path, 'phh2o_0-100cm_mean_agg.tif',        sep='/'))
  sand           = rast(paste(pkg.env$gis_path, 'sand_0-100cm_mean_agg.tif',         sep='/'))
  soc            = rast(paste(pkg.env$gis_path, 'soc_0-100cm_mean_agg.tif',          sep='/'))
  # tidy variables, names, and subset for maize-only
  crop_calendar        = crop_calendar['Maize']
  crop_calendar        = crop_calendar[[c(3,7)]]
  names(crop_calendar) = c('plant.date','harvest.date')

  min_N                 = min_N['mai']
  min_N                 = min_N[[165]]
  min_N                 = min_N/10 # g m-2
  names(min_N)          = 'fertN.amt'

  org_N                 = org_N[[165]]
  names(org_N)          = 'orgN.amt'

  names(org_CN)         = 'orgCN.ratio'

  names(residue_return) = 'res.rtrn.amt'

  bdod                  = bdod[[c(1,3,2,5,4)]]
  clay                  = clay[[c(1,3,2,5,4)]]
  phh2o                 = phh2o[[c(1,3,2,5,4)]]
  sand                  = sand[[c(1,3,2,5,4)]]
  soc                   = soc[[c(1,3,2,5,4)]]

  # UPDATE - create rotated for all cell - no filtering
  # create a vector of rotated grid cell numbers needed for weather file script
  gridid.xy       = setDT(as.data.frame(xyFromCell(org_CN, gridid.dt[,gridid])))
  gridid.xy.rot   = gridid.xy
  gridid.xy.rot$x = (gridid.xy.rot$x + 360) %% 360 # http://www.idlcoyote.com/map_tips/lonconvert.html
  org_CN.rot      = rotate(org_CN, left = FALSE)

  rotated.gridid  = cellFromXY(org_CN.rot, gridid.xy.rot)

  # add rotated.gridid to gridid.dt
  gridid.dt[, gridid.rotated := rotated.gridid]
  setcolorder(gridid.dt, c('gridid', 'gridid.rotated', 'regionid', 'pset_id', 'crop', 'irr'))

  # merge rasters and create data.table filtered for gridid
  all_raster      = c(crop_calendar, min_N, org_N, org_CN, residue_return,
                      bdod, clay, phh2o, sand, soc)
  raster_table    = as.data.frame(all_raster, xy = TRUE, cells = TRUE, na.rm = FALSE)
  raster_table    = setDT(raster_table)
  unique.gridid   = sort(unique(gridid.dt$gridid))

  raster_table = raster_table[cell %in% unique.gridid,]

  # raster_table_var  = names(raster_table)

  # hybrid DT (each row = scenario for gridid)
  # code NA to 0 (for most continuous variables, double check this)
  #

  # # wide to long format
  # main_table     = melt(main_table, id.vars = c("cell"),
  #                        measure.vars = main_table_var[-1])
  # # group variables by cell
  # main_table     = main_table[order(rank(cell))]
  # # round plant and harvest dates
  # main_table[variable %in% names(crop_calendar)[c(3,7,14,18,25,29,36,40)], round(value, digits=0)]

  # use focal to align crop_area and crop_cal?
  # keep these rotated cell numbers, could add row with main cell # and the unrotated cell #
  # consider deleting NaN crop_area cells (but hold off until aligned with Yi's sims)
  # need to add a 'crop_code' level in variable column

  return(main_table)
  fwrite(main_table, paste(pkg.env$out_path,"cell_data_table.csv", sep = "/")) #csv
  save(main_table, file = paste(pkg.env$out_path,"cell_data_table.RData", sep = "/")) #rda
}

# #' This function tests if crop area is allocated to winter or spring wheat where wheat is grown.
# #'
# #' @import data.table
# #' @importFrom lubridate ymd
# #' @import terra
# #' @export
# wwheat = function(main_table, mat, map, N_or_S) {
#   # first need to identify if Wheat_A > 0
#   # need to test if crop_calendar indicates WWheat and SWheat grown...
#   # test whether midwinter falls inside the interval between planting and harvesting
#   plant_day = main_table$plant_date #update to correct name (WWheat or SWheat)
#   harv_day =  main_table$harv_date #update to correct name (WWheat or SWheat)
#   if (harv_day < plant_day) harv_day = harv_day + 365
#   midwinter = yday(ymd(c(150621,151221)))[(N_or_S == 'N') + 1L]
#   has_winter = midwinter %between% c(plant_day, harv_day)
#   # winter wheat is determined by having a cold month, a long growing season, and midwinter falling inside the growing period
#   wwheat = (temp_coldest_month %between% c(-10, 7) & (calendar$tot.days > 150) & (gsd >= 335) & has_winter)
# }
