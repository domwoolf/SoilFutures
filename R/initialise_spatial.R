#' Create a data.table containing all variables required to run a DayCent simulation
#'
#' Returns a large table of all data required to run global simulations.
#'
#' This function generates a data.table with rows for every combination of scenario, crop and ssp
#' and the raster cell numbers corresponding to locations where that crop is grown globally.
#' Data values are automatically populated using spatial crop calendar, crop management, and soil data.
#'
#' @import data.table
#' @import terra
#' @export
create_cell_data_table = function() {

  crop_area      = rast(paste(pkg.env$gis_path, 'spam_crop_area_agg_.tif',           sep='/'))
  crop_calendar  = rast(paste(pkg.env$gis_path, 'crop_calendar.tif',                 sep='/'))
  tillage        = rast(paste(pkg.env$gis_path, 'tillage_by_crop_agg.tif',           sep='/'))
  min_N          = rast(paste(pkg.env$gis_path, 'fertilizer-n_by_crop.tif',          sep='/'))
  org_N          = rast(paste(pkg.env$gis_path, 'manure-n_cropland.tif',             sep='/'))
  org_CN         = rast(paste(pkg.env$gis_path, 'global_manure_weighted_CN_agg.tif', sep='/'))
  residue_return = rast(paste(pkg.env$gis_path, 'wirsenius_global_res_rtrn_agg.tif', sep='/'))
  bdod           = rast(paste(pkg.env$gis_path, 'bdod_0-100cm_mean_agg.tif',         sep='/'))
  clay           = rast(paste(pkg.env$gis_path, 'clay_0-100cm_mean_agg.tif',         sep='/'))
  phh2o          = rast(paste(pkg.env$gis_path, 'phh2o_0-100cm_mean_agg.tif',        sep='/'))
  sand           = rast(paste(pkg.env$gis_path, 'sand_0-100cm_mean_agg.tif',         sep='/'))
  soc            = rast(paste(pkg.env$gis_path, 'soc_0-100cm_mean_agg.tif',          sep='/'))

  names(org_N)   = paste(names(org_N), "org_N", sep = "_")

  all_raster     = c(crop_area, crop_calendar, tillage, min_N, org_N, org_CN, residue_return,
                     bdod, clay, phh2o, sand, soc)
  # create data.table
  main_table     = as.data.frame(all_raster, xy = TRUE, cells = TRUE)
  main_table     = setDT(main_table)

  main_table_var = names(main_table)
  # wide to long format
  main_table     = melt(main_table, id.vars = c("cell"),
                         measure.vars = main_table_var[-1])
  # group variables by cell
  main_table     = main_table[order(rank(cell))]
  # round plant and harvest dates
  main_table[variable %in% names(crop_calendar)[c(3,7,14,18,25,29,36,40)], round(value, digits=0)]

  # use focal to align crop_area and crop_cal?
  # keep these rotated cell numbers, could add row with main cell # and the unrotated cell #
  # consider deleting NaN crop_area cells (but hold off until aligned with Yi's sims)
  # need to add a 'crop_code' level in variable column

  return(main_table)
  fwrite(main_table, paste(pkg.env$out_path,"cell_data_table.csv", sep = "/")) #csv
  save(main_table, file = paste(pkg.env$out_path,"cell_data_table.RData", sep = "/")) #rda
}

#' This function tests if crop area is allocated to winter or spring wheat where wheat is grown.
#'
#' @import data.table
#' @importFrom lubridate ymd
#' @import terra
#' @export
wwheat = function(main_table, mat, map, N_or_S) {
  # first need to identify if Wheat_A > 0
  # need to test if crop_calendar indicates WWheat and SWheat grown...
  # test whether midwinter falls inside the interval between planting and harvesting
  plant_day = main_table$plant_date #update to correct name (WWheat or SWheat)
  harv_day =  main_table$harv_date #update to correct name (WWheat or SWheat)
  if (harv_day < plant_day) harv_day = harv_day + 365
  midwinter = yday(ymd(c(150621,151221)))[(N_or_S == 'N') + 1L]
  has_winter = midwinter %between% c(plant_day, harv_day)
  # winter wheat is determined by having a cold month, a long growing season, and midwinter falling inside the growing period
  wwheat = (temp_coldest_month %between% c(-10, 7) & (calendar$tot.days > 150) & (gsd >= 335) & has_winter)
}
