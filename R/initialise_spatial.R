#' @import data.table
#' @import lubridate
#' @import terra
#' @export
wwheat = function(main_table, mat, map, N_or_S) {
  # test whether midwinter falls inside the interval between planting and harvesting
  plant_day = main_table$plant_date
  harv_day =  main_table$harv_date
  if (harv_day < plant_day) harv_day = harv_day + 365
  midwinter = yday(ymd(c(150621,151221)))[(N_or_S == 'N') + 1L]
  has_winter = midwinter %between% c(plant_day, harv_day)
  # winter wheat is determined by having a cold month, a long growing season, and midwinter falling inside the growing period
  wwheat = (temp_coldest_month %between% c(-10, 7) & (calendar$tot.days > 150) & (gsd >= 335) & has_winter)
}


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
# following is linked to local paths
# final function will link to pkg.env$gis_path
  base_path      = '/home/shelby/Documents/gis/final_harmonized_rasters'

  crop_area      = rast(paste(base_path, 'spam_crop_area_agg_.tif',           sep='/'))
  crop_calendar  = rast(paste(base_path, 'crop_calendar.tif',                 sep='/'))
  tillage        = rast(paste(base_path, 'tillage_by_crop_agg.tif',           sep='/'))
  min_N          = rast(paste(base_path, 'fertilizer-n_by_crop.tif',          sep='/'))
  org_N          = rast(paste(base_path, 'manure-n_cropland.tif',             sep='/'))
  org_CN         = rast(paste(base_path, 'global_manure_weighted_CN_agg.tif', sep='/'))
  residue_return = rast(paste(base_path, 'wirsenius_global_res_rtrn_agg.tif', sep='/'))
  bdod           = rast(paste(base_path, 'bdod_0-100cm_mean_agg.tif',         sep='/'))
  clay           = rast(paste(base_path, 'clay_0-100cm_mean_agg.tif',         sep='/'))
  phh2o          = rast(paste(base_path, 'phh2o_0-100cm_mean_agg.tif',        sep='/'))
  sand           = rast(paste(base_path, 'sand_0-100cm_mean_agg.tif',         sep='/'))
  soc            = rast(paste(base_path, 'soc_0-100cm_mean_agg.tif',          sep='/'))

  all_raster     = c(crop_area, crop_calendar, tillage, min_N, org_N, org_CN, residue_return,
                     bdod, clay, phh2o, sand, soc)

  main_table     = as.data.frame(all_raster, xy = TRUE, cells = TRUE)
  main_table     = setDT(main_table)

  # keep these rotated cell numbers, could add row with main cell # and the unrotated cell #
  # Convert table from wide to long (want to search rows not rows and then columns)
  # if value 0 for crop_area delete it (because this is what daycent will use for sims!)
  # round crop calendar days (but after table built)
  # keep these rotated cell numbers, could add row with main cell # and the unrotated cell #

  # following makes a dummy example dataset for development:
  # set.seed(123)
  # n=20
  # main_table = data.table(
  #   crop           = rep(pkg.env$crop_types, each=5),
  #   cell           = 1:n,    # in final version, cell should refer to specific cell number in the unrotated rasters...
  #                            # The table should only include cells where each crop is to be simulated
  #   crop_area      = runif(n),
  #   plant_date     = round(runif(n, yday(ymd(150320)), yday(ymd(150425)))),
  #   harv_date      = round(runif(n, yday(ymd(150820)), yday(ymd(150925)))),
  #   tillage        = 'K'
  #     # min_N          =
  #     # org_N          =
  #     # org_CN         =
  #     # residue_return =
  #     # BD             =
  #     # clay           =
  #     # pH             =
  #     # sand           =
  #     # SOC            =
  # )
  return(main_table)
  fwrite(main_table, file = "cell_data.csv") # need to save into function?
  # save as RDA format using "save()"
}
