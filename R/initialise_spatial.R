#' @import data.table
#' @importFrom lubridate ymd
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
#' @export
create_main_table = function() {
  # Final version of this function will read values directly from gis files
  # For development we just create a dummy table with fake values

  # crop_area      = rast(paste(pkg.env$gis_path, 'spam',      'area.tif',     sep='/'))
  # crop_calendar  = rast(paste(pkg.env$gis_path, 'sacks',     'calendar.tif', sep='/'))
  # tillage        = rast(paste(pkg.env$gis_path, 'tillage',   'tillage.tif',  sep='/'))
  # min_N          = rast(paste(pkg.env$gis_path, 'fert',      'fert.tif',     sep='/'))
  # org_N          = rast(paste(pkg.env$gis_path, 'fert',      'manure.tif',   sep='/'))
  # org_CN         = rast(paste(pkg.env$gis_path, 'fert',      'manureCN.tif', sep='/'))
  # residue_return = rast(paste(pkg.env$gis_path, 'residue',   'residue.tif',  sep='/'))
  # BD             = rast(paste(pkg.env$gis_path, 'soilgrids', 'BD.tif',       sep='/'))
  # clay           = rast(paste(pkg.env$gis_path, 'soilgrids', 'clay.tif',     sep='/'))
  # pH             = rast(paste(pkg.env$gis_path, 'soilgrids', 'pH.tif',       sep='/'))
  # sand           = rast(paste(pkg.env$gis_path, 'soilgrids', 'sand.tif',     sep='/'))
  # SOC            = rast(paste(pkg.env$gis_path, 'soilgrids', 'SOC.tif',      sep='/'))

  # following makes a dummy example dataset for development:
  set.seed(123)
  n=20
  main_table = data.table(
    crop           = rep(pkg.env$crop_types, each=5),
    cell           = 1:n,    # in final version, cell should refer to specific cell number in the unrotated rasters...
                             # The table should only include cells where each crop is to be simulated
    crop_area      = runif(n),
    plant_date     = round(runif(n, yday(ymd(150320)), yday(ymd(150425)))),
    harv_date      = round(runif(n, yday(ymd(150820)), yday(ymd(150925)))),
    tillage        = 'K'
      # min_N          =
      # org_N          =
      # org_CN         =
      # residue_return =
      # BD             =
      # clay           =
      # pH             =
      # sand           =
      # SOC            =
  )
  return(main_table)
}
