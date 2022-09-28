# Provides:
#  - Download daily gridded cmip6 climate projections
#  - For data source, see https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6
#  - For each GCM X SSP X variable convert to geotiff with one layer per day (Gregorian) through to 2100
#  - Aggregates to 0.5 degree resolution
# Dominic Woolf 7/4/22

#' @import data.table
#' @import terra
#' @importFrom lubridate leap_year

initialize_weather = function() {
  index = data.table()
  index[, year := 2015:2100]
  index[, days.in.year := c(365,366)[leap_year(year) + 1L]]
  weather_data = index[, .(days.in.year, doy = seq_len(days.in.year)), keyby=year]
  weather_data[, doy_360 := doy]
  weather_data[, doy_365 := doy]
  weather_data[doy_360 > 360, doy_360 := doy_360 - .N, by=year]
  weather_data[doy_365 > 365, doy_365 := 365]
  # calculate vectors to convert calendar to raster layer index
  shift_days = weather_data[, last(doy_360), by=year][, .(year, shift_360 = data.table::shift(cumsum(V1), fill=0))]
  weather_data = weather_data[shift_days]
  weather_data[, idx_360 := doy_360 + shift_360][, shift_360 := NULL]
  shift_days = weather_data[, last(doy_365), by=year][, .(year, shift_365 = data.table::shift(cumsum(V1), fill=0))]
  weather_data = weather_data[shift_days]
  weather_data[, idx_365 := doy_365 + shift_365][, shift_365 := NULL]
  weather_data[, idx_gregorian := .I]
  assign('weather_data', weather_data, pkg.env)
  return(invisible(weather_data))
}

load_climate_vbl = function(vbl, path) {
  clim_files = list.files(path = path, pattern = paste(vbl, '.+tif$'), full.names = TRUE)
  clim_files = sort(clim_files)
  return(rast(clim_files))
}

load_climate = function(ssp, gcm) {
  climate_path = paste(pkg.env$gis_path, ssp, gcm)
  sapply(pkg.env$climate_vbls, load_climate_vbl, climate_path, USE.NAMES = TRUE)
}

extract_weather = function(climate, gcm, weather) {
  # dummy code for now
   return(NULL)
}
