# Dominic Woolf 17/9/22

#' @import data.table
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

#' @importFrom terra rast
load_climate_vbl = function(vbl, path) {
  clim_files = list.files(path = path, pattern = paste0(vbl, '.+tif$'), full.names = TRUE)
  clim_files = sort(clim_files)
  return(rast(clim_files))
}

#' Loads climate data
#'
#' For specified gcm and ssp, creates a list of global daily rasters.  Each element of the list corresponds to a specific climate variable.
#' These are mean daily temperature (tas), minimum daily temperature (tasmin), maximum daily temperature (tasmax),
#' and precipitation (pr).  Each layer of the rasters corresponds to a day from 2015 to 2100 (using the gcms calendar)
#'
#' @param ssp Chareacter secifying the ssp.
#' @param gcm Character, specifying the climate model (gcm).
#' @returns list of rasters (see details)
#' @export
load_climate = function(ssp, gcm) {
  climate_path = paste(pkg.env$gis_path, 'climate', 'cmip6_0.5deg', ssp, gcm, sep='/')
  sapply(pkg.env$climate_vbls, load_climate_vbl, climate_path, USE.NAMES = TRUE)
}

#' #' Create a weather file
#'
#' Used for side effect of writing a weather file based on location.
#' Location is specified as a raster cell number.
#'
#' This function creates a weather file for a specified climate model, ssp, and location.
#' Raw climate model data is converted into daily values in the Gregorian calendar, which sometimes
#' requires date conversion from the native gcm calendar.  Daily vectors are constructed from 2015 through
#' to 2100 for each of, minimum daily temperature (tasmin), maximum daily temperature (tasmax),
#' and precipitation (pr).  These vectors are written as columns in the weather file which is written to
#' a uniquely named file.  File naming is unique to allow parallel execution of DayCent, with each instance
#' using a different weather file.
#'
#' @param climate list of spatial rast objects.
#' One element of list for each of the climate variables (tasmin, tasmax, pr).
#' Usually created using load_climate()
#' @param cell integer, specifying the raster cell number from which to extract values.
#' Note that climate rasters are in rotated (0-360 degree longitude) coordinates.
#' @param .gcm character (length one) giving name of the cmip6 model the climate is from
#' @param weather data.table containing gregorian calendar and index numbers to extract gregorian values
#' from a daily climate rast that uses a different calendar. Usually created using initialize_weather()
#' @importFrom terra rast extract
#' @export
extract_weather = function(climate, cell, .gcm, ssp, weather) {
  calendars    = c('proleptic_gregorian', '365_day', 'standard',      '360_day')
  calendar_idx = c('idx_gregorian',       'idx_365', 'idx_gregorian', 'idx_360')
  calendar = cmip6_calendars[gcm == .gcm, calendar]
  .SDcols = c('year', 'doy')
  .SDcols = c(.SDcols, calendar_idx[match(calendar, calendars)])
  weather = copy(weather)[, .SD, .SDcols = .SDcols]
  setnames(weather, 3, 'idx')
  daily_pr     = unname(unlist(extract(climate$pr,     cell)))
  daily_tasmin = unname(unlist(extract(climate$tasmin, cell)))
  daily_tasmax = unname(unlist(extract(climate$tasmax, cell)))
  weather[, pr     := daily_pr[idx]]
  weather[, tasmin := daily_tasmin[idx]/10]
  weather[, tasmax := daily_tasmax[idx]/10]
  weather_fname = paste0(pkg.env$out_path, '/weather_', ssp, '_', .gcm, '_', cell, '.wth')
  fwrite(weather, weather_fname, sep = ' ')
  return(weather_fname)
}
