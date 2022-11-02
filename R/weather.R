# Dominic Woolf 17/9/22

#' @import data.table
# #' @importFrom lubridate leap_year
#' @param start_year Character, specifying the start year of the simulation.
#' @param end_year Character, specifying the end year of the simulation.
#' @export
initialize_weather = function(start_year, end_year) {
  weather_data = data.table(.date = seq.Date(ymd(paste0(start_year,"-01-01")),ymd(paste0(end_year,"-12-31")), 1))
  weather_data[, dom := mday(.date)] # day of month
  weather_data[, m   := month(.date)]
  weather_data[, y   := year(.date)]
  weather_data[, doy := yday(.date)] # day of year
  setkey(weather_data, y)

  # calculate vectors to convert calendar to raster layer index
  weather_data[, doy_360 := doy]
  weather_data[, doy_365 := doy]
  weather_data[doy_360 > 360, doy_360 := doy_360 - .N, by=y]
  weather_data[doy_365 > 365, doy_365 := 365]
  shift_days = weather_data[, last(doy_360), by=y][, .(y, shift_360 = data.table::shift(cumsum(V1), fill=0))]
  weather_data = weather_data[shift_days]
  weather_data[, idx_360 := doy_360 + shift_360][, shift_360 := NULL]
  shift_days = weather_data[, last(doy_365), by=y][, .(y, shift_365 = data.table::shift(cumsum(V1), fill=0))]
  weather_data = weather_data[shift_days]
  weather_data[, idx_365 := doy_365 + shift_365][, shift_365 := NULL]
  weather_data[, idx_gregorian := .I]
  assign('weather_data', weather_data, pkg.env)
  return(invisible(weather_data))
}

#' @importFrom terra rast
load_climate_vbl = function(vbl, path, start_year, end_year) {
  clim_files = list.files(path = path, pattern = paste0(vbl, '.+tif$'), full.names = TRUE)
  clim_files = sort(clim_files)
  clim_files_s = grep(start_year, clim_files)
  clim_files_e = grep(end_year, clim_files)
  clim_files = clim_files[clim_files_s:clim_files_e]
  return(rast(clim_files))
}

#' Loads climate data
#'
#' For specified gcm and ssp, creates a list of global daily rasters.  Each element of the list corresponds to a specific climate variable.
#' These are mean daily temperature (tas), minimum daily temperature (tasmin), maximum daily temperature (tasmax),
#' and precipitation (pr).  Each layer of the rasters corresponds to a day from a specified start year to end year (using the gcms calendar)
#'
#' @param ssp Chareacter secifying the ssp.
#' @param gcm Character, specifying the climate model (gcm).
#' @returns list of rasters (see details)
#' @export
load_climate = function(ssp, gcm, start_year, end_year) {
  climate_path = paste(pkg.env$gis_path, 'climate', 'cmip6_0.5deg', ssp, gcm, sep='/')
  sapply(pkg.env$climate_vbls, load_climate_vbl, climate_path, start_year, end_year, USE.NAMES = TRUE)
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
#' @import lubridate
#' @importFrom terra rast extract
#' @export
make_weather_file = function(climate, .gridid, cell, .gcm, ssp, weather, cmip6_calendars = copy(cmip6_calendars)) {
  calendars    = c('proleptic_gregorian', '365_day', 'standard',      '360_day')
  calendar_idx = c('idx_gregorian',       'idx_365', 'idx_gregorian', 'idx_360')
  calendar = cmip6_calendars[gcm == .gcm, calendar]
  # Daycent column order: day of month, month, calendar year, day of year, maximum air temperature (°C), minimum air temperature (°C), and precipitation (cm).
  .SDcols = c('dom', 'm', 'y', 'doy')
  idx_col = calendar_idx[match(calendar, calendars)]
  .SDcols = c(.SDcols, idx_col)
  w = copy(weather)[, .SD, .SDcols = .SDcols]
  setnames(w, idx_col, 'idx')
  daily_pr     = unname(unlist(extract(climate$pr,     cell)))
  daily_tasmin = unname(unlist(extract(climate$tasmin, cell)))
  daily_tasmax = unname(unlist(extract(climate$tasmax, cell)))
  w[, tasmax := daily_tasmax[idx]/10]
  w[, tasmin := daily_tasmin[idx]/10]
  w[, pr     := daily_pr[idx]]
  # apply historical or cmip6 wth file construction
  if(ssp == 'historical') {
    w_block = w[y != 1996,]
    w_2100  = w[y == 1997,]
    w       = rbind(w, w_block, w_block, w_block, w_block, w_block, w_block[y != 2008,], w_2100)
    w_y = data.table(.date = seq.Date(ymd("2016-01-01"),ymd("2100-12-31"), 1))
    w_y[, y   := year(.date)]
    w[, y:= w_y$y]
    w[, idx := NULL]
    w[tasmin > tasmax, c('tasmin', 'tasmax') := (tasmin + tasmax)/2]
    weather_fname = paste0(pkg.env$tmp_path, '/weather_', ssp, '_', .gcm, '_', cell, '.wth')
    fwrite(w, weather_fname, sep = ' ', col.names = FALSE)

    # summary statistics
    blocks = c('2016-2020', '2021-2025','2026-2030', '2031-2035',
               '2036-2040', '2041-2045', '2046-2050', '2051-2055',
               '2056-2060', '2061-2065', '2066-2070', '2071-2075',
               '2076-2080', '2081-2085', '2086-2090', '2091-2095',
               '2096-2100')
    w_calc = copy(w)[, sum_pr := sum(pr), by = y]
    # w_calc[, .SD[y %in% 2016:2020], by = y]
    # w_calc[y %in% 2016:2020, `:=` (tasmax_mean = mean(tasmax), tasmax_sd = sd(tasmax), tasmax_q10 = quantile(tasmax, probs = (0.1)),
                                   # tasmax_q25 = quantile(tasmax, probs = (0.25)), tasmax_q50 = quantile(tasmax, probs = (0.5)),
                                   # tasmax_q75 = quantile(tasmax, probs = (0.75)), tasmax_q90 = quantile(tasmax, probs = (0.9)),
                                   # tasmin_mean = mean(tasmin),tasmin_sd = sd(tasmin), tasmin_q10 = quantile(tasmin, probs = (0.1)),
                                   # tasmin_q25 = quantile(tasmin, probs = (0.25)), tasmin_q50 = quantile(tasmin, probs = (0.5)),
                                   # tasmin_q75 = quantile(tasmin, probs = (0.75)), tasmin_q90 = quantile(tasmin, probs = (0.9)),
                                   # MAP = mean(sum_pr), MAP_sd = sd(sum_pr), MAP_q10 = quantile(sum_pr, probs = (0.1)),
                                   # MAP_q25 = quantile(sum_pr, probs = (0.25)), MAP_q50 = quantile(sum_pr, probs = (0.5)), MAP_q75 = quantile(sum_pr, probs = (0.75)),
                                   # MAP_q90 = quantile(sum_pr, probs = (0.9)))]

  } else {
    # add tas and precip calcs here
    w[, idx := NULL]
    w[tasmin > tasmax, c('tasmin', 'tasmax') := (tasmin + tasmax)/2] # eliminate rare anomalies in the data where min > max
    weather_fname = paste0(pkg.env$tmp_path, '/weather_', ssp, '_', .gcm, '_', cell, '.wth')
    fwrite(w, weather_fname, sep = ' ', col.names = FALSE)
    return(weather_fname)
  }
}
