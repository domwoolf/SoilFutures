# Dominic Woolf 17/9/22

#' Create a data table with structure for wth file
#' @import data.table
#' @param start_year Character, specifying the start year of the simulation.
#' @param end_year Character, specifying the end year of the simulation.
#' @export
initialize_weather = function(start_year, end_year) {
  weather_data = data.table(.date = seq.Date(as.Date(paste0(start_year,"-01-01")), as.Date(paste0(end_year,"-12-31")), 1))
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
  weather_data[, idx_gregorian  := .I]
  historical_days = seq_len(as.Date(paste0(pkg.env$end_year[2], "-12-31")) - as.Date(paste0(pkg.env$start_year[2], "-01-01")))
  weather_data[, idx_historical := rep_len(historical_days, .N)]
  assign('weather_data', weather_data, pkg.env)  ## !!! Why this??? we return the dt from this function - why not just use that?
  return(weather_data)
}

#' @importFrom terra rast
#' @export
load_climate_vbl = function(vbl, path, start_year, end_year) {
  clim_files   = list.files(path = path, pattern = paste0(vbl, '.+tif$'), full.names = TRUE)
  clim_files   = sort(clim_files)
  clim_files_s = grep(start_year, clim_files)
  clim_files_e = grep(end_year, clim_files)
  clim_files   = clim_files[clim_files_s:clim_files_e]
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


#' Split vector into equal sized chunks
#' @export
make_blocks = function(x, n = 5) {
  x[(floor((seq_along(x) - 1) / n)) *n + 1L]
}


#' Create a weather file
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
#' @param tmp.dir directory to create in tmp.path, set with arg[1]
#' @param out.dir directory to create in out.path, set with arg[2]
#' @importFrom terra rast extract
#' @export
make_weather_file = function(climate, .gridid, cell, .gcm, ssp, weather = pkg.env$weather_data, cmip6_calendars = cmip6_calendars, tmp.dir, out.dir) {
  calendars    = c('proleptic_gregorian', '365_day', 'standard',      '360_day', 'historical')
  calendar_idx = c('idx_gregorian',       'idx_365', 'idx_gregorian', 'idx_360', 'idx_historical')
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
  w[tasmin > tasmax, c('tasmin', 'tasmax') := (tasmin + tasmax)/2] # eliminate rare anomalies in the data where min > max
  weather_fname = paste0(pkg.env$tmp_path, '/', tmp.dir,'/weather_', ssp, '_', .gcm, '_', .gridid, '.wth')
  fwrite(w, weather_fname, sep = ' ', col.names = FALSE)

  # summary statistics
  prob = seq(0.1, 0.9, 0.1) # quantile probabilities for summary stats
  w[, sum_pr := sum(pr), by = y]
  w_mean.sd  = w[, .( # mean, sd for every year
    mtasmax  = mean(tasmax),
    sdtasmax = sd(tasmax),
    mtasmin  = mean(tasmin),
    sdtasmin = sd(tasmin),
    mpr      = mean(sum_pr),
    sdpr     = sd(sum_pr)),
    by = .(y = make_blocks(y))]
  w_summary = w[, .( # quantile for every year
                    prob     = prob,
                    qtasmax  = quantile(tasmax, prob),
                    qtasmin  = quantile(tasmin, prob),
                    qpr      = quantile(sum_pr, prob)
                    ),
                  by = .(y = make_blocks(y))]
  w_sum = dcast(w_summary, y ~ prob, value.var = colnames(w_summary)[-3])
  w_sum = w_sum[w_mean.sd, on = .(y = y)]
  w_sum[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum, c('gridid', 'y', 'gcm', 'ssp'))
  w_sum_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_summary_statistics.csv')
  fwrite(w_sum, w_sum_fname, append = TRUE)
  return(weather_fname)
}
