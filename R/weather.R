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
  clim_files   = list.files(path = paste(path, vbl, sep = '/'), pattern = paste0(vbl, '.+tif$'), full.names = TRUE)
  clim_files   = sort(clim_files)
  clim_files_s = grep(start_year, clim_files)
  clim_files_e = grep(end_year, clim_files)
  clim_files   = clim_files[clim_files_s:clim_files_e]
  return(rast(clim_files))
}

#' Loads climate data for 0.5 deg
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

#' Loads climate data for 0.25 deg
#'
#' For specified gcm and ssp, creates a list of global daily rasters.  Each element of the list corresponds to a specific climate variable.
#' These are mean daily temperature (tas), minimum daily temperature (tasmin), maximum daily temperature (tasmax),
#' and precipitation (pr).  Each layer of the rasters corresponds to a day from a specified start year to end year (using the gcms calendar)
#'
#' @param ssp Chareacter secifying the ssp.
#' @param gcm Character, specifying the climate model (gcm).
#' @returns list of rasters (see details)
#' @export
load_climate_25deg = function(ssp, gcm, start_year, end_year) {
  climate_path = paste(pkg.env$gis_path, 'climate', 'cmip6_0.25deg', ssp, gcm, sep='/')
  sapply(pkg.env$climate_vbls, load_climate_vbl, climate_path, start_year, end_year, USE.NAMES = TRUE)
}

#' Split vector into equal sized chunks
make_blocks = function(x, n = 5) {
  x[(floor((seq_along(x) - 1) / n)) *n + 1L]
}

make_blocks = function(x, n = 5, offset=1, method = 'year') {
  # old definition commented out below:
  #    return( x[(floor((seq_along(x) - 1) / n)) *n + 1L] )
  if (method == 'decade') {
    # rounds years to the decade in which they occur
    return(floor(((x-offset)) / n)) * n
  } else {
    # rounds years to nearest decade, with years ending in 5 consistently rounded down
    return(((x + 4) %/% 10) * 10)
  }
}


#' Rounding to n year increments
#' @param x, number
#' @param n, integer, number of years in increment, default is 5
make_blocks2 = function(x, n = 5) {
  (floor(((x-1)) / n)) *n + 1L
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
#' @param .gridid integer, cell number (-180 to 180 longitude) from cell_data_table.
#' @param cell integer, specifying the raster cell number from which to extract values.
#' Note that climate rasters are in rotated (0-360 degree longitude) coordinates.
#' @param .gcm character (length one) giving name of the cmip6 model.
#' @param .ssp character (length one) giving name of ssp scenario.
#' @param weather data.table containing gregorian calendar and index numbers to extract gregorian values
#' from a daily climate rast that uses a different calendar. Usually created using initialize_weather()
#' @param tmp.dir directory to create in tmp.path, set with arg[1]
#' @param out.dir directory to create in out.path, set with arg[2]
#' @importFrom terra rast extract
#' @export
make_weather_file = function(climate, .gridid, cell, .gcm, .ssp, weather = pkg.env$weather_data, cmip6_calendars = cmip6_calendars, tmp.dir, out.dir) {
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
  w[, tasmax := round(daily_tasmax[idx]/10, 1)]
  w[, tasmin := round(daily_tasmin[idx]/10, 1)]
  w[, pr     := round(daily_pr[idx]/10, 1)] # updated to convert to cm from mm
  w[tasmin > tasmax, c('tasmin', 'tasmax') := (tasmin + tasmax)/2] # eliminate rare anomalies in the data where min > max
  w[, idx    := NULL]
  weather_fname = paste0('weather_', .ssp, '_', .gcm, '_', .gridid, '.wth')
  fwrite(w, paste0(tmp.dir, '/', weather_fname), sep = ' ', col.names = FALSE) # removed pkg.env$tmp.path
  return(weather_fname)
}
#' Create a weather file from 0.25 degree data
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
#' @param .gridid integer, cell number (-180 to 180 longitude) from cell_data_table.
#' @param .xy dataframe, specifying the rotated x coordinate and unrotated y coordinate from which to extract values.
#' Note that climate rasters are in rotated (0-360 degree longitude) coordinates.
#' @param .gcm character (length one) giving name of the cmip6 model.
#' @param .ssp character (length one) giving name of ssp scenario.
#' @param weather data.table containing gregorian calendar and index numbers to extract gregorian values
#' from a daily climate rast that uses a different calendar. Usually created using initialize_weather()
#' @param tmp.dir directory to create in tmp.path, set with arg[1]
#' @param out.dir directory to create in out.path, set with arg[2]
#' @importFrom terra rast extract
#' @export
make_weather_file_25deg = function(climate, .gridid, .xy, .gcm, .ssp, weather = pkg.env$weather_data, cmip6_calendars = cmip6_calendars, tmp.dir, out.dir) {
  calendars    = c('proleptic_gregorian', '365_day', 'standard',      '360_day', 'historical')
  calendar_idx = c('idx_gregorian',       'idx_365', 'idx_gregorian', 'idx_360', 'idx_historical')
  calendar = cmip6_calendars[gcm == .gcm, calendar]
  # Daycent column order: day of month, month, calendar year, day of year, maximum air temperature (°C), minimum air temperature (°C), and precipitation (cm).
  .SDcols = c('dom', 'm', 'y', 'doy')
  idx_col = calendar_idx[match(calendar, calendars)]
  .SDcols = c(.SDcols, idx_col)
  w = copy(weather)[, .SD, .SDcols = .SDcols]
  setnames(w, idx_col, 'idx')
  daily_pr     = unname(unlist(extract(climate$pr,     .xy)))
  daily_tasmin = unname(unlist(extract(climate$tasmin, .xy)))
  daily_tasmax = unname(unlist(extract(climate$tasmax, .xy)))
  w[, tasmax := round(daily_tasmax[idx]/10, 1)]
  w[, tasmin := round(daily_tasmin[idx]/10, 1)]
  w[, pr     := round(daily_pr[idx]/10, 1)] # updated to convert to cm from mm
  w[tasmin > tasmax, c('tasmin', 'tasmax') := (tasmin + tasmax)/2] # eliminate rare anomalies in the data where min > max
  w[, idx    := NULL]
  weather_fname = paste0('weather_', .ssp, '_', .gcm, '_', .gridid, '.wth')
  fwrite(w, paste0(tmp.dir, '/', weather_fname), sep = ' ', col.names = FALSE) # removed pkg.env$tmp.path
  return(weather_fname)
}

#' Fix weather file precipitation
#'
#' Used to convert precipitation in weather files (.wth) from mm/day to cm/day.
#'
#' @import data.table
#' @param .wthfile character, name of weather file in working directory
#' @export
weather_pr_mm_to_cm = function(.wthfile) {
  w_name   = .wthfile
  mm_to_cm = 10L
  w_file   = fread(.wthfile)
  if (NCOL(w_file) == 7) {
    w_file[, V7 := V7/mm_to_cm]
    fwrite(w_file, w_name, sep = ' ', col.names = FALSE)
    }
}


#' Calculate bio-climatic variables
#'
#' Calculates bioclimatic variables from daily values for Tmin, Tmax, and precipitation
#'
#' @import data.table
#' @importFrom moments kurtosis, skewness
#' @param prec vector of daily precipitation
#' @param tmin vector of daily minimum temperature
#' @param tmax vector of daily maximum temperature
#' @returns A named list (bio1... bio19), length  19 of the following bioclimatic variables
#'  - bio1 : Annual Mean Temperature
#'  - bio2 : Mean Diurnal Range
#'  - bio4 : Temperature Seasonality
#'  - bio5 : Max Temperature of Warmest Period
#'  - bio6 : Min Temperature of Coldest Period
#'  - bio7 : Temperature Annual Range (P5-P6)
#'  - bio3 : Isothermality (P2 / P7)
#'  - bio8 : Mean Temperature of Wettest Quarter
#'  - bio9 : Mean Temperature of Driest Quarter
#'  - bio10: Mean Temperature of Warmest Quarter
#'  - bio11: Mean Temperature of Coldest Quarter
#'  - bio12: Annual Precipitation
#'  - bio13: Precipitation of Wettest Period
#'  - bio14: Precipitation of Driest Period
#'  - bio15: Precipitation Seasonality
#'  - bio16: Precipitation of Wettest Quarter
#'  - bio17: Precipitation of Driest Quarter
#'  - bio18: Precipitation of Warmest Quarter
#'  - bio19: Precipitation of Coldest Quarter
#' @export
biov = function(prec, tmin, tmax) {
  quarterly = function(x, fun=sum)  {
    c(fun(x[1:3]), fun(x[4:6]), fun(x[7:9]), fun(x[10:12]))
  }

  tavg = (tmin + tmax) / 2
  wet = quarterly(prec)              # precip by quarter (3 months)
  wetqrt = which.max(wet)
  dryqrt = which.min(wet)
  tmp = quarterly(tavg) / 3          # average temp by quarter
  hot = which.max(tmp)
  cold = which.min(tmp)

  # Note: in following, we define seasonality as range (Dismo package used sd*100), according to
  # https://www.sciencedirect.com/science/article/abs/pii/S0012825221003445
  # https://www.nature.com/articles/s41597-020-00726-5

  p = as.list(rep(NA, 19))
  names(p) = paste0('bio', 1:19)
  p[[1]]  = mean(tavg)            # P1  Annual Mean Temperature
  p[[2]]  = mean(tmax-tmin)       # P2  Mean Diurnal Range
  p[[4]]  = diff(range(tavg))     # P4  Temperature Seasonality
  p[[5]]  = max(tmax)             # P5  Max Temperature of Warmest Period
  p[[6]]  = min(tmin)             # P6  Min Temperature of Coldest Period
  p[[7]]  = p[[5]] - p[[6]]       # P7  Temperature Annual Range (P5-P6)
  p[[3]]  = p[[2]] / p[[7]]       # P3  Isothermality (P2 / P7)
  p[[8]]  = tmp[wetqrt]           # P8  Mean Temperature of Wettest Quarter
  p[[9]]  = tmp[dryqrt]           # P9  Mean Temperature of Driest Quarter
  p[[10]] = max(tmp)              # P10 Mean Temperature of Warmest Quarter
  p[[11]] = min(tmp)              # P11 Mean Temperature of Coldest Quarter
  p[[12]] = sum(prec)             # P12 Annual Precipitation
  p[[13]] = max(prec)             # P13 Precipitation of Wettest Period
  p[[14]] = min(prec)             # P14 Precipitation of Driest Period
  p[[15]] = diff(range(prec))     # P15 Precipitation Seasonality
  p[[16]] = max(wet)              # P16 Precipitation of Wettest Quarter
  p[[17]] = min(wet)              # P17 Precipitation of Driest Quarter
  p[[18]] = wet[hot]              # P18. Precipitation of Warmest Quarter
  p[[19]] = wet[cold]             # P19. Precipitation of Coldest Quarter
  return(p)
}


#' Calculate weather statistics
#'
#' Used to calculate summary statistics of weather file data, over successive periods of time (default is decadal).
#'
#' @import data.table
#' @importFrom moments kurtosis, skewness
#' @param w data.table of weather file
#' @param plant_day
#' @param harvest_day
#' @param NY Number of years in block (default = 10)
#' @param prob probability bins for quantile decomposition (default = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
#' @param bins.tmax bin boundaries for histogram decomposition of tmax. Default = c(-Inf, seq(0, 35, by = 5), Inf)
#' @param bins.tmin bin boundaries for histogram decomposition of tmin. Default = c(-Inf, seq(0, 35, by = 5), Inf).
#' Default is based on https://journals-ametsoc-org.proxy.library.cornell.edu/view/journals/clim/35/17/JCLI-D-21-0617.1.xml
#' by which we use power law breaks to characterize precip, with <=4mm/day as cutoff
#' using round(10^seq.int(log10(2), log10(150), length.out = 8))
#' @param bins.pr bin boundaries for histogram decomposition of precipitation. Default = c(0, 2, 4, 7, 13, 24, 44, 81, 150, Inf)
#' @export
calc_weather_stats = function(w, ssp, gcm, cell,
                              plant_day, harvest_day,
                              NY = 10,
                              prob = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                              bins.tmax  = c(-Inf, seq(0, 35, by = 5), Inf),
                              bins.tmin  = c(-Inf, seq(0, 35, by = 5), Inf),
                              bins.pr    = c(0, 2, 4, 7, 13, 24, 44, 81, 150, Inf)) {

  w[, y_block := make_blocks(y, n = NY)]
  w[, pr := pr * 10] # convert precip from cm/day to mm/day

  # --------------------------------------------
  # mean, sd for every block
  # --------------------------------------------
  w_stats  = w[, .(
    ssp       = ssp,
    gcm       = gcm,
    cell      = cell,
    m.tasmin  = mean(tasmin),
    sd.tasmin = sd(tasmin),
    m.tasmax  = mean(tasmax),
    sd.tasmax = sd(tasmax),
    m.pr      = mean(pr),
    sd.pr     = sd(pr),
    sk.pr     = skewness(pr),
    k.pr      = kurtosis(pr)
    ),
    by = .(y_block)]

  # --------------------------------------------
  # growing season stats
  # --------------------------------------------
  growing_season = w[, between(doy, plant_day, harvest_day)]
  if (plant_day > harvest_day) growing_season = !growing_season
  w.gs  = w[growing_season, .(
    gs.m.tasmin  = mean(tasmin),
    gs.sd.tasmin = sd(tasmin),
    gs.m.tasmax  = mean(tasmax),
    gs.sd.tasmax = sd(tasmax),
    gs.m.pr      = mean(pr),
    gs.sd.pr     = sd(pr),
    gs.sk.pr     = skewness(pr),
    gs.k.pr      = kurtosis(pr)
    ),
    by = .(y_block)]
  w_stats = cbind(w_stats, w.gs[,-1])

  # --------------------------------------------
  # quantiles by year block
  # --------------------------------------------
  wq = w[, .(
    prob     = prob,
    q.tasmin  = quantile(tasmin, prob),
    q.tasmax  = quantile(tasmax, prob),
    q.pr      = quantile(pr, prob)),
    by = y_block]
  wq = dcast(wq, y_block ~ prob, value.var =c("q.tasmin", "q.tasmax", "q.pr"))
  w_stats = cbind(w_stats, wq[,-1])

  # --------------------------------------------
  # histograms
  # --------------------------------------------
  hist.tasmin = w[, cut(tasmin, bins.tmin), by = y_block]
  hist.tasmin = hist.tasmin[, as.list(table(V1)), by = y_block][, -1]
  setnames(hist.tasmin, new = \(nn) paste0('tasmin.', nn))

  hist.tasmax = w[, cut(tasmax, bins.tmax), by = y_block]
  hist.tasmax = hist.tasmax[, as.list(table(V1)), by = y_block][, -1]
  setnames(hist.tasmax, new = \(nn) paste0('tasmax.', nn))

  hist.pr = w[, cut(pr, bins.pr, include.lowest = TRUE), by = y_block]
  hist.pr = hist.pr[, as.list(table(V1)), by = y_block][, -1]
  setnames(hist.pr, new = \(nn) paste0('pr.', nn))

  w_stats = cbind(w_stats, hist.tasmin, hist.tasmax, hist.pr)

  # --------------------------------------------
  # consecutive dry/rainy days
  # --------------------------------------------
  rle.pr = w[, cut(pr, c(0, 4, Inf), include.lowest = TRUE), by = y_block]
  rle.pr  = rle.pr[, rle(as.integer(V1)), by = y_block]
  rle.pr[, values := c('rle.dry', 'rle.rain')[values]]
  rle.pr = rle.pr[, .(pr.days = mean(lengths)), by = .(y_block, values)]
  rle.pr = dcast(rle.pr, y_block ~ values, value.var = 'pr.days')
  #if no rainy/dry days, then add column of zeroes
  missing.cols = setdiff(c('rle.dry', 'rle.rain'), names(rle.pr))
  if (length(missing.cols)) rle.pr[, (missing.cols) := 0]
  w_stats = cbind(w_stats, rle.pr[, -1])

  # same again, but only in growing season
  rle.pr = w[growing_season, cut(pr, c(0, 4, Inf), include.lowest = TRUE), by = y_block]
  rle.pr  = rle.pr[, rle(as.integer(V1)), by = y_block]
  rle.pr[, values := c('gs.rle.dry', 'gs.rle.rain')[values]]
  rle.pr = rle.pr[, .(pr.days = mean(lengths)), by = .(y_block, values)]
  rle.pr = dcast(rle.pr, y_block ~ values, value.var = 'pr.days')
  #if no rainy/dry days, then add column of zeroes
  missing.cols = setdiff(c('gs.rle.dry', 'gs.rle.rain'), names(rle.pr))
  if (length(missing.cols)) rle.pr[, (missing.cols) := 0]
  w_stats = cbind(w_stats, rle.pr[, -1])

  # --------------------------------------------
  # bioclimatic variables
  # --------------------------------------------
  w.monthly = w[, .(tasmin = mean(tasmin), tasmax=mean(tasmax), pr=sum(pr)/uniqueN(y)), by = .(y_block, month)]
  w.bio = w.monthly[, biov(pr, tasmin, tasmax), by=y_block][, -1]
  w_stats = cbind(w_stats, w.bio)

  # --------------------------------------------
  return(w_stats)
}


#' Create weather statistics file
#'
#' Used for side effect of writing (appending) to a weather statisics file.
#'
#'@import data.table
#'@param weather_fname data.table, containing 86 years (2016-2100) of tasmin, tasmax, and pr data for gridcell.
#'Must be read in to function as a file.path
#' @param .gridid integer, cell number (-180 to 180 longitude) from cell_data_table.
#' @param .gcm character (length one) giving name of the cmip6 model.
#' @param .ssp character (length one) giving name of ssp scenario.
#' @param .plant_date integer, planting date for crop from cell_data_table.
#' @param .harv_date integer, harvest data for crop from cell_data_table.
#' @param out.dir directory to create in out.path, set with arg[2]
#' @export
make_weather_stats = function(weather_fname, gridid, ssp, gcm, plant_day, harvest_day, out.dir) {
  w = fread(weather_fname)
  names(w)  = c('dom', 'month', 'y', 'doy', 'tasmax', 'tasmin', 'pr')
  w_stats = calc_weather_stats(w, ssp, gcm, gridid, plant_day, harvest_day)
  fname = paste0(pkg.env$out_path, out.dir, '/weather_stats.csv')
  fwrite(w_stats, fname, append = TRUE)
  return(invisible(NULL))
}
