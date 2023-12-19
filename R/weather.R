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

#' Create weather statistics file
#'
#' Used for side effect of writing a weather files based on location.
#' Location is specified as a raster cell number.
#'
#' This function creates a weather statistics files for a specified climate model, ssp, and location.
#' Raw climate model data was converted into daily values in the Gregorian calendar, which sometimes
#' requires date conversion from the native gcm calendar.  Daily vectors are constructed from 2015 through
#' to 2100 for each of, minimum daily temperature (tasmin), maximum daily temperature (tasmax),
#' and precipitation (pr).  These vectors are written as columns in the weather file which is written to
#' a uniquely named file.  File naming is unique to allow parallel execution of DayCent, with each instance
#' using a different weather file.
#'
#'There are 4 files created in this function:
#'1. Annual weather statistics using daily means and total annual pr, five-year intervals AND simulation period.
#'This file includes data on quantiles, means, and standard deviations for tasmin, tasmax, and pr.
#'Estimates were made over the entire simulation period and in five-year intervals.
#'
#'2. Histogram bins of variable frequencies, five-year intervals AND simulation period.
#'This file includes frequency data on tasmin, tasmax, and pr, including consecutive days of pr,
#'consecutive dry days, consecutive days exceeding 75% quantile and below 25% quantile for tasmin, tasmax.
#'
#'3. Monthly weather statistics using daily means, five-year intervals AND simulation period.
#'This file includes data on quantiles, means, and standard deviations for tasmin, tasmax, and pr.
#'Estimates were made monthly over the entire simulation period and in five-year intervals.
#'
#'4. Bioclimatic variables using monthly means, five-year intervals AND simulation period.
#'19 Bioclimatic variables were estimated using the biovars function in the dismo package.
#'
#'5. Growing season weather statistics using daily means, five-year intervals AND simulation period.
#'This file includes data on quantiles, means, and standard deviations for tasmin, tasmax, and pr.
#'Estimates were made over the growing season for the entire simulation period and in five-year intervals.
#'
#'6. Non-growing season weather statistics using daily means, five-year intervals AND simulation period.
#'This file includes data on quantiles, means, and standard deviations for tasmin, tasmax, and pr.
#'Estimates were made over the non-growin season for the entire simulation period and in five-year intervals.
#'
#'7. Histogram bins of growing and non-growing season variable frequencies, five-year intervals AND simulation period.
#'This file includes frequency data on tasmin, tasmax, and pr, including consecutive days of pr,
#'consecutive dry days, consecutive days exceeding 75% quantile and below 25% quantile for tasmin, tasmax.
#'
#'@import data.table
#'@import dismo
#'@param weather_fname data.table, containing 86 years (2016-2100) of tasmin, tasmax, and pr data for gridcell.
#'Must be read in to function as a file.path
#' @param .gridid integer, cell number (-180 to 180 longitude) from cell_data_table.
#' @param .gcm character (length one) giving name of the cmip6 model.
#' @param .ssp character (length one) giving name of ssp scenario.
#' @param .plant_date integer, planting date for crop from cell_data_table.
#' @param .harv_date integer, harvest data for crop from cell_data_table.
#' @param out.dir directory to create in out.path, set with arg[2]
#' @export
make_weather_stats = function(weather_fname, .gridid, .ssp, .gcm, .plant_date, .harv_date, out.dir) {
  w         = fread(weather_fname)
  names(w)  = c('dom', 'month', 'y', 'doy', 'tasmax', 'tasmin', 'pr')
  prob      = seq(0.1, 0.9, 0.1) # quantile probabilities for summary stats
  freqtmax  = seq(-35, 35, by = 5)
  freqtmin  = seq(-35, 35, by = 5)
  freqpr    = c(seq(0, 4, by = 0.5), seq(5, 10, by = 1), seq(12, 20, by = 2), 25, 30)
  freqdays  = c(seq(0,4, by = 1), seq(5, 10, by = 2), 11)

  # File 1 | Annual weather statistics using daily means and total annual pr, five-year intervals
  # annual weather statistics, five-year intervals
  w_daily   = copy(w) # CHANGE
    # estimate annual daily values
  w_daily[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]
  w_mean.sd  = w_daily[, .( # mean, sd for every block
    mtasmax  = mean(tasmax),
    sdtasmax = sd(tasmax),
    mtasmin  = mean(tasmin),
    sdtasmin = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr)),
    by = .(y_block)]
  w_summary = w_daily[, .( # quantile for every year
    prob     = prob,
    qtasmax  = quantile(tasmax, prob),
    qtasmin  = quantile(tasmin, prob),
    qpr      = quantile(pr, prob)),
  by = .(y_block)]
  w_sum = dcast(w_summary, y_block ~ prob, value.var = colnames(w_summary)[-2])
  w_sum = w_sum[w_mean.sd, on = .(y_block = y_block)]
  w_sum[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_sum = w_sum[, c(-5:-13)]
    # estimate total annual precipitation
  w_yearly  = copy(w) # CHANGE
  w_yearly[, sum_pr := sum(pr), by = y]
  w_yearly[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]
  w_yearly.mean.sd  = w_yearly[, .( # mean, sd for every block
    ann.mpr      = mean(sum_pr),
    ann.sdpr     = sd(sum_pr)),
    by = .(y_block)]
  w_summary.yr = w_yearly[, .( # quantile for every year
    prob       = prob,
    q_ann.mpr  = quantile(sum_pr, prob)),
    by = .(y_block)]
  w_sum.yr = dcast(w_summary.yr, y_block ~ prob, value.var = colnames(w_summary.yr)[-2])
  w_sum.yr = w_sum.yr[w_yearly.mean.sd, on = .(y_block = y_block)]
  w_sum.yr[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum.yr, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_sum.yr = w_sum.yr[, c(-1:-13)]
    # bind tables
  w_sum    = cbind(w_sum, w_sum.yr) # SEE IF THIS CAN BE AVOIDED
    # annual weather statistics, simulation period
    # estimate annual daily values
  w_mean.sd_ann  = w_daily[, .( # mean, sd
    y_block        = 'sim-period',
    mtasmax        = mean(tasmax),
    sdtasmax       = sd(tasmax),
    mtasmin        = mean(tasmin),
    sdtasmin       = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr))]
  w_summary_ann    = w_daily[, .( # quantile
    y_block        = 'sim-period',
    prob           = prob,
    qtasmax        = quantile(tasmax, prob),
    qtasmin        = quantile(tasmin, prob),
    qpr            = quantile(pr, prob))]
  w_sum_ann        = dcast(w_summary_ann, y_block ~ prob, value.var = colnames(w_summary_ann))
  w_sum_ann        = w_sum_ann[w_mean.sd_ann, on = .(y_block = y_block)]
  w_sum_ann[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum_ann, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_sum_ann        = w_sum_ann[, c(-5:-22)]
    # estimate total annual precipitation
  w_yearly.mean.sd_ann  = w_yearly[, .( # mean, sd
    y_block             = 'sim-period',
    ann.mpr             = mean(sum_pr),
    ann.sdpr            = sd(sum_pr))]
  w_summary.yr_ann = w_yearly[, .( # quantile for every year
    y_block        = 'sim-period',
    prob           = prob,
    q_ann.mpr      = quantile(sum_pr, prob))]
  w_sum.yr_ann     = dcast(w_summary.yr_ann, y_block ~ prob, value.var = colnames(w_summary.yr_ann))
  w_sum.yr_ann     = w_sum.yr_ann[w_yearly.mean.sd_ann, on = .(y_block = y_block)]
  w_sum.yr_ann[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum.yr_ann, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_sum.yr_ann     = w_sum.yr_ann[, c(-1:-22)]
    # bind tables
    # SEE IF THIS CAN BE AVOIDED
  w_sum_ann    = cbind(w_sum_ann, w_sum.yr_ann)
  w_sum        = rbind(w_sum, w_sum_ann)
    # save
  w_sum_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_daily_annual_statistics.csv')
  fwrite(w_sum, w_sum_fname, append = TRUE)

  # File 2 | Histogram bins of variable frequencies, five-year blocks AND simulation period
  # frequencies, and counts of days (pr, dry | greater than 75% or less than 25% for tmin, tmax)

  w_hist     = copy(w)
  w_hist[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]
    # pr
  w_hist.sim.pr = transform(table(cut(w_hist$pr, freqpr)))
  setDT(w_hist.sim.pr)
  w_hist.sim.pr[, y_block := 'sim-period']
  w_hist.sim.pr[, var := 'pr']
  names(w_hist.sim.pr)[1] = 'Bin'
  setcolorder(w_hist.sim.pr, c('y_block', 'var'))

  w_hist.pr = data.table()
  for (block in unique(w_hist$y_block)) {
    pr_freq = transform(table(cut(w_hist[y_block == block, pr], freqpr)))
    setDT(pr_freq)
    pr_freq[, y_block := block]
    pr_freq[, var := 'pr']
    names(pr_freq)[1] = 'Bin'
    setcolorder(pr_freq, c('y_block', 'var'))
    w_hist.pr = rbind(w_hist.pr, pr_freq)
  }
    # consecutive pr days > 0 | one or more days
  cons_pr         = w_hist[, .SD[.N >= 1 & all(pr > 0)], rleid(pr > 0)]
  cons_pr_count   = cons_pr[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_pr_sim.per = transform(table(cut(cons_pr_count$N, freqdays))) # count by sim period
  setDT(cons_pr_sim.per)
  cons_pr_sim.per[, y_block := 'sim-period']
  cons_pr_sim.per[, var := 'days_pr']
  names(cons_pr_sim.per)[1] = 'Bin'
  setcolorder(cons_pr_sim.per, c('y_block', 'var'))

  cons_pr_blk = data.table() # count by block
  for (block in unique(cons_pr$y_block)) {
    cons_pr_freq = transform(table(cut(cons_pr_count[y_block == block, N], freqdays)))
    setDT(cons_pr_freq)
    cons_pr_freq[, y_block := block]
    cons_pr_freq[, var := 'days_pr']
    names(cons_pr_freq)[1] = 'Bin'
    setcolorder(cons_pr_freq, c('y_block', 'var'))
    cons_pr_blk = rbind(cons_pr_blk, cons_pr_freq)
  }
    # consecutive dry days > 0 | one or more days
  cons_dry = w_hist[, .SD[.N >= 1 & all(pr == 0)], rleid(pr == 0)]
  cons_dry_count   = cons_dry[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_dry_sim.per = transform(table(cut(cons_dry_count$N, freqdays))) # count by sim period
  setDT(cons_dry_sim.per)
  cons_dry_sim.per[, y_block := 'sim-period']
  cons_dry_sim.per[, var := 'days_dry']
  names(cons_dry_sim.per)[1] = 'Bin'
  setcolorder(cons_dry_sim.per, c('y_block', 'var'))

  cons_dry_blk = data.table() # count by block
  for (block in unique(cons_dry$y_block)) {
    cons_dry_freq = transform(table(cut(cons_dry_count[y_block == block, N], freqdays)))
    setDT(cons_dry_freq)
    cons_dry_freq[, y_block := block]
    cons_dry_freq[, var := 'days_dry']
    names(cons_dry_freq)[1] = 'Bin'
    setcolorder(cons_dry_freq, c('y_block', 'var'))
    cons_dry_blk = rbind(cons_dry_blk, cons_dry_freq)
  }
    # tasmin
  w_hist.sim.tasmin = transform(table(cut(w_hist$tasmin, freqtmin)))
  setDT(w_hist.sim.tasmin)
  w_hist.sim.tasmin[, y_block := 'sim-period']
  w_hist.sim.tasmin[, var := 'tasmin']
  names(w_hist.sim.tasmin)[1] = 'Bin'
  setcolorder(w_hist.sim.tasmin, c('y_block', 'var'))

  w_hist.tasmin = data.table()
  for (block in unique(w_hist$y_block)) {
    tasmin_freq = transform(table(cut(w_hist[y_block == block, tasmin], freqtmin)))
    setDT(tasmin_freq)
    tasmin_freq[, y_block := block]
    tasmin_freq[, var := 'tasmin']
    names(tasmin_freq)[1] = 'Bin'
    setcolorder(tasmin_freq, c('y_block', 'var'))
    w_hist.tasmin = rbind(w_hist.tasmin, tasmin_freq)
  }
    # consecutive tasmin days > 75th percentile | one or more days
  tmin.75 = quantile(w_hist$tasmin, probs = c(.75))
  cons_tasmin75         = w_hist[, .SD[.N >= 1 & all(tasmin > tmin.75)], rleid(tasmin > tmin.75)]
  cons_tasmin_count75   = cons_tasmin75[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_tasmin_sim.per75 = transform(table(cut(cons_tasmin_count75$N, freqdays))) # count by sim period
  setDT(cons_tasmin_sim.per75)
  cons_tasmin_sim.per75[, y_block := 'sim-period']
  cons_tasmin_sim.per75[, var := 'days_grtr.75.tasmin']
  names(cons_tasmin_sim.per75)[1] = 'Bin'
  setcolorder(cons_tasmin_sim.per75, c('y_block', 'var'))

  cons_tasmin_blk75 = data.table() # count by block
  for (block in unique(cons_tasmin75$y_block)) {
    cons_tasmin_freq = transform(table(cut(cons_tasmin_count75[y_block == block, N], freqdays)))
    setDT(cons_tasmin_freq)
    cons_tasmin_freq[, y_block := block]
    cons_tasmin_freq[, var := 'days_grtr.75.tasmin']
    names(cons_tasmin_freq)[1] = 'Bin'
    setcolorder(cons_tasmin_freq, c('y_block', 'var'))
    cons_tasmin_blk75 = rbind(cons_tasmin_blk75, cons_tasmin_freq)
  }
    # consecutive tasmin days < 25th percentile | one or more days
  tmin.25 = quantile(w_hist$tasmin, probs = c(.25))
  cons_tasmin         = w_hist[, .SD[.N >= 1 & all(tasmin < tmin.25)], rleid(tasmin < tmin.25)]
  cons_tasmin_count   = cons_tasmin[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_tasmin_sim.per = transform(table(cut(cons_tasmin_count$N, freqdays))) # count by sim period
  setDT(cons_tasmin_sim.per)
  cons_tasmin_sim.per[, y_block := 'sim-period']
  cons_tasmin_sim.per[, var := 'days_less.25.tasmin']
  names(cons_tasmin_sim.per)[1] = 'Bin'
  setcolorder(cons_tasmin_sim.per, c('y_block', 'var'))

  cons_tasmin_blk = data.table() # count by block
  for (block in unique(cons_tasmin$y_block)) {
    cons_tasmin_freq = transform(table(cut(cons_tasmin_count[y_block == block, N], freqdays)))
    setDT(cons_tasmin_freq)
    cons_tasmin_freq[, y_block := block]
    cons_tasmin_freq[, var := 'days_less.25.tasmin']
    names(cons_tasmin_freq)[1] = 'Bin'
    setcolorder(cons_tasmin_freq, c('y_block', 'var'))
    cons_tasmin_blk = rbind(cons_tasmin_blk, cons_tasmin_freq)
  }
    # tasmax
  w_hist.sim.tasmax = transform(table(cut(w_hist$tasmax, freqtmax)))
  setDT(w_hist.sim.tasmax)
  w_hist.sim.tasmax[, y_block := 'sim-period']
  w_hist.sim.tasmax[, var := 'tasmax']
  names(w_hist.sim.tasmax)[1] = 'Bin'
  setcolorder(w_hist.sim.tasmax, c('y_block', 'var'))

  w_hist.tasmax = data.table()
  for (block in unique(w_hist$y_block)) {
    tasmax_freq = transform(table(cut(w_hist[y_block == block, tasmax], freqtmax)))
    setDT(tasmax_freq)
    tasmax_freq[, y_block := block]
    tasmax_freq[, var := 'tasmax']
    names(tasmax_freq)[1] = 'Bin'
    setcolorder(tasmax_freq, c('y_block', 'var'))
    w_hist.tasmax = rbind(w_hist.tasmax, tasmax_freq)
  }
    # consecutive tasmax days > 75th percentile | one or more days
  tmax.75 = quantile(w_hist$tasmax, probs = c(.75))
  cons_tasmax75         = w_hist[, .SD[.N >= 1 & all(tasmax > tmax.75)], rleid(tasmax > tmax.75)]
  cons_tasmax_count75   = cons_tasmax75[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_tasmax_sim.per75 = transform(table(cut(cons_tasmax_count75$N, freqdays))) # count by sim period
  setDT(cons_tasmax_sim.per75)
  cons_tasmax_sim.per75[, y_block := 'sim-period']
  cons_tasmax_sim.per75[, var := 'days_grtr.75.tasmax']
  names(cons_tasmax_sim.per75)[1] = 'Bin'
  setcolorder(cons_tasmax_sim.per75, c('y_block', 'var'))

  cons_tasmax_blk75 = data.table() # count by block
  for (block in unique(cons_tasmax75$y_block)) {
    cons_tasmax_freq = transform(table(cut(cons_tasmax_count75[y_block == block, N], freqdays)))
    setDT(cons_tasmax_freq)
    cons_tasmax_freq[, y_block := block]
    cons_tasmax_freq[, var := 'days_grtr.75.tasmax']
    names(cons_tasmax_freq)[1] = 'Bin'
    setcolorder(cons_tasmax_freq, c('y_block', 'var'))
    cons_tasmax_blk75 = rbind(cons_tasmax_blk75, cons_tasmax_freq)
  }
    # consecutive tasmax days < 25th percentile | one or more days
  tmax.25 = quantile(w_hist$tasmax, probs = c(.25))
  cons_tasmax         = w_hist[, .SD[.N >= 1 & all(tasmax < tmax.25)], rleid(tasmax < tmax.25)]
  cons_tasmax_count   = cons_tasmax[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_tasmax_sim.per = transform(table(cut(cons_tasmax_count$N, freqdays))) # count by sim period
  setDT(cons_tasmax_sim.per)
  cons_tasmax_sim.per[, y_block := 'sim-period']
  cons_tasmax_sim.per[, var := 'days_less.25.tasmax']
  names(cons_tasmax_sim.per)[1] = 'Bin'
  setcolorder(cons_tasmax_sim.per, c('y_block', 'var'))

  cons_tasmax_blk = data.table() # count by block
  for (block in unique(cons_tasmax$y_block)) {
    cons_tasmax_freq = transform(table(cut(cons_tasmax_count[y_block == block, N], freqdays)))
    setDT(cons_tasmax_freq)
    cons_tasmax_freq[, y_block := block]
    cons_tasmax_freq[, var := 'days_less.25.tasmax']
    names(cons_tasmax_freq)[1] = 'Bin'
    setcolorder(cons_tasmax_freq, c('y_block', 'var'))
    cons_tasmax_blk = rbind(cons_tasmax_blk, cons_tasmax_freq)
  }
    # rbind all histogram tables
  histogram.tbl = rbind(w_hist.sim.pr, w_hist.pr, w_hist.sim.tasmin, w_hist.tasmin,
                        w_hist.sim.tasmax, w_hist.tasmax, cons_dry_sim.per, cons_dry_blk,
                        cons_dry_sim.per, cons_pr_blk, cons_tasmin_sim.per, cons_tasmin_blk,
                        cons_tasmax_sim.per, cons_tasmax_blk, cons_tasmin_sim.per75, cons_tasmin_blk75,
                        cons_tasmax_sim.per75, cons_tasmax_blk75)
  histogram.tbl[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(histogram.tbl, c('gridid', 'y_block', 'gcm', 'ssp'))
    # save
  w_hist_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_histogram-statistics.csv')
  fwrite(histogram.tbl, w_hist_fname, append = TRUE)

  # File 3 | Monthly weather statistics using daily means, five-year intervals AND simulation period
    # bioclimatic variables

  w_monthly   = copy(w)
  w_monthly[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]

    # estimate monthly daily values
  w_mon.mean.sd  = w_monthly[, .( # mean, sd
    mtasmax      = mean(tasmax),
    sdtasmax     = sd(tasmax),
    mtasmin      = mean(tasmin),
    sdtasmin     = sd(tasmin),
    daily.mpr    = mean(pr),
    daily.sdpr   = sd(pr)),
    by           = .(month, y_block)]
  w_mon.mean.sd$month = as.factor(w_mon.mean.sd$month)
  levels(w_mon.mean.sd$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_mon.summary  = w_monthly[, .( # quantile for every month
    prob         = prob,
    qtasmax      = quantile(tasmax, prob),
    qtasmin      = quantile(tasmin, prob),
    qpr          = quantile(pr, prob)),
    by           = .(month, y_block)]
  w_mon.summary$month = as.factor(w_mon.summary$month)
  levels(w_mon.summary$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_mon.sum = dcast(w_mon.summary, month + y_block ~ prob, value.var = colnames(w_mon.summary)[c(-2,-3)])
  w_mon.sum = w_mon.sum[ w_mon.mean.sd, on = .(y_block = y_block, month = month)]
  w_mon.sum[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_mon.sum, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_mon.sum = w_mon.sum[, c(-6:-14)]
    # estimate total monthly precipitation
  w_mon.yearly = copy(w)
  w_mon.yearly[, sum_pr := sum(pr), by = .(y, month)]
  w_mon.yearly[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]
  w_mon.yearly.mean.sd  = w_mon.yearly[, .( # mean, sd for every block
    mon.mpr             = mean(sum_pr),
    mon.sdpr            = sd(sum_pr)),
    by                  = .(month, y_block)]
  w_mon.yearly.mean.sd$month = as.factor(w_mon.yearly.mean.sd$month)
  levels(w_mon.yearly.mean.sd$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_mon.summary.yr = w_mon.yearly[, .( # quantile for every year
    prob       = prob,
    q_mon.mpr  = quantile(sum_pr, prob)),
    by = .(month, y_block)]
  w_mon.summary.yr$month = as.factor(w_mon.summary.yr$month)
  levels(w_mon.summary.yr$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_mon.sum.yr = dcast(w_mon.summary.yr, month + y_block ~ prob, value.var = colnames(w_mon.summary.yr)[c(-2,-3)])
  w_mon.sum.yr = w_mon.sum.yr[w_mon.yearly.mean.sd, on = .(y_block = y_block, month = month)]
  w_mon.sum.yr[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_mon.sum.yr, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_mon.sum.yr = w_mon.sum.yr[, c(-1:-14)]
    # bind tables
  w_mon.sum    = cbind(w_mon.sum, w_mon.sum.yr)
  # monthly weather statistics, simulation period
    # estimate monthly daily values
  w_mean.sd_mon  = w_monthly[, .( # mean, sd
    y_block        = 'sim-period',
    mtasmax        = mean(tasmax),
    sdtasmax       = sd(tasmax),
    mtasmin        = mean(tasmin),
    sdtasmin       = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr)),
    by             = .(month)]
  w_mean.sd_mon$month = as.factor(w_mean.sd_mon$month)
  levels(w_mean.sd_mon$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_summary_mon    = w_monthly[, .( # quantile
    y_block        = 'sim-period',
    prob           = prob,
    qtasmax        = quantile(tasmax, prob),
    qtasmin        = quantile(tasmin, prob),
    qpr            = quantile(pr, prob)),
    by             = .(month)]
  w_summary_mon$month = as.factor(w_summary_mon$month)
  levels(w_summary_mon$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_sum_mon        = dcast(w_summary_mon, month ~ prob, value.var = colnames(w_summary_mon))
  w_sum_mon        = w_sum_mon[w_mean.sd_mon, on = .(month = month)]
  w_sum_mon[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum_mon, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_sum_mon        = w_sum_mon[, c(-6:-32)]
    # estimate total monthly precipitation
  w_yearly.mean.sd_mon  = w_mon.yearly[, .( # mean, sd
    y_block             = 'sim-period',
    mon.mpr             = mean(sum_pr),
    mon.sdpr            = sd(sum_pr)),
    by                  = .(month)]
  w_yearly.mean.sd_mon$month = as.factor(w_yearly.mean.sd_mon$month)
  levels(w_yearly.mean.sd_mon$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_summary.yr_mon = w_mon.yearly[, .( # quantile for every year
    y_block        = 'sim-period',
    prob           = prob,
    q_mon.mpr      = quantile(sum_pr, prob)),
    by             = .(month)]
  w_summary.yr_mon$month = as.factor(w_summary.yr_mon$month)
  levels(w_summary.yr_mon$month) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  w_sum.yr_mon     = dcast(w_summary.yr_mon, month ~ prob, value.var = colnames(w_summary.yr_mon))
  w_sum.yr_mon     = w_sum.yr_mon[w_yearly.mean.sd_mon, on = .(month = month)]
  w_sum.yr_mon[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_sum.yr_mon, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_sum.yr_mon     = w_sum.yr_mon[, c(-1:-32)]
    # bind tables
  w_sum_mon    = cbind(w_sum_mon, w_sum.yr_mon)
  w_mon.sum    = rbind(w_mon.sum, w_sum_mon)
    # save
  w_sum_mon_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_daily_monthly_statistics.csv')
  fwrite(w_mon.sum, w_sum_mon_fname, append = TRUE)

  # File 4 | Bioclimatic variables, five-year intervals AND simulation period
    # simulation period
  biovars_sim.period = biovars(w_mean.sd_mon[,daily.mpr], w_mean.sd_mon[, mtasmin],
                               w_mean.sd_mon[, mtasmax])
  biovars_sim.period = setDT(as.data.table(biovars_sim.period))
  biovars_sim.period[, `:=` (gridid = .gridid, y_block = 'sim-period', ssp = .ssp, gcm = .gcm)]
  setcolorder(biovars_sim.period, c('gridid', 'y_block', 'gcm', 'ssp'))
    # five-year intervals
  biovars_blk   = data.table() # biovars by block
  for (block in unique(w_mon.mean.sd$y_block)) {
    biovars_tbl = biovars(w_mon.mean.sd[y_block == block,daily.mpr], w_mon.mean.sd[y_block == block, mtasmin],
                          w_mon.mean.sd[y_block == block, mtasmax])
    biovars_tbl = setDT(as.data.table(biovars_tbl))
    biovars_tbl[, `:=` (gridid = .gridid, y_block = block, ssp = .ssp, gcm = .gcm)]
    setcolorder(biovars_tbl, c('gridid', 'y_block', 'gcm', 'ssp'))
    biovars_blk = rbind(biovars_blk, biovars_tbl)
  }
  biovars_tbl   = rbind(biovars_blk, biovars_sim.period)
    # save
  biovars_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_biovars_statistics.csv')
  fwrite(biovars_tbl, biovars_fname, append = TRUE)
}

#' Create weather growing and non-growing season statistics file
#'
#' Used for side effect of writing a weather files based on location.
#' Location is specified as a raster cell number.
#'
#' This function creates a weather statistics files for a specified climate model, ssp, and location.
#' Raw climate model data was converted into daily values in the Gregorian calendar, which sometimes
#' requires date conversion from the native gcm calendar.  Daily vectors are constructed from 2015 through
#' to 2100 for each of, minimum daily temperature (tasmin), maximum daily temperature (tasmax),
#' and precipitation (pr).  These vectors are written as columns in the weather file which is written to
#' a uniquely named file.  File naming is unique to allow parallel execution of DayCent, with each instance
#' using a different weather file.
#'
#'There are 3 files created in this function:
#'1. Growing season weather statistics using daily means, five-year intervals AND simulation period.
#'This file includes data on quantiles, means, and standard deviations for tasmin, tasmax, and pr.
#'Estimates were made over the growing season for the entire simulation period and in five-year intervals.
#'
#'2. Non-growing season weather statistics using daily means, five-year intervals AND simulation period.
#'This file includes data on quantiles, means, and standard deviations for tasmin, tasmax, and pr.
#'Estimates were made over the non-growin season for the entire simulation period and in five-year intervals.
#'
#'3. Histogram bins of growing and non-growing season variable frequencies, five-year intervals AND simulation period.
#'This file includes frequency data on tasmin, tasmax, and pr, including consecutive days of pr,
#'consecutive dry days, consecutive days exceeding 75% quantile and below 25% quantile for tasmin, tasmax.
#'
#'@import data.table
#'@import dismo
#'@param weather_fname data.table, containing 86 years (2016-2100) of tasmin, tasmax, and pr data for gridcell.
#'Must be read in to function as a file.path
#' @param .gridid integer, cell number (-180 to 180 longitude) from cell_data_table.
#' @param .gcm character (length one) giving name of the cmip6 model.
#' @param .ssp character (length one) giving name of ssp scenario.
#' @param .crop character (length one) giving crop name from cell_data_table.
#' @param .plant_date integer, planting date for crop from cell_data_table.
#' @param .harv_date integer, harvest data for crop from cell_data_table.
#' @param out.dir directory to create in out.path, set with arg[2]
#' @export

make_weather_grwszn_stats = function(weather_fname, .gridid, .crop, .ssp, .gcm, .plant_date, .harv_date, out.dir) {
  w         = fread(weather_fname)
  names(w)  = c('dom', 'month', 'y', 'doy', 'tasmax', 'tasmin', 'pr')
  prob      = seq(0.1, 0.9, 0.1) # quantile probabilities for summary stats
  freqtmax  = seq(-35, 35, by = 5)
  freqtmin  = seq(-35, 35, by = 5)
  freqpr    = c(seq(0, 4, by = 0.5), seq(5, 10, by = 1), seq(12, 20, by = 2), 25, 30)
  freqdays  = c(seq(0,4, by = 1), seq(5, 10, by = 2), 11)

  # File 1 | Growing season weather statistics using daily means, five-year intervals AND simulation period
  # weather statistics
  w_growing = copy(w)
  if (.plant_date < .harv_date) {
    w_growing = w_growing[doy >= .plant_date & doy <= .harv_date,]
  } else {
    w_growing = w_growing[doy >= .plant_date | doy <= .harv_date,]
  }
  w_growing[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]
  w_gr.mean.sd  = w_growing[, .( # mean, sd for every block
    mtasmax  = mean(tasmax),
    sdtasmax = sd(tasmax),
    mtasmin  = mean(tasmin),
    sdtasmin = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr)),
    by = .(y_block)]
  w_gr.summary = w_growing[, .( # quantile for every year
    prob     = prob,
    qtasmax  = quantile(tasmax, prob),
    qtasmin  = quantile(tasmin, prob),
    qpr      = quantile(pr, prob)),
    by = .(y_block)]
  w_gr.sum = dcast(w_gr.summary, y_block ~ prob, value.var = colnames(w_gr.summary)[-2])
  w_gr.sum = w_gr.sum[w_gr.mean.sd, on = .(y_block = y_block)]
  w_gr.sum[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_gr.sum, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_gr.sum = w_gr.sum[, c(-5:-13)]
  # estimate total growing season precipitation
  w_gryearly  = copy(w_growing)
  w_gryearly[, sum_pr := sum(pr), by = y]
  w_gryearly.mean.sd  = w_gryearly[, .( # mean, sd for every block
    gr.mpr      = mean(sum_pr),
    gr.sdpr     = sd(sum_pr)),
    by = .(y_block)]
  w_grsummary.yr = w_gryearly[, .( # quantile for every year
    prob       = prob,
    q_gr.mpr  = quantile(sum_pr, prob)),
    by = .(y_block)]
  w_grsum.yr = dcast(w_grsummary.yr, y_block ~ prob, value.var = colnames(w_grsummary.yr)[-2])
  w_grsum.yr = w_grsum.yr[w_gryearly.mean.sd, on = .(y_block = y_block)]
  w_grsum.yr[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_grsum.yr, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_grsum.yr = w_grsum.yr[, c(-1:-13)]
  # bind tables
  w_gr.sum    = cbind(w_gr.sum, w_grsum.yr)
  # growing season weather statistics, simulation period
  # estimate growing season daily values
  w_grmean.sd_ann  = w_growing[, .( # mean, sd
    y_block        = 'sim-period',
    mtasmax        = mean(tasmax),
    sdtasmax       = sd(tasmax),
    mtasmin        = mean(tasmin),
    sdtasmin       = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr))]
  w_grsummary_ann  = w_growing[, .( # quantile
    y_block        = 'sim-period',
    prob           = prob,
    qtasmax        = quantile(tasmax, prob),
    qtasmin        = quantile(tasmin, prob),
    qpr            = quantile(pr, prob))]
  w_grsum_ann        = dcast(w_grsummary_ann, y_block ~ prob, value.var = colnames(w_grsummary_ann))
  w_grsum_ann        = w_grsum_ann[w_grmean.sd_ann, on = .(y_block = y_block)]
  w_grsum_ann[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_grsum_ann, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_grsum_ann        = w_grsum_ann[, c(-5:-22)]
  # estimate total gr precipitation
  w_gryearly.mean.sd_ann  = w_gryearly[, .( # mean, sd
    y_block               = 'sim-period',
    gr.mpr                = mean(sum_pr),
    gr.sdpr               = sd(sum_pr))]
  w_grsummary.yr_ann      = w_gryearly[, .( # quantile for every year
    y_block               = 'sim-period',
    prob                  = prob,
    q_gr.mpr              = quantile(sum_pr, prob))]
  w_grsum.yr_ann     = dcast(w_grsummary.yr_ann, y_block ~ prob, value.var = colnames(w_grsummary.yr_ann))
  w_grsum.yr_ann     = w_grsum.yr_ann[w_gryearly.mean.sd_ann, on = .(y_block = y_block)]
  w_grsum.yr_ann[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_grsum.yr_ann, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_grsum.yr_ann     = w_grsum.yr_ann[, c(-1:-22)]
  # bind tables
  w_grsum_ann     = cbind(w_grsum_ann, w_grsum.yr_ann)
  w_gr.sum        = rbind(w_gr.sum, w_grsum_ann)
  w_gr.sum[, crop := .crop]
  setcolorder(w_gr.sum, c('gridid', 'y_block', 'gcm', 'ssp', 'crop'))
  # save
  w_grsum_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_daily_growing-season_statistics.csv')
  fwrite(w_gr.sum, w_grsum_fname, append = TRUE)

  # File 2 | Non-growing season weather statistics using daily means, five-year intervals AND simulation period
  # weather statistics
  w_ngrowing = copy(w)
  if (.plant_date < .harv_date) {
    w_ngrowing = w_ngrowing[doy <= .plant_date | doy >= .harv_date,]
  } else {
    w_ngrowing = w_ngrowing[doy <= .plant_date & doy >= .harv_date,]
  }
  w_ngrowing[, ("y_block") := lapply(.SD, make_blocks2), .SDcols = "y"]
  w_ngr.mean.sd  = w_ngrowing[, .( # mean, sd for every block
    mtasmax  = mean(tasmax),
    sdtasmax = sd(tasmax),
    mtasmin  = mean(tasmin),
    sdtasmin = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr)),
    by = .(y_block)]
  w_ngr.summary = w_ngrowing[, .( # quantile for every year
    prob     = prob,
    qtasmax  = quantile(tasmax, prob),
    qtasmin  = quantile(tasmin, prob),
    qpr      = quantile(pr, prob)),
    by = .(y_block)]
  w_ngr.sum = dcast(w_ngr.summary, y_block ~ prob, value.var = colnames(w_ngr.summary)[-2])
  w_ngr.sum = w_ngr.sum[w_ngr.mean.sd, on = .(y_block = y_block)]
  w_ngr.sum[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_ngr.sum, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_ngr.sum = w_ngr.sum[, c(-5:-13)]
  # estimate total ngrowing season precipitation
  w_ngryearly  = copy(w_ngrowing)
  w_ngryearly[, sum_pr := sum(pr), by = y]
  w_ngryearly.mean.sd  = w_ngryearly[, .( # mean, sd for every block
    ngr.mpr      = mean(sum_pr),
    ngr.sdpr     = sd(sum_pr)),
    by = .(y_block)]
  w_ngrsummary.yr = w_ngryearly[, .( # quantile for every year
    prob       = prob,
    q_ngr.mpr  = quantile(sum_pr, prob)),
    by = .(y_block)]
  w_ngrsum.yr = dcast(w_ngrsummary.yr, y_block ~ prob, value.var = colnames(w_ngrsummary.yr)[-2])
  w_ngrsum.yr = w_ngrsum.yr[w_ngryearly.mean.sd, on = .(y_block = y_block)]
  w_ngrsum.yr[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_ngrsum.yr, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_ngrsum.yr = w_ngrsum.yr[, c(-1:-13)]
  # bind tables
  w_ngr.sum    = cbind(w_ngr.sum, w_ngrsum.yr)
  # ngrowing season weather statistics, simulation period
  # estimate ngrowing season daily values
  w_ngrmean.sd_ann  = w_ngrowing[, .( # mean, sd
    y_block        = 'sim-period',
    mtasmax        = mean(tasmax),
    sdtasmax       = sd(tasmax),
    mtasmin        = mean(tasmin),
    sdtasmin       = sd(tasmin),
    daily.mpr      = mean(pr),
    daily.sdpr     = sd(pr))]
  w_ngrsummary_ann  = w_ngrowing[, .( # quantile
    y_block        = 'sim-period',
    prob           = prob,
    qtasmax        = quantile(tasmax, prob),
    qtasmin        = quantile(tasmin, prob),
    qpr            = quantile(pr, prob))]
  w_ngrsum_ann        = dcast(w_ngrsummary_ann, y_block ~ prob, value.var = colnames(w_ngrsummary_ann))
  w_ngrsum_ann        = w_ngrsum_ann[w_ngrmean.sd_ann, on = .(y_block = y_block)]
  w_ngrsum_ann[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_ngrsum_ann, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_ngrsum_ann        = w_ngrsum_ann[, c(-5:-22)]
  # estimate total ngrowing precipitation
  w_ngryearly.mean.sd_ann  = w_ngryearly[, .( # mean, sd
    y_block                = 'sim-period',
    ngr.mpr                = mean(sum_pr),
    ngr.sdpr               = sd(sum_pr))]
  w_ngrsummary.yr_ann      = w_ngryearly[, .( # quantile for every year
    y_block                = 'sim-period',
    prob                   = prob,
    q_ngr.mpr              = quantile(sum_pr, prob))]
  w_ngrsum.yr_ann     = dcast(w_ngrsummary.yr_ann, y_block ~ prob, value.var = colnames(w_ngrsummary.yr_ann))
  w_ngrsum.yr_ann     = w_ngrsum.yr_ann[w_ngryearly.mean.sd_ann, on = .(y_block = y_block)]
  w_ngrsum.yr_ann[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(w_ngrsum.yr_ann, c('gridid', 'y_block', 'gcm', 'ssp'))
  w_ngrsum.yr_ann     = w_ngrsum.yr_ann[, c(-1:-22)]
  # bind tables
  w_ngrsum_ann     = cbind(w_ngrsum_ann, w_ngrsum.yr_ann)
  w_ngr.sum        = rbind(w_ngr.sum, w_ngrsum_ann)
  w_ngr.sum[, crop := .crop]
  setcolorder(w_ngr.sum, c('gridid', 'y_block', 'gcm', 'ssp', 'crop'))
  # save
  w_ngrsum_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_daily_ngrowing-season_statistics.csv')
  fwrite(w_ngr.sum, w_ngrsum_fname, append = TRUE)

  # File 3 | Histogram bins of variable frequencies for growing and non-growing season, five-year blocks AND simulation period
  # frequencies, and counts of days (pr, dry | greater than 75% or less than 25% for tmin, tmax)
  # pr
  w_growing.sim.pr = transform(table(cut(w_growing$pr, freqpr)))
  setDT(w_growing.sim.pr)
  w_growing.sim.pr[, y_block := 'sim-period']
  w_growing.sim.pr[, var := 'pr']
  w_growing.sim.pr[, period := 'growing-season']
  names(w_growing.sim.pr)[1] = 'Bin'
  setcolorder(w_growing.sim.pr, c('y_block', 'var', 'period'))

  w_growing.pr = data.table()
  for (block in unique(w_growing$y_block)) {
    pr_freq = transform(table(cut(w_growing[y_block == block, pr], freqpr)))
    setDT(pr_freq)
    pr_freq[, y_block := block]
    pr_freq[, var := 'pr']
    pr_freq[, period := 'growing-season']
    names(pr_freq)[1] = 'Bin'
    setcolorder(pr_freq, c('y_block', 'var', 'period'))
    w_growing.pr = rbind(w_growing.pr, pr_freq)
  }
  # consecutive pr days > 0 | one or more days
  cons_gr_pr         = w_growing[, .SD[.N >= 1 & all(pr > 0)], rleid(pr > 0)]
  cons_gr_pr_count   = cons_gr_pr[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_gr_pr_sim.per = transform(table(cut(cons_gr_pr_count$N, freqdays))) # count by sim period
  setDT(cons_gr_pr_sim.per)
  cons_gr_pr_sim.per[, y_block := 'sim-period']
  cons_gr_pr_sim.per[, var := 'days_pr']
  cons_gr_pr_sim.per[, period := 'growing-season']
  names(cons_gr_pr_sim.per)[1] = 'Bin'
  setcolorder(cons_gr_pr_sim.per, c('y_block', 'var', 'period'))

  cons_gr_pr_blk = data.table() # count by block
  for (block in unique(cons_gr_pr$y_block)) {
    cons_gr_pr_freq = transform(table(cut(cons_gr_pr_count[y_block == block, N], freqdays)))
    setDT(cons_gr_pr_freq)
    cons_gr_pr_freq[, y_block := block]
    cons_gr_pr_freq[, var := 'days_pr']
    cons_gr_pr_freq[, period := 'growing-season']
    names(cons_gr_pr_freq)[1] = 'Bin'
    setcolorder(cons_gr_pr_freq, c('y_block', 'var', 'period'))
    cons_gr_pr_blk = rbind(cons_gr_pr_blk, cons_gr_pr_freq)
  }
  # consecutive dry days > 0 | one or more days
  cons_gr_dry = w_growing[, .SD[.N >= 1 & all(pr == 0)], rleid(pr == 0)]
  cons_gr_dry_count   = cons_gr_dry[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_gr_dry_sim.per = transform(table(cut(cons_gr_dry_count$N, freqdays))) # count by sim period
  setDT(cons_gr_dry_sim.per)
  cons_gr_dry_sim.per[, y_block := 'sim-period']
  cons_gr_dry_sim.per[, var := 'days_dry']
  cons_gr_dry_sim.per[, period := 'growing-season']
  names(cons_gr_dry_sim.per)[1] = 'Bin'
  setcolorder(cons_gr_dry_sim.per, c('y_block', 'var', 'period'))

  cons_gr_dry_blk = data.table() # count by block
  for (block in unique(cons_gr_dry$y_block)) {
    cons_gr_dry_freq = transform(table(cut(cons_gr_dry_count[y_block == block, N], freqdays)))
    setDT(cons_gr_dry_freq)
    cons_gr_dry_freq[, y_block := block]
    cons_gr_dry_freq[, var := 'days_dry']
    cons_gr_dry_freq[, period := 'growing-season']
    names(cons_gr_dry_freq)[1] = 'Bin'
    setcolorder(cons_gr_dry_freq, c('y_block', 'var', 'period'))
    cons_gr_dry_blk = rbind(cons_gr_dry_blk, cons_gr_dry_freq)
  }
  # tasmin
  w_growing.sim.tasmin = transform(table(cut(w_growing$tasmin, freqtmin)))
  setDT(w_growing.sim.tasmin)
  w_growing.sim.tasmin[, y_block := 'sim-period']
  w_growing.sim.tasmin[, var := 'tasmin']
  w_growing.sim.tasmin[, period := 'growing-season']
  names(w_growing.sim.tasmin)[1] = 'Bin'
  setcolorder(w_growing.sim.tasmin, c('y_block', 'var', 'period'))

  w_growing.tasmin = data.table()
  for (block in unique(w_growing$y_block)) {
    tasmin_freq = transform(table(cut(w_growing[y_block == block, tasmin], freqtmin)))
    setDT(tasmin_freq)
    tasmin_freq[, y_block := block]
    tasmin_freq[, var := 'tasmin']
    tasmin_freq[, period := 'growing-season']
    names(tasmin_freq)[1] = 'Bin'
    setcolorder(tasmin_freq, c('y_block', 'var', 'period'))
    w_growing.tasmin = rbind(w_growing.tasmin, tasmin_freq)
  }
  # consecutive tasmin days > 75th percentile | one or more days
  grtmin.75 = quantile(w_growing$tasmin, probs = c(.75))
  cons_gr_tasmin75         = w_growing[, .SD[.N >= 1 & all(tasmin > grtmin.75)], rleid(tasmin > grtmin.75)]
  cons_gr_tasmin_count75   = cons_gr_tasmin75[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_gr_tasmin_sim.per75 = transform(table(cut(cons_gr_tasmin_count75$N, freqdays))) # count by sim period
  setDT(cons_gr_tasmin_sim.per75)
  cons_gr_tasmin_sim.per75[, y_block := 'sim-period']
  cons_gr_tasmin_sim.per75[, var := 'days_grtr.75.tasmin']
  cons_gr_tasmin_sim.per75[, period := 'growing-season']
  names(cons_gr_tasmin_sim.per75)[1] = 'Bin'
  setcolorder(cons_gr_tasmin_sim.per75, c('y_block', 'var', 'period'))

  cons_gr_tasmin_blk75 = data.table() # count by block
  for (block in unique(cons_gr_tasmin75$y_block)) {
    cons_gr_tasmin_freq = transform(table(cut(cons_gr_tasmin_count75[y_block == block, N], freqdays)))
    setDT(cons_gr_tasmin_freq)
    cons_gr_tasmin_freq[, y_block := block]
    cons_gr_tasmin_freq[, var := 'days_grtr.75.tasmin']
    cons_gr_tasmin_freq[, period := 'growing-season']
    names(cons_gr_tasmin_freq)[1] = 'Bin'
    setcolorder(cons_gr_tasmin_freq, c('y_block', 'var', 'period'))
    cons_gr_tasmin_blk75 = rbind(cons_gr_tasmin_blk75, cons_gr_tasmin_freq)
  }
  # consecutive tasmin days < 25th percentile | one or more days
  grtmin.25 = quantile(w_growing$tasmin, probs = c(.25))
  cons_gr_tasmin         = w_growing[, .SD[.N >= 1 & all(tasmin < grtmin.25)], rleid(tasmin < grtmin.25)]
  cons_gr_tasmin_count   = cons_gr_tasmin[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_gr_tasmin_sim.per = transform(table(cut(cons_gr_tasmin_count$N, freqdays))) # count by sim period
  setDT(cons_gr_tasmin_sim.per)
  cons_gr_tasmin_sim.per[, y_block := 'sim-period']
  cons_gr_tasmin_sim.per[, var := 'days_less.25.tasmin']
  cons_gr_tasmin_sim.per[, period := 'growing-season']
  names(cons_gr_tasmin_sim.per)[1] = 'Bin'
  setcolorder(cons_gr_tasmin_sim.per, c('y_block', 'var', 'period'))

  cons_gr_tasmin_blk = data.table() # count by block
  for (block in unique(cons_gr_tasmin$y_block)) {
    cons_gr_tasmin_freq = transform(table(cut(cons_gr_tasmin_count[y_block == block, N], freqdays)))
    setDT(cons_gr_tasmin_freq)
    cons_gr_tasmin_freq[, y_block := block]
    cons_gr_tasmin_freq[, var := 'days_less.25.tasmin']
    cons_gr_tasmin_freq[, period := 'growing-season']
    names(cons_gr_tasmin_freq)[1] = 'Bin'
    setcolorder(cons_gr_tasmin_freq, c('y_block', 'var', 'period'))
    cons_gr_tasmin_blk = rbind(cons_gr_tasmin_blk, cons_gr_tasmin_freq)
  }
  # tasmax
  w_growing.sim.tasmax = transform(table(cut(w_growing$tasmax, freqtmax)))
  setDT(w_growing.sim.tasmax)
  w_growing.sim.tasmax[, y_block := 'sim-period']
  w_growing.sim.tasmax[, var := 'tasmax']
  w_growing.sim.tasmax[, period := 'growing-season']
  names(w_growing.sim.tasmax)[1] = 'Bin'
  setcolorder(w_growing.sim.tasmax, c('y_block', 'var', 'period'))

  w_growing.tasmax = data.table()
  for (block in unique(w_growing$y_block)) {
    tasmax_freq = transform(table(cut(w_growing[y_block == block, tasmax], freqtmax)))
    setDT(tasmax_freq)
    tasmax_freq[, y_block := block]
    tasmax_freq[, var := 'tasmax']
    tasmax_freq[, period := 'growing-season']
    names(tasmax_freq)[1] = 'Bin'
    setcolorder(tasmax_freq, c('y_block', 'var', 'period'))
    w_growing.tasmax = rbind(w_growing.tasmax, tasmax_freq)
  }
  # consecutive tasmax days > 75th percentile | one or more days
  grtmax.75 = quantile(w_growing$tasmax, probs = c(.75))
  cons_gr_tasmax75         = w_growing[, .SD[.N >= 1 & all(tasmax > grtmax.75)], rleid(tasmax > grtmax.75)]
  cons_gr_tasmax_count75   = cons_gr_tasmax75[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_gr_tasmax_sim.per75 = transform(table(cut(cons_gr_tasmax_count75$N, freqdays))) # count by sim period
  setDT(cons_gr_tasmax_sim.per75)
  cons_gr_tasmax_sim.per75[, y_block := 'sim-period']
  cons_gr_tasmax_sim.per75[, var := 'days_grtr.75.tasmax']
  cons_gr_tasmax_sim.per75[, period := 'growing-season']
  names(cons_gr_tasmax_sim.per75)[1] = 'Bin'
  setcolorder(cons_gr_tasmax_sim.per75, c('y_block', 'var', 'period'))

  cons_gr_tasmax_blk75 = data.table() # count by block
  for (block in unique(cons_gr_tasmax75$y_block)) {
    cons_gr_tasmax_freq = transform(table(cut(cons_gr_tasmax_count75[y_block == block, N], freqdays)))
    setDT(cons_gr_tasmax_freq)
    cons_gr_tasmax_freq[, y_block := block]
    cons_gr_tasmax_freq[, var := 'days_grtr.75.tasmax']
    cons_gr_tasmax_freq[, period := 'growing-season']
    names(cons_gr_tasmax_freq)[1] = 'Bin'
    setcolorder(cons_gr_tasmax_freq, c('y_block', 'var', 'period'))
    cons_gr_tasmax_blk75 = rbind(cons_gr_tasmax_blk75, cons_gr_tasmax_freq)
  }
  # consecutive tasmax days < 25th percentile | one or more days
  grtmax.25 = quantile(w_growing$tasmax, probs = c(.25))
  cons_gr_tasmax         = w_growing[, .SD[.N >= 1 & all(tasmax < grtmax.25)], rleid(tasmax < grtmax.25)]
  cons_gr_tasmax_count   = cons_gr_tasmax[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_gr_tasmax_sim.per = transform(table(cut(cons_gr_tasmax_count$N, freqdays))) # count by sim period
  setDT(cons_gr_tasmax_sim.per)
  cons_gr_tasmax_sim.per[, y_block := 'sim-period']
  cons_gr_tasmax_sim.per[, var := 'days_less.25.tasmax']
  cons_gr_tasmax_sim.per[, period := 'growing-season']
  names(cons_gr_tasmax_sim.per)[1] = 'Bin'
  setcolorder(cons_gr_tasmax_sim.per, c('y_block', 'var', 'period'))

  cons_gr_tasmax_blk = data.table() # count by block
  for (block in unique(cons_gr_tasmax$y_block)) {
    cons_gr_tasmax_freq = transform(table(cut(cons_gr_tasmax_count[y_block == block, N], freqdays)))
    setDT(cons_gr_tasmax_freq)
    cons_gr_tasmax_freq[, y_block := block]
    cons_gr_tasmax_freq[, var := 'days_less.25.tasmax']
    cons_gr_tasmax_freq[, period := 'growing-season']
    names(cons_gr_tasmax_freq)[1] = 'Bin'
    setcolorder(cons_gr_tasmax_freq, c('y_block', 'var', 'period'))
    cons_gr_tasmax_blk = rbind(cons_gr_tasmax_blk, cons_gr_tasmax_freq)
  }
  # rbind all histogram tables
  grhistogram.tbl = rbind(w_growing.sim.pr, w_growing.pr, w_growing.sim.tasmin, w_growing.tasmin,
                          w_growing.sim.tasmax, w_growing.tasmax, cons_gr_dry_sim.per, cons_gr_dry_blk,
                          cons_gr_dry_sim.per, cons_gr_pr_blk, cons_gr_tasmin_sim.per, cons_gr_tasmin_blk,
                          cons_gr_tasmax_sim.per, cons_gr_tasmax_blk, cons_gr_tasmin_sim.per75, cons_gr_tasmin_blk75,
                          cons_gr_tasmax_sim.per75, cons_gr_tasmax_blk75)
  grhistogram.tbl[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(grhistogram.tbl, c('gridid', 'y_block', 'gcm', 'ssp', 'period'))
  # Non-growing season
  # pr
  w_ngrowing.sim.pr = transform(table(cut(w_ngrowing$pr, freqpr)))
  setDT(w_ngrowing.sim.pr)
  w_ngrowing.sim.pr[, y_block := 'sim-period']
  w_ngrowing.sim.pr[, var := 'pr']
  w_ngrowing.sim.pr[, period := 'ngrowing-season']
  names(w_ngrowing.sim.pr)[1] = 'Bin'
  setcolorder(w_ngrowing.sim.pr, c('y_block', 'var', 'period'))

  w_ngrowing.pr = data.table()
  for (block in unique(w_ngrowing$y_block)) {
    pr_freq = transform(table(cut(w_ngrowing[y_block == block, pr], freqpr)))
    setDT(pr_freq)
    pr_freq[, y_block := block]
    pr_freq[, var := 'pr']
    pr_freq[, period := 'ngrowing-season']
    names(pr_freq)[1] = 'Bin'
    setcolorder(pr_freq, c('y_block', 'var', 'period'))
    w_ngrowing.pr = rbind(w_ngrowing.pr, pr_freq)
  }
  # consecutive pr days > 0 | one or more days
  cons_ngr_pr         = w_ngrowing[, .SD[.N >= 1 & all(pr > 0)], rleid(pr > 0)]
  cons_ngr_pr_count   = cons_ngr_pr[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_ngr_pr_sim.per = transform(table(cut(cons_ngr_pr_count$N, freqdays))) # count by sim period
  setDT(cons_ngr_pr_sim.per)
  cons_ngr_pr_sim.per[, y_block := 'sim-period']
  cons_ngr_pr_sim.per[, var := 'days_pr']
  cons_ngr_pr_sim.per[, period := 'ngrowing-season']
  names(cons_ngr_pr_sim.per)[1] = 'Bin'
  setcolorder(cons_ngr_pr_sim.per, c('y_block', 'var', 'period'))

  cons_ngr_pr_blk = data.table() # count by block
  for (block in unique(cons_ngr_pr$y_block)) {
    cons_ngr_pr_freq = transform(table(cut(cons_ngr_pr_count[y_block == block, N], freqdays)))
    setDT(cons_ngr_pr_freq)
    cons_ngr_pr_freq[, y_block := block]
    cons_ngr_pr_freq[, var := 'days_pr']
    cons_ngr_pr_freq[, period := 'ngrowing-season']
    names(cons_ngr_pr_freq)[1] = 'Bin'
    setcolorder(cons_ngr_pr_freq, c('y_block', 'var', 'period'))
    cons_ngr_pr_blk = rbind(cons_ngr_pr_blk, cons_ngr_pr_freq)
  }
  # consecutive dry days > 0 | one or more days
  cons_ngr_dry = w_ngrowing[, .SD[.N >= 1 & all(pr == 0)], rleid(pr == 0)]
  cons_ngr_dry_count   = cons_ngr_dry[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_ngr_dry_sim.per = transform(table(cut(cons_ngr_dry_count$N, freqdays))) # count by sim period
  setDT(cons_ngr_dry_sim.per)
  cons_ngr_dry_sim.per[, y_block := 'sim-period']
  cons_ngr_dry_sim.per[, var := 'days_dry']
  cons_ngr_dry_sim.per[, period := 'ngrowing-season']
  names(cons_ngr_dry_sim.per)[1] = 'Bin'
  setcolorder(cons_ngr_dry_sim.per, c('y_block', 'var', 'period'))

  cons_ngr_dry_blk = data.table() # count by block
  for (block in unique(cons_ngr_dry$y_block)) {
    cons_ngr_dry_freq = transform(table(cut(cons_ngr_dry_count[y_block == block, N], freqdays)))
    setDT(cons_ngr_dry_freq)
    cons_ngr_dry_freq[, y_block := block]
    cons_ngr_dry_freq[, var := 'days_dry']
    cons_ngr_dry_freq[, period := 'ngrowing-season']
    names(cons_ngr_dry_freq)[1] = 'Bin'
    setcolorder(cons_ngr_dry_freq, c('y_block', 'var', 'period'))
    cons_ngr_dry_blk = rbind(cons_ngr_dry_blk, cons_ngr_dry_freq)
  }
  # tasmin
  w_ngrowing.sim.tasmin = transform(table(cut(w_ngrowing$tasmin, freqtmin)))
  setDT(w_ngrowing.sim.tasmin)
  w_ngrowing.sim.tasmin[, y_block := 'sim-period']
  w_ngrowing.sim.tasmin[, var := 'tasmin']
  w_ngrowing.sim.tasmin[, period := 'ngrowing-season']
  names(w_ngrowing.sim.tasmin)[1] = 'Bin'
  setcolorder(w_ngrowing.sim.tasmin, c('y_block', 'var', 'period'))

  w_ngrowing.tasmin = data.table()
  for (block in unique(w_ngrowing$y_block)) {
    tasmin_freq = transform(table(cut(w_ngrowing[y_block == block, tasmin], freqtmin)))
    setDT(tasmin_freq)
    tasmin_freq[, y_block := block]
    tasmin_freq[, var := 'tasmin']
    tasmin_freq[, period := 'ngrowing-season']
    names(tasmin_freq)[1] = 'Bin'
    setcolorder(tasmin_freq, c('y_block', 'var', 'period'))
    w_ngrowing.tasmin = rbind(w_ngrowing.tasmin, tasmin_freq)
  }
  # consecutive tasmin days > 75th percentile | one or more days
  ngrtmin.75 = quantile(w_ngrowing$tasmin, probs = c(.75))
  cons_ngr_tasmin75         = w_ngrowing[, .SD[.N >= 1 & all(tasmin > ngrtmin.75)], rleid(tasmin > ngrtmin.75)]
  cons_ngr_tasmin_count75   = cons_ngr_tasmin75[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_ngr_tasmin_sim.per75 = transform(table(cut(cons_ngr_tasmin_count75$N, freqdays))) # count by sim period
  setDT(cons_ngr_tasmin_sim.per75)
  cons_ngr_tasmin_sim.per75[, y_block := 'sim-period']
  cons_ngr_tasmin_sim.per75[, var := 'days_ngrtr.75.tasmin']
  cons_ngr_tasmin_sim.per75[, period := 'ngrowing-season']
  names(cons_ngr_tasmin_sim.per75)[1] = 'Bin'
  setcolorder(cons_ngr_tasmin_sim.per75, c('y_block', 'var', 'period'))

  cons_ngr_tasmin_blk75 = data.table() # count by block
  for (block in unique(cons_ngr_tasmin75$y_block)) {
    cons_ngr_tasmin_freq = transform(table(cut(cons_ngr_tasmin_count75[y_block == block, N], freqdays)))
    setDT(cons_ngr_tasmin_freq)
    cons_ngr_tasmin_freq[, y_block := block]
    cons_ngr_tasmin_freq[, var := 'days_ngrtr.75.tasmin']
    cons_ngr_tasmin_freq[, period := 'ngrowing-season']
    names(cons_ngr_tasmin_freq)[1] = 'Bin'
    setcolorder(cons_ngr_tasmin_freq, c('y_block', 'var', 'period'))
    cons_ngr_tasmin_blk75 = rbind(cons_ngr_tasmin_blk75, cons_ngr_tasmin_freq)
  }
  # consecutive tasmin days < 25th percentile | one or more days
  ngrtmin.25 = quantile(w_ngrowing$tasmin, probs = c(.25))
  cons_ngr_tasmin         = w_ngrowing[, .SD[.N >= 1 & all(tasmin < ngrtmin.25)], rleid(tasmin < ngrtmin.25)]
  cons_ngr_tasmin_count   = cons_ngr_tasmin[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_ngr_tasmin_sim.per = transform(table(cut(cons_ngr_tasmin_count$N, freqdays))) # count by sim period
  setDT(cons_ngr_tasmin_sim.per)
  cons_ngr_tasmin_sim.per[, y_block := 'sim-period']
  cons_ngr_tasmin_sim.per[, var := 'days_less.25.tasmin']
  cons_ngr_tasmin_sim.per[, period := 'ngrowing-season']
  names(cons_ngr_tasmin_sim.per)[1] = 'Bin'
  setcolorder(cons_ngr_tasmin_sim.per, c('y_block', 'var', 'period'))

  cons_ngr_tasmin_blk = data.table() # count by block
  for (block in unique(cons_ngr_tasmin$y_block)) {
    cons_ngr_tasmin_freq = transform(table(cut(cons_ngr_tasmin_count[y_block == block, N], freqdays)))
    setDT(cons_ngr_tasmin_freq)
    cons_ngr_tasmin_freq[, y_block := block]
    cons_ngr_tasmin_freq[, var := 'days_less.25.tasmin']
    cons_ngr_tasmin_freq[, period := 'ngrowing-season']
    names(cons_ngr_tasmin_freq)[1] = 'Bin'
    setcolorder(cons_ngr_tasmin_freq, c('y_block', 'var', 'period'))
    cons_ngr_tasmin_blk = rbind(cons_ngr_tasmin_blk, cons_ngr_tasmin_freq)
  }
  # tasmax
  w_ngrowing.sim.tasmax = transform(table(cut(w_ngrowing$tasmax, freqtmax)))
  setDT(w_ngrowing.sim.tasmax)
  w_ngrowing.sim.tasmax[, y_block := 'sim-period']
  w_ngrowing.sim.tasmax[, var := 'tasmax']
  w_ngrowing.sim.tasmax[, period := 'ngrowing-season']
  names(w_ngrowing.sim.tasmax)[1] = 'Bin'
  setcolorder(w_ngrowing.sim.tasmax, c('y_block', 'var', 'period'))

  w_ngrowing.tasmax = data.table()
  for (block in unique(w_ngrowing$y_block)) {
    tasmax_freq = transform(table(cut(w_ngrowing[y_block == block, tasmax], freqtmax)))
    setDT(tasmax_freq)
    tasmax_freq[, y_block := block]
    tasmax_freq[, var := 'tasmax']
    tasmax_freq[, period := 'ngrowing-season']
    names(tasmax_freq)[1] = 'Bin'
    setcolorder(tasmax_freq, c('y_block', 'var', 'period'))
    w_ngrowing.tasmax = rbind(w_ngrowing.tasmax, tasmax_freq)
  }
  # consecutive tasmax days > 75th percentile | one or more days
  ngrtmax.75 = quantile(w_ngrowing$tasmax, probs = c(.75))
  cons_ngr_tasmax75         = w_ngrowing[, .SD[.N >= 1 & all(tasmax > ngrtmax.75)], rleid(tasmax > ngrtmax.75)]
  cons_ngr_tasmax_count75   = cons_ngr_tasmax75[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_ngr_tasmax_sim.per75 = transform(table(cut(cons_ngr_tasmax_count75$N, freqdays))) # count by sim period
  setDT(cons_ngr_tasmax_sim.per75)
  cons_ngr_tasmax_sim.per75[, y_block := 'sim-period']
  cons_ngr_tasmax_sim.per75[, var := 'days_grtr.75.tasmax']
  cons_ngr_tasmax_sim.per75[, period := 'ngrowing-season']
  names(cons_ngr_tasmax_sim.per75)[1] = 'Bin'
  setcolorder(cons_ngr_tasmax_sim.per75, c('y_block', 'var', 'period'))

  cons_ngr_tasmax_blk75 = data.table() # count by block
  for (block in unique(cons_ngr_tasmax75$y_block)) {
    cons_ngr_tasmax_freq = transform(table(cut(cons_ngr_tasmax_count75[y_block == block, N], freqdays)))
    setDT(cons_ngr_tasmax_freq)
    cons_ngr_tasmax_freq[, y_block := block]
    cons_ngr_tasmax_freq[, var := 'days_grtr.75.tasmax']
    cons_ngr_tasmax_freq[, period := 'ngrowing-season']
    names(cons_ngr_tasmax_freq)[1] = 'Bin'
    setcolorder(cons_ngr_tasmax_freq, c('y_block', 'var', 'period'))
    cons_ngr_tasmax_blk75 = rbind(cons_ngr_tasmax_blk75, cons_ngr_tasmax_freq)
  }
  # consecutive tasmax days < 25th percentile | one or more days
  ngrtmax.25 = quantile(w_ngrowing$tasmax, probs = c(.25))
  cons_ngr_tasmax         = w_ngrowing[, .SD[.N >= 1 & all(tasmax < ngrtmax.25)], rleid(tasmax < ngrtmax.25)]
  cons_ngr_tasmax_count   = cons_ngr_tasmax[, .N, keyby = .(rleid, y_block)] # count rleid
  cons_ngr_tasmax_sim.per = transform(table(cut(cons_ngr_tasmax_count$N, freqdays))) # count by sim period
  setDT(cons_ngr_tasmax_sim.per)
  cons_ngr_tasmax_sim.per[, y_block := 'sim-period']
  cons_ngr_tasmax_sim.per[, var := 'days_less.25.tasmax']
  cons_ngr_tasmax_sim.per[, period := 'ngrowing-season']
  names(cons_ngr_tasmax_sim.per)[1] = 'Bin'
  setcolorder(cons_ngr_tasmax_sim.per, c('y_block', 'var', 'period'))

  cons_ngr_tasmax_blk = data.table() # count by block
  for (block in unique(cons_ngr_tasmax$y_block)) {
    cons_ngr_tasmax_freq = transform(table(cut(cons_ngr_tasmax_count[y_block == block, N], freqdays)))
    setDT(cons_ngr_tasmax_freq)
    cons_ngr_tasmax_freq[, y_block := block]
    cons_ngr_tasmax_freq[, var := 'days_less.25.tasmax']
    cons_ngr_tasmax_freq[, period := 'ngrowing-season']
    names(cons_ngr_tasmax_freq)[1] = 'Bin'
    setcolorder(cons_ngr_tasmax_freq, c('y_block', 'var', 'period'))
    cons_ngr_tasmax_blk = rbind(cons_ngr_tasmax_blk, cons_ngr_tasmax_freq)
  }
  # rbind all histongram tables
  ngrhistogram.tbl = rbind(w_ngrowing.sim.pr, w_ngrowing.pr, w_ngrowing.sim.tasmin, w_ngrowing.tasmin,
                           w_ngrowing.sim.tasmax, w_ngrowing.tasmax, cons_ngr_dry_sim.per, cons_ngr_dry_blk,
                           cons_ngr_dry_sim.per, cons_ngr_pr_blk, cons_ngr_tasmin_sim.per, cons_ngr_tasmin_blk,
                           cons_ngr_tasmax_sim.per, cons_ngr_tasmax_blk, cons_ngr_tasmin_sim.per75, cons_ngr_tasmin_blk75,
                           cons_ngr_tasmax_sim.per75, cons_ngr_tasmax_blk75)
  ngrhistogram.tbl[, `:=` (gridid = .gridid, ssp = .ssp, gcm = .gcm)]
  setcolorder(ngrhistogram.tbl, c('gridid', 'y_block', 'gcm', 'ssp', 'period'))
  # bind gr and ngr histogram.tbl
  grhistogram.tbl  = rbind(grhistogram.tbl, ngrhistogram.tbl)
  grhistogram.tbl[, crop := .crop]
  setcolorder(grhistogram.tbl, c('gridid', 'y_block', 'gcm', 'ssp', 'crop'))
  # save
  w_growinghist_fname = paste0(pkg.env$out_path, '/', out.dir,'/weather_histogram-statistics_gr_ngr_season.csv')
  fwrite(grhistogram.tbl, w_growinghist_fname, append = TRUE)
}
