# Provides:
#  - Download daily gridded cmip6 climate projections
#  - For data source, see https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6
#  - For each GCM X SSP X variable convert to geotiff with one layer per day (Gregorian) through to 2100
#  - Aggregates to 0.5 degree resolution
# Dominic Woolf 7/4/22

#' @import data.table
#' @import terra
#' @import lubridate

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

extract_weather = function (

)

  # --------------------------------------------------------------------------
# Subset only the ssp and variables required
# --------------------------------------------------------------------------
climate_vbls = c('tasmin', 'tasmax', 'tas', 'pr')
ssps = c('ssp126', "ssp370")
index = index[ssp %in% ssps & vbl %in% vbls]

# --------------------------------------------------------------------------
# sanity checks
# --------------------------------------------------------------------------
stopifnot(
  # only one model run provided per gcm
  index[, length(unique(run)), by = gcm][, all(V1 == 1)],
  # only daily data
  index[, all(timestep == 'day')],
  # only keep ssp x gcm combinations that provide all variables
  index[, uniqueN(vbls), by = .(gcm, ssp)][, all(V1==length(vbls))]
)

# --------------------------------------------------------------------------
# Tidy up index
# --------------------------------------------------------------------------
index[, c('x', 'timestep', 'run') := NULL]
setkey(index, gcm, ssp, vbl, year)
gcms = index[, unique(gcm)]

# server url provided in the NASA index file does not work - replace with updated url:
bad.server = 'https://ds.nccs.nasa.gov/thredds2/fileServer/AMES/NEX/GDDP-CMIP6'
good.server = 'https://portal.nccs.nasa.gov/datashare/nexgddp_cmip6'
index[, fileUrl := gsub(bad.server, good.server, fileUrl)]

# add output type, encoding, and scaling factors, based on variable
index[, ot    := ifelse(vbl=='pr', '-ot UInt16', '-ot Int16')]
index[, scale := ifelse(vbl=='pr',
                        '-scale 0 0.7585069 0 65535',    # precip multiplied by 86,400 to convert to mm/day
                        '-scale 0 373.15 -2731.5 1000')] # temp converted from K to C and scaling factor of 10
index[, co  := '-co COMPRESS=ZSTD -co PREDICTOR=2']
index[, out := gsub('nc', 'tif', file.name)]

# --------------------------------------------------------------------------
# for each gcm, ssp and variable download the rasters and convert to geotiff
# --------------------------------------------------------------------------
if (testing) {
  gcms = gcms[1]; ssps=ssps[1]; vbls=vbls[1]
  .gcm = gcms[1]; .ssp=ssps[1]; .vbl=vbls[1]
}
for (.gcm in gcms) {
  for (.ssp in ssps) {
    for (.vbl in vbls) {
      print(paste(.gcm, .ssp, .vbl))
      files.to.get = index[gcm==.gcm & ssp==.ssp & vbl==.vbl]
      if (testing) files.to.get = head(files.to.get) # only do a few years for validating the script
      # download and convert to geotiff
      files.to.get[, {
        print(paste('Downloading', fileUrl, 'to', file.name, 'started at', Sys.time()))
        if (!file.exists(file.name)) download.file(fileUrl, file.name,
                                                   method = 'wget', quiet = TRUE,
                                                   extra="--random-wait --retry-on-http-error=503")
        cmd = paste('gdal_translate ', ot, scale, co, file.name, out)
        system(cmd)
        file.remove(file.name)
      }, by=fileUrl]
      r.list = lapply(files.to.get$out, rast)
      files.to.get[, Nlayer := sapply(r.list, nlyr)]

      # if there is mismatch between number of layers in a file, and number of days in year
      # then we insert N additional layers at year end corresponding to the last N layers of that year's data
      where.to.insert = files.to.get[, which(Nlayer < days.in.year)]
      what.to.insert  = lapply(where.to.insert, function (i) {
        r.list[[i]] [[ files.to.get[i, tail(seq_len(Nlayer), days.in.year - Nlayer)] ]]
      })
      where.to.insert = where.to.insert + seq_along(where.to.insert) - 1L # we insert items incrementally, so the index increments each time
      for (i in seq_along(where.to.insert)) {
        r.list = append(r.list, what.to.insert[[i]], where.to.insert[i])
      }
      r = rast(r.list)
      r = aggregate(r, fact=2, fun="mean") # from 0.25 to 0.5 degrees
      outfile = paste0(.gcm, '_', .ssp, '_', .vbl, '.tif')
      datatype = files.to.get[1, c('INT2U', 'INT2S')[match(ot, c('-ot UInt16', '-ot Int16'))] ]
      gdal.co  = c('COMPRESS=ZSTD', 'PREDICTOR=2')
      r = rotate(r, filename = outfile, datatype = datatype, gdal = gdal.co)

      # tidy up - remove temporary files
      file.remove(files.to.get$out)
      file.remove(paste0(files.to.get$out, '.aux.xml'))
    }
  }
}
