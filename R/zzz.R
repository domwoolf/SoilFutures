pkg.env = NULL
.onLoad = function(...) {
  pkg.env <<- new.env(parent = emptyenv())
  assign('crop_types',   c('maiz', 'wwht', 'swht', 'soyb'), pkg.env)
  assign('climate_vbls', c('tasmin', 'tasmax', 'tas', 'pr'), pkg.env)
  assign('ssps',         c('historical','ssp126', "ssp370"), pkg.env)
  assign('scenario',     c('conv', 'res', 'ntill', 'ccg', 'ccl', 'ccg-ntill', 'ccl-ntill','rewild'), pkg.env)
  assign('start_year',   c(2016, 1996), pkg.env)
  assign('end_year',     c(2100, 2008), pkg.env)
  assign('irr',          c(0, 1),       pkg.env)
  # cat(paste('SOIL_FUTURES_GIS_PATH =', Sys.getenv('SOIL_FUTURES_GIS_PATH'), '\n))
  assign('gis_path', Sys.getenv('SOIL_FUTURES_GIS_PATH'), pkg.env)
  if (isFALSE(nchar(pkg.env$gis_path))) {
    assign('gis_path', readline(
      "Warning: SOIL_FUTURES_GIS_PATH environment variable not set.
      Please enter path to GIS files> "), pkg.env)
  }
  # cat(paste('SOIL_FUTURES_OUT_PATH =', Sys.getenv('SOIL_FUTURES_OUT_PATH'), '\n))
  assign('out_path', Sys.getenv('SOIL_FUTURES_OUT_PATH'), pkg.env)
  if (isFALSE(nchar(pkg.env$out_path))) {
    assign('out_path', readline(
      "Warning: SOIL_FUTURES_OUT_PATH environment variable not set.
      Please enter path to OUT files> "), pkg.env)
  }
  # cat(paste('SOIL_FUTURES_TMP_PATH =', Sys.getenv('SOIL_FUTURES_TMP_PATH'), '\n))
  assign('tmp_path', Sys.getenv('SOIL_FUTURES_TMP_PATH'), pkg.env)
  if (isFALSE(nchar(pkg.env$tmp_path))) {
    assign('tmp_path', readline(
      "Warning: SOIL_FUTURES_TMP_PATH environment variable not set.
      Please enter path to TMP files> "), pkg.env)
  }
}
