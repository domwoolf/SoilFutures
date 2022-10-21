pkg.env = NULL
.onLoad = function(...) {
  pkg.env <<- new.env(parent = emptyenv())
  assign('crop_types',   c('maiz', 'wwheat', 'swheat', 'soy'), pkg.env)
  assign('climate_vbls', c('tasmin', 'tasmax', 'tas', 'pr'), pkg.env)
  assign('ssps',         c('historical','ssp126', "ssp370"), pkg.env)
  assign('scenario',     c('conv', 'residue', 'ntill', 'ccg', 'ccl'), pkg.env)
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
      Please enter path to GIS files> "), pkg.env)
  }
}
