pkg.env = NULL
.onLoad = function(...) {
  pkg.env <<- new.env(parent = emptyenv())
  assign('crop_types',   c('maize', 'wwheat', 'swheat', 'soy'), pkg.env)
  assign('climate_vbls', c('tasmin', 'tasmax', 'tas', 'pr'), pkg.env)
  assign('ssps',         c('ssp126', "ssp370"), pkg.env)
  assign('gis_path', Sys.getenv('SOIL_FUTURES_GIS_PATH'), pkg.env)
  if (isFALSE(nchar(pkg.env$gis_path))) {
    assign('gis_path', readline(
      "Warning: SOIL_FUTURES_GIS_PATH environment variable not set.
      Please enter path to GIS files> "), pkg.env)
  }
  assign('out_path', Sys.getenv('SOIL_FUTURES_OUT_PATH'), pkg.env)
  if (isFALSE(nchar(pkg.env$out_path))) {
    assign('out_path', readline(
      "Warning: SOIL_FUTURES_OUT_PATH environment variable not set.
      Please enter path to GIS files> "), pkg.env)
  }
}
