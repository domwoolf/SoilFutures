pkg.env = NULL
.onLoad = function(...) {
  # assign('pkg.env', new.env(parent = emptyenv()), .GlobalEnv)
  pkg.env <<- new.env(parent = emptyenv())
  assign('crop_types', c('maize', 'wwheat', 'swheat', 'soy'), pkg.env)
  assign('gis_path', Sys.getenv('SOIL_FUTURES_GIS_PATH'), pkg.env)
  if (!isTRUE(nchar(pkg.env$gis_path))) {
    assign('gis_path', readline("Warning: SOIL_FUTURES_GIS_PATH environment variable not set. Please enter path to GIS files> "), pkg.env)
  }
  assign('out_path', Sys.getenv('SOIL_FUTURES_OUT_PATH'), pkg.env)
  if (!isTRUE(nchar(pkg.env$out_path))) {
    assign('out_path', readline("Warning: SOIL_FUTURES_OUT_PATH environment variable not set. Please enter path to GIS files> "), pkg.env)
  }
}
