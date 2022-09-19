.onLoad = function() {
  crop_types <<- c('maize', 'wwheat', 'swheat', 'soy')
  gis_path <<- Sys.getenv('SOIL_FUTURES_GIS_PATH')
  if (!isTRUE(nchar(gis_path))) gis_path <<- readline("Warning: SOIL_FUTURES_GIS_PATH environment variable not set. Please enter path to GIS files> ")
  out_path <<- Sys.getenv('SOIL_FUTURES_OUT_PATH')
  if (!isTRUE(nchar(out_path))) out_path <<- readline("Warning: SOIL_FUTURES_OUT_PATH environment variable not set. Please enter base path for output files> ")
}
