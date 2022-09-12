wwheat = function(calendar, mat, map, N_or_S) {
  gsd = # growing season days
    plant_date = calendar$plant
  harv_date  = calendar$harvest
  tot.days   =

    has_winter = ifelse(N_or_S == 'N', plant_date + )
  ifelse(between(tasmin, -10, 7) & (tot.days > 150) & (gsd >= 335))
}

create_main_table = function(gis_path) {
  # crop_area      = rast(paste(gis_path, 'spam',      'area.tif',     sep='/'))
  # crop_calendar  = rast(paste(gis_path, 'sacks',     'calendar.tif', sep='/'))
  # tillage        = rast(paste(gis_path, 'tillage',   'tillage.tif',  sep='/'))
  # min_N          = rast(paste(gis_path, 'fert',      'fert.tif',     sep='/'))
  # org_N          = rast(paste(gis_path, 'fert',      'manure.tif',   sep='/'))
  # org_CN         = rast(paste(gis_path, 'fert',      'manureCN.tif', sep='/'))
  # residue_return = rast(paste(gis_path, 'residue',   'residue.tif',  sep='/'))
  # BD             = rast(paste(gis_path, 'soilgrids', 'BD.tif',       sep='/'))
  # clay           = rast(paste(gis_path, 'soilgrids', 'clay.tif',     sep='/'))
  # pH             = rast(paste(gis_path, 'soilgrids', 'pH.tif',       sep='/'))
  # sand           = rast(paste(gis_path, 'soilgrids', 'sand.tif',     sep='/'))
  # SOC            = rast(paste(gis_path, 'soilgrids', 'SOC.tif',      sep='/'))

  # following makes a dummy example dataset for development:
  set.seed(123)
  n=20
  main_table = data.table(
    crop           = rep(crop_types)
    cell           = 1:n,
    crop_area      = runif(n),
    plant_date     =
      harv_date  =
      tillage        =
      min_N          =
      org_N          =
      org_CN         =
      residue_return =
      BD             =
      clay           =
      pH             =
      sand           =
      SOC            =
  )
}
