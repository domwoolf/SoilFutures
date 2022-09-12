# Schedule.R
#
# contains functions to construct DayCent schedule files from spatial data
#

#' Reads planting and harvest date from crop calendar rasters
read_crop_calendar = function(crop, cell) {

}



#' Create a DayCent Schedule file
#'
#' Used for side effect of writing a schedule file based on spatial data.
#'
#' This function creates a schedule file for a specified scenario and crop,
#' using spatial climate, crop calendar, crop management, and soil data.
#'
#' @param crop character, one of 'maize', 'wwheat', 'swheat', 'soy'
#' @param cell location of spatial data, cell number assumes all rasters in common form
#' @param scenario character, specifies scenario

#' @returns invisibly returns boolean indicating whether file was written succesfully.
create_sched = function(crop, cell, scenario, main_table) {
  crop_calendar = read_crop_calendar(crop, cell)
  N_or_S = c('N', 'S')[crop_calendar$plant > crop_calendar$harvest + 1L]
  sched_template = paste0(template_path, N_or_S, '_', scenario, '.sch')

  }
