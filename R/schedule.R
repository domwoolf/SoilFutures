# schedule.R
#
# contains functions to construct DayCent schedule files from spatial data
#
#' Create an omad.100 file
#'
#' Used for writing an omad.100 file based on spatial data.
#'
#' This function modifies the omad.100 file by creating a new entry for a cell
#' using spatial manure-N and manure-CN data.
#'
create_omad = function(){

}

#' Create a DayCent Schedule file
#'
#' Used for writing a schedule file based on spatial data.
#'
#' This function creates a schedule file for a specified scenario and crop,
#' using spatial climate, crop calendar, and crop management data.
#'
#' @param cell_data single row data.table providing input data for the simulation.
#'  If more than one row is provided, only first will be used
#' @param schedule_table data.table providing template of schedule file.
#' @param ssp character, specifies scenario
#' @param gcm character, specifies scenario
#' @param crop character, one of 'maize', 'wwheat', 'swheat', 'soy'
#' @param scenario character, specifies scenario

#' @returns invisibly returns boolean indicating whether file was written successfully.
#' @export

cell_data = fread(paste(pkg.env$out_path, "cell_data_table.csv", sep = "/")) # for testing

create_sched = function(cell_data, schedule_table = copy(schedule_template), ssp, gcm, crop, scenario, weather_filename) {
  cell_data = cell_data[[1,1]] # we only process a single cell's data
  schedule_path = paste(pkg.env$out_path, ssp, gcm)
  schedule_filename = cell_data[1, paste(scenario, '_', crop, '_', cell, '.sch')]
  N_or_S = c('N', 'S')[cell_data$plant > cell_data$harvest + 1L] # determine "cropping hemisphere" by whether planting date comes before or after harvest date in Gregorian calendar
  schedule_table = schedule_table[scenario %in% c(scenario,           'all') &
                                  ssp      %in% c(ssp,                'all') &
                                  N_or_S   %in% c(N_or_S,             'all') &
                                  crop     %in% c(crop,               'all')]
  #<block name>
  #<start_year>
  #<end_year>

  # change tillage value from number to letter
  # need to add in checks for tillage = X or whatever value it is (for no till cult B deleted)
  # spring wheat check condition for S. Hemi harvest (could span into two years)
  schedule_table[, schedule := gsub('<fname>',               schedule_filename)]
  schedule_table[, schedule := gsub('<weather_file>',        weather_filename)]

  # grab filtered table here based on function input for 'scenario', 'N or S'
  schedule_table[, schedule := gsub('<crop_code>',           cell_data$crop_code)]
  schedule_table[, schedule := gsub('<cult_day_preharvest>', cell_data$cult_day_preharvest)]
  schedule_table[, schedule := gsub('<manure>',              cell_data$manure)]
  schedule_table[, schedule := gsub('<plant_day>',           cell_data$plant_day)]
  schedule_table[, schedule := gsub('<harvest_day>',         cell_data$harvest_day)]
  schedule_table[, schedule := gsub('<cult_day_postharvest>',cell_data$cult_day_postharvest)]
  schedule_table[, schedule := gsub('<harvest_day>',         cell_data$harvest_day)]

  fwrite(schedule_table[, schedule], schedule_filename, quote = FALSE, col.names = FALSE)
  return(schedule_filename)
  }
