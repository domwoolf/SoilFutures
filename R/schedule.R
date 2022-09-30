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
#' @import data.table
#' @import stringr
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

# for testing, delete below later
cell_data = fread(paste(pkg.env$out_path, "cell_data_table.csv", sep = "/")) # for testing
schedule_table = fread(paste(pkg.env$out_path, "SoilFutures","data-raw", "schedule_template.csv", sep = "/"))

create_sched = function(cell_data, schedule_table = copy(schedule_template), ssp, gcm, crop, scenario, weather_filename) {
  # variable definition
  start_year          = 2015
  end_year            = 2100
  cell_data           = cell_data[cell %in% c(cell),] # we only process a single cell's data
  schedule_path       = paste(pkg.env$out_path,'/', ssp,'_', gcm, sep = "")
  schedule_filename   = cell_data[1, paste(scenario, '_', crop, '_', cell, '.sch', sep = "")]
  block_name          = paste(crop, '_', scenario, sep = "")
  crop_cultivar       = crop.100_type # this will need to be added to the cell_data table
  scenario.sched      = scenario
  # cropping hemisphere (by planting and harvest date in Gregorian calendar)
  plant.date          = round(cell_data[variable %in% paste('plant_', str_to_title(crop), sep = ""), value])
  harvest.date        = round(cell_data[variable %in% paste('harvest_', str_to_title(crop), sep = ""), value])
  N_or_S              = fifelse(plant.date > harvest.date, 'S','N')

  # tillage value recode
  cell_data[variable %like% c('till'), variable := paste(c(pkg.env$crop_types[1], 'wheat', pkg.env$crop_types[4]), '_till', sep = "")]

  if (cell_data[variable %in% paste(crop,'_till', sep = ""), value] == 1 ||
      is.na(cell_data[variable %in% paste(crop,'_till', sep = ""), value])) {
    cult_opt = 'K'
  } else if (cell_data[variable %in% paste(crop,'_till', sep = ""), value] == 2||
             cell_data[variable %in% paste(crop,'_till', sep = ""), value] == 6) {
    cult_opt = 'G'
  } else if (cell_data[variable %in% paste(crop,'_till', sep = ""), value] == 4) {
    cult_opt = 'B'
  } else {
    cult_opt = 'J'
  }

  # crop schedule_table recode
  if (crop == pkg.env$crop_types[1] || crop == pkg.env$crop_types[4]) {
    crop.in.sched = "c-s"
  } else if (crop == pkg.env$crop_types[2]) {
    crop.in.sched = 'ww'
  } else {
    crop.in.sched = 'sw'
  }

  #create .sch header
  schedule_table_head = schedule_table[scenario %in% c('all') &
                                       ssp      %in% c('all') &
                                       `N-or_S` %in% c('all') &
                                       crop     %in% c('all')]

  schedule_table_head[, schedule := gsub('<fname>',               paste('site_', cell, '.100', sep = ""), schedule_table_head$schedule)]
  schedule_table_head[, schedule := gsub('<weather_file>',        weather_filename,                       schedule_table_head$schedule)]
  schedule_table_head[, schedule := gsub('<block_name>',          block_name,                             schedule_table_head$schedule)]
  schedule_table_head[, schedule := gsub('<start_year>',          start_year,                             schedule_table_head$schedule)]
  schedule_table_head[, schedule := gsub('<end_year>',            end_year,                               schedule_table_head$schedule)]
  schedule_table_head[, schedule := gsub('<crop_cultivar>',       crop_cultivar,                          schedule_table_head$schedule)]

  # if no-till remove pre-plant, post-harvest CULT events
  # spring wheat check condition for S. Hemi harvest (could span into two years)

  # create .sch block

  schedule_table_block = schedule_table[scenario   %in% c(scenario.sched) &
                                          ssp      %in% c('all')          &
                                          `N-or_S` %in% c(N_or_S)         &
                                          crop     %in% c(crop.in.sched)]
  schedule_table_block[, schedule := gsub('<plant_day>',           plant.date, schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<harvest_day>',         harvest.date, schedule_table_block$schedule)]
  # stopped here
  schedule_table_block[, schedule := gsub('<cult_day_postharvest>',cell_data$cult_day_postharvest)]
  schedule_table_block[, schedule := gsub('<harvest_day>',         cell_data$harvest_day)]
  schedule_table_block[, schedule := gsub('<crop_code>',           cell_data$crop_code)]
  schedule_table_block[, schedule := gsub('<cult_day_preharvest>', cell_data$cult_day_preharvest)]
  schedule_table_block[, schedule := gsub('<manure>',              cell_data$manure)]

  fwrite(schedule_table[, schedule], schedule_filename, quote = FALSE, col.names = FALSE)
  return(schedule_filename)
  }
