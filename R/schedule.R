# schedule.R
#
# contains functions to construct DayCent schedule files from spatial data
#
#' Create an omad.100 file
#'
#' Used for writing an omad.100 file based on spatial data.
#'
#' This function modifies the omad.100 file by creating a new event entry for a cell
#' using spatial manure-N and manure-CN data.
#'
#'@import data.table
#'
#'@param omad.100 DayCent input file with organic matter events
#'@param cell_data multiple row data.table providing input data for the simulation.

# for testing, delete below later
cell_data = fread(paste(pkg.env$out_path, "cell_data_table.csv", sep = "/")) # for testing
omad.100 = fread('/home/shelby/Documents/daycent/dev_30cmLAI4RriceGrass/input_files/omad.100', sep = "", header = FALSE)

create_omad = function(omad.100, cell_data){
  start_year     = 2015
  omad.cell      = paste(cell,'_mn', sep='')     # name of omad event for cell
  cell_data      = cell_data[cell %in% c(cell),] # we only process a single cell's data

  #extract cell data
  manureN.cell   = round(cell_data[variable %in% start_year, value]/10, digits = 1) #g m-2 conversion
  ASTREC1.cell   = round(cell_data[variable %in% 'weighted_C.N', value], digits = 1)
  ASTGC.cell     = round(manureN.cell*ASTREC1.cell, digits = 1)
  # add NA check here?

  omad_event     = omad.100[1:8,] # copy event structure
  omad_event[, V1 := gsub('^M',           omad.cell,    omad_event$V1)]
  omad_event[, V1 := gsub('STRAW-MANURE', 'CELL-EVENT', omad_event$V1)]
  omad_event[, V1 := gsub('100.0',        ASTGC.cell,   omad_event$V1)]
  omad_event[, V1 := gsub('30.0',         ASTREC1.cell, omad_event$V1)]

  omad.100.cell  = rbind(omad_event, omad.100)

  fwrite(omad.100.cell, paste(pkg.env$out_path, '/omad.100', sep = ''), quote = FALSE, col.names = FALSE)
  return(omad.100.cell)
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
#' @param cell_data multiple row data.table providing input data for the simulation.
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
  pre.harvest.cult    = 14
  post.harvest.cult   = 30
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

  # fertilizer value recode
  cell_data[variable %like% c(paste(start_year, '_', sep = "")), variable := paste(start_year, '_', c(pkg.env$crop_types[1], pkg.env$crop_types[4], 'wheat'), sep = "")]
  fertN.values = cell_data[variable %like% c(paste(start_year, '_', sep = "")),]
  fertN.match  = grep(crop, fertN.values$variable, value = TRUE)
  fertN.amt    = ifelse(fertN.values[variable %in% fertN.match, value] > 0, round((fertN.values[variable %in% fertN.match, value])/10, digits = 1), 0) # convert to g m-2 (currently kg ha)

  # res.rtrn
  # since narrow range of values --> create table of names to be read into this function?
  # modify harv.100 in advance (no need to keep overwriting)
  # match value from cell_data to the harv_code?
  # could be a similar table to the schedule_table (look up by scenario type)

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

  # spring wheat check condition for S. Hemi harvest (could span into two years)

  # create .sch block
  # what about for scenarios? eg the cult_day_postharvest is different if it is cover crop, wheat
  schedule_table_block = schedule_table[scenario   %in% c(scenario.sched) &
                                          ssp      %in% c('all')          &
                                          `N-or_S` %in% c(N_or_S)         &
                                          crop     %in% c(crop.in.sched)]
  schedule_table_block[, schedule := gsub('<plant_day>',            plant.date,                                schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<harvest_day>',          harvest.date,                              schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<cult_day_preharvest>',  (plant.date - pre.harvest.cult),           schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<cult_day_postharvest>', (harvest.date + post.harvest.cult),        schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<cult-opt>',             cult_opt,                                  schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<crop_cultivar>',        crop_cultivar,                             schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<manure>',               omad.cell,                                 schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<fert-amt>',             paste('(',fertN.amt, 'N,1.0F)', sep = ""), schedule_table_block$schedule)]
  schedule_table_block[, schedule := gsub('<res-amt>',              res.rtrn,                                  schedule_table_block$schedule)]
  # how to handle NA cases for fertilizer, manure? also need checks for plant/harv dates
  # if no-till remove pre-plant, post-harvest CULT events (add ifelse here)

  # bind header, block; drop all cols except schedule, remove header (looks like below dowing this)
  fwrite(schedule_table[, schedule], schedule_filename, quote = FALSE, col.names = FALSE)
  return(schedule_filename)
  }
