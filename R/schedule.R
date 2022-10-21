# schedule.R
#
# contains functions to construct DayCent run files from spatial data
#
#' Create a crop.100 file
#'
#' Used for writing a crop.100 file based on Yang et al. (2022) ERL
#'
#' This function accepts parameter values from a csv file and creates a crop.100 for
#' all simulations for a crop type in a cell.
#' @import data.table
#'
#' @param cell_data multiple row data.table providing input data for the simulation.
#' @param cell unique cell value to be passed to function.

create_crop = function(cell_data, cell) {
  # get gridid, crop parameter data
  cell_crop_data = unique(cell_data[gridid == cell, .(gridid, crop, dc_cropname, RUETB, PPDF1, PPDF2, PPDF3, PPDF4,
                                     PLTMRF, FRTC1, DDBASE, KLIGHT, SLA, DDLAIMX)])

  # create parameter vectors
  param.vector   = c("<value_crop>      <crop_name>","<value_RUETB>     RUETB",
                   "<value_PPDF1>     PPDF(1)",    "<value_PPDF2>     PPDF(2)",
                   "<value_PPDF3>     PPDF(3)",    "<value_PPDF4>     PPDF(4)",
                   "0.0               BIOFLG",     "1800.0            BIOK5",       "<value_PLTMRF>               PLTMRF",
                   "150.0             FULCAN",     "5                 FRTCINDX",    "<value_FRTC1>               FRTC(1)",
                   "0.1               FRTC(2)",    "90.0              FRTC(3)",     "0.1               FRTC(4)",
                   "0.1               FRTC(5)",    "0.3               CFRTCN(1)",   "0.25              CFRTCN(2)",
                   "0.5               CFRTCW(1)",  "0.1               CFRTCW(2)",   "700.0             BIOMAX",
                   "15.0              PRAMN(1,1)", "150.0             PRAMN(2,1)",  "190.0             PRAMN(3,1)",
                   "62.5              PRAMN(1,2)", "150.0             PRAMN(2,2)",  "150.0             PRAMN(3,2)",
                   "40.0              PRAMX(1,1)", "230.0             PRAMX(2,1)",  "230.0             PRAMX(3,1)",
                   "125.0             PRAMX(1,2)", "230.0             PRAMX(2,2)",  "230.0             PRAMX(3,2)",
                   "45.0              PRBMN(1,1)", "390.0             PRBMN(2,1)",  "340.0             PRBMN(3,1)",
                   "0.0               PRBMN(1,2)", "0.0               PRBMN(2,2)",  "0.0               PRBMN(3,2)",
                   "60.0              PRBMX(1,1)", "420.0             PRBMX(2,1)",  "420.0             PRBMX(3,1)",
                   "0.0               PRBMX(1,2)", "0.0               PRBMX(2,2)",  "0.0               PRBMX(3,2)",
                   "0.12              FLIGNI(1,1)","0.0               FLIGNI(2,1)", "0.06              FLIGNI(1,2)",
                   "0.0               FLIGNI(2,2)","0.06              FLIGNI(1,3)", "0.0               FLIGNI(2,3)",
                   "0.55              HIMAX",      "0.7               HIWSF",       "1.0               HIMON(1)",
                   "0.0               HIMON(2)",   "0.75              EFRGRN(1)",   "0.6               EFRGRN(2)",
                   "0.6               EFRGRN(3)",  "0.04              VLOSSP",      "0.0               FSDETH(1)",
                   "0.0               FSDETH(2)",  "0.0               FSDETH(3)",   "500.0             FSDETH(4)",
                   "0.1               FALLRT",     "0.05              RDRJ",        "0.05              RDRM",      "0.14              RDSRFC",
                   "2.0               RTDTMP",     "0.0               CRPRTF(1)",   "0.0               CRPRTF(2)",
                   "0.0               CRPRTF(3)",  "0.05              MRTFRAC",     "0.0               SNFXMX(1)",
                   "-15.0             DEL13C",     "1.0               CO2IPR(1)",   "0.82              CO2ITR(1)",
                   "1.0               CO2ICE(1,1,1)", "1.0            CO2ICE(1,1,2)","1.0              CO2ICE(1,1,3)",
                   "1.0               CO2ICE(1,2,1)", "1.0            CO2ICE(1,2,2)","1.0              CO2ICE(1,2,3)",
                   "1.0               CO2IRS(1)",  "0.10000           CKMRSPMX(1)", "0.15000           CKMRSPMX(2)",
                   "0.05000           CKMRSPMX(3)","0.00000           CMRSPNPP(1)", "0.00000           CMRSPNPP(2)",
                   "1.25000           CMRSPNPP(3)","1.00000           CMRSPNPP(4)", "4.00000           CMRSPNPP(5)",
                   "1.50000           CMRSPNPP(6)","0.23000           CGRESP(1)",   "0.23000           CGRESP(2)",
                   "0.23000           CGRESP(3)",  "0.25000           NO3PREF(1)",  "6.00000           CLAYPG",
                   "0.50000           CMIX ",      "-13.000           TMPGERM",     "<value_DDBASE>    DDBASE",
                   "-3.5              TMPKILL",    "10                BASETEMP",    "30                BASETEMP(2)",
                   "-1	              BASETEMP(3)","-1	              BASETEMP(4)", "100               MNDDHRV",
                   "550               MXDDHRV",    "120.0             CURGDYS",     "0.5               CLSGRES",
                   "0.12              CMXTURN",    "1.0               NPP2CS(1)",   "2.0               CAFUE",
                   "1.40              EMAX",       "1.2 	            KCET",        "<value_KLIGHT>    KLIGHT",
                   "<value_SLA>       SLA",        "0.9               LEAFCL",      "0.9               LEAFEMERG",
                   "0.25              LEAFMX",     "0.02              LEAFPM",      "80                DDEMERG",
                   "<value_DDLAIMX>   DDLAIMX",    "-1		            DDPPSTART",   "-1		             DDPPEND",
                   "-1		            PPTYPE",     "-1		            PPCRITICAL",  "-1		             PPSNST",
                   "-1		            VERNALSNST", "0.0               LUXEUPF(1)",  "0.0               LUXEUPF(2)",
                   "0.0               LUXEUPF(3)", "0.0               CSTGEUPF(1)", "0.0               CSTGEUPF(2)",
                   "0.0               CSTGEUPF(3)","0                 CSTGDYS",     "1.0               CSTGA2DRAT")

  # create crop.100 DT, replace values
  crop.100 = data.table(crop.100 = param.vector)
  crop.100[, crop.100 := gsub('<value_crop>',   cell_crop_data[,dc_cropname], crop.100)]
  crop.100[, crop.100 := gsub('<crop_name>',    cell_crop_data[,crop],        crop.100)]
  crop.100[, crop.100 := gsub('<value_RUETB>',  cell_crop_data[,RUETB],       crop.100)]
  crop.100[, crop.100 := gsub('<value_PPDF1>',  cell_crop_data[,PPDF1],       crop.100)]
  crop.100[, crop.100 := gsub('<value_PPDF2>',  cell_crop_data[,PPDF2],       crop.100)]
  crop.100[, crop.100 := gsub('<value_PPDF3>',  cell_crop_data[,PPDF3],       crop.100)]
  crop.100[, crop.100 := gsub('<value_PPDF4>',  cell_crop_data[,PPDF4],       crop.100)]
  crop.100[, crop.100 := gsub('<value_PLTMRF>', cell_crop_data[,PLTMRF],      crop.100)]
  crop.100[, crop.100 := gsub('<value_FRTC1>',  cell_crop_data[,FRTC1],       crop.100)]
  crop.100[, crop.100 := gsub('<value_DDBASE>', cell_crop_data[,DDBASE],      crop.100)]
  crop.100[, crop.100 := gsub('<value_KLIGHT>', cell_crop_data[,KLIGHT],      crop.100)]
  crop.100[, crop.100 := gsub('<value_SLA>',    cell_crop_data[,SLA],         crop.100)]
  crop.100[, crop.100 := gsub('<value_DDLAIMX>',cell_crop_data[,PPDF4],       crop.100)]

  fwrite(crop.100, paste(pkg.env$out_path, '/crop.100', sep = ''), quote = FALSE, col.names = FALSE)
  return(crop.100)
}
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

create_omad = function(cell_data, cell){
  # get gridid, omad data
  cell_omad_data = unique(cell_data[gridid == cell, .(gridid, crop, orgN.amt, orgCN.ratio)])

  # create parameter vectors
  param.vector     = c("<value_name>    CELL-EVENT",
                        "1               'OMADTYP'",
                        "<value_ASTGC>   'ASTGC'",
                        "0.0             'ASTLBL'",
                        "0.13            'ASTLIG'",
                        "<value_ASTREC1> 'ASTREC(1)'",
                        "300.0           'ASTREC(2)'",
                        "300.0           'ASTREC(3)'")
  if(cell_omad_data[,orgCN.ratio] > 0) {
    # create omad.100 DT, replace values
    omad.100 = data.table(omad.100 = param.vector)
    omad.100[, omad.100 := gsub('<value_name>',   cell_omad_data[,gridid],                                  omad.100)]
    omad.100[, omad.100 := gsub('<value_ASTREC1>',cell_omad_data[,orgCN.ratio],                             omad.100)]
    omad.100[, omad.100 := gsub('<value_ASTGC>',  (cell_omad_data[,orgCN.ratio]*cell_omad_data[,orgN.amt]), omad.100)]

    fwrite(omad.100, paste(pkg.env$out_path, '/omad.100', sep = ''), quote = FALSE, col.names = FALSE)
    return(omad.100)
  } else {
    print("No omad.100 returned because C:N reported as 0.")
  }
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
create_sched = function(cell_data, schedule_table = copy(schedule_template), cell, ssp, gcm, crop, scenario, start.yr, end.yr, weather_filename) {
  # variable definition
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

  # bind header, block
  fwrite(schedule_table[, schedule], schedule_filename, quote = FALSE, col.names = FALSE)
  return(schedule_filename)
  }
