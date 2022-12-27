# schedule.R
#
# contains functions to construct DayCent run files from spatial data
#
#' Create a crop.100 file
#'
#' Used for writing a crop.100 file based on Yang et al. (2022) ERL
#'
#' This function accepts parameter values from a csv file and creates a crop.100 for
#' all simulations for a crop type in a cell. N.B. crop.100 written with FRTCINX at 7 (forced harvest).
#' @import data.table
#'
#' @param cell_data multiple row data.table providing input data for the simulation.
#' @param cell unique cell value to be passed to function.
#' @param cover_crop table template of cover crop parameters to include in the crop.100.
#' @param tmp.dir directory to create in tmp.path, set with arg[1]
#' @export
create_crop = function(cell_data = copy(cell_data), cell, cover_crop = copy(cover_crop_template), tmp.dir) {
  # need to update for different crops - consider different functions...
  # be sure to add in rewild crops/trees to template file
  # get gridid, crop parameter data
  cell_crop_data = unique(cell_data[gridid == cell, .(gridid, crop, dc_cropname, RUETB, PPDF1, PPDF2, PPDF3, PPDF4,
                                     PLTMRF, FRTC1, DDBASE, KLIGHT, SLA, DDLAIMX)])

  # create parameter vectors
  param.vector   = c("<value_crop>      <crop_name>","<value_RUETB>     RUETB",
                   "<value_PPDF1>     PPDF(1)",    "<value_PPDF2>     PPDF(2)",
                   "<value_PPDF3>     PPDF(3)",    "<value_PPDF4>     PPDF(4)",
                   "0.0               BIOFLG",     "1800.0            BIOK5",       "<value_PLTMRF>               PLTMRF",
                   "150.0             FULCAN",     "7.0                 FRTCINDX",    "<value_FRTC1>               FRTC(1)",
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
  # add index matching for crop type
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
  crop.100[, crop.100 := gsub('<value_DDLAIMX>',cell_crop_data[,DDLAIMX],     crop.100)]

  # read in cover_crop.100 to append to C6_00s for gridid
  colnames(cover_crop) = 'crop.100'
  crop.100             = rbind(crop.100, cover_crop)

  fwrite(crop.100, paste(pkg.env$tmp_path, tmp.dir,'crop.100', sep = '/'), quote = FALSE, col.names = FALSE)
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
#'@param cell_data multiple row data.table providing input data for the simulation.
#'@param cell unique cell value to be passed to function.
#'@param tmp.dir directory to create in tmp.path, set with arg[1]
#'@export
create_omad = function(cell_data = cell_data, cell, tmp.dir){
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
    # create omad.100 DT, replace values
    omad.100 = data.table(omad.100 = param.vector)
    omad.100[, omad.100 := gsub('<value_name>',   'O_cell',                                                 omad.100)]
    omad.100[, omad.100 := gsub('<value_ASTREC1>',cell_omad_data[,orgCN.ratio],                             omad.100)]
    omad.100[, omad.100 := gsub('<value_ASTGC>',  (cell_omad_data[,orgCN.ratio]*cell_omad_data[,orgN.amt]), omad.100)]

    fwrite(omad.100, paste(pkg.env$tmp_path, tmp.dir,'omad.100', sep = '/'), quote = FALSE, col.names = FALSE)
    return(omad.100)
}

#' Create a DayCent Schedule file for CSU extended simulations
#'
#' Used for writing a schedule file based on spatial data and CSU spin-ups.
#'
#' This function creates a schedule file for a specified scenario and crop,
#' using spatial climate, crop calendar, and crop management data.
#'
#' @import data.table
#' @import stringr
#'
#' @param cell_data multiple row data.table providing input data for the simulation.
#' @param schedule_table data.table providing template of schedule file.
#' @param .gridid unique cell value to be passed to function.
#' @param .ssp character, specifies scenario
#' @param .gcm character, specifies scenario
#' @param .crop character, one of 'maiz', 'wwheat', 'swheat', 'soy'
#' @param .scenario character, specifies scenario
#' @param .irr binary, specifies irrigation level
#' @param start_yr integer, specifies start year of simulation
#' @param end_year integer, specifies end year of simulation
#' @param weather_fname name of input weather file
#' @param tmp.dir directory to create in tmp.path, set with arg[1]

#' @returns invisibly returns boolean indicating whether file was written successfully.
#' @export
create_csu_sched = function(cell_data, schedule_table = copy(schedule_template), .gridid, .ssp, .gcm, .crop, .scenario, .irr, start_year, end_year, weather_fname, tmp.dir) {
  # variable definition
  cell_sch_data       = cell_data
  schedule_path       = paste(pkg.env$tmp_path, tmp.dir, sep = '/')
  schedule_filename   = cell_sch_data[1, paste(.scenario, '_', .crop, '_irr',.irr, '_', .gridid, '.sch', sep = "")]

  # add ifelse here...
  # look-up separately extracted DT with schl file info based on the crop, scenario, eq_schl_name
  # rationale is the two sequences of files
  # need to select proper one and rebuild to match simulation...
  block_name          = cell_sch_data[1, paste(.scenario, '_', .crop, '_',.irr, sep = "")]
  crop_cultivar       = cell_sch_data[1, paste(dc_cropname, sep = "")]

  # cropping hemisphere (by planting and harvest date in Gregorian calendar)
  plant.date          = cell_sch_data[,plant.date]
  harvest.date        = cell_sch_data[,harvest.date]
  crop_hemi           = fifelse(plant.date > harvest.date, 'S','N') # this should catch imperfect 'N' or 'S' assignments

  # event doy
  pre.harv.cult       = 14
  post.harv.cult      = 30
  post.harv.cc.cult   = 1L
  pre.crop.cult       = 1L

  # read in schedule template from file
  crop.index          = match(.crop, pkg.env$crop_types)
  cell_schedule_f     = schedule_table[scenario       %in% .scenario &
                                         N_or_S       %in% crop_hemi &
                                         irr          %in% .irr      &
                                         get(.crop)   %in% crop.index ]
  cell_schedule_f[, schedule := gsub('<fname>',        paste(cell_sch_data[,gridid],cell_sch_data[,run_seq],'site.100', sep = '_'), schedule)]
  cell_schedule_f[, schedule := gsub('<weather_file>', weather_fname,   schedule)]
  cell_schedule_f[, schedule := gsub('<block_name>',   block_name,      schedule)]
  cell_schedule_f[, schedule := gsub('<start_year>',   start_year,      schedule)]
  cell_schedule_f[, schedule := gsub('<end_year>',     end_year,        schedule)]
  cell_schedule_f[, schedule := gsub('<crop_cultivar>',crop_cultivar,   schedule)]
  ifelse(.ssp %in% 'historical',
         cell_schedule_f[, schedule := gsub('<co2_option>', 1L, schedule)],
         cell_schedule_f[, schedule := gsub('<co2_option>', gsub('ssp','',.ssp), schedule)])
  # create .sch block
  cell_schedule_f[, schedule := gsub('<plant_day>',    plant.date,      schedule)]
  cell_schedule_f[, schedule := gsub('<harvest_day>',  harvest.date,    schedule)]
  cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  (plant.date - pre.harv.cult),  schedule)]
  cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
  # cover crop check for post-harv cult
  ifelse(.scenario %in% 'conv'| .scenario %in% 'res'| .scenario %in% 'ntill',
         cell_schedule_f[, schedule := gsub('<cult_day_postharvest>', (harvest.date + post.harv.cult), schedule)],
         cell_schedule_f[, schedule := gsub('<cult_day_postharvest>', (harvest.date + post.harv.cc.cult), schedule)])
  cell_schedule_f[, schedule := gsub('<fert-amt>',     paste('(',cell_sch_data[, fertN.amt],'N,1.0F)', sep = ''),                   schedule)]
  cell_schedule_f[, schedule := gsub('<manure>',       'O_cell',                                                                    schedule)]
  cell_schedule_f[, schedule := gsub('<res-amt>',      paste('G',(cell_sch_data[, res.rtrn.amt]*100), sep = ''),                    schedule)]
  # IRIG events
  cell_schedule_f[, schedule := gsub('<irr_day>',      (plant.date -1L),                                                            schedule)]
  # ccg and ccl scenarios
  cell_schedule_f[, schedule := gsub('<cc_plant_day>', (harvest.date + 7),                                                          schedule)]
  ifelse(.scenario %in% 'ccg', cell_schedule_f[, schedule := gsub('<cc_cultivar>', 'RYE', schedule)], cell_schedule_f[, schedule := gsub('<cc_cultivar>', 'CLVC',                schedule)])
  cell_schedule_f[, schedule := gsub('<cc_harvest_day>',(plant.date - pre.harv.cult - 1L),                                          schedule)]
  cell_schedule_f[, schedule := gsub('<cc_harv-cult_day>',(plant.date - pre.harv.cult),                                             schedule)]
  # remove 0 OMAD lines
  if(cell_sch_data[1, orgCN.ratio] == 0) cell_schedule_f = cell_schedule_f[!schedule %like% 'O_cell']

  fwrite(list(cell_schedule_f[, schedule]), paste(schedule_path, '/',schedule_filename, sep = ''), quote = FALSE, col.names = FALSE)
  return(schedule_filename)
  }
