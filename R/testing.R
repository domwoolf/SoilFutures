# testing.R
#
# Contains functions required in the testing simulation script for Soil Futures.
#
#' Check crop.100 file
#'
#' Rewrite the crop.100 if it does not match the gridid and crop in the simulation row
#'
#' @param .gridid integer, gridid associated with row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .crop character, crop associated with row in data.table
#' @param .p_crop character, crop of previous row
#' @param .simrow data.table, single row data.table
#' @param .date character, date of test simulation
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @param .model character (binary), DayCent model version either 'public' or 'SF'
#' @export
write_new_crop100_t = function(.gridid, .p_gridid, .crop, .p_crop, .simrow, .date, .argsgcm.ssp,
                               .arg_rowstart, .arg_rowend, .model) {
  if (.gridid != .p_gridid |
      .crop != .p_crop) {
    # crop.100 rewrite
    # print('Running crop.100.')
    file.remove('crop.100')
    create_crop(
      .simrow,
      .gridid,
      cover_crop_template,
      paste0(.date, '/', .argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend)
    )
  }
  # modify for inventory model version
  if (.model %in% 'public') {
    .crop_100 = 'crop.100'
    crop_100 = setDT(read.table(.crop_100, sep = '\t'))
    crop_100[, V1 := gsub('RUETB',   'PRDX',   V1)]
    crop_100[, V1 := gsub('7.0               FRTCINDX', '2.0               FRTCINDX', V1)]
    crop_100 = crop_100[!V1 %in% '-1     BASETEMP(3)',]
    crop_100 = crop_100[!V1 %in% '-1     BASETEMP(4)',]
    crop_100 = crop_100[!V1 %like% 'KCET',]
    crop_100 = crop_100[!V1 %like% 'KLIGHT',]
    crop_100 = crop_100[!V1 %like% 'SLA',]
    crop_100 = crop_100[!V1 %like% 'LEAFCL',]
    crop_100 = crop_100[!V1 %like% 'LEAFEMERG',]
    crop_100 = crop_100[!V1 %like% 'LEAFMX',]
    crop_100 = crop_100[!V1 %like% 'LEAFPM',]
    crop_100 = crop_100[!V1 %like% 'DDEMERG',]
    crop_100 = crop_100[!V1 %like% 'DDLAIMX',]
    crop_100 = crop_100[!V1 %like% 'DDPPSTART',]
    crop_100 = crop_100[!V1 %like% 'DDPPEND',]
    crop_100 = crop_100[!V1 %like% 'PPTYPE',]
    crop_100 = crop_100[!V1 %like% 'PPCRITICAL',]
    crop_100 = crop_100[!V1 %like% 'PPSNST',]
    crop_100 = crop_100[!V1 %like% 'VERNALSNST',]
    crop_100 = crop_100[!V1 %like% 'LUXEUPF(1)',]
    crop_100 = crop_100[!V1 %like% 'LUXEUPF(2)',]
    crop_100 = crop_100[!V1 %like% 'LUXEUPF(3)',]
    crop_100 = crop_100[!V1 %like% 'CSTGEUPF(1)',]
    crop_100 = crop_100[!V1 %like% 'CSTGEUPF(2)',]
    crop_100 = crop_100[!V1 %like% 'CSTGEUPF(3)',]
    crop_100 = crop_100[!V1 %like% 'CSTGDYS',]
    crop_100 = crop_100[!V1 %like% 'CSTGA2DRAT',]
    fwrite(crop_100, paste0(pkg.env$tmp_path, '/', .date, '/', .argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend, '/','crop.100'), quote = FALSE, col.names = FALSE)
  }
}

#' Check omad.100 file
#'
#' Rewrite the omad.100 if it does not match the gridid in the simulation row
#'
#' @param .gridid integer, gridid associated with row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .simrow data.table, single row data.table
#' @param .date character, date of test simulation
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @export
write_new_omad100_t = function(.gridid, .p_gridid, .simrow, .date, .argsgcm.ssp,
                             .arg_rowstart, .arg_rowend) {
  if (.gridid != .p_gridid) {
    file.remove('omad.100')
    create_omad(.simrow,
                .gridid,
                paste0(.date, '/',.argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend))
  }
}

#' Check site.100 file
#'
#' Rewrite the site.100 if it does not match the gridid, crop, irr in the simulation row
#'
#' @param .gridid integer, gridid associated with row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .crop character, crop associated with row in data.table
#' @param .p_crop character, crop of previous row
#' @param .irr integer, binary irrigation level associated with row in data.table
#' @param .p_irr integer, irr level of previous row
#' @param .run_seq integer, run sequence number associated with row in data.table
#' @param .date character, date of test simulation
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @param sitefile character, current site.100 file
#' @param .model character (binary), DayCent model version either 'public' or 'SF'
#' @export
write_new_site100_t = function(.gridid, .p_gridid, .crop, .p_crop, .irr, .p_irr,
                             .run_seq, .date, .argsgcm.ssp, .arg_rowstart, .arg_rowend,
                             sitefile, .model) {
  if (.gridid != .p_gridid |
      .crop != .p_crop |
      .irr != .p_irr) {
    file.remove(sitefile)
    sitefile = create_site(
      .gridid,
      .run_seq,
      .irr,
      .crop,
      cell_data_site,
      paste0(.date, '/', .argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend)
    )
  }
  if (.model %in% 'public') {
    .site_100 = paste0(.gridid, '_', .run_seq, '_', .crop, '_site.100')
    site_100  = setDT(read.table(.site_100, sep = '\t'))
    site_100  = site_100[!V1 %like% 'DOFWUE',]
    site_100[, V1 := gsub('NADJMAX',   'NADJFC',   V1)]
    site_100[, V1 := gsub('NADJMIN',   'NADJWP',   V1)]
    site_100  = site_100[!V1 %like% 'NADJCPT',]
    site_100  = site_100[!V1 %like% 'NADJNIT',]
    site_100  = site_100[!V1 %like% 'NADJWFP',]
    site_100  = site_100[!V1 %like% 'MAXNITF',]
    site_100  = site_100[!V1 %like% 'BASENIT',]
    site_100  = site_100[!V1 %like% 'MINNH4',]
    site_100  = site_100[!V1 %like% 'CO2DEN',]
    site_100  = site_100[!V1 %like% 'WFPSSLP',]
    site_100  = site_100[!V1 %like% 'MAXPH',]
    site_100  = site_100[!V1 %like% 'DPHPAR',]
    site_100  = site_100[!V1 %like% 'IPPAR',]
    site_100  = site_100[!V1 %like% 'IDPAR',]
    site_100  = site_100[!V1 %like% 'MAXDFC',]
    site_100  = site_100[!V1 %like% 'DFCPAR',]
    site_100  = site_100[!V1 %like% 'TA9PAR',]
    site_100  = site_100[!V1 %like% 'STEMPMF',]
    site_100  = site_100[!V1 %like% 'VMAXCAP',]
    site_100  = site_100[!V1 %like% 'FSMPAR',]
    fwrite(site_100, paste0(pkg.env$tmp_path, '/', .date, '/', .argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend, '/',
                            .gridid, '_', .run_seq, '_', .crop, '_site.100'), quote = FALSE, col.names = FALSE)
    }
  return(sitefile)
}

#' Create a DayCent Schedule file for non-cover crop simulations
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
#' @param .crop character, one of 'maiz', 'wwheat', 'swheat', 'soyb'
#' @param .scenario character, specifies scenario
#' @param .irr binary, specifies irrigation level
#' @param .site_fname character, name of site.100 file
#' @param start_yr integer, specifies start year of simulation
#' @param end_year integer, specifies end year of simulation
#' @param weather_fname name of input weather file
#' @param tmp.dir directory to create in tmp.path, set with arg[1]

#' @returns invisibly returns boolean indicating whether file was written successfully.
#' @export
non_cc_sched_t = function(cell_data, schedule_table = copy(schedule_template), .gridid, .ssp, .gcm, .crop, .scenario,
                        .irr, .site_fname, start_year, end_year, weather_fname, tmp.dir) {
  # variable definition
  cell_sch_data       = cell_data
  schedule_path       = paste(pkg.env$tmp_path, tmp.dir, sep = '/')
  schedule_filename   = cell_sch_data[1, paste(.scenario, '_', .crop, '_irr',.irr, '_', .gridid, '.sch', sep = "")]
  block_name          = cell_sch_data[1, paste(.scenario, '_', .crop, '_',.irr, sep = "")]
  crop_cultivar       = cell_sch_data[1, paste(dc_cropname, sep = "")]

  # cropping hemisphere (by planting and harvest date in Gregorian calendar)
  plant.date          = cell_sch_data[,plant.date]
  harvest.date        = cell_sch_data[,harvest.date]
  crop_hemi           = fifelse(plant.date > harvest.date, 'S','N') # this should catch imperfect 'N' or 'S' assignments

  # event doy
  pre.harv.cult       = 14
  post.harv.cult      = 14 # changed all to accommodate for winter wheat; consistent with pre harvest
  post.harv.cc.cult   = 1L
  pre.crop.cult       = 1L

  cell_schedule_f     = schedule_table[scenario       %in% .scenario &
                                         N_or_S       %in% crop_hemi &
                                         irr          %in% .irr      &
                                         get(.crop)        ==   1]
  cell_schedule_f[, schedule := gsub('<fname>',        .site_fname,     schedule)]
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

  # DOY check for post-harv cult greater than 365
  if (harvest.date + post.harv.cult > 365L) {
    cell_schedule_f[, schedule := gsub('<cult_day_postharvest>', 365L, schedule)]
  } else {
    cell_schedule_f[, schedule := gsub('<cult_day_postharvest>', (harvest.date + post.harv.cult), schedule)]
  }
  cell_schedule_f[, schedule := gsub('<fert-amt>',     paste('(',cell_sch_data[, fertN.amt],'N,0.6F)', sep = ''),                   schedule)]
  cell_schedule_f[, schedule := gsub('<manure>',       'O_cell',                                                                    schedule)]
  cell_schedule_f[, schedule := gsub('<res-amt>',      paste('G',(cell_sch_data[, res.rtrn.amt]*100), sep = ''),                    schedule)]
  # IRIG events
  cell_schedule_f[, schedule := gsub('<irr_day>',      (plant.date -1L),                                                            schedule)]

  # DOY check for cc harvest day & cult events where < plant.date - event <= 1L
  if (plant.date <= 15L) {
    cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  plant.date - 6L,               schedule)]
    cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
  } else {
    cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  (plant.date - pre.harv.cult + 1L),  schedule)]
    cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
  }
  # remove 0 OMAD lines
  if(cell_sch_data[1, orgCN.ratio] == 0) cell_schedule_f = cell_schedule_f[!schedule %like% 'O_cell']

  fwrite(list(cell_schedule_f[, schedule]), paste(schedule_path, '/',schedule_filename, sep = ''), quote = FALSE, col.names = FALSE)
  return(schedule_filename)
}
#' Create a DayCent Schedule file for cover crop simulations based on GDD harvest dates
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
#' @param GDD_harvest_dt data.table providing template of growing degree day harvest dates.
#' @param .gridid unique cell value to be passed to function.
#' @param .ssp character, specifies scenario
#' @param .gcm character, specifies scenario
#' @param .crop character, one of 'maiz', 'wwheat', 'swheat', 'soyb'
#' @param .scenario character, specifies scenario
#' @param .irr binary, specifies irrigation level
#' @param .site_fname character, name of site.100 file
#' @param start_yr integer, specifies start year of simulation
#' @param end_year integer, specifies end year of simulation
#' @param weather_fname name of input weather file
#' @param tmp.dir directory to create in tmp.path, set with arg[1]

#' @returns invisibly returns boolean indicating whether file was written successfully.
#' @export
cc_sched_t = function(cell_data, schedule_table = copy(covercrop_schedule_template), GDD_harvest_dt = copy(GDD_harvest_dates), .gridid, .ssp, .gcm, .crop, .scenario,
                    .irr, .site_fname, start_year, end_year, weather_fname, tmp.dir) {

  cell_sch_data       = cell_data
  GDD_harvest_dt      = GDD_harvest_dt[gridid %in% .gridid & crop %in% .crop & irr %in% .irr
                                       & gcm %in% .gcm & ssp %in% .ssp,]
  # variable definition
  harvest_dates               = colnames(GDD_harvest_dt[,7:91])
  cult_post_harvest_dates     = gsub('harvest_day_', 'cult_day_postharvest_', harvest_dates)
  cultkill_post_harvest_dates = gsub('harvest_day_', 'cultkill_day_postharvest_', harvest_dates)
  cc_plant_dates      = gsub('harvest_day_', 'cc_plant_day_', harvest_dates)
  schedule_path       = paste(pkg.env$tmp_path, tmp.dir, sep = '/')
  schedule_filename   = cell_sch_data[1, paste(.scenario, '_', .crop, '_irr',.irr, '_', .gridid, '.sch', sep = "")]
  block_name          = cell_sch_data[1, paste(.scenario, '_', .crop, '_',.irr, sep = "")]
  crop_cultivar       = cell_sch_data[1, paste(dc_cropname, sep = "")]
  # replace 0's with cell_data harvest date
  GDD_harvest_dt[, (colnames(GDD_harvest_dt[,7:91])) := lapply(.SD, function(x)
    ifelse(x == 0, cell_sch_data[,harvest.date], x)), .SDcols = colnames(GDD_harvest_dt[,7:91])]
  # replace NA's with cell_data harvest data
  GDD_harvest_dt[, (colnames(GDD_harvest_dt[,7:91])) := lapply(.SD, function(x)
    ifelse(is.na(x), cell_sch_data[,harvest.date], x)), .SDcols = colnames(GDD_harvest_dt[,7:91])]
  # cropping hemisphere (by planting and harvest date in Gregorian calendar)
  plant.date          = cell_sch_data[,plant.date]
  harvest_main        = cell_sch_data[,harvest.date]
  crop_hemi           = fifelse(plant.date > harvest_main, 'S','N') # this should catch imperfect 'N' or 'S' assignments

  # event doy
  pre.harv.cult       = 14
  post.harv.cult      = 14 # changed all to accommodate for winter wheat; consistent with pre harvest
  post.harv.cc.cult   = 1L
  pre.crop.cult       = 1L

  if (harvest_main < 358) {
    cell_schedule_f     = schedule_table[scenario       %in% .scenario &
                                           N_or_S       %in% crop_hemi &
                                           irr          %in% .irr      &
                                           get(.crop)        ==   1    &
                                           harv_error        ==   0]
    cell_schedule_f[, schedule := gsub('<fname>',        .site_fname,     schedule)]
    cell_schedule_f[, schedule := gsub('<weather_file>', weather_fname,   schedule)]
    cell_schedule_f[, schedule := gsub('<block_name>',   block_name,      schedule)]
    cell_schedule_f[, schedule := gsub('<start_year>',   start_year,      schedule)]
    cell_schedule_f[, schedule := gsub('<end_year>',     end_year,        schedule)]
    cell_schedule_f[, schedule := gsub('<crop_cultivar>',crop_cultivar,        schedule)]

    # co2 option
    ifelse(.ssp %in% 'historical',
           cell_schedule_f[, schedule := gsub('<co2_option>', 1L, schedule)],
           cell_schedule_f[, schedule := gsub('<co2_option>', gsub('ssp','',.ssp), schedule)])
    # nitrogen, residue
    cell_schedule_f[, schedule := gsub('<fert-amt>',     paste('(',cell_sch_data[, fertN.amt],'N,0.6F)', sep = ''),                   schedule)]
    cell_schedule_f[, schedule := gsub('<manure>',       'O_cell',                                                                    schedule)]
    cell_schedule_f[, schedule := gsub('<res-amt>',      paste('G',(cell_sch_data[, res.rtrn.amt]*100), sep = ''),                    schedule)]
    # IRIG events
    cell_schedule_f[, schedule := gsub('<irr_day>',      (plant.date -1L),                                                            schedule)]

    # create .sch block with dynamic GDD harvest dates
    cell_schedule_f[, schedule := gsub('<plant_day>',    plant.date,      schedule)]
    counter = 1L
    for(h_date in harvest_dates) {
      cell_schedule_f[, schedule := gsub(h_date, GDD_harvest_dt[,get(h_date)], schedule)]
      cell_schedule_f[, schedule := gsub(cult_post_harvest_dates[counter], (GDD_harvest_dt[,get(h_date)] + post.harv.cc.cult), schedule)]
      cell_schedule_f[, schedule := gsub(cultkill_post_harvest_dates[counter], (GDD_harvest_dt[,get(h_date)] + post.harv.cc.cult + 1L), schedule)]
      if (GDD_harvest_dt[,get(h_date)] + 7 > 365L) {
        cell_schedule_f[, schedule := gsub(cc_plant_dates[counter], 365L, schedule)]
      } else {
        cell_schedule_f[, schedule := gsub(cc_plant_dates[counter], (GDD_harvest_dt[,get(h_date)] + 7), schedule)]
      }
      counter = counter + 1L
    }

    # cover crop cultivar
    ifelse(.scenario %like% 'ccg', cell_schedule_f[, schedule := gsub('<cc_cultivar>', 'RYE', schedule)], cell_schedule_f[, schedule := gsub('<cc_cultivar>', 'CLVC', schedule)])

    # DOY check for cc harvest day & cult events where < plant.date - event <= 1L
    if (plant.date <= 15L) {
      # no cc
      cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  plant.date - 6L,               schedule)]
      cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
      # cc
      cell_schedule_f[, schedule := gsub('<cc_harvest_day>',(plant.date - 8L),                    schedule)]
      cell_schedule_f[, schedule := gsub('<cc_harv-cult_day>',(plant.date - 7L),                  schedule)]
    } else {
      # no cc
      cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  (plant.date - pre.harv.cult + 1L),  schedule)]
      cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
      # cc
      cell_schedule_f[, schedule := gsub('<cc_harvest_day>',(plant.date - pre.harv.cult - 1L),                                          schedule)]
      cell_schedule_f[, schedule := gsub('<cc_harv-cult_day>',(plant.date - pre.harv.cult),                                             schedule)]
    }
    # remove 0 OMAD lines
    if(cell_sch_data[1, orgCN.ratio] == 0) cell_schedule_f = cell_schedule_f[!schedule %like% 'O_cell']

    fwrite(list(cell_schedule_f[, schedule]), paste(schedule_path, '/',schedule_filename, sep = ''), quote = FALSE, col.names = FALSE)
    return(schedule_filename)
  } else {
    cell_schedule_f     = schedule_table[scenario       %in% .scenario &
                                           N_or_S       %in% crop_hemi &
                                           irr          %in% .irr      &
                                           get(.crop)        ==   1    &
                                           harv_error        ==   1]
    cell_schedule_f[, schedule := gsub('<fname>',        .site_fname,     schedule)]
    cell_schedule_f[, schedule := gsub('<weather_file>', weather_fname,   schedule)]
    cell_schedule_f[, schedule := gsub('<block_name>',   block_name,      schedule)]
    cell_schedule_f[, schedule := gsub('<start_year>',   start_year,      schedule)]
    cell_schedule_f[, schedule := gsub('<end_year>',     end_year,        schedule)]
    cell_schedule_f[, schedule := gsub('<crop_cultivar>',crop_cultivar,        schedule)]

    # co2 option
    ifelse(.ssp %in% 'historical',
           cell_schedule_f[, schedule := gsub('<co2_option>', 1L, schedule)],
           cell_schedule_f[, schedule := gsub('<co2_option>', gsub('ssp','',.ssp), schedule)])
    # nitrogen, residue
    cell_schedule_f[, schedule := gsub('<fert-amt>',     paste('(',cell_sch_data[, fertN.amt],'N,1.0F)', sep = ''),                   schedule)]
    cell_schedule_f[, schedule := gsub('<manure>',       'O_cell',                                                                    schedule)]
    cell_schedule_f[, schedule := gsub('<res-amt>',      paste('G',(cell_sch_data[, res.rtrn.amt]*100), sep = ''),                    schedule)]
    # IRIG events
    cell_schedule_f[, schedule := gsub('<irr_day>',      (plant.date -1L),                                                            schedule)]

    # create .sch block with dynamic GDD harvest dates
    cell_schedule_f[, schedule := gsub('<plant_day>',    plant.date,      schedule)]
    counter = 1L
    for(h_date in harvest_dates) {
      cell_schedule_f[, schedule := gsub(h_date, GDD_harvest_dt[,get(h_date)], schedule)]
      cell_schedule_f[, schedule := gsub(cult_post_harvest_dates[counter], (GDD_harvest_dt[,get(h_date)] + post.harv.cc.cult), schedule)]
      if (GDD_harvest_dt[,get(h_date)] + 7 >= 365L) {
        cell_schedule_f[, schedule := gsub(cc_plant_dates[counter], 7L, schedule)]
        cell_schedule_f[, schedule := gsub(cultkill_post_harvest_dates[counter], 6L, schedule)]
      } else {
        cell_schedule_f[, schedule := gsub(cc_plant_dates[counter], (GDD_harvest_dt[,get(h_date)] + 7), schedule)]
      }
      counter = counter + 1L
    }

    # cover crop cultivar
    ifelse(.scenario %like% 'ccg', cell_schedule_f[, schedule := gsub('<cc_cultivar>', 'RYE', schedule)], cell_schedule_f[, schedule := gsub('<cc_cultivar>', 'CLVC', schedule)])

    # DOY check for cc harvest day & cult events where < plant.date - event <= 1L
    if (plant.date <= 15L) {
      # no cc
      cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  plant.date - 6L,               schedule)]
      cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
      # cc
      cell_schedule_f[, schedule := gsub('<cc_harvest_day>',(plant.date - 8L),                    schedule)]
      cell_schedule_f[, schedule := gsub('<cc_harv-cult_day>',(plant.date - 7L),                  schedule)]
    } else {
      # no cc
      cell_schedule_f[, schedule := gsub('<cult_day_preharvest>',  (plant.date - pre.harv.cult + 1L),  schedule)]
      cell_schedule_f[, schedule := gsub('<cult_kill_day>',        (plant.date - pre.crop.cult),  schedule)]
      # cc
      cell_schedule_f[, schedule := gsub('<cc_harvest_day>',(plant.date - pre.harv.cult - 1L),                                          schedule)]
      cell_schedule_f[, schedule := gsub('<cc_harv-cult_day>',(plant.date - pre.harv.cult),                                             schedule)]
    }
    # remove 0 OMAD lines
    if(cell_sch_data[1, orgCN.ratio] == 0) cell_schedule_f = cell_schedule_f[!schedule %like% 'O_cell']

    fwrite(list(cell_schedule_f[, schedule]), paste(schedule_path, '/',schedule_filename, sep = ''), quote = FALSE, col.names = FALSE)
    return(schedule_filename)
  }
}

#' Prepare DayCent output for pre-processing
#'
#' Reformat DayCent output and save results to two tables: annual results, growing season results
#'
#' @param .lis_fname character, lis file name
#' @param .gridid integer, gridid associated with row in data.table
#' @param .scenario character, scenario associated with row in data.table
#' @param .crop character, crop associated with row in data.table
#' @param .irr integer, binary irrigation level associated with row in data.table
#' @param .ssp character, ssp associated with row in data.table
#' @param .gcm character, gcm associated with row in data.table
#' @param .plant_date integer, doy of planting associated with row in data.table
#' @param .harv_date integer, doy of harvest associated with row in data.table
#' @param .clim_ipcc integer, value associated with ipcc climate type from row in data.table
#' @param .date character, date of test simulation
#' @param .main_out_path character, directory to write results table to
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @param .out_tbl_fname character, name of annual results file
#' @param .out_gr_tbl_fname character, name of growing, non-growing season results file
#' @export
results_processing_t = function(.lis_fname, .gridid, .scenario, .crop, .irr, .ssp, .gcm,
                              .plant_date, .harv_date, .clim_ipcc, .date, .argsgcm.ssp, .main_out_path,
                              .arg_rowstart, .arg_endrow, .out_tbl_fname, .out_gr_tbl_fname) {
  # prepare data files

  file.rename('year_summary.out', paste0('year_summary_', .lis_fname, '.out'))
  file.rename('nflux.out', paste0('nflux_', .lis_fname, '.out'))
  file.rename('harvest.csv', paste0('harvest_', .lis_fname, '.csv'))
  file.rename('bio.out', paste0('bio_', .lis_fname, '.out'))

  # check for rewild, adjust harvest.csv

  if(.scenario %in% 'rewild') {
    harvest.csv  = fread(paste0('harvest_', .lis_fname, '.csv'))
    add.df = data.table(matrix(NA, nrow = 85, ncol = 82))
    names(add.df) = names(harvest.csv)
    harvest.csv = rbind(harvest.csv, add.df)
    harvest.csv[, time := 2016:2100]
    harv.keep    = c('time', 'dayofyr', 'crpval', 'agcacc', 'bgcjacc', 'bgcmacc',
                     'cgrain', 'egrain(N)', 'resid', 'reside(N)', 'irrapp', 'fertapp(N)',
                     'omadapp', 'omaeapp(N)')
    harvest.csv  = harvest.csv[, ..harv.keep]
    harvest.csv[, gridid := .gridid][, scenario := .scenario][, irr := .irr][, ssp := .ssp][, gcm := .gcm]
    setcolorder(harvest.csv, c('gridid', 'scenario', 'irr', 'ssp', 'gcm'))
    harvest.csv[, time := floor(time)]
  } else {
    harvest.csv  = fread(paste0('harvest_', .lis_fname, '.csv'))
    harv.keep    = c('time', 'dayofyr', 'crpval', 'agcacc', 'bgcjacc', 'bgcmacc',
                     'cgrain', 'egrain(N)', 'resid', 'reside(N)', 'irrapp', 'fertapp(N)',
                     'omadapp', 'omaeapp(N)')
    harvest.csv  = harvest.csv[, ..harv.keep]
    harvest.csv[, gridid := .gridid][, scenario := .scenario][, irr := .irr][, ssp := .ssp][, gcm := .gcm]
    setcolorder(harvest.csv, c('gridid', 'scenario', 'irr', 'ssp', 'gcm'))
    harvest.csv[, time := floor(time)]
  }

  year_sum.out = fread(paste0('year_summary_', .lis_fname, '.out'))
  year_sum.out[, gridid := .gridid][, scenario := .scenario][, irr := .irr][, ssp := .ssp][, gcm := .gcm]
  setcolorder(year_sum.out, c('gridid', 'scenario', 'irr', 'ssp', 'gcm'))
  year_sum.out[, time := floor(time)]

  lis.file     = fread(paste0(.lis_fname, '.lis'),
                       skip = "time",
                       blank.lines.skip = TRUE)
  lis.file[, gridid := .gridid][, scenario := .scenario][, irr := .irr][, ssp := .ssp][, gcm := .gcm]
  setcolorder(lis.file, c('gridid', 'scenario', 'irr', 'ssp', 'gcm'))
  lis.file     = lis.file[time != 2101, ]

  nflux.out    = fread(paste0('nflux_', .lis_fname, '.out'))
  nflux.drop   = c('dnit_N2-N', 'NO-N', 'CUM-N2O(gN/ha)', 'CUM-NO(gN/ha)', 'NH3-N')
  nflux.out    = nflux.out[, -..nflux.drop]
  nflux.out[, gridid := .gridid][, scenario := .scenario][, irr := .irr][, ssp := .ssp][, gcm := .gcm]
  nflux.out[, time := floor(time)]

  # table 1 - annual sum
  nflux.annsum.out = nflux.out
  nflux.annsum.out = nflux.annsum.out[, .(
    gridid         = gridid,
    scenario       = scenario,
    irr            = irr,
    ssp            = ssp,
    gcm            = gcm,
    ann.nit_N2O.N  = sum(`nit_N2O-N`),
    ann.dnit_N2O.N = sum(`dnit_N2O-N`)
  ),
  by             = .(time)]
  nflux.annsum.out = unique(nflux.annsum.out, by = c('time'))

  # table 2 - grw season, ngrw season sum
  nflux.szn.out    = nflux.out
  if(.plant_date < .harv_date) {
    nflux.grszn.out = nflux.szn.out[dayofyr >= .plant_date &
                                      dayofyr <= .harv_date]
  } else {
    nflux.grszn.out = nflux.szn.out[dayofyr >= .plant_date |
                                      dayofyr <= .harv_date]
  }
  nflux.grszn.out  = nflux.grszn.out[, .(
    gridid         = gridid,
    scenario       = scenario,
    irr            = irr,
    ssp            = ssp,
    gcm            = gcm,
    nit_N2O.N   = sum(`nit_N2O-N`),
    dnit_N2O.N  = sum(`dnit_N2O-N`)
  ),
  by             = .(time)]
  nflux.grszn.out  = unique(nflux.grszn.out, by = c('time'))
  nflux.grszn.out[, period := 'grw']
  setcolorder(nflux.grszn.out,
              c('gridid', 'scenario', 'irr', 'ssp', 'gcm', 'period'))
  if(.plant_date < .harv_date) {
    nflux.ngrszn.out = nflux.szn.out[dayofyr < .plant_date |
                                       dayofyr > .harv_date]
  } else {
    nflux.ngrszn.out = nflux.szn.out[dayofyr < .plant_date &
                                       dayofyr > .harv_date]
  }
  nflux.ngrszn.out = nflux.ngrszn.out[, .(
    gridid         = gridid,
    scenario       = scenario,
    irr            = irr,
    ssp            = ssp,
    gcm            = gcm,
    nit_N2O.N   = sum(`nit_N2O-N`),
    dnit_N2O.N  = sum(`dnit_N2O-N`)
  ),
  by               = .(time)]
  nflux.ngrszn.out = unique(nflux.ngrszn.out, by = c('time'))
  nflux.ngrszn.out[, period := 'ngrw']
  setcolorder(nflux.ngrszn.out,
              c('gridid', 'scenario', 'irr', 'ssp', 'gcm', 'period'))
  nflux.szn.out    = rbind(nflux.grszn.out, nflux.ngrszn.out)

  # results output
  table_out    = year_sum.out[harvest.csv,       on = .(
    gridid = gridid,
    scenario = scenario,
    irr = irr,
    ssp = ssp,
    gcm = gcm,
    time = time
  )]
  table_out    = table_out[lis.file,             on = .(
    gridid = gridid,
    scenario = scenario,
    irr = irr,
    ssp = ssp,
    gcm = gcm,
    time = time
  )]
  table_out    = table_out[nflux.annsum.out,     on = .(
    gridid = gridid,
    scenario = scenario,
    irr = irr,
    ssp = ssp,
    gcm = gcm,
    time = time
  )]

  table_out    = table_out[, lapply(.SD, function(x) {
    ifelse(is.na(x), 0, x)
  })]
  table_out[, clim_ipcc := .clim_ipcc]
  table_out[, crop := .crop]
  setcolorder(table_out,
              c(
                'gridid',
                'crop',
                'scenario',
                'irr',
                'ssp',
                'gcm',
                'clim_ipcc'
              ))
  names.final   = names(table_out)
  names.numeric = table_out[, which(sapply(.SD, is.numeric))]
  names.char    = as.numeric(table_out[, which(sapply(.SD, is.character))])
  table_round   = table_out[, lapply(.SD, round, digits = 3), .SDcols = names.numeric]
  table_out     = cbind(table_out[, ..names.char], table_round)
  setcolorder(table_out, c(names.final))
  fwrite(
    table_out,
    paste(
      .main_out_path,
      '/',
      .date,
      '/',
      .argsgcm.ssp,
      '/',
      .arg_rowstart,
      '-',
      .arg_endrow,
      '/',
      .out_tbl_fname,
      sep = ''
    ),
    append = TRUE
  )

  nflux.szn.out[, clim_ipcc := .clim_ipcc]
  nflux.szn.out[, crop := .crop]
  setcolorder(
    nflux.szn.out,
    c(
      'gridid',
      'crop',
      'scenario',
      'irr',
      'ssp',
      'gcm',
      'clim_ipcc',
      'period'
    )
  )
  fwrite(
    nflux.szn.out,
    paste(
      .main_out_path,
      '/',
      .date,
      '/',
      .argsgcm.ssp,
      '/',
      .arg_rowstart,
      '-',
      .arg_endrow,
      '/',
      .out_gr_tbl_fname,
      sep = ''
    ),
    append = TRUE
  )
}
