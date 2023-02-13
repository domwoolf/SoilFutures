# simulation.R
#
# Contains functions required in the simulation script for Soil Futures.
#
#' Print row in simulation
#'
#' Track simulation row runs with a print statement.
#'
#' @param row integer, current row in data.table
#' @param .gridid integer, gridid associated with row in data.table
#' @param .ssp character, ssp of row in data.table
#' @param .gcm character, gcm of row in data.table
#' @param .crop character, crop of crow in data.table
#' @param .irr integer, binary value of irrigation level
#' @param .scenario character, scenario of row in data.table
#' @export
pr_sim_run = function(.row, .gridid, .ssp, .gcm, .crop, .irr, .scenario) {
  row_to_return = paste0(
      'Running row ',
      .row,
      ' (gridid: ',
      .gridid,
      ' gcm: ',
      .gcm,
      ' ssp: ',
      .ssp,
      ' crop: ',
      .crop,
      ' irr level: ',
      .irr,
      ' scenario: ',
      .scenario,
      ').'
    )
  print(row_to_return)
}
#' Change weather file name
#'
#' Determines if weather file should be removed and new weather file added based on contents
#' of simulation data.table.
#'
#' @param .gridid integer, gridid associated with row in data.table
#' @param .ssp character, ssp of row in data.table
#' @param .gcm character, gcm of row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .p_ssp character, ssp of previous row
#' @param .p_gcm character, gcm of previous row
#' @param w_fname character, name of weather file in tmp directory
#' @param arg.gcm character, third argument defined in bash script
#' @param arg.ssp character, fourth argument defined in bash script
#' @export
wthfile_exists_wfname = function(.gridid, .ssp, .gcm, .p_gridid, .p_ssp, .p_gcm,
                                 .w_fname, arg.gcm, arg.ssp) {
  if((.gridid != .p_gridid) |
     (.ssp != .p_ssp) |
     (.gcm != .p_gcm)) {
    unlink(.w_fname)
    w_fname      = paste0('weather_',
                          arg.ssp,
                          '_',
                          arg.gcm,
                          '_',
                          .gridid,
                          '.wth')
  }
  return(w_fname)
}
#' Change weather file path
#'
#' Determines if new weather path created added based on contents of simulation data.table.
#'
#' @param w_fname character, name of weather file in tmp directory
#' @param .gridid integer, gridid associated with row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .ssp character, ssp of row in data.table
#' @param .p_ssp character, ssp of previous row
#' @param .gcm character, gcm of row in data.table
#' @param .p_gcm character, gcm of previous row
#' @param .grab_wpath character, directory with weather file
#' @export
name_of_wpath = function(w_fname, .grab_wpath, .gridid, .p_gridid, .ssp,
                         .p_ssp, .gcm, .p_gcm) {
  if((.gridid != .p_gridid) |
     (.ssp != .p_ssp) |
     (.gcm != .p_gcm)) {

    w_fname.path = paste0(
      grab_wth_path,
      '/',
      w_fname
    )
  }
  return(w_fname.path)
}
#' Check if weather file exists in directory
#'
#' Checks the weather directory specified for the existence of weather file. If the file
#' does not exist this function creates the specified weather file and saves it.
#'
#' @param .w_fname.path character, directory of weather file
#' @param .start_yr vector, start year for weather file
#' @param .end_yr vector, end year for weather file
#' @param .gridid integer, gridid associated with row in data.table
#' @param .gridid_rotated integer, rotated gridid associated with row in data.table
#' @param .ssp character, ssp of row in data.table
#' @param .gcm character, gcm of row in data.table
#' @param .grab_wth_path character, directory to write wth file to
#' @param .argsgcm.ssp character, second argument defined in bash script
#' @export
wthfile_exists_wpath = function(.w_fname.path, .start_yr, .end_yr, .gridid, .gridid_rotated,
                                .ssp, .gcm, .grab_wth_path, .argsgcm.ssp) {
  if (!file.exists(w_fname.path)) {
    initial_wth = initialize_weather(.start_yr[1], .end_yr[1])
    if (.gcm %in% 'historical') {
      list_climate = load_climate(.ssp,
                                  .gcm,
                                  .start_yr[2],
                                  .end_yr[2])
    } else {
      list_climate = load_climate(.ssp,
                                  .gcm,
                                  .start_yr[1],
                                  .end_yr[1])
    }
    make_weather_file(
      list_climate,
      .gridid,
      .gridid_rotated,
      .gcm,
      .ssp,
      weather = initial_wth,
      cmip6_calendars,
     .grab_wth_path,
     .argsgcm.ssp
    )
  }
}
#' Check for bad weather file
#'
#' This function checks if the weather file has less than the required 7 columns
#' and breaks the simulation loop for gridid associated with this weather file
#' @import data.table
#' @param .w_fname character, weather file
#' @param .row integer, row number in simulation data.table
#' @param .gridid integer, gridid associated with row in data.table
#' @param .ssp character, ssp of row in data.table
#' @param .gcm character, gcm of row in data.table
#' @export
bad_wth_check = function(.w_fname, .row, .gridid, .ssp, .gcm) {
  wth.cols = fread(.w_fname)
  if (NCOL(wth.cols) != 7) {
    print(
      paste0(
        'WTH file for ',
        .row,
        ' (gridid: ',
        .gridid,
        ' ssp: ',
        .ssp,
        ' gcm: ',
        .gcm,
        ' does not contain proper number of columns.
        Moving to next row in data table.'
      )
    )
    next
  }
}
#' Check crop.100 file
#'
#' Rewrite the crop.100 if it does not match the gridid and crop in the simulation row
#'
#' @param .gridid integer, gridid associated with row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .crop character, crop associated with row in data.table
#' @param .p_crop character, crop of previous row
#' @param .simrow data.table, single row data.table
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @export
write_new_crop100 = function(.gridid, .p_gridid, .crop, .p_crop, .simrow, .argsgcm.ssp,
                             .arg_rowstart, .arg_rowend) {
  if (.gridid != .p_gridid |
      .crop != .p_crop) {
    # crop.100 rewrite
    # print('Running crop.100.')
    file.remove('crop.100')
    create_crop(
      .simrow,
      .gridid,
      cover_crop_template,
      paste0(.argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend)
    )
  }
}
#' Check omad.100 file
#'
#' Rewrite the omad.100 if it does not match the gridid in the simulation row
#'
#' @param .gridid integer, gridid associated with row in data.table
#' @param .p_gridid integer, gridid of previous row
#' @param .simrow data.table, single row data.table
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @export
write_new_omad100 = function(.gridid, .p_gridid, .simrow, .argsgcm.ssp,
                             .arg_rowstart, .arg_rowend) {
  if (.gridid != .p_gridid) {
    file.remove('omad.100')
    create_omad(.simrow,
                .gridid,
                paste0(.argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend))
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
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @param sitefile character, current site.100 file
#' @export
write_new_site100 = function(.gridid, .p_gridid, .crop, .p_crop, .irr, .p_irr,
                             .run_seq, .argsgcm.ssp, .arg_rowstart, .arg_rowend,
                             sitefile) {
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
      paste0(.argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend)
    )
  }
  return(sitefile)
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
#' @param .main_out_path character, directory to write results table to
#' @param .argsgcm.ssp character, first argument giving name of directory for gcm-ssp
#' @param .arg_rowstart integer, fifth argument giving start row of simulation
#' @param .arg_endrow integer, sixth argument giving end row of simulation
#' @param .out_tbl_fname character, name of annual results file
#' @param .out_gr_tbl_fname character, name of growing, non-growing season results file
#' @export
results_processing = function(.lis_fname, .gridid, .scenario, .crop, .irr, .ssp, .gcm,
                              .plant_date, .harv_date, .clim_ipcc, .argsgcm.ssp, .main_out_path,
                              .arg_rowstart, .arg_endrow, .out_tbl_fname, .out_gr_tbl_fname) {
  # prepare data files

  file.rename('year_summary.out', paste0('year_summary_', .lis_fname, '.out'))
  file.rename('nflux.out', paste0('nflux_', .lis_fname, '.out'))
  file.rename('harvest.csv', paste0('harvest_', .lis_fname, '.csv'))

  # check for rewild, adjust harvest.csv

  if(.scenario %in% 'rewild') {
    harvest.csv  = fread(paste0('harvest_', .lis_fname, '.csv'))
    add.df = data.table(matrix(NA, nrow = 85, ncol = 82))
    names(add.df) = names(add.csv)
    harvest.csv = rbind(harvest.csv, add.df)
    harvest.csv[, time := 2016:2100]
    harvest.csv  = harvest.csv[, c(1:9, 28:29, 32, 33, 39:40)]
    harvest.csv[, gridid := .gridid][, scenario := .scenario][, irr := .irr][, ssp := .ssp][, gcm := .gcm]
    setcolorder(harvest.csv, c('gridid', 'scenario', 'irr', 'ssp', 'gcm'))
    harvest.csv[, time := floor(time)]
  } else {
    harvest.csv  = fread(paste0('harvest_', .lis_fname, '.csv'))
    harvest.csv  = harvest.csv[, c(1:9, 28:29, 32, 33, 39:40)]
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
  nflux.out    = nflux.out[, -5:-9]
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
#' Track simulation times
#'
#' Write row run time to a log file to track length of each simulation
#'
#' @param .row integer,
#' @param .old time, start of simulation
#' @param .end.daycent time, end of daycent execution
#' @param .start.daycent time, start of daycent execution
#' @param .gridid integer, gridid associated with row in data.table
#' @param .gcm character, gcm associated with row in data.table
#' @param .ssp character, ssp associated with row in data.table
#' @param .crop character, crop associated with row in data.table
#' @param .irr integer, binary irrigation level associated with row in data.table
#' @param .scenario character, scenario associated with row in data.table
#' @param .log.file character, name of log file to write to
#' @export
sim_time_logf = function(.row, .old, .end.daycent, .start.daycent, .gridid, .gcm,
                         .ssp, .crop, .irr, .scenario, .log.file) {
  if(.row %% 50 == 1) {
    end       = Sys.time()
    print(end)
    elapsed   = end - .old
    dcent.elp = .end.daycent - .start.daycent
    line      = paste0('Time for row ', .row, ' (gridid: ', .gridid,
                       ' gcm: ', .gcm, ' ssp: ', .ssp, ' crop: ', .crop,
                       ' irr: ', .irr, ' scenario: ', .scenario, ') started:',
                       .old, ' and ended:', end, ' for elapsed:', elapsed, ', ',
                       'Daycent elapsed:', dcent.elp, '.')
    write(line, .log.file, append = TRUE)
  }
}
