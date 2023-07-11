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
  # modify for 2016 model version
  if (.model %in% 'public') {
    .crop_100 = 'crop.100'
    crop_100 = setDT(read.table(.crop_100, sep = '\t'))
    crop_100[, V1 := gsub('RUETB',   'PRDX',   V1)]
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
#' @export
write_new_site100_t = function(.gridid, .p_gridid, .crop, .p_crop, .irr, .p_irr,
                             .run_seq, .date, .argsgcm.ssp, .arg_rowstart, .arg_rowend,
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
      paste0(.date, '/', .argsgcm.ssp, '/', .arg_rowstart, '-', .arg_rowend)
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
