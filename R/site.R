# site.R
#
# contains code to extract site and equilibrium sch files from a data.table
#
#' Create a DayCent site file
#'
#' Used for writing a site file using spatial data.
#'
#' This function creates a universal site file for a grid cell.
#'
#' @import data.table
#' @param .gridid integer, grid cell ID number
#' @param .run_seq integer, land use cohort ID number
#' @param .irr Integer, irrigation level
#' @param .crop character, name of crop aligned with gridid and run_seq
#' @param cell_data_site Data table, contains spatial data associated with each cell and site.100 file
#' @param tmp.dir character, temporary directory to write the site.100 file to
#' @export
create_site = function(.gridid, .run_seq, .irr, .crop, cell_data_site = copy(cell_data_site), tmp.dir){
  site.file     = paste(.gridid, .run_seq, .crop, 'site.100', sep = '_')
  site.100      = cell_data_site[gridid    %in% .gridid  &
                                   irr     %in% .irr     &
                                   run_seq %in% .run_seq &
                                   crop    %in% .crop, site100]
  fwrite(list(site.100), paste(pkg.env$tmp_path, tmp.dir, site.file, sep = '/'), quote = FALSE)
  return(site.file)
}

#' Create a DayCent equilibrium schedule file
#'
#' Used for extracting a schedule file from a data table.
#'
#' This function creates a universal equilibrium schedule file for a grid cell based on a land use ID.
#'
#' @import data.table
#' @param .gridid integer, grid cell ID number
#' @param .schl_name character, name of land use ID associated with grid cell ID number
#' @param .schl_seq integer, schedule file number in the equilibrium run sequence
#' @param EQ_sch_data data table, contains data associated with each grid cell and its 2 eq sch files
#' @param tmp.dir character, temporary directory to write the schedule files to
#' @export
create_eq_sch = function(.gridid, .schl_name, .schl_seq, EQ_sch_data = copy(EQ_sch_data), tmp.dir){
  eq.file     = paste0(.gridid, '_',.schl_name, '_', .schl_seq, '.sch')
  eq.100      = EQ_sch_data[schl_name %in% .schl_name  &
                            schl_seq  %in% .schl_seq, schl_data]
  fwrite(list(eq.100), paste(pkg.env$tmp_path, tmp.dir, eq.file, sep = '/'), quote = FALSE)
  return(eq.file)
}
