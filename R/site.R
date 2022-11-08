# site.R
#
# contains code to extract site file from cell_data table
#
#' Create a DayCent site file
#'
#' Used for writing a site file using spatial data.
#'
#' This function creates a universal site file for a grid cell.
#'
#' @import data.table
#' @param cell Integer, grid cell ID number
#' @param .irr Integer, irrigation level
#' @param cell_data Data table, contains spatial data associated with each cell and site.100 file
#' @export
create_site = function(.gridid, .run_seq, .irr, cell_data_site = copy(cell_data_site), tmp.dir){
  site.file     = paste(.gridid, .run_seq, 'site.100', sep = '_')
  site.100      = cell_data_site[gridid    %in% .gridid &
                                   irr     %in% .irr  &
                                   run_seq %in% .run_seq, site100]
  fwrite(list(site.100), paste(pkg.env$tmp_path, tmp.dir, site.file, sep = '/'), quote = FALSE)
  return(site.file)
}
