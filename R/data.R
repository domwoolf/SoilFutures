#' Soil Futures Datasets
#'
#' CMIP6 Calendars.
#'
#' Calendar types of the CMIP6 models.
#'
#' @format ## `cmip6_calendars`
#' A data frame with 39 rows and 2 columns:
#' \describe{
#'   \item{gcm}{General Circulation Model}
#'   \item{calendar}{Calendar type; 5 levels}
#'   ...
#' }
#' @source NASA, Bridget Thrasher.
"cmip6_calendars"
#'
#' Cover Crop Template.
#'
#' crop.100 parameters for DayCent simulations. Starts with cover crops and followed by
#' crops required for rewild scenarios.
#'
#' @format ## `cover_crop_template`
#' A data frame with 11982 rows and 1 column:
#' \describe{
#'   \item{V1}{crop.100 parameters; start of crop indicated by crop code in all caps}
#'   ...
#' }
#' @source Colorado State University, Yi Yang.
"cover_crop_template"
#'
#' Schedule Template.
#'
#' Schedule assignments for scenarios in DayCent simulations. Follows sch file format.
#' Does not include rewild scenario
#'
#' @format ## `schedule_template`
#' A data frame with 2812 rows and 8 columns:
#' \describe{
#'   \item{scenario}{Scenario in simulation; five levels}
#'   \item{N_or_S}{North or South Hemisphere - loosely defined.}
#'   \item{irr}{Irrgiation, binary}
#'   \item{maiz}{Maize, binary}
#'   \item{soyb}{Soybean, binary.}
#'   \item{wwht}{Winter wheat, binary.}
#'   \item{swht}{Spring wheat, binary.}
#'   \item{schedule}{Schedule lines relevant for the scenario.}
#'   ...
#' }
#' @source Cornell University, S.C. McClelland
"schedule_template"
#'
#' Extended site.100 files.
#'
#' The extended site.100 files for each crop x irrigation x gridid combination.
#' Extended site files identified a priori based on land use cohort selection criteria
#' for simulations.
#'
#' @format ## `cell_data_site`
#' A data frame with 87387 rows and 7 columns:
#' \describe{
#'   \item{gridid}{Grid ID}
#'   \item{regionid}{Region ID}
#'   \item{crop}{Crop, four levels}
#'   \item{irr}{Irrigation, binary}
#'   \item{run_seq}{Run sequence, identified from land use cohort selection process.}
#'   \item{eq_schl_name}{Equilibrium schedule file code.}
#'   \item{site100}{Extended site.100 data.}
#'   ...
#' }
#' @source Colorado State University, Yi Yang.
"cell_data_site"
#'
#' Northern Hemisphere Rewild Schedule Files.
#'
#' The extended site.100 files for each crop x irrigation x gridid combination.
#' Extended site files identified a priori based on land use cohort selection criteria
#' for simulations.
#'
#' @format ## `eq_pnh_sch_template`
#' A data frame with 1971 rows and 2 columns:
#' \describe{
#'   \item{schl_name}{Schedule File Name}
#'   \item{schl_data}{Schedule lines relevant for the land use type.}
#'   ...
#' }
#' @source Colorado State University, Yi Yang. Cornell University, S.C. McClelland.
"eq_pnh_sch_template"
#'
#' Southern Hemisphere Rewild Schedule Files.
#'
#' The extended site.100 files for each crop x irrigation x gridid combination.
#' Extended site files identified a priori based on land use cohort selection criteria
#' for simulations.
#'
#' @format ## `eq_psh_sch_template`
#' A data frame with 1970 rows and 2 columns:
#' \describe{
#'   \item{schl_name}{Schedule File Name}
#'   \item{schl_data}{Schedule lines relevant for the land use type.}
#'   ...
#' }
#' @source Colorado State University, Yi Yang. Cornell University, S.C. McClelland.
"eq_psh_sch_template"
