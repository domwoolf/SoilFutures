% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule.R
\name{create_csu_sched}
\alias{create_csu_sched}
\title{Create a DayCent Schedule file for CSU extended simulations}
\usage{
create_csu_sched(
  cell_data,
  schedule_table = copy(schedule_template),
  .gridid,
  .ssp,
  .gcm,
  .crop,
  .scenario,
  .irr,
  start_year,
  end_year,
  weather_fname,
  tmp.dir
)
}
\arguments{
\item{cell_data}{multiple row data.table providing input data for the simulation.}

\item{schedule_table}{data.table providing template of schedule file.}

\item{.gridid}{unique cell value to be passed to function.}

\item{.ssp}{character, specifies scenario}

\item{.gcm}{character, specifies scenario}

\item{.crop}{character, one of 'maiz', 'wwheat', 'swheat', 'soy'}

\item{.scenario}{character, specifies scenario}

\item{.irr}{binary, specifies irrigation level}

\item{end_year}{integer, specifies end year of simulation}

\item{weather_fname}{name of input weather file}

\item{tmp.dir}{directory to create in tmp.path, set with arg[1]}

\item{start_yr}{integer, specifies start year of simulation}
}
\value{
invisibly returns boolean indicating whether file was written successfully.
}
\description{
Used for writing a schedule file based on spatial data and CSU spin-ups.
}
\details{
This function creates a schedule file for a specified scenario and crop,
using spatial climate, crop calendar, and crop management data.
}
