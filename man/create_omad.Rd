% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule.R
\name{create_omad}
\alias{create_omad}
\title{Create an omad.100 file}
\usage{
create_omad(cell_data = cell_data, cell, tmp.dir)
}
\arguments{
\item{cell_data}{multiple row data.table providing input data for the simulation.}

\item{cell}{unique cell value to be passed to function.}

\item{tmp.dir}{directory to create in tmp.path, set with arg[1]}
}
\description{
Used for writing an omad.100 file based on spatial data.
}
\details{
This function modifies the omad.100 file by creating a new event entry for a cell
using spatial manure-N and manure-CN data.
}
