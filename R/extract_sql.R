# extract_sql.R
#
# contains code to extract extended site file from SQL database for DayCent simulations.
#
#' Create a connection to MySQL database
#'@import DBI
#'@import RMySQL
#'@import rstudioapi
#'@import data.table
#'
#'@param db.name is database name (ie 'GlobSim_hist)
#'@param host.name is host name (ie 'localhost)
#'@param port.no is port number (ie 3306)
#'@param user.name is user name ('shelby')
#'
#'@export
establish.con = function(db.name, host.name, port.no, user.name) {
  con = dbConnect(RMySQL::MySQL(),
    dbname   = db.name,
    host     = host.name,
    port     = port.no,
    user     = user.name,
    password = rstudioapi::askForPassword("Database password"),
  )
  return(con)
}
#' Select a table from MySQL database
#'@import DBI
#'@import RMySQL
#'@import rstudioapi
#'@import data.table
#'
#'@param con connection created to MySQL database
#'@param cohort the cohort table (ie 'lu')
#'
#'@export
select.tbl = function(con, cohort) {
  table.list = dbListTables(con)
  table      = grep(cohort, table.list, value = TRUE)
  return(table)
}
#' Query table in MySQL database
#'@import DBI
#'@import RMySQL
#'@import rstudioapi
#'@import data.table
#'
#'@param con connection created to MySQL database
#'@param table the cohort table in MySQL database
#'@param run_seq_data vector of run_sequences to extract
#'
#'@export
sql.query = function(con, table, run_seq_data) {
  result = dbSendQuery(con, paste('SELECT gridid, irr, run_seq, site100 FROM', table,
                                  'WHERE run_seq = (', run_seq_data,')'))
  result.dt = dbFetch(result, n = -1)
  result.dt = setDT(result.dt)
}
