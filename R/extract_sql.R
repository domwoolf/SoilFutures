# extract_sql.R
#
# contains code to extract extended site file from SQL database for DayCent simulations.
#
#'@import DBI
#'@import RMySQL
#'@import rstudioapi
#'@import data.table
#'
#'@param db.name is database name (ie 'GlobSim_hist)
#'@param host.name is host name (ie 'localhost)
#'@param port.no is port number (ie 3306)
#'@param user.name is user name ('shelby')

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

select.tbl = function(con, cohort) {
  table.list = dbListTables(con)
  table      = grep(cohort, table.list, value = TRUE)
  return(table)
}

sql.query = function(con, table, gridid, run_seq) {
  result = dbSendQuery(con, paste('SELECT site100 FROM', table,
                                  'WHERE gridid =',gridid,
                                  'AND run_seq = ',run_seq))
  result.dt = fetch(result)
  fwrite(result.dt, paste(pkg.env$out_path, '/',gridid,'_',run_seq,'_','site.100', sep = ''), quote = FALSE, col.names = FALSE)
  return(result.dt)
}
### Below notes on running an extended site.100 ###
# 5) Run DayCent:
#   I usually run it by:
# a) pathing the name of the build
# b) pathing the folder containing the set of 100 files (using the -l switch)
# c) supplying the name of the schedule file
# d) providing an over-ride to the site file (--site <site file name>)
#
# Note: the site file is usually an extended site file with the values from the EQ & Spin-up runs
# stored as ending values that become the starting values used by the current run. This file also stores the soils data,
# so there is no explicit use of a soils file when running using an NRI site from the Inventory.
#
# e) provide a log file name which can be helpful
# /data/ogle/Daycent/Builds/Trunk/DDcentEVI_Trunk -l /data/ogle/Daycent/Builds/Trunk/100_Files/INV2018 -s NRI_32415_GC.sch -n --site SU_ext_site.100 >! NRI_GC_run.log
#
# This should generate the BIN file and any outfile specified in the outfiles.in file.
#
# 6) Run the LIS file extraction to get the variables out of the BIN file:
# - The 1st parameter is the name of the BIN file, which is usually the same as the schedule file name
# - The 2nd Parameter is the name of the LIS file to be generated, again usually the same as the SchlFile name, but you can change it to what ever you really want.
# - The 3rd parameter specifies the outvars.txt file using the '-c outvars.txt' switch
# /data/ogle/Daycent/Builds/Trunk/DDlist100 NRI_32415_GC NRI_32415_GC -c outvars.txt
