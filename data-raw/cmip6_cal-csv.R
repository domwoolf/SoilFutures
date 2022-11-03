## code to prepare `cmip6_calendars.csv` dataset goes here

library(data.table)
cmip6_calendars= fread('cmip6_calendars.csv')

usethis::use_data(cmip6_calendars, overwrite = TRUE)
