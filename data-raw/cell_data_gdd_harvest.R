## code to prepare individual gcm x spp growing degree day harvest dates as .rda goes here

library(data.table)
gdd_files         = paste(getwd(), 'data-raw/gcm-ssp-harvest-dates', '',sep = '/')
GDD_harvest_dates = list.files(gdd_files, pattern = 'GDD-harvest-dates.rds', full.names = TRUE)
GDD_harvest_dates2 = list.files(gdd_files, pattern = 'GDD-harvest-dates-2.rds', full.names = TRUE)
GDD_harvest_dates  = c(GDD_harvest_dates, GDD_harvest_dates2)
# bind into one table
GDD_harvest_dates = lapply(GDD_harvest_dates, readRDS)
GDD_harvest_dates = rbindlist(GDD_harvest_dates, use.names = TRUE, idcol = NULL)
setDT(GDD_harvest_dates)
GDD_harvest_dates = setorder(GDD_harvest_dates, gridid, crop)
# update column names
old.names         = colnames(GDD_harvest_dates[,7:91])
new.names         = outer('<harvest_day_', paste0(as.character(1:85),'>'), paste0)
setnames(GDD_harvest_dates, old = c(old.names), new = c(new.names))

# replace NA's with 0's
GDD_harvest_dates[is.na(GDD_harvest_dates)] = 0

# make unique table
GDD_harvest_dates = unique(GDD_harvest_dates)

# OPTIONAL - Filter by reduced gridid (> 1 ha)
usethis::use_data(GDD_harvest_dates, overwrite = TRUE)
