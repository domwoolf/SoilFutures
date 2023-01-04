## code to prepare `cover_crop_template` dataset as .rda goes here

library(data.table)
cover_crop_template = fread('data-raw/crop_full-sf.100', sep = '', header = FALSE)

usethis::use_data(cover_crop_template, overwrite = TRUE)
