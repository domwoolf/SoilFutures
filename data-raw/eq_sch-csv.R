## code to prepare rewild schedule template datasets as .rda goes here

library(data.table)
eq_pnh_sch_template = fread('data-raw/eq_sch_pnh-template.csv')
eq_psh_sch_template = fread('data-raw/eq_sch_psh-template.csv')

usethis::use_data(eq_pnh_sch_template, overwrite = TRUE)
usethis::use_data(eq_psh_sch_template, overwrite = TRUE)
