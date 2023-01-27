
library(data.table)

aldo_clc_category <- fread(here("data-raw", "aldo_clc_category_enh.csv"))

usethis::use_data(aldo_clc_category, overwrite = TRUE)
