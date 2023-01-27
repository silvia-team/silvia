library(data.table)

aldo_Ref_Sols_stocks <- fread(here("data-raw", "aldo_Ref_Sols.csv"))
aldo_Ref_Sols_stocks <- aldo_Ref_Sols_stocks[, c(2, 9:18)]
aldo_Ref_Sols_stocks <- melt(
  aldo_Ref_Sols_stocks,
  id.vars = "EPCI_Siren",
  variable.name = "aldo_soil_category",
  value.name = "soil_carbon_content")

# Corresponding CLC codes 1
aldo_clc_category_art <- fread(here("data-raw", "aldo_clc_category_imp.csv"))
aldo_clc_category_art <- aldo_clc_category_art[, list(aldo_soil_category, clc_category)]

aldo_Ref_Sols_stocks_art <- merge(aldo_clc_category_art, aldo_Ref_Sols_stocks,
                         by = "aldo_soil_category",
                         all = TRUE, allow.cartesian=TRUE)
setnames(aldo_Ref_Sols_stocks_art, "soil_carbon_content", "soil_carbon_content_art")

# Corresponding CLC codes 2
aldo_clc_category_enh <- fread(here("data-raw", "aldo_clc_category_enh.csv"))
aldo_clc_category_enh <-  aldo_clc_category_enh[, list(aldo_soil_category_enh, clc_category)]

aldo_Ref_Sols_stocks_enh <- merge(aldo_clc_category_enh, aldo_Ref_Sols_stocks,
                             by.x = "aldo_soil_category_enh",
                             by.y = "aldo_soil_category")
setnames(aldo_Ref_Sols_stocks_enh, "soil_carbon_content", "soil_carbon_content_enh")
aldo_Ref_Sols_stocks_enh <- aldo_Ref_Sols_stocks_enh[, list(EPCI_Siren, clc_category, soil_carbon_content_enh)]

# Merge all
aldo_Ref_Sols_stocks <- merge(aldo_Ref_Sols_stocks_art, aldo_Ref_Sols_stocks_enh,
                         by= c("EPCI_Siren", "clc_category"), all = T)

aldo_Ref_Sols_stocks <- aldo_Ref_Sols_stocks[!is.na(clc_category) & !is.na(EPCI_Siren), ]

aldo_Ref_Sols_stocks <- update_epcis(aldo_Ref_Sols_stocks, "EPCI_Siren")

usethis::use_data(aldo_Ref_Sols_stocks, overwrite = TRUE)


