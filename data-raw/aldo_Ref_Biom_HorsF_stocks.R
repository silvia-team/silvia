library(data.table)

aldo_Ref_Biom_HorsF_stocks <- fread(here("data-raw", "aldo_Ref_Biom_HorsF.csv"))
aldo_Ref_Biom_HorsF_stocks <- aldo_Ref_Biom_HorsF_stocks[, c(1, 4:13)]
aldo_Ref_Biom_HorsF_stocks[is.na(aldo_Ref_Biom_HorsF_stocks), ] <- 0
aldo_Ref_Biom_HorsF_stocks <- unique(aldo_Ref_Biom_HorsF_stocks)
aldo_Ref_Biom_HorsF_stocks <- melt(
  aldo_Ref_Biom_HorsF_stocks,
  id.vars = c("siren"),
  variable.name = "aldo_biomass_category",
  value.name = "biomass_carbon_content")
setnames(aldo_Ref_Biom_HorsF_stocks, "siren", "EPCI_Siren")

aldo_Ref_Biom_HorsF_stocks <- update_epcis(aldo_Ref_Biom_HorsF_stocks, "EPCI_Siren")


# Corresponding CLC codes 1

aldo_clc_category_imp <- fread(here("data-raw", "aldo_clc_category_imp.csv"))
aldo_clc_category_imp <- aldo_clc_category_imp[, c(1, 3, 5)]


aldo_Ref_Biom_HorsF_stocks_imp <- merge(aldo_Ref_Biom_HorsF_stocks,
                                    aldo_clc_category_imp,
                                    by= "aldo_biomass_category", allow.cartesian = T)

setnames(aldo_Ref_Biom_HorsF_stocks_imp, "biomass_carbon_content", "biomass_carbon_content_art")


# Corresponding CLC codes 2

aldo_clc_category_arb <- fread(here("data-raw", "aldo_clc_category_enh.csv"))
aldo_clc_category_arb <- aldo_clc_category_arb[, list(aldo_biomass_category_arb, clc_category)]

aldo_Ref_Biom_HorsF_stocks_arb <- merge(aldo_Ref_Biom_HorsF_stocks, aldo_clc_category_arb,
                        by.x= "aldo_biomass_category",
                        by.y ="aldo_biomass_category_arb")

aldo_Ref_Biom_HorsF_stocks_arb <- aldo_Ref_Biom_HorsF_stocks_arb[, list(
  EPCI_Siren,
  clc_category,
  biomass_carbon_content_arb= biomass_carbon_content)]

aldo_Ref_Biom_HorsF_stocks <- merge(aldo_Ref_Biom_HorsF_stocks_imp, aldo_Ref_Biom_HorsF_stocks_arb,
                   by= c("EPCI_Siren", "clc_category"), all = T)

aldo_Ref_Biom_HorsF_stocks <- update_epcis(aldo_Ref_Biom_HorsF_stocks, "EPCI_Siren")

usethis::use_data(aldo_Ref_Biom_HorsF_stocks, overwrite = TRUE)







