library(data.table)
library(here)

# retrieve carbon stocks in forests
aldo_Ref_Biom_foret <- fread(here("data-raw", "aldo_Ref_Biom_foret.csv"))
aldo_Ref_Biom_foret <- aldo_Ref_Biom_foret[, c(1,2, 3, 6)]
aldo_Ref_Biom_foret[is.na(aldo_Ref_Biom_foret), ] <- 0
setnames(aldo_Ref_Biom_foret, c("EPCI_Siren","epci_area_surface", "aldo_biomass_category", "biomass_carbon_content"))

aldo_Ref_Biom_Peup <- fread(here("data-raw", "aldo_Ref_Biom_Peup.csv"))
aldo_Ref_Biom_Peup <- aldo_Ref_Biom_Peup[, c(1,2, 5)]
aldo_Ref_Biom_Peup[is.na(aldo_Ref_Biom_Peup), ] <- 0
aldo_Ref_Biom_Peup$aldo_biomass_category <- "Peupleraies"
setnames(aldo_Ref_Biom_Peup, c("EPCI_Siren","epci_area_surface", "biomass_carbon_content", "aldo_biomass_category"))

aldo_Ref_Biom_foret_stocks <- rbind(aldo_Ref_Biom_foret, aldo_Ref_Biom_Peup)
aldo_Ref_Biom_foret_stocks[, aldo_biomass_category := tolower(aldo_biomass_category)]
aldo_Ref_Biom_foret_stocks <- aldo_Ref_Biom_foret_stocks[aldo_biomass_category != "total", ]
aldo_Ref_Biom_foret_stocks <- unique(aldo_Ref_Biom_foret_stocks)

# Corresponding CLC codes
aldo_clc_category_imp <- fread(here("data-raw", "aldo_clc_category_imp.csv"))
aldo_clc_category_imp <- aldo_clc_category_imp[, c(3, 5)]

aldo_Ref_Biom_foret_stocks <- merge(aldo_Ref_Biom_foret_stocks, aldo_clc_category_imp, by= "aldo_biomass_category",
                                    allow.cartesian = T)

aldo_Ref_Biom_foret_stocks <- aldo_Ref_Biom_foret_stocks[!is.na(clc_category) & !is.na(EPCI_Siren), ]

aldo_Ref_Biom_foret_stocks <- update_epcis(aldo_Ref_Biom_foret_stocks, "EPCI_Siren")

usethis::use_data(aldo_Ref_Biom_foret_stocks, overwrite = TRUE)
