## code to prepare `aldo_ref_stocks_soil` dataset

# path to the the Aldo excel tool
path_to_aldo <- here("data", "aldo", "base_data",  "Outil ALDO_2021_12.xlsx")


# retrieve carbon stocks in biomass (without forest)
aldo_ref_stocks_biomass_wo_forests <- read_excel(path_to_aldo, sheet = "Ref_Biom_HorsF")
aldo_ref_stocks_biomass_wo_forests <- as.data.table(aldo_ref_stocks_biomass_wo_forests)
aldo_ref_stocks_biomass_wo_forests <- aldo_ref_stocks_biomass_wo_forests[, c(1, 4:13)]
aldo_ref_stocks_biomass_wo_forests[is.na(aldo_ref_stocks_biomass_wo_forests), ] <- 0
aldo_ref_stocks_biomass_wo_forests <- unique(aldo_ref_stocks_biomass_wo_forests)
aldo_ref_stocks_biomass_wo_forests <- melt(
  aldo_ref_stocks_biomass_wo_forests,
  id.vars = c("siren"),
  variable.name = "aldo_biomass_category",
  value.name = "biomass_carbon_content")
setnames(aldo_ref_stocks_biomass_wo_forests, "siren", "EPCI_Siren")

aldo_ref_stocks_biomass_wo_forests <- update_epcis(aldo_ref_stocks_biomass_wo_forests, "EPCI_Siren")

usethis::use_data(aldo_ref_stocks_biomass_wo_forests, overwrite = T)

path_to_aldo <- here("data", "aldo", "base_data",  "Outil ALDO_2021_12.xlsx")

sheets <- c("Ref_Sols", "Ref_Biom_HorsF", "Ref_Biom_foret", "Ref_Prod_Bois", "Ref_Biom_Peup")

Ref_Sols <- read_excel(path_to_aldo, sheet = "Ref_Sols")
Ref_Sols <- as.data.table(Ref_Sols)
fwrite(Ref_Sols, file = here("data", "aldo_Ref_Sols.csv"))

use_data_raw(name = "aldo_Ref_Sols_stocks")
use_data_raw(name = "aldo_Ref_Sols_flows")

Ref_Biom_HorsF <- read_excel(path_to_aldo, sheet = "Ref_Biom_HorsF")
Ref_Biom_HorsF <- as.data.table(Ref_Biom_HorsF)
fwrite(Ref_Biom_HorsF, file = here("data", "aldo_Ref_Biom_HorsF.csv"))

use_data_raw(name = "aldo_Ref_Biom_HorsF_stocks")
use_data_raw(name = "aldo_Ref_Biom_HorsF_flows")


Ref_Biom_foret <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
Ref_Biom_foret <- as.data.table(Ref_Biom_foret)
fwrite(Ref_Biom_foret, file = here("data", "aldo_Ref_Biom_foret.csv"))

use_data_raw(name = "aldo_Ref_Biom_foret_stocks")
use_data_raw(name = "aldo_Ref_Biom_foret_flows")

Ref_Prod_Bois <- read_excel(path_to_aldo, sheet = "Ref_Prod_Bois")
Ref_Prod_Bois <- as.data.table(Ref_Prod_Bois)
fwrite(Ref_Prod_Bois, file = here("data", "aldo_Ref_Prod_Bois.csv"))

use_data_raw(name = "aldo_Ref_Prod_Bois")

Ref_Biom_Peup <- read_excel(path_to_aldo, sheet = "Ref_Biom_Peup")
Ref_Biom_Peup <- as.data.table(Ref_Biom_Peup)
fwrite(Ref_Biom_Peup, file = here("data", "aldo_Ref_Biom_Peup.csv"))

use_data_raw(name = "aldo_Ref_Biom_Peup")

path_to_aldo_clc <- here("data", "aldo", "base_data", "aldo_clc_categories.xlsx")

aldo_clc_category_imp <- read_excel(path_to_aldo_clc, sheet = "imp")
aldo_clc_category_imp <- as.data.table(aldo_clc_category_imp)
fwrite(aldo_clc_category_imp, file = here("data", "aldo_clc_category_imp.csv"))

aldo_clc_category_enh <- read_excel(path_to_aldo_clc, sheet = "enh")
aldo_clc_category_enh <- as.data.table(aldo_clc_category_enh)
fwrite(aldo_clc_category_enh, file = here("data", "aldo_clc_category_enh.csv"))



