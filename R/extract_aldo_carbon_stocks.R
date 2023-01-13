#' Extract Aldo carbon stocks
#'
#' Extracts the data for the amount of carbon stored in soils from the Aldo
#' Excel tool.
#'
#' @usage
#' extract_aldo_carbon_stocks()
#'
#' @param path_to_aldo the path to the excel tool Aldo
#'
#' @return A data.table object with carbon stocks ratio in different tanks
#'
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
#'
extract_aldo_carbon_stocks <- function() {

  # path to the the Aldo excel tool
  path_to_aldo <- here("data", "aldo", "base_data",  "Outil ALDO_2021_12.xlsx")

  # retrieve carbon stocks in soils
  soil <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  soil <- as.data.table(soil)
  soil <- soil[, c(2, 9:18)]
  soil <- melt(
    soil,
    id.vars = "EPCI_Siren",
    variable.name = "aldo_soil_category",
    value.name = "soil_carbon_content")


  # retrieve carbon stocks in biomass (without forest)
  biomass_wo_forests <- read_excel(path_to_aldo, sheet = "Ref_Biom_HorsF")
  biomass_wo_forests <- as.data.table(biomass_wo_forests)
  biomass_wo_forests <- biomass_wo_forests[, c(1, 4:13)]
  biomass_wo_forests[is.na(biomass_wo_forests), ] <- 0
  biomass_wo_forests <- unique(biomass_wo_forests)
  biomass_wo_forests <- melt(
    biomass_wo_forests,
    id.vars = c("siren"),
    variable.name = "aldo_biomass_category",
    value.name = "biomass_carbon_content")
  setnames(biomass_wo_forests, "siren", "EPCI_Siren")


  # retrieve carbon stocks in forests
  biomass_forests_1 <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
  biomass_forests_1 <- as.data.table(biomass_forests_1)
  biomass_forests_1 <- biomass_forests_1[, c(1,2, 3, 6)]
  biomass_forests_1[is.na(biomass_forests_1), ] <- 0
  setnames(biomass_forests_1, c("EPCI_Siren","epci_area_surface", "aldo_biomass_category", "biomass_carbon_content"))

  biomass_forests_2 <- read_excel(path_to_aldo, sheet = "Ref_Biom_Peup")
  biomass_forests_2 <- as.data.table(biomass_forests_2)
  biomass_forests_2 <- biomass_forests_2[, c(1,2, 5)]
  biomass_forests_2[is.na(biomass_forests_2), ] <- 0
  biomass_forests_2$aldo_biomass_category <- "Peupleraies"
  setnames(biomass_forests_2, c("EPCI_Siren","epci_area_surface", "biomass_carbon_content", "aldo_biomass_category"))

  biomass_forests <- rbind(biomass_forests_1, biomass_forests_2)
  biomass_forests[, aldo_biomass_category := tolower(aldo_biomass_category)]
  biomass_forests <- biomass_forests[aldo_biomass_category != "total"]

  # retrieve carbon stocks in harvested wood
  harvested_wood <- read_excel(path_to_aldo, sheet = "Ref_Prod_Bois")
  harvested_wood <- as.data.table(harvested_wood)
  harvested_wood <- harvested_wood[, c(1,3, 7, 8, 9)]
  harvested_wood[is.na(harvested_wood), ] <- 0
  setnames(harvested_wood, c("EPCI_Siren","wood_composition", "BO_harvest", "BI_harvest", "BE_harvest"))

  # update outated epcis
  if(!file.exists(here("data", "ign", "epcis_old_new.csv"))){
    update_outdated_epcis()
  }
  else{
    soil <- update_epcis(soil, "EPCI_Siren")
    biomass_wo_forests <- update_epcis(biomass_wo_forests, "EPCI_Siren")
    biomass_forests <- update_epcis(biomass_forests, "EPCI_Siren")
    harvested_wood <- update_epcis(harvested_wood, "EPCI_Siren")
  }

  # save the carbon content estimations in each tank
  fwrite(soil, here("data", "aldo", "downloaded_data", "carbon_content_soil.csv"))
  fwrite(biomass_wo_forests, here("data", "aldo", "downloaded_data", "biomass_wo_forests.csv"))
  fwrite(biomass_forests, here("data", "aldo", "downloaded_data", "biomass_forests.csv"))
  fwrite(harvested_wood, here("data", "aldo", "downloaded_data", "harvested_wood.csv"))


  return(dt)
}


