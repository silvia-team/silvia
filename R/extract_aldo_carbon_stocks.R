
#' Extracts the data for the amount of carbon stored in soils from the Aldo
#' Excel tool.
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
#'
extract_aldo_carbon_stocks <- function() {

  path_to_aldo <- here("data", "aldo", "base_data",  "Outil ALDO_2021_12.xlsx")

  soil <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  soil <- as.data.table(soil)
  soil <- soil[, c(2, 9:18)]
  soil <- melt(
    soil,
    id.vars = "EPCI_Siren",
    variable.name = "aldo_soil_category",
    value.name = "soil_carbon_content")



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



  dt1 <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
  dt1 <- as.data.table(dt1)
  dt1 <- dt1[, c(1,2, 3, 6)]
  dt1[is.na(dt1), ] <- 0
  setnames(dt1, c("EPCI_Siren","epci_area_surface", "aldo_biomass_category", "biomass_carbon_content"))

  dt2 <- read_excel(path_to_aldo, sheet = "Ref_Biom_Peup")
  dt2 <- as.data.table(dt2)
  dt2 <- dt2[, c(1,2, 5)]
  dt2[is.na(dt2), ] <- 0
  dt2$aldo_biomass_category <- "Peupleraies"
  setnames(dt2, c("EPCI_Siren","epci_area_surface", "biomass_carbon_content", "aldo_biomass_category"))

  biomass_forests <- rbind(dt1, dt2)
  biomass_forests[, aldo_biomass_category := tolower(aldo_biomass_category)]
  biomass_forests <- biomass_forests[aldo_biomass_category != "total"]


  harvested_wood <- read_excel(path_to_aldo, sheet = "Ref_Prod_Bois")
  harvested_wood <- as.data.table(harvested_wood)
  harvested_wood <- harvested_wood[, c(1,3, 7, 8, 9)]
  harvested_wood[is.na(harvested_wood), ] <- 0

  if(!file.exists(here("data", "ign", "epcis_old_new.csv"))){
    write_update_epcis()
  }
  else{
    soil <- update_epcis(soil, "EPCI_Siren")
    biomass_wo_forests <- update_epcis(biomass_wo_forests, "EPCI_Siren")
    biomass_forests <- update_epcis(biomass_forests, "EPCI_Siren")
    harvested_wood <- update_epcis(harvested_wood, "SIREN_EPCI")
  }

  fwrite(soil, here("data", "aldo", "carbon_content_soil.csv"))
  fwrite(biomass_wo_forests, here("data", "aldo","biomass_wo_forests.csv"))
  fwrite(biomass_forests, here("data", "aldo", "biomass_forests.csv"))
  fwrite(harvested_wood, here("data", "aldo", "harvested_wood.csv"))

  setnames(harvested_wood, c("EPCI_Siren","wood_composition", "BO_harvest", "BI_harvest", "BE_harvest"))

  return(dt)
}




















