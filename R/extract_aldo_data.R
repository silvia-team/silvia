##############################
# Load libraries

library("readxl")
library("data.table")
library("here")
library("dplyr")

##############################

#' Extracts the data for the amount of carbon stored in soils from the Aldo
#' Excel tool.
#'
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
#'
#'
carbon_content_soil <- function(path_to_aldo) {

  dt <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  dt <- as.data.table(dt)
  dt <- dt[, c(2, 9:18)]

  dt <- melt(
    dt,
    id.vars = "EPCI_Siren",
    variable.name = "aldo_soil_category",
    value.name = "soil_carbon_content"
  )


  return(dt)
}


#' Extracts the data for the amount of carbon stored in biomass outside of
#' forests from the Aldo Excel tool.
#'
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
carbon_content_biomass_wo_forests <- function(path_to_aldo) {

  dt <- read_excel(path_to_aldo, sheet = "Ref_Biom_HorsF")
  dt <- as.data.table(dt)
  dt <- dt[, c(1, 4:13)]
  dt <- unique(dt)

  dt <- melt(
    dt,
    id.vars = c("siren"),
    variable.name = "aldo_biomass_category",
    value.name = "biomass_carbon_content"
  )

  dt[is.na(biomass_carbon_content), biomass_carbon_content := 0]

  setnames(dt, "siren", "EPCI_Siren")

  return(dt)

}


#' Extracts the data for the amount of carbon stored in forests from the Aldo
#' Excel tool.
#'
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
carbon_content_biomass_forests <- function(path_to_aldo) {

  dt <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
  dt <- as.data.table(dt)
  dt <- dt[, c(1,2, 3, 6)]

  setnames(dt, c("EPCI_Siren","epci_area_surface", "aldo_biomass_category", "biomass_carbon_content"))

  dt[, aldo_biomass_category := tolower(aldo_biomass_category)]
  dt <- dt[aldo_biomass_category != "total"]

  return(dt)

}

#' Extracts the data for the amount of carbon stored in harvested wood from the Aldo
#' Excel tool.
#'
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
carbon_content_harvested_wood <- function(path_to_aldo) {

  dt <- read_excel(path_to_aldo, sheet = "Ref_Prod_Bois")
  dt <- as.data.table(dt)
  dt <- dt[, c(1,3, 7, 8, 9)]

  setnames(dt, c("EPCI_Siren","wood_composition", "BO_harvest", "BI_harvest", "BE_harvest"))

  return(dt)

}



#' Save the data of the amount of carbon stored in soils (1), in the biomass outside
#' of forests (2), and in forests (3) from the ALDO Excel tool.
#' The data is stored in the "data" directory
#' @param path_to_aldo
#'
#' @return None
#' @export
#' @importFrom data.table fwrite
#' @importFrom here here
carbon_content <- function(path_to_aldo) {

  soil <- carbon_content_soil(path_to_aldo)
  biomass_wo_forests <- carbon_content_biomass_wo_forests(path_to_aldo)
  biomass_forests <- carbon_content_biomass_forests(path_to_aldo)
  harvested_wood <- carbon_content_harvested_wood(path_to_aldo)

  fwrite(soil, here("data", "carbon_content_soil.csv"))
  fwrite(biomass_wo_forests, here("data", "biomass_wo_forests.csv"))
  fwrite(biomass_forests, here("data", "biomass_forests.csv"))
  fwrite(harvested_wood, here("data", "harvested_wood.csv"))

  return(invisible(0))

}
# path_to_aldo <- "../../ALDO/Outil ALDO_2021_12.xlsx"
# # dt <- carbon_content(path_to_aldo)
#
# dt_carbon_flows <- carbon_flows_soil(path_to_aldo)
# dt_biomass_flows <- carbon_flows_biomass_wo_forest(path_to_aldo)

