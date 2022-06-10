
#' Extracts the data for the amount of carbon stored in soils from the Aldo
#' Excel tool.
#'
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames
#' @importFrom readxl read_excel
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

  setnames(dt, "EPCI_Siren", "epci_insee_id")

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
  dt <- dt[, c(2, 4:13)]
  dt <- unique(dt)

  dt <- melt(
    dt,
    id.vars = c("CODE_REG"),
    variable.name = "aldo_biomass_category",
    value.name = "biomass_carbon_content"
  )

  dt[is.na(biomass_carbon_content), biomass_carbon_content := 0]

  setnames(dt, "CODE_REG", "region_insee_id")

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
  dt <- dt[, c(1, 3, 6)]

  setnames(dt, c("epci_insee_id", "aldo_biomass_category", "biomass_carbon_content"))

  dt[, aldo_biomass_category := tolower(aldo_biomass_category)]
  dt <- dt[aldo_biomass_category != "total"]

  return(dt)

}


#' Extracts the data for the amount of carbon stored in forests from the Aldo
#' Excel tool.
#'
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @export
#' @importFrom data.table fwrite
#' @importFrom here here
carbon_content <- function(path_to_aldo) {

  soil <- carbon_content_soil(path_to_aldo)
  biomass_wo_forests <- carbon_content_biomass_wo_forests(path_to_aldo)
  biomass_forests <- carbon_content_biomass_forests(path_to_aldo)

  fwrite(soil, here("data", "carbon_content_soil.csv"))
  fwrite(biomass_wo_forests, here("data", "biomass_wo_forests.csv"))
  fwrite(biomass_forests, here("data", "biomass_forests.csv"))

  return(invisible(0))

}
