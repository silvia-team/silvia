#' Get carbon flows balance
#'
#' @description
#' Compute the carbon flows balance between the two years, of the chosen territory.
#'
#' There are four flows categories :
#' * carbon flows from land use changes (ktCO2e/an)
#' * carbon flows from forest (ktCO2e/an)
#' * carbon flows from harvested wood (ktCO2e/an)
#' * total carbon flows (ktCO2e/an)
#'
#'
#' @param flows sf object returned by `get_carbon_flows()` function
#' @param data_path path to where the data is stored
#'
#' @return a data.table with the flows in ktCO2/year
#'
#' @export
#'
#' @importFrom data.table data.table
#' @importFrom sf st_area
#'
get_carbon_flows_balance <- function(flows, data_path) {

  epcis <- unique(flows$EPCI_Siren)

  ### Compute total flows on the territory
  total_biomass_flows <- round(sum(flows$area * 1e-03 * flows$flow_biomass, na.rm=T),digits = 2)
  total_biomass_flows <- -as.numeric(total_biomass_flows)

  total_soil_flows <- round(sum(flows$area *1e-03 *  (flows$flow_soil - flows$flow_N2O), na.rm= T), digits = 2)
  total_soil_flows <- - as.numeric(total_soil_flows)

  total_harvested_wood_flows <- invisible(aldo_Ref_Prod_Bois_flows)
  total_harvested_wood_flows <- - sum(total_harvested_wood_flows[EPCI_Siren %in% epcis, wood_carbon_flows])

  total_forest_flows <- get_forest_flows(data_path)
  total_forest_flows <- sum(total_forest_flows$total_flows_area, na.rm= T)
  total_forest_flows <- round(total_forest_flows*1e-03* 1e-04, digits = 2)

  total_area_flows <- sum(total_biomass_flows, total_soil_flows, total_forest_flows, total_harvested_wood_flows)
  total_area_flows <- round(as.numeric(total_area_flows), digits = 2)

  flows_category <- c("carbon flows from land use changes (ktCO2e/an)", "carbon flows from forest (ktCO2e/an)",
                      "carbon flows from harvested wood (ktCO2e/an)", "total carbon flows (ktCO2e/an)")

  flows_value <- c(total_biomass_flows + total_soil_flows, total_forest_flows,
                   total_harvested_wood_flows, total_area_flows)

  dt_total_flows <- data.table(flows_category, flows_value)

  return(dt_total_flows)

}
