#' Get carbon stocks balance
#'
#' Compute total carbon stocks at a certain date, of a chosen region.
#' Four flows categories : stocks in biomass in forest, stocks in biomass out forest,
#' stocks in soils, and total cabon stocks
#'
#' @param stocks st object returned by 'get_carbon_storage' function
#'
#' @return a data.table with the flows in ktCO2/year
#'
#' @export
#'
#' @importFrom data.table as.data.table
#' @importFrom sf st_area
get_carbon_stocks_balance <- function(stocks){

  ### Compute total flows on the territory
  # Maybe add the possibility to use BD foret' areas ?

  epcis <- unique(stocks$EPCI_Siren)

  stock_biomass_in_forest <- round(sum(st_area(stocks) * 1e-04 * 1e-03 * stocks$biomass_carbon_content_in_fo),digits = 2)
  stock_biomass_in_forest <- as.numeric(stock_biomass_in_forest)

  stock_biomass_out_forest <- round(sum(st_area(stocks) * 1e-04 * 1e-03 * stocks$biomass_carbon_content_out_fo),digits = 2)
  stock_biomass_out_forest <- as.numeric(stock_biomass_out_forest)

  stock_in_soils <- round(sum(st_area(stocks) * 1e-04 *1e-03 *stocks$soil_carbon_content, na.rm= T), digits = 2)
  stock_in_soils <- as.numeric(stock_in_soils)

  stocks_in_harvested_wood <- invisible(aldo_Ref_Prod_Bois_stocks)
  stocks_in_harvested_wood <- round(sum(stocks_in_harvested_wood[EPCI_Siren %in% epcis, wood_carbon_stocks]*1e-3), digits= 2)

  total_carbon_stocks <- sum(stock_biomass_in_forest, stock_biomass_out_forest+ stock_in_soils+stocks_in_harvested_wood)

  stocks_category <- c("carbon stocks in biomass (ktCO2e)", "carbon stocks in soils (ktCO2e)",
                       "carbon stocks in harvested wood (ktCO2e)", "total carbon stocks (ktCO2e)")

  stocks_value <- c(stock_biomass_in_forest + stock_biomass_out_forest, stock_in_soils, stocks_in_harvested_wood, total_carbon_stocks)

  dt_total_stocks <- data.table(stocks_category, stocks_value)

  return(dt_total_stocks)

}
