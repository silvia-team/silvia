
#' Retrieve the carbon flows between two years, of a chosen region
#' @param year_from
#' @param year_to
#' @return a sf object with the carbon flows of the region
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom here here
get_carbon_flows <- function(year_from, year_to) {

  dt_geom <- silvia::get_land_use_changes(year_from, year_to, remove_unchanged = FALSE, count =T)
  dt <- st_drop_geometry(dt_geom)
  epcis <- unique(dt$EPCI_Siren)

  forest_codes <- c(311, 312, 313, 324)

  delta_years = year_to - year_from
  load(here("data", "epci.rda"))

  #biomass

  biomass_flows_wo_forests <- as.data.table(read.csv(here("data", "aldo","biomass_flows_wo_forests.csv")))
  biomass_flows_wo_forests <- biomass_flows_wo_forests[biomass_flows_wo_forests$EPCI_Siren %in% epcis,]
  biomass_flows_wo_forests <- biomass_flows_wo_forests[!is.na(from_clc)]
  biomass_flows_wo_forests[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  biomass_flows_wo_forests[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]
  biomass_flows_wo_forests <- unique(biomass_flows_wo_forests)
  # biomass_flows_wo_forests <- biomass_flows_wo_forests %>% select(-c(EPCI_Siren))

  # Convert to CO2eq
  biomass_flows_wo_forests[, flow := flow*44/12]


  dt <- merge(dt,
              biomass_flows_wo_forests,
              by.x = c("EPCI_Siren", "code_initial", "code_final"),
              by.y = c("EPCI_Siren", "from_clc", "to_clc"),
              all.x = T)

  dupli_ids <- dt$ID[duplicated(dt$ID_unique)]
  dt_unique <- dt[!(dt$ID_unique %in% dupli_ids), ]
  dt_dupl <- dt[dt$ID_unique %in% dupli_ids, ]
  dt_dupl <- dt_dupl  %>%
    group_by(EPCI_Siren, ID_unique, code_initial, code_final, soil_category_initial,
             biomass_category_initial, soil_category_final, biomass_category_final,
             area, code_initial_first, code_final_first, unit) %>%
    summarise(
              X_mean_initial = mean(X_mean_initial),
              X_mean_final = mean(X_mean_final),
              artif_rate = X_mean_final - X_mean_initial,
              from_id = paste0(from_id, collapse = " , "),
              to_id = paste0(to_id, collapse = " , "),
              max_flow = max(flow),
              min_flow = min(flow)
              )

  dt_dupl$flow <-0
  dt_dupl$flow[dt_dupl$artif_rate < 0] <- dt_dupl$min_flow[dt_dupl$artif_rate < 0]
  dt_dupl$flow[dt_dupl$artif_rate > 0] <-
    dt_dupl$max_flow* dt_dupl$artif_rate + dt_dupl$min_flow* (1-dt_dupl$artif_rate)
  dt_dupl <- dt_dupl  %>% select(-c(artif_rate, max_flow, min_flow))

  dt <- rbind(dt_unique, dt_dupl)

  dt <- setnames(dt, c("flow"), c("flow_biomass"))
  dt <- dt %>% select(-c(from_id, to_id))
  dt$unit <- "tC/ha"

  #soils

  soil_flows <- as.data.table(read.csv(here("data", "aldo", "soil_flows.csv")))
  soil_flows <- soil_flows[soil_flows$EPCI_Siren  %in% epcis]
  soil_flows[, flow := ifelse(is.na(flow), 0, flow)]
  soil_flows[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  soil_flows[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]

  # soil_flows <- soil_flows %>% select(-c(EPCI_Siren))


  # Add litters
  `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
  soil_flows$flow[soil_flows$from_clc %in% c(311,312,313,324) & !(soil_flows$to_clc %in% c(311,312,313,324,141)) ] %+=% -9
  soil_flows$flow[!(soil_flows$from_clc %in% c(311,312,313,324)) & soil_flows$to_clc %in% c(311,312,313,324,141) ] %+=% 9

  # soil_flows <- soil_flows[, list(from_clc, to_clc, flow)]
  soil_flows <- unique(soil_flows)

  # Convert to CO2eq
  soil_flows[, flow := flow*44/12]


  ### Add N2O flux related to carbon destocking in soils and litter
  soil_flows[, flow_N2O := ifelse(flow < 0.0,
                                  flow *(1/15*0.01*44/25 + 1/15*0.3*0.0075*44/28)*298
                                  ,0.0)]

  dt <- merge(dt,
              soil_flows,
              by.x = c("EPCI_Siren", "code_initial", "code_final", "unit"),
              by.y = c("EPCI_Siren", "from_clc", "to_clc", "unit"),
              all.x = T)


  dupli_ids <- dt$ID[duplicated(dt$ID_unique)]
  dt_unique <- dt[!(dt$ID_unique %in% dupli_ids), ]
  dt_dupl <- dt[dt$ID_unique %in% dupli_ids, ]
  dt_dupl <- dt_dupl  %>%
    group_by(EPCI_Siren, ID_unique, code_initial, code_final, soil_category_initial,
             biomass_category_initial, soil_category_final, biomass_category_final,
             area, code_initial_first, code_final_first, flow_biomass, unit) %>%
    summarise(
      X_mean_initial = mean(X_mean_initial),
      X_mean_final = mean(X_mean_final),
      artif_rate = X_mean_final - X_mean_initial,
      from_id = paste0(from_id, collapse = " , "),
      to_id = paste0(to_id, collapse = " , "),
      max_flow = max(flow),
      min_flow = min(flow),
      max_flow_N2O = max(flow_N2O),
      min_flow_N2O = min(flow_N2O)
    )

  dt_dupl$flow <-0
  dt_dupl$flow[dt_dupl$artif_rate < 0] <- dt_dupl$min_flow[dt_dupl$artif_rate < 0]
  dt_dupl$flow[dt_dupl$artif_rate > 0] <-
    dt_dupl$max_flow* dt_dupl$artif_rate + dt_dupl$min_flow* (1-dt_dupl$artif_rate)

  dt_dupl$flow_N2O <-0
  dt_dupl$flow_N2O[dt_dupl$artif_rate < 0] <- dt_dupl$min_flow_N2O[dt_dupl$artif_rate < 0]
  dt_dupl$flow_N2O[dt_dupl$artif_rate > 0] <-
    dt_dupl$max_flow_N2O* dt_dupl$artif_rate + dt_dupl$min_flow_N2O* (1-dt_dupl$artif_rate)

  dt_dupl <- dt_dupl  %>% select(-c(artif_rate, max_flow, min_flow, max_flow_N2O, min_flow_N2O))

  dt <- rbind(dt_unique, dt_dupl)

  dt <- setnames(dt, c("flow"), c("flow_soil"))
  dt <- dt %>% select(-c(from_id, to_id))
  dt$unit <- "tC/ha"


  dt$soil_category_initial[!is.na(dt$from_id)] <- dt$from_id[!is.na(dt$from_id)]
  dt$soil_category_final[!is.na(dt$to_id)] <- dt$to_id[!is.na(dt$to_id)]

  # dt <- dt  %>% select(-c(from_id, to_id))


  # # forests
  forest_flows <- as.data.table(read.csv(here("data", "aldo","forest_flows.csv")))
  forest_flows <- forest_flows[forest_flows$EPCI_Siren %in% epcis,]
  forest_flows[, flow := ifelse(is.na(flow), 0, flow)]
  forest_flows <- forest_flows[, list(EPCI_Siren, flow,clc_category)]

  # Convert to CO2eq
  forest_flows[, flow := flow*44/12]

  dt <- merge(dt,
              forest_flows,
              by.x = c("EPCI_Siren", "code_final"),
              by.y = c("EPCI_Siren", "clc_category"),
              all.x = T)
  dt <- setnames(dt, c("flow"), c("flow_forest"))
  dt$flow_forest[is.na(dt$flow_forest)] <- 0

  dt$flow_biomass[dt$code_initial == dt$code_final] <- 0
  dt$flow_soil[dt$code_initial == dt$code_final] <- 0
  dt$flow_N2O[dt$code_initial == dt$code_final] <- 0

  # Convert to tCO2/ha.year
  dt$flow_biomass <- -dt$flow_biomass/delta_years
  dt$flow_soil <- - dt$flow_soil/delta_years
  dt$flow_N2O <- - dt$flow_N2O/delta_years
  dt$flow_forest <- - dt$flow_forest
  dt$flow_forest[dt$code_initial != dt$code_final] <- dt$flow_forest/delta_years

  dt$flow_biomass[is.na(dt$flow_biomass)] <- 0
  dt$flow_soil[is.na(dt$flow_soil)] <- 0
  dt$flow_N2O[is.na(dt$flow_N2O)] <- 0

  dt$total_flows <- round(dt$flow_soil + dt$flow_biomass + dt$flow_forest + dt$flow_N2O, digits =1)

  dt[is.na(dt)] <- 0

  dt <- merge(dt, dt_geom %>% select(ID_unique), by= "ID_unique")

  dt <- st_as_sf(dt)

  return(dt)

}



#' Compute total carbon flows between two years, of a chosen region.
#' Four flows categories : biomass from land use changes, soil from land
#' use changes, total forest flows (photosynthesis), total area flows
#' @param flows st object returned by 'get_carbon_flows' funciton
#' @return a data.table with the flows in ktCO2/year
#' @export
#' @importFrom data.table data.table
#' @importFrom sf st_area
get_total_carbon_flows <- function(flows){

  ### Compute total flows on the territory
  flows$area <- st_area(flows)

  total_biomass_flows <- round(sum(flows$area * 1e-04 * 1e-03 * flows$flow_biomass),digits = 2)
  total_biomass_flows <- as.numeric(total_biomass_flows)

  total_soil_flows <- round(sum(flows$area * 1e-04 *1e-03 *  (flows$flow_soil + flows$flow_N2O)), digits = 2)
  total_soil_flows <- as.numeric(total_soil_flows)

  total_forest_flows <- round(sum(flows$area * 1e-04 *1e-03 *  flows$flow_forest), digits = 2)
  total_forest_flows <- as.numeric(total_forest_flows)

  total_area_flows <- sum(flows$area * 1e-04 *1e-03 *  flows$total_flows)
  total_area_flows <- round(as.numeric(total_area_flows), digits = 2)

  flows_category <- c("biomass_flows_from_land_use_changes", "soil_flows_from_land_use_changes",
                      "total_forest_flows", "total_area_flows")

  flows_value <- c(total_biomass_flows, total_soil_flows, total_forest_flows, total_area_flows)

  dt_total_flows <- data.table(flows_category, flows_value)

  return(dt_total_flows)


}

