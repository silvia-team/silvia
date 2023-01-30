#' Get carbon flows
#'
#' Retrieve the carbon flows between two years, of a chosen region
#'
#' @param year_from the CLC reference year (1990, 2000, 2006, 2012, 2018)
#' @param year_to the year to be compared to the reference year,
#' must be superior to reference year
#' @param data_path path to where the data is stored
#'
#' @return a sf object with the carbon flows of the region
#'
#' @importFrom data.table as.data.table setnames fread
#' @importFrom here here
#'
#' @export
#'
get_carbon_flows <- function(year_from, year_to, data_path) {

  delta_years = year_to - year_from

  dt_geom <- get_land_use_changes(year_from, year_to, data_path)
  dt <- sf::st_drop_geometry(dt_geom)
  dt <- dt %>% filter(area >0)

  epcis <- unique(dt$EPCI_Siren)

  # retrieve carbon flows in biomass (without forest) ------------------------


  biomass_flows_wo_forests <- invisible(aldo_Ref_Biom_HorsF_flows)
  biomass_flows_wo_forests <- biomass_flows_wo_forests[EPCI_Siren %in% epcis,]

  biomass_flows_wo_forests[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  biomass_flows_wo_forests[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]
  biomass_flows_wo_forests <- unique(biomass_flows_wo_forests)


  # Convert to CO2eq
  biomass_flows_wo_forests[, flow := flow*44/12]

  biomass_flows_wo_forests <- biomass_flows_wo_forests[EPCI_Siren %in% epcis, ]

  biomass_flows_wo_forests <- biomass_flows_wo_forests[, list(
    EPCI_Siren= as.character(EPCI_Siren),
    from_clc= as.character(from_clc),
    to_clc= as.character(to_clc),
    from_id, to_id, flow, unit
  )]

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

  dt_dupl <- as.data.table(dt_dupl)
  # dt_dupl$flow <-0
  dt_dupl <- dt_dupl[, flow:= ifelse(artif_rate < 0, min_flow, max_flow*artif_rate + min_flow*(1-artif_rate))]


  dt_dupl <- dt_dupl[, -c("artif_rate", "max_flow", "min_flow")]

  dt <- rbind(dt_unique, dt_dupl)

  dt <- setnames(dt, c("flow"), c("flow_biomass"))
  dt <- dt[, -c("from_id", "to_id")]
  dt$unit <- "tC/ha"
  dt$EPCI_Siren <- as.character(dt$EPCI_Siren)


  # retrieve carbon flows in soil -------------------------------------------


  soil_flows <- invisible(aldo_Ref_Sols_flows)
  soil_flows <- soil_flows[EPCI_Siren  %in% epcis]
  soil_flows[, flow := ifelse(is.na(flow), 0, flow)]
  soil_flows[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  soil_flows[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]

  # Add litters
  soil_flows <- soil_flows[, flow := ifelse(from_clc %in% c(311,312,313,324) & !(soil_flows$to_clc %in% c(311,312,313,324,141)), flow - 9, flow)]
  soil_flows <- soil_flows[, flow := ifelse(!(from_clc %in% c(311,312,313,324)) & soil_flows$to_clc %in% c(311,312,313,324,141), flow + 9, flow)]


  soil_flows <- soil_flows[, list(
    EPCI_Siren= as.character(EPCI_Siren),
    from_clc= as.character(from_clc),
    to_clc= as.character(to_clc),
    from_id, to_id, flow, unit
  )]

  # Convert to CO2eq
  soil_flows[, flow := flow*44/12]

  # Add N2O flux related to carbon destocking in soils and litter
  soil_flows[, flow_N2O := ifelse(flow < 0.0,
                                  flow *(1/15*0.01*44/25 + 1/15*0.3*0.0075*44/28)*298,
                                  0.0)]

  soil_flows <- unique(soil_flows)
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
  dt_dupl <- as.data.table(dt_dupl)
  dt_dupl <- dt_dupl[, flow := ifelse(artif_rate < 0, min_flow, max_flow*artif_rate + min_flow* (1-artif_rate)) ]
  dt_dupl <- dt_dupl[, flow := ifelse(artif_rate == 0, 0, flow) ]


  dt_dupl <- dt_dupl[, flow_N2O := ifelse(artif_rate < 0, min_flow_N2O, max_flow_N2O*artif_rate + min_flow_N2O* (1-artif_rate)) ]
  dt_dupl <- dt_dupl[, flow_N2O := ifelse(artif_rate == 0, 0, flow) ]


  dt_dupl <- dt_dupl[, -c("artif_rate", "max_flow", "min_flow", "max_flow_N2O", "min_flow_N2O")]
  dt <- rbind(dt_unique, dt_dupl)

  dt <- setnames(dt, c("flow"), c("flow_soil"))
  dt <- dt %>% select(-c(from_id, to_id))
  dt$unit <- "tC/ha"


  dt$soil_category_initial[!is.na(dt$from_id)] <- dt$from_id[!is.na(dt$from_id)]
  dt$soil_category_final[!is.na(dt$to_id)] <- dt$to_id[!is.na(dt$to_id)]


  # retrieve carbon flows in biomass (with forest) --------------------------

  forest_flows <- invisible(aldo_Ref_Biom_foret_flows)
  forest_flows <- forest_flows[EPCI_Siren %in% epcis,]
  forest_flows[, flow := ifelse(is.na(flow), 0, flow)]
  forest_flows <- forest_flows[, list(EPCI_Siren= as.character(EPCI_Siren), flow, clc_category= as.character(clc_category))]

  # Convert to CO2eq
  forest_flows[, flow := flow*44/12]

  dt <- merge(dt,
              forest_flows,
              by.x = c("EPCI_Siren", "code_final"),
              by.y = c("EPCI_Siren", "clc_category"),
              all.x = T, allow.cartesian = T)
  dt <- setnames(dt, c("flow"), c("flow_forest"))
  dt <- dt[, flow_forest := ifelse(is.na(flow_forest), 0, flow_forest)]
  dt[, flow_biomass:= ifelse(code_initial == code_final, 0, flow_biomass)]
  dt[, flow_soil:= ifelse(code_initial == code_final, 0, flow_soil)]
  dt[, flow_N2O:= ifelse(code_initial == code_final, 0, flow_N2O)]


  # Convert to tCO2/ha.year
  dt <- dt[, list(ID_unique, EPCI_Siren, code_initial, code_initial_first, code_final, code_final_first,
            soil_category_initial, soil_category_final, biomass_category_initial, biomass_category_final,
            X_mean_initial, X_mean_final, area, unit,
            flow_biomass= -flow_biomass/delta_years,
            flow_soil= -flow_soil/delta_years,
            flow_N2O= -flow_N2O/delta_years,
            flow_forest= - flow_forest)]

  dt <- dt[, flow_forest:= ifelse(code_initial != dt$code_final, flow_forest/delta_years, flow_forest)]


  dt <- dt[, total_flows := round(flow_soil + flow_biomass + flow_forest + flow_N2O, digits =1)]

  dt[is.na(dt)] <- 0
  dt_geom <- dt_geom %>% select(ID_unique)
  dt <- merge(dt_geom, dt, by= "ID_unique")

  dt <- st_as_sf(dt)

  return(dt)

}

#' Retrieve the carbon flows in the forest,
#' using the BD foret 2
#' @return forest flows in tC.an-1
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom here here
#' @importFrom sf st_read st_transform st_intersection st_area st_drop_geometry
#' @importFrom happign  get_wfs get_layers_metadata
#' @importFrom dplyr mutate
get_forest_flows <- function(){

  forest_geom <- st_read(here(data_path, "bd_foret", "zone_bd_foret.gpkg"))
  forest_geom <- tibble::rowid_to_column(forest_geom, "ID_unique")
  forest_geom <- st_transform(forest_geom, 3035)
  forest <- st_drop_geometry(forest_geom)
  forest <- as.data.table(forest)

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(forest_geom))
  epcis <- unique(shape$SIREN_EPCI)

  forest[, essence := ifelse(like(tfv_g11, "conifères"), "conifere", NA)]
  forest[, essence := ifelse(like(tfv_g11, "feuillus"), "feuillu", essence)]
  forest[, essence := ifelse(like(tfv_g11, "mixte"), "mixte", essence)]
  forest[, essence := ifelse(like(tfv_g11, "Peupleraie"), "peupleraies", essence)]

  dt <- invisible(aldo_Ref_Biom_foret_flows)
  dt <- dt[EPCI_Siren %in% epcis, list(EPCI_Siren= as.character(EPCI_Siren), composition, flow)]

  dt <- dt[, list(flow= mean(flow)), by= c("EPCI_Siren", "composition")]
  forest_flows <- merge(forest, dt, by.x = c("SIREN_EPCI", "essence"), by.y = c("EPCI_Siren", "composition"),
                        all.x= T, allow.cartesian = T)

  forest_flows[, total_flows := ifelse(is.na(essence), 0, -flow*44/12)]
  forest_flows[, total_flows:= ifelse(total_flows > 0, -total_flows, total_flows)]
  forest_flows[, total_flows := total_flows*area]

  sum_forest_flows <- sum(forest_flows$total_flows, na.rm= T)

  return(sum_forest_flows)
}


retrieve_flows_from_harvested_wood <- function(){

  shape <- st_read( here(data_path, "territory", "territory.gpkg") )
  epcis <- unique(shape$SIREN_EPCI)

  dt_harvested_wood <- fread(here("data", "aldo", "downloaded_data", "harvested_wood.csv"))
  dt_harvested_wood <- dt_harvested_wood[EPCI_Siren %in% c(epcis, 0), ]

  dt_harvested_wood <- melt(
    dt_harvested_wood,
    id.vars = c("EPCI_Siren", "wood_composition"),
    measure.vars = c("BO_harvest", "BI_harvest", "BE_harvest"),
    variable.name = "wood_use",
    value.name = "wood_harvested"
  )

  # Set France Epci code for France to 0
  dt_harvested_wood$EPCI_Siren[is.na(dt_harvested_wood$EPCI_Siren)] <- 0
  france_harvested_wood <-  dt_harvested_wood[EPCI_Siren == 0, list(
                                              wood_harvested_france= sum(wood_harvested)), by = "wood_use"]

  wood_use <- c("BO_harvest", "BI_harvest")
  france_wood_flows <- c(812000, 751000)
  dt_france_wood_stocks <- data.table(wood_use, france_wood_flows)

  harvested_wood_flows <- merge(france_harvested_wood, dt_france_wood_stocks, by = "wood_use")
  harvested_wood_flows[, ratio_stocks_harvest := france_wood_flows /wood_harvested_france]

  harvested_wood_flows <- harvested_wood_flows[, list(wood_use, ratio_stocks_harvest)]

  harvested_wood_flows <- merge(dt_harvested_wood, harvested_wood_flows,
                                 by = "wood_use")

  harvested_wood_flows[, carbon_flows := round(wood_harvested *ratio_stocks_harvest)]

  harvested_wood_flows <- harvested_wood_flows[EPCI_Siren %in% epcis,
                                               list(wood_carbon_flows = sum(carbon_flows)*1e-3),
                                               by= "EPCI_Siren"]
  return(harvested_wood_flows)

}



#' Compute total carbon flows between two years, of a chosen region.
#' Four flows categories : biomass from land use changes, soil from land
#' use changes, total forest flows (photosynthesis), total area flows
#' @param flows st object returned by 'compute_carbon_flows' funciton
#' @param bd boolean to use bd foret or not
#' @return a data.table with the flows in ktCO2/year
#' @export
#' @importFrom data.table data.table
#' @importFrom sf st_area
get_total_carbon_flows <- function(flows, bd= T) {

  epcis <- unique(flows$EPCI_Siren)

  ### Compute total flows on the territory
  flows$area <- st_area(flows)

  total_biomass_flows <- round(sum(flows$area * 1e-04 * 1e-03 * flows$flow_biomass, na.rm=T),digits = 2)
  total_biomass_flows <- -as.numeric(total_biomass_flows)

  total_soil_flows <- round(sum(flows$area * 1e-04 *1e-03 *  (flows$flow_soil - flows$flow_N2O), na.rm= T), digits = 2)
  total_soil_flows <- - as.numeric(total_soil_flows)

  total_harvested_wood_flows <- invisible(aldo_Ref_Prod_Bois_flows)
  total_harvested_wood_flows <- - sum(total_harvested_wood_flows[EPCI_Siren %in% epcis, wood_carbon_flows])

  if (bd == T){
    total_forest_flows <- get_forest_flows()
    total_forest_flows <- round(total_forest_flows*1e-03* 1e-04, digits = 2)
  }
  else {
    total_forest_flows <- round(sum(flows$area * 1e-04 *1e-03 *  flows$flow_forest), digits = 2)
    total_forest_flows <- as.numeric(total_forest_flows)
  }


  total_area_flows <- sum(total_biomass_flows, total_soil_flows, total_forest_flows, total_harvested_wood_flows)
  total_area_flows <- round(as.numeric(total_area_flows), digits = 2)

  flows_category <- c("carbon flows from land use changes (ktCO2e/an)", "carbon flows from forest (ktCO2e/an)",
                      "carbon flows from harvested wood (ktCO2e/an)", "total carbon flows (ktCO2e/an)")

  flows_value <- c(total_biomass_flows + total_soil_flows, total_forest_flows,
                   total_harvested_wood_flows, total_area_flows)

  dt_total_flows <- data.table(flows_category, flows_value)

  return(dt_total_flows)

}

