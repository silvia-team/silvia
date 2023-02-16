
#' Get forest flows
#'
#' @description
#' Retrieve the carbon flows in the forests,
#' using the BD foret 2
#'
#' @param data_path path to where the data is stored

#'
#' @return forest flows in tC.an-1
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom here here
#' @importFrom sf st_read st_transform st_intersection st_area st_drop_geometry st_as_sf
#' @importFrom happign  get_wfs get_layers_metadata
#' @importFrom dplyr mutate
#'
get_forest_flows <- function(data_path){

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
  forest_flows[, total_flows_area := total_flows*area]
  forest_flows <- forest_flows[, list(ID_unique, total_flows, total_flows_area)]

  forest_flows <- merge(forest_geom, forest_flows, by = "ID_unique")
  forest_flows <- st_as_sf(forest_flows)

  return(forest_flows)
}
