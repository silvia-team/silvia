# ##############################
# # Load libraries
#
# library(ggplot2)
# library(here)
# library(networkD3)
# source(here("R", "stocks.R"))
# # source(here("R", "land_use_mapping.R"))
#
# ##############################
#
#
# land_use_changes <- function(dt_from, dt_to, year_from, year_to, epci) {
#
#   dt_land_use_changes <-  st_intersection(dt_from, dt_to)
#   # dt_land_use_changes <- dt_land_use_changes[dt_land_use_changes$code != dt_land_use_changes$code.1 ,]
#   dt_land_use_changes <- dt_land_use_changes %>%
#     select(code_initial = code,
#            soil_category_initial = aldo_soil_category,
#            biomass_category_initial = aldo_biomass_category,
#            code_final = code.1,
#            soil_category_final = aldo_soil_category.1,
#            biomass_category_final = aldo_biomass_category.1,
#            total_carbon_content_initial = total_carbon_content,
#            total_carbon_content_final = total_carbon_content.1
#            )
#   dt_land_use_changes <-  na.omit(dt_land_use_changes)
#
#   return(dt_land_use_changes)
# }
#
#
# mapping_land_use_changes <- function(land_use_changes, epci_name, nomenclature_level,year_from, year_to, dt_to) {
#
#   land_use_changes <- land_use_changes[dt_land_use_changes$code_initial != dt_land_use_changes$code_final ,]
#
#   path_to_nomenclature <-  here("data", "clc-nomenclature-c_1.xls")
#   setnames(land_use_changes, "code_final", "code")
#   land_use <- nomenclature_level(land_use_changes, path_to_nomenclature, nomenclature_level, "code")
#
#
#   p <- ggplot(land_use)
#   p <- p + geom_sf(aes(fill = libelle_fr), color = NA)
#   p <- p + scale_fill_manual(values = land_use$color,
#                              breaks = land_use$libelle_fr)
#   p <- p + labs(
#     title = paste("Changement d'occupation des sols entre", year_from, " et ", year_to,  " - ",epci_name, "\n"),
#     caption = "Données Corine Land Cover et ALDO (GIS)\n"
#   )
#   p <- p + theme_void()
#   p <- p + theme(
#     legend.position = "right",
#     legend.justification = "left",
#     panel.border = element_blank(),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.title.position = "plot",
#     plot.subtitle = element_text(face = "italic", size = 12),
#     legend.title = element_blank(),
#     plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
#     strip.text = element_text(size = 14),
#     legend.margin = margin(0, 0, 0.5, 0, "cm")
#   )
#   p <- p + geom_sf(data = st_union(dt_to), fill = NA)
#
#
#   return(p)
#
# }
#
# mapping_artif <- function(dt_land_use_changes, epci_name, year_from, year_to, dt_to) {
#
#   artif_code <- c(111, 112, 121, 122, 123, 124, 131, 132, 133, 142, 141)
#
#
#   land_use_changes <- dt_land_use_changes %>%
#     filter(code_final %in%  artif_code |
#              code_initial  %in%  artif_code)
#
#   land_use_changes$changes <- "Surfaces artificialisées avant 1990"
#   land_use_changes$changes[!(land_use_changes$code_final %in%  artif_code) &
#                              land_use_changes$code_initial  %in%  artif_code] <- "Surfaces dé-artificialisées après 1990"
#   land_use_changes$changes[land_use_changes$code_final %in%  artif_code &
#                              !(land_use_changes$code_initial  %in%  artif_code)] <- "Surfaces artificialisées depuis 1990"
#
#   artif_new <- sum(st_area(land_use_changes[land_use_changes$changes =="Surfaces artificialisées depuis 1990",]))*1e-6
#   total_area <- sum(st_area(dt_to))*1e-6
#
#   rate <-  (artif_new/total_area)*100
#
#   speed_artif <- artif_new*1e-04/(year_to-year_from)
#
#
#
#   p <- ggplot(land_use_changes)
#   p <- p + geom_sf(aes(fill = changes), color = NA)
#   # p <- p + geom_sf(fill = NA, color = "gray50", size = 0.5,
#   #                  data = . %>% group_by(changes) %>% summarise())
#   # p <- p + scale_fill_manual(values = land_use$color,
#   #                            breaks = land_use$libelle_fr)
#   p <- p + labs(
#     # title = paste("Artificialisation des sols entre", year_from, "et", year_to,  " - ",epci_name, "\n"),
#     # caption = "Données Corine Land Cover\n"
#   )
#   p <- p + theme_void()
#   p <- p + theme(
#     legend.position = "right",
#     legend.justification = "left",
#     panel.border = element_blank(),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.title.position = "plot",
#     plot.subtitle = element_text(face = "italic", size = 12),
#     legend.title = element_blank(),
#     plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
#     strip.text = element_text(size = 14),
#     legend.margin = margin(0, 0, 0.5, 0, "cm")
#   )
#   p <- p + scale_fill_manual(values = c("#ebb0b0", "#f06060","#79d18b"), name= "Cluster Group")
#   p <- p + geom_sf(data = st_union(dt_to), fill = NA)
#
#   return(p)
#
# }
#
retrieve_flows <- function(dt_land_use_changes, epci) {


  #biomass

  biomass_flows_wo_forests <- as.data.table(read.csv(here("data", "biomass_flows_wo_forests.csv")))
  biomass_flows_wo_forests <- biomass_flows_wo_forests[biomass_flows_wo_forests$EPCI_Siren == epci]
  biomass_flows_wo_forests[, flow := ifelse(is.na(flow), 0, flow)]
  biomass_flows_wo_forests[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  biomass_flows_wo_forests[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]
  biomass_flows_wo_forests <- biomass_flows_wo_forests[, list(from_id,to_id, flow)]
  biomass_flows_wo_forests <- unique(biomass_flows_wo_forests)

  dt <- merge(dt_land_use_changes,
                   biomass_flows_wo_forests,
                   by.x = c("biomass_category_initial", "biomass_category_final"),
                   by.y = c("from_id", "to_id"),
                   all.x = T)
  dt <- setnames(dt, c("flow"), c("flow_biomass"))
  dt$flow_biomass[is.na(dt$flow_biomass)] <- 0

  #soils

  soil_flows <- as.data.table(read.csv(here("data", "soil_flows.csv")))
  soil_flows <- soil_flows[soil_flows$EPCI_Siren == epci]
  soil_flows[, flow := ifelse(is.na(flow), 0, flow)]
  soil_flows[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  soil_flows[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]

  # Add litters
  `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
  soil_flows$flow[soil_flows$from_clc %in% c(311,312,313,324) & !(soil_flows$to_clc %in% c(311,312,313,324,141)) ] %+=% -9
  soil_flows$flow[!(soil_flows$from_clc %in% c(311,312,313,324)) & soil_flows$to_clc %in% c(311,312,313,324,141) ] %+=% 9

  soil_flows <- soil_flows[, list(from_id,to_id, flow)]
  soil_flows <- unique(soil_flows)

  dt <- merge(dt,
                   soil_flows,
                   by.x = c("soil_category_initial", "soil_category_final"),
                   by.y = c("from_id", "to_id"),
                   all.x = T)
  setnames(dt,  c("flow"), c("flow_soil"))
  dt$flow_soil[is.na(dt$flow_soil)] <- 0

  # # forests
  forest_flows <- as.data.table(read.csv(here("data", "forest_flows.csv")))
  forest_flows <- forest_flows[forest_flows$EPCI_Siren == epci,]
  forest_flows[, flow := ifelse(is.na(flow), 0, flow)]
  forest_flows <- forest_flows[, list(flow,clc_category)]

  dt <- merge(dt,
              forest_flows,
              by.x = c("code"),
              by.y = c("clc_category"),
              all.x = T)
  dt <- setnames(dt, c("flow"), c("flow_forest"))
  dt$flow_forest[is.na(dt$flow_forest)] <- 0

  return(dt)

}

retrieve_forest_flows <- function(dt_to, year_to, epci) {
  forest_flows <- as.data.table(read.csv(here("data", "forest_flows.csv")))
  forest_flows <- forest_flows[forest_flows$EPCI_Siren == epci,]
  forest_flows[, flow := ifelse(is.na(flow), 0, flow)]
  forest_flows <- forest_flows[, list(flow,clc_category)]

  dt <- merge(dt_to,
              forest_flows,
              by.x = c("code"),
              by.y = c("clc_category"))

  dt <- setnames(dt, c("flow"), c("flow_forest"))
  dt <- dt %>% select(code, flow_forest)


  return(dt)

}
#
# retrieve_total_flows<- function(land_use_changes, dt_to, year_to, epci) {
#
#   soil_biomass_flows  <- retrieve_biomass_soil_flows(dt_land_use_changes, epci)
#   forest_flows <- retrieve_forest_flows(dt_to, year_to, epci)
#
#   total_flows <- merge(soil_biomass_flows,
#                        forest_flows,
#                        by = "code")
# }


#' Collect land use changes between two years
#' @param year_from
#' @param year_to
#' @param remove_unchanged
#' @return a sf object with the land use changes between year_to and year_from
#' @export
#' @importFrom sf st_intersection st_area
get_land_use_changes <- function(year_from, year_to, remove_unchanged = TRUE) {

  delta_years = year_to - year_from
  epci <- silvia:::epci

  ### Retrieve carbon stocks from the chosen years
  dt_from <- silvia::get_carbon_storage(year_from)
  dt_to <- silvia::get_carbon_storage(year_to)

  dt_land_use_changes <-  st_intersection(dt_from, dt_to)
  dt_land_use_changes <- dt_land_use_changes %>%
    select(code_initial = code,
           soil_category_initial = aldo_soil_category,
           biomass_category_initial = aldo_biomass_category,
           code_final = code.1,
           soil_category_final = aldo_soil_category.1,
           biomass_category_final = aldo_biomass_category.1,
    )
  dt_land_use_changes <-  na.omit(dt_land_use_changes)
  dt_land_use_changes$area <- as.numeric(st_area(dt_land_use_changes))*1e-4

  dt_land_use_changes$code_initial_first <- as.numeric(substr(dt_land_use_changes$code_initial, 1, 1))
  dt_land_use_changes$code_final_first <- as.numeric(substr(dt_land_use_changes$code_final, 1, 1))

  if (remove_unchanged == TRUE){
    dt_land_use_changes <- dt_land_use_changes %>% filter(code_initial != code_final)
  }

  return(dt_land_use_changes)
}

