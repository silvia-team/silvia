#
#
# get_carbon_flows <- function(dt) {
#
#   epci <- silvia:::epci
#
#   #biomass
#
#   biomass_flows_wo_forests <- as.data.table(read.csv(here("data", "biomass_flows_wo_forests.csv")))
#   biomass_flows_wo_forests <- biomass_flows_wo_forests[biomass_flows_wo_forests$EPCI_Siren == epci]
#   biomass_flows_wo_forests[, flow := ifelse(is.na(flow), 0, flow)]
#   biomass_flows_wo_forests[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
#   biomass_flows_wo_forests[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]
#   biomass_flows_wo_forests <- biomass_flows_wo_forests[, list(from_id,to_id, flow)]
#   biomass_flows_wo_forests <- unique(biomass_flows_wo_forests)
#
#   dt <- merge(dt,
#               biomass_flows_wo_forests,
#               by.x = c("biomass_category_initial", "biomass_category_final"),
#               by.y = c("from_id", "to_id"),
#               all.x = T)
#   dt <- setnames(dt, c("flow"), c("flow_biomass"))
#   dt$flow_biomass[is.na(dt$flow_biomass)] <- 0
#
#   #soils
#
#   soil_flows <- as.data.table(read.csv(here("data", "soil_flows.csv")))
#   soil_flows <- soil_flows[soil_flows$EPCI_Siren == epci]
#   soil_flows[, flow := ifelse(is.na(flow), 0, flow)]
#   soil_flows[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
#   soil_flows[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]
#
#   # Add litters
#   `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
#   soil_flows$flow[soil_flows$from_clc %in% c(311,312,313,324) & !(soil_flows$to_clc %in% c(311,312,313,324,141)) ] %+=% -9
#   soil_flows$flow[!(soil_flows$from_clc %in% c(311,312,313,324)) & soil_flows$to_clc %in% c(311,312,313,324,141) ] %+=% 9
#
#   soil_flows <- soil_flows[, list(from_id,to_id, flow)]
#   soil_flows <- unique(soil_flows)
#
#   dt <- merge(dt,
#               soil_flows,
#               by.x = c("soil_category_initial", "soil_category_final"),
#               by.y = c("from_id", "to_id"),
#               all.x = T)
#   setnames(dt,  c("flow"), c("flow_soil"))
#   dt$flow_soil[is.na(dt$flow_soil)] <- 0
#
#   # # forests
#   forest_flows <- as.data.table(read.csv(here("data", "forest_flows.csv")))
#   forest_flows <- forest_flows[forest_flows$EPCI_Siren == epci,]
#   forest_flows[, flow := ifelse(is.na(flow), 0, flow)]
#   forest_flows <- forest_flows[, list(flow,clc_category)]
#
#   dt <- merge(dt,
#               forest_flows,
#               by.x = c("code_final"),
#               by.y = c("clc_category"),
#               all.x = T)
#   dt <- setnames(dt, c("flow"), c("flow_forest"))
#   dt$flow_forest[is.na(dt$flow_forest)] <- 0
#
#   return(dt)
#
# }
#
# year_from = 1990
# year_to = 2018
# delta_years = year_to - year_from
#
# dt <- get_land_use_changes(year_from, year_to)
# flows <- get_carbon_flows(dt)
#
#
# library(sf)
# library(dplyr)
# library(ggplot2)
# library(classInt)
#
# dt_to <- get_carbon_storage(2018)
#
# name_of_the_territory <- "test"
#
# flows$flow_biomass <- flows$flow_biomass/delta_years
# flows$flow_soil <- flows$flow_soil/delta_years
#
#
# # ###  Get total flows on the territory
# flows$total_flows <- - round(flows$flow_soil + flows$flow_biomass + flows$flow_forest, digits =1)
#
# total_biomass_flows <- round(sum(st_area(flows$geometry) * 1e-04 * 1e-03 * flows$flow_biomass))
# total_biomass_flows <- as.numeric(- total_biomass_flows)
#
# total_soil_flows <- round(sum(st_area(flows$geometry) * 1e-04 *1e-03 *  flows$flow_soil))
# total_soil_flows <- as.numeric(- total_soil_flows)
#
# total_forest_flows <- round(sum(st_area(flows$geometry) * 1e-04 *1e-03 *  flows$flow_forest))
# total_forest_flows <- as.numeric(- total_forest_flows)
#
# total_area_flows <- sum(st_area(flows) * 1e-04 *1e-03 *  flows$total_flows)
# total_area_flows <- round(as.numeric(total_area_flows))
#
#
#
# flows[is.na(flows)] <- 0
# breaks_qt <- classIntervals(flows$total_flows, n = 7, style = "quantile")
# flows <- mutate(flows, total_flows_intervals = cut(total_flows, unique(breaks_qt$brks)))
#
#
#
# # #############################
#
# p3 <- ggplot(flows)
# p3 <- p3 + geom_sf(aes(fill = total_flows_intervals, geometry = geometry), color = NA)
# # p3 <- p3 + facet_wrap(~year)
# p3 <- p3 + labs(
#   title = paste("Flux de carbone entre", year_from, "et", year_to,  " - ", name_of_the_territory, "\n"),
#   subtitle = bquote(atop('Flux total de C : ' ~ .(f1(total_area_flows)) ~ ktC/an ~ ' = '
#                          ~  .(f1(round(total_area_flows*44/12))) ~ ktCO['2, eq']/an ~ '\n',
#                          atop("Flux de C (biomasse + forêts) : "~ .(f1(total_biomass_flows + total_forest_flows)) ~ ktC/an ~ ' = '
#                               ~  .(f1(round((total_biomass_flows + total_forest_flows)*44/12))) ~ ktCO['2']/an ~ '\n' ,
#                               "Flux de C (sol et litière) : "~ .(f1(total_soil_flows)) ~ ktC/an ~ ' = '
#                               ~  .(f1(round(total_soil_flows*44/12))) ~ ktCO['2']/an ~ '\n'
#                          ))),
#
#   caption = "* Une valeur négative correspond à une séquestration, positive à une émission vers l'atmosphère \n
#   Données : Corine Land Cover et ALDO (GIS)"
#   ,
#   fill = "Flux de carbone (tC/ha.an)"
# )
#
# p3 <- p3 + theme_void()
# p3 <- p3 + theme(
#   legend.position = "right",
#   legend.justification = "left",
#   plot.caption = element_text(hjust = 0),
#   panel.border = element_blank(),
#   plot.title = element_text(face = "bold", size = 14),
#   plot.title.position = "plot",
#   plot.subtitle = element_text(face = "italic", size = 12),
#   plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
#   strip.text = element_text(size = 14),
#   legend.margin = margin(0, 0, 0.5, 0, "cm")
# )
#
# p3 <- p3 + scale_fill_brewer(palette =  "Spectral")
# p3 <- p3 + geom_sf(data = st_union(dt_to), fill = NA)
# p3
#
#
#
