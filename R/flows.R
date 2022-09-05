
#' Retrieve the carbon flows between two years, of a chosen region
#' @param year_from
#' @param year_to
#' @return a sf object with the carbon flows of the region
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom here here
get_carbon_flows <- function(year_from, year_to) {

  dt <- silvia::get_land_use_changes(year_from, year_to, remove_unchanged = FALSE)


  forest_codes <- c(311, 312, 313, 324)
  # dt <- dt[dt$code_initial != dt$code_final | dt$code_final %in% forest_codes,]

  delta_years = year_to - year_from
  load(here("data", "epci.rda"))

  #biomass

  biomass_flows_wo_forests <- as.data.table(read.csv(here("data", "aldo","biomass_flows_wo_forests.csv")))
  biomass_flows_wo_forests <- biomass_flows_wo_forests[biomass_flows_wo_forests$EPCI_Siren == epci,]
  biomass_flows_wo_forests[, flow := ifelse(is.na(from_clc), 0, flow)]
  biomass_flows_wo_forests[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  biomass_flows_wo_forests[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]
  biomass_flows_wo_forests <- biomass_flows_wo_forests[, list(from_id,to_id, flow)]
  biomass_flows_wo_forests <- unique(biomass_flows_wo_forests)

  # Convert to CO2eq
  biomass_flows_wo_forests[, flow := flow*44/12]


  dt <- merge(dt,
              biomass_flows_wo_forests,
              by.x = c("biomass_category_initial", "biomass_category_final"),
              by.y = c("from_id", "to_id"),
              all.x = T)
  dt <- setnames(dt, c("flow"), c("flow_biomass"))
  dt$flow_biomass[is.na(dt$flow_biomass)] <- 0

  #soils

  soil_flows <- as.data.table(read.csv(here("data", "aldo", "soil_flows.csv")))
  soil_flows <- soil_flows[soil_flows$EPCI_Siren == epci]
  soil_flows[, flow := ifelse(is.na(flow), 0, flow)]
  soil_flows[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
  soil_flows[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]

  # Add litters
  `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
  soil_flows$flow[soil_flows$from_clc %in% c(311,312,313,324) & !(soil_flows$to_clc %in% c(311,312,313,324,141)) ] %+=% -9
  soil_flows$flow[!(soil_flows$from_clc %in% c(311,312,313,324)) & soil_flows$to_clc %in% c(311,312,313,324,141) ] %+=% 9

  soil_flows <- soil_flows[, list(from_clc, to_clc, flow)]
  soil_flows <- unique(soil_flows)

  # Convert to CO2eq
  soil_flows[, flow := flow*44/12]


  ### Add N2O flux related to carbon destocking in soils and litter
  soil_flows[, flow_N2O := ifelse(flow < 0.0,
                                  flow *(1/15*0.01*44/25 + 1/15*0.3*0.0075*44/28)*298
                                  ,0.0)]


  dt <- merge(dt,
              soil_flows,
              by.x = c("code_initial", "code_final"),
              by.y = c("from_clc", "to_clc"),
              all.x = T)
  setnames(dt,  c("flow"), c("flow_soil"))
  dt$flow_soil[is.na(dt$flow_soil)] <- 0
  dt$flow_N2O[is.na(dt$flow_N2O)] <- 0


  # # forests
  forest_flows <- as.data.table(read.csv(here("data", "aldo","forest_flows.csv")))
  forest_flows <- forest_flows[forest_flows$EPCI_Siren == epci,]
  forest_flows[, flow := ifelse(is.na(flow), 0, flow)]
  forest_flows <- forest_flows[, list(flow,clc_category)]

  # Convert to CO2eq
  forest_flows[, flow := flow*44/12]

  dt <- merge(dt,
              forest_flows,
              by.x = c("code_final"),
              by.y = c("clc_category"),
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

  dt$total_flows <- round(dt$flow_soil + dt$flow_biomass + dt$flow_forest + dt$flow_N2O, digits =1)

  dt[is.na(dt)] <- 0
  # dt <- dt[dt$total_flows != 0, ]

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

  total_biomass_flows <- round(sum(st_area(flows$geometry) * 1e-04 * 1e-03 * flows$flow_biomass))
  total_biomass_flows <- as.numeric(total_biomass_flows)

  total_soil_flows <- round(sum(st_area(flows$geometry) * 1e-04 *1e-03 *  (flows$flow_soil + flows$flow_N2O)))
  total_soil_flows <- as.numeric(total_soil_flows)

  total_forest_flows <- round(sum(st_area(flows$geometry) * 1e-04 *1e-03 *  flows$flow_forest))
  total_forest_flows <- as.numeric(total_forest_flows)

  total_area_flows <- sum(st_area(flows) * 1e-04 *1e-03 *  flows$total_flows)
  total_area_flows <- round(as.numeric(total_area_flows))

  flows_category <- c("biomass_flows_from_land_use_changes", "soil_flows_from_land_use_changes",
                      "total_forest_flows", "total_area_flows")

  flows_value <- c(total_biomass_flows, total_soil_flows, total_forest_flows, total_area_flows)

  dt_total_flows <- data.table(flows_category, flows_value)


}


library(sf)
library(dplyr)
library(ggplot2)
library(classInt)


#' Plot carbon flows of a chosen region.
#' @param flows st object returned by 'get_carbon_flows' funciton
#' @return a data.table with the flows in ktCO2/year
#' @export
#' @importFrom data.table data.table
#' @importFrom sf st_union
#' @importFrom classInt classIntervals
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_brewer theme
map_carbon_flows <- function(flows){

  breaks_qt_pos <- classIntervals(flows$total_flows[flows$total_flows>0], n = 5, style = "quantile")
  breaks_qt_neg <- classIntervals(flows$total_flows[flows$total_flows<0], n = 6, style = "quantile")
  breaks_qt <- unique(c(breaks_qt_pos$brks, breaks_qt_neg$brks, 0))
  flows <- mutate(flows, total_flows_intervals = cut(trunc(total_flows), breaks_qt, right= F))


  p3 <- ggplot(flows)
  p3 <- p3 + geom_sf(aes(fill = total_flows_intervals, geometry = geometry), color = NA)
  p3 <- p3 + labs(
    # title = paste("Flux de carbone entre", year_from, "et", year_to,  " - ", name_of_the_territory, "\n"),
    # subtitle = bquote(atop('Flux total : '~  .(f1(round(total_area_flows))) ~ ktCO['2, eq']/an ~ '\n',
    #                        atop("Flux biomasse + forêts : "~  .(f1(round((total_biomass_flows + total_forest_flows)))) ~ ktCO['2']/an ~ '\n' ,
    #                             "Flux sol et litière : "~  .(f1(round(total_soil_flows))) ~ ktCO['2']/an ~ '\n'
    #                        ))),
    #
    caption = "Une valeur négative correspond à une séquestration, positive à une émission vers l'atmosphère \n
    Données : Corine Land Cover et ALDO (GIS)"
     ,
    fill = "Flux de carbone (tCO2eq/ha.an)"
  )

  p3 <- p3 + theme_void()
  p3 <- p3 + theme(
    legend.position = "right",
    legend.justification = "left",
    plot.caption = element_text(hjust = 0),
    panel.border = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", size = 12),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
    strip.text = element_text(size = 14),
    legend.margin = margin(0, 0, 0.5, 0, "cm"),
    legend.spacing.y = unit(0.5, 'cm')
  )
  p3 <- p3 + scale_fill_brewer(palette =  "RdYlGn", direction = -1)
  # p3 <- p3 + geom_sf(data = st_union(flows), fill = NA)

  return(p3)

}

