# library(here)
# source(here("R", "land_use_changes.R"))
# library(stringr)
# library(classInt)
#
# nomenclature_level <- function(clc, path_to_nomenclature, level, code_name) {
#
#   setnames(clc, code_name, "code")
#
#   if (level == 1){
#     clc_nomenclature <- as.data.table(read_excel(path_to_nomenclature, sheet = "nomenclature_clc_niveau_1"))
#     clc_nomenclature <- rename(clc_nomenclature, code = code_clc_niveau_1)
#     clc$code <- substr(as.character(clc$code), 1, 1)
#   }
#   else if (level == 2) {
#     clc_nomenclature <- as.data.table(read_excel(path_to_nomenclature, sheet = "nomenclature_clc_niveau_2"))
#     clc_nomenclature <- rename(clc_nomenclature, code = code_clc_niveau_2)
#     clc$code <- substr(as.character(clc$code), 1, 2)
#   }
#   else if (level == 3) {
#     clc_nomenclature <- as.data.table(read_excel(path_to_nomenclature, sheet = "nomenclature_clc_niveau_3"))
#     clc_nomenclature <- rename(clc_nomenclature, code = code_clc_niveau_3)
#   }
#   colors_clc <-  paste(clc_nomenclature$rouge, clc_nomenclature$vert, clc_nomenclature$bleu)
#   color_clc_rgb <-sapply(strsplit(as.character(colors_clc), " "), function(x) {
#     rgb(x[1], x[2], x[3], m=255)
#   })
#   color_clc_rgb <-  as.list(strsplit(color_clc_rgb, " ")[])
#   clc_nomenclature$color <- color_clc_rgb
#
#
#   clc_1 <- merge(clc, clc_nomenclature, by= "code")
#   clc_1 <- clc_1 %>%
#     group_by(code, libelle_fr, color) %>%
#     summarize(geometry = st_union(geometry))
#
#   setnames(clc, "code", code_name)
#
#   return(clc_1)
# }
#
#
# ### Choose the reference years among the following ones : 1990, 2000, 2006, 2012, 2018
# year_from = 1990
# year_to = 2018
# delta_years = year_to - year_from
#
#
# ### Retrieve carbon stocks from the chosen years
# dt_from <- get_carbon_storage(year_from)
# dt_to <- retrieve_stocks(year_to, epci)
#
#
# ### Identify the land use changes between the two reference years
# dt_land_use_changes  <- land_use_changes(dt_from, dt_to, year_from, year_to, epci)
#
#
#
# ### Mapping land use changes
# name_of_the_territory = "Grand Genève - PACA 3"
# # p <- mapping_land_use_changes(dt_land_use_changes, name_of_the_territory, 2, year_from, year_to, dt_to)
# # p
#
# p <- mapping_artif(dt_land_use_changes, name_of_the_territory , year_from, year_to, dt_to)
# p
#
# #
# # ggsave(plot = p,width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Artifisialisation des sols du Grand Genève - PACA 3 entre 1990 et 2018.svg"))
#
#
#
#
# ### Retrieve carbon flows between the chosen reference years
# flows  <- retrieve_flows(dt_land_use_changes, epci)
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
#                     ~  .(f1(round(total_area_flows*44/12))) ~ ktCO['2, eq']/an ~ '\n',
#                     atop("Flux de C (biomasse + forêts) : "~ .(f1(total_biomass_flows + total_forest_flows)) ~ ktC/an ~ ' = '
#                     ~  .(f1(round((total_biomass_flows + total_forest_flows)*44/12))) ~ ktCO['2']/an ~ '\n' ,
#                     "Flux de C (sol et litière) : "~ .(f1(total_soil_flows)) ~ ktC/an ~ ' = '
#                     ~  .(f1(round(total_soil_flows*44/12))) ~ ktCO['2']/an ~ '\n'
#                     ))),
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
# p3
#
#
