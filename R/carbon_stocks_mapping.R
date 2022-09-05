
# plot_carbon_stocks <- function(dt){
#
#   total_soils_area_stocks <- round(sum(st_area(dt) * 1e-04* 1e-03 * dt$soil_carbon_content))
#   total_soils_area_stocks <- as.numeric(total_soils_area_stocks)
#
#   total_biomass_area_stocks <- round(sum(st_area(dt$geometry) * 1e-04 * 1e-03 * dt$biomass_carbon_content))
#   total_biomass_area_stocks <- as.numeric(total_biomass_area_stocks)
#
#   total_area_stocks <- sum(st_area(dt) * 1e-04 * 1e-03 * dt$total_carbon_content)
#   total_area_stocks <- as.numeric(total_area_stocks)
#   epci_wood_stocks <- retrieve_harvested_wood(epci)
#
#   total_area_stocks <-  round(total_area_stocks + epci_wood_stocks)
#
#
#   # -------------------------------------------------------------------------------------
#   # -------------------------------------  PLOTS  ---------------------------------------
#   # -------------------------------------------------------------------------------------
#
#   p1 <- ggplot(dt)
#   p1 <- p1 + geom_sf(aes(fill = soil_carbon_content), color = NA)
#   # p1 <- p1 + facet_wrap(~year)
#   p1 <- p1 + labs(
#     title = paste("Stocks de carbone dans les sols et dans les litières - ", epci_name, "\n"),
#     subtitle = bquote('Total des stocks : ' ~ .(f1(total_soils_area_stocks)) ~ ktC ~ ' = '
#                       ~  .(f1(round(total_soils_area_stocks*44/12))) ~ ktCO['2, eq'] ~ '\n'),
#     caption = "Données Corine Land Cover et ALDO (GIS)\n",
#     fill = "Intensité du stockage de\n carbone dans le sol + litière (tC/ha)"
#   )
#   p1 <- p1 + theme_void()
#   p1 <- p1 + theme(
#     legend.position = "right",
#     legend.justification = "left",
#     panel.border = element_blank(),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.title.position = "plot",
#     plot.subtitle = element_text(face = "italic", size = 12),
#     plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
#     strip.text = element_text(size = 14),
#     legend.margin = margin(0, 0, 0.5, 0, "cm")
#   )
#   p1 <- p1 + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges"))
#   # ggsave(plot = p1, width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Carte des stocks de carbone (sols) du Grand Genève - PACA 3 en 2018.svg"))
#   print(p1)
#
#
#   p2 <- ggplot(dt)
#   p2 <- p2 + geom_sf(aes(fill = biomass_carbon_content, geometry = geometry), color = NA)
#   # p2 <- p2 + facet_wrap(~year)
#   p2 <- p2 + labs(
#     title = paste("Stocks de carbone dans la biomasse - ", epci_name, "\n"),
#     subtitle = bquote('Total des stocks : ' ~ .(f1(total_biomass_area_stocks)) ~ ktC ~ ' = '
#                       ~  .(f1(round(total_biomass_area_stocks*44/12))) ~ ktCO['2, eq'] ~ '\n'),
#     caption = "Données Corine Land Cover et ALDO (GIS)\n",
#     fill = "Intensité du stockage de\n carbone dans la biomasse (tC/ha)"
#   )
#   p2 <- p2 + theme_void()
#   p2 <- p2 + theme(
#     legend.position = "right",
#     legend.justification = "left",
#     panel.border = element_blank(),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.title.position = "plot",
#     plot.subtitle = element_text(face = "italic", size = 12),
#     plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
#     strip.text = element_text(size = 14),
#     legend.margin = margin(0, 0, 0.5, 0, "cm")
#   )
#   # p <- p +scale_fill_brewer(palette = "Oranges")
#   p2 <- p2 + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Greens"))
#   # ggsave(plot = p2, width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Carte des stocks de carbone (biomasse) du Grand Genève - PACA 3 en 2018.svg"))
#   print(p2)
#
#
#   p3 <- ggplot(dt)
#   p3 <- p3 + geom_sf(aes(fill = total_carbon_content, geometry = geometry), color = NA)
#   # p3 <- p3 + facet_wrap(~year)
#   p3 <- p3 + labs(
#     title = paste("Stocks de carbone (Sols + Biomasse + Litière) - ", epci_name, '\n'),
#     subtitle = bquote(atop('Total des stocks : ' ~ .(f1(total_area_stocks)) ~ ktC ~ ' = '
#                            ~  .(f1(round(total_area_stocks*44/12))) ~ ktCO['2'] ~ '\n',
#                            "dont produits bois : "~ .(f1(epci_wood_stocks)) ~ ktC ~ ' = '
#                            ~  .(f1(round(epci_wood_stocks*44/12))) ~ ktCO['2'] ~ '\n')),
#
#     caption = "Données Corine Land Cover et ALDO (GIS)\n",
#     fill = "Intensité du stockage\n de carbone (tC/ha)"
#   )
#   p3 <- p3 + theme_void()
#   p3 <- p3 + theme(
#     legend.position = "right",
#     legend.justification = "left",
#     panel.border = element_blank(),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.title.position = "plot",
#     plot.subtitle = element_text(face = "italic", size = 12),
#     plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
#     strip.text = element_text(size = 14),
#     legend.margin = margin(0, 0, 0.5, 0, "cm")
#   )
#   p3 <- p3 + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Greys"))
#   # ggsave(plot = p3, width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Carte des stocks de carbone (total) du Grand Genève - PACA 3 en 2018.svg"))
#   print(p3)
#
# }

#' Map the carbon storage on the chosen region.
#' @param dt
#' @return a ggplot with the map
#' @export
#' @importFrom sf st_read st_write st_area
#' @importFrom here here
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme
map_carbon_storage <- function(dt){

  total_area_stocks <- sum(st_area(dt) * 1e-04 * 1e-03 * dt$total_carbon_content)
  total_area_stocks <- as.numeric(total_area_stocks)
  load(here("data", "epci.rda"))
  epci_wood_stocks <- retrieve_harvested_wood(epci)
  total_area_stocks <-  round(total_area_stocks + epci_wood_stocks)

  p <- ggplot(dt)
  p <- p + geom_sf(aes(fill = total_carbon_content, geometry = geometry), color = NA)
  p <- p + labs(
    caption = "Données Corine Land Cover et ALDO (GIS)\n",
    fill = "Intensité du stockage\n de carbone (tC/ha)"
  )
  p <- p + theme_void()
  p <- p + theme(
    legend.position = "right",
    legend.justification = "left",
    panel.border = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", size = 12),
    strip.text = element_text(size = 14),
    legend.margin = margin(0, 0, 0.5, 0, "cm")
  )
  p <- p + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Greys"))
  # ggsave(plot = p, width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Carte des stocks de carbone (total) du Grand Genève - PACA 3 en 2018.svg"))

  return(p)

}

