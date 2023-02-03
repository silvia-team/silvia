#' Plot carbon flows
#'
#' @description
#' Plot carbon flows of a chosen region.
#'
#' @param flows sf object returned by `get_carbon_flows()` function
#' @param data_path path to where the data is stored
#'
#' @return map of carbon flows
#'
#' @importFrom data.table data.table
#' @importFrom sf st_union st_crs
#' @importFrom classInt classIntervals
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin scale_fill_gradient2
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
#'
#'
#' @export
#'
plot_carbon_flows <- function(flows, data_path){

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(flows))
  shape <- shape %>% summarise(geom= st_union(geom))
  shape <- nngeo::st_remove_holes(shape)


  p <- ggplot(flows)
  p <- p + geom_sf(aes(fill = total_flows), colour = NA)

  p <- p + labs(
    caption = "Une valeur négative correspond à une séquestration, positive à une émission vers l'atmosphère \n",
    # Données : Corine Land Cover et ALDO (GIS-Sol)"
    fill = "Flux de carbone (tCO2eq/ha.an)"
  )

  p <- p + theme_void()
  p <- p + theme(
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
  p <- p +scale_fill_gradient2(midpoint = 0, mid="#FFFFFF", high="#bd0502", low="#016623")

  p <- p + geom_sf(data = st_union(shape), fill = NA)


  return(p)

}
