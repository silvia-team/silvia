#' Plot carbon storage
#'
#' @description
#' Plot the carbon storage on the chosen territory
#'
#' @param year the reference year to be studied (1990, 2000, 2006, 2012, 2018)
#' @param data_path path to where the data is stored
#'
#' @return a ggplot with the map
#' @export
#' @importFrom sf st_read st_write st_area st_geometry
#' @importFrom here here
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn scale_color_brewer
#'
#' @export
#'
plot_carbon_storage <- function(year, data_path){

  dt <- get_carbon_stocks(year, data_path)

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(dt))
  shape <- shape %>% summarise(geom= st_union(geom))
  shape <- nngeo::st_remove_holes(shape)

  p <- ggplot(dt)
  p <- p + geom_sf(aes(fill = total_carbon_content), color = NA)
  p <- p + labs(
    # caption = "Données Corine Land Cover et ALDO (GIS)\n",
    fill = "Intensité du stockage\n de carbone (tCO2/ha)"
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
  p <- p + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlOrRd"))
  p <- p + geom_sf(data = st_union(shape), fill = NA)

  return(p)

}


