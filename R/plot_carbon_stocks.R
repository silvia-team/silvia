#' Plot carbon stocks
#'
#' @description
#' Plot the carbon stocks on the chosen territory
#'
#' @param stocks sf object returned by `get_carbon_stocks()` function
#' @param data_path path to where the data is stored
#'
#' @return a ggplot with the map
#' @export
#' @importFrom sf st_read st_write st_area st_geometry
#' @importFrom here here
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn scale_color_brewer
#'
#' @export
#'
plot_carbon_stocks <- function(stocks, data_path){

  shape <- st_read(here(data_path, "territory", "territory.gpkg"), quiet = T)
  shape <- st_transform(shape, st_crs(stocks))
  shape <- shape %>% summarise(geom= st_union(geom))
  shape <- nngeo::st_remove_holes(shape)

  p <- ggplot(stocks)
  p <- p + geom_sf(aes(fill = total_carbon_content), color = NA)
  p <- p + labs(
    # caption = "Données Corine Land Cover et ALDO (GIS)\n",
    fill = "Intensité du stockage \nde carbone (tCO2/ha)"
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


