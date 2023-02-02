
#' Plot forest flows
#'
#' @description
#' Plot carbon flows in forest of the chosen territory.
#'
#'
#' @param data_path path to where the data is stored
#'
#' @return map of carbon flows by the forest
#'
#' @importFrom data.table as.data.table like
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_union st_crs
#' @importFrom here here
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin scale_fill_gradient2
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
#'
#' @export
#'
plot_forest_flows <- function(data_path){

  # retrieve the forest geopackage previously downloaded
  forest_geom <- st_read(here(data_path, "bd_foret", "zone_bd_foret.gpkg"))

  # remove the geometry to speed up the function
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

  # Retrieve forest flows
  dt <- invisible(aldo_Ref_Biom_foret_flows)
  dt <- dt[EPCI_Siren %in% epcis, list(EPCI_Siren= as.character(EPCI_Siren), composition, flow)]

  dt <- dt[, list(flow= mean(flow)), by= c("EPCI_Siren", "composition")]
  forest_flows <- merge(forest, dt, by.x = c("SIREN_EPCI", "essence"), by.y = c("EPCI_Siren", "composition"),
                        all.x= T, allow.cartesian = T)
  forest_flows[, total_flows := ifelse(is.na(essence), 0, -flow*44/12)]
  forest_flows[, total_flows:= ifelse(total_flows > 0, -total_flows, total_flows)]

  forest_geom <- forest_geom %>% select(ID_unique)
  forest_flows <- merge(forest_flows, forest_geom, by = "ID_unique")


  # plot
  p <- ggplot(forest_flows)
  p <- p + geom_sf(aes(fill = total_flows, geometry = geom), color = NA)

  p <- p + labs(
    caption = "Une valeur négative correspond à une séquestration, positive à une émission vers l'atmosphère \n",
    # Données : Corine Land Cover et BD Forêt V2",
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
  p <- p + scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(9, "YlGn")))
  p <- p + geom_sf(data = st_union(shape), fill = NA)

  return(p)
}


