#' Select territory
#'
#' Spatial boundaries to be studied.
#' The codes of the different french territories
#' (communes, EPCI, départements, régions) are  referenced here
#' [INSEE's codes](https://www.insee.fr/fr/information/6051727)
#'
#'
#' The function returns the map of the created perimeter
#' and store the gpkg file in 'data->arep' folder as "territory.gpkg".
#'
#' @usage
#' select_territory(
#'     regions_fr = NULL,
#'     departments_fr = NULL,
#'     epcis_fr= NULL,
#'     communes_fr = NULL,
#'     communes_ch= NULL)
#'
#' @param regions_fr A `character` or a list of `character` of french
#' region codes to be included in the perimeter
#' @param departments_fr A `character` or a list of `character` of french
#' departements codes to be included in the perimeter
#' @param epcis_fr A `character` or a list of `character` of french
#' EPCIs codes to be included in the perimeter
#' @param communes_fr A `character` or a list of `character` of french
#' municipalities codes to be included in the perimeter
#'
#' @return Map of the selected territory
#'
#' @examples
#' # Shape of the "Grand Annecy" EPCI
#'
#' select_territory(epcis_fr = "200066793")
#'
#'
#' # Shape of the union of a region (PACA 93), a departement (Occitanie 31),
#' two epcis (in the Rhône department), and one city (Albi 81004)
#'
#' select_territory(regions_fr = "93", departments_fr = "31", epcis_fr = c("246900740", "200046977"), communes_fr = "81004")
#' @export
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write st_transform st_combine
#' @importFrom sf st_geometry st_union st_bbox st_geometry
#' @importFrom nngeo st_remove_holes
select_territory  <- function(
    regions_fr = NULL,
    departments_fr = NULL,
    epcis_fr= NULL,
    communes_fr = NULL) {


  cities_fr_geom <- st_read(here("data", "arep", "cities_fr.gpkg"))
  cities_fr_geom <- tibble::rowid_to_column(cities_fr_geom, "ID_unique")

  cities_fr <- st_drop_geometry(cities_fr_geom)
  cities_fr <- as.data.table(cities_fr)

  zone_fr <- cities_fr[INSEE_COM %in% communes_fr |
                         INSEE_DEP %in% departments_fr |
                         SIREN_EPCI %in% epcis_fr |
                         INSEE_REG %in% regions_fr, ]
  zone_fr <- unique(zone_fr)

  cities_fr_ID <- cities_fr_geom %>% select(ID_unique)

  zone_fr <- merge(cities_fr_ID, zone_fr, by = "ID_unique")
  zone_fr <- st_as_sf(zone_fr)
  sf::st_geometry(zone_fr) <- "geom"

  zone_fr$area <- st_area(zone_fr)
  zone <- zone_fr %>% group_by(SIREN_EPCI) %>%
    summarise(geom= st_union(geom), area= as.numeric(sum(area))) %>%
    filter(area >100)

  zone <- nngeo::st_remove_holes(zone)
  zone <- st_transform(zone, 3035)

  st_write(zone, here("data", "arep", "territory.gpkg"), delete_dsn = TRUE)


  p <- ggplot()
  p <- p + geom_sf(data = zone, fill = NA)
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

  return(p)
}
