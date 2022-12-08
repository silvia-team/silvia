#' Crop the territory perimeter as one multipolygon
#' The function returns the 'box' of the created perimeter
#' and store the gpkg file in 'data->arep' folder
#' @param regions_fr
#' @param departments_fr
#' @param epcis_fr
#' @param communes_ch
#' @param communes_fr
#' @return the coordinates of the box that delineates the territory
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
    communes_ch= NULL,
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

  if (!is.null(communes_ch)){
    cities_ch <- st_read(here("data","arep", "cities_ch.gpkg"))
    zone_ch <- cities_ch[cities_ch$num_OFS %in% communes_ch, ]
    zone_ch <- st_union(zone_ch, by_feature = FALSE, is_coverage = TRUE)
    zone_ch <- st_transform(zone_ch, st_crs(zone_fr))
    zone_ch <- st_as_sf(zone_ch)
    sf::st_geometry(zone_ch) <- "geom"
    zone_ch <- zone_ch %>% summarise(
      geom= st_union(geom), SIREN_EPCI = "200011773", area = as.numeric(st_area(geom)))
    zone <- rbind(zone, zone_ch)
    zone <- zone %>% group_by(SIREN_EPCI) %>%
      summarise(geom= st_union(geom))

  }

  zone <- nngeo::st_remove_holes(zone)

  p <- ggplot()
  p <- p + geom_sf(data = zone, fill = NA)
  p

  zone <- st_transform(zone, 3035)

  st_write(zone, here("data", "arep", "territory.gpkg"), delete_dsn = TRUE)

  return(sf::st_bbox(zone))
}
