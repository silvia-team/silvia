#' Select territory
#'
#' Crop spatial boundaries to be studied.
#' Codes of the different french territories
#' (communes, EPCI, départements, régions) are  referenced here
#' [INSEE's codes](https://www.insee.fr/fr/information/6051727)
#'
#'
#' The function returns the map of the created perimeter
#' and store the gpkg file in 'data->arep' folder as "territory.gpkg".
#'
#' @usage
#' select_territory(
#'     regions_fr = list(),
#'     departments_fr = list(),
#'     epcis_fr= list(),
#'     communes_fr = list(),
#'     communes_ch= list(),
#'     local = F)
#'
#'
#'
#' @param regions_fr A `character` or a list of `character` of french
#' region codes to be included in the perimeter
#' @param departments_fr A `character` or a list of `character` of french
#' departements codes to be included in the perimeter
#' @param epcis_fr A `character` or a list of `character` of french
#' EPCIs codes to be included in the perimeter
#' @param communes_fr A `character` or a list of `character` of french
#' municipalities codes to be included in the perimeter
#' @param local If TRUE, the data is loaded from the "arep" folder. If false, a request is done on a api.
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
#'
#'
#' @export
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write st_transform st_combine
#' @importFrom sf st_geometry st_union st_bbox st_geometry
#' @importFrom nngeo st_remove_holes
#' @importFrom httr2 request req_url_path resp_body_string req_url_query
#' @importFrom jsonlite fromJSON
#' @importFrom happign get_apicarto_commune
#' @importFrom ggplot2 ggplot geom_sf
#'
#'
select_territory  <- function(
    regions_fr = list(),
    departments_fr = list(),
    epcis_fr= list(),
    communes_fr = list(),
    local = F) {


  if (local == T){
    # load the french administrative division file
    cities_fr_geom <- st_read(here("data", "arep", "cities_fr.gpkg"))

    # create a unique ID for each zone and transform the sf table to a data table to speed up the sorting
    cities_fr_geom <- tibble::rowid_to_column(cities_fr_geom, "ID_unique")
    cities_fr <- st_drop_geometry(cities_fr_geom)
    cities_fr <- as.data.table(cities_fr)

    # select the chosen territories
    borders <- cities_fr[INSEE_COM %in% communes_fr |
                           INSEE_DEP %in% departments_fr |
                           SIREN_EPCI %in% epcis_fr |
                           INSEE_REG %in% regions_fr, ]
    borders <- unique(borders)

    cities_fr_ID <- cities_fr_geom %>% select(ID_unique)

    # retrieve the geometries after the sorting
    borders <- merge(cities_fr_ID, borders, by = "ID_unique")
    borders <- st_as_sf(borders)
    sf::st_geometry(borders) <- "geom"

    borders <- borders %>% group_by(SIREN_EPCI) %>%
      summarise(geom= st_union(geom))
  }


  else {

    # retrieve cities' epci
    cities_epcis <- get_apigouv_insee_codes(communes_fr, epcis_fr, departments_fr, regions_fr)

    # Gather codes of cities and departments
    zones <- c(cities_epcis[code %in% communes_fr,code],
             cities_epcis[!(code %in% communes_fr),codeDepartement])
    zones <- unique(zones)

    # Request the ign api to retrieve geometries of the selected territories
    borders <- lapply(zones,
                       test_apicarto_commune)


    # Bind territories together
    borders <- do.call(rbind, borders)
    sf::sf_use_s2(FALSE)

    borders <- merge(cities_epcis[, list(code_insee=code, codeEpci)], borders, by = "code_insee")
    borders <- st_as_sf(borders)
    borders <- borders %>% group_by(SIREN_EPCI=codeEpci)  %>% summarise(geom= st_union(geometry))

  }


  # remove any holes in the geometry and project the geometry into the french system
  borders <- nngeo::st_remove_holes(borders)
  borders <- st_transform(borders, 3035)

  # save the geometry of the territory
  st_write(borders, here("data", "arep", "territory.gpkg"), delete_dsn = TRUE)

  # plot the geometry
  p <- ggplot()
  p <- p + geom_sf(data = borders, fill = NA)
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
