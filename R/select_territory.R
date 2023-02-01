#' Select territory
#'
#' @description
#' `select_territory` crops spatial boundaries to be studied.
#' Codes of the different french territories
#' (communes, EPCI, départements, régions) are  referenced here
#' [INSEE's codes](https://www.insee.fr/fr/information/6051727)
#'
#' @description
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
#' @param regions_fr A `character` or a list of `character` of french
#' region codes to be included in the perimeter
#' @param departments_fr A `character` or a list of `character` of french
#' departements codes to be included in the perimeter
#' @param epcis_fr A `character` or a list of `character` of french
#' EPCIs codes to be included in the perimeter
#' @param communes_fr A `character` or a list of `character` of french
#' municipalities codes to be included in the perimeter
#' @param data_path path to where the data is stored
#'
#' @return shape of the selected territory
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write st_transform st_combine
#' @importFrom sf st_geometry st_union st_bbox st_geometry
#' @importFrom nngeo st_remove_holes
#' @importFrom httr2 request req_url_path resp_body_string req_url_query
#' @importFrom jsonlite fromJSON
#' @importFrom happign get_apicarto_commune
#'
#'
select_territory  <- function(
    regions_fr = list(),
    departments_fr = list(),
    epcis_fr= list(),
    communes_fr = list(),
    data_path) {

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


  # remove any holes in the geometry and project the geometry into the french system
  borders <- nngeo::st_remove_holes(borders)
  borders <- st_transform(borders, 3035)

  # save the geometry of the territory
  st_write(borders, here(data_path, "territory", "territory.gpkg"), delete_dsn = TRUE)

  return(borders)
}
