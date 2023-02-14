#' Apigouv insee codes
#'
#' Retrieve insee codes from either epcis codes of departement codes
#' Codes of the different french territories
#' (communes, EPCI, départements, régions) are  referenced here
#' [INSEE's codes](https://www.insee.fr/fr/information/6051727)
#'
#'
#' The function returns the map of the created perimeter
#' and store the gpkg file in 'data->arep' folder as "territory.gpkg".
#'
#' @usage
#'
#' get_apigouv_insee_codes(
#'     epcis_fr,
#'     regions_fr)
#'
#' @param communes_fr A `character` or a list of `character` of french
#' cities insee codes
#' @param epcis_fr A `character` or a list of `character` of french
#' epcis insee codes
#' @param departments_fr A `character` or a list of `character` of french
#' department insee codes
#' @param regions_fr A `character` or a list of `character` of french
#' regions insee codes
#'
#'
#' @return `get_apigouv_insee_codes` returns a list of cities and departments insee codes
#'
#' @examples
#'
#' # Retrieve insee city codes in the epci of "Grand Annecy"
#' get_apigouv_insee_codes(epcis_fr = "200066793")
#'
#' select_territory(regions_fr = "93", departments_fr = "31", epcis_fr = c("246900740", "200046977"), communes_fr = "81004")
#' @export
#'
#' @importFrom data.table fwrite as.data.table
#' @importFrom httr2 request req_url_path req_url_query req_perform resp_body_string
#' @importFrom jsonlite fromJSON
get_apigouv_insee_codes <- function(communes_fr= list(),
                                    epcis_fr = list(),
                                    departments_fr = list(),
                                    regions_fr= list()){


  all_city_codes <- list()

  # Build a request for each city
  for (city in communes_fr){

    res <- request("https://geo.api.gouv.fr") %>%
      req_url_path("communes") %>%
      req_url_query(
        code = city
      ) %>%
      req_perform()%>%
      resp_body_string()

    df <- jsonlite::fromJSON(res)
    df <- as.data.table(df)

    city_codes_city <- df[, c("code", "codeEpci", "codeDepartement")]
    all_city_codes <- rbind(all_city_codes, city_codes_city)

  }

  # Build a request for each epci
  for (epci in epcis_fr){

    res <- request("https://geo.api.gouv.fr") %>%
      req_url_path("communes") %>%
      req_url_query(
        codeEpci = epci
      ) %>%
      req_perform()  %>%
      resp_body_string()

    df <- jsonlite::fromJSON(res)
    df <- as.data.table(df)

    city_codes_epci <- df[, c("code", "codeEpci", "codeDepartement")]

    all_city_codes <- rbind(all_city_codes, city_codes_epci)
  }

  # Build a request for each department
  for (dep in departments_fr){

    res <- request("https://geo.api.gouv.fr") %>%
      req_url_path("communes") %>%
      req_url_query(
        codeDepartement = dep
      ) %>%
      req_perform()  %>%
      resp_body_string()

    df <- jsonlite::fromJSON(res)
    df <- as.data.table(df)

    city_codes_dep <- df[, c("code", "codeEpci", "codeDepartement")]

    all_city_codes <- rbind(all_city_codes, city_codes_dep)
  }

  # Build a request for each region
  for (region in regions_fr){
    res <- request("https://geo.api.gouv.fr") %>%
      req_url_path("communes") %>%
      req_url_query(
        codeRegion = region
      ) %>%
      req_perform()  %>%
      resp_body_string()

    df <- jsonlite::fromJSON(res)
    df <- as.data.table(df)

    city_codes_region <- df[, c("code", "codeEpci", "codeDepartement")]

    all_city_codes <- rbind(all_city_codes, city_codes_region)
  }

  city_codes <- unique(all_city_codes)
  city_codes <- as.data.table(city_codes)

  return(city_codes)

}
