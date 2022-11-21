
#' Save the data of the IGN (french) cities classification.
#' The data is stored in the "data -> arep" directory
#' @param path_cities
#' @param path_cities_category
#' @return None
#'
#' @importFrom data.table fwrite setnames
#' @importFrom here here
#' @importFrom sf st_read st_write
retrieve_cities_fr <- function(path_cities_fr, path_cities_category_fr) {

  # Load IGN cities geometries
  cities <- st_read(path_cities_fr, quiet = TRUE)

  # Add cities categories
  city_category <- as.data.table(read_excel(path_cities_category_fr, sheet = "Composition_communale", skip = 5))
  city_category <- city_category[, list(CODGEO, STATUT_2017)]
  setnames(city_category, c("INSEE_COM", "city_category"))
  city_category[city_category == "H", city_category := "R"]  #Replace H by R ?

  cities <- merge(cities, city_category, by = "INSEE_COM")
  st_write(cities, here("data", "arep", "cities_fr.gpkg"), delete_dsn = TRUE)

  return(invisible(0))
}


#' Save the data of the swiss cities classification.
#' The data is stored in the "data -> arep" directory
#' @param path_cities_ch
#' @return None
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write
#'
retrieve_cities_ch <- function(path_cities_ch){

  cities_ch <- st_read(path_cities_ch, quiet = TRUE)
  cities_ch <- cities_ch %>%
    select(GMDNR, KTNR)
  names(cities_ch) <- c('num_OFS', "KTNR",  "geometry")

  st_write(cities_ch, here("data", "arep",  "cities_ch.gpkg"), delete_dsn = TRUE)

  return(invisible(0))
}

#' Save the data of french departments classification.
#' The data is stored in the "data -> arep" directory
#' @param path_departements_fr
#' @return None
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write
retrieve_departements_fr <- function(path_departments_fr) {

  # Load IGN departements geometries
  departements <- st_read(path_departements_fr, quiet = TRUE)
  departements <- departements %>%
    select(NOM, INSEE_DEP)
  names(departements) <- c('nom', "insee_dep",  "geometry")
  st_write(departements, here("data", "arep", "departments_fr.gpkg"), delete_dsn = TRUE)

  return(invisible(0))
}

#' Save the data of french regions classification.
#' The data is stored in the "data -> arep" directory
#' @param path_regions_fr
#' @return None
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write
retrieve_regions_fr <- function(path_regions_fr) {

  # Load IGN regions geometries
  regions <- st_read(path_regions_fr, quiet = TRUE)
  regions <- regions %>%
    select(NOM, INSEE_REG)
  names(regions) <- c('nom', "insee_reg",  "geometry")
  st_write(regions, here("data", "arep", "regions_fr.gpkg"), delete_dsn = TRUE)

  return(invisible(0))
}

# path_cities_fr <- here("data", "ign", "france","COMMUNE.shp")
# path_cities_category_fr <- here("data", "insee", "UU2020_au_01-01-2022.xlsx")
# retrieve_cities_fr(path_cities_fr, path_cities_category_fr)
#
# path_cities_ch <- here("data", "ign", "suisse", "g1g22_20220501.shp")
# retrieve_cities_ch(path_cities_ch)
#
# path_departments_fr <- here("data", "ign", "france","DEPARTEMENT.shp")
# retrieve_departements_fr(path_departments_fr)

# path_regions_fr <- here("data", "ign", "france","REGION.shp")
# retrieve_regions_fr(path_regions_fr)
