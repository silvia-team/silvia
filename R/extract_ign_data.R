##############################
# Load libraries

library(sf)
library(data.table)
library(here)

##############################

#' Save the data of the IGN cities classification.
#' The data is stored in the "data -> ign" directory
#' @param path_cities
#' @param path_cities_category
#' @return None
#' @export
#' @importFrom data.table fwrite setnames
#' @importFrom here here
retrive_cities <- function(path_cities, path_cities_category) {

  # Load IGN cities geometries
  cities <- st_read(path_cities, quiet = TRUE)

  # Add cities categories
  city_category <- as.data.table(read_excel(path_cities_category, sheet = "Composition_communale", skip = 5))
  city_category <- city_category[, list(CODGEO, STATUT_2017)]
  setnames(city_category, c("INSEE_COM", "city_category"))
  city_category[city_category == "H", city_category := "R"]  #Replace H by R ?

  cities <- merge(cities, city_category, by = "INSEE_COM")
  st_write(cities, here("data",  "cities.gpkg"), delete_dsn = TRUE)

  return(invisible(0))
}

retrive_cities_ch <- function(path_cities_ch){

  cities_ch <- st_read(path_cities_ch, quiet = TRUE)
  cities_ch <- cities_ch %>%
    select(GMDNR, KTNR)
  names(cities_ch) <- c('num_OFS', "KTNR",  "geometry")

  st_write(cities_ch, here("data",  "cities_ch.gpkg"), delete_dsn = TRUE)

  return(invisible(0))

}

path_cities <- here("data", "ign",
                    "ADMIN-EXPRESS_3-1__SHP__FRA_L93_2022-06-21/ADMIN-EXPRESS_3-1__SHP__FRA_L93_2022-06-21/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2022-06-21/ADE_3-1_SHP_LAMB93_FR/COMMUNE.shp")
path_cities_category <- here("data", "insee", "UU2020_au_01-01-2022.xlsx")

path_cities_ch <- here("data", "ign", "suisse", "g1g22_20220501.shp")

# retrive_cities(path_cities, path_cities_category)

retrive_cities_ch(path_cities_ch)



