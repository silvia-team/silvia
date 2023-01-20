#'Load ign aldo data
#'
#' Check if Write a `cities.gpkg` files with data on french  cities.
#'
#' @usage load_ign_aldo_data()
#'
#'
#' @return None
#' @importFrom data.table merge as.data.table setnames
#' @importFrom here here
#' @importFrom sf st_read st_write
#' @importFrom readxl read_excel
load_ign_aldo_data <- function(){

  if(!file.exists(here("data", "ign", "cities_files","COMMUNE.shp"))){
    # dir.create(here("data", "ign", "cities_files"))
    # stop("The folder silvia/data/ign/cities_files/COMMUNE.shp doesn't exist.
    # You need to download the french 'decoupage administraif' of 'France métropolitaine' here :
    # [geoservice](https://geoservices.ign.fr/adminexpress). It takes about 10 minutes.
    # Paste all the 'COMMUNE' files in the 'silvia/data/ign/cities_files' folder")
  }

  else{
    path_cities_fr <- here("data", "ign", "cities_files", "COMMUNE.shp")
    cities <- st_read(path_cities_fr, quiet = TRUE)

    path_cities_category_fr <- here("data", "insee", "communes", "UU2020_au_01-01-2022.xlsx")
    city_category <- read_excel(path_cities_category_fr, sheet = "Composition_communale", skip = 5)
    city_category <- as.data.table(city_category)
    city_category <- city_category[, list(INSEE_COM= CODGEO, STATUT_2017)]
    city_category[city_category == "H", city_category := "R"]
    cities <- merge(cities, city_category, by = "INSEE_COM")

    st_write(cities, here("data", "arep", "cities_fr.gpkg"), delete_dsn = TRUE)
     }
}
#'
