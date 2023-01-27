#' Get land use changes
#'
#' @description
#' `get_land_use_changes` collects land use changes between two years
#'
#' @usage
#' get_land_use_changes(year_from = 2000, year_to = 2018)
#'
#' @param year_from the CLC reference year (1990, 2000, 2006, 2012, 2018)
#' @param year_to the year to be compared to the reference year,
#' must be superior to reference year
#' @param data_path path to where the data is stored
#'
#' @return a sf object with the land use changes between year_to and year_from
#'
#' @importFrom sf st_intersection st_area st_buffer
get_land_use_changes <- function(year_from, year_to, data_path) {

  # Check if year_from < year_to
  if(year_from > year_to){
    stop("year_from must be inferior to year_to")
  }

  options(warn = -1)

  delta_years = year_to - year_from

  ### Retrieve carbon stocks from the chosen years
  dt_from <- get_carbon_stocks(year_from, data_path)
  dt_from <- sf::st_buffer(dt_from, dist=0)
  dt_to <- get_carbon_stocks(year_to, data_path)
  dt_to <- sf::st_buffer(dt_to, dist=0)

  # difference between two years
  dt_land_use_changes_geom <-  st_intersection(dt_from, dt_to)

  dt_land_use_changes_geom <- tibble::rowid_to_column(dt_land_use_changes_geom, "ID_unique")
  dt_land_use_changes_geom$area <- as.numeric(st_area(dt_land_use_changes_geom))*1e-4
  dt_land_use_changes <- st_drop_geometry(dt_land_use_changes_geom)
  dt_land_use_changes <- as.data.table(dt_land_use_changes)
  dt_land_use_changes <- dt_land_use_changes[area >0.1, ]


  dt_land_use_changes <- dt_land_use_changes[, list(
    EPCI_Siren,
    ID_unique,
    code_initial = code,
    soil_category_initial = aldo_soil_category,
    biomass_category_initial = aldo_biomass_category,
    code_final = code.1,
    soil_category_final = aldo_soil_category.1,
    biomass_category_final = aldo_biomass_category.1,
    X_mean_initial = X_mean,
    X_mean_final = X_mean.1,
    area
    )]

  dt_land_use_changes[, code_initial_first := as.numeric(substr(code_initial, 1, 1))]
  dt_land_use_changes[, code_final_first := as.numeric(substr(code_final, 1, 1))]

  forest_codes <- c(311, 312, 313, 324)
  dt_land_use_changes <- dt_land_use_changes[code_initial != code_final | (code_initial %in% forest_codes & code_initial == code_final), ]

  dt_land_use_changes_geom <- dt_land_use_changes_geom %>% select(ID_unique)
  dt_land_use_changes <- merge(dt_land_use_changes, dt_land_use_changes_geom, by = "ID_unique")

  dt_land_use_changes <- st_as_sf(dt_land_use_changes)

  return(dt_land_use_changes)
}

