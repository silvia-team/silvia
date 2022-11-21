
#' Collect land use changes between two years
#' @param year_from
#' @param year_to
#' @param remove_unchanged
#' @return a sf object with the land use changes between year_to and year_from
#' @export
#' @importFrom sf st_intersection st_area
#' @importFrom tibble rownames
get_land_use_changes <- function(year_from, year_to, remove_unchanged = TRUE, count = FALSE) {

  delta_years = year_to - year_from
  load(here("data", "epci.rda"))

  ### Retrieve carbon stocks from the chosen years
  dt_from <- silvia::get_carbon_storage(year_from, count)
  dt_to <- silvia::get_carbon_storage(year_to, count)

  dt_land_use_changes <-  st_intersection(dt_from, dt_to)
  dt_land_use_changes <- tibble::rowid_to_column(dt_land_use_changes, "ID_unique")
  dt_land_use_changes <- dt_land_use_changes %>%
    select(EPCI_Siren,
           ID_unique,
           code_initial = code,
           soil_category_initial = aldo_soil_category,
           biomass_category_initial = aldo_biomass_category,
           code_final = code.1,
           soil_category_final = aldo_soil_category.1,
           biomass_category_final = aldo_biomass_category.1,
           X_mean_initial = X_mean,
           X_mean_final = X_mean.1
    )
  # dt_land_use_changes <-  na.omit(dt_land_use_changes)
  dt_land_use_changes$area <- as.numeric(st_area(dt_land_use_changes))*1e-4

  dt_land_use_changes$code_initial_first <- as.numeric(substr(dt_land_use_changes$code_initial, 1, 1))
  dt_land_use_changes$code_final_first <- as.numeric(substr(dt_land_use_changes$code_final, 1, 1))

  if (remove_unchanged == TRUE){
    dt_land_use_changes <- dt_land_use_changes %>% filter(code_initial != code_final)
  }

  return(dt_land_use_changes)
}

