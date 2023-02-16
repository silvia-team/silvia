#' Setups the path pointing to the directory where data will be saved
#'
#' Checks if the path points to an existing folder, and builds four new folders
#' that will hold the downloaded data : bd_foret, copernicus, corine_land_cover, territory.
#'
#'
#' @param data_path A string pointing to the directory where the downloaded data will be saved
#'
#' @importFrom cli cli_alert_info
#' @return data_path
#'
#'
#' @return A valid path pointing to the directory where the downloaded data will be saved
#'
#' @export
#'
setup_path <- function(data_path){

  data_path <- file.path(data_path)

  if(!file.exists(data_path)){
    stop("\nA valid path is required to build the repositories.")
  }

  data_path <- path.expand(data_path)

  dir.create(here(data_path, "copernicus"))
  dir.create(here(data_path, "bd_foret"))
  dir.create(here(data_path, "corine_land_cover"))
  dir.create(here(data_path, "territory"))

  cli_alert_info(paste0("Downloaded data will be stored at ", data_path))

  return(data_path)

}
