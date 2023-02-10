#' Setups the path pointing to the directory where data will be saved
#'
#' Checks if the path points to an existing folder, and builds four new folders
#' that will hold the downloaded data : bd_foret, copernicus, corine_land_cover, territory.
#'
#'
#' @param data_path A string pointing to the directory where the downloaded data will be saved
#'
#' @return data_path
#'
#' @importFrom checkmate assert_directory_exists
#'
#' @examples
#'
#' data_path <- setup_path(data_path = "D:/data_silvia")
#'
#' @return A valid path pointing to the directory where the downloaded data will be saved
#'
#' @export
#'
setup_path <- function(data_path){

  data_path <- file.path(data_path)

  file.exists(data_path)

  data_path <- path.expand(data_path)

  dir.create(here(data_path, "copernicus"))
  dir.create(here(data_path, "bd_foret"))
  dir.create(here(data_path, "corine_land_cover"))
  dir.create(here(data_path, "territory"))

  message("\nDownloaded data will be stored at ", data_path)

  return(data_path)

}
