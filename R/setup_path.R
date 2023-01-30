#' Setup Path
#'
#' @description
#' `setup_path` ...
#'
#'
#' @param data_path A `character` or a list of `character` of french
#' region codes to be included in the perimeter
#'
#' @return data_path
#'
#'
#'
#' @importFrom checkmate assert_directory_exists
#'
#' @export
#'
setup_path <- function(data_path){

  checkmate::assert_directory_exists(data_path)

  data_path <- path.expand(data_path)

  dir.create(here(data_path, "territory"))
  dir.create(here(data_path, "copernicus"))
  dir.create(here(data_path, "corine_land_cover"))
  dir.create(here(data_path, "bd_foret"))

  message("\nDownloaded data will be stored at ", data_path)

  return(data_path)

}
