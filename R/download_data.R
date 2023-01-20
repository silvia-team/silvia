#' Download data
#'
#' Download data from IGN and ALDO needed to run the tool.
#'
#'
#' @usage
#' download_data()
#'
#' @export
#'
#' @return None
#' @importFrom data.table fwrite
#' @importFrom happign get_layers_metadata get_wfs
#' @importFrom here here
#' @importFrom sf st_read st_write st_transform
download_data <- function(){

  options(warn=-1)

  # create a folder to deposit downloaded data from the Aldo excel tool
  dir.create(here("data", "aldo", "downloaded_data"))

  # create a folder to deposit downloaded data during the use of the tool (data from APIs mostly)
  dir.create(here("data", "arep"))

  dir.create(here("data", "copernicus"))

  # load carbon storage data from Aldo
  extract_aldo_carbon_stocks()

  # load carbon flows data from Aldo
  extract_aldo_carbon_flows()

  # load_ign_aldo_data()

}

