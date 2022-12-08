
#' download the files needed to run the tool
#' @return None
#' @importFrom data.table fwrite
#' @importFrom happign get_layers_metadata get_wfs
#' @importFrom here here
#' @importFrom sf st_read st_write st_transform
load_data <- function(){

  # load carbon storage data from Aldo
  extract_aldo_carbon_stocks()

  # load carbon flows data from Aldo
  extract_aldo_carbon_flows()

  load_ign_aldo_data()
  }

