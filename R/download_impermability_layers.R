
#' Download impermability layers on the territory, for the selected years,
#' using 'happign' package
#' The function returns nothing but store raster files
#' in the "data->copernicus" folder
#'
#' @param shape shape of the territory
#' @param data_path path to where the data is stored
#'
#' @importFrom happign  get_wms_raster
#' @importFrom here here
#' @importFrom raster brick aggregate mean writeRaster
download_impermability_layer <- function(shape, data_path){

  options(warn=-1)
  message("\n Download of the impermeability layer...")

  do.call(file.remove, list(list.files(here(data_path, "copernicus"), full.names = TRUE)))

  apikey_impermab <- "clc"
  year <- "15"
  name_impermab_layer <- paste0("LANDCOVER.HR.IMD.CLC", year)
  impermab <- happign::get_wms_raster(shape= shape, apikey= apikey_impermab,
                                      layer_name = name_impermab_layer, resolution= 40,
                                      filename = here(data_path, "copernicus", paste0("impermab_", year, ".tif")))
  file_path <- here(data_path, "copernicus", paste0("impermab_", year, ".tif"))
  impermab <- raster::brick(file_path, package= "raster")
  impermab <- raster::aggregate(impermab, fact=6)
  impermab <- raster::calc(impermab, fun = mean, na.rm = T)
  raster::writeRaster(x= impermab, filename = file_path, overwrite= T)

}
