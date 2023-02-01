#' Download bd foret layer on the territory, for the selected years,
#' using 'happign' package
#' The function returns nothing but store gpkg files
#' in the "data->arep" folder
#'
#' @param shape shape of the territory
#' @param data_path path to where the data is stored
#'
#' @importFrom happign  get_layers_metadata get_wfs
#' @importFrom sf st_intersection st_area st_write
#'
download_bd_foret <- function(shape, data_path){

  message("\nDownload of the BD foret layer...")

  shape <- st_transform(shape, 4326)

  apikey_forest <- "environnement"
  name_forest_layer <- "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale"
  forest_wfs <- get_wfs(shape = shape, apikey = apikey_forest, layer_name = name_forest_layer)
  sf::sf_use_s2(FALSE)
  forest <- st_intersection(forest_wfs, shape)
  forest$area <- st_area(forest)

  st_write(forest, here(data_path, "bd_foret", "zone_bd_foret.gpkg"), delete_dsn = TRUE)
}
