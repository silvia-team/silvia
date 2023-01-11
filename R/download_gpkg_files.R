#' Download the territory chosen with the data from Corine Land Cover
#' of the chosen years, using 'happign' package
#' The function returns nothing but store the gpkg file
#' in the "data->arep" folder
#'
#' @param years
#' @param bd_foret
#' @param download_impermab
#' @return None
#'
#' @importFrom data.table as.data.table
#' @importFrom sf st_intersection sf_use_s2 st_read st_transform st_write st_crs
#' @importFrom stringr str_sub
#' @importFrom happign  get_wfs get_layers_metadata
#' @importFrom exactextractr exact_extract
#' @importFrom raster crop raster mask
download_gpkg_files <- function(years, bd_foret = T, download_impermab = T){

  shape <- st_read(here("data", "arep", "territory.gpkg"))
  shape <- st_transform(shape, 4326)

  if (download_impermab == T){
    download_impermability_layers(shape= shape)
  }


  apikey <- "clc"
  clc_layers <- as.data.table(happign::get_layers_metadata(apikey = apikey, data_type = "wfs"))

  for (year in years){
    print(year)
    year_abr <- str_sub(year,-2,-1)
    title <- paste0("LANDCOVER.CLC", year_abr, "_FR:clc", year_abr, "_fr.title")
    name_clc_layer <- clc_layers[Title == title, Name]
    clc <- get_wfs(shape = shape, apikey = apikey,
                   layer_name = name_clc_layer)
    sf::sf_use_s2(FALSE)
    clc <- st_intersection(clc, shape)


    {
      if (download_impermab == T){
        if (year %in% c(1990, 2000, 2006, 2012)){
          path_imper <- here("data", "copernicus", "impermab_12.tif")
        }
        else if (year == 2018){
          path_imper <- here("data", "copernicus", "impermab_15.tif")
        }
      }
      else {
        if (year %in% c(2006, 2012, 2018)){
          path_imper <- paste0("D:\\docs\\impermab\\merged_", as.character(year), ".tif")
        } else {
          path_imper <- "D:\\docs\\impermab\\merged_2012.tif"
        }
      }
    }

    impermab <- raster(path_imper)
    clc <- st_transform(clc, sf::st_crs(impermab))
    site_imper <- raster::crop(impermab, raster::extent(clc))
    clc$mean <- exact_extract(site_imper, clc, 'mean')
    clc$mean <- - (clc$mean- max(clc$mean))
    clc$X_mean <- clc$mean/max(clc$mean)
    clc <- st_transform(clc, sf::st_crs(shape))
    clc <- st_intersection(clc, shape)
    clc <- st_transform(clc, 3035)
    st_write(clc, here("data", "arep", paste0("zone_", as.character(year), ".gpkg")), delete_dsn = TRUE)
  }

  if (bd_foret == T){
    apikey_forest <- "environnement"
    layers_forest <- get_layers_metadata(apikey = apikey_forest, data_type = "wfs")
    name_forest_layer <- "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale"
    forest_wfs <- get_wfs(shape = shape, apikey = apikey_forest, layer_name = name_forest_layer)
    sf::sf_use_s2(FALSE)
    forest <- st_intersection(forest_wfs, shape)
    forest$area <- st_area(forest)
    st_write(forest, here("data", "arep", "zone_bd_foret.gpkg"), delete_dsn = TRUE)
  }
}


#' Download impermability layers on the territory, for the selected years,
#' using 'happign' package
#' The function returns nothing but store raster files
#' in the "data->copernicus" folder
#'
#' @param shape
#' @param years
#' @return None
#'
#' @importFrom happign  get_wms_raster
#' @importFrom here here
#' @imporrtFrom raster brick aggregate mean writeRaster
download_impermability_layers <- function(shape, years = c("12", "15")){

  do.call(file.remove, list(list.files(here("data", "copernicus"), full.names = TRUE)))
  for (year in years){
    apikey_impermab <- "clc"
    name_impermab_layer <- paste0("LANDCOVER.HR.IMD.CLC", year)
    impermab <- happign::get_wms_raster(shape= shape, apikey= apikey_impermab,
                                        layer_name = name_impermab_layer, resolution= 30,
                                        filename = here("data", "copernicus", paste0("impermab_", year, ".tif")))
    file_path <- here("data", "copernicus", paste0("impermab_", year, ".tif"))
    impermab <- raster::brick(file_path, package= "raster")
    impermab <- raster::aggregate(impermab, fact=6)
    impermab <- raster::calc(impermab, fun = mean, na.rm = T)
    raster::writeRaster(x= impermab, filename = file_path, overwrite= T)

  }
}
