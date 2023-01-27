#' Download the territory chosen with the data from Corine Land Cover
#' of the chosen years, using 'happign' package
#' The function returns nothing but store the gpkg file
#' in the "data->arep" folder
#'
#' @param years years to download
#' @param data_path path to where the data is stored
#' @param bd_foret boolean to download bd_foret
#'
#'
#' @importFrom data.table as.data.table
#' @importFrom sf st_intersection sf_use_s2 st_read st_transform st_write st_crs
#' @importFrom stringr str_sub
#' @importFrom happign  get_wfs get_layers_metadata
#' @importFrom exactextractr exact_extract
#' @importFrom raster crop raster mask
download_gpkg_files <- function(years, data_path, bd_foret = F){

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, 4326)

  # download impermability layers
  download_impermability_layers(shape= shape, data_path = data_path)

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


    if (year %in% c(1990, 2000, 2006, 2012)){
          path_imper <- here(data_path, "copernicus", "impermab_12.tif")
        }
    else if (year == 2018){
          path_imper <- here(data_path,  "copernicus", "impermab_15.tif")
    }


    # Intersection and computation of impermability with clc...
    impermab <- raster(path_imper)
    clc <- st_transform(clc, sf::st_crs(impermab))
    site_imper <- raster::crop(impermab, raster::extent(clc))
    clc$mean <- exact_extract(site_imper, clc, 'mean')
    clc$mean <- - (clc$mean- max(clc$mean))
    clc$X_mean <- clc$mean/max(clc$mean)
    clc <- st_transform(clc, sf::st_crs(shape))
    clc <- st_intersection(clc, st_geometry(shape))
    clc <- st_transform(clc, 3035)

    clc$year <- year

    clc <- clc %>%
      select(contains(c("ID", "code", "area", "year", "X_mean", "SIREN_EPCI"))) %>%
      dplyr::rename_at(dplyr::vars(contains("code")), ~c("code"))

    st_write(clc, here(data_path, "corine_land_cover", paste0("zone_", as.character(year), ".gpkg")), delete_dsn = TRUE)
  }

  if (bd_foret == T){
    download_bd_foret(shape= shape, data_path = data_path)
  }
}

