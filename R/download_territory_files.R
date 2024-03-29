#' @title
#' Download all the territory's files
#'
#' @description
#' `download_territory_files` downloads all the necessary files related to the territory.
#'
#' communes, EPCI, départements, and régions INSEE codes are  referenced here
#' [INSEE's codes](https://www.insee.fr/fr/information/6051727)
#'
#' @param communes_fr A `character` or a list of `character` of french
#' municipalities codes to be included in the perimeter
#' @param epcis_fr A `character` or a list of `character` of french
#' EPCIs codes to be included in the perimeter
#' @param departments_fr A `character` or a list of `character` of french
#' departements codes to be included in the perimeter
#' @param regions_fr A `character` or a list of `character` of french
#' region codes to be included in the perimeter
#' @param years years to download (1990, 2000, 2006, 2012, 2018)
#' @param data_path path to where the data is stored
#'
#'
#' @importFrom data.table as.data.table fwrite
#' @importFrom here here
#' @importFrom sf st_intersection sf_use_s2 st_read st_transform st_write st_crs
#' @importFrom sf st_combine st_union st_geometry
#' @importFrom stringr str_sub
#' @importFrom happign  get_wfs get_layers_metadata get_apicarto_commune
#' @importFrom raster crop raster mask
#' @importFrom ggplot2 ggplot geom_sf
#' @importFrom nngeo st_remove_holes
#' @importFrom httr2 request req_url_path resp_body_string req_url_query
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert cli_alert_success
#'
#'
#' @export
#'
download_territory_files <- function(communes_fr = list(),
                                     epcis_fr = list(),
                                     departments_fr = list(),
                                     regions_fr = list(),
                                     years,
                                     data_path){


  options(warn=-1)

  # check if selected years are correct
  clc_years <- c("1990", 1990, "2000", 2000, "2006", 2006, "2012", 2012, "2018", 2018)
  for (year in years){
    stopifnot("The selected years must be one of the following : 1990, 2000, 2006, 2012, 2018" = year %in% clc_years)
  }

  # Download and plot the selected territory

  cli_alert("Downloading territory's borders...")
  shape <- suppressMessages(
    select_territory(communes_fr = communes_fr,
                            epcis_fr = epcis_fr,
                            departments_fr = departments_fr,
                            regions_fr = regions_fr,
                            data_path = data_path)
    )
  shape <- st_transform(shape, 4326)

  # plot the geometry
  p <- ggplot()
  p <- p + geom_sf(data = shape, fill = NA)
  p <- p + theme_void()
  p <- p + theme(
    legend.position = "right",
    legend.justification = "left",
    panel.border = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", size = 12),
    strip.text = element_text(size = 14),
    legend.margin = margin(0, 0, 0.5, 0, "cm")
  )
  cli_alert_success("Downloaded territory's borders.")


  # download impermeability layers
  cli_alert("Downloading impermeability layer...")
  suppressMessages(download_impermability_layer(shape= shape, data_path = data_path))
  cli_alert_success("Downloaded impermeability layer.")

  # Download CLC layers

  apikey <- "clc"
  clc_layers <- as.data.table(get_layers_metadata(apikey = apikey, data_type = "wfs"))
  for (year in years){
    cli_alert(paste0("Downloading Corine Land Cover ", year, "..."))
    year_abr <- str_sub(year,-2,-1)
    title <- paste0("LANDCOVER.CLC", year_abr, "_FR:clc", year_abr, "_fr.title")
    name_clc_layer <- clc_layers[Title == title, Name]
    clc <- get_wfs(shape = shape, apikey = apikey,
                   layer_name = name_clc_layer)
    suppressMessages(sf::sf_use_s2(FALSE))
    clc <- suppressMessages(st_intersection(clc, shape))


    # Intersection and computation of impermeability with clc...

    path_imper <- here(data_path,  "copernicus", "impermab_15.tif")
    impermab <- raster(path_imper)
    clc <- st_transform(clc, sf::st_crs(impermab))
    site_imper <- raster::crop(impermab, raster::extent(clc))
    clc$mean <- exact_extract(site_imper, clc, 'mean')
    clc$mean <- - (clc$mean- max(clc$mean))
    clc$X_mean <- clc$mean/max(clc$mean)
    clc <- st_transform(clc, sf::st_crs(shape))
    clc <- suppressMessages(st_intersection(clc, st_geometry(shape)))
    clc <- st_transform(clc, 3035)

    clc$year <- year

    clc <- clc %>%
      select(contains(c("ID", "code", "area", "year", "X_mean", "SIREN_EPCI"))) %>%
      dplyr::rename_at(dplyr::vars(contains("code")), ~c("code"))

    st_write(clc, here(data_path, "corine_land_cover", paste0("zone_", as.character(year), ".gpkg")),
             delete_dsn = TRUE, quiet = T)
    cli_alert_success(paste0("Downloaded Corine Land Cover ", year, "."))
  }

   #download bd_foret
   cli_alert("Downloading BD foret layer...")
   download_bd_foret(shape= shape, data_path = data_path)
   cli_alert_success("Downloaded BD foret layer.")

   print(p)
}

