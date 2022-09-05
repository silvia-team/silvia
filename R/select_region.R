

epci_annecy <- c(200066793)


### GRAND GENEVE ###

epcis_fr <- c(247400724, 247400583, 200000172, 240100891, 200067551, 240100750, 200011773, 247400690)

path_communes_ch <- here('data', 'autre', "communes_suisses.xlsx")
communes_ch <- read_excel(path_communes_ch, sheet = "GDE")
cantons_ch_grand_geneve <- c(2228, 2500)
communes_ch_grand_geneve <- communes_ch %>%
  filter(GDEBZNR %in% cantons_ch_grand_geneve)
communes_ch <- communes_ch_grand_geneve$GDENR


### PACA 3 ###

epcis_PACA3 <- c(247400724, 247400583, 200000172) # Grand Genève, PACA 3
communes_ch_PACA3 = c("6635", "6636", "6640", "6613", "6612", "6621")
communes_fr_PACA3 = c("74008", "74012", "74040", "74094", "74118", "74133", "74145", "74153", "74298", "74305")

##############################
#
# epci <- epcis_Grand_Geneve[1]
# epci <- 200046977
# usethis::use_data(epci, internal = TRUE, overwrite = TRUE)

crop_region  <- function(epcis= c(),
                         communes_ch= NULL,
                         communes_fr = NULL) {
  cities <- st_read(here("data", "cities.gpkg"))
  selected_cities <- cities[cities$SIREN_EPCI %in% epcis | cities$INSEE_COM %in% communes_fr, ]
  region_fr <- st_transform(selected_cities, 3035)
  region_fr <- region_fr$geom
  region_fr <- st_combine(region_fr)

  cities_ch <- st_read(here("data", "cities_ch.gpkg"))
  # cities_ch <- cities_ch %>% mutate(Nom = str_replace_all(Nom, "\\s*\\([^\\)]+\\)", ""))
  selected_cities_ch <- cities_ch[cities_ch$num_OFS %in% communes_ch, ]
  region_ch <- st_transform(selected_cities_ch, 3035)
  st_geometry(region_ch) <- "geom"
  region_ch <- region_ch$geom
  region_ch <- st_combine(region_ch)


  region <- st_union(region_fr, region_ch, by_feature = FALSE, is_coverage = TRUE)
  region <- nngeo::st_remove_holes(region)


  region <- st_transform(region, 3035) # projected coordinate system for Europe
  st_write(region, here("data", "arep", "region.gpkg"), delete_dsn = TRUE)

  return(st_bbox(region))
}


#' Merge the territory chosen with the data from Corine Land Cover
#' of the chosen years
#' The function returns nothing but store the gpkg file
#' in the "data->arep" folder
#'
#'
#' @param box
#' @param yearn
#' @return None
#'
#' @importFrom stringr str_split
write_gpkg_files  <- function(box, years){

  box <- str_split(box, " ")
  box <- round(as.numeric(box))
  box <- paste(box,collapse=" ")

  # ---- Run the code above to get the perimeter of your EPCI ---- #
  command1 <- '"C:\\Program Files\\QGIS 3.22.7\\bin\\ogr2ogr"'
  command2 <- paste("-spat", box, "-clipsrc")
  command3 <- '"C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\territory.gpkg"'

  for (year in years) {
    command4 <- paste0('"', "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_", year, ".gpkg",'"',  sep = "")
    command5 <- paste0('"',"C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG_",
                       year, "\\CLC_Europe_", year, ".gpkg", '"', sep = "")
    final_command <- noquote(paste(command1, command2, command3, command4, command5))
    system(final_command)
  }
}

#' Crop the territory perimeter as one multipolygon
#' The function returns the 'box' of the created perimeter
#' and store the gpkg file in 'data->arep' folder
#' @param regions_fr
#' @param departments_fr
#' @param epcis_fr
#' @param communes_ch
#' @param communes_fr
#' @return the coordinates of the box that delineates the territory
#'
#' @importFrom data.table fwrite
#' @importFrom here here
#' @importFrom sf st_read st_write st_transform st_combine st_geometry st_union
#' @importFrom nngeo st_remove_holes
crop_region_V2  <- function(
    regions_fr = NULL,
    departments_fr = NULL,
    epcis_fr= NULL,
    communes_ch= NULL,
    communes_fr = NULL) {

  sf_objects <- c()


  if (!is.null(communes_fr)){
    cities_fr <- st_read(here("data", "arep", "cities_fr.gpkg"))
    selected_cities_fr <- cities_fr[cities_fr$INSEE_COM %in% communes_fr, ]
    region_fr <- st_transform(selected_cities_fr, 3035)
    region_fr <- region_fr$geom
    region_fr <- st_combine(region_fr)
    sf_objects <- append(sf_objects, region_fr)
  }
  if (!is.null(communes_ch)){
    cities_ch <- st_read(here("data","arep", "cities_ch.gpkg"))
    selected_cities_ch <- cities_ch[cities_ch$num_OFS %in% communes_ch, ]
    region_ch <- st_transform(selected_cities_ch, 3035)
    st_geometry(region_ch) <- "geom"
    region_ch <- region_ch$geom
    region_ch <- st_combine(region_ch)
    sf_objects <- append(sf_objects, region_ch)
  }
  if (!is.null(departments_fr)){
    selected_departments_fr <- st_read(here("data","arep", "departments_fr.gpkg"))
    selected_departments_fr <- selected_departments_fr[selected_departments_fr$insee_dep %in% departments_fr, ]
    departments_fr <- st_transform(selected_departments_fr, 3035)
    st_geometry(departments_fr) <- "geom"
    departments_fr <- departments_fr$geom
    sf_objects <- append(sf_objects, departments_fr)
  }
  if (!is.null(epcis_fr)){
    epcis <- st_read(here("data", "arep", "cities_fr.gpkg"))
    selected_epcis <- epcis[epcis$SIREN_EPCI %in% epcis_fr, ]
    epcis_fr <- st_transform(selected_epcis, 3035)
    st_geometry(epcis_fr) <- "geom"
    epcis_fr <- epcis_fr$geom
    sf_objects <- append(sf_objects, epcis_fr)

    #Store the epci for other package functions

    epci <- selected_epcis$SIREN_EPCI[1]
    usethis::use_data(epci, overwrite = TRUE)

  }
  if (!is.null(regions_fr)){
    regions <- st_read(here("data", "arep", "regions_fr.gpkg"))
    selected_regions_fr <- regions[regions$insee_reg %in% regions_fr, ]
    regions_fr <- st_transform(selected_regions_fr, 3035)
    st_geometry(regions_fr) <- "geom"
    regions_fr <- regions_fr$geom
    sf_objects <- append(sf_objects, regions_fr)
  }

  if (is.null(epcis_fr)){
    if (!is.null(regions_fr)){
      reg_number <- selected_regions_fr$insee_reg[1]
      epcis <- st_read(here("data", "arep", "cities_fr.gpkg"))
      epci <- epcis$SIREN_EPCI[epcis$INSEE_REG == reg_number][1]
      # usethis::use_data(epci, internal = TRUE, overwrite = TRUE)
    }
    else if (!is.null(departments_fr)){
      dep_number <- selected_departments_fr$insee_dep[1]
      epcis <- st_read(here("data", "arep", "cities_fr.gpkg"))
      epci <- epcis$SIREN_EPCI[epcis$INSEE_DEP == dep_number][1]
      # usethis::use_data(epci, overwrite = TRUE)
    }

    else if (!is.null(communes_fr)){
      selected_cities_fr <- cities_fr[cities_fr$INSEE_COM == communes_fr[1], ]
      epci <- selected_cities_fr$SIREN_EPCI
    }
    usethis::use_data(epci,overwrite = TRUE)
  }

  nb_territories <- length(sf_objects)

  if (nb_territories == 1){
    territory <- st_remove_holes(sf_objects[1])
    territory <- st_transform(territory, 3035)
  }

  else if (nb_territories > 1){
    territory <- sf_objects[1]
    for (i in 2:nb_territories)
      territory <- st_union(territory, sf_objects[i], by_feature = FALSE, is_coverage = TRUE)
  }
  territory <- st_transform(territory, 3035)
  territory <- nngeo::st_remove_holes(territory)

  st_write(territory, here("data", "arep", "territory.gpkg"), delete_dsn = TRUE)

  return(st_bbox(territory))
}

# box <- crop_region_V2(epcis_fr = epci_annecy)
# write_gpkg_files(box, c(1990, 2000, 2006, 2012, 2018))
