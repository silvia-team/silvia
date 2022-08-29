##############################
# Load libraries

library(here)
library(sf)
library(readxl)
library(data.table)
library(nngeo)

##############################

# Filter cities

# epcis <- c(200066793) #Annecy


### GRAND GENEVE ###

epcis_Grand_Geneve <- c(247400724, 247400583, 200000172, 240100891, 200067551, 240100750, 200011773, 247400690)

path_communes_ch <- here('data', 'autre', "communes_suisses.xlsx")
communes_ch <- read_excel(path_communes_ch, sheet = "GDE")
cantons_ch_grand_geneve <- c(2228, 2500)
communes_ch_grand_geneve <- communes_ch %>%
  filter(GDEBZNR %in% cantons_ch_grand_geneve)
communes_ch_grand_geneve <- communes_ch_grand_geneve$GDENR


### PACA 3 ###

epcis_PACA3 <- c(247400724, 247400583, 200000172) # Grand Genève, PACA 3
communes_ch_PACA3 = c("6635", "6636", "6640", "6613", "6612", "6621")
communes_fr_PACA3 = c("74008", "74012", "74040", "74094", "74118", "74133", "74145", "74153", "74298", "74305")

##############################

epci <- epcis_Grand_Geneve[1]

retrieve_epci_name <- function(path_to_aldo, epcis) {

  dt <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  dt <- as.data.table(dt)
  dt <- dt[, c(2, 5)]

  colnames(dt) <- c('EPCI_Siren', 'EPCI_name')

  epci_name <- dt$EPCI_name[dt$EPCI_Siren == epci]

  return(epci_name)
}

path_to_aldo <- "../../ALDO/Outil ALDO_2021_12.xlsx"
epci_name <- retrieve_epci_name(path_to_aldo, epci)

crop_region  <- function(path_to_aldo, epcis= c(),
                         communes_ch= NULL, communes_fr = NULL) {
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

# crop_region(path_to_aldo, epcis =  epcis_Grand_Geneve,
#              communes_ch = communes_ch_grand_geneve, communes_fr = NULL)

# ---- Run the code above to get the perimeter of your EPCI ---- #

# 1990
# "C:\Program Files\QGIS 3.22.7\bin\ogr2ogr" -spat 3988837 2541888 4054963 2611182 -clipsrc "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_1990.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG_1990\CLC_Europe_1990.gpkg"

#2000
# "C:\Program Files\QGIS 3.22.7\bin\ogr2ogr" -spat 3988837 2541888 4054963 2611182 -clipsrc "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_2000.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG_2000\CLC_Europe_2000.gpkg"

#2006
# "C:\Program Files\QGIS 3.22.7\bin\ogr2ogr" -spat 3988837 2541888 4054963 2611182 -clipsrc "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_2006.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG_2006\CLC_Europe_2006.gpkg"

# 2012
# "C:\Program Files\QGIS 3.22.7\bin\ogr2ogr" -spat 3988837 2541888 4054963 2611182 -clipsrc "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_2012.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG_2012\CLC_Europe_2012.gpkg"

# 2018.
# "C:\Program Files\QGIS 3.22.7\bin\ogr2ogr" -spat 3988837 2541888 4054963 2611182 -clipsrc "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_2018.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG_2018\CLC_Europe_2018.gpkg"


