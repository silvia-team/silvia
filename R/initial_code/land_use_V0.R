library(data.table)
library(sf)
library(here)
library(readxl)
library(dplyr)
library(ggplot2)
source(here("plots", "colors.R"))

# Load IGN cities geometries
path <- "C:/Users/pouchaif/Documents/dev/mobility_arep/data/ign/admin_express/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19/ADMIN-EXPRESS-COG_3-0__SHP__FRA_2021-05-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2021-05-19/ADECOG_3-0_SHP_LAMB93_FR/COMMUNE.shp"
cities <- st_read(path, quiet = TRUE)

# Add cities categories
path <- "D:/data/insee/UU2020_au_01-01-2021.xlsx"
city_category <- as.data.table(read_excel(path, sheet = "Composition_communale", skip = 5))
city_category <- city_category[, list(CODGEO, STATUT_2017)]
setnames(city_category, c("INSEE_COM", "city_category"))
city_category[city_category == "H", city_category := "R"]

cities <- merge(cities, city_category, by = "INSEE_COM")

# Filter cities
selected_cities <- cities[cities$SIREN_EPCI == 200066793, ]

region <- st_union(selected_cities)
region <- st_transform(region, 3035)

st_write(region, here("data", "arep", "region.gpkg"), delete_dsn = TRUE)

st_bbox(region)

# Ogr2Pgr commands to crop CLC data
# "C:\Program Files\QGIS 3.16\bin\ogr2ogr" -spat 4000600 2521000 4038100 2558300  -clipsrc region.gpkg clc_1990.gpkg "C:\Users\pouchaif\Documents\dev\luxembourg-in-transition\data\land.copernicus.eu\U2000_CLC1990_V2020_20u1.gpkg"
# "C:\Program Files\QGIS 3.16\bin\ogr2ogr" -spat 4000600 2521000 4038100 2558300  -clipsrc region.gpkg clc_2000.gpkg "C:\Users\pouchaif\Documents\dev\luxembourg-in-transition\data\land.copernicus.eu\U2006_CLC2000_V2020_20u1.gpkg"
# "C:\Program Files\QGIS 3.16\bin\ogr2ogr" -spat 4000600 2521000 4038100 2558300  -clipsrc region.gpkg clc_2018.gpkg "C:\Users\pouchaif\Documents\dev\luxembourg-in-transition\data\land.copernicus.eu\U2018_CLC2018_V2020_20u1.gpkg"


clc_1990 <- st_read(here("data", "arep", "clc_1990.gpkg"))
clc_2000 <- st_read(here("data", "arep", "clc_2000.gpkg"))
clc_2018 <- st_read(here("data", "arep", "clc_2018.gpkg"))

clc_1990$year <- 1990
clc_2000$year <- 2000
clc_2018$year <- 2018

colnames(clc_1990) <- c("code", "id", "remark", "Area_Ha", "Shape", "year")
colnames(clc_2000) <- c("code", "id", "remark", "Area_Ha", "Shape", "year")
colnames(clc_2018) <- c("code", "id", "remark", "Area_Ha", "Shape", "year")

clc <- rbind(clc_1990, clc_2018)
clc <- st_transform(clc, 2154)

clc$code_0 <- substr(as.character(clc$code), 1, 1)
clc$code_0 <- ifelse(clc$code_0 == "4", "5", clc$code_0)

clc$code_label <- factor(
  clc$code_0,
  c("1", "2", "3", "5"),
  c("Surfaces artificialisées", "Surfaces agricoles", "Surfaces de forêts et semi-naturelles", "Eaux et marais")
)

clc_0 <- clc %>%
  group_by(code_0, code_label, year) %>% 
  summarize(geometry = st_union(Shape))


levels <- rev(c(
  c("Surfaces artificialisées", "Surfaces agricoles", "Surfaces de forêts et semi-naturelles", "Eaux et marais")
))
plot_colors <- rev(c(
  colors[["population"]][["0"]],
  colors[["agriculture"]][["3"]],
  colors[["forest"]][["2"]],
  colors[["equipments"]][["2"]]
))
names(plot_colors) <- levels

p <- ggplot(clc_0)
p <- p + geom_sf(aes(fill = code_label), color = NA)
p <- p + scale_fill_manual(values = plot_colors)
p <- p + facet_wrap(~year)
p <- p + labs(
  title = "Evolution de l'occupation des sols du Grand Annecy",
  subtitle = "Entre 1990 et 2018, données Corine Land Cover.\n"
)
p <- p + theme_void()
p <- p + theme(
  legend.position = "top",
  legend.justification = "left",
  panel.border = element_blank(),
  text = element_text(family = "Avenir"),
  plot.title = element_text(face = "bold", size = 14),
  plot.title.position = "plot",
  plot.subtitle = element_text(face = "italic", size = 12),
  legend.title = element_blank(),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
  strip.text = element_text(size = 14),
  legend.margin = margin(0, 0, 0.5, 0, "cm")
)
p


ggsave(plot = p, filename = here("img", "Grand Annecy - Evolution de l'occupation des sols entre 1990 et 2018.svg"), width = 10, height = 7)



clc <- rbind(clc_1990, clc_2000, clc_2018)
clc <- st_transform(clc, 2154)

clc$area <- as.numeric(st_area(clc))

clc <- as.data.table(clc)


clc[, code_0 := substr(code, 1, 1)]
clc[, code_0 := ifelse(code_0 == "4", "5", code_0)]

clc_total <- clc[, list(area = sum(area)), by = list(year, code_0)]

clc_total[, p := area/sum(area), by = list(year)]


p <- ggplot(clc_total)
p <- p + geom_area(aes(x = year, y = p, fill = code_0), alpha = 0.15)
p <- p + geom_col(aes(x = year, y = p, fill = code_0), width = 1)
p


pop_artif <- clc_total[code_0 == "1"]
pop_artif[year == 1990, population := 151545]
pop_artif[year == 2000, population := 166833 + (180381-166833)/(2008-1999)]
pop_artif[year == 2018, population := 203784]


p <- ggplot(pop_artif)
p <- p + geom_line(aes(x = population, y = area))
p

pop_artif[, area_per_pers := area/population]
pop_artif

pop_artif[, delta_pop := population - shift(population, 1)]
pop_artif[, delta_area := area - shift(area, 1)]

pop_artif[, land_use_per_new_pers := delta_area/delta_pop]

pop_artif

85000*220/1e6
