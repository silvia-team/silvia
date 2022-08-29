library(data.table)
library(sf)
library(here)
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# source(here("plots", "colors.R"))

# ------------------ Useful functions --------------------- #

f1 <- function(num) {
  format(num, big.mark = ' ')
}
# --------------------------------------------------------- #

# Load IGN cities geometries
path <- "C:/Users/bohnenkl/Documents/GitHub/silvia/data/ign/ADMIN-EXPRESS_3-1__SHP__FRA_L93_2022-06-21/ADMIN-EXPRESS_3-1__SHP__FRA_L93_2022-06-21/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2022-06-21/ADE_3-1_SHP_LAMB93_FR/COMMUNE.shp"
cities <- st_read(path, quiet = TRUE)

# Add cities categories
path <- "C:/Users/bohnenkl/Documents/GitHub/silvia/data/insee/UU2020_au_01-01-2022.xlsx"
city_category <- as.data.table(read_excel(path, sheet = "Composition_communale", skip = 5))
city_category <- city_category[, list(CODGEO, STATUT_2017)]
setnames(city_category, c("INSEE_COM", "city_category"))
city_category[city_category == "H", city_category := "R"]  #Replace H by R ?

cities <- merge(cities, city_category, by = "INSEE_COM")

# Filter cities
epci <- 200066793 #Annecy
# epci <- 246900740 #COPAMO
# epci <- 200054781 # Paris
# epci <-  242010056 #Adjaccio
# epci <-  200069656 # bcp forets

retrieve_epci_name <- function(path_to_aldo, epci) {

  dt <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  dt <- as.data.table(dt)
  dt <- dt[, c(2, 5)]

  colnames(dt) <- c('EPCI_Siren', 'EPCI_name')

  epci_name <- dt$EPCI_name[dt$EPCI_Siren == epci]

  return(epci_name)
}

path_to_aldo <- "../../ALDO/Outil ALDO_2021_12.xlsx"
epci_name <- retrieve_epci_name(path_to_aldo, epci)


selected_cities <- cities[cities$SIREN_EPCI == epci, ]

region <- st_union(selected_cities) # keep only the region border
region <- st_transform(region, 2154) # projected coordinate system for France
# region <- st_transform(region, 3035) # projected coordinate system for Europe


st_write(region, here("data", "arep", "region.gpkg"), delete_dsn = TRUE)

st_bbox(region)

# Ogr2Pgr commands to crop CLC data
# "C:\Program Files\QGIS 3.16\bin\ogr2ogr" -spat 4000600 2521000 4038100 2558300  -clipsrc region.gpkg clc_1990.gpkg "C:\Users\pouchaif\Documents\dev\luxembourg-in-transition\data\land.copernicus.eu\U2000_CLC1990_V2020_20u1.gpkg"
# "C:\Program Files\QGIS 3.16\bin\ogr2ogr" -spat 4000600 2521000 4038100 2558300  -clipsrc region.gpkg clc_2000.gpkg "C:\Users\pouchaif\Documents\dev\luxembourg-in-transition\data\land.copernicus.eu\U2006_CLC2000_V2020_20u1.gpkg"
# "C:\Program Files\QGIS 3.16\bin\ogr2ogr" -spat 4000600 2521000 4038100 2558300  -clipsrc region.gpkg clc_2018.gpkg "C:\Users\pouchaif\Documents\dev\luxembourg-in-transition\data\land.copernicus.eu\U2018_CLC2018_V2020_20u1.gpkg"

# "C:\\Program Files\\QGIS 3.22.7\\bin\\ogr2ogr" -spat 929677.4 6519479.1  959255.3 6557345.5  -clipsrc "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_2018.gpkg" "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG\\CORINE_LAND_COVER_FRANCE_METROPOLITAINE_EPSG2154.gpkg"

# perimeter <- paste(as.numeric(st_bbox(region)), collapse = " ")
# qg <- ' "'
# qd <- '" '
# command1 <- "C:\\Program Files\\QGIS 3.22.7\\bin\\ogr2ogr"
# command2 <- "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\region.gpkg"
# command3 <- "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\arep\\clc_2018.gpkg"
# command4 <- "C:\\Users\\bohnenkl\\Documents\\GitHub\\silvia\\data\\copernicus\\CLC_PNE_RG\\CORINE_LAND_COVER_FRANCE_METROPOLITAINE_EPSG2154.gpkg"
#
# command <- paste(qg, command1,qd,  "-spat ", perimeter, "-clipsrc ",
#                  qg, command2, qd, qg, command3,qd, qg,command4, qd, sep = "")

# clc_1990 <- st_read(here("data", "arep", "clc_1990.gpkg"))
# clc_2000 <- st_read(here("data", "arep", "clc_2000.gpkg"))

clc_2018 <- st_read(here("data", "arep", "clc_2018.gpkg"))

# clc_2018 <- st_read(here("data", "copernicus",
#                          "CLC_PNE_RG", "CORINE_LAND_COVER_FRANCE_METROPOLITAINE_EPSG2154.gpkg"))

# clc_1990$year <- 1990
# clc_2000$year <- 2000
clc_2018$year <- 2018

# clc_2018 <- clc_2018[, -1]

# colnames(clc_1990) <- c("code", "id", "remark", "Area_Ha", "geom", "year")
# colnames(clc_2000) <- c("code", "id", "remark", "Area_Ha", "geom", "year")
colnames(clc_2018) <- c("objectif", "code", "id", "remark", "Area_Ha", "geom", "year")

# clc <- rbind(clc_1990, clc_2018)
clc <- rbind(clc_2018)
# clc <- st_transform(clc, 2154)

clc <- clc_2018





clc$code_0 <- substr(as.character(clc$code), 1, 1)
clc$code_0 <- ifelse(clc$code_0 == "4", "5", clc$code_0) # gather "Eaux et marais"

clc$code_label <- factor(
  clc$code_0,
  c("1", "2", "3", "5"),
  c("Surfaces artificialisées", "Surfaces agricoles", "Surfaces de forêts et semi-naturelles", "Eaux et marais")
)

clc_0 <- clc %>%
  group_by(code_0, code_label, year) %>%
  summarize(geometry = st_union(geom))


levels <- rev(c(
  c("Surfaces artificialisées", "Surfaces agricoles", "Surfaces de forêts et semi-naturelles", "Eaux et marais")
))

# plot_colors <- rev(c(
#   colors[["population"]][["0"]],
#   colors[["agriculture"]][["3"]],
#   colors[["forest"]][["2"]],
#   colors[["equipments"]][["2"]]
# ))
# names(plot_colors) <- levels

p <- ggplot(clc_0)
p <- p + geom_sf(aes(fill = code_label), color = NA)
#p <- p + scale_fill_manual(values = plot_colors)
p <- p + facet_wrap(~year)
p <- p + labs(
  title = paste("Evolution de l'occupation des sols - ", epci_name),
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


#ggsave(plot = p, filename = here("img", "Grand Annecy - Evolution de l'occupation des sols entre 1990 et 2018.svg"), width = 10, height = 7)



# clc <- rbind(clc_1990, clc_2000, clc_2018)
# clc <- st_transform(clc, 2154)
#
# clc$area <- as.numeric(st_area(clc))
#
# clc <- as.data.table(clc)
#
#
# clc[, code_0 := substr(code, 1, 1)]
# clc[, code_0 := ifelse(code_0 == "4", "5", code_0)]
#
# clc_total <- clc[, list(area = sum(area)), by = list(year, code_0)]
#
# clc_total[, p := area/sum(area), by = list(year)]
#
#
# p <- ggplot(clc_total)
# p <- p + geom_area(aes(x = year, y = p, fill = code_0), alpha = 0.15)
# p <- p + geom_col(aes(x = year, y = p, fill = code_0), width = 1)
# p
#
#
# pop_artif <- clc_total[code_0 == "1"]
# pop_artif[year == 1990, population := 151545]
# pop_artif[year == 2000, population := 166833 + (180381-166833)/(2008-1999)]
# pop_artif[year == 2018, population := 203784]
#
#
# p <- ggplot(pop_artif)
# p <- p + geom_line(aes(x = population, y = area))
# p
#
# pop_artif[, area_per_pers := area/population]
# pop_artif
#
# pop_artif[, delta_pop := population - shift(population, 1)]
# pop_artif[, delta_area := area - shift(area, 1)]
#
# pop_artif[, land_use_per_new_pers := delta_area/delta_pop]
#
# pop_artif
#
# 85000*220/1e6

# ------------------------------------------------------------------------------- #


nomenclature_level <- function(clc, path_to_nomenclature, level) {
  if (level == 1){
    clc_nomenclature <- as.data.table(read_excel(path_to_nomenclature, sheet = "nomenclature_clc_niveau_1"))
    clc_nomenclature <- rename(clc_nomenclature, code = code_clc_niveau_1)
    clc$code <- substr(as.character(clc$code), 1, 1)
  }
  else if (level == 2) {
    clc_nomenclature <- as.data.table(read_excel(path_to_nomenclature, sheet = "nomenclature_clc_niveau_2"))
    clc_nomenclature <- rename(clc_nomenclature, code = code_clc_niveau_2)
    clc$code <- substr(as.character(clc$code), 1, 2)
  }
  else if (level == 3) {
    clc_nomenclature <- as.data.table(read_excel(path_to_nomenclature, sheet = "nomenclature_clc_niveau_3"))
    clc_nomenclature <- rename(clc_nomenclature, code = code_clc_niveau_3)
  }
  colors_clc <-  paste(clc_nomenclature$rouge, clc_nomenclature$vert, clc_nomenclature$bleu)
  color_clc_rgb <-sapply(strsplit(as.character(colors_clc), " "), function(x) {
    rgb(x[1], x[2], x[3], m=255)
  })
  color_clc_rgb <-  as.list(strsplit(color_clc_rgb, " ")[])
  clc_nomenclature$color <- color_clc_rgb


  clc_1 <- merge(clc, clc_nomenclature, by= "code")
  clc_1 <- clc_1 %>%
    group_by(code, libelle_fr, year, color) %>%
    summarize(geometry = st_union(geometry))

  return(clc_1)
  }


path_to_nomenclature <-  "C:/Users/bohnenkl/Documents/GitHub/silvia/data/clc-nomenclature-c_1.xls"


clc_1 <- nomenclature_level(clc, path_to_nomenclature, 3)


p <- ggplot(clc_1)
p <- p + geom_sf(aes(fill = libelle_fr), color = NA)
p <- p + scale_fill_manual(values = clc_1$color,
                           breaks = clc_1$libelle_fr)
# p <- p + facet_wrap(~year)
p <- p + labs(
  title = paste("Occupation des sols - ",epci_name),
  subtitle = "Données Corine Land Cover.\n"
)
p <- p + theme_void()
p <- p + theme(
  legend.position = "right",
  legend.justification = "left",
  panel.border = element_blank(),
  plot.title = element_text(face = "bold", size = 14),
  plot.title.position = "plot",
  plot.subtitle = element_text(face = "italic", size = 12),
  legend.title = element_blank(),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
  strip.text = element_text(size = 14),
  legend.margin = margin(0, 0, 0.5, 0, "cm")
)
p

# ------------- Retrieves soils carbon contents --------------------- #
path_to_aldo_clc <- "../../ALDO/aldo_clc.xlsx"
dt_soils <- aldo_clc_category(path_to_aldo_clc, "soils")

dt_soils_epci <- merge(dt_occ, dt_soils)
dt_soils_epci <- na.omit(dt_soils_epci)

dt_soils_content <- read.csv("data/carbon_content_soil.csv")
dt_soils_stocks <- merge(dt_soils_epci, dt_soils_content,
                         by = c("EPCI_Siren", "aldo_soil_category"))
dt_soils_stocks <- dt_soils_stocks %>% filter(EPCI_Siren == epci)

dt_soils_stocks_zone <- merge(x=clc, y=dt_soils_stocks,
                              by.x ="code", by.y = "clc_category",
                              all= TRUE)

dt_soils_stocks_zone$soil_carbon_content[is.na(dt_soils_stocks_zone$soil_carbon_content)] <- 0


# Add litters
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
dt_soils_stocks_zone$soil_carbon_content[dt_soils_stocks_zone$code %in% c(311,312,313,324)] %+=% 9


# Set to 0 carbon stocks in water surfaces
# dt_soils_stocks_zone$soil_carbon_content[dt_soils_stocks_zone$code %in% c(511,512,521,523)] <- 0
dt_soils_stocks_zone <- dt_soils_stocks_zone %>% tidyr::drop_na(objectif)
dt_soils_stocks_zone$soil_carbon_content <- round(dt_soils_stocks_zone$soil_carbon_content)

total_soils_area_stocks <- round(sum(st_area(dt_soils_stocks_zone) * 1e-04 * dt_soils_stocks_zone$soil_carbon_content))
total_soils_area_stocks <- as.numeric(total_soils_area_stocks)

test1 <- dt_soils_stocks_zone %>%
  group_by(code) %>%
  summarise(sum_area = sum(as.numeric(st_area(geometry)* 1e-04)),
            sum_stocks = sum(as.numeric(st_area(geometry) * 1e-04 * dt_soils_stocks_zone$soil_carbon_content)))

test2 <- dt_soils_stocks_zone %>%
  group_by(aldo_soil_category, soil_carbon_content) %>%
  summarise(sum_area = sum(as.numeric(st_area(geometry)* 1e-04)))
  # summarize(sum_stocks = sum_area * soil_carbon_content)

p <- ggplot(dt_soils_stocks_zone)
p <- p + geom_sf(aes(fill = soil_carbon_content), color = NA)
# p <- p + facet_wrap(~year)
p <- p + labs(
  title = paste("Stocks de carbone dans les sols et dans les litières - ", epci_name, "\n"),
  subtitle = bquote('Total des stocks : ' ~ .(f1(total_soils_area_stocks)) ~ tC ~ ' = '
                    ~  .(f1(round(total_soils_area_stocks*44/12))) ~ tCO['2, eq'] ~ '\n'),
  caption = "Données Corine Land Cover et ALDO (GIS)\n",
  fill = "Intensité du stockage de\n carbone dans le sol + litière (tC/ha)"
)
p <- p + theme_void()
p <- p + theme(
  legend.position = "right",
  legend.justification = "left",
  panel.border = element_blank(),
  plot.title = element_text(face = "bold", size = 14),
  plot.title.position = "plot",
  plot.subtitle = element_text(face = "italic", size = 12),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
  strip.text = element_text(size = 14),
  legend.margin = margin(0, 0, 0.5, 0, "cm")
)
# p <- p +scale_fill_brewer(palette = "Oranges")
p <- p + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges"))
p

# ------------- Retrieves biomass carbon stocks in forests --------------------- #

path_to_aldo_clc <- "../../ALDO/aldo_clc.xlsx"
dt_biomass <- aldo_clc_category(path_to_aldo_clc, "biomass")
dt_biomass_forests <- read.csv("data/biomass_forests.csv")
dt_biomass_forests <- dt_biomass_forests %>% filter(EPCI_Siren == epci)
dt_biomass_forests_epci <- merge(dt_biomass_forests, dt_biomass, by= "aldo_biomass_category")

dt_biomass_stocks_zone <- merge(x=clc, y=dt_biomass_forests_epci,
                                by.x ="code", by.y = "clc_category",
                                all= TRUE)

dt_biomass_stocks_zone$biomass_carbon_content[is.na(dt_biomass_stocks_zone$biomass_carbon_content)] <- 0


# ------------- Retrieves biomass carbon stocks out of forests --------------------- #

dt_biomass_wo_forests <- read.csv("data/biomass_wo_forests.csv")
dt_biomass_wo_forests <- dt_biomass_wo_forests %>% filter(EPCI_Siren == epci)
dt_biomass_wo_forests_epci <- merge(dt_biomass_wo_forests, dt_biomass, by= "aldo_biomass_category")

dt_biomass_wo_forests_stocks_zone <- merge(x=clc, y=dt_biomass_wo_forests_epci, by.x ="code", by.y = "clc_category")

# ------------- Retrieves biomass carbon stocks everywhere --------------------- #

biomass_carbon_stocks <- plyr::rbind.fill(dt_biomass_stocks_zone, dt_biomass_wo_forests_stocks_zone)

biomass_carbon_stocks <- biomass_carbon_stocks %>% tidyr::drop_na(objectif)
biomass_carbon_stocks$biomass_carbon_content <- round(biomass_carbon_stocks$biomass_carbon_content)

total_biomass_area_stocks <- round(sum(st_area(biomass_carbon_stocks$geometry) * 1e-04 * biomass_carbon_stocks$biomass_carbon_content))
total_biomass_area_stocks <- as.numeric(total_biomass_area_stocks)

p <- ggplot(biomass_carbon_stocks)
p <- p + geom_sf(aes(fill = biomass_carbon_content, geometry = geometry), color = NA)
# p <- p + facet_wrap(~year)
p <- p + labs(
  title = paste("Stocks de carbone dans les sols et dans les litières - ", epci_name, "\n"),
  subtitle = bquote('Total des stocks : ' ~ .(f1(total_biomass_area_stocks)) ~ tC ~ ' = '
                    ~  .(f1(round(total_biomass_area_stocks*44/12))) ~ tCO['2, eq'] ~ '\n'),
  caption = "Données Corine Land Cover et ALDO (GIS)\n",
  fill = "Intensité du stockage de\n carbone dans le sol + litière (tC/ha)"
)
p <- p + theme_void()
p <- p + theme(
  legend.position = "right",
  legend.justification = "left",
  panel.border = element_blank(),
  plot.title = element_text(face = "bold", size = 14),
  plot.title.position = "plot",
  plot.subtitle = element_text(face = "italic", size = 12),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
  strip.text = element_text(size = 14),
  legend.margin = margin(0, 0, 0.5, 0, "cm")
)
# p <- p +scale_fill_brewer(palette = "Oranges")
p <- p + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Greens"))
p

# ------------- Retrieves carbon stocks everywhere --------------------- #

total_carbon_stocks <- merge(dt_soils_stocks, dt_biomass_wo_forests_epci,
                             by = c("EPCI_Siren", "clc_category"), all= T)
total_carbon_stocks <- merge(total_carbon_stocks, dt_biomass_forests_epci,
                             by = c("EPCI_Siren", "clc_category"), all = T)

total_carbon_stocks <- total_carbon_stocks[!is.na(clc_category)]

total_carbon_stocks <- total_carbon_stocks %>%
  group_by(clc_category) %>%
  summarise(clc_category,
            total_stocks = sum(soil_carbon_content,
                               biomass_carbon_content.x,
                               biomass_carbon_content.y,
                               na.rm=TRUE)) %>%
  summarise(clc_category,
            total_stocks= round(total_stocks))


total_carbon_stocks_zone <- merge(x=clc, y=total_carbon_stocks,
                                  by.x ="code", by.y = "clc_category",
                                  all= TRUE)


total_carbon_stocks_zone <- total_carbon_stocks_zone %>% tidyr::drop_na(objectif)
total_carbon_stocks_zone$total_stocks[is.na(total_carbon_stocks_zone$total_stocks)] <- 0

# Add litters
total_carbon_stocks_zone$total_stocks[total_carbon_stocks_zone$code %in% c(311,312,313,324)] %+=% 9

# Set to 0 carbon stocks in water surfaces
# total_carbon_stocks_zone$total_stocks[total_carbon_stocks_zone$code %in% c(511,512,521,523)] <- 0

total_area_stocks <- round(sum(st_area(total_carbon_stocks_zone) * 1e-04 * total_carbon_stocks_zone$total_stocks))
total_area_stocks <- as.numeric(total_area_stocks)
epci_wood_stocks <- dt_harvested_wood$wood_carbon_stocks[dt_harvested_wood$EPCI_Siren == epci]*12/44

total_area_stocks <-  total_area_stocks + epci_wood_stocks

p <- ggplot(total_carbon_stocks_zone)
p <- p + geom_sf(aes(fill = total_stocks, geometry = geometry), color = NA)
# p <- p + facet_wrap(~year)
p <- p + labs(
  title = paste("Stocks de carbone (Sols + Biomasse + Litière) - ", epci_name, '\n'),
  subtitle = bquote(atop('Total des stocks : ' ~ .(f1(total_area_stocks)) ~ tC ~ ' = '
                    ~  .(f1(round(total_area_stocks*44/12))) ~ tCO['2, eq'] ~ '\n',
                    "dont produits bois : "~ .(f1(epci_wood_stocks)) ~ tC ~ ' = '
                    ~  .(f1(epci_wood_stocks*44/12)) ~ tCO['2, eq'] ~ '\n')),

  caption = "Données Corine Land Cover et ALDO (GIS)\n",
  fill = "Intensité du stockage\n de carbone (tC/ha)"
)
p <- p + theme_void()
p <- p + theme(
  legend.position = "right",
  legend.justification = "left",
  panel.border = element_blank(),
  plot.title = element_text(face = "bold", size = 14),
  plot.title.position = "plot",
  plot.subtitle = element_text(face = "italic", size = 12),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
  strip.text = element_text(size = 14),
  legend.margin = margin(0, 0, 0.5, 0, "cm")
)
# p <- p +scale_fill_manual(values = as.list(strsplit(colorRampPalette(brewer.pal(9, "Blues"))(colourCount), " ")[]))
p <- p + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Greys"))
p

# ------------- Retrieves carbon stocks in harvested wood --------------------- #

