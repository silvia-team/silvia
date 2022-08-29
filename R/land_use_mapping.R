##############################
# Load libraries

library(ggplot2)
library(here)
library(treemapify)
source(here("R", "stocks.R"))

##############################

# --------------------------------------------------------- #

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
    group_by(code, libelle_fr, color) %>%
    summarize(geometry = st_union(geometry))

  return(clc_1)
}

plot_land_use <- function(dt, col_code_name, epci_name, nomenclature_level) {


  path_to_nomenclature <-  here("data", "clc-nomenclature-c_1.xls")
  land_use <- nomenclature_level(dt, path_to_nomenclature, nomenclature_level)


  p <- ggplot(land_use)
  p <- p + geom_sf(aes(fill = libelle_fr), color = NA)
  p <- p + scale_fill_manual(values = land_use$color,
                             breaks = land_use$libelle_fr)
  # p <- p + facet_wrap(~year)
  p <- p + labs(
    title = paste("Occupation des sols - ",epci_name),
    caption = "Données Corine Land Cover et ALDO (GIS)\n"
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

  return(p)

}

land_use_treemap <- function(dt, col_code_name, epci_name) {

  path_to_nomenclature <-  here("data", "clc-nomenclature-c_1.xls")
  dt <- nomenclature_level(dt, path_to_nomenclature, 1)
  total_area <- as.numeric(sum(st_area(dt) * 1e-04))
  dt$area_percent <- 100*as.numeric(st_area(dt)*1e-04)/total_area
  dt <- st_drop_geometry(dt)

  dt <- as.data.table(dt)


  dt[, libelle_fr := ifelse(libelle_fr == "Territoires agricoles", "Agriculture", libelle_fr)]
  dt[, libelle_fr := ifelse(libelle_fr == "Forêts et milieux semi-naturels", "Forêts", libelle_fr)]
  dt[, libelle_fr := ifelse(libelle_fr == "Territoires artificialisés", "Espaces bâtis", libelle_fr)]
  dt[, libelle_fr := ifelse(libelle_fr == "Surfaces en eau", "Eau et zones humides", libelle_fr)]

  area_zh <- dt$area_percent[dt$libelle_fr == "Zones humides"]
  dt[, area_percent := ifelse(libelle_fr == "Surfaces en eau", area_percent + area_zh, area_percent)]
  dt <- dt[dt$libelle_fr != 'Zones humides']

  dt <- dt %>%
    mutate(legend=paste(libelle_fr, paste(round(area_percent), '%', sep = ""), sep ="\n"))

  dt <- dt[order(area_percent)]

  p <- ggplot(dt, aes(area = area_percent, fill = libelle_fr))
  p <- p + geom_treemap()
  p <- p + geom_treemap_text(colour = "white",
                             grow = F, aes(label = legend), show.legend = FALSE)
  p <- p + scale_fill_manual(values = c("#e8c961", "#8dd6d4", "#e35656", "#3b784b"))
  p <- p + theme(legend.position = "none")

  return(p)

}



dt <- st_read(here("data", "arep", "clc_2018.gpkg"))
colnames(dt) <- c("objectif", "code", "id", "remark", "Area_Ha", "geom")
#
p1 <- land_use_treemap(dt, col_code_name, epci_name)
ggsave(plot = p1, width = 5, height = 5, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Répartition des sols du Grand Genève en 2018.svg"))


total_area <- round(sum(st_area(dt) * 1e-06))

#
p2 <- plot_land_use(dt, "code", "Grand Genève - PACA 3", 2)
ggsave(plot = p2, width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Carte de l'occupation des sols du Grand Genève en 2018.svg"))
p2
