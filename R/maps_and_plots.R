
#### Map carbon storage ####


#' Map the carbon storage on the chosen region.
#' @param dt data table returned by `get_carbon_stocks`
#' @param data_path path to where the data is stored
#'
#' @return a ggplot with the map
#' @export
#' @importFrom sf st_read st_write st_area st_geometry
#' @importFrom here here
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn scale_color_brewer
#'
#' @export
#'
map_carbon_storage <- function(dt, data_path){

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(dt))
  shape <- shape %>% summarise(geom= st_union(geom))
  shape <- nngeo::st_remove_holes(shape)

  p <- ggplot(dt)
  p <- p + geom_sf(aes(fill = total_carbon_content), color = NA)
  p <- p + labs(
    caption = "Données Corine Land Cover et ALDO (GIS)\n",
    fill = "Intensité du stockage\n de carbone (tCO2/ha)"
  )
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
  # p <- p + scale_color_brewer(palette  = "YlGn")
  p <- p + scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlOrRd"))
  # p <- p +scale_fill_gradient2(high="#88212C", low="#E04E51")
  p <- p + geom_sf(data = st_union(shape), fill = NA)

  # ggsave(plot = p, width = 10, height = 10, filename = here("imgs", "Genève", "Grand Genève - PACA 3", "Carte des stocks de carbone (total) du Grand Genève - PACA 3 en 2018.svg"))

  return(p)

}



#### Map carbon flows ####


#' Plot carbon flows of a chosen region.
#' @param flows st object returned by 'get_carbon_flows' funciton
#' @param data_path path to where the data is stored
#'
#' @return map of carbon flows
#'
#' @importFrom data.table data.table
#' @importFrom sf st_union st_crs
#' @importFrom classInt classIntervals
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin scale_fill_gradient2
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
#'
#' @export
#'
map_carbon_flows <- function(flows, data_path){

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(flows))
  shape <- shape %>% summarise(geom= st_union(geom))
  shape <- nngeo::st_remove_holes(shape)


  p <- ggplot(flows)
  p <- p + geom_sf(aes(fill = total_flows), colour = NA)

  p <- p + labs(
    caption = "Une valeur négative correspond à une séquestration, positive à une émission vers l'atmosphère \n
    Données : Corine Land Cover et ALDO (GIS-Sol)"
    ,
    fill = "Flux de carbone (tCO2eq/ha.an)"
  )

  p <- p + theme_void()
  p <- p + theme(
    legend.position = "right",
    legend.justification = "left",
    plot.caption = element_text(hjust = 0),
    panel.border = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", size = 12),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
    strip.text = element_text(size = 14),
    legend.margin = margin(0, 0, 0.5, 0, "cm"),
    legend.spacing.y = unit(0.5, 'cm')
  )
  p <- p +scale_fill_gradient2(midpoint = 0, mid="#FFFFFF", high="#bd0502", low="#016623")

  p <- p + geom_sf(data = st_union(shape), fill = NA)


  return(p)

}


#' Plot carbon flows in forest of a chosen region.
#'
#' @param data_path path to where the data is stored
#'
#' @return map of carbon flows by the forest
#'
#' @importFrom data.table as.data.table
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_union st_crs
#' @importFrom classInt classIntervals
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin scale_fill_gradient2
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
#'
#' @export
#'
map_forest_flows <- function(data_path){

  forest_geom <- st_read(here(data_path, "bd_foret", "zone_bd_foret.gpkg"))
  forest_geom <- tibble::rowid_to_column(forest_geom, "ID_unique")
  forest_geom <- st_transform(forest_geom, 3035)
  forest <- st_drop_geometry(forest_geom)
  forest <- as.data.table(forest)

  shape <- st_read(here(data_path, "territory", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(forest_geom))
  epcis <- unique(shape$SIREN_EPCI)

  forest[, essence := ifelse(like(tfv_g11, "conifères"), "conifere", NA)]
  forest[, essence := ifelse(like(tfv_g11, "feuillus"), "feuillu", essence)]
  forest[, essence := ifelse(like(tfv_g11, "mixte"), "mixte", essence)]
  forest[, essence := ifelse(like(tfv_g11, "Peupleraie"), "peupleraies", essence)]
  # forest <- forest[!is.na(essence), ]

  # #Retrieve forest flows
  dt <- invisible(aldo_Ref_Biom_foret_flows)
  dt <- dt[EPCI_Siren %in% epcis, list(EPCI_Siren= as.character(EPCI_Siren), composition, flow)]

  dt <- dt[, list(flow= mean(flow)), by= c("EPCI_Siren", "composition")]
  forest_flows <- merge(forest, dt, by.x = c("SIREN_EPCI", "essence"), by.y = c("EPCI_Siren", "composition"),
                        all.x= T, allow.cartesian = T)
  forest_flows[, total_flows := ifelse(is.na(essence), 0, -flow*44/12)]
  forest_flows[, total_flows:= ifelse(total_flows > 0, -total_flows, total_flows)]

  forest_geom <- forest_geom %>% select(ID_unique)
  forest_flows <- merge(forest_flows, forest_geom, by = "ID_unique")

  p <- ggplot(forest_flows)
  p <- p + geom_sf(aes(fill = total_flows, geometry = geom), color = NA)

  p <- p + labs(
    caption = "Une valeur négative correspond à une séquestration, positive à une émission vers l'atmosphère \n
    Données : Corine Land Cover et BD Forêt V2",
    fill = "Flux de carbone (tCO2eq/ha.an)"
  )

  p <- p + theme_void()
  p <- p + theme(
    legend.position = "right",
    legend.justification = "left",
    plot.caption = element_text(hjust = 0),
    panel.border = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", size = 12),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
    strip.text = element_text(size = 14),
    legend.margin = margin(0, 0, 0.5, 0, "cm"),
    legend.spacing.y = unit(0.5, 'cm')
  )
  p <- p + scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(9, "YlGn")))
  p <- p + geom_sf(data = st_union(shape), fill = NA)

  return(p)
}




#### Plot land use changes on a sankey ####

#' Plot land use changes between two year.
#' Results are presented in a sankey diagram
#' @param year_from the CLC reference year (1990, 2000, 2006, 2012, 2018)
#' @param year_to the year to be compared to the reference year,
#' must be superior to reference year
#' @param data_path path to where the data is stored
#'
#' @return The sankey diagram
#' @export
#' @importFrom data.table setnames as.data.table
#' @importFrom here here
#' @importFrom sf st_read st_write st_drop_geometry
#' @importFrom ggsankey make_long theme_sankey geom_sankey_label geom_sankey
#' @importFrom ggplot2 scale_colour_viridis_d  scale_fill_manual
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
#' @importFrom dplyr rename_at select mutate filter summarise group_by
#'
#' @export
#'
plot_land_use_changes <- function(year_from, year_to, data_path, level= "soft"){

  land_use_changes <- get_land_use_changes(year_from, year_to, data_path= data_path)
  land_use_changes <- st_drop_geometry(land_use_changes)
  land_use_changes <- as.data.table(land_use_changes)
  land_use_changes <- land_use_changes[code_initial != code_final, ]

  year_from <- as.character(year_from)
  year_to <- as.character(year_to)


  if (level =="soil"){
    land_use_changes <- land_use_changes %>%
      group_by(soil_category_initial, soil_category_final) %>%
      summarise(area= round(sum(area))) %>%
      filter(area>=10,
             soil_category_initial != soil_category_final)

    land_use_changes <- as.data.table(land_use_changes)

    setnames(land_use_changes, "soil_category_initial", year_from)
    setnames(land_use_changes, "soil_category_final", year_to)


  }

  else if (level =="soft"){
    land_use_changes <- land_use_changes %>%
      group_by(code_initial_first, code_final_first) %>%
      summarise(area= round(sum(area)))%>%
      filter(area>=10,
             code_initial_first != code_final_first)

    land_use_changes <- as.data.table(land_use_changes)

    node <- c(1, 2, 3, 4, 5)
    node_name <- c("Surfaces artificialisées", "Zones agricoles", "Forêts et zones semi-naturelles",
                   "Zones humides", "Plans d'eau")

    node_names <- data.table(node, node_name)
    land_use_changes <- merge(land_use_changes, node_names, by.x="code_initial_first", by.y ="node")
    setnames(land_use_changes, "node_name", year_from)
    land_use_changes <- merge(land_use_changes, node_names, by.x="code_final_first", by.y ="node")
    setnames(land_use_changes, "node_name", year_to)

  }

  df <- land_use_changes %>%
    ggsankey::make_long(year_from, year_to, value = area)

  df <- as.data.table(df)


  sum_node <- df[!(is.na(next_node)),]%>%
    group_by(node)%>%
    summarise(n_node = round(sum(value)),
              n_node = format(n_node, big.mark = ' '))

  sum_next_node <- df[is.na(next_node),]%>%
    group_by(node)%>%
    summarise(n_next_node = round(sum(value)),
              n_next_node = format(n_next_node, big.mark = ' ')
    )


  df <- merge(df, sum_node, by= c('node'), all.x = TRUE, no.dups = TRUE)
  df <- merge(df, sum_next_node, by= c('node'), all.x = TRUE, no.dups = TRUE)

  df[, n := ifelse(is.na(next_node),n_next_node , n_node)]

  df$node <- gsub("(?<=^|; )([a-z])", "\\U\\1", tolower(df$node), perl = T)
  df$next_node <- gsub("(?<=^|; )([a-z])", "\\U\\1", tolower(df$next_node), perl = T)

  df <- as.data.table(df)

  title <- paste("Changement d'affectation des sols entre", year_from, "et", year_to)

  if (level =="soil"){

    colors <- c("#D71B1B", "#5A7C6E", "#FDF392", "#7B5348", "#000000","#B5BC8F", "#88212C", "#50808F")
    names(colors) <- c("Cultures", "Forêts", "Prairies", "Sols artificiels arborés et buissonants",
                       "Sols artificiels imperméabilisés", "Vergers", "Vignes", "Zones humides")
  }

  else if (level =="soft"){
    colors <- c("#D71B1B", "#FDF392","#5A7C6E", "#50808F", "#1B515D")
    names(colors) <- c("Surfaces artificialisées", "Zones agricoles", "Forêts et zones semi-naturelles",
                       "Zones humides", "Plans d'eau")
  }



  p <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                      fill = factor(node), label = paste0(node, " : ",  n,  " ", "ha"), value = value))
  p <- p + geom_sankey(flow.alpha = 0.35, width = 0.05,  node.color = "black")
  p <- p + geom_sankey_label(size = 3.5, color = "black", fill= "white", hjust = 0.5)
  p <- p + theme_sankey(base_size = 20)
  p <- p + theme(axis.title.x = element_blank())
  p <- p + theme(legend.position = "none")
  p <- p + labs(
    title = title,
    subtitle = "Données Corine Land Cover\n"
  )
  p <- p + theme(axis.title.x = element_blank())
  p <- p + theme(
    legend.position = "none",
    axis.text = element_text(face = "bold", color = "#000000", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", size = 12)
  )
  p <- p + scale_fill_manual(values=colors)


  return(p)

}


