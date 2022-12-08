
#### Map carbon storage ####


#' Map the carbon storage on the chosen region.
#' @param dt
#' @return a ggplot with the map
#' @export
#' @importFrom sf st_read st_write st_area st_geometry
#' @importFrom here here
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn scale_color_brewer
#'
map_carbon_storage <- function(dt){

  total_area_stocks <- sum(st_area(dt) * 1e-04 * 1e-03 * dt$total_carbon_content)
  total_area_stocks <- as.numeric(total_area_stocks)
  epci_wood_stocks <- retrieve_harvested_wood(dt)
  total_area_stocks <-  round(total_area_stocks + epci_wood_stocks)

  shape <- st_read(here("data", "arep", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(dt))

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
#' @return map of carbon flows
#' @export
#' @importFrom data.table data.table
#' @importFrom sf st_union st_crs
#' @importFrom classInt classIntervals
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin scale_fill_gradient2
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
map_carbon_flows <- function(flows){

  # breaks_qt_pos <- classIntervals(flows$total_flows[flows$total_flows>0], n = 4, style = "quantile")
  # breaks_qt_neg <- classIntervals(flows$total_flows[flows$total_flows<0], n = 5, style = "quantile")
  # breaks_qt_neg$brks[1] <- min(breaks_qt_neg$brks*1.01)
  # breaks_qt <- unique(c(breaks_qt_pos$brks, breaks_qt_neg$brks, 0))
  # flows <- mutate(flows, total_flows_intervals = cut(trunc(total_flows), breaks_qt, right= T))
  shape <- st_read(here("data", "arep", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(flows))
  flows <- st_as_sf(flows)
  flows <- as.data.table(flows)

  p <- ggplot(flows)
  p <- p + geom_sf(aes(fill = total_flows, geometry = geometry), color = NA)

  p <- p + labs(
    # title = paste("Flux de carbone entre", year_from, "et", year_to,  " - ", name_of_the_territory, "\n"),
    # subtitle = bquote(atop('Flux total : '~  .(f1(round(total_area_flows))) ~ ktCO['2, eq']/an ~ '\n',
    #                        atop("Flux biomasse + forêts : "~  .(f1(round((total_biomass_flows + total_forest_flows)))) ~ ktCO['2']/an ~ '\n' ,
    #                             "Flux sol et litière : "~  .(f1(round(total_soil_flows))) ~ ktCO['2']/an ~ '\n'
    #                        ))),
    #
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
  # p <- p + scale_fill_brewer(palette =  "RdYlGn", direction = -1)
  p <- p +scale_fill_gradient2(midpoint = 0, mid="#f2efe6", high="#bd0502", low="#016623")
  # p <- p + scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(9, "RdYlGn")))

  p <- p + geom_sf(data = st_union(shape), fill = NA)


  return(p)

}


#' Plot carbon flows in forest of a chosen region.
#' @return map of carbon flows by the forest
#' @export
#' @importFrom data.table data.table
#' @importFrom tibble rowid_to_column
#' @importFrom sf st_union st_crs
#' @importFrom classInt classIntervals
#' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin scale_fill_gradient2
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
map_forest_flows <- function(){

  forest_geom <- st_read(here("data", "arep", "zone_bd_foret.gpkg"))
  forest_geom <- tibble::rowid_to_column(forest_geom, "ID_unique")
  forest_geom <- st_transform(forest_geom, 3035)
  forest <- st_drop_geometry(forest_geom)
  forest <- as.data.table(forest)

  shape <- st_read(here("data", "arep", "territory.gpkg"))
  shape <- st_transform(shape, st_crs(forest_geom))
  epcis <- unique(shape$SIREN_EPCI)

  forest[, essence := ifelse(like(tfv_g11, "conifères"), "conifere", NA)]
  forest[, essence := ifelse(like(tfv_g11, "feuillus"), "feuillu", essence)]
  forest[, essence := ifelse(like(tfv_g11, "mixte"), "mixte", essence)]
  forest[, essence := ifelse(like(tfv_g11, "Peupleraie"), "peupleraie", essence)]
  # forest <- forest[!is.na(essence), ]

  #Retrieve forest flows
  dt1 <- as.data.table(read_excel(path_to_aldo, sheet = "Ref_Biom_foret"))
  dt1 <- update_epcis(dt1, "SIREN_EPCI")
  dt1 <- dt1[SIREN_EPCI %in% epcis, ]
  dt1 <- dt1[, c(1, 3, 12)]
  dt1[is.na(dt1), ] <- 0

  dt2 <- as.data.table(read_excel(path_to_aldo, sheet = "Ref_Biom_Peup"))
  dt2 <- update_epcis(dt2, "SIREN_EPCI")
  dt2 <- dt2[SIREN_EPCI %in% epcis, ]
  dt2 <- dt2[, c(1, 12)]
  dt2[is.na(dt2), ] <- 0
  dt2$COMPOSITION <- "Peupleraie"

  dt <- rbind(dt1, dt2)
  # dt <- update_epcis(dt, "SIREN_EPCI")
  dt$SIREN_EPCI <- as.character(dt$SIREN_EPCI)
  setnames(dt, "BILAN_CARB (tC∙ha-1∙an-1)", "flow")
  dt <- dt %>% dplyr::mutate(COMPOSITION = tolower(COMPOSITION))
  dt <- dt[dt$COMPOSITION != 'total']
  dt <- dt[, list(flow= mean(flow)), by= c("SIREN_EPCI", "COMPOSITION")]
  forest_flows <- merge(forest, dt, by.x = c("SIREN_EPCI", "essence"), by.y = c("SIREN_EPCI", "COMPOSITION"),
                        all.x= T, allow.cartesian = T)
  forest_flows[, total_flows := ifelse(is.na(essence), 0, -flow*44/12)]

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
#' @param year_from
#' @param year_to
#' @return The sankey diagram
#' @export
#' @importFrom data.table setnames
#' @importFrom here here
#' @importFrom sf st_read st_write st_drop_geometry
#' @importFrom ggsankey make_long theme_sankey geom_sankey_label geom_sankey
#' @importFrom ggplot2 scale_colour_viridis_d  scale_fill_manual
#' #' @importFrom ggplot2 ggplot geom_sf labs scale_colour_gradient theme aes theme_void margin
#' @importFrom ggplot2 element_blank element_text scale_colour_gradient scale_fill_gradientn unit scale_fill_brewer
#' @importFrom dplyr rename_at select mutate filter summarise group_by
plot_land_use_changes <- function(year_from, year_to, level= "soft"){

  land_use_changes <- silvia::get_land_use_changes(year_from, year_to)
  land_use_changes <- st_drop_geometry(land_use_changes)

  year_from <- as.character(year_from)
  year_to <- as.character(year_to)
  land_use_changes <- as.data.table(land_use_changes)

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
