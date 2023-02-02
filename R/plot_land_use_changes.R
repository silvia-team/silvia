#' Plot land use changes
#'
#' @description
#' Plot land use changes between two year.
#' Results are presented in a sankey diagram, with the reference year on the left,
#' and the year to be compared to on the right.
#' Note that only soils that have changed occupation between the two years are considered.
#'
#' @param year_from the CLC reference year (1990, 2000, 2006, 2012, 2018)
#' @param year_to the year to be compared to the reference year,
#' must be superior to reference year
#' @param data_path path to where the data is stored
#'
#' @return The sankey diagram
#'
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
#' @examples
#' # Plot land use changes between 1990 and 2018
#'
#' plot_land_use_changes(year_from = 1990, year_to = 2018, data_path= data_path)
#'
plot_land_use_changes <- function(year_from, year_to, data_path){

  land_use_changes <- get_land_use_changes(year_from, year_to, data_path= data_path)
  land_use_changes <- st_drop_geometry(land_use_changes)
  land_use_changes <- as.data.table(land_use_changes)
  land_use_changes <- land_use_changes[code_initial != code_final, ]

  year_from <- as.character(year_from)
  year_to <- as.character(year_to)


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


  colors <- c("#D71B1B", "#FDF392","#5A7C6E", "#50808F", "#1B515D")
  names(colors) <- c("Surfaces artificialisées", "Zones agricoles", "Forêts et zones semi-naturelles",
                     "Zones humides", "Plans d'eau")



  p <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                      fill = factor(node), label = paste0(node, " : ",  n,  " ", "ha"), value = value))
  p <- p + geom_sankey(flow.alpha = 0.35, width = 0.05,  node.color = "black")
  p <- p + geom_sankey_label(size = 3.5, color = "black", fill= "white", hjust = 0.5)
  p <- p + theme_sankey(base_size = 20)
  p <- p + theme(axis.title.x = element_blank())
  p <- p + theme(legend.position = "none")
  # p <- p + labs(
  #   title = title,
  #   subtitle = "Données Corine Land Cover\n"
  # )
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


