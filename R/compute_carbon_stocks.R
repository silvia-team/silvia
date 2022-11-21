

# ------------------ Useful functions --------------------- #

f1 <- function(num) {
  format(num, big.mark = ' ')
}
# --------------------------------------------------------- #

aldo_clc_category <- function(path_to_aldo_clc, perimeter) {

  dt <- read_excel(path_to_aldo_clc, sheet = "imp")
  dt <- as.data.table(dt)

  if (perimeter =="biomass"){
    dt <- dt[, c(3, 5)]
  }
  else if (perimeter == "soils"){
    dt <- dt[, c(1, 5)]
  }
  else if (perimeter == "short"){
    dt <- dt[, c(2, 5)]
  }


  return(dt)
}





occupation_clc <- function(path_to_aldo){
  dt <- read_excel(path_to_aldo, sheet = "Occ_CLC18")
  dt <- as.data.table(dt)
  dt <- dt[, c(1, 2:45)]

  setnames(dt, "siren", "EPCI_Siren")

  dt <- melt(
    dt,
    id.vars = "EPCI_Siren",
    variable.name = "clc_category",
    value.name = "occupation_surface"
  )

  return(dt)
}

retrieve_clc<- function(year, count= FALSE){

  if (count == T){
    file_name <- paste('zone_', year, ".gpkg", sep = '')
    clc <- st_read(here("data", "arep", file_name))
    artif_codes <- c(111,112,121,122,123,124,131,132,133,142)
    # clc$X_mean[!(clc$code %in% artif_codes)] <- 0
    max_imper <- max(clc$mean)
    clc$X_mean <- clc$mean/max_imper

  }
  else if (count == F) {
    file_name <- paste('zone_', year, ".gpkg", sep = '')
    clc <- st_read(here("data", "arep", file_name))
    clc$X_mean <- 0
  }

  clc$year <- year
  clc <- clc %>%
    select(contains(c("ID", "code", "area", "year", "X_mean", "SIREN_EPCI"))) %>%
    dplyr::rename_at(dplyr::vars(contains("code")), ~c("code"))

  return(clc)
}







retrieve_soil_stocks <- function(clc, path_to_aldo_clc, epci){

  # ------------- Retrieves soils carbon contents --------------------- #
  epcis <- unique(clc$SIREN_EPCI)

  dt_soils <- aldo_clc_category(path_to_aldo_clc, "soils")
  dt_soils_enh <-   as.data.table(read_excel(path_to_aldo_clc, sheet = "enh"))
  dt_soils_enh <- dt_soils_enh %>% select(aldo_soil_category_enh, clc_category)

  dt_soils_content <- read.csv(here("data", "carbon_content_soil.csv"))
  dt_soils_content <- dt_soils_content %>% filter(EPCI_Siren %in% epcis)
  dt_soils_content$EPCI_Siren <- as.character(dt_soils_content$EPCI_Siren)

  dt_soils_stocks <- merge(dt_soils, dt_soils_content,
                           by = "aldo_soil_category",
                           all = TRUE, allow.cartesian=TRUE)
  setnames(dt_soils_stocks, "soil_carbon_content", "soil_carbon_content_art")


  dt_soils_stocks_enh <- merge(dt_soils_enh, dt_soils_content,
                           by.x = "aldo_soil_category_enh",
                           by.y = "aldo_soil_category")
  setnames(dt_soils_stocks_enh, "soil_carbon_content", "soil_carbon_content_enh")
  dt_soils_stocks_enh <- dt_soils_stocks_enh %>%
    select(EPCI_Siren, clc_category, soil_carbon_content_enh)

  dt_soils_stocks <- merge(dt_soils_stocks, dt_soils_stocks_enh,
                           by= c("EPCI_Siren", "clc_category"), all = T)

  dt_soils_stocks <- dt_soils_stocks[!is.na(clc_category)]

  dt_stocks <- merge(x=clc, y=dt_soils_stocks,
                                by.x =c("SIREN_EPCI", "code"), by.y = c("EPCI_Siren", "clc_category"),
                                all= TRUE, allow.cartesian=TRUE)

  dt_stocks <- dt_stocks[!is.na(dt_stocks$ID), ]

  # dt_stocks <- as.data.table(dt_stocks)


  dt_stocks <- dt_stocks %>%
    mutate(soil_carbon_content = X_mean*(soil_carbon_content_art -soil_carbon_content_enh) + soil_carbon_content_enh)
  dt_stocks$soil_carbon_content[is.na(dt_stocks$soil_carbon_content)] <-
    dt_stocks$soil_carbon_content_art[is.na(dt_stocks$soil_carbon_content)]


  # Add litters
  `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
  dt_stocks$soil_carbon_content[dt_stocks$code %in% c(311,312,313,324)] %+=% 9


  dt_stocks$soil_carbon_content[dt_stocks$code %in% c(511,512,521,523)] <- 0
  # dt_stocks <- dt_stocks %>% tidyr::drop_na(code)
  dt_stocks$soil_carbon_content <- round(dt_stocks$soil_carbon_content)

  dt_stocks <- dt_stocks %>%
    select(EPCI_Siren= SIREN_EPCI, ID, code, soil_carbon_content, year, X_mean, geometry)

  return(dt_stocks)
}


retrieve_biomass_stocks_in_forests <- function(dt, path_to_aldo_clc, epci){

  epcis <- unique(clc$SIREN_EPCI)

  dt_biomass <- aldo_clc_category(path_to_aldo_clc, "biomass")
  dt_biomass_forests <- read.csv("data/biomass_forests.csv")
  dt_biomass_forests <- dt_biomass_forests %>% filter(EPCI_Siren %in% epcis)
  dt_biomass_forests_epci <- merge(dt_biomass_forests, dt_biomass, by= "aldo_biomass_category")
  dt_biomass_forests_epci$EPCI_Siren <- as.character(dt_biomass_forests_epci$EPCI_Siren)

  dt <- merge(x=dt, y=dt_biomass_forests_epci,
              by.x =c("EPCI_Siren", "code"), by.y = c("EPCI_Siren", "clc_category"),
              all.x= TRUE)
  dt <- dt %>%
    select(code, EPCI_Siren, ID, soil_carbon_content,
           biomass_carbon_content_in_fo = biomass_carbon_content,
           X_mean, geometry)

  dt$biomass_carbon_content_in_fo[is.na(dt$biomass_carbon_content_in_fo)] <- 0

  return(dt)
}




retrieve_biomass_stocks_out_forests <- function(dt, path_to_aldo_clc, epci){

  epcis <- unique(clc$SIREN_EPCI)

  path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")
  dt_biomass <- aldo_clc_category(path_to_aldo_clc, "biomass")
  dt_biomass_arb <- as.data.table(read_excel(path_to_aldo_clc, sheet = "enh"))
  dt_biomass_arb <- dt_biomass_arb %>% select(aldo_biomass_category_arb, clc_category)

  dt_biomass_wo_forests <- read.csv("data/biomass_wo_forests.csv")
  dt_biomass_wo_forests <- dt_biomass_wo_forests %>% filter(EPCI_Siren %in% epcis)
  dt_biomass_wo_forests_epci <- merge(dt_biomass_wo_forests, dt_biomass, by= "aldo_biomass_category")
  setnames(dt_biomass_wo_forests_epci, "biomass_carbon_content", "biomass_carbon_content_art")


  dt_biomass_arb <- merge(dt_biomass_wo_forests, dt_biomass_arb,
                                          by.x= "aldo_biomass_category",
                                          by.y ="aldo_biomass_category_arb")
  dt_biomass_arb <- dt_biomass_arb %>% select(EPCI_Siren, clc_category, biomass_carbon_content)
  setnames(dt_biomass_arb, "biomass_carbon_content", "biomass_carbon_content_arb")


  dt_merged <- merge(dt_biomass_wo_forests_epci, dt_biomass_arb,
              by= c("EPCI_Siren", "clc_category"), all = T)

  # dt <- as.data.table(dt)
  dt_merged$EPCI_Siren <- as.character(dt_merged$EPCI_Siren)
  dt <- merge(x=dt, y=dt_merged, by.x =c("EPCI_Siren", "code"), by.y = c("EPCI_Siren", "clc_category"), all.x = T)

  # dt <- as.data.table(dt)

  dt <- dt %>%
    mutate(biomass_carbon_content_out_fo = X_mean*(biomass_carbon_content_art -biomass_carbon_content_arb) + biomass_carbon_content_arb)
  dt$biomass_carbon_content_out_fo[is.na(dt$biomass_carbon_content_out_fo)] <-
    dt$biomass_carbon_content_art[is.na(dt$biomass_carbon_content_out_fo)]


  dt <- dt %>%
    select(code, EPCI_Siren, ID, soil_carbon_content,
           biomass_carbon_content_in_fo,
           biomass_carbon_content_out_fo,
           X_mean, geometry
    )

  dt$biomass_carbon_content_out_fo[is.na(dt$biomass_carbon_content_out_fo)] <- 0

  return(dt)
}


retrieve_biomass_stocks_everywhere <- function(dt, path_to_aldo_clc, epci){

  dt <- retrieve_biomass_stocks_in_forests(dt, path_to_aldo_clc, epci)

  dt <- retrieve_biomass_stocks_out_forests(dt, path_to_aldo_clc, epci)

  dt$biomass_carbon_content <- dt$biomass_carbon_content_in_fo + dt$biomass_carbon_content_out_fo
  dt$total_carbon_content <- dt$biomass_carbon_content + dt$soil_carbon_content


  return(dt)
}




retrieve_harvested_wood <- function(epci){

  dt_harvested_wood <- read.csv("data/harvested_wood.csv")
  dt_harvested_wood <- as.data.table(dt_harvested_wood)

  dt_harvested_wood <- melt(
    dt_harvested_wood,
    id.vars = c("EPCI_Siren", "wood_composition"),
    measure.vars = c("BO_harvest", "BI_harvest", "BE_harvest"),
    variable.name = "wood_use",
    value.name = "wood_harvested"
  )

  # Set France Epci code for France to 0
  dt_harvested_wood$EPCI_Siren[is.na(dt_harvested_wood$EPCI_Siren)] <- 0
  france_harvested_wood <-  dt_harvested_wood[dt_harvested_wood$EPCI_Siren == 0]
  france_harvested_wood <- france_harvested_wood %>%
    group_by(wood_use) %>%
    summarise(wood_harvested_france = sum(wood_harvested))

  wood_use <- c("BO_harvest", "BI_harvest")
  france_wood_stocks <- c(177419001, 258680001)
  dt_france_wood_stocks <- data.frame(wood_use, france_wood_stocks)

  france_harvested_wood <- merge(france_harvested_wood, dt_france_wood_stocks, by = "wood_use")
  france_harvested_wood$ratio_stocks_harvest <- france_harvested_wood$france_wood_stocks / france_harvested_wood$wood_harvested_france

  france_harvested_wood <- france_harvested_wood %>% select(wood_use, ratio_stocks_harvest)

  france_harvested_wood <- merge(dt_harvested_wood, france_harvested_wood,
                                  by = "wood_use")

  france_harvested_wood$carbon_stocks <- round(france_harvested_wood$wood_harvested * france_harvested_wood$ratio_stocks_harvest)

  dt_harvested_wood <- france_harvested_wood %>%
    group_by(EPCI_Siren) %>%
    summarise(wood_carbon_stocks = sum(carbon_stocks))

  epci_wood_stocks <- round(dt_harvested_wood$wood_carbon_stocks[dt_harvested_wood$EPCI_Siren == epci]* 1e-03 *12/44)

  return(epci_wood_stocks)

}






#' Retrieve carbon stocks of the chosen region
#' @param year
#' @param count
#' @return a sf object with the carbon stored in every polygon
#' @export
#' @importFrom data.table as.data.table
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom sf st_as_sf
#' @importFrom dplyr rename_at select mutate filter summarise group_by
get_carbon_storage <-  function(year, count = F){

  path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")
  clc <- retrieve_clc(year,count)
  if ("id" %in% names(clc)){
    setnames(clc, "id", "ID")
  }
  load(here("data", "epci.rda"))
  dt <- retrieve_soil_stocks(clc, path_to_aldo_clc, epci)
  dt <- retrieve_biomass_stocks_everywhere(dt, path_to_aldo_clc, epci)
  clc_num <- read_excel(path_to_aldo_clc, sheet= "imp")
  clc_num <- as.data.table(clc_num)
  clc_num <- clc_num[, c(1, 3, 5)]
  clc_num <-  clc_num[!is.na(clc_category)]

  dt <- merge(dt, clc_num, by.x= "code", by.y = "clc_category")

  ### Convert to tCO2
  dt <- dt %>%
    mutate(
      soil_carbon_content = soil_carbon_content*44/12,
      biomass_carbon_content_in_fo = biomass_carbon_content_in_fo*44/12,
      biomass_carbon_content_out_fo = biomass_carbon_content_out_fo*44/12,
      biomass_carbon_content = biomass_carbon_content*44/12,
      total_carbon_content = total_carbon_content*44/12
      )

  return(dt)

}



#' Compute total carbon stocks at a certain date, of a chosen region.
#' Four flows categories : stocks in biomass in forest, stocks in biomass out forest,
#' stocks in soils, and total cabon stocks
#' @param carbon_stocks st object returned by 'get_carbon_storage' funciton
#' @return a data.table with the flows in ktCO2/year
#' @export
#' @importFrom data.table data.table
#' @importFrom sf st_area
get_total_carbon_stocks <- function(carbon_stocks){

  ### Compute total flows on the territory

  stock_biomass_in_forest <- round(sum(st_area(carbon_stocks$geometry) * 1e-04 * 1e-03 * carbon_stocks$biomass_carbon_content_in_fo),digits = 2)
  stock_biomass_in_forest <- as.numeric(stock_biomass_in_forest)

  stock_biomass_out_forest <- round(sum(st_area(carbon_stocks$geometry) * 1e-04 * 1e-03 * carbon_stocks$biomass_carbon_content_out_fo),digits = 2)
  stock_biomass_out_forest <- as.numeric(stock_biomass_out_forest)

  stock_in_soils <- round(sum(st_area(carbon_stocks$geometry) * 1e-04 *1e-03 *carbon_stocks$soil_carbon_content, na.rm= T), digits = 2)
  stock_in_soils <- as.numeric(stock_in_soils)

  total_carbon_stocks <- round(sum(st_area(carbon_stocks$geometry) * 1e-04 *1e-03 *  carbon_stocks$total_carbon_content, na.rm =T), digits = 2)
  total_carbon_stocks <- as.numeric(total_carbon_stocks)


  stocks_category <- c("stock_biomass_in_forest", "stock_biomass_out_forest",
                      "stock_in_soil", "total_carbon_stocks")

  stocks_value <- c(stock_biomass_in_forest, stock_biomass_out_forest, stock_in_soils, total_carbon_stocks)

  dt_total_stocks <- data.table(stocks_category, stocks_value)

  return(dt_total_stocks)

}
