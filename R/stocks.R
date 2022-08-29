##############################
# Load libraries

library(readxl)
library(data.table)
library(here)
library(magrittr)
library(dplyr)
source(here("R", "select_region.R"))


# ------------------ Useful functions --------------------- #

f1 <- function(num) {
  format(num, big.mark = ' ')
}
# --------------------------------------------------------- #

aldo_clc_category <- function(path_to_aldo_clc, perimeter) {

  dt <- read_excel(path_to_aldo_clc)
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

retrieve_clc<- function(year){
  file_name <- paste('clc_', year, ".gpkg", sep = '')
  clc <- st_read(here("data", "arep", file_name))
  clc$year <- year
  clc <- clc %>%
    select(contains(c("code", "area", "year"))) %>%
    rename_at(vars(contains("code")), ~c("code"))


  return(clc)
}



retrieve_soil_stocks <- function(clc, path_to_aldo_clc, epci){

  # ------------- Retrieves soils carbon contents --------------------- #

  dt_soils <- aldo_clc_category(path_to_aldo_clc, "soils")


  dt_soils_content <- read.csv(here("data", "carbon_content_soil.csv"))
  dt_soils_stocks <- merge(dt_soils, dt_soils_content,
                           by = "aldo_soil_category",
                           all = TRUE, allow.cartesian=TRUE)
  dt_soils_stocks <- dt_soils_stocks %>% filter(EPCI_Siren == epci)
  dt_soils_stocks <- dt_soils_stocks[!is.na(clc_category)]

  dt_soils_stocks_zone <- merge(x=clc, y=dt_soils_stocks,
                                by.x ="code", by.y = "clc_category",
                                all= TRUE, allow.cartesian=TRUE)

  dt_soils_stocks_zone$soil_carbon_content[is.na(dt_soils_stocks_zone$soil_carbon_content)] <- 0


  # Add litters
  `%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
  dt_soils_stocks_zone$soil_carbon_content[dt_soils_stocks_zone$code %in% c(311,312,313,324)] %+=% 9


  # Set to 0 carbon stocks in water surfaces
  dt_soils_stocks_zone$soil_carbon_content[dt_soils_stocks_zone$code %in% c(511,512,521,523)] <- 0
  dt_soils_stocks_zone <- dt_soils_stocks_zone %>% tidyr::drop_na(code)
  dt_soils_stocks_zone$soil_carbon_content <- round(dt_soils_stocks_zone$soil_carbon_content)

  dt_soils_stocks_zone <- dt_soils_stocks_zone %>%
    select(EPCI_Siren, code, soil_carbon_content, year)

  return(dt_soils_stocks_zone)
}


retrieve_biomass_stocks_in_forests <- function(dt, path_to_aldo_clc, epci){

  dt_biomass <- aldo_clc_category(path_to_aldo_clc, "biomass")
  dt_biomass_forests <- read.csv("data/biomass_forests.csv")
  dt_biomass_forests <- dt_biomass_forests %>% filter(EPCI_Siren == epci)
  dt_biomass_forests_epci <- merge(dt_biomass_forests, dt_biomass, by= "aldo_biomass_category")

  dt <- merge(x=dt, y=dt_biomass_forests_epci,
              by.x ="code", by.y = "clc_category",
              all= TRUE)
  dt <- dt %>%
    select(code, EPCI_Siren = EPCI_Siren.x, soil_carbon_content,
           biomass_carbon_content_in_fo = biomass_carbon_content)

  dt$biomass_carbon_content_in_fo[is.na(dt$biomass_carbon_content_in_fo)] <- 0

  return(dt)
}


retrieve_biomass_stocks_out_forests <- function(dt, path_to_aldo_clc, epci){

  path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")
  dt_biomass <- aldo_clc_category(path_to_aldo_clc, "biomass")

  dt_biomass_wo_forests <- read.csv("data/biomass_wo_forests.csv")
  dt_biomass_wo_forests <- dt_biomass_wo_forests %>% filter(EPCI_Siren == epci)
  dt_biomass_wo_forests_epci <- merge(dt_biomass_wo_forests, dt_biomass, by= "aldo_biomass_category")

  dt <- merge(x=dt, y=dt_biomass_wo_forests_epci, by.x ="code", by.y = "clc_category", all = T)

  dt <- dt %>%
    select(code, EPCI_Siren = EPCI_Siren.x, soil_carbon_content,
           biomass_carbon_content_in_fo,
           biomass_carbon_content_out_fo = biomass_carbon_content
    )

  dt$biomass_carbon_content_out_fo[is.na(dt$biomass_carbon_content_out_fo)] <- 0

  return(dt)
}


retrieve_biomass_stocks_everywhere <- function(dt, path_to_aldo_clc, epci){

  dt <- retrieve_biomass_stocks_in_forests(dt, path_to_aldo_clc, epci)

  dt <- retrieve_biomass_stocks_out_forests(dt, path_to_aldo_clc, epci)

  dt$biomass_carbon_content <- dt$biomass_carbon_content_in_fo + dt$biomass_carbon_content_out_fo
  dt$total_carbon_content <- dt$biomass_carbon_content + dt$soil_carbon_content

  # dt <-  na.omit(dt)

  return(dt)
}


retrieve_harvested_wood <- function(epci){

  dt_harvested_wood <- read.csv("data/harvested_wood.csv")
  dt_harvested_wood <- setDT(dt_harvested_wood)

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


retrieve_stocks <-  function(year, epci){

  path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")

  clc <- retrieve_clc(year)
  dt <- retrieve_soil_stocks(clc, path_to_aldo_clc, epci)
  dt <- retrieve_biomass_stocks_everywhere(dt, path_to_aldo_clc, epci)


  clc_num <- read_excel(path_to_aldo_clc)
  clc_num <- as.data.table(clc_num)
  clc_num <- clc_num[, c(1, 3, 5)]
  clc_num <-  clc_num[!is.na(clc_category)]

  dt <- merge(dt, clc_num, by.x= "code", by.y = "clc_category")

  return(dt)
}


