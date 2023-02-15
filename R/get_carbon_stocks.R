#' Get carbon stocks
#'
#' @description
#' `get_carbon_stocks` calculates the carbon stocks of the selected
#' territory for the selected year.
#' All stocks are in tCO2e/ha.
#'
#' @usage
#' get_carbon_stocks(year)
#'
#' @param year the reference year to be studied (1990, 2000, 2006, 2012, 2018)
#' @param data_path path to where the data is stored
#'
#' @importFrom data.table as.data.table
#' @importFrom here here
#' @importFrom readxl read_excel
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr rename_at select mutate filter summarise group_by
#'
#'
#' @return a sf object with the carbon stored in every polygon
#'
#' @examples
#'# Retrieve carbon stocks in 2018
#'
#' dt <- get_carbon_stocks("2018", data_path)
#'
#' @export
#'
get_carbon_stocks <-  function(year, data_path){

  # retrieve CLC ------------------------------------------------------------

  file_name <- paste0('zone_', year, ".gpkg", sep = '')
  clc_geom <- st_read(here(data_path, "corine_land_cover", file_name), quiet = T)

  clc_geom <- clc_geom %>% select(c("code", "X_mean", "SIREN_EPCI"))
  clc_geom$year <- year
  clc_geom <- tibble::rowid_to_column(clc_geom, "ID_unique")
  clc <- sf::st_drop_geometry(clc_geom)


  epcis <- unique(clc$SIREN_EPCI)

  # retrieve carbon stocks in soil -------------------------------------------

  aldo_Ref_Sols_stocks <- invisible(aldo_Ref_Sols_stocks)
  aldo_Ref_Sols_stocks <- aldo_Ref_Sols_stocks[EPCI_Siren %in% epcis, ]

  dt_stocks <- merge(x=clc, y=aldo_Ref_Sols_stocks,
                     by.x =c("SIREN_EPCI", "code"),
                     by.y = c("EPCI_Siren", "clc_category"),
                     all= TRUE, allow.cartesian=TRUE)

  dt_stocks <- as.data.table(dt_stocks)

  dt_stocks <- dt_stocks[!is.na(dt_stocks$ID), ]
  dt_stocks[, soil_carbon_content := X_mean*(soil_carbon_content_art -soil_carbon_content_enh) + soil_carbon_content_enh]
  dt_stocks[, soil_carbon_content := ifelse(is.na(soil_carbon_content),
                                            soil_carbon_content_art,
                                            soil_carbon_content)]

  # Add litters
  dt_stocks[, soil_carbon_content := ifelse(code %in% c(311,312,313,324),
                                            soil_carbon_content +9,
                                            soil_carbon_content)]

  # Consider that inland waters don't stock carbon
  dt_stocks[, soil_carbon_content := ifelse(code %in% c(511,512,521,523),
                                            0,
                                            soil_carbon_content)]

  dt_stocks <- dt_stocks [, list(ID_unique,
                                 EPCI_Siren= SIREN_EPCI,
                                 code,
                                 soil_carbon_content= round(soil_carbon_content),
                                 year,
                                 X_mean)]


  # retrieve carbon stocks in biomass (in forest) ----------------------------

  aldo_Ref_Biom_foret_stocks <- invisible(aldo_Ref_Biom_foret_stocks)
  aldo_Ref_Biom_foret_stocks[, EPCI_Siren := as.character(EPCI_Siren)]
  aldo_Ref_Biom_foret_stocks[, clc_category := as.character(clc_category)]
  aldo_Ref_Biom_foret_stocks <- aldo_Ref_Biom_foret_stocks[EPCI_Siren %in% epcis, ]

  dt_stocks <- merge(x=dt_stocks, y=aldo_Ref_Biom_foret_stocks,
              by.x =c("EPCI_Siren", "code"), by.y = c("EPCI_Siren", "clc_category"),
              all.x= TRUE, allow.cartesian = T)

  dt_stocks <- as.data.table(dt_stocks)
  dt_stocks <- dt_stocks[, biomass_carbon_content:= ifelse(is.na(dt_stocks$biomass_carbon_content),
                                             0,
                                             biomass_carbon_content)]

  dt_stocks <- dt_stocks[, list(
    ID_unique,
    code,
    EPCI_Siren,
    soil_carbon_content,
    biomass_carbon_content_in_fo = biomass_carbon_content,
    X_mean)]


  # retrieve carbon stocks in biomass (out forest) ----------------------------

  aldo_Ref_Biom_HorsF_stocks <- invisible(aldo_Ref_Biom_HorsF_stocks)
  aldo_Ref_Biom_HorsF_stocks[, EPCI_Siren := as.character(EPCI_Siren)]
  aldo_Ref_Biom_HorsF_stocks[, clc_category := as.character(clc_category)]
  aldo_Ref_Biom_HorsF_stocks <- aldo_Ref_Biom_HorsF_stocks[EPCI_Siren %in% epcis, ]

  dt_stocks <- merge(x=dt_stocks, y=aldo_Ref_Biom_HorsF_stocks, by.x =c("EPCI_Siren", "code"),
                     by.y = c("EPCI_Siren", "clc_category"), all.x = T, allow.cartesian = T)

  dt_stocks <- dt_stocks[, biomass_carbon_content_out_fo := X_mean*(biomass_carbon_content_art -biomass_carbon_content_arb) + biomass_carbon_content_arb]
  dt_stocks <- dt_stocks[, biomass_carbon_content_out_fo:= ifelse(is.na(biomass_carbon_content_out_fo), 0, biomass_carbon_content_out_fo)]


  dt_stocks <- dt_stocks [, list(
    ID_unique,
    code,
    aldo_soil_category,
    aldo_biomass_category,
    EPCI_Siren,
    soil_carbon_content,
    biomass_carbon_content_in_fo,
    biomass_carbon_content_out_fo,
    X_mean
  )]

  dt_stocks[, biomass_carbon_content :=  biomass_carbon_content_in_fo + biomass_carbon_content_out_fo]
  dt_stocks[, total_carbon_content := biomass_carbon_content + soil_carbon_content]
  dt_stocks[, total_carbon_content := ifelse(is.na(total_carbon_content), 0, total_carbon_content)]


  ### Convert to tCO2
  dt_stocks <- dt_stocks[, list(
    ID_unique,
    code,
    EPCI_Siren,
    aldo_soil_category,
    aldo_biomass_category,
    soil_carbon_content = soil_carbon_content*44/12,
    biomass_carbon_content_in_fo = biomass_carbon_content_in_fo*44/12,
    biomass_carbon_content_out_fo = biomass_carbon_content_out_fo*44/12,
    biomass_carbon_content = biomass_carbon_content*44/12,
    total_carbon_content = total_carbon_content*44/12,
    X_mean
    )
  ]

  dt_stocks[, stocks_unit := "tCO2e/ha"]
  clc_geom <- clc_geom %>% select(ID_unique)
  dt_stocks <- merge(dt_stocks, clc_geom, by = "ID_unique")
  dt_stocks <- dt_stocks[, -c("ID_unique")]
  dt_stocks <- st_as_sf(dt_stocks)

  return(dt_stocks)
}

