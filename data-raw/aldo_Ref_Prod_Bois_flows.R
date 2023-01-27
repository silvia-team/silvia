
dt_harvested_wood <- fread(here("data-raw", "aldo_Ref_Prod_Bois.csv"))
dt_harvested_wood <- dt_harvested_wood[, c(1,3, 7, 8, 9)]
dt_harvested_wood[is.na(dt_harvested_wood), ] <- 0
setnames(dt_harvested_wood, c("EPCI_Siren","wood_composition", "BO_harvest", "BI_harvest", "BE_harvest"))

# dt_harvested_wood <- dt_harvested_wood[EPCI_Siren %in% c(epcis, 0), ]

dt_harvested_wood <- melt(
  dt_harvested_wood,
  id.vars = c("EPCI_Siren", "wood_composition"),
  measure.vars = c("BO_harvest", "BI_harvest", "BE_harvest"),
  variable.name = "wood_use",
  value.name = "wood_harvested"
)

# Set France Epci code for France to 0
dt_harvested_wood[, EPCI_Siren:= ifelse(is.na(EPCI_Siren), 0, EPCI_Siren)]
france_harvested_wood <-  dt_harvested_wood[EPCI_Siren == 0, list(
  wood_harvested_france= sum(wood_harvested)), by = "wood_use"]

wood_use <- c("BO_harvest", "BI_harvest")
france_wood_flows <- c(812000, 751000)
dt_france_wood_stocks <- data.table(wood_use, france_wood_flows)

harvested_wood_flows <- merge(france_harvested_wood, dt_france_wood_stocks, by = "wood_use")
harvested_wood_flows[, ratio_stocks_harvest := france_wood_flows /wood_harvested_france]

harvested_wood_flows <- harvested_wood_flows[, list(wood_use, ratio_stocks_harvest)]

harvested_wood_flows <- merge(dt_harvested_wood, harvested_wood_flows,
                              by = "wood_use")

harvested_wood_flows[, carbon_flows := round(wood_harvested *ratio_stocks_harvest)]

aldo_Ref_Prod_Bois_flows <- harvested_wood_flows[,
                                             list(wood_carbon_flows = sum(carbon_flows)*1e-3),
                                             by= "EPCI_Siren"]

usethis::use_data(aldo_Ref_Prod_Bois_flows, overwrite = TRUE)
