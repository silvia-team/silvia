library(data.table)

epcis <- unique(dt$SIREN_EPCI)

aldo_Ref_Prod_Bois <- fread(here("data-raw", "aldo_Ref_Prod_Bois.csv"))

aldo_Ref_Prod_Bois <- aldo_Ref_Prod_Bois[, c(1,3, 7, 8, 9)]
aldo_Ref_Prod_Bois[is.na(aldo_Ref_Prod_Bois), ] <- 0
setnames(aldo_Ref_Prod_Bois, c("EPCI_Siren","wood_composition", "BO_harvest", "BI_harvest", "BE_harvest"))

aldo_Ref_Prod_Bois <- melt(
  aldo_Ref_Prod_Bois,
  id.vars = c("EPCI_Siren", "wood_composition"),
  measure.vars = c("BO_harvest", "BI_harvest", "BE_harvest"),
  variable.name = "wood_use",
  value.name = "wood_harvested"
)

# Set France Epci code for France to 0
aldo_Ref_Prod_Bois[, EPCI_Siren :=ifelse(is.na(EPCI_Siren), 0, EPCI_Siren)]
france_harvested_wood <-  aldo_Ref_Prod_Bois[EPCI_Siren == 0, ]
france_harvested_wood <- france_harvested_wood %>%
  group_by(wood_use) %>%
  summarise(wood_harvested_france = sum(wood_harvested))

wood_use <- c("BO_harvest", "BI_harvest")
france_wood_stocks <- c(177419001, 258680001)
dt_france_wood_stocks <- data.table(wood_use, france_wood_stocks)

france_harvested_wood <- merge(france_harvested_wood, dt_france_wood_stocks, by = "wood_use")
france_harvested_wood <- as.data.table(france_harvested_wood)

france_harvested_wood[, ratio_stocks_harvest := france_wood_stocks / wood_harvested_france]
france_harvested_wood <- france_harvested_wood[, list(wood_use, ratio_stocks_harvest)]
france_harvested_wood <- unique(france_harvested_wood)

france_harvested_wood <- merge(aldo_Ref_Prod_Bois, france_harvested_wood,
                               by = "wood_use")

france_harvested_wood[, carbon_stocks := round(wood_harvested *ratio_stocks_harvest)]

aldo_Ref_Prod_Bois <- france_harvested_wood %>%
  group_by(EPCI_Siren) %>%
  summarise(wood_carbon_stocks = round(sum(carbon_stocks), digits=1))

aldo_Ref_Prod_Bois <- as.data.table(aldo_Ref_Prod_Bois)

usethis::use_data(aldo_Ref_Prod_Bois, overwrite = TRUE)
