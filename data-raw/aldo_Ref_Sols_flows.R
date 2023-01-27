library(data.table)

aldo_Ref_Sols_flows <- fread(here("data-raw", "aldo_Ref_Sols.csv"))
aldo_Ref_Sols_flows <- aldo_Ref_Sols_flows[, c(2, 19:47)]
aldo_Ref_Sols_flows[is.na(aldo_Ref_Sols_flows), ] <- 0
aldo_Ref_Sols_flows <- update_epcis(aldo_Ref_Sols_flows, "EPCI_Siren")

aldo_Ref_Sols_flows <- melt(
  aldo_Ref_Sols_flows,
  id.vars = "EPCI_Siren",
  variable.name = "affectation",
  value.name = "flow"
)

aldo_Ref_Sols_flows <- aldo_Ref_Sols_flows[, c("f", "from", "to1", "bonus", "zpc") := data.table::tstrsplit(affectation, "_", fixed=TRUE)]
aldo_Ref_Sols_flows$bonus[aldo_Ref_Sols_flows$bonus == "%zpc"] <-""
aldo_Ref_Sols_flows[, bonus:= ifelse(bonus == "%zpc", "", bonus)]
aldo_Ref_Sols_flows[, to := paste(to1, bonus)]
aldo_Ref_Sols_flows[, to := ifelse(to =="art arb", "art_arb", to)]
aldo_Ref_Sols_flows[, to := ifelse(to %in% c("art  imp", "art imp"), "art_imp", to)]
aldo_Ref_Sols_flows[, to := ifelse(to =="art enh", "art_enh", to)]
aldo_Ref_Sols_flows[, from := gsub("sosl", "sols", from)]
aldo_Ref_Sols_flows[, to := gsub("sosl", "sols", to)]
aldo_Ref_Sols_flows[, from := gsub("prairies", "prai", from)]
aldo_Ref_Sols_flows[, to := gsub('\\s+', '', to)]
aldo_Ref_Sols_flows[, from := gsub('\\s+', '', from)]

# Correspondance CLC codes
aldo_clc_category_imp <- fread(here("data-raw", "aldo_clc_category_imp.csv"))
aldo_clc_category_imp <- aldo_clc_category_imp[, c(1, 2, 5)]
aldo_clc_category_imp <-  aldo_clc_category_imp[!is.na(aldo_soil_category)]

aldo_clc_category_enh <- fread(here("data-raw", "aldo_clc_category_enh.csv"))
aldo_clc_category_enh <-  aldo_clc_category_enh[, list(aldo_soil_category_enh,
                                                       aldo_soil_category_short_enh,
                                                       clc_category)]
names(aldo_clc_category_enh) <- names(aldo_clc_category_imp)
aldo_clc_category <- rbind(aldo_clc_category_imp, aldo_clc_category_enh)



aldo_Ref_Sols_flows <- merge(aldo_Ref_Sols_flows, aldo_clc_category, by.x= "from", by.y = "aldo_soil_category_short", allow.cartesian = T, all.x= T)
setnames(aldo_Ref_Sols_flows, "aldo_soil_category", "from_id")
setnames(aldo_Ref_Sols_flows, "clc_category", "from_clc")
aldo_Ref_Sols_flows <- merge(aldo_Ref_Sols_flows, aldo_clc_category, by.x= "to", by.y = "aldo_soil_category_short", allow.cartesian = T)
setnames(aldo_Ref_Sols_flows, "aldo_soil_category", "to_id")
setnames(aldo_Ref_Sols_flows, "clc_category", "to_clc")

## Retrieve units
aldo_Ref_Sols_flows[, unit := "tC/ha/an"]
artif_soils <- c("sols artificiels arborés et buissonants",
                 "sols artificiels imperméabilisés", "sols artificiels enherbés")
aldo_Ref_Sols_flows[, unit := ifelse(to_id %in% artif_soils, "tC/ha", unit)]
aldo_Ref_Sols_flows[, unit := ifelse(to_id == "zones humides" | from_id == "zones humides", "tC/ha", unit)]
aldo_Ref_Sols_flows <- aldo_Ref_Sols_flows[, list(EPCI_Siren, from_clc, from_id, to_clc, to_id, flow, unit)]

aldo_Ref_Sols_flows[, flow := ifelse(is.na(flow), 0, flow)]
aldo_Ref_Sols_flows[, flow := ifelse(unit == "tC/ha/an", flow*20, flow)]
aldo_Ref_Sols_flows[, unit := ifelse(unit == "tC/ha/an", "tC/ha", unit)]

# Add litters
aldo_Ref_Sols_flows <- aldo_Ref_Sols_flows[, flow := ifelse(
  from_clc %in% c(311,312,313,324) & !(soil_flows$to_clc %in% c(311,312,313,324,141)),
  flow - 9,
  flow)]
aldo_Ref_Sols_flows <- aldo_Ref_Sols_flows[, flow := ifelse(
  !(from_clc %in% c(311,312,313,324)) & soil_flows$to_clc %in% c(311,312,313,324,141),
  flow + 9,
  flow)]


aldo_Ref_Sols_flows <- aldo_Ref_Sols_flows[, list(
  EPCI_Siren= as.character(EPCI_Siren),
  from_clc= as.character(from_clc),
  to_clc= as.character(to_clc),
  from_id, to_id, flow, unit
  )]

aldo_Ref_Sols_flows <- unique(aldo_Ref_Sols_flows)

# Convert to CO2eq
aldo_Ref_Sols_flows[, flow := flow*44/12]


### Add N2O flux related to carbon destocking in soils and litter
aldo_Ref_Sols_flows[, flow_N2O := ifelse(flow < 0.0,
                                flow *(1/15*0.01*44/25 + 1/15*0.3*0.0075*44/28)*298
                                ,0.0)]

aldo_Ref_Sols_flows <- unique(aldo_Ref_Sols_flows)

aldo_Ref_Sols_flows <- update_epcis(aldo_Ref_Sols_flows, "EPCI_Siren")

usethis::use_data(aldo_Ref_Sols_flows, overwrite = TRUE)
