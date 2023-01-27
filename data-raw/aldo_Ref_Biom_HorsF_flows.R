library(data.table)

aldo_Ref_Biom_HorsF_flows <- fread(here("data-raw", "aldo_Ref_Biom_HorsF.csv"))
aldo_Ref_Biom_HorsF_flows <- aldo_Ref_Biom_HorsF_flows[, c(1, 14:91)]
aldo_Ref_Biom_HorsF_flows[is.na(aldo_Ref_Biom_HorsF_flows), ] <- 0
aldo_Ref_Biom_HorsF_flows <- update_epcis(aldo_Ref_Biom_HorsF_flows, "siren")


aldo_Ref_Biom_HorsF_flows <- melt(
  aldo_Ref_Biom_HorsF_flows,
  id.vars = "siren",
  variable.name = "affectation",
  value.name = "flow"
)

aldo_Ref_Biom_HorsF_flows[, c("from", "to") := data.table::tstrsplit(affectation, " vers ", fixed=TRUE)]
aldo_Ref_Biom_HorsF_flows[, c("to", "unit") := data.table::tstrsplit(to, " (", fixed=TRUE)]
aldo_Ref_Biom_HorsF_flows[, unit := ifelse(is.na(unit), "tC/ha", unit)]
aldo_Ref_Biom_HorsF_flows[, unit := ifelse(unit == "tC/Ha/an)", "tC/ha/an", unit)]
aldo_Ref_Biom_HorsF_flows[, from := tolower(from)]
aldo_Ref_Biom_HorsF_flows[, to := tolower(to)]
aldo_Ref_Biom_HorsF_flows[, from := gsub("aritificiels", "artificiels", from)]
aldo_Ref_Biom_HorsF_flows[, to := gsub("aritificiels", "artificiels", to)]
aldo_Ref_Biom_HorsF_flows[, from := gsub("vigne$", "vignes", from)]
aldo_Ref_Biom_HorsF_flows[, to := gsub("vigne$", "vignes", to)]
aldo_Ref_Biom_HorsF_flows[, from := gsub("sosl", "sols", from)]
aldo_Ref_Biom_HorsF_flows[, to := gsub("sosl", "sols", to)]


aldo_clc_category_imp <- fread(here("data-raw", "aldo_clc_category_imp.csv"))
aldo_clc_category_imp <- aldo_clc_category_imp[, c(3, 4, 5)]
aldo_clc_category_imp <-  aldo_clc_category_imp[!is.na(aldo_flows_categories), ]

aldo_clc_category_enh <- fread(here("data-raw", "aldo_clc_category_enh.csv"))
aldo_clc_category_enh[, aldo_flows_categories := aldo_biomass_category_arb]
aldo_clc_category_enh <-  aldo_clc_category_enh[, list(aldo_flows_categories,
                                                       aldo_biomass_category_arb,
                                                       clc_category)]
names(aldo_clc_category_enh) <- names(aldo_clc_category_imp)
aldo_clc_category <- rbind(aldo_clc_category_imp, aldo_clc_category_enh)

aldo_Ref_Biom_HorsF_flows <- merge(aldo_Ref_Biom_HorsF_flows, aldo_clc_category, by.x= "from", by.y = "aldo_flows_categories", allow.cartesian = T, all.x = T)
setnames(aldo_Ref_Biom_HorsF_flows, "aldo_biomass_category", "from_id")
setnames(aldo_Ref_Biom_HorsF_flows, "clc_category", "from_clc")

aldo_Ref_Biom_HorsF_flows <- merge(aldo_Ref_Biom_HorsF_flows, aldo_clc_category, by.x= "to", by.y = "aldo_flows_categories", allow.cartesian = T, all.x = T)
setnames(aldo_Ref_Biom_HorsF_flows, "aldo_biomass_category", "to_id")
setnames(aldo_Ref_Biom_HorsF_flows, "siren", "EPCI_Siren")
setnames(aldo_Ref_Biom_HorsF_flows, "clc_category", "to_clc")
aldo_Ref_Biom_HorsF_flows <- aldo_Ref_Biom_HorsF_flows[, list(EPCI_Siren, from_clc, from_id, to_clc, to_id , flow, unit)]
aldo_Ref_Biom_HorsF_flows <- unique(aldo_Ref_Biom_HorsF_flows)
aldo_Ref_Biom_HorsF_flows <- aldo_Ref_Biom_HorsF_flows[!is.na(from_clc) & !is.na(to_clc), ]


aldo_Ref_Biom_HorsF_flows <- update_epcis(aldo_Ref_Biom_HorsF_flows, "EPCI_Siren")

usethis::use_data(aldo_Ref_Biom_HorsF_flows, overwrite = TRUE)
