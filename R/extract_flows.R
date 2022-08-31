# ##############################
# # Load libraries
#
# library("readxl")
# library("data.table")
# library("here")
# library("dplyr")
#
# ##############################
#
#
# carbon_flows_soil <- function(path_to_aldo) {
#
#   dt <- read_excel(path_to_aldo, sheet = "Ref_Sols")
#   dt <- as.data.table(dt)
#   dt <- dt[, c(2, 19:47)]
#
#   dt <- melt(
#     dt,
#     id.vars = "EPCI_Siren",
#     variable.name = "affectation",
#     value.name = "flow"
#   )
#
#   dt <- dt[, c("f", "from", "to1", "bonus", "zpc") := tstrsplit(affectation, "_", fixed=TRUE)]
#   dt$bonus[dt$bonus == "%zpc"] <-""
#   dt$to <- paste(dt$to1, dt$bonus)
#   dt$to[dt$to =="art arb"] <- "art_arb"
#   dt$to[dt$to %like% " imp"] <- "art_imp"
#   dt$to[dt$to =="art enh"] <- "art_enh"
#   dt$to[dt$to ==""] <- "art_enh"
#   dt[, from := gsub("sosl", "sols", from)]
#   dt[, to := gsub("sosl", "sols", to)]
#   dt$to <- gsub('\\s+', '', dt$to)
#   dt$from <- gsub('\\s+', '', dt$from)
#
#
#   path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")
#   clc_num <- read_excel(path_to_aldo_clc)
#   clc_num <- as.data.table(clc_num)
#   clc_num <- clc_num[, c(1, 2, 5)]
#   clc_num <-  unique(clc_num)
#
#   dt <- merge(dt, clc_num, by.x= "from", by.y = "aldo_soil_category_short", allow.cartesian = T, all.x= T)
#   setnames(dt, "aldo_soil_category", "from_id")
#   setnames(dt, "clc_category", "from_clc")
#   dt <- merge(dt, clc_num, by.x= "to", by.y = "aldo_soil_category_short", allow.cartesian = T)
#   setnames(dt, "aldo_soil_category", "to_id")
#   setnames(dt, "clc_category", "to_clc")
#
#   dt <- dt %>%
#     select(EPCI_Siren, from_clc, from_id, to_clc, to_id, flow)
#
#   dt$unit <- "tC/ha/an"
#   dt[dt$unit == "tC/Ha/an)", unit := "tC/ha/an"]
#
#
#   return(dt)
# }
#
#
# carbon_flows_biomass_wo_forest <- function(path_to_aldo) {
#
#   dt <- read_excel(path_to_aldo, sheet = "Ref_Biom_HorsF")
#   dt <- as.data.table(dt)
#   dt <- dt[, c(1, 14:91)]
#
#   dt <- melt(
#     dt,
#     id.vars = "siren",
#     variable.name = "affectation",
#     value.name = "flow"
#   )
#
#   dt <- dt[, c("from", "to") := tstrsplit(affectation, " vers ", fixed=TRUE)]
#   dt <- dt[, c("to", "unit") := tstrsplit(to, " (", fixed=TRUE)]
#   dt[is.na(unit), unit := "tC/ha"]
#   dt[dt$unit == "tC/Ha/an)", unit := "tC/ha/an"]
#   dt$from <- tolower(dt$from)
#   dt$to <- tolower(dt$to)
#   dt[, from := gsub("sosl", "sols", from)]
#   dt[, to := gsub("sosl", "sols", to)]
#   dt$to <- gsub('\\s+', '', dt$to)
#   dt$from <- gsub('\\s+', '', dt$from)
#
#   path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")
#   clc_num <- read_excel(path_to_aldo_clc)
#   clc_num <- as.data.table(clc_num)
#   clc_num <- clc_num[, c(3, 4, 5)]
#   clc_num <-  clc_num[!is.na(aldo_flows_categories)]
#
#   dt <- merge(dt, clc_num, by.x= "from", by.y = "aldo_flows_categories", allow.cartesian = T, all.x = T)
#   setnames(dt, "aldo_biomass_category", "from_id")
#   setnames(dt, "clc_category", "from_clc")
#   dt <- merge(dt, clc_num, by.x= "to", by.y = "aldo_flows_categories", allow.cartesian = T)
#   setnames(dt, "aldo_biomass_category", "to_id")
#   setnames(dt, "siren", "EPCI_Siren")
#   setnames(dt, "clc_category", "to_clc")
#
#   dt <- dt %>%
#     select(EPCI_Siren, from_clc, from_id, to_clc, to_id, flow, unit)
#
#
#   return(dt)
# }
#
# carbon_flows_forest <- function(path_to_aldo) {
#   dt <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
#   dt <- as.data.table(dt)
#   dt <- dt[, c(1, 3, 12)]
#
#   setnames(dt, "BILAN_CARB (tC∙ha-1∙an-1)", "flow")
#   dt <- dt %>% mutate(COMPOSITION = tolower(COMPOSITION))
#   dt <- dt[dt$COMPOSITION != 'total']
#
#   path_to_aldo_clc <- here("data", "aldo_clc_categories.xlsx")
#   clc_num <- read_excel(path_to_aldo_clc)
#   clc_num <- as.data.table(clc_num)
#   clc_num <- clc_num[, c(3, 5)]
#
#   dt <- merge(dt, clc_num, by.x= "COMPOSITION", by.y = "aldo_biomass_category", allow.cartesian = T)
#   colnames(dt) <- c('composition', 'EPCI_Siren', 'flow', 'clc_category')
#   dt$unit <- "tC/ha/an"
#
#   return(dt)
#
#
# }
#
# carbon_flows <- function(path_to_aldo) {
#
#   soil_flows <- carbon_flows_soil(path_to_aldo)
#   biomass_flows_wo_forests <- carbon_flows_biomass_wo_forest(path_to_aldo)
# #
# #   occupation_flows <- rbind(soil_flows, biomass_flows_wo_forests)
#   forest_flows <- carbon_flows_forest(path_to_aldo)
#
#   fwrite(biomass_flows_wo_forests, here("data", "biomass_flows_wo_forests.csv"))
#   fwrite(soil_flows, here("data", "soil_flows.csv"))
#   fwrite(forest_flows, here("data", "forest_flows.csv"))
#
#   return(invisible(0))
# }
#
# path_to_aldo <- "../../ALDO/Outil ALDO_2021_12.xlsx"
#
# forest <- carbon_flows_forest(path_to_aldo)
# biomass_flows_wo_forests <- carbon_flows_biomass_wo_forest(path_to_aldo)
# carbon_flows(path_to_aldo)
