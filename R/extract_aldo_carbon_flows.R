#' Extracts the data for carbon flows from the Aldo Excel tool.
#' @param path_to_aldo
#'
#' @return A data.table object.
#' @importFrom data.table as.data.table melt setnames tstrsplit like
#' @importFrom readxl read_excel
#'
#'
extract_aldo_carbon_flows <- function(path_to_aldo) {

  path_to_aldo <- here("data", "aldo", "base_data",  "Outil ALDO_2021_12.xlsx")

  soil_flows <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  soil_flows <- as.data.table(soil_flows)
  soil_flows <- soil_flows[, c(2, 19:47)]
  soil_flows[is.na(soil_flows), ] <- 0
  soil_flows <- update_epcis(soil_flows, "EPCI_Siren")

  soil_flows <- melt(
    soil_flows,
    id.vars = "EPCI_Siren",
    variable.name = "affectation",
    value.name = "flow"
  )

  soil_flows <- soil_flows[, c("f", "from", "to1", "bonus", "zpc") := data.table::tstrsplit(affectation, "_", fixed=TRUE)]
  soil_flows$bonus[soil_flows$bonus == "%zpc"] <-""
  soil_flows[, bonus:= ifelse(bonus == "%zpc", "", bonus)]
  soil_flows[, to := paste(to1, bonus)]
  soil_flows[, to := ifelse(to =="art arb", "art_arb", to)]
  soil_flows[, to := ifelse(to %in% c("art  imp", "art imp"), "art_imp", to)]
  soil_flows[, to := ifelse(to =="art enh", "art_enh", to)]
  soil_flows[, from := gsub("sosl", "sols", from)]
  soil_flows[, to := gsub("sosl", "sols", to)]
  soil_flows[, from := gsub("prairies", "prai", from)]
  soil_flows[, to := gsub('\\s+', '', to)]
  soil_flows[, from := gsub('\\s+', '', from)]

  path_to_aldo_clc <- here("data", "aldo", "base_data", "aldo_clc_categories.xlsx")
  clc_num <- read_excel(path_to_aldo_clc)
  clc_num <- as.data.table(clc_num)
  clc_num_soils <- clc_num[, c(1, 2, 5)]
  clc_num_soils <-  clc_num_soils[!is.na(aldo_soil_category)]

  clc_num_enh <- as.data.table(read_excel(path_to_aldo_clc, sheet = "enh"))
  clc_num_enh <-  clc_num_enh[, list(aldo_soil_category_enh, aldo_soil_category_short_enh, clc_category)]
  names(clc_num_enh) <- names(clc_num_soils)
  clc_num_soils <- rbind(clc_num_soils, clc_num_enh)

  soil_flows <- merge(soil_flows, clc_num_soils, by.x= "from", by.y = "aldo_soil_category_short", allow.cartesian = T, all.x= T)
  setnames(soil_flows, "aldo_soil_category", "from_id")
  setnames(soil_flows, "clc_category", "from_clc")
  soil_flows <- merge(soil_flows, clc_num_soils, by.x= "to", by.y = "aldo_soil_category_short", allow.cartesian = T)
  setnames(soil_flows, "aldo_soil_category", "to_id")
  setnames(soil_flows, "clc_category", "to_clc")

  ## Retrieve units
  soil_flows[, unit := "tC/ha/an"]
  artif_soils <- c("sols artificiels arborés et buissonants",
                   "sols artificiels imperméabilisés", "sols artificiels enherbés")
  soil_flows[, unit := ifelse(to_id %in% artif_soils, "tC/ha", unit)]
  soil_flows[, unit := ifelse(to_id == "zones humides" | from_id == "zones humides", "tC/ha", unit)]
  soil_flows <- soil_flows[, list(EPCI_Siren, from_clc, from_id, to_clc, to_id, flow, unit)]



  biomass_flows_wo_forests <- read_excel(path_to_aldo, sheet = "Ref_Biom_HorsF")
  biomass_flows_wo_forests <- as.data.table(biomass_flows_wo_forests)
  biomass_flows_wo_forests <- biomass_flows_wo_forests[, c(1, 14:91)]
  biomass_flows_wo_forests[is.na(biomass_flows_wo_forests), ] <- 0
  biomass_flows_wo_forests <- update_epcis(biomass_flows_wo_forests, "siren")


  biomass_flows_wo_forests <- melt(
    biomass_flows_wo_forests,
    id.vars = "siren",
    variable.name = "affectation",
    value.name = "flow"
  )

  biomass_flows_wo_forests[, c("from", "to") := data.table::tstrsplit(affectation, " vers ", fixed=TRUE)]
  biomass_flows_wo_forests[, c("to", "unit") := data.table::tstrsplit(to, " (", fixed=TRUE)]
  biomass_flows_wo_forests[, unit := ifelse(is.na(unit), "tC/ha", unit)]
  biomass_flows_wo_forests[, unit := ifelse(unit == "tC/Ha/an)", "tC/ha/an", unit)]
  biomass_flows_wo_forests[, from := tolower(from)]
  biomass_flows_wo_forests[, to := tolower(to)]
  biomass_flows_wo_forests[, from := gsub("aritificiels", "artificiels", from)]
  biomass_flows_wo_forests[, to := gsub("aritificiels", "artificiels", to)]
  biomass_flows_wo_forests[, from := gsub("vigne$", "vignes", from)]
  biomass_flows_wo_forests[, to := gsub("vigne$", "vignes", to)]
  biomass_flows_wo_forests[, from := gsub("sosl", "sols", from)]
  biomass_flows_wo_forests[, to := gsub("sosl", "sols", to)]


  clc_num_biom_wo_forests <- clc_num[, c(3, 4, 5)]
  clc_num_biom_wo_forests <-  clc_num_biom_wo_forests[!is.na(aldo_flows_categories), ]

  clc_num_arb <- as.data.table(read_excel(path_to_aldo_clc, sheet = "enh"))
  clc_num_arb[, aldo_flows_categories := aldo_biomass_category_arb]
  clc_num_arb <-  clc_num_arb[, list(aldo_flows_categories, aldo_biomass_category_arb, clc_category)]
  names(clc_num_arb) <- names(clc_num_biom_wo_forests)
  clc_num_biom_wo_forests <- rbind(clc_num_biom_wo_forests, clc_num_arb)

  biomass_flows_wo_forests <- merge(biomass_flows_wo_forests, clc_num_biom_wo_forests, by.x= "from", by.y = "aldo_flows_categories", allow.cartesian = T, all.x = T)
  setnames(biomass_flows_wo_forests, "aldo_biomass_category", "from_id")
  setnames(biomass_flows_wo_forests, "clc_category", "from_clc")

  biomass_flows_wo_forests <- merge(biomass_flows_wo_forests, clc_num_biom_wo_forests, by.x= "to", by.y = "aldo_flows_categories", allow.cartesian = T, all.x = T)
  setnames(biomass_flows_wo_forests, "aldo_biomass_category", "to_id")
  setnames(biomass_flows_wo_forests, "siren", "EPCI_Siren")
  setnames(biomass_flows_wo_forests, "clc_category", "to_clc")
  biomass_flows_wo_forests <- biomass_flows_wo_forests[, list(EPCI_Siren, from_clc, from_id, to_clc, to_id , flow, unit)]


  dt1_forest <- read_excel(path_to_aldo, sheet = "Ref_Biom_foret")
  dt1_forest <- as.data.table(dt1_forest)
  dt1_forest <- dt1_forest[, c(1, 3, 12)]
  dt1_forest[is.na(dt1_forest), ] <- 0
  dt1_forest <- update_epcis(dt1_forest, "SIREN_EPCI")

  dt2_forest <- read_excel(path_to_aldo, sheet = "Ref_Biom_Peup")
  dt2_forest <- as.data.table(dt2_forest)
  dt2_forest <- dt2_forest[, c(1, 12)]
  dt2_forest[is.na(dt2_forest), ] <- 0
  dt2_forest$COMPOSITION <- "Peupleraies"
  dt2_forest <- update_epcis(dt2_forest, "SIREN_EPCI")

  forest_flows <- rbind(dt1_forest, dt2_forest)

  setnames(forest_flows, "BILAN_CARB (tC∙ha-1∙an-1)", "flow")
  forest_flows <- forest_flows[, COMPOSITION := tolower(COMPOSITION)]
  forest_flows <- forest_flows[forest_flows$COMPOSITION != 'total', ]

  clc_num_forest <- clc_num[, c(3, 5)]

  forest_flows <- merge(forest_flows, clc_num_forest, by.x= "COMPOSITION", by.y = "aldo_biomass_category", allow.cartesian = T)
  colnames(forest_flows) <- c('composition', 'EPCI_Siren', 'flow', 'clc_category')
  forest_flows[,  unit := "tC/ha/an"]


  fwrite(biomass_flows_wo_forests, here("data", "aldo", "downloaded_data","biomass_flows_wo_forests.csv"))
  fwrite(soil_flows, here("data", "aldo", "downloaded_data","soil_flows.csv"))
  fwrite(forest_flows, here("data","aldo", "downloaded_data", "forest_flows.csv"))



  return(dt)
}

