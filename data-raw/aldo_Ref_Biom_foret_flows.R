  library(data.table)


  aldo_Ref_Biom_foret <- fread(here("data-raw", "aldo_Ref_Biom_foret.csv"))
  aldo_Ref_Biom_foret <- aldo_Ref_Biom_foret[, c(1, 3, 12)]
  aldo_Ref_Biom_foret[is.na(aldo_Ref_Biom_foret), ] <- 0
  names(aldo_Ref_Biom_foret) <- c("SIREN_EPCI", "COMPOSITION", "BILAN_CARB")

  aldo_Ref_Biom_Peup <- fread(here("data-raw", "aldo_Ref_Biom_Peup.csv"))
  aldo_Ref_Biom_Peup <- as.data.table(aldo_Ref_Biom_Peup)
  aldo_Ref_Biom_Peup <- aldo_Ref_Biom_Peup[, c(1, 12)]
  aldo_Ref_Biom_Peup[is.na(aldo_Ref_Biom_Peup), ] <- 0
  aldo_Ref_Biom_Peup$COMPOSITION <- "Peupleraies"
  names(aldo_Ref_Biom_Peup) <- c("SIREN_EPCI", "BILAN_CARB", "COMPOSITION")

  aldo_Ref_Biom_foret_flows <- rbind(aldo_Ref_Biom_foret, aldo_Ref_Biom_Peup)

  setnames(aldo_Ref_Biom_foret_flows, "BILAN_CARB", "flow")
  aldo_Ref_Biom_foret_flows <- aldo_Ref_Biom_foret_flows[, COMPOSITION := tolower(COMPOSITION)]
  aldo_Ref_Biom_foret_flows <- aldo_Ref_Biom_foret_flows[COMPOSITION != 'total', ]

  aldo_clc_category_imp <- fread(here("data-raw", "aldo_clc_category_imp.csv"))
  aldo_clc_category_imp <- aldo_clc_category_imp[, c(3, 5)]

  aldo_Ref_Biom_foret_flows <- merge(aldo_Ref_Biom_foret_flows, aldo_clc_category_imp, by.x= "COMPOSITION", by.y = "aldo_biomass_category", allow.cartesian = T)
  colnames(aldo_Ref_Biom_foret_flows) <- c('composition', 'EPCI_Siren', 'flow', 'clc_category')
  aldo_Ref_Biom_foret_flows[,  unit := "tC/ha/an"]
  aldo_Ref_Biom_foret_flows <- aldo_Ref_Biom_foret_flows[composition != 'total']

  aldo_Ref_Biom_foret_flows <- update_epcis(aldo_Ref_Biom_foret_flows, "EPCI_Siren")

  usethis::use_data(aldo_Ref_Biom_foret_flows, overwrite = TRUE)
