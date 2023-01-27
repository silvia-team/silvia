library(data.table)

epci_2012 <- fread(here("data-raw", "Composition_communale_EPCI_2012.csv"))

epci_2022 <- fread(file= here("data-raw", "Composition_communale_EPCI_2022.csv"))

aldo_Ref_Sols <-   fread(here("data-raw", "aldo_Ref_Sols.csv"))
aldo_Ref_Sols <- aldo_Ref_Sols[, c(2, 5)]
names(dt) <- c("EPCI_adlo", 'LIBEPCI')

missing_epcis <- unique(epci_2022[!(EPCI %in% dt$EPCI_adlo), ])
update_outdated_epcis <- merge(missing_epcis[, list(new_epci = EPCI, CODGEO)], epci_2012[, list(old_epci= EPCI, CODGEO)], by = "CODGEO")
update_outdated_epcis <- unique(update_outdated_epcis[old_epci!= new_epci & old_epci != "ZZZZZZZZZ" , list(old_epci, new_epci)])

usethis::use_data(update_outdated_epcis, overwrite = TRUE)
