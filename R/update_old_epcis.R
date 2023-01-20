#' Update outdated epcis
#'
#' Write a file that matches old epcis found in aldo with the new classification
#'
#' @details
#' The function write a `epcis_old_new.csv` file with the corresponding old/new epcis.
#'
#' @usage
#' update_outdated_epcis()
#'
#' @return None
#' @importFrom readxl read_excel
#' @importFrom data.table as.data.table
#' @importFrom here here
#'
update_outdated_epcis <- function(){
  path_2012 <- here("data", "insee", "epci", "epci-au-01-01-2012.xls")
  epci_2012 <- as.data.table(read_excel(path_2012, sheet = "Composition communale des EPCI",  skip =1))
  path_2021 <- here("data", "insee", "epci","Intercommunalite_Metropole_au_01-01-2022.xlsx")
  epci_2021 <- as.data.table(read_excel(path_2021, sheet = "Composition_communale", skip = 5))


  path_to_aldo <- here("data", "aldo", "base_data",  "Outil ALDO_2021_12.xlsx")
  dt <- read_excel(path_to_aldo, sheet = "Ref_Sols")
  dt <- as.data.table(dt)
  dt <- dt[, c(2, 5)]
  names(dt) <- c("EPCI_adlo", 'LIBEPCI')

  missing_epcis <- unique(epci_2021[!(EPCI %in% dt$EPCI_adlo), ])
  corr_epci <- merge(missing_epcis[, list(new_epci = EPCI, CODGEO)], epci_2012[, list(old_epci= EPCI, CODGEO)], by = "CODGEO")
  corr_epci <- unique(corr_epci[old_epci!= new_epci & old_epci != "ZZZZZZZZZ" , list(old_epci, new_epci)])

  fwrite(corr_epci, here("data", "ign", "epcis_old_new.csv"))
}


#'
#' Update old epcis found in aldo to the new classification
#'
#' @param dt
#' @param col_name
#' @return dt
#' @importFrom data.table setnames fread
#' @importFrom here here
#' @importFrom dplyr select
update_epcis <- function(dt, col_name){
  corr_epci <- fread(here("data", "ign", "epcis_old_new.csv"))
  setnames(dt, col_name, "epci")
  dt <- merge(dt, corr_epci, by.x= "epci", by.y = "old_epci", all.x = T)
  dt <- as.data.table(dt)
  dt <- dt[, epci := ifelse(is.na(new_epci), epci, new_epci)]
  dt <- dt[, new_epci :=NULL]
  setnames(dt, "epci", col_name)

  return(dt)
}
