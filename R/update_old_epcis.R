#'
#' Update old epcis found in aldo to the new classification
#'
#' @param dt a data table with old epci codes
#' @param col_name name of the column with epci code
#'
#' @return dt a data table with updated epci codes
#'
#' @importFrom data.table setnames fread
#' @importFrom here here
#' @importFrom dplyr select
#'
update_epcis <- function(dt, col_name){
  corr_epci <- invisible(update_outdated_epcis)
  setnames(dt, col_name, "epci")
  dt <- merge(dt, corr_epci, by.x= "epci", by.y = "old_epci", all.x = T)
  dt <- as.data.table(dt)
  dt <- dt[, epci := ifelse(is.na(new_epci), epci, new_epci)]
  dt <- dt[, new_epci :=NULL]
  setnames(dt, "epci", col_name)

  return(dt)
}
