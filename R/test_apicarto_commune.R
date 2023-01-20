
#' Test apicarto_commune function
#' Check if the insee code exists
#'
#' @param city_num the insee code of the city
#'
#' @return either an error message, or nothing
#'
#' @importFrom happign get_apicarto_commune
test_apicarto_commune <- function(city_num) {
  return(tryCatch(
    get_apicarto_commune(city_num),
    error=function(e){
      message(paste0("This this insee or department code was not found the insee database : ", city_num))
      return(NULL)
    }))
}
