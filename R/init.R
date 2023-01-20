#' @export
#'
.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Please make sure you are connected to the internet.")

  download_data()

}
