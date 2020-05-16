#' States as coded
#' @description State names are used throughout the package and are encoded
#' when the simulation is run. This is a convenience function that simply
#' returns a character vector of the state abbreviations recognized
#' by other functions.
#'
#' N.B. The vector is intended to be zero-indexed. So AUS is the
#' zeroth element of \code{states}.
#'
#'
#'
#' @export

states <- function() {
  # ABS Catalogue numbers
  c("AUS", "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OTH")
}
