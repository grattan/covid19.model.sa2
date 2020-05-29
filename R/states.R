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


state_population <- function(state) {
  if (is.character(state)) {
    state <- match(state, states()[-1])
  }
  c(6865690L, 5452864L, 4257348L, 1541787L, 2240063L, 460366L,
    183938L, 359583L, 3246L)[state]
}
