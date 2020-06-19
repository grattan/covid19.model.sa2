#' Platform utilities
#' @description Various utilities to determine the suitability of
#' the environment for running, such as 64-bit and sufficient memory.
#'
#' @param pkgs Packages required. Defaults to suggested packages.
#'
#' @return \code{TRUE} on 64-bit environments with suggested packages.
#'
#' @export

run_covid19_example <- function(pkgs = c("tibble", "testthat", "covr")) {
  is64bit() &&
    !identical(Sys.getenv("TRAVIS"), "true") &&
    all(vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = NA)) &&
    TRUE
}

