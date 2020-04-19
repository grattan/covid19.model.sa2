
.onLoad <- function(libname = find.package("covid19.model.sa2"), pkgname = "covid19.model.sa2") {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(".")
  }

  if (is.null(getOption("covid19.model.sa2_dataEnv"))) {
    options("covid19.model.sa2_dataEnv" = new.env())
  }

  if (is.null(getOption("covid19.model.sa2_nThread"))) {
    options("covid19.model.sa2_nThread" = 1L)
  }

  invisible(NULL)
}


