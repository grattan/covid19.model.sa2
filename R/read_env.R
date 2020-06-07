
# nocov start
read_dataEnv <- function(obj = NULL) {
  if (is.null(obj)) {
    obj <- ls(envir = getOption("covid19.model.sa2_dataEnv"))
  }
  if (length(obj) == 1L) {
    return(get0(obj, envir = getOption("covid19.model.sa2_dataEnv")))
  } else {
    mget(obj, envir = getOption("covid19.model.sa2_dataEnv"))
  }
}

clear_dataEnv <- function() {
  e <- getOption("covid19.model.sa2_dataEnv")
  rm(list = ls(envir = e), envir = e)
}

# nocov end
