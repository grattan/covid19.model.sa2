

read_dataEnv <- function(obj = NULL) {
  if (is.null(obj)) {
    obj <- ls(envir = getOption("covid19.model.sa2_dataEnv"))
  }
  mget(obj, envir = getOption("covid19.model.sa2_dataEnv"))
}

