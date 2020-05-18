
.onLoad <- function(libname = find.package("covid19.model.sa2"), pkgname = "covid19.model.sa2") {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(".",
                             "school_days_per_wk", "all_full_time",
                             "schools_open", "only_Year12",
                             "age_based_lockdown", "tests_by_state",
                             "max_persons_per_event",
                             "max_persons_per_supermarket",
                             "workplaces_open",
                             "workplace_size_lmu",
                             "workplace_size_lsi",
                             "workplace_size_beta",
                             unlist(sapply(dir(file.path(find.package("covid19.model.sa2"),
                                                         "extdata"),
                                               pattern = "\\.fst",
                                               full.names = TRUE,
                                               recursive = TRUE),
                                           fst_columns))))

  }

  if (is.null(getOption("covid19.model.sa2_dataEnv"))) {
    options("covid19.model.sa2_dataEnv" = new.env())
  }

  if (is.null(getOption("covid19.model.sa2_nThread"))) {
    options("covid19.model.sa2_nThread" = 1L)
  }

  if (is.null(getOption("covid19.model.sa2.fst2_progress"))) {
    options("covid19.model.sa2.fst2_progress" = interactive())
  }

  if (is.null(getOption("covid19.model.sa2_useDataEnv"))) {
    options(covid19.model.sa2_useDataEnv = FALSE)
  }

  # Initializing seems to help
  tt <- as.integer(Sys.time()) %% 65536L
  aa <- 8L * tt
  ll <- do_lemire_rand(aa, do_lemire_rand(2096, c(aa, integer(1024))))
  tt <- aa <- ll <- NULL


  invisible(NULL)
}

.onUnload <- function(libpath) {
  library.dynam.unload("covid19.model.sa2", libpath)
}

