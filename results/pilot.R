
args <- commandArgs(trailingOnly = TRUE)
if (any(startsWith(args, "--cores="))) {
  cores_arg <- args[which.max(startsWith(args, "--cores="))]
  cores <- as.integer(sub("--cores=", "", cores_arg))

  # must be before fst
  options(fst_threads = cores)

} else {
  cores <- getOption("covid19.model.sa2_nThread", parallel::detectCores() - 2L)
}

verbosity <- 0L
if (any(startsWith(args, "--verbose="))) {
  verbose_arg <- args[which.max(startsWith(args, "--verbose="))]
  verbosity <- as.integer(sub("--verbose=", "", verbose_arg))
}




library(yaml)
library(fst)
library(data.table)
library(magrittr)
library(hutils)
library(hutilscpp)

library(covid19.model.sa2)
attachme()

ResultsDir <- "."
stopifnot(file.exists("pilot.R"))
if (dir.exists("/dev/shm")) {
  ResultsDir <- provide.dir("/dev/shm/covid19-pilot/")
  message("Using ResultsDir = ", ResultsDir)
}



options(covid19.model.sa2_nThread = cores)
options(covid19.model.sa2_useDataEnv = TRUE)

PolicyGrid <-
  CJ(schools_open = c(FALSE, TRUE),
     only_Year12 = c(FALSE, TRUE),
     school_days_per_wk = 1:5,
     contact_tracing_days_before_test = c(0:1),
     contact_tracing_days_until_result = c(3L, 5L),
     cafes_open = c(FALSE, TRUE),
     workplaces_open = c(0, 0.25, 0.5),
     workplace_size_max = c(10L, 50L, 100L)) %>%
  .[only_Year12 %implies% schools_open] %>%
  .[(school_days_per_wk > 1L) %implies% schools_open]

EpiGrid <- CJ(r_distribution = c("cauchy", "lnorm", "pois"),
              r_location = c(2/5, 1/5))

for (pr in nrow(PolicyGrid):1) {
  Policys <- PolicyGrid[pr]
  for (er in nrow(EpiGrid):1) {
    if (verbosity > 0) {
      message("pr = ", pr, "\t", "er = ", er, "\t", as.character(Sys.time()))
    }
    cat(pr, "\t", er, "\t", as.character(Sys.time()),  file = file.path(ResultsDir, "pilot.log"), append = TRUE)

    thisResultsDir <- provide.dir(file.path(ResultsDir,  paste0("P-", pr, "/", "E-", er)))


    Epis <- EpiGrid[er]

    ThisPolicy <-
      with(Policys,
           set_policypars(schools_open = schools_open,
                          only_Year12 = only_Year12,
                          school_days_per_wk = school_days_per_wk,
                          contact_tracing_days_before_test = contact_tracing_days_before_test,
                          contact_tracing_days_until_result = contact_tracing_days_until_result,
                          cafes_open = cafes_open,
                          workplaces_open = workplaces_open,
                          workplace_size_max = workplace_size_max))
    policy.yaml <- file.path(thisResultsDir, "Policy.yaml")
    nInfected.txt <- file.path(thisResultsDir, "nInfected.txt")
    if (file.exists(policy.yaml) &&
        file.exists(nInfected.txt)) {
      next  # likely other program has written
    }



    cat(as.yaml(unpack_multipolicy(list(ThisPolicy))),
        file = policy.yaml,
        sep = "\n")



    S <- simulate_sa2(50,
                      PolicyPars = ThisPolicy,
                      EpiPars = with(Epis,
                                     set_epipars(r_distribution = r_distribution,
                                                 r_location = r_location)),
                      returner = 1L,
                      showProgress = verbosity)
    write_fst(S, file.path(thisResultsDir, "N_by_Status.fst"), compress = 0L)


    # # these are constant for each, drop for disk space
    # cols_to_drop <- c("state", "pid", "Age",
    #                   "short_school_id", "short_dzn",
    #                   "seqN", "HouseholdSize")
    # invisible(S$Statuses[, (cols_to_drop) := NULL])
    #
    # writeLines(as.character(S$nInfected), nInfected.txt)
    # write_fst(S$Statuses, file.path(thisResultsDir, "SStatuses.fst"))
    # S <- NULL
  }
}
