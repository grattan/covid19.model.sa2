
arg2int <- function(..arg_ = c("--cores=", "--verbose=", "--machine="), default = 0L) {
  ..arg_ <- match.arg(..arg_)
  command_args <- commandArgs(trailingOnly = TRUE)
  arg <- command_args[which.max(startsWith(command_args, ..arg_))]

  if (length(arg)) {
    arg <- sub(..arg_, "", arg, fixed = TRUE)
    as.integer(arg, fixed = TRUE)
  } else {
    default
  }
}
stopifnot(file.exists("runner.R"), dir.exists("by-state"))

library(yaml)
library(fst)
library(data.table)
library(magrittr)
library(hutils)
library(hutilscpp)


library(covid19.model.sa2)

cores <- arg2int("--cores=", getOption("covid19.model.sa2_nThread", 1L))
verbose <- arg2int("--verbose=", 0L)
machine <- arg2int("--machine=", 1L)
options(covid19.model.sa2_useDataEnv = TRUE)
options(covid19.model.sa2.fst2_progress = FALSE)


PolicyGrid <-
  CJ(schools_open = c(FALSE, TRUE),
     only_Year12 = c(FALSE, TRUE),
     school_days_per_wk = c(1L, 3L, 5L),
     contact_tracing_days_before_test = c(0:1),
     contact_tracing_days_until_result = c(3L, 5L),
     cafes_open = c(FALSE, TRUE),
     workplaces_open = c(0, 0.25, 1),
     workplace_size_max = c(10L, 50L, 100L),
     do_age_based_lockdown = c(FALSE, TRUE),
     incubation_distribution = c("pois", "dirac"),
     incubation_mean = c(4, 6),
     r_distribution = c("cauchy", "lnorm"),
     r_location = c(0.05, 0.10, 0.25, 0.75),
     r_scale = c(0.05, 0.25, 0.5),
     resistance_threshold = c(400L, 900L),
     r_schools_location = c(0.05, 0.5, 1)) %>%
  .[only_Year12 %implies% schools_open] %>%
  .[(school_days_per_wk > 1L) %implies% schools_open] %>%
  .[(r_schools_location != r_location) %implies% schools_open] %>%
  .[(r_location <= 0.1) %implies% (r_distribution == "cauchy")] %>%
  .[(r_scale == 0.5) %implies% (r_distribution != "cauchy")] %>%
  .[(workplace_size_max > 10) %implies% (workplaces_open > 0)] %>%
  .[]

fwrite(PolicyGrid, "PolicyGrid")

i <- 1L
while (i < nrow(PolicyGrid)) {
  Policyi.csv <- paste0("./by-state/P-", i, "/Policy.csv")
  if (file.exists(Policyi.csv)) {
    i <- i + machine
    next
  }

  fwrite(PolicyGrid[i], provide.file(Policyi.csv), nThread = 1L)
  outi <- with(PolicyGrid[i], {
    if (isTRUE(do_age_based_lockdown)) {
      age_based_lockdown <- 65:100
    } else {
      age_based_lockdown <- integer(100)
    }

    policy <- set_policypars(schools_open = schools_open,
                             only_Year12 = only_Year12,
                             school_days_per_wk = school_days_per_wk,
                             contact_tracing_days_before_test = contact_tracing_days_before_test,
                             contact_tracing_days_until_result = contact_tracing_days_until_result,
                             workplaces_open = workplaces_open,
                             cafes_open = cafes_open,
                             age_based_lockdown = age_based_lockdown,
                             workplace_size_max = workplace_size_max)

    epis <- set_epipars(incubation_distribution = incubation_distribution,
                        incubation_mean = incubation_mean,
                        r_distribution = r_distribution,
                        r_location = r_location,
                        r_scale = r_scale,
                        resistance_threshold = resistance_threshold,
                        r_schools_location = r_schools_location)

    simulate_sa2(56,
                 PolicyPars = policy,
                 EpiPars = epis,
                 nThread = cores,
                 showProgress = verbose,
                 returner = 2L)

  })
  fwrite(outi, paste0("./by-state/P-", i, "/outi.csv"))
  i <- i + 1L
}






