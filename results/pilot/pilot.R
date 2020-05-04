library(yaml)
library(fst)
library(data.table)
library(magrittr)
library(hutils)
library(hutilscpp)

library(covid19.model.sa2)
attachme()

stopifnot(file.exists("pilot.R"))

options(covid19.model.sa2_nThread = parallel::detectCores() - 2L)
options(covid19.model.sa2_useDataEnv = TRUE)

PolicyGrid <- CJ(schools_open = c(FALSE, TRUE),
                 only_Year12 = c(FALSE, TRUE),
                 school_days_per_wk = 1:5,
                 contact_tracing_days_before_test = c(0:1),
                 contact_tracing_days_until_result = c(3L, 5L),
                 cafes_open = c(FALSE, TRUE),
                 workplaces_open = c(0, 0.25, 0.5),
                 workplace_size_max = c(10L, 50L, 100L))
PolicyGrid[only_Year12 %implies% schools_open]
PolicyGrid[(school_days_per_wk > 1L) %implies% schools_open]

EpiGrid <- CJ(r_distribution = c("cauchy", "lnorm", "pois"),
              r_location = c(2/5, 1/5))


for (pr in 1:nrow(PolicyGrid)) {
  Policys <- PolicyGrid[pr]
  for (er in 1:nrow(EpiGrid)) {
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
    provide.file(policy.yaml <- paste0("P-", pr, "/", "E-", er, "Policy.yaml"))
    cat(as.yaml(unpack_multipolicy(list(ThisPolicy))),
        file = policy.yaml,
        sep = "\n")



    S <- simulate_sa2(50,
                      PolicyPars = ThisPolicy,
                      EpiPars = with(Epis,
                                     set_epipars(r_distribution = r_distribution,
                                                 r_location = r_location)))

    write_fst(S$Statuses, paste0("P-", pr, "/", "E-", er, "SStatuses.fst"))
  }
}
