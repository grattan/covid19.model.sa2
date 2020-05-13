
library(data.table)
library(magrittr)
library(hutils)


PolicyGrid <-
  CJ(by_state = c(FALSE, TRUE),
     InitialStatus = list(
       list(dead = 100, healed = 7000, active = 100, critical = 1),
       list(dead = 100, healed = 6000, active = 2000, critical = 100),
       NA),
     schools_open = c(FALSE, TRUE),
     only_Year12 = c(FALSE, TRUE),
     school_days_per_wk = c(1L, 3L, 5L),
     do_contact_tracing = c(FALSE, TRUE),
     contact_tracing_days_before_test = 0,
     contact_tracing_days_until_result = 3L,
     cafes_open = c(FALSE, TRUE),
     workplaces_open = c(0, 0.50, 1),
     workplace_size_max = c(10L, 75L),
     do_age_based_lockdown = c(FALSE, TRUE),
     incubation_distribution = "pois",
     incubation_mean = 5,
     r_distribution = c("cauchy", "lnorm"),
     r_location = c(0.05, 0.25, 0.75),
     r_scale = c(0.05, 0.25, 0.5),
     resistance_threshold = 600L,
     r_schools_location = c("same", "less"),
  sorted = FALSE
  ) %>%
  .[, r_schools_location := ifelse(r_schools_location == "same",
                                   r_location,
                                   r_location * 0.25)] %>%
  .[by_state %implies% (is.na(InitialStatus))] %>%
  .[is.na(InitialStatus) %implies% by_state] %>%
  .[only_Year12 %implies% schools_open] %>%
  .[(school_days_per_wk > 1L) %implies% schools_open] %>%
  .[(r_location == 0.05) %implies% (r_distribution == "cauchy")] %>%
  .[(r_scale == 0.5) %implies% (r_distribution != "cauchy")] %>%
  .[(workplace_size_max > 10) %implies% (workplaces_open > 0)] %>%
  .[]
