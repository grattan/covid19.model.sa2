test_that(paste(as.character(Sys.time()), "set_initial_by_state works"), {
  skip_if_not_installed("data.table")
  library(data.table)
  aus <- read_sys("australia.fst")
  aus[, status := set_initial_by_state(state)]
  expect_true(is.integer(aus[["status"]]))  # essentially check that the defaults are ok


  A <- set_initial_by_state(1L, .population = 10e3)
  expect_equal(length(A), 10e3)

  B <- set_initial_by_state(1L,
                            dead = 10, healed = 100, active = 400, critical = 50,
                            .population = 500e3)
  expect_equal(sum(B == status_killed()), 10)
  expect_equal(sum(B == status_critic()), 50)


})


test_that(paste(as.character(Sys.time()), "set_initial retrospectively"), {
  skip_if(is32bit())
  skip_if_not_installed("data.table")
  library(data.table)

  cases <- read_sys("time_series_cases.fst")
  killed <- read_sys("time_series_deaths.fst")
  healed <- read_sys("time_series_recovered.fst")


  S <- simulate_sa2(5,
                    .first_day = "2020-04-01",
                    EpiPars = set_epipars(incubation_distribution = "dirac",
                                          incubation_mean = 5,
                                          illness_distribution = "dirac",
                                          illness_mean = 5))

  # Number of cases should be consistent with cumulative cases
  expect_true(abs(S$Statuses[V1 != 0, .N] - 4860L) < 50L)
})


test_that(paste(as.character(Sys.time()), "Set initial by state using a wide tribble"), {
  skip_on_cran()
  skip_if(is32bit())
  skip_if_not_installed("withr")
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  manual_initial_status <-
    tibble::tribble(
      ~state, ~active, ~critical, ~dead, ~healed,
      "NSW",     100,         9,     6,      30,
      "VIC",      40,         5,     4,      20,
      "QLD",      30,         2,     3,      10,
       "SA",      10,         0,     0,      10,
       "WA",      12,         0,     0,      10,
      "TAS",      20,         0,     0,       1,
       "NT",       1,         0,     0,       3,
      "ACT",    1000,         0,     0,       1,
      "OTH",     100,         0,     0,       0)

  library(data.table)
  clear_dataEnv()
  S <- simulate_sa2(5,
                    .first_day = "2020-06-01",
                    InitialStatus = manual_initial_status,
                    EpiPars = set_epipars(incubation_distribution = "dirac",
                                          incubation_mean = 100L),
                    PolicyPars = set_policy_no_restrictions(),
                    use_dataEnv = FALSE)
  n_act_active <- S$Statuses[and3s(state == 8, V1 == status_insymp()), .N]
  # Account for quarantine
  n_act_active <- n_act_active / (1 - p_quarantine_by_date("2020-06-01"))
  expect_true(n_act_active %between% c(900, 1100))

test_that(paste(as.character(Sys.time()), "Set initial by state using a wide tribble"), {
  skip_on_cran()
  skip_if(is32bit())
  skip_if_not_installed("withr")
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  manual_initial_status <-
    tibble::tribble(
      ~state, ~active, ~critical, ~dead, ~healed,
      "NSW",     100,         9,     6,      30,
      "VIC",      40,         5,     4,      20,
      "QLD",      30,         2,     3,      10,
       "SA",      10,         0,     0,      10,
       "WA",      12,         0,     0,      10,
      "TAS",      20,         0,     0,       1,
       "NT",       1,         0,     0,       3,
      "ACT",    1000,         0,     0,       1,
      "OTH",     100,         0,     0,       0)

  library(data.table)
  clear_dataEnv()
  S <- simulate_sa2(5,
                    .first_day = "2020-06-01",
                    InitialStatus = manual_initial_status,
                    EpiPars = set_epipars(incubation_distribution = "dirac",
                                          incubation_mean = 100L),
                    PolicyPars = set_policy_no_restrictions(),
                    use_dataEnv = FALSE)
  n_act_active <- S$Statuses[and3s(state == 8, V1 == status_insymp()), .N]
  # Account for quarantine
  n_act_active <- n_act_active / (1 - p_quarantine_by_date("2020-06-01"))
  expect_true(n_act_active %between% c(900, 1100))

})

test_that(paste(as.character(Sys.time()), "error handling"), {
  expect_error(set_initial_by_state(), "missing.*no default")
  expect_error(set_initial_by_state("NSW", first_yday = list(5, 5)), "atomic")
  expect_error(set_initial_by_state("NSW", first_yday = c(5, 5)), "length.one")
  expect_error(set_initial_by_state("NSW", first_yday = c(NA_real_)), "NA")
  expect_error(set_initial_by_state("NSW", first_yday = c(5.5)), "whole number")
  expect_error(set_initial_by_state("NSW", first_yday = c(-1)), "earliest")
  expect_error(set_initial_by_state("NSW", first_yday = c(9999)), "latest allowed")
})



