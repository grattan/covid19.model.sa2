test_that(paste(as.character(Sys.time()), "simulation works"), {
  skip_on_cran()
  skip_if_not(identical(.Platform$r_arch, "x64"))
  skip_if_not(is64bit())
  skip_if_not_installed("data.table")
  library(data.table)
  DAYS <- 17L
  for (env_opt in c(getOption("covid19.model.sa2_useDataEnv", TRUE),
                    !getOption("covid19.model.sa2_useDataEnv", TRUE))) {
    for (thr in c(1L, 2L)) {

      # These tests take a long time (hours) and for some purposes
      # we can speed it up. On my computer, log the run, and don't
      # test both thread modes when just testing coverage
      if (Sys.getenv("USERNAME") == "hughp") {
        cat(as.character(Sys.time()), "\tsimulations\t", normalizePath(getwd(), winslash = "/"), "\n",
            file = "~/testthat-log.txt",
            append = TRUE)

        if (requireNamespace("covr", quietly = TRUE) && covr::in_covr()) {
          if (thr == 1L) {
            next
          }
        }
        options(covid19.model.sa2_nThread = 10L)
      }
      COVID_CLOUD_ID <- Sys.getenv("COVID_CLOUD_ID")
      if (COVID_CLOUD_ID != "") {

        if (COVID_CLOUD_ID == "A") {
          if (thr == 1L) {
            next
          } else {
            thr <- 16L
          }
        }
      }



      expect_true(is.logical(env_opt))

      S0 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = FALSE,
                                                     do_contact_tracing = FALSE),
                         # Use a constant 100% attack rate for testing
                         EpiPars = set_epipars(a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0),
                         use_dataEnv = env_opt)[["nInfected"]]
      S5 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     workplaces_open = 0.1,
                                                     schools_open = TRUE,
                                                     only_Year12 = FALSE,
                                                     school_days_per_wk = 3L,
                                                     max_persons_per_supermarket = 50),
                         EpiPars = set_epipars(q_supermarket = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 1,
                                               a_workplace_rate = 0),
                         nThread = thr)[["nInfected"]]
      expect_true(is.atomic(S5))
      skip_on_travis()

      S1 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 25,
                                                     do_contact_tracing = FALSE),
                         EpiPars = set_epipars(q_supermarket = 1/20,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S1))

      S2 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 40),
                         EpiPars = set_epipars(q_supermarket = 1/20,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S2))

      S3 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 40),
                         EpiPars = set_epipars(q_supermarket = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S3))


      S4 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     schools_open = TRUE,
                                                     only_Year12 = TRUE,
                                                     school_days_per_wk = 3L,
                                                     max_persons_per_supermarket = 45),
                         EpiPars = set_epipars(q_supermarket = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S4))



      S6 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     workplaces_open = 0.1,
                                                     schools_open = TRUE,
                                                     only_Year12 = FALSE,
                                                     school_days_per_wk = 5L,
                                                     max_persons_per_supermarket = 60,
                                                     lockdown_triggers__schools = FALSE),
                         EpiPars = set_epipars(q_supermarket = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 1,
                                               a_workplace_rate = 0))[["nInfected"]]

      stopifnot(is.atomic(S0),
                is.atomic(S1),
                is.atomic(S2),
                is.atomic(S3),
                is.atomic(S4),
                is.atomic(S4))

      skip_if(mean(head(S0) < head(S1)) < 0.8)
      skip_if(mean(head(S1) < head(S2)) < 0.8)
      skip_if(mean(head(S2) < head(S3)) < 0.8)
      skip_if(mean(head(S3) < head(S4)) < 0.8)

      if (env_opt) {
        expect_gt(mean(S0 < S1), 0.8)
        expect_gt(mean(S1 < S2), 0.8)
        expect_gt(mean(S2 < S3), 0.5)
        expect_gt(mean(S3 < S4), 0.5)
        expect_gt(mean(S4 < S5), 0.5)
        expect_gt(mean(S5 < S6), 0.5)
      } else {
        expect_gt(sum(c(`>`(mean(S0 < S1), 0.8),
                        `>`(mean(S1 < S2), 0.8),
                        `>`(mean(S2 < S3), 0.5),
                        `>`(mean(S3 < S4), 0.5),
                        `>`(mean(S5 < S6), 0.5),
                        `>`(mean(S4 < S5), 0.5))),
                  3)
      }
    }
  }

})

test_that(paste(as.character(Sys.time()), "prev segfaulting"), {
  skip_on_cran()
  skip_if(is32bit())
  skip_on_travis()
  skip_if_not_installed("data.table")
  skip_if_not_installed("parallel")

  n_threads <- function(m = 2L) {
    if (identical(Sys.getenv("TRAVIS"), "true")) {
      return(1L)
    }
    pmax.int(1L, parallel::detectCores() - m)
  }

  DAYS <- 45L

  S <- simulate_sa2(DAYS,
                    nThread = n_threads(),
                    PolicyPars = set_policypars(workplaces_open = 1,
                                                workplace_size_max = 1000))
  expect_true(hasName(S[[2]], "V45")) # i.e. 'have we got here?'
  S <- NULL  # memory is limited
  S <- simulate_sa2(DAYS - 10L,
                    nThread = n_threads(4L),
                    PolicyPars = set_policypars(workplaces_open = 0.5,
                                                workplace_size_max = 50))
  expect_true(hasName(S[[2]], "V35"))
})

test_that(paste(as.character(Sys.time()), "a_household_infections"), {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if(is32bit())
  skip_if_not(identical(.Platform$r_arch, "x64"))
  library(data.table)
  # Policy to expose household effects
  PolicyH <- set_policypars(do_contact_tracing = FALSE,
                            supermarkets_open = FALSE,
                            cafes_open = FALSE,
                            workplaces_open = FALSE)


  withr::with_seed(10, {
    SH000 <- simulate_sa2(40,
                          Policy = PolicyH,
                          EpiPars = set_epipars(a_household_rate = 0.00,
                                                q_household = 1),
                          returner = 1,
                          .first_day = "2020-05-01",
                          unseen_infections = rep(100L, 40),
                          overseas_arrivals = integer(40))
  })
  withr::with_seed(10, {
    SH005 <- simulate_sa2(40,
                          Policy = PolicyH,
                          EpiPars = set_epipars(a_household_rate = 0.05,
                                                q_household = 1),
                          returner = 1,
                          .first_day = "2020-05-01",
                          unseen_infections = rep(100L, 40),
                          overseas_arrivals = integer(40))
  })
  withr::with_seed(10, {
    SH025 <- simulate_sa2(40,
                          Policy = PolicyH,
                          EpiPars = set_epipars(a_household_rate = 0.25,
                                                q_household = 1),
                          returner = 1,
                          .first_day = "2020-05-01",
                          unseen_infections = rep(100L, 40),
                          overseas_arrivals = integer(40))
  })

  s000 <- SH000[Status == "Suscep"][["N"]]
  s005 <- SH005[Status == "Suscep"][["N"]]
  s025 <- SH025[Status == "Suscep"][["N"]]

  expect_lt(mean(tail(s000, 10) < tail(s005, 10)), 0.5)
  expect_lt(mean(tail(s005, 10) < tail(s025, 10)), 0.5)

})


test_that(paste(as.character(Sys.time()), "returner 3 no race condition"), {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(is64bit())
  library(data.table)
  S <- simulate_sa2(10, nThread = parallel::detectCores() - 1L, returner = 3)
  pop <- data.table(S3 = S$Status12)[, Day := rep_each(1:10, .N)][, .(N3 = sum(S3)), by = .(Day)]
  expect_true(is_constant(pop[["N3"]]))
  expect_equal(pop[["N3"]][1], fst_rows("australia.fst"))
})


test_that(paste(as.character(Sys.time()), "workplaces/schools infect"), {
  skip_if_not(is64bit())
  skip_on_travis()
  skip_if_not_installed('covr')
  library(hutilscpp)

  DAYS_TO_SIM <- 60L

  for (pw in c(0.5, 0.6, 0.7, 0.9, 1.0)) {
    if (covr::in_covr() && pw != 0.6) {
      next
    }
    W <- simulate_sa2(DAYS_TO_SIM,
                      .first_day = "2020-03-23",
                      returner = 4,
                      PolicyPars = set_policypars(supermarkets_open = FALSE,
                                                  cafes_open = FALSE,
                                                  do_contact_tracing = FALSE,
                                                  schools_open = FALSE,
                                                  workplaces_open = pw,
                                                  workplace_size_max = 2e9),
                      EpiPars = set_epipars(incubation_distribution = "dirac",
                                            incubation_mean = 100,
                                            q_workplace = pw,
                                            q_household = 0),
                      overseas_arrivals = integer(DAYS_TO_SIM))
    WSources <- .subset2(W, "InfectionSource")

    # Implementation detail
    expect_equal(source_workplace(), 19L)
    # Assumes source_workplace
    expect_gt(which_first(WSources == 19L), 0L)
  }
  clear_dataEnv()

  new_infected <- integer(6)
  i <- 1L
  for (ps in c(0.5, 0.6, 0.7, 0.9, 1.0)) {
    if (covr::in_covr() && ps != 0.9) {
      next
    }
    S <-
      simulate_sa2(DAYS_TO_SIM,
                   .first_day = "2020-04-23",
                   returner = 4,
                   PolicyPars = set_policypars(supermarkets_open = TRUE,
                                               cafes_open = FALSE,
                                               do_contact_tracing = FALSE,
                                               schools_open = TRUE,
                                               workplaces_open = 0,
                                               workplace_size_max = 1,
                                               lockdown_triggers__schools = set_lockdown_triggers__schools(FALSE)),
                   EpiPars = set_epipars(incubation_distribution = "dirac",
                                         incubation_mean = 100,
                                         q_workplace = 0,
                                         a_schools_rate = ps,
                                         q_school = ps/100,
                                         q_supermarket = 1/10000,
                                         q_household = 1),
                   overseas_arrivals = integer(DAYS_TO_SIM))
    SSources <- .subset2(S, "InfectionSource")
    new_infected[i] <- sum(S$InfectedOn > yday("2020-04-23"), na.rm = TRUE)
    i <- i + 1L
    # Implementation detail
    expect_equal(source_school(), 20L)
    expect_gt(which_first(SSources == 20L), 0L)

    # The more likely school transmission, the more infections

  }
  if (!covr::in_covr()) {
    expect_false(do_is_unsorted_pint(new_infected))
  }
})

test_that(paste(as.character(Sys.time()), "other SA2"), {
  skip_if_not(is64bit())
  S <- simulate_sa2(40,
                    returner = 4L,
                    PolicyPars = set_policypars(supermarkets_open = FALSE,
                                                cafes_open = FALSE,
                                                travel_outside_sa2 = TRUE),
                    EpiPars = set_epipars(q_supermarket = 0.001,
                                          incubation_distribution = 'dirac',
                                          incubation_mean = 50))
  expect_true(source_other_sa2() %in% S$InfectionSource)
})


test_that(paste(as.character(Sys.time()), "contact tracing tests can be capped"), {
  skip_if_not(is64bit())
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  skip_if_not_installed("magrittr")
  skip_if_not_installed("covr")
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
      "ACT",    2000,         0,     0,       1,
      "OTH",     100,         0,     0,       0)
  library(magrittr)
  library(data.table)
  each. <- if (identical(Sys.getenv("TRAVIS"), "true") || covr::in_covr()) 1L else 5L

  DAYS_TO_SIM <- 47L

  S_by_tests <-
    lapply(rep(c(10L, 1000L), each = each.), function(tests) {
      withr::with_seed(1, {
        simulate_sa2(DAYS_TO_SIM,
                     returner = 2,
                     .first_day = "2020-06-01",
                     InitialStatus = manual_initial_status,
                     EpiPars = set_epipars(q_supermarket = 1/1000,
                                           incubation_distribution = "dirac",
                                           incubation_mean = 8L),
                     PolicyPars = set_policypars(tests_by_state = rep(tests, 10L),
                                                 cafes_open = FALSE),
                     unseen_infections = integer(DAYS_TO_SIM),
                     overseas_arrivals = integer(DAYS_TO_SIM)) %>%
          .[, n_tests := tests] %>%
          .[]
      })
    }) %>%
    rbindlist(idcol = "id")

  S_by_tests %>%
    .[, id2 := (id - 1L) %% each.] %>%
    .[]

  if (covr::in_covr()) {
    S_by_tests %>%
      .[Day %in% c(24L, 25L, 45L)] %>%
      .[State == "ACT" & Status == "Isolated"] %>%
      .[, expect_lte(first(N), last(N),
                     label = paste0("@id,Day ", .BY[["id2"]], ",", .BY[["Day"]],
                                    " = ", first(N))),
        by = .(id2, Day)]
  } else {
    S_by_tests %>%
      .[Day >= 24] %>%
      .[State == "ACT" & Status == "Isolated"] %>%
      .[, expect_lte(first(N), last(N),
                     label = paste0("@id,Day ", .BY[["id2"]], ",", .BY[["Day"]],
                                    " = ", first(N))),
        by = .(id2, Day)]
  }
  clear_dataEnv()

})

test_that(paste(as.character(Sys.time()), "age-based lockdown"), {
  skip_if_not(is64bit())
  skip_if_not_installed("data.table")
  library(data.table)
  S <- simulate_sa2(4,
                    PolicyPars = set_policypars(age_based_lockdown = 65:100,
                                                do_contact_tracing = FALSE),
                    EpiPars = set_epipars(q_supermarket = 0.1))
  a <- S$Statuses[, sum_le_eq(Age, 65L, V2, 32L)]

  #
  expect_equal(a, 0)
})

test_that(paste(as.character(Sys.time()), "Multipolicy-historical"), {
  skip_if_not(is64bit())
  skip_if_not_installed("tibble")
  manual_initial_status <-
    tibble::tribble(
      ~state, ~active, ~critical, ~dead, ~healed,
      "NSW",     0,         9,     6,      30,
      "VIC",     0,         5,     4,      20,
      "QLD",     0,         2,     3,      10,
      "SA",      0,         0,     0,      10,
      "WA",      0,         0,     0,      10,
      "TAS",     0,         0,     0,       1,
      "NT",      0,         0,     0,       3,
      "ACT",  8000,         0,     0,       1,
      "OTH",    00,         0,     0,       0)
  S <- simulate_sa2(19,
                    returner = 3,
                    InitialStatus = manual_initial_status,
                    EpiPars = set_epipars(q_school = 1,
                                          a_schools_rate = 1,
                                          incubation_mean = 100,
                                          incubation_distribution = "dirac",
                                          a_workplace_rate = 1,
                                          a_household_rate = 1,
                                          q_workplace = 0.001,
                                          p_asympto = 1,
                                          supermarket_beta_shape1 = 3,
                                          supermarket_beta_shape2 = 3,
                                          ResistanceByAge = rep(0, 101),
                                          q_supermarket = 3/10000),
                    # Policy = set_policypars(schools_open = TRUE,
                    #                         school_days_per_wk = 5L),
                    MultiPolicy = "historical",
                    .first_day = "2020-05-25")
  # very basic: we have school infections
  expect_true(source_school() %in% S$InfectionSource)
})

test_that(paste(as.character(Sys.time()), "early return"), {
  skip_if_not(is64bit())
  skip_if_not_installed("data.table")
  library(data.table)
  options(datatable.auto.index = FALSE)
  options(datatable.use.index = FALSE)
  DAYS_TO_SIM <- 45
  S <- simulate_sa2(DAYS_TO_SIM,
                    EpiPars = set_epipars(incubation_mean = 2,
                                          illness_mean = 5),
                    returner = 1L,
                    .first_day = "2020-06-01",
                    PolicyPars = set_policypars(supermarkets_open = FALSE,
                                                cafes_open = FALSE),
                    overseas_arrivals = integer(DAYS_TO_SIM))
  expect_equal(S[Day == DAYS_TO_SIM, .(Status, N)][Status %in% c("NoSymp", "InSymp"), sum(N)], 0L)
  S <- NULL
})

test_that(paste(as.character(Sys.time()), "workplace caps bind"), {
  skip_if_not(is64bit())
  skip_if_not_installed("tibble")
  skip_if_not_installed("hutilscpp")
  library(hutilscpp)
  skip_if_not_installed("data.table")
  library(data.table)
  options(datatable.auto.index = FALSE)
  options(datatable.use.index = FALSE)
  skip_on_travis()  # too much memory required
  skip_on_cran()  # nThread
  aus <- read_typical()
  aus[, Status := 0L]
  aus[which_first(nColleagues == 11L), Status := 1L]
  aus[which_first(nColleagues == 5L), Status := 1L]
  aus[or3s(Status == 1L), InfectedOn := yday("2020-06-01")]
  aus[, Incubation := 10L]
  aus[, Illness := 10L]

  S <- simulate_sa2(55,
                    returner = 0,
                    .first_day = "2020-06-02",
                    InitialStatus = manual_initial_status,
                    EpiPars = set_epipars(q_workplace = 1,
                                          a_workplace_rate = 1,
                                          q_supermarket = 0,
                                          incubation_mean = 25,
                                          ResistanceByAge = rep(0, 101),
                                          incubation_distribution = "dirac"),
                    Policy = set_policypars(workplaces_open = 1,
                                            workplace_size_max = 7,
                                            do_contact_tracing = FALSE),
                    nThread = 10,
                    myaus = aus)
  sw <- source_workplace()
  expect_equal(S$Statuses[and3s(nColleagues >= 10, Source == sw), .N], 0)
  expect_gt(S$Statuses[and3s(nColleagues < 6, Source == sw), .N], 0)
})


test_that(paste(as.character(Sys.time()), "only_Year12"), {
  skip_if_not(is64bit())
  withr::with_seed(53, {
    S <- simulate_sa2(8,
                      returner = 4,
                      .first_day = as.Date("2020-04-09"),
                      PolicyPars = set_policypars(supermarkets_open = FALSE,
                                                  schools_open = TRUE,
                                                  only_Year12 = TRUE,
                                                  school_days_per_wk = c("NSW" = 3L),
                                                  do_contact_tracing = FALSE,
                                                  cafes_open = FALSE),
                      EpiPars = set_epipars(incubation_mean = 24,
                                            incubation_distribution = "lnorm",
                                            a_schools_rate = 1,
                                            q_school = 1/2))
  })


  expect_true(source_school() %in% S$InfectionSource)
})


test_that(paste(as.character(Sys.time()), "major events (0)"), {
  skip_if_not(is64bit())
  S <- simulate_sa2(25,
                    .first_day = "2020-03-30",
                    PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                cafes_open = FALSE,
                                                do_contact_tracing = FALSE,
                                                max_persons_per_event = 100e3,
                                                nPersonsByEvent = c(10e3L, 5e3L),
                                                n_major_events_weekday = 0L,
                                                n_major_events_weekend = 10L),
                    EpiPars = set_epipars(q_supermarket = 0.005,
                                          q_major_event = 0.001),
                    returner = 4)
  expect_true(source_stadia() %in% S$InfectionSource)
})
