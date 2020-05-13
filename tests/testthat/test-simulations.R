test_that("simulation works", {
  skip_on_cran()
  skip_if_not(identical(.Platform$r_arch, "x64"))
  skip_if_not_installed("data.table")
  library(data.table)
  DAYS <- 18L
  for (env_opt in c(getOption("covid19.model.sa2_useDataEnv", TRUE),
                    !getOption("covid19.model.sa2_useDataEnv", TRUE))) {
    for (thr in c(1L, 2L)) {
      expect_true(is.logical(env_opt))

      S0 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = FALSE,
                                                     do_contact_tracing = FALSE),
                         # Use a constant 100% attack rate for testing
                         EpiPars = set_epipars(a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0),
                         use_dataEnv = env_opt)[["nInfected"]]
      S1 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 25,
                                                     do_contact_tracing = FALSE),
                         EpiPars = set_epipars(r_supermarket_location = 1/20,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S1))

      S2 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 40),
                         EpiPars = set_epipars(r_supermarket_location = 1/20,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S2))

      S3 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 40),
                         EpiPars = set_epipars(r_supermarket_location = 1/10,
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
                         EpiPars = set_epipars(r_supermarket_location = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0))[["nInfected"]]
      expect_true(is.atomic(S4))

      if (!identical(thr, 1L)) {
        skip_on_cran()
        skip_on_travis()
      }
      invisible(gc())
      S5 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     workplaces_open = 0.1,
                                                     schools_open = TRUE,
                                                     only_Year12 = FALSE,
                                                     school_days_per_wk = 3L,
                                                     max_persons_per_supermarket = 50),
                         EpiPars = set_epipars(r_supermarket_location = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
                                               a_workplace_rate = 0),
                         nThread = thr)[["nInfected"]]

      S6 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     workplaces_open = 0.1,
                                                     schools_open = TRUE,
                                                     only_Year12 = FALSE,
                                                     school_days_per_wk = 5L,
                                                     max_persons_per_supermarket = 60),
                         EpiPars = set_epipars(r_supermarket_location = 1/10,
                                               a_household_rate = 0,
                                               a_schools_rate = 0,
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

test_that("prev segfaulting", {
  skip_on_cran()
  skip_if(is32bit())
  skip_if_not_installed("data.table")
  skip_if_not_installed("parallel")

  S <- simulate_sa2(50,
                    nThread = pmax(1L, parallel::detectCores() - 2L),
                    PolicyPars = set_policypars(workplaces_open = 1,
                                                workplace_size_max = 1000))
  expect_true(hasName(S[[2]], "V45")) # i.e. 'have we got here?'
  S <- simulate_sa2(40,
                    nThread = pmax(1L, parallel::detectCores() - 4L),
                    PolicyPars = set_policypars(workplaces_open = 0.5,
                                                workplace_size_max = 50))
  expect_true(hasName(S[[2]], "V35"))
})

test_that("a_household_infections", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not(identical(.Platform$r_arch, "x64"))
  library(data.table)
  SH000 <- simulate_sa2(40, EpiPars = set_epipars(a_household_rate = 0.00), returner = 1)
  SH005 <- simulate_sa2(40, EpiPars = set_epipars(a_household_rate = 0.05), returner = 1)
  SH025 <- simulate_sa2(40, EpiPars = set_epipars(a_household_rate = 0.25), returner = 1)
  s000 <- SH000[Status == "Suscep"][["N"]]
  s005 <- SH005[Status == "Suscep"][["N"]]
  s025 <- SH025[Status == "Suscep"][["N"]]

  expect_lt(mean(tail(s000, 10) <  tail(s005, 10)), 0.5)



})





