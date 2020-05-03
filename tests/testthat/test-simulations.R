test_that("multiplication works", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  library(data.table)
  DAYS <- 18L
  for (env_opt in c(getOption("covid19.model.sa2_useDataEnv", TRUE),
                    !getOption("covid19.model.sa2_useDataEnv", TRUE))) {
    for (thr in c(1L, 2L)) {
      expect_true(is.logical(env_opt))

      S0 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = FALSE),
                         use_dataEnv = env_opt)[["nInfected"]]
      S1 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 25),
                         EpiPars = set_epipars(r_supermarket_location = 1/20))[["nInfected"]]
      expect_true(is.atomic(S1))

      S2 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 40),
                         EpiPars = set_epipars(r_supermarket_location = 1/20))[["nInfected"]]
      expect_true(is.atomic(S2))

      S3 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     max_persons_per_supermarket = 40),
                         EpiPars = set_epipars(r_supermarket_location = 1/10))[["nInfected"]]
      expect_true(is.atomic(S3))


      S4 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     schools_open = TRUE,
                                                     only_Year12 = TRUE,
                                                     school_days_per_wk = 3L,
                                                     max_persons_per_supermarket = 45),
                         EpiPars = set_epipars(r_supermarket_location = 1/10))[["nInfected"]]

      if (!identical(thr, 1L)) {
        skip_on_cran()
        skip_on_travis()
      }

      S5 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     workplaces_open = 0.1,
                                                     schools_open = TRUE,
                                                     only_Year12 = FALSE,
                                                     school_days_per_wk = 3L,
                                                     max_persons_per_supermarket = 50),
                         EpiPars = set_epipars(r_supermarket_location = 1/10),
                         nThread = thr)[["nInfected"]]

      S6 <- simulate_sa2(DAYS,
                         PolicyPars = set_policypars(supermarkets_open = TRUE,
                                                     workplaces_open = 0.1,
                                                     schools_open = TRUE,
                                                     only_Year12 = FALSE,
                                                     school_days_per_wk = 5L,
                                                     max_persons_per_supermarket = 60),
                         EpiPars = set_epipars(r_supermarket_location = 1/10))[["nInfected"]]

      stopifnot(is.atomic(S0),
                is.atomic(S1),
                is.atomic(S2),
                is.atomic(S3),
                is.atomic(S4),
                is.atomic(S4))

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
