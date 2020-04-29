test_that("Able to model partial return to school", {
  states <- c("AUS", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT", "OTH")
  NAGES <- 21L

  # Test non-failure(!) and nThread > 1  (must be 2 by WRE policy)

  PolicyNSW1DayAWk_A <-
    set_policypars(schools_open = TRUE,
                   only_Year12 = TRUE,
                   school_days_per_wk = sapply(states,
                                               function(s) {
                                                 if (s == "NSW") {
                                                   rep(1L, NAGES)
                                                 } else {
                                                   rep(0L, NAGES)
                                                 }
                                               },
                                               simplify = FALSE))

  NSWPupils1DayAWeek_A <-
     simulate_sa2(50,
                  PolicyPars = PolicyNSW1DayAWk_A,
                  nThread = 2)[[1]]

  PolicyNSW1DayAWk_B <-
    set_policypars(schools_open = TRUE,
                   only_Year12 = TRUE,
                   school_days_per_wk = c(AUS = 0L,
                                          NSW = 1L,
                                          VIC = 0L,
                                          QLD = 0L,
                                          WA = 0L,
                                          SA = 0L,
                                          TAS = 0L,
                                          ACT = 0L,
                                          NT = 0L,
                                          OTH = 0L))

  NSWPupils1DayAWeek_B <-
    simulate_sa2(50,
                 PolicyPars = PolicyNSW1DayAWk_B,
                 nThread = 2)[[1]]

  # schools days default is 7
  PolicyNSW1DayAWk_C <-
    set_policypars(schools_open = TRUE,
                   only_Year12 = TRUE,
                   school_days_per_wk = c(NSW = 1L))

  NSWPupils1DayAWeek_C <-
    simulate_sa2(50,
                 PolicyPars = PolicyNSW1DayAWk_C,
                 nThread = 2)[[1]]

  # Just a simple test to show completion
  expect_true(is.numeric(NSWPupils1DayAWeek_A))
  expect_true(is.numeric(NSWPupils1DayAWeek_B))
  expect_true(is.numeric(NSWPupils1DayAWeek_C))

})
