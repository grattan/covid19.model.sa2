test_that(paste(as.character(Sys.time()), "Able to model partial return to school"), {
  skip_if_not(identical(.Platform$r_arch, "x64"))  # i386 too small memory-wise


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
     simulate_sa2(20,
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
    simulate_sa2(20,
                 PolicyPars = PolicyNSW1DayAWk_B,
                 nThread = 2)[[1]]
  expect_true(is.numeric(NSWPupils1DayAWeek_A))

  # schools days default is 7
  PolicyNSW1DayAWk_C <-
    set_policypars(schools_open = TRUE,
                   only_Year12 = TRUE,
                   school_days_per_wk = c(NSW = 1L))

  expect_true(is.numeric(NSWPupils1DayAWeek_B))

  NSWPupils1DayAWeek_C <-
    simulate_sa2(20,
                 PolicyPars = PolicyNSW1DayAWk_C,
                 nThread = 2)[[1]]

  # Just a simple test to show completion


  expect_true(is.numeric(NSWPupils1DayAWeek_C))

})

test_that(paste(as.character(Sys.time()), "coverage of school attendance"), {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  library(data.table)
  no_school_ld <- set_lockdown_triggers__schools(do_school_lockdown = FALSE)

  withr::with_seed(28, {
    S1 <- simulate_sa2(100,
                       returner = 1,
                       .first_day = "2020-05-01",
                       EpiPars = set_epipars(q_school = 1/100),
                       PolicyPars = set_policypars(schools_open = TRUE,
                                                   do_contact_tracing = FALSE,
                                                   school_days_per_wk = 1L,
                                                   lockdown_triggers__schools = no_school_ld))
  })
  withr::with_seed(28, {
    S4 <- simulate_sa2(100,
                       returner = 1,
                       .first_day = "2020-05-01",
                       EpiPars = set_epipars(q_school = 1/100),
                       PolicyPars = set_policypars(schools_open = TRUE,
                                                   do_contact_tracing = FALSE,
                                                   school_days_per_wk = 4L,
                                                   lockdown_triggers__schools = no_school_ld))
  })
  merge.data.table(S1, S4, by = c("Day", "Status")) %>%
    .[Status == "NoSymp"] %>%
    tail %>%
    .[, expect_lte(N.x, N.y), by = .(Day)]
})
