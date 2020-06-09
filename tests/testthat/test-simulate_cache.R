test_that(paste(as.character(Sys.time()), "simulate_cache validates and invalidates"), {
  skip_on_cran()
  skip_if(is32bit())
  skip_if_not_installed("withr")
  skip_if_not_installed("tibble")

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
      "ACT",       0,         0,     0,       1)



  withr::with_seed(430L, {
    SByState <- simulate_sa2(5L, returner = 1L, use_dataEnv = TRUE, .first_day = "2020-04-01")
    SInitial <- simulate_sa2(5L, returner = 1L,
                             InitialStatus = manual_initial_status,
                             .first_day = "2020-04-01",
                             use_dataEnv = TRUE)

    # dead < critical

    expect_false(SByState$N[2] < SByState$N[6])
    expect_true(SInitial$N[2] < SInitial$N[5])

    SN6_first_run <- SInitial$N[6]
  })

  SByState <- SInitial <- NULL

  skip("Not yet reproducible.")

  withr::with_seed(430L, {
    SByState <- simulate_sa2(5L, returner = 1L, use_dataEnv = TRUE)
    SInitial <- simulate_sa2(5L, returner = 1L,
                             InitialStatus = list(dead = 0L,
                                                  healed = 0L,
                                                  active = 50e3L,
                                                  critical = 10e3L),
                             by_state = FALSE,
                             use_dataEnv = TRUE)
    SN6_second_ruin <- SInitial$N[6]

    # Test reproducible
    expect_equal(SN6_second_ruin, SN6_first_run)
  })
})
