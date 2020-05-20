test_that("simulate_cache validates and invalidates", {
  skip_on_cran()
  withr::with_seed(430L, {
    SByState <- simulate_sa2(5L, returner = 1L, use_dataEnv = TRUE)
    SInitial <- simulate_sa2(5L, returner = 1L,
                             InitialStatus = list(dead = 0L,
                                                  healed = 0L,
                                                  active = 50e3L,
                                                  critical = 10e3L),
                             by_state = FALSE,
                             use_dataEnv = TRUE)

    # dead < critical

    expect_false(SByState$N[2] < SByState$N[6])
    expect_true(SInitial$N[2] < SInitial$N[6])

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
