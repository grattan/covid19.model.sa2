test_that("set_initial_by_state works", {
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


test_that("set_initial retrospectively", {
  cases <- read_sys("time_series_cases.fst")
  killed <- read_sys("time_series_deaths.fst")
  healed <- read_sys("time_series_recovered.fst")


})

