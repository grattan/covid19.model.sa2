test_that("set_initial_by_state works", {
  A <- set_initial_by_state(1L, .population = 10e3)
  expect_equal(sum(A), 10e3)
  B <-
  withr::with_seed(55L, {
    set_initial_by_state(1L,
                         dead = 10L,
                         healed = 100L,
                         active = 400L,
                         critical = 10L,
                         .population = 10e3)
  })
  expect_true(sum(B == -2) == 12L)
  expect_true(sum(B == -1) == 111L)

})
