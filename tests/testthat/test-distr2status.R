test_that("distr2status works", {
  ans <- distr2status(N = 1e6L,
                      dead = 10L,
                      healed = 20L,
                      active = 30L,
                      critical = 40L)
  expect_equal(sum(ans == status_critic()), 40L)
})
