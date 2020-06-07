test_that("returners", {
  skip_if_not(is64bit())
  library(data.table)
  # Do a large one to test everything
  SR1 <- simulate_sa2(51,
                      PolicyPars = set_policy_no_restrictions(),
                      .first_day = yday("2020-03-25"),
                      returner = 1L)
  expect_equal(SR1[, sum(N)], 51 * fst_rows("australia.fst"))
  SR2 <- simulate_sa2(51,
                      PolicyPars = set_policy_no_restrictions(),
                      .first_day = yday("2020-03-25"),
                      returner = 2L)
  expect_equal(SR1[, sum(N)], 51 * fst_rows("australia.fst"))
 })
