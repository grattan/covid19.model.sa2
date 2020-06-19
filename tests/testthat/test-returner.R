test_that(paste(as.character(Sys.time()), "returners"), {
  skip_if_not(is64bit())
  library(data.table)
  # Do a large one to test everything
  SR1 <- simulate_sa2(41,
                      PolicyPars = set_policy_no_restrictions(),
                      .first_day = yday("2020-03-25"),
                      returner = 1L)
  expect_equal(SR1[, sum(N)], 41 * fst_rows("australia.fst"))
  SR1 <- NULL
  SR2 <- simulate_sa2(41,
                      PolicyPars = set_policy_no_restrictions(),
                      .first_day = yday("2020-03-25"),
                      returner = 2L)
  expect_equal(SR1[, sum(N)], 41 * fst_rows("australia.fst"))
 })
