test_that("set_policy works with states", {
  for (d in 1:5) {
    out <- set_policypars(school_days_per_wk = list(NSW = d))
    expect_equal(out$NSW[1], d)
  }
})
