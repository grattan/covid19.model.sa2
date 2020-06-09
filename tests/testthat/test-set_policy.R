test_that(paste(as.character(Sys.time()), "set_policy works with states"), {
  for (d in 1:5) {
    out <- set_policypars(school_days_per_wk = list(NSW = d))
    expect_equal(dollars(out, school_days_per_wk, NSW)[1], d)
  }
})
