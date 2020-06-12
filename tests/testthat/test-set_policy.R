test_that(paste(as.character(Sys.time()), "set_policy works with states"), {
  for (d in 1:5) {
    out <- set_policypars(school_days_per_wk = list(NSW = d))
    expect_equal(dollars(out, school_days_per_wk, NSW)[1], d)
  }
})

test_that(paste(as.character(Sys.time()), "set_multipolicy"), {
  expect_equal(set_multipolicy(NULL), set_multipolicy(.first_yday = "2020-01-01"))
})

test_that(paste(as.character(Sys.time()), "update_policy"), {
  P <- set_policypars(supermarkets_open = TRUE, only_Year12 = FALSE)
  PP <- update_policypars(P, supermarkets_open = FALSE, only_Year12 = TRUE)
  expect_false(P$only_Year12)
  expect_true(PP$only_Year12)
})

test_that(paste(as.character(Sys.time()), "error handling"), {
  expect_error(set_policypars(school_days_per_wk = integer(500)),
               regexp = "length(school_days_per_wk) = 500",
               fixed = TRUE)
  expect_error(set_policypars(school_days_per_wk = list(NSW = list())),
               regexp = "not atomic",
               fixed = TRUE)
  expect_error(set_policypars(school_days_per_wk = list(NSW = integer(500))),
               regexp = "length 500",
               fixed = TRUE)
  expect_error(set_policypars(school_days_per_wk = list(NSW = c(NA_integer_))),
               regexp = "missing value",
               fixed = TRUE)
  expect_error(set_policypars(school_days_per_wk = list(NSW = c(6))),
               regexp = "[0, 5]",
               fixed = TRUE)
  expect_error(set_policypars(school_days_per_wk = list(NSW = c(2.5))),
               regexp = "non-integer",
               fixed = TRUE)
  expect_warning(set_policypars(school_days_per_wk = list(a = 1)),
                 regexp = "not in a recognized format")

  expect_error(set_policypars(age_based_lockdown = c(NA, 1:100)),
               regexp = "missing value")
  expect_error(set_policypars(age_based_lockdown = c(-1:101)),
               regexp = "unfixable")

})






