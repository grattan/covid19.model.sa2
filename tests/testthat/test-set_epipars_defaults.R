test_that("set_epipars works", {
  ans <- set_epipars()
  expect_true(all(c("CHECKED", "illness_distribution",
                    "illness_mean", "illness_sigma",
                    "incubation_distribution",
                    "incubation_mean", "incubation_sigma",
                    "p_asympto", "p_critical",
                    "p_death") %in% names(ans)))

  ans1 <- set_epipars(p_asympto = 0.5)
  expect_equal(length(ans1), length(ans))
  expect_equal(ans1[["p_asympto"]], 0.5)
})
