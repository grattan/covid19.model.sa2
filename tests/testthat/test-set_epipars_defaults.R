test_that("set_epipars_defaults works", {
  ans <- set_epipars_defaults(list())
  expect_true(all(c("CHECKED", "asympto", "duration_active", "lambda_infectious",
                    "cau_l", "cau_s") %in% names(ans)))

  ans1 <- set_epipars_defaults(asympto = 0.5)
  expect_equal(length(ans1), length(ans))
  expect_equal(ans1[["asympto"]], 0.5)
})
