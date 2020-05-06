test_that("test_array4k works", {
  skip_if_not_installed("data.table")
  library(data.table)

  DT <- CJ(a = 0:10, b = 0:12, c = 0:16, d = 0:18)
  DT[, i_true := .I - 1L]
  DT[, i_test := test_array4k(a, b, c, d, 11L, 13L, 17L, 19L)]
  expect_identical(DT[["i_true"]], DT[["i_test"]])

})
