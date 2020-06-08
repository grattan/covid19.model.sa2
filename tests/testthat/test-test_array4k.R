test_that("test_array4k works", {
  skip_if_not_installed("data.table")
  library(data.table)

  DT <- CJ(a = 0:10, b = 0:12, c = 0:16, d = 0:18)
  DT[, i_true := .I - 1L]
  DT[, i_test := test_array4k(a, b, c, d, 11L, 13L, 17L, 19L)]
  expect_identical(DT[["i_true"]], DT[["i_test"]])

  expect_error(test_array4k(w = integer(101),
                            x = integer(103),
                            y = integer(107),
                            z = integer(10),
                            nw = 1e6,
                            nx = 1e3,
                            ny = 1e1,
                            nz = 1e2),
               regexp = "only available for integer.length")

})
