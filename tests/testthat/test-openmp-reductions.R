
if (!("testthat" %in% .packages())) {
  library(testthat)
  library(covid19.model.sa2)
}

test_that("openmp reduces with count_by", {
  skip_if_not(.Platform$GUI == "RTerm")
  library(data.table)

  DT <- data.table(x = rep_len(sample(0:2309, size = 50e6, replace = TRUE), 333e6),
                   y = rep_len(sample(0:100, size = 10e6, replace = TRUE), 333e6))
  DT[, z := rep_len(0:11, .N)]

  counts <- DT[, count_by(x, y, 10)]




  cat("\n\n",
      DT[x == 0 & y == 0, .N],
      DT[x == 0 & y == 1, .N],
      DT[x == 0 & y == 2, .N],
      DT[x == 0 & y == 3, .N],
      DT[x == 0 & y == 4, .N], "\n\n")
  cat("\n\n", head(counts), "\n\n")

  expect_true(TRUE)

})
