

test_that(paste(as.character(Sys.time()), "openmp reduces with count_by"), {
  skip_if_not(.Platform$GUI == "RTerm")

  # Determine 32 bit or 64 bit
  x <- tryCatch(is.logical(seq_len(2^31 - 1)),
                error = function(e) {
                  TRUE
                })
  if (isTRUE(x)) {
    skip("Unable to allocate memory.")
  }
  library(data.table)

  DT <- data.table(x = rep_len(sample(0:2309, size = 50e6, replace = TRUE), 333e6),
                   y = rep_len(sample(0:100, size = 10e6, replace = TRUE), 333e6))
  DT[, z := rep_len(0:11, .N)]

  counts <- DT[, count_by_sa2_age_status(x, y, z, 10)]

  and3s <- function(...) {
    if (requireNamespace("hutilscpp", quietly = TRUE) &&
        "and3s" %in% getNamespaceExports("hutilscpp")) {
      hutilscpp::and3s(..., .parent_nframes = 2L, nThread = parallel::detectCores())
    } else {
      hutilscpp::and3(...)
    }
  }





#
#   cat("\n\n",
#       DT[and3s(x == 0, y == 0, z == 0), .N],
#       DT[and3s(x == 0, y == 0, z == 1), .N],
#       DT[and3s(x == 0, y == 0, z == 2), .N],
#       DT[and3s(x == 0, y == 0, z == 3), .N],
#       DT[and3s(x == 0, y == 0, z == 4), .N], "\n\n")
#   cat("\n\n", head(counts), "\n\n")

  expect_true(TRUE)

})
