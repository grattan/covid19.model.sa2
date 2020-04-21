

# Non-loud
samp <- function(x, size = length(x), replace = size > length(x), loud = FALSE, prob = NULL) {
  hutils::samp(x, size, replace, loud, prob)
}

isnt_testing <- function() {
  !requireNamespace("testthat", quietly = TRUE) || !testthat::is_testing()
}
