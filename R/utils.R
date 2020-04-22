

# Non-loud
samp <- function(x, size = length(x), replace = size > length(x), loud = FALSE, prob = NULL) {
  hutils::samp(x, size, replace, loud, prob)
}

# weighted sample (rather than prob)
wsamp <- function(x, size, w) {
  probs <- w / sum(w)
  samp(x, size = size, prob = probs)
}

isnt_testing <- function() {
  !requireNamespace("testthat", quietly = TRUE) || !testthat::is_testing()
}
