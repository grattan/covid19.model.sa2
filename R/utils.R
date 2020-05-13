

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

"%||%" <- function(a, b) if (is.null(a)) b else a

"%<=%" <- function(a, b) {
  if (is.null(a)) {
    eval.parent(substitute(a <- b))
  }
}

accel <- function(x, FUN) {
  FUN <- match.fun(FUN)
  if (length(x) <= 1L) {
    return(FUN(x))
  }
  setDT(list(x = x))[, "ans" := FUN(.BY[[1]]), by = "x"][["ans"]]
}

# put in hutils soon
ematch <- function(x, table) {
  # exists and match
  if (anyNA(out <- fastmatch::fmatch(x, table))) {
    stop(glue("{vname(x)} contained elements, no matched with {vname(table)}"))
  }
  out
}

is32bit <- function() {
  tryCatch(is.logical(seq_len(2^31 - 1)),
           error = function(e) {
             TRUE
           })
}

which_last <- function(x) {
  for (i in length(x):1) {
    if (x[i]) {
      return(i)
    }
  }
  return(0L)
}


