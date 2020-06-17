

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
  !requireNamespace("testthat", quietly = TRUE) || !testthat::is_testing()  # nocov
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
    stop(glue("{vname(x)} contained elements, that are not matched with {vname(table)}"))
  }
  out
}

is32bit <- function() {
  tryCatch(is.logical(seq_len(2^31 - 1)), error = function(e) TRUE)
}

is64bit <- function() {
  tryCatch(is.integer(seq_len(2^31 - 1)), error = function(e) FALSE)
}

which_last <- function(x) {
  for (i in length(x):1) {
    if (x[i]) {
      return(i)
    }
  }
  return(0L)
}

rep_each <- function(x, len) {
  rep(x, each = len / length(x), length.out = len)
}

g <- glue::glue


# nocov start

and3s <- function(...) {
  if ("and3s" %in% getNamespaceExports("hutilscpp")) {
    return(eval.parent(substitute(hutilscpp::and3s(...))))
  }
  hutilscpp::and3(...)
}

or3s <- function(...) {
  if ("or3s" %in% getNamespaceExports("hutilscpp")) {
    return(eval.parent(substitute(hutilscpp::or3s(...))))
  }
  hutilscpp::or3(...)
}
# nocov end


yday2date <- function(.yday) {
  stopifnot(is.integer(.yday), !anyNA(.yday), min(.yday) >= 0L)
  as.Date("2019-12-31") + .yday
}






