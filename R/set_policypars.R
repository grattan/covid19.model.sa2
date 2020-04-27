#' Set policy parameters
#' @description Used to supply policy parameters to the main
#' \code{\link{simulate_sa2}} function,
#' with defaults.
#'
#' @param supermarkets_open \code{TRUE|FALSE}. Should supermarkets remain open?
#' @param schools_open \code{FALSE|TRUE}. Should schools remain open?
#' @param only_Year12 \code{FALSE|TRUE}. If schools open, should they be
#' restricted to Year 12 students only? No effect if \code{schools_open = FALSE}.
#' @param do_contact_tracing \code{TRUE|FALSE}. Should contact tracing occur?
#' If \code{FALSE} households are not isolated if tested.
#' @param contact_tracing_days_before_test The number of days following
#' the end of the incubation period
#' before the person gets tested.
#' @param contact_tracing_days_until_result The number of days between a test
#' and the result being known.
#' @param contact_tracing_only_sympto \code{TRUE|FALSE} Is contact tracing only
#' applied to symptomatic cases?
#'
#'
#' @param tests_by_state \code{integer(10)} The number of tests per day that states
#' perform. First entry is the total tests available across Australia.
#' If any entry is negative,
#' or \code{NA}, the most recent day's number of tests performed are
#' used.
#'
#' @return A list of the components.
#'
#' @export
#'







set_policypars <- function(supermarkets_open = TRUE,
                           schools_open = FALSE,
                           only_Year12 = FALSE,
                           do_contact_tracing = TRUE,
                           contact_tracing_days_before_test = 0L,
                           contact_tracing_days_until_result = 3L,
                           contact_tracing_only_sympto = TRUE,
                           tests_by_state = NULL,
                           max_persons_per_event = 5L,
                           max_persons_per_supermarket = 200L) {
  checkmate::assert_logical(supermarkets_open,
                            any.missing = FALSE,
                            len = 1L)
  checkmate::assert_logical(schools_open,
                            any.missing = FALSE,
                            len = 1L)
  checkmate::assert_logical(only_Year12,
                            any.missing = FALSE,
                            len = 1L)
  checkmate::assert_logical(do_contact_tracing,
                            any.missing = FALSE,
                            len = 1L)

  tests_by_state <- .fix_tests_by_state(tests_by_state)
  if (!missing(max_persons_per_event)) {
    .NotYetUsed("max_persons_per_event")
  }
  if (!missing(max_persons_per_supermarket)) {
    .NotYetUsed("max_persons_per_supermarket")
  }


  mget(ls())
}


.fix_tests_by_state <- function(tests_by_state) {
  # First check whether it's fine
  if (checkmate::test_integerish(tests_by_state,
                                 lower = 0,
                                 upper = .Machine$integer.max,
                                 len = 10,
                                 any.missing = FALSE)) {
    return(as.integer(tests_by_state))
  }


  out <- integer(10)  # will be zero for every 'non state'

  if (is.null(tests_by_state) ||
      anyNA(tests_by_state) ||
      min(tests_by_state) < 0) {
    time_series_tests <- read_sys("time_series_tests.fst")
    last_tests <- last(time_series_tests[, lapply(.SD, diff)])
    states <- c("AUS", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")

    for (i in seq_along(out)) {
      # honour non-NA entries
      if (is.numeric(tests_by_state) &&
          length(tests_by_state) == 1 &&
          !anyNA(tests_by_state)) {
        # Assume to be Australia
        if (is.double(tests_by_state)) {
          if (tests_by_state < 0) {
            next
          }
          if (tests_by_state > .Machine$integer.max) {
            out[1] <- .Machine$integer.max
          } else {
            out[1] <- as.integer(tests_by_state)
          }
        }
        out[1] <- tests_by_state
        next
      }

      if (hasName(last_tests, states[i])) {
        out[i] <- last_tests[[states[i]]]
      }
    }
    if (is.na(out[1]) || out[1] == 0) {
      # Note even if out[1] == 0 is intended,
      # it is only allowed if all of out is zero.
      out[1] <- sum(out[-1])
    }
  }
  if (length(tests_by_state) == 9) {
    # Assume it doesn't include Australia -- shove to the front
    out <- c(sum(out), out)
  }
  out
}

