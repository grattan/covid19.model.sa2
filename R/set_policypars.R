#' Set policy parameters
#' @description Used to supply policy parameters to the main
#' \code{\link{simulate_sa2}} function,
#' with defaults.
#'
#' @param supermarkets_open \code{TRUE|FALSE}. Should supermarkets remain open?
#' @param schools_open \code{FALSE|TRUE}. Should schools remain open?
#' @param only_Year12 \code{FALSE|TRUE}. If schools open, should they be
#' restricted to Year 12 students only? No effect if \code{schools_open = FALSE}.
#' @param school_days_per_wk Specifies how many days a week pupils attend
#' school. By default, students attend full-time. Only applied after
#' \code{schools_open} and \code{only_Year12}. In particular, has no
#' effect if \code{schools_open = FALSE} and if \code{only_Year12 = TRUE} then
#' all other students attend 0 times per week.
#'
#' If a single number, applies to all states. Otherwise must be a
#' named list or vector.
#'
#' If a named vector, names must be state names.
#' (See \code{\link{states}}.) Omitted states take the value 5 (i.e.
#' full-time).
#'
#' If a named list, each element must be a length-21 vector which
#' specifies the number of times per week students of each age 0-20
#' attend in the given state (with the last
#' element being the attendance per week of teachers).'
#'
#'
#'
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
#'
#' @param max_persons_per_supermarket Maximum number of people allowed in a
#' supermarket (within one hour i.e. concurrently).
#' @param max_persons_per_event Not yet used.
#'
#' @param workplaces_open Are workplaces to be open?
#' Can be logical \code{FALSE} or \code{TRUE} or a number in \code{[0, 1]},
#' the proportion of workplaces that remain open.
#' @param workplace_size_max The maximum size of any workplace (we assume
#' that everyone interacts on a single day).
#' @param workplace_size_beta,workplace_size_lmu,workplace_size_lsi
#' Parameters for the distribution of workplace sizes. \code{_beta} is the
#' rate distribution for the geometric distribution; \code{_lmu} and
#' \code{_lsi} are the parameters for the lognormal distribution.
#'
#' @return A list of the components.
#'
#' @export
#'







set_policypars <- function(supermarkets_open = TRUE,
                           schools_open = FALSE,
                           only_Year12 = FALSE,
                           school_days_per_wk = NULL,
                           do_contact_tracing = TRUE,
                           contact_tracing_days_before_test = 0L,
                           contact_tracing_days_until_result = 3L,
                           contact_tracing_only_sympto = TRUE,
                           tests_by_state = NULL,
                           max_persons_per_event = 5L,
                           max_persons_per_supermarket = 200L,
                           workplaces_open = FALSE,
                           workplace_size_max = 1L,
                           workplace_size_beta = 13,
                           workplace_size_lmu = -1,
                           workplace_size_lsi = -1) {
  checkmate::assert_logical(supermarkets_open,
                            any.missing = FALSE,
                            len = 1L)
  checkmate::assert_logical(schools_open,
                            any.missing = FALSE,
                            len = 1L)
  checkmate::assert_logical(only_Year12,
                            any.missing = FALSE,
                            len = 1L)

  school_days_per_wk <- .fix_school_days_per_wk(school_days_per_wk)


  checkmate::assert_logical(do_contact_tracing,
                            any.missing = FALSE,
                            len = 1L)
  checkmate::assert_int(contact_tracing_days_until_result)
  checkmate::assert_int(contact_tracing_days_before_test)

  tests_by_state <- .fix_tests_by_state(tests_by_state)
  if (!missing(max_persons_per_event)) {
    .NotYetUsed("max_persons_per_event")
  }

  workplaces_open <- as.double(workplaces_open)
  checkmate::assert_number(workplaces_open, any.missing = FALSE, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_int(workplace_size_max, lower = 0L, finite = TRUE)
  checkmate::assert_number(workplace_size_beta, finite = TRUE)
  checkmate::assert_number(workplace_size_lmu, finite = TRUE)
  checkmate::assert_number(workplace_size_lsi, finite = TRUE)

  workplaces_open <- as.integer(workplaces_open * 1000)



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

      if (hasName(last_tests, states()[i])) {
        out[i] <- last_tests[[states()[i]]]
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


.fix_school_days_per_wk <- function(x) {
  NAGES <- 21L

  # Combinations of weekdays for school days
  week15combns <- lapply(1:5, combn, x = 5L)

  # 5 -> number of days in a week, 20 -> number of 'ages' in a school (with 20 = teacher)
  # Note age is zero-based
  # simplify = FALSE to give a named list
  out <- sapply(states(), function(s) rep(5L, NAGES), simplify = FALSE)
  # Must be after
  out[["week15combns"]] <- week15combns
  out[["all_full_time"]] <- FALSE

  if (is.null(x)) {
    out[["all_full_time"]] <- TRUE
    return(out)
  }
  # If a single number (but not a named vector) then assume all
  # states are days_per_wk
  if (is.atomic(x) && length(x) == 1L && is.null(names(x))) {
    x <- checkmate::assert_int(x, lower = 0, upper = 5, coerce = TRUE)
    out <- sapply(states(), function(s) rep(x, NAGES), simplify = FALSE)
    out[["week15combns"]] <- week15combns
    out[["all_full_time"]] <- (x == 5L)
    return(out)
  }

  if (length(x) > 180) {
    stop("`length(", checkmate::vname(x), ") = ", length(x), "`, which is not ",
         "supported. ")
  }

  # Is the input referring to states?
  # A: Yes if any states are in the names of x.  If x has no names, then
  # the following is FALSE.
  if (any(states() %in% names(x))) {
    for (s in states()) {
      if (hasName(x, s)) {
        xs <- x[[s]]
        if (!is.atomic(xs)) {
          stop(glue("Element '{s}' of `{vname(x)}` was not atomic.",
                    "`{vname(x)}` must be a named vector or named ",
                    "list of atomic integers."))
        }

        if (length(xs) != 1L && length(xs) != NAGES) {
          stop(glue("`{vname(x)}` contained an element",
                    "of length {length(xs)} for state '{s}'. ",
                    "The only permissible lengths for state subelements are 1 or {NAGES}."))
        }

        if (anyNA(xs)) {
          stop(glue("`{vname(x)}` contained an element",
                    "with missing values for state '{s}'. ",
                    "Only integer values from 0 to 5 are permitted for states' days per week."))
        }
        if (min(xs) < 0 || max(xs) > 5) {
          stop(glue("`{vname(x)}` contained an element",
                    "with values outside [0, 5]  for state '{s}'. ",
                    "Only integer values from 0 to 5 are permitted for states' days per week."))
        }
        if (is.double(xs) && any(xs != as.integer(xs))) {
          stop(glue("`{vname(x)}` contained an element",
                    "with non-integer values for state '{s}'. ",
                    "Only integer values from 0 to 5 are permitted for states' days per week."))
        }

        out[[s]] <- rep_len(as.integer(xs), NAGES)
      }
      return(out)
    }
  }
  warning(glue("`{vname(x)}` was not in a recognized format and will be ignored."))

  out
}





