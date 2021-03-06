#' Set policy parameters
#'
#' @name set_policypars
#'
#' @description Used to supply policy parameters to the main
#' \code{\link{simulate_sa2}} function,
#' with defaults.
#'
#' @param yday_start Optional. Specify the start date. Used in conjunction with
#' \code{MultiPolicy} argument to \code{simulate_sa2}.
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
#' @param contact_tracing_success \describe{
#' \item{\code{double(1) : [0, 1]}}{The proportion of contacts successfully traced.}
#' }
#'
#'
#' @param tests_by_state \code{integer(10)} The number of tests per day that states
#' perform. First entry is the total tests available across Australia.
#'
#' @param max_persons_per_supermarket Maximum number of people allowed in a
#' supermarket (within one hour i.e. concurrently).
#'
#' @param nPersonsByEvent The number of persons to attend each event.
#'
#' @param max_persons_per_event,n_major_events_weekday,n_major_events_weekend
#' Policies around major events. Maximum of 255 major events per day and
#' values of `max_persons_per_event` below 1000 are rounded to zero.
#'
#' @param cafes_open (TEMPORARY).
#' @param age_based_lockdown Integer vector of ages to lockdown. Either a vector
#' of the ages (0-100) or a length-101 vector specifying the ages to be lockdown
#' (as 1).
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
#' @param travel_outside_sa2 Should travel outside a person's SA2 be modelled?
#'
#' @param lockdown_triggers__schools
#' A list constructed by \code{\link{set_lockdown_triggers__schools}}.
#'
#' @param isol_compliance Chance of a person in isolation going about their
#' normal business while in isolation.
#'
#' @param ... Arguments passed to \code{set_policypars}.
#'
#'
#'
#' @return A list of the components.
#'
#' @export
#'







set_policypars <- function(yday_start = 0L,
                           supermarkets_open = TRUE,
                           schools_open = FALSE,
                           only_Year12 = FALSE,
                           school_days_per_wk = NULL,
                           do_contact_tracing = TRUE,
                           contact_tracing_days_before_test = 0L,
                           contact_tracing_days_until_result = 3L,
                           contact_tracing_only_sympto = TRUE,
                           contact_tracing_success = 0.9,
                           tests_by_state = NULL,
                           max_persons_per_event = 5L,
                           nPersonsByEvent = c(20e3L, 5e3, 1500L),
                           n_major_events_weekday = 2L,
                           n_major_events_weekend = 10L,
                           max_persons_per_supermarket = 200L,
                           cafes_open = TRUE,
                           age_based_lockdown = integer(101),
                           workplaces_open = FALSE,
                           workplace_size_max = 1L,
                           workplace_size_beta = 13,
                           workplace_size_lmu = -1,
                           workplace_size_lsi = -1,
                           travel_outside_sa2 = FALSE,
                           lockdown_triggers__schools = set_lockdown_triggers__schools(),
                           isol_compliance = 0.7) {

  if (!is.integer(yday_start)) {
    yday_start <- yday(yday_start)
  }
  checkmate::check_int(yday, lower = 0L)


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
  checkmate::assert_number(contact_tracing_success, lower = 0, upper = 1)


  tests_by_state_was_null <- is.null(tests_by_state)
  tests_by_state <- .fix_tests_by_state(tests_by_state)

  checkmate::assert_int(max_persons_per_event, lower = 1L)

  workplaces_open <- as.double(workplaces_open)
  checkmate::assert_number(workplaces_open, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_int(workplace_size_max, lower = 0L)
  checkmate::assert_number(workplace_size_beta, finite = TRUE)
  checkmate::assert_number(workplace_size_lmu, finite = TRUE)
  checkmate::assert_number(workplace_size_lsi, finite = TRUE)

  age_based_lockdown <- .fix_age_based_lockdown(age_based_lockdown)

  travel_outside_sa2 <- checkmate::assert_logical(travel_outside_sa2, len = 1L)

  school_lockdown_triggers_exist <-
    !is.null(lockdown_triggers__schools) &&
    !isFALSE(lockdown_triggers__schools) &&
    !isFALSE(lockdown_triggers__schools[["do_school_lockdown"]])

  if (is.null(lockdown_triggers__schools) || isFALSE(lockdown_triggers__schools)) {
    lockdown_triggers__schools <-
      set_lockdown_triggers__schools(do_school_lockdown = FALSE)
  }

  checkmate::assert_number(isol_compliance, lower = 0, upper = 1)


  out <- mget(ls(sorted = FALSE))
  attr(out, "original_call") <- match.call()
  out
}

#' @rdname set_policypars
#' @export
set_policy_no_restrictions <- function(...) {
  set_policypars(supermarkets_open = TRUE,
                 schools_open = TRUE,
                 only_Year12 = FALSE,
                 school_days_per_wk = 5L,
                 do_contact_tracing = FALSE,
                 max_persons_per_event = .Machine$integer.max,
                 max_persons_per_supermarket = .Machine$integer.max %/% 2L,
                 cafes_open = TRUE,
                 age_based_lockdown = integer(101),
                 workplaces_open = TRUE,
                 workplace_size_max = .Machine$integer.max %/% 2L,
                 lockdown_triggers__schools = NULL,
                 ...)
}


set_multipolicy <- function(.first_yday = NULL) {
  out <-
    list(set_policy_no_restrictions(yday_start = "2020-01-01"),
         set_policypars(yday_start = "2020-03-22",
                        max_persons_per_event = 5L,
                        workplaces_open = 0.1,
                        schools_open = FALSE,
                        travel_outside_sa2 = FALSE),

         # Tas quarantine,
         # NT borders close on 21st,
         # WA, SA 24th
         set_policypars(yday_start = "2020-03-20",
                        max_persons_per_event = 10L,
                        workplaces_open = 0.2,
                        workplace_size_max = 10,
                        schools_open = TRUE,
                        school_days_per_wk = c("ACT" = 0L,
                                               "VIC" = 1L)),

         # VIC stage 3
         set_policypars(yday_start = "2020-03-30",
                        workplaces_open = 0.2,
                        workplace_size_max = 10,
                        schools_open = TRUE,
                        school_days_per_wk = c("VIC" = 0L)),

         # WA/QLD/NT easing of restriction
         ## QLD: Recreation permitted, within 50km of home only

         set_policypars(yday_start = "2020-04-26",
                        workplaces_open = 0.2,
                        workplace_size_max = 10,
                        schools_open = TRUE,
                        school_days_per_wk = c("ACT" = 0L,
                                               "NSW" = 5L,
                                               "VIC" = 0L,
                                               "QLD" = 5L,
                                               "SA" = 5L,
                                               "WA" = 5L,
                                               "TAS" = 5L,
                                               "NT" = 5L)),

         # Easing
         set_policypars(yday_start = "2020-05-15",
                        schools_open = TRUE,
                        school_days_per_wk = c("ACT" = 0L,
                                               "NSW" = 5L,
                                               "VIC" = 0L,
                                               "QLD" = 5L,
                                               "SA" = 5L,
                                               "WA" = 5L,
                                               "TAS" = 5L,
                                               "NT" = 5L),
                        workplaces_open = 0.5,
                        max_persons_per_event = 100L),

         set_policypars(yday_start = "2020-06-01",
                        schools_open = TRUE,
                        school_days_per_wk = c("ACT" = 5L,
                                               "NSW" = 5L,
                                               "VIC" = 0L,
                                               "QLD" = 5L,
                                               "SA" = 5L,
                                               "WA" = 5L,
                                               "TAS" = 5L,
                                               "NT" = 5L),
                        workplaces_open = 0.9,
                        workplace_size_max = 100L),
         set_policypars(yday_start = "2020-07-10",
                        schools_open = FALSE,
                        contact_tracing_success = 0.4,
                        workplaces_open = 0.7),
         set_policypars(yday_start = "2020-08-01",
                        schools_open = FALSE,
                        workplaces_open = 0.4,
                        contact_tracing_success = 0.25,
                        workplace_size_max = 20),
         set_policypars(yday_start = "2020-09-09",
                        workplaces_open = 0.7,
                        schools_open = TRUE,
                        contact_tracing_success = 0.5))
  if (is.null(.first_yday)) {
    return(out)
  }
  yday_std <- function(x) {
    if (is.integer(x)) {
      x
    } else {
      yday(x)
    }
  }

  out[vapply(out, function(e) yday_std(e[["yday_start"]]) >= yday_std(.first_yday), FALSE)]
}


update_policypars <- function(Policy,
                              yday_start = NULL,
                              supermarkets_open = NULL,
                              schools_open = NULL,
                              only_Year12 = NULL,
                              school_days_per_wk = NULL,
                              do_contact_tracing = NULL,
                              contact_tracing_days_before_test = NULL,
                              contact_tracing_days_until_result = NULL,
                              contact_tracing_only_sympto = NULL,
                              contact_tracing_success = NULL,
                              tests_by_state = NULL,
                              max_persons_per_event = NULL,
                              n_major_events_weekday = NULL,
                              n_major_events_weekend = NULL,
                              max_persons_per_supermarket = NULL,
                              cafes_open = NULL,
                              age_based_lockdown = NULL,
                              workplaces_open = NULL,
                              workplace_size_max = NULL,
                              workplace_size_beta = NULL,
                              workplace_size_lmu = NULL,
                              workplace_size_lsi = NULL,
                              travel_outside_sa2 = NULL) {
  args <- ls()
  for (arg in args) {
    if (!is.null(get(arg))) {
      Policy[[arg]] <- get(arg)
    }
  }
  Policy
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

  if (is.null(tests_by_state)) {
    time_series_tests <- read_sys("time_series_tests.fst")
    for (s in states()) {
      if (hasName(time_series_tests, s)) {
        v <- .subset2(time_series_tests, s)
        i <- which(complete.cases(v))
        v <- cummax(coalesce(v, 0L))
        set(time_series_tests, i = i, j = s, value = v[i])
      }
    }
    tests_by_state <- last(time_series_tests[, lapply(.SD, diff)])
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
          stop(glue("Element '{s}' of `{vname(x)}` was not atomic. ",
                    "`{vname(x)}` must be a named vector or named ",
                    "list of atomic integers."))
        }

        if (length(xs) != 1L && length(xs) != NAGES) {
          stop(glue("`{vname(x)}` contained an element ",
                    "of length {length(xs)} for state '{s}'. ",
                    "The only permissible lengths for state subelements are 1 or {NAGES}."))
        }

        if (anyNA(xs)) {
          stop(glue("`{vname(x)}` contained an element ",
                    "with missing values for state '{s}'. ",
                    "Only integer values from 0 to 5 are permitted for states' days per week."))
        }
        if (min(xs) < 0 || max(xs) > 5) {
          stop(glue("`{vname(x)}` contained an element ",
                    "with values outside [0, 5]  for state '{s}'. ",
                    "Only integer values from 0 to 5 are permitted for states' days per week."))
        }
        if (is.double(xs) && any(xs != as.integer(xs))) {
          stop(glue("`{vname(x)}` contained an element ",
                    "with non-integer values for state '{s}'. ",
                    "Only integer values from 0 to 5 are permitted for states' days per week."))
        }

        out[[s]] <- rep_len(as.integer(xs), NAGES)
      }
    }
    return(out)
  }
  warning(glue("`{vname(x)}` was not in a recognized format and will be ignored."))

  out
}

.fix_age_based_lockdown <- function(x) {
  if (anyNA(x)) {
    stop(vname(x), " contained missing value.")
  }
  if (length(x) == 101L && is.integer(x)) {
    return(x)
  }
  if (is.integer(x) &&
      length(x) <= 101 &&
      min(x, na.rm = TRUE) >= 0 &&
      max(x, na.rm = TRUE) <= 100) {
    # interpret as the ages to lockdown
    out <- integer(101)
    out[x + 1L] <- 1L
    return(out)
  }
  stop(vname(x), " unfixable at this time.")
}






dollars <- function(x, .name, ..., .x = vname(x), TRY_EVAL = FALSE) {
  # equiv to x$name$..1$..2 but safer if name is misspelled
  name <- as.character(substitute(.name))
  if (!hasName(x, name)) {
    if (isFALSE(TRY_EVAL) || !hasName(x, .name)) {
      stop(.x, " did not contain a subelement '", name, "'.")
    }
    name <- .name
  }
  if (missing(..1)) {
    return(.subset2(x, name))
  }
  dollars(.subset2(x, name), ..., .x = .x)
}








