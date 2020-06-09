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
#' If any entry is negative,
#' or \code{NA}, the most recent day's number of tests performed are
#' used.
#'
#'
#' @param max_persons_per_supermarket Maximum number of people allowed in a
#' supermarket (within one hour i.e. concurrently).
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
                           n_major_events_weekday = 28L,
                           n_major_events_weekend = 56L,
                           max_persons_per_supermarket = 200L,
                           cafes_open = TRUE,
                           age_based_lockdown = integer(101),
                           workplaces_open = FALSE,
                           workplace_size_max = 1L,
                           workplace_size_beta = 13,
                           workplace_size_lmu = -1,
                           workplace_size_lsi = -1,
                           travel_outside_sa2 = FALSE,
                           lockdown_triggers__schools = set_lockdown_triggers__schools()) {

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
    !isFALSE(lockdown_triggers__schools[["do_school_lockdown"]])


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


set_multipolicy <- function() {
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
                      schools_open = TRUE,
                      school_days_per_wk = c("ACT" = 0L,
                                             "VIC" = 1L)),

       # VIC stage 3
       set_policypars(yday_start = "2020-03-30",
                      schools_open = FALSE),

       # WA/QLD/NT easing of restriction
       ## QLD: Recreation permitted, within 50km of home only

       set_policypars(yday_start = "2020-04-26",
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
                      school_days_per_wk = c("ACT" = 0L,
                                             "NSW" = 5L,
                                             "VIC" = 0L,
                                             "QLD" = 5L,
                                             "SA" = 5L,
                                             "WA" = 5L,
                                             "TAS" = 5L,
                                             "NT" = 5L),
                      workplaces_open = 0.9))
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

  if (is.null(tests_by_state) ||
      anyNA(tests_by_state) ||
      min(tests_by_state) < 0) {
    time_series_tests <- read_sys("time_series_tests.fst")
    for (s in states()) {
      if (hasName(time_series_tests, s)) {
        v <- .subset2(time_series_tests, s)
        i <- which(complete.cases(v))
        v <- cummax(coalesce(v, 0L))
        set(time_series_tests, i = i, j = s, value = v[i])
      }
    }
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




read_yaml_config <- function(config.yaml) {
  config <- yaml::read_yaml(config.yaml)
  if (!hasName(config, "policy")) {
    stop(vname(config.yaml), " did not have a field 'policy:' at top level.")
  }
  Policy <- config[["policy"]]


}

write_yaml_config <- function(Epi, Policies) {

  # if we have a number that has been converted to an integer [0,1000]
  # convert it back for yaml
  ki2dbl <- function(x) if (is.integer(x)) x / 1000 else x

  illness_distribution <-
    illness_mean <-
    illness_sigma <-
    incubation_distribution <-
    incubation_mean <-
    incubation_sigma <-
    p_critical <-
    p_death <-
    r_distribution <- r_location <- r_supermarket_location <-
    r_scale <- r_supermarket_scale <- resistance_threshold <-  NULL

  config <- list(
    epidemiology = list(
      illness = list(
        distribution = dollars(Epi, illness_distribution),
                mean = dollars(Epi, illness_mean),
               sigma = dollars(Epi, illness_sigma)
      ),
      incubation = list(
        distribution = dollars(Epi, incubation_distribution),
                mean = dollars(Epi, incubation_mean),
               sigma = dollars(Epi, incubation_sigma)
      ),
      prognosis = list(
        critical = ki2dbl(dollars(Epi, p_critical)),
           death = ki2dbl(dollars(Epi, p_death))

      ),
      reinfection = list(
        distribution = decode_distr(dollars(Epi, r_distribution)),
        location = list(
          default = dollars(Epi, r_location),
          supermarket = dollars(Epi, r_supermarket_location)
        ),
        scale = list(
          default = dollars(Epi, r_scale),
          supermarket = dollars(Epi, r_supermarket_scale)
        )
      ),
      resistance = list(
        default = dollars(Epi, resistance_threshold),
        work = dollars(Epi, resistance_threshold)
      )
    ),
    policy = unpack_multipolicy(Policies))
}

unpack_multipolicy <- function(MultiPolicy) {
  if (length(MultiPolicy) == 1) {
    ki2dbl <- function(x) if (is.integer(x)) x / 1000 else x

    Policy <- MultiPolicy[[1L]]


    .start_date <- Policy[["start_date"]] %||% "2020-01-01"

    schools_all_full_time <-
      hasName(Policy, "school_days_per_wk") &&
      hasName(Policy[["school_days_per_wk"]], "all_full_time") &&
      isTRUE(dollars(Policy, school_days_per_wk, all_full_time))

    .schools <-
      if (schools_all_full_time) {
        list(aus = list(open = dollars(Policy, schools_open),
                        only_Year12 = dollars(Policy, only_Year12),
                        all_full_time = TRUE))
      } else {
        days_per_wk2_list <- function(state) {
          x <- dollars(Policy, school_days_per_wk)[[state]]
          if (is_constant(x)) {
            x <- x[1L]
            if (x == 5L) {
              x <- NA_integer_  # i.e. erase
            }
          }
          x
        }
        list(aus = list(open = dollars(Policy, schools_open),
                        only_Year12 = dollars(Policy, only_Year12),
                        all_full_time = FALSE),
             aus = list(days_per_wk = days_per_wk2_list('AUS')),
             nsw = list(days_per_wk = days_per_wk2_list('NSW')),
             vic = list(days_per_wk = days_per_wk2_list('VIC')),
             qld = list(days_per_wk = days_per_wk2_list('QLD')),
              wa = list(days_per_wk = days_per_wk2_list('WA')),
              sa = list(days_per_wk = days_per_wk2_list('SA')),
             tas = list(days_per_wk = days_per_wk2_list('TAS')),
             act = list(days_per_wk = days_per_wk2_list('ACT')),
              nt = list(days_per_wk = days_per_wk2_list('NT')),
             oth = list(days_per_wk = days_per_wk2_list('OTH')))
      }
    .schools <- .schools[!vapply(.schools, function(x) length(x[[1]]) == 1 && is.na(x[[1]]), FALSE)]

    .age_lockdown <- dollars(Policy, age_based_lockdown)
    .age_lockdown_in_force <- any(as.logical(.age_lockdown))
    if (.age_lockdown_in_force) {
      .age_lockdown <- list(in_force = TRUE,
                            age_lwr = which_first(.age_lockdown > 0L),
                            age_upr = which_last(.age_lockdown > 0L))
    } else {
      .age_lockdown <- list(in_force = FALSE)
    }


    tests_by_state_was_null <- dollars(Policy, tests_by_state_was_null)
    .tests_by_state <-
      if (tests_by_state_was_null) {
        "auto"
      } else {
        dollars(Policy, tests_by_state)
      }


    # workplace_size_max <- resistance_threshold <- NULL
    workplace_size_max <- resistance_threshold <- NULL

    # assume
    list(start_date = "2020-01-01",
         schools = .schools,
         places = list(
           default = list(
             max_visitors = dollars(Policy, max_persons_per_event)
           ),
           supermarkets = list(
             max_visitors = dollars(Policy, max_persons_per_supermarket)
           )
         ),
         work = list(
           prop_open = dollars(Policy, workplaces_open),
           workplace_size = list(
             max = dollars(Policy, workplace_size_max),
             beta = dollars(Policy, workplace_size_beta),
             lmu = dollars(Policy, workplace_size_lmu),
             lsi = dollars(Policy, workplace_size_lsi)
           )),
         age_lockdown = .age_lockdown,
         contact_tracing = list(
           tests_by_state = .tests_by_state,
           aus = list(
             in_force = dollars(Policy, "do_contact_tracing"),
             days_before_test = dollars(Policy, "contact_tracing_days_before_test"),
             days_until_result = dollars(Policy, "contact_tracing_days_until_result"),
             p_asympto_cases = 0.5 * as.double(dollars(Policy, "contact_tracing_only_sympto")))))
  } else {
    lapply(MultiPolicy, unpack_multipolicy)
  }
}

dollars <- function(x, .name, ..., .x = vname(x), TRY_EVAL = FALSE) {
  # equiv to x$name$..1$..2 but safer if name is misspelled
  name <- as.character(substitute(.name))
  if (!hasName(x, name)) {
    if (!hasName(x, .name) && isFALSE(TRY_EVAL)) {
      stop(.x, " did not contain a subelement '", name, "'.")
    }
    name <- .name
  }
  if (missing(..1)) {
    return(.subset2(x, name))
  }
  dollars(.subset2(x, name), ..., .x = .x)
}








