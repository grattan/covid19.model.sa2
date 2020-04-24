#' Set the status among a population
#' @param state_id An integer vector of states, the match of
#'  "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT".
#' If \code{dead = NULL} then \code{state_id} must be length-one.
#'
#' @param dead The number of patients who have died.
#' @param healed The number of patients who have recovered.
#' @param active The number of patients who have tested positive
#' and have not recovered, died, or been admitted to intensive care.
#' @param critical The number of patients in intensive care.
#' @param cases Optionally, the number of cumulative cases. That is,
#' \code{dead + healed + active + critical}.
#'
#' @param cases_by_state A table of total cases (cumulative) by date,
#' ordered by date, and having columns matching the states in \code{state_id}.
#' Alternatively a string to be read in by \code{\link{read_sys}}, the path
#' to a file with this data.
#' @param deaths_by_state,recovered_by_state As for \code{cases_by_state}.
#' @param asympto The proportion of patients who are asymptomatic.
#' @param p_critical The proportion of symptomatic cases who are critical.
#' @param .population The size of the output, the population. By default,
#' set to the length of \code{state_id}.
#'
#' @return An integer vector of length \code{.population} with values
#' as given in the status table \code{\link{simulate_sa2}}. The statuses
#' are randomly provided throughout the vector in the distribution of
#' \code{dead, healed, active, critical}.
#'
#' @export


set_initial_by_state <- function(state_id,
                                 dead = NULL,
                                 healed = NULL,
                                 active = NULL,
                                 critical = NULL,
                                 cases = NULL,
                                 cases_by_state = "time_series_cases.fst",
                                 deaths_by_state = "time_series_deaths.fst",
                                 recovered_by_state = "time_series_recovered.fst",
                                 asympto = 0.48,
                                 p_critical = 0.03,
                                 .population = length(state_id)) {
  if (missing(state_id)) {
    stop("'state_id' is missing, with no default.")
  }
  .states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")
  if (state_id_was_character <- is.character(state_id)) {
    state_id <- match(state_id, .states)
  }

  if (length(state_id) == 1L && (anyNA(state_id) || state_id > length(.states))) {
    return(integer(.population))
  }

  # Must be all NULL or none NULL
  n_nulls <- is.null(dead) + is.null(healed) + is.null(active) + is.null(critical)
  if (n_nulls != 0L && n_nulls != 4L) {
    stop(if (is.null(dead)) "`dead = NULL`" else "`dead` was not NULL",
         ", yet healed`, `active`, or `critical` were ",
         if (n_nulls) "`NULL" else "not all NULL.",
         "These arguments must be all NULL or all integers.",
         " (n_nulls = ", n_nulls, ").")
  }
  if (n_nulls == 0 && is.null(cases)) {
    # cases can be left NULL since it's just
    # the sum of the others.
    cases <- dead + healed + active + critical
  }

  if (is.character(cases_by_state)) {
    cases_by_state <- read_sys(cases_by_state, fst2_progress = FALSE)
    deaths_by_state <- read_sys(deaths_by_state, fst2_progress = FALSE)
    recovered_by_state <- read_sys(recovered_by_state, fst2_progress = FALSE)
  }

  if (nrow(cases_by_state) > 1) {
    cases_by_state <- last(cases_by_state)
    deaths_by_state <- last(deaths_by_state)
    recovered_by_state <- last(recovered_by_state)
  }

  if (length(state_id) != 1L) {
    # Perform this function 'by' state_id.
    DT <- setDT(list(x = state_id))
    DT[, "out" := set_initial_by_state(.BY[[1]],
                                       dead = dead,
                                       healed = healed,
                                       active = active,
                                       critical = critical,
                                       cases = cases,
                                       cases_by_state = cases_by_state,
                                       deaths_by_state = deaths_by_state,
                                       recovered_by_state = recovered_by_state,
                                       asympto = asympto,
                                       .population = .N),
       by = "x"]
    return(DT[["out"]])
  }

  # If the variables are NULL at this point we should be able to consult the tables
  # to identify them
  dead %<=% .subset2(deaths_by_state, .states[state_id])
  healed %<=% .subset2(recovered_by_state, .states[state_id])
  cases %<=% .subset2(cases_by_state, .states[state_id])

  # Note the precedence
  active %<=% (cases - healed - dead)
  # Have to be very careful mixing doubles with ints
  # as this will causing rounding errors and we have
  # to get the same length out as in.
  sympto <- 1 - asympto
  nosymp <- as.integer(asympto * active)

  # Note that insymp <- sympto * active is wrong because of rounding.
  insymp <- active - nosymp

  critical %<=% (as.integer(p_critical * insymp))


  if (.population < (dead + healed + active + critical)) {
    tot_cases <- dead + healed + active + critical
    stop(glue::glue("`.population = {.population}`, yet ",
                    "(dead + healed + active + critical) = {tot_cases}. ",
                    "state_id = {state_id}"),
         "Ensure the population at least the number of total cases.")
  }
  dqsamp_status(dead, healed, nosymp, insymp, critical, .population, asympto)
}

dqsamp_status <- function(dead, healed, nosymp, insymp, critical, .population, asympto) {
  n_status0 <- .population - (dead + healed + nosymp + insymp + critical)
  status <-
    rep(c(status_killed(),
          status_healed(),
          status_suscep(),
          status_nosymp(),
          status_insymp(),
          status_critic()),
        times = c(dead,
                  healed,
                  n_status0,
                  nosymp,
                  insymp,
                  critical))
  if (length(status) != .population) {
    stop("Internal error: length(status) != .population.")
  }

  dqrng::dqsample(status)
}

