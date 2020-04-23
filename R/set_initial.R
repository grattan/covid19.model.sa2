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

  if (is.null(dead)) {
    if (length(state_id) != 1L) {
      stop("`dead = NULL` but length(state_id) != 1.")
    }
    if (is.character(cases_by_state)) {
      cases_by_state <- read_sys(cases_by_state, fst2_progress = FALSE)
      deaths_by_state <- read_sys(deaths_by_state, fst2_progress = FALSE)
      recovered_by_state <- read_sys(recovered_by_state, fst2_progress = FALSE)
    }
    # checkmate::assert_list(cases_by_state)
    # checkmate::assert_list(deaths_by_state)
    # checkmate::assert_list(recovered_by_state)

    cases_by_state <- last(cases_by_state)
    deaths_by_state <- last(deaths_by_state)
    recovered_by_state <- last(recovered_by_state)

    dead <- .subset2(deaths_by_state, .states[state_id])
    healed <- .subset2(recovered_by_state, .states[state_id])
    cases <- .subset2(cases_by_state, .states[state_id])
    active <- cases - healed - dead
    critical <- p_critical * active

  } else if (length(state_id) != 1L) {
    # Perform this function 'by' state_id.
    DT <- setDT(list(x = state_id))
    DT[, out := set_initial_by_state(.BY[[1]],
                                     dead, healed, active, critical,
                                     asympto = asympto,
                                     .population = .N),
       by = "x"]
    return(DT[["out"]])
  }

  if (.population < (dead + healed + active + critical)) {
    tot_cases <- dead + healed + active + critical
    stop(glue::glue("`.population = {.population}`, yet ",
                    "(dead + healed + active + critical) = {tot_cases}. "),
         "Ensure the population at least the number of total cases.")
  }
  wsamp_status(dead, healed, active, critical, .population, asympto)
}

wsamp_status <- function(dead, healed, active, critical, .population, asympto) {
  n_status0 <- .population - (dead + healed + active + critical)
  sympto <- 1 - asympto
  wsamp(c(-2L, -1L, 0L, 1L, 2L, 3L),
        size = .population,
        w = c(dead, healed, n_status0, active * c(asympto, sympto), critical))
}

