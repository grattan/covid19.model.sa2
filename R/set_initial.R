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
#'
#' @param cases_by_state A table of total cases (cumulative) by date,
#' ordered by date, and having columns matching the states in \code{state_id}.
#' Alternatively a string to be read in by \code{\link{read_sys}}, the path
#' to a file with this data.
#' @param deaths_by_state,recovered_by_state As for \code{cases_by_state}.
#' @param asympto The proportion of cases who are asymptomatic.
#' \strong{Note: active cases are assumed to be all symptomatic. Thus,
#' if the number of active cases is 100 and }\code{asympto = 0.75}
#' \strong{ the number of total cases is 400.}
#' @param p_critical The proportion of symptomatic cases who are critical.
#' @param first_yday An integer, the \code{\link[data.table:IDateTime]{yday}} to set
#' as the initial day.
#'
#' @param p_isol The proportion of cases in isolation.
#'
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
                                 first_yday = NULL,
                                 p_isol = NULL,
                                 .population = NULL) {
  if (missing(state_id)) {
    stop("'state_id' is missing, with no default.")
  }
  .states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")
  if (state_id_was_character <- is.character(state_id)) {
    state_id <- match(state_id, .states)
  }

  if (is.null(.population) && length(state_id) == 1L) {
    .population <- state_population(state_id)
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
    cc <- function(dt) {
      if (hasName(dt, "lgl")) {
        dt[, "lgl" := NULL]
      }
      dt[complete.cases(dt)]
    }
    cases_by_state <- cc(read_sys(cases_by_state, fst2_progress = FALSE))
    deaths_by_state <- cc(read_sys(deaths_by_state, fst2_progress = FALSE))
    recovered_by_state <- cc(read_sys(recovered_by_state, fst2_progress = FALSE))
    impute_time_series(cases_by_state, deaths_by_state, recovered_by_state)
  }




  if (nrow(cases_by_state) > 1) {
    if (is.null(first_yday)) {
      cases_by_state <- last(cases_by_state)
      deaths_by_state <- last(deaths_by_state)
      recovered_by_state <- last(recovered_by_state)
      first_yday <- cases_by_state[, yday(Date)]
    } else {
      earliest_allowed_date <-
        recovered_by_state[complete.cases(recovered_by_state), min(Date)]

      earliest_allowed_yday <- yday(earliest_allowed_date)
      max_allowed_date <- recovered_by_state[, max(Date)]
      max_allowed_yday <- yday(max_allowed_date)

      orig_first_yday <- first_yday
      if (is.character(first_yday) || inherits(first_yday, "Date")) {
        first_yday <- yday(first_yday)
      }
      if (!is.atomic(first_yday)) {
        stop(g("`first_yday` was class {toString(class(orig_first_yday))}, but must be atomic."))
      }
      if (length(first_yday) != 1) {
        stop(g("`first_yday` had length {length(first_yday)}, but must be length-one."))
      }
      if (anyNA(first_yday)) {
        stop(g("`first_yday` was NA, which is not permitted."))
      }
      if (is.double(first_yday)) {
        if (first_yday != as.integer(first_yday)) {
          stop(g("`first_yday = {orig_first_yday}`, but must be a whole number."))
        }
        first_yday <- as.integer(first_yday)
      }
      if (first_yday < earliest_allowed_yday) {
        stop(g("`first_yday = {orig_first_yday}`, but the earliest allowed yday, ",
               "given available data, is {earliest_allowed_yday}."))
      }
      if (first_yday > max_allowed_yday) {
        stop(g("`first_yday = {orig_first_yday}`, but the latest allowed yday, ",
               "given available data, is {max_allowed_yday}."))
      }


      cases_by_state <- cases_by_state[yday(Date) == first_yday]
      deaths_by_state <- deaths_by_state[yday(Date) == first_yday]
      recovered_by_state <- recovered_by_state[yday(Date) == first_yday]
    }
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
                                       first_yday = first_yday,
                                       p_isol = p_isol,
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
  nosymp <- as.integer((asympto / (1 - asympto)) * active)

  # We assume all active casees are symptomatic
  insymp <- active

  critical %<=% (as.integer(p_critical * insymp))

  p_isol %<=% p_quarantine_by_date(as.Date("2020-01-01") + first_yday)

  if (.population < (dead + healed + active + critical)) {
    tot_cases <- dead + healed + active + critical
    stop(glue::glue("`.population = {.population}`, yet ",
                    "(dead + healed + active + critical) = {tot_cases}. ",
                    "state_id = {state_id}."), "\n",
         "Ensure the population at least the number of total cases.")
  }
  dqsamp_status(dead, healed, nosymp, insymp, critical, .population, asympto, p_isol)
}


impute_time_series <- function(time_series_cases, time_series_deaths, time_series_healed) {
  #
  # time_series_deaths <- read_sys("time_series_deaths.fst", fst2_progress = FALSE)
  # time_series_healed <- read_sys("time_series_recovered.fst", fst2_progress = FALSE)
  # time_series_cases <- read_sys("time_series_cases.fst", fst2_progress = FALSE)

  # Victoria is the most comprehensive
  NSW_r_VIC <- time_series_healed[complete.cases(NSW), mean(NSW / VIC)]

  state_r_VIC <- function(s) {
    eval.parent(substitute(time_series_healed[complete.cases(get(s)), mean(get(s) / VIC)]))
  }
  state_r_TAS <- function(s) {
    eval.parent(substitute(time_series_healed[complete.cases(get(s)), mean(get(s) / TAS)]))
  }

  for (DT in list(time_series_cases, time_series_deaths, time_series_healed)) {
    for (j in names(DT)) {
      if (j %in% c("Date", "VIC", "Total")) {
        next
      }
      r_vic <- state_r_VIC(j)
      r_tas <- state_r_TAS(j)
      vic <- .subset2(DT, "VIC")
      tas <- .subset2(DT, "TAS")
      v <- .subset2(DT, j)
      DT[, (j) := coalesce(v, as.integer(vic * r_vic), as.integer(tas * r_tas))]
    }
  }

  list(time_series_cases, time_series_deaths, time_series_healed)
}

p_quarantine_by_date <- function(.date, cap = TRUE) {
  if (is.integer(.date) && min(.date) >= 0L && max(.date) <= 366L) {
    # is yday
    .date <- as.Date("2019-12-31") + .date
  } else if (is.character(.date)) {
    .date <- as.Date(.date)
  }

  nsw_abroad <- read_sys("time_series_nsw_sources.fst", columns = c("Date", "overseas"))
  nsw_abroad[, nsw_abroad := cumsum(overseas)]
  nsw_abroad[, overseas := NULL]

  vic_abroad <- read_sys("time_series_vic_sources.fst", columns = c("Date", "overseas"))
  vic_abroad[, vic_abroad := cumsum(overseas)]
  vic_abroad[, overseas := NULL]

  cases <- read_sys("time_series_cases.fst", columns = c("Date", "NSW", "VIC"))
  cases[, tot_cases := NSW + VIC]
  cases[, c("NSW", "VIC") := NULL]

  p_by_date <-
    cases[nsw_abroad][vic_abroad] %>%
    .[, tot_cases := nafill(tot_cases, type = "locf")] %>%
    .[, nsw_abroad := nafill(nsw_abroad, type = "locf")] %>%
    .[, p := (nsw_abroad + vic_abroad) / tot_cases]
  if (cap) {
    p_by_date[, p := pmin.int(p, 1)]
  }
  setkeyv(p_by_date, "Date")
  p_by_date[.(.date), p, roll = "nearest"]
}



dqsamp_status <- function(dead, healed, nosymp,
                          insymp, critical,
                          .population,
                          asympto,
                          p_isol = 0) {


  nosymp_isola <- as.integer(p_isol * nosymp)
  nosymp_libre <- nosymp - nosymp_isola
  insymp_isola <- as.integer(p_isol * insymp)
  insymp_libre <- insymp - insymp_isola


  n_status0 <- .population - (dead + healed + nosymp + insymp + critical)
  status <-
    rep(c(status_killed(),
          status_healed(),
          status_suscep(),
          status_nosymp(),
          status_nosymp() + isolated_plus(),
          status_insymp(),
          status_insymp() + isolated_plus(),
          status_critic()),
        times = c(dead,
                  healed,
                  n_status0,
                  nosymp_libre,
                  nosymp_isola,
                  insymp_libre,
                  insymp_isola,
                  critical))
  if (length(status) != .population) {
    stop("Internal error: length(status) != .population.")
  }

  dqrng::dqset.seed(seed = updateLemireSeedFromR()[1:2])
  dqrng::dqsample(status)
}


set_initial_stochastic <- function(aus, .yday, p_asympto = 0.48, nThread = 1L) {
  # Everything in war is simple, but the simplest thing is hard.

  stopifnot(is.data.table(aus), hasName(aus, "state"))
  cases <- read_sys("time_series_cases.fst")[yday(Date) <= .yday]
  if (hasName(cases, "lgl")) {
    cases[, "lgl" := NULL]
  }
  cases <- cases[complete.cases(cases)]
  NewCases_by_Date <-
    cases[, lapply(.SD, function(x) if (inherits(x, "Date")) x[-1] else diff(cummax(x)))]


  CasesUnwt <-
    NewCases_by_Date %>%
    .[, InfectedOn := yday(Date)] %>%
    .[, c("Date", "Total") := NULL] %>%
    melt.data.table(id.vars = c("InfectedOn"),
                    variable.name = "State",
                    variable.factor = FALSE) %>%

    # Account for hidden asymptomatic
    # .[, value := as.integer(value * (1 + p_asympto))] %>%
    weight2rows("value", discard_weight.var = TRUE)

  n_cases_by_state <- function(state) {
    the_state <- force(state)
    stopifnot(length(the_state) == 1L)
    if (is.integer(the_state)) {
      the_state <- states()[state - 1L]
    }

    CasesUnwt[State == the_state, .N]

  }

  n_Status0_by_state <- function(state) {
    state_population(state) - n_cases_by_state(state)
  }

  pid_by_state <- function(state) {
    the_state <- force(state)
    state_no_aus <- states()[-1]
    if (!is.integer(state)) {
      the_state <- match(state, state_no_aus)
    }
    aus[state == the_state, identity(pid)]
  }

  deaths <- read_sys("time_series_deaths.fst")[yday(Date) == .yday]
  deathss <- function(state) {
    the_state <- state
    if (is.integer(the_state)) {
      the_state <- states()[state + 1L]
    }
    if (hasName(deaths, the_state)) {
      deaths[[the_state]]
    } else {
      0L
    }
  }

  rep_time <- function(x, times, .default) {
    if (min(length(times)) == 0 || anyNA(times) || min(times) <= 0L) {
      return(.default)
    }
    rep(x, times)
  }

  CasesReWt <- copy(CasesUnwt)
  CasesReWt[, pid := sample(pid_by_state(.BY[[1]]), size = .N), by = "State"]


  stopifnot(hasName(aus, "Incubation"),
            hasName(aus, "Illness"))
  InfectedOn <- Incubation <- Illness <- NULL

  # In case rerun
  aus[, InfectedOn := NA_integer_]
  aus[CasesReWt, InfectedOn := i.InfectedOn, on = "pid"]
  aus[, Status := 0L]

  # | ------------------------ | ---------------- | -------------| ------------|
  #                                                       ^yday
  # 0                     InfectedOn              b              c             d
  #  b = InfectedOn + Incubation = IncubationEnds
  #  c = InfectedOn + Incubation + Illness = IllnessEnds


  withr::with_options(list("hutilscpp.nThread" = nThread), {
    # InfectedOn >= 0L within and3s implies !is.na
    aus[and3s(InfectedOn >= 0L, InfectedOn >= 0L), IncubationEnds := InfectedOn + Incubation]
    aus[and3s(InfectedOn >= 0L, InfectedOn >= 0L), IllnessEnds := IncubationEnds + Illness]
    aus[and3s(InfectedOn %between% c(0L, .yday), IncubationEnds >= .yday), Status := 1L]
    aus[and3s(InfectedOn %between% c(0L, .yday), IncubationEnds %in% 1:.yday, IllnessEnds >= .yday), Status := 2L]
    aus[and3s(InfectedOn %between% c(0L, .yday), IncubationEnds %in% 1:.yday, IllnessEnds %in% 1:.yday), Status := -1L]
    aus[Status == -1L,
        Status := rep_time(c(-2L, -1L),
                           times = c(deathss(.BY[[1]]),
                                     .N - deathss(.BY[[1]])),
                           .default = Status),
        by = .(state)]

    aus[and3s(Status %in% 1:2, Status > 0L),
        Status := Status + fifelse(runif(.N) < p_quarantine_by_date(.yday),
                                   isolated_plus(),
                                   0L)]
  })

  aus
}







