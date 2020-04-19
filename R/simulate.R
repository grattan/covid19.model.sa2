#' Simulate the spread of COVID-19 through Australia
#' @description
#' Simulates the spread of COVID-19 given today's condition, and
#' the (imputed) distribution of indivdiuals among households,
#' schools, and other places, from day to day.
#'
#'
#' @param days_to_simulate \describe{
#' \item{\code{integer(1)}}{The number of days to simulate.}
#' }
#' @param InitialStatus \describe{
#' \item{\code{list(4)}}{A list of four named components giving the
#' number of dead, healed, active, and critical on the day zero.}
#' }
#' @param PolicyPars \describe{
#' \item{\code{list()}}{A list describing the policy parameters.
#' (\code{list(schools_open = TRUE|FALSE, only_Year12 = TRUE|FALSE)}
#' are supported but no others. An empty list corresponds to current settings.)}
#' }
#' @param EpiPars \describe{
#' \item{\code{list()}}{A list of epidemiological parameters. Set to reasonable
#' values by default.}
#' }
#' @param .first_day \describe{
#' \item{\code{integer(1)}}{The first day to model. By convention, set to the
#' day of the year of 2020. Used to predict the outcomes of those presently
#' ill with COVID-19 based on imputed duration of infection.}
#' }
#' @param .population (Not used.)
#' @param verbose_timer Should the time be printed at points during the computation?
#' @param dataEnv An environment into which the data will be saved and
#' retrieved. Used to save time reading after the first time.
#'
#' @return
#' A list of \code{days_to_simulate + 1} components. The first
#' component is the inital status of each individual and
#' subsequent components are the statuses of each day simulated.
#'
#' @details
#' The \strong{Status} of an individual is
#'
#' | Status | Description                    |
#' | -----: | :----------------------------- |
#' |     -2 | Dead                           |
#' |     -1 | Healed                         |
#' |      0 | Susceptible                    |
#' |      1 | Infected, not showing symptoms |
#' |      2 | Infected, showing symptoms     |
#' |      3 | Critical                       |
#'
#' @md
#'
#'
#' @export


simulate_sa2 <- function(days_to_simulate = 300,
                         PolicyPars = list(),
                         InitialStatus = list(dead = 70,
                                              healed = 4230,
                                              active = 2258,
                                              critical = 48),
                         EpiPars = list(),
                         .first_day = yday(Sys.Date()),
                         .population = 25e6,
                         verbose_timer = TRUE,
                         dataEnv = getOption("covid19.model.sa2_dataEnv", new.env())) {
  ## Each day a person can
  ## stay in the household
  ## journey outside
  ## be admitted to hospital etsq

  ## if a person journeys out they do so for a purpose
  ## they can move to a different SA2
  ## or they can move for a daily activity

  ## if they go out for work
  ## their destination is their dzn of work
  ## unless they are work in a hospital, school, or aged care

  ## if they go out for school as a pupil
  ## (only occurs if they are a child)
  ## they go to a school in their SA2

  ## if they go out to purchase groceries
  ## the destination is stochastic
  ## Other places by predefined times
  ## per year

  ## Only interested in magnitude of interactions
  ## (above a certain limit?)

  ## Process is
  ## Loop over each day
  ### Loop over each person
  #### identify their places that day
  ### add up all the interactions
  ### sleep!
  ### next day
  hh_ss <- function (x. = "", form = "%H:%M:%S") {
    if (verbose_timer) {
      cat(x., as.character(format(Sys.time(), format = form)), "\n")
    } else {
      invisible(NULL)
    }
  }
  hh_ss("Start\t")

  read_sys <- function(file, columns = NULL) {
    # Distinguish between objects with different columns
    .file <- paste0(c(file, columns), collapse = "-")
    if (exists(.file, envir = dataEnv)) {
      return(get(.file, envir = dataEnv))
    }
    if (file.exists(file)) {
      ans <- fst::read_fst(file, columns = columns, as.data.table = TRUE)
    } else {
      sys_file <- system.file("extdata", file, package = "covid19.model.sa2")
      if (!nzchar(sys_file)) {
        stop(glue::glue("`file = {file}`, yet this file does not exist, "),
             "either by path or in the package system file.")
      }
      ans <- fst::read_fst(sys_file, columns = columns, as.data.table = TRUE)
    }
    # assign(.file, value = copy(ans), envir = dataEnv)
    ans
  }

  aus <- read_sys("australia.fst")
  nSupermarkets_by_sa2 <- read_fst("data-raw/google/tmp/nSupermarkets_by_sa2.fst", as.data.table = TRUE)

  sa2_by_hid <-
    read_sys("house.fst") %>%
    .[read_sys("sa2_codes.fst", columns = c("sa2_name", "sa2")),
      sa2 := as.integer(i.sa2),
      on = "sa2_name"] %>%
    .[]

  demo_by_person <- read_sys("person_demography.fst")

  Cases.csv <- fread("data-raw/pappubahry/AU_COVID19/time_series_cases.csv", key = "Date")
  Recovered.csv <- fread("data-raw/pappubahry/AU_COVID19/time_series_recovered.csv", key = "Date")
  Deaths.csv <- fread("data-raw/pappubahry/AU_COVID19/time_series_deaths.csv", key = "Date")

  hh_ss("post-read")

  diff_along <- function(x) c(NA, diff(x))

  # Use Victoria to get good idea about the duration of current cases
  Victoria <-
    Cases.csv %>%
    .[Recovered.csv] %>%
    .[, .(Date, VicCases = VIC, VicRecovered = coalesce(i.VIC, 0L))] %>%
    .[Deaths.csv] %>%
    .[, .(Date, VicCases, VicRecovered, VicDeaths = coalesce(VIC, 0L))] %>%
    .[, Date := as.Date(Date)] %>%
    .[, Concluded := VicRecovered + VicDeaths] %>%
    .[, NewCases := diff_along(VicCases)] %>%
    .[, dConcluded := diff_along(Concluded)] %>%
    .[-1] %>%
    .[, VicActive := cumsum(NewCases) - cumsum(dConcluded)] %>%
    .[, Yday := yday(Date)]

  n_concluded_cases <- Victoria[, last(VicRecovered)]
  # One row for every (concluded) cases
  # Note it's not clear whether the very early cases' recovery
  # was recorded, so we exclude the first cases under the assumption
  # that their recovery was not recorded (else we get ~50 day
  # spells of illness).

  N_by_Duration <-
    data.table(YdayIn  = weight2rows(Victoria, "NewCases")[["Yday"]][seq_len(n_concluded_cases)],
               YdayOut = weight2rows(Victoria, "dConcluded")[["Yday"]][seq_len(n_concluded_cases)]) %>%
    .[, Duration := YdayOut - YdayIn] %>%
    # Exclude Jan/early Feb cases
    .[YdayIn > 33] %>%
    .[, .N, keyby = .(Duration)]

  Policy  <- set_policy_defaults(PolicyPars)
  EpiPars <- set_epipars_defaults(EpiPars)


  asympto <- EpiPars$asympto
   sympto <- 1 - asympto

  # weighted sample (rather than prob)
  wsamp <- function(x, size, w) {
    probs <- w / sum(w)
    samp(x, size = size, prob = probs, loud = FALSE)
  }

  # For text width
  IS <- InitialStatus
  n_status0 <- nrow(aus) - sum(unlist(IS))
  samp_status <-
    sample(c(-2L, -1L, 0L, 1L, 2L),
           size = nrow(aus),
           replace = TRUE,
           prob = c(IS$dead, IS$healed, n_status0, IS$active, IS$critical) / nrow(aus))


  aus[, Status := samp_status]
  # If infected, they are infected days ago
  # according to N_by_Duration
  aus[Status > 0L,
      InfectedOn := .first_day - wsamp(N_by_Duration$Duration,
                                       size = .N,
                                       w = N_by_Duration$N)]

  aus[sa2_by_hid, sa2 := i.sa2, on = "hid"]
  aus[demo_by_person, Age := i.age, on = "pid"]
  aus[, Resistance := rep_len(sample(1:1000, size = 13381L, replace = TRUE), .N)]

  nPlacesByDestType <-
    lapply(1:106, function(i) {
      if (i == 98L) {
        read_fst("data-raw/google/tmp/nSupermarkets_by_sa2.fst",
                 columns = "nSupermarkets")[[1L]]
      } else {
        integer(0)
      }
    })

  # Times per year each person visits the matching type
  weekly <- rep_len(52L, nrow(aus))

  FreqsByDestType <-
    lapply(1:106, function(i) {
      if (i == 98L) {
        ## Assume supermarket visits are beta distributed
        rep_len(as.integer(360 * rbeta(1e6, 3, 1)), nrow(aus))
      } else {
        weekly
      }
    })


  # aus must be keyed by SA2 so that households
  # from the same sa2 are contiguous
  setkey(aus, sa2)
  stopifnot(haskey(aus), identical(first(key(aus)), "sa2"))
  aus[, stopifnot(is.integer(Status),
                  is.integer(sa2))]

  # Quicker to do it this way(!)
  aus[nSupermarkets_by_sa2, nSupermarketsAvbl := i.nSupermarkets, on = "sa2"]

  # Turn School Id into short id to use for school id
  aus[complete.cases(school_id),
      short_school_id := match(school_id, sort(unique(school_id), na.last = TRUE))]



  EpiPars <- set_epipars_defaults(EpiPars)

  out <-
    with(aus,
         do_au_simulate(Status,
                        InfectedOn,
                        sa2,
                        Age = Age,
                        School = short_school_id,
                        PlaceTypeBySA2 = integer(0),
                        Employment = Age, # not implemented
                        Resistance = Resistance,
                        Policy = Policy,
                        nPlacesByDestType = nPlacesByDestType,
                        FreqsByDestType = FreqsByDestType,
                        Epi = EpiPars,
                        nSupermarketsAvbl = nSupermarketsAvbl,
                        yday_start = .first_day,
                        days_to_sim = days_to_simulate,
                        N = nrow(aus)))

  hh_ss("final")
  out
}


set_epipars_defaults <- function(EpiPars = list()) {
  "%||%" <- function(a, b) if (is.null(a)) b else a
  get_epi_arg <- function(nom, default) {
    v <- EpiPars[[nom]] %||% default
    if (is.integer(default)) {
      checkmate::assert_integerish(v, any.missing = FALSE, len = 1L)
      v <- as.integer(v)
    }
    assign(paste0(".", nom), value = v, envir = parent.frame())
  }
  get_epi_arg("asympto", 0.48)
  get_epi_arg("duration_active", 13L)
  get_epi_arg("lambda_infectious", 10L)
  get_epi_arg("cau_l", 2)
  get_epi_arg("cau_s", 0.01)
  get_epi_arg("incubation_m", 5)
  get_epi_arg("illness_m", 15)

  list(CHECKED = TRUE,
       asympto = .asympto,
       duration_active = .duration_active,
       lambda_infectious = .lambda_infectious,
       cau_l = .cau_l,
       cau_s = .cau_s,
       incubation_m = .incubation_m,
       illness_m = .illness_m)
}

get_epi_arg <- function(nom, default, List) {
  assign(nom, value = List[[paste0(".", nom)]] %||% default, envir = parent.frame())
}

set_policy_defaults <- identity ## temp

