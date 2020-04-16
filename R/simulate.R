#' Simulate this
#' @export


simulate_sa2 <- function(days_to_simulate = 300,
                         InitialStatus = list(dead = 63,
                                              healed = 3747,
                                              active = 2592,
                                              critical = 66),
                         CauchyM = integer(0),  # allow changes every day
                         .population = 25e6) {
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

  read_fst <- function(...) fst::read_fst(..., as.data.table = TRUE)

  aus <- read_fst("data-raw/int/australia.fst")
  sa2_by_hid <-
    read_fst("data-raw/int/house.fst") %>%
    .[read_fst("data-raw/int/sa2_codes.fst", columns = c("sa2_name", "sa2")),
      sa2 := as.integer(i.sa2),
      on = "sa2_name"] %>%
    .[, sa2_name := NULL] %>%
    .[]

  demo_by_person <- read_fst("data-raw/int/person_demography.fst")



  # For text width
  IS <- InitialStatus
  n_status0 <- nrow(aus) - sum(unlist(IS))
  samp_status <-
    sample(c(-2L, -1L, 0L, 1L, 2L),
           size = nrow(aus),
           replace = TRUE,
           prob = c(IS$dead, IS$healed, n_status0, IS$active, IS$critical) / nrow(aus))


  aus[, Status := samp_status]
  aus[sa2_by_hid, SA2 := i.sa2, on = "hid"]
  aus[demo_by_person, Age := i.age, on = "pid"]
  aus[, Resistance := rep_len(samp(1:6000, size = 13381L), .N)]

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
  setkey(aus, SA2)
  stopifnot(haskey(aus), identical(first(key(aus)), "SA2"))
  aus[, stopifnot(is.integer(Status),
                  is.integer(SA2))]


  aus[, do_au_simulate(Status,
                       SA2,
                       Age,
                       PlaceTypeBySA2 = integer(0),
                       Employment = Age, # not implemented
                       Resistance,
                       CauchyM = CauchyM,
                       nPlacesByDestType = nPlacesByDestType,
                       FreqsByDestType = FreqsByDestType,
                       yday_start = 1L,
                       days_to_sim = days_to_simulate,
                       N = nrow(aus))]
}


