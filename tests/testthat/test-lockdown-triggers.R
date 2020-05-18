test_that("lockdown triggers work", {
  skip_if(is32bit())
  skip_if_not_installed("data.table")
  library(data.table)

  aus <- read_typical()

  # Keep all status in one school. Need a largeish number
  # but enough so that infectors are visible
  aus[, Status := 0L]
  aus[, InfectedOn := NA_integer_]
  aus[sa2 == 116021310L & !is.na(short_school_id), # (large school-population sa2)
      Status := fifelse(pid %in% head(pid, 1000L) == 0L, 1L, 0L)]

  # Set the InfectedOn to day before start day
  aus[Status == 1L, InfectedOn := 134L]

  nPlacesByDestType <-
    lapply(1:106, function(i) {
      if (i == 98L) {
        read_sys("nSupermarkets_by_sa2.fst",
                 columns = "nSupermarkets")[[1L]]
      } else {
        integer(0)
      }
    })

  # Times per year each person visits the matching type
  weekly <- rep_len(52L, nrow(aus))

  FreqsByDestType <-
    lapply(1:106, function(i) {
      if (i == 15L) {
        # cafes
        # assume uniformly n/week
        cafe <- 52L * (0:7)
        return(rep_len(cafe, nrow(aus)))
      }
      if (i == 98L) {
        ## Assume supermarket visits are beta distributed
        rep_len(as.integer(360 * rbeta(1e6, 3, 1)), nrow(aus))
      } else {
        weekly
      }
    })



  INT_MAX <- .Machine$integer.max
  no_lockdown_triggers <-
    set_lockdown_triggers__schools(default_schools_with_infections = INT_MAX,
                                   default_schools_with_infections_geq = INT_MAX,
                                   default_schools_with_infections_duration_of_lockdown = 0L,
                                   default_schools_with_any_critical = INT_MAX,
                                   default_schools_with_any_critical_duration_of_lockdown = 0L)

  # Just one school in order to trigger immediately
  lockdown_triggers <-
    set_lockdown_triggers__schools(default_schools_with_infections = 1L,
                                   default_schools_with_infections_geq = 1L,
                                   default_schools_with_infections_duration_of_lockdown = 56L)

  Policy_w_lockdown <-
    set_policypars(supermarkets_open = FALSE,
                   workplaces_open = 0,
                   cafes_open = FALSE,
                   schools_open = TRUE,
                   do_contact_tracing = FALSE,
                   lockdown_triggers__schools = lockdown_triggers)

  Policy_no_lockdown <-
    set_policypars(supermarkets_open = FALSE,
                   workplaces_open = 0,
                   schools_open = TRUE,
                   cafes_open = FALSE,
                   do_contact_tracing = FALSE,
                   lockdown_triggers__schools = no_lockdown_triggers)

  Epi <-
    set_epipars(incubation_distribution = "dirac",
                incubation_mean = 14L,
                illness_distribution = "dirac",
                illness_mean = 1L,
                a_household_rate = 0,
                a_schools_rate = 1,
                q_school = 0.25,
                r_distribution = "dirac",
                r_location = 1,
                r_schools_distribution = "dirac",
                r_schools_location = 1,
                p_asympto = 0.5,
                p_critical = 0,
                p_death = 0)

  simulate_minimal <- function(Policy, days_to_sim = 24) {
    aus <- copy(aus)
    N <- nrow(aus)



    do_au_simulate(Status = dollars(aus, Status),
                   InfectedOn = dollars(aus, InfectedOn),
                   State = dollars(aus, state),
                   SA2 = dollars(aus, sa2),
                   hid = dollars(aus, hid),
                   seqN = dollars(aus, seqN),
                   HouseholdSize = dollars(aus, HouseholdSize),
                   Age = dollars(aus, Age),
                   School = dollars(aus, short_school_id),
                   DZN = dollars(aus, short_dzn),
                   wid = dollars(aus, wid),
                   nColleagues = dollars(aus, nColleagues),
                   PlaceTypeBySA2 = integer(0),
                   LabourForceStatus = dollars(aus, LabourForceStatus),
                   Resistance = integer(N),  # No resistance

                   # TODO: this should work with a 'fixed' seed (n.b. Seed[1] == 0 means no seed)
                   Seed = integer(2048),

                   Policy = Policy,
                   nPlacesByDestType = nPlacesByDestType,
                   FreqsByDestType = FreqsByDestType,
                   Epi = Epi,
                   nSupermarketsAvbl = integer(nrow(aus)),
                   SupermarketTypical = integer(nrow(aus)),
                   minPlaceID_nPlacesByDestType = minPlaceID_nPlacesByDestType,
                   yday_start = 135L,  # a Thursday (one day of pre-trigger school)
                   days_to_sim = days_to_sim,
                   N = N,
                   display_progress = getOption("covid19.showProgress", 0L),
                   optionz = 0L,
                   nThread = getOption("covid19.model.sa2_nThread", 1L))
  }

  S_w_lockdown <- simulate_minimal(Policy_w_lockdown)
  S_no_lockdown <- simulate_minimal(Policy_no_lockdown)

  expect_equal(S_w_lockdown$nInfected[20], 0L)
  expect_gt(S_no_lockdown$nInfected[20], 10L)
})
