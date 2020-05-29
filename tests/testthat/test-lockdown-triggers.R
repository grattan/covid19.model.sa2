test_that("lockdown triggers work", {
  skip_if(is32bit())
  skip_on_travis()
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  library(data.table)

  aus <- read_typical()

  # Keep all status in one school. Need a largeish number
  # but enough so that infectors are visible
  aus[, Status := 0L]
  aus[, InfectedOn := NA_integer_]
  aus[sa2 == 116021310L & !is.na(short_school_id), # (large school-population sa2)
      Status := fifelse((pid %% 13L) == 0L, 1L, 0L)]

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
                q_school = 0.005,
                resistance_threshold = 1000L,
                p_asympto = 0.5,
                p_critical = 0,
                p_death = 0)

  simulate_minimal <- function(Policy, SeedOriginal = rep(1L, 500L), days_to_sim = 24) {
    aus <- copy(aus)
    N <- nrow(aus)

    do_au_simulate(Status = dollars(aus, Status),
                   InfectedOn = dollars(aus, InfectedOn),
                   SA2 = dollars(aus, sa2),
                   hid = dollars(aus, hid),
                   Age = dollars(aus, Age),
                   School = dollars(aus, short_school_id),
                   DZN = dollars(aus, short_dzn),
                   wid = dollars(aus, wid),
                   nColleagues = dollars(aus, nColleagues),
                   PlaceTypeBySA2 = integer(0),
                   LabourForceStatus = dollars(aus, LabourForceStatus),
                   SeedOriginal = SeedOriginal,
                   Policy = Policy,
                   MultiPolicy = list(),
                   nPlacesByDestType = nPlacesByDestType,
                   Epi = Epi,
                   Incubation = rep(14L, nrow(aus)),
                   Illness = rep(1L, nrow(aus)),
                   nSupermarketsAvbl = integer(nrow(aus)),
                   SupermarketTypical = integer(nrow(aus)),
                   minPlaceID_nPlacesByDestType = minPlaceID_nPlacesByDestType,
                   yday_start = 135L,  # a Thursday (one day of pre-trigger school)
                   days_to_sim = days_to_sim,
                   N = N,
                   display_progress = getOption("covid19.showProgress", 0L),
                   optionz = getOption("optionz", 0L),
                   nThread = getOption("covid19.model.sa2_nThread", 1L))
  }

  withr::with_seed(40, {
    S_w_lockdown <- simulate_minimal(Policy_w_lockdown)
  })
  withr::with_seed(40, {
    S_no_lockdown <- simulate_minimal(Policy_no_lockdown)
  })
  expect_equal(S_w_lockdown$nInfected[20], 0L)
  expect_gt(S_no_lockdown$nInfected[20], 10L)
})
