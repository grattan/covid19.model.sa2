test_that("a_household_rate", {
  skip_if(is32bit())
  skip_on_travis()
  skip_if_not_installed("magrittr")
  skip_if_not_installed("data.table")
  library(magrittr)
  library(data.table)

  # Policy at maximal lockdown to isolate household effects
  Policy <- set_policypars(supermarkets_open = FALSE,
                           cafes_open = FALSE,
                           do_contact_tracing = FALSE)
  Epi000 <- set_epipars(a_household_rate = 0.0,
                        incubation_distribution = "dirac",
                        incubation_mean = 4,
                        illness_distribution = "dirac",
                        illness_mean = 4,
                        r_distribution = "dirac",
                        q_household = 1,
                        r_location = 2,
                        r_schools_location = 1)
  Epi050 <- set_epipars(a_household_rate = 0.5,
                        incubation_distribution = "dirac",
                        incubation_mean = 4,
                        illness_distribution = "dirac",
                        illness_mean = 4,
                        r_distribution = "dirac",
                        q_household = 1,
                        r_location = 2,
                        r_schools_location = 1)
  Epi100 <- set_epipars(a_household_rate = 1,
                        incubation_distribution = "dirac",
                        incubation_mean = 4,
                        illness_distribution = "dirac",
                        illness_mean = 4,
                        r_distribution = "dirac",
                        q_household = 1,
                        r_location = 2,
                        r_schools_location = 1)

  aus <- read_sys("australia.fst")
  aus[, c("seqN", "HouseholdSize") := do_seqN_N(hid, pid)]
  aus[, Status := 0L]
  aus[pid < 200e3L, Status := fifelse(seqN == 1L & HouseholdSize >= 2L & runif(.N) > 0.5, 1L, 0L)]
  aus[Status != 0L, InfectedOn := rep_len(98:100, .N)]
  aus[, Age := 42L]
  aus[sample.int(.N, size = 3135825L), School := rep_len(1:9501, .N)]
  aus[, DZN := 1L]
  aus[, wid := 1L]
  aus[, nColleagues := 1L]
  aus[, LabourForceStatus := 1L]
  aus[, Resistance := 000L]


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
        return(dqrng::dqsample(cafe, size = 1e6, replace = TRUE))
      }
      if (i == 98L) {
        ## Assume supermarket visits are beta distributed
        rep_len(as.integer(360 * rbeta(1e6, 3, 1)), nrow(aus))
      } else {
        weekly
      }
    })

  FreqsByDestType <-
    lapply(FreqsByDestType, function(x) rep_len(as.integer(x), nrow(aus)))

  simulate_minimal <- function(epi, days_to_sim = 14L) {
    aus <- copy(aus)
    do_au_simulate(Status = aus$Status,
                   InfectedOn = aus$InfectedOn,
                   State = aus$state,
                   SA2 = aus$sa2,
                   hid = aus$hid,
                   seqN = aus$seqN,
                   HouseholdSize = aus$HouseholdSize,
                   Age = aus$Age,
                   School = aus$School,
                   DZN = aus$DZN,
                   wid = aus$wid,
                   nColleagues = aus$nColleagues,
                   PlaceTypeBySA2 = integer(0),
                   LabourForceStatus = aus$LabourForceStatus,
                   Resistance = aus$Resistance,
                   Seed = integer(2048),
                   Policy = Policy,
                   nPlacesByDestType = nPlacesByDestType,
                   FreqsByDestType = FreqsByDestType,
                   Epi = epi,
                   nSupermarketsAvbl = integer(nrow(aus)),
                   SupermarketTypical = integer(nrow(aus)),
                   minPlaceID_nPlacesByDestType = minPlaceID_nPlacesByDestType,
                   yday_start = 100L,
                   days_to_sim = days_to_sim,
                   N = nrow(aus),
                   nThread = getOption("covid19.model.sa2_nThread", 1L)) %>%
      .[["nInfected"]]
  }
  S_a_h_000 <- simulate_minimal(Epi000)
  S_a_h_050 <- simulate_minimal(Epi050)
  S_a_h_100 <- simulate_minimal(Epi100)
  expect_equal(S_a_h_000[1], S_a_h_050[1])
  expect_equal(S_a_h_000[1], S_a_h_100[1])
  expect_true(all(S_a_h_000 <= S_a_h_050))
  expect_true(all(S_a_h_050 <= S_a_h_100))

})
