#' Set epidemiological parameters
#' @description Used to supply epidemiological parameters to the main
#' simulate function, with defaults.
#'
#' @param incubation_distribution One of `"pois"`, `"lnorm"`, and `"dirac"`,
#' whether the incubation period is Poisson, log-normal, or constant.
#'
#' @param incubation_mean The intended average of the distribution, must be
#' positive, and a whole number for `"pois"` and `"diract"`.
#' @param incubation_sigma A measure of the spread of the distribution.
#'
#' @param illness_distribution,illness_mean,illness_sigma As for \code{incubation} above.
#'
#' @param r_distribution The distribution of the number of infections from each
#' infected person, one of `"cauchy"`, `"lnorm"`, `"pois"`, or `"dirac"`.
#'
#' @param r_location,r_scale Parameters for \code{r_distribution}. The number of
#' infections \strong{per day}.
#'
#' @param resistance_threshold An integer in \code{[0, 1000]}, the resistance
#' required to not be infected. A value of 0
#' means no-one will be infected; a value of 1000 means everyone will.
#'
#' @param p_asympto A number in \code{[0, 1]}, the proportion of cases that
#' are asymptomatic.
#' @param p_critical A number in \code{[0, 1]}, the proportion of
#' \strong{symptomatic} patients that require ICU.
#' @param p_death A number in \code{[0, 1]}, the proportion of
#' \strong{critical cases} that die.
#'
#' @return A list of the components, plus an entry \code{CHECKED} having the
#' value \code{TRUE}.
#' @export


set_epipars <- function(incubation_distribution = c("pois", "lnorm", "dirac"),
                        incubation_mean = 5,
                        incubation_sigma = 0.44,
                        illness_distribution = c("pois", "lnorm", "dirac"),
                        illness_mean = 15,
                        illness_sigma = 1,
                        r_distribution = c("cauchy", "lnorm", "pois", "dirac"),
                        r_location = 2/5,
                        r_scale = 1,
                        r_schools_distribution = r_distribution,
                        r_schools_location = r_location,
                        r_schools_scale = r_scale,
                        r_supermarket_location = r_location,
                        r_supermarket_scale = r_scale,
                        resistance_threshold = 400L,
                        p_asympto = 0.48,
                        p_critical = 0.02,
                        p_death = 0.01) {
  incubation_distribution <- match.arg(incubation_distribution)
  switch(incubation_distribution,
         "pois" = {
           checkmate::assert_number(incubation_mean, lower = 0)
         },
         "lnorm" = {
           checkmate::assert_number(incubation_mean, finite = TRUE, lower = 1)
           checkmate::assert_number(incubation_sigma, finite = TRUE, lower = 0)
         },
         "dirac" = {
           checkmate::check_int(incubation_mean, lower = 1)
         })

  illness_distribution <- match.arg(illness_distribution)
  switch(illness_distribution,
         "pois" = {
           checkmate::assert_int(illness_mean, lower = 1)
         },
         "lnorm" = {
           checkmate::assert_number(illness_mean, finite = TRUE, lower = 0)
           checkmate::assert_number(illness_sigma, finite = TRUE, lower = 0)
         },
         "dirac" = {
           checkmate::check_int(illness_mean, finite = TRUE, lower = 1)
         })

  r_distribution <- match.arg(r_distribution)
  if (r_distribution == "dirac") {
    # integerish
    checkmate::assert_int(r_location, lower = 1)
    checkmate::assert_int(r_supermarket_location, lower = 1)
    checkmate::assert_int(r_schools_location, lower = 1)
  } else {
    checkmate::assert_number(r_location, finite = TRUE, lower = 0)
    checkmate::assert_number(r_supermarket_location, finite = TRUE, lower = 0)
    checkmate::assert_number(r_schools_location, finite = TRUE, lower = 0)
    checkmate::assert_number(r_scale, finite = TRUE, lower = 0)
  }

  checkmate::assert_number(p_asympto, finite = TRUE, lower = 0, upper = 1)
  checkmate::assert_number(p_critical, finite = TRUE, lower = 0, upper = 1)
  checkmate::assert_number(p_death, finite = TRUE, lower = 0, upper = 1)

  # Convert to integer to 1000
  p_asympto <- as.integer(1000 * p_asympto)
  p_critical <- as.integer(1000 * p_critical)
  p_death <- as.integer(1000 * p_death)

  # Convert to int for convenience
  incubation_distribution <- match_distr(incubation_distribution)
  illness_distribution <- match_distr(illness_distribution)
  r_distribution <- match_distr(r_distribution)

  checkmate::assert_number(resistance_threshold,
                           lower = 0,
                           upper = 1000)

  CHECKED <- TRUE

  mget(ls())
}

# to integer
match_distr <- function(arg, choices = c("pois", "lnorm", "dirac", "cauchy")) {
  match(arg, choices)
}
