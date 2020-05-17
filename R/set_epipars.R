#' Set epidemiological parameters
#' @description Used to supply epidemiological parameters to the main
#' simulate function, with defaults.
#'
#' Note that all parameters are taken together. In particular, attack probabilities,
#' transmission probabilities, and reproduction parameters affect the overall
#' transmission multiplicatively .
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
#' @param a_household_rate \code{double(1)} in \code{[0, 1]}, the
#' proportion of households in which household transmission is possible.
#'
#' @param a_workplace_rate \code{double(1)} in \code{[0, 1]}, the proportion
#' of workplaces in which transmission between colleagues is possible.
#'
#' @param a_schools_rate \code{double(1)} in \code{[0, 1]}, the proportion of
#' schools in which tranmission between students is possible.
#'
#' @param q_household Daily transmission probability among household members.
#' @param q_school Daily transmission probability among students of the same school.
#' @param q_school_grade Daily transmission probability among students of the same
#' school and age.
#'
#' @param r_distribution The distribution of the number of infections from each
#' infected person, one of `"cauchy"`, `"lnorm"`, `"pois"`, or `"dirac"`.
#'
#' @param r_location,r_scale Parameters for \code{r_distribution}. The number of
#' infections \strong{per day}.
#'
#' If \code{r_distribution = "dirac"} there is no distribution;
#' the infection will be deterministic. If an integer is provided, all infectious
#' persons infect the same number of people. Can also be provided in the form
#' \code{a/b} where \code{a} and {b} are whole numbers, in which case each
#' infectious person will infect \code{a} individuals every \code{b} days
#' (precisely, not on average).
#'
#' @param r_schools_distribution,r_schools_location,r_schools_scale Variables
#' particular for schools.
#'
#' @param r_supermarket_location,r_supermarket_scale Variables particular
#' for supermarket.
#'
#' @param resistance_threshold An integer in \code{[0, 1000]}, the resistance
#' required to not be infected. A value of 0
#' means no-one will be infected; a value of 1000 means everyone will.
#'
#' @param r_work_location Parameters for work infection distribution.
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
#'
#' @source \url{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30287-5/fulltext}
#' for the attack rates.
#'
#' See also AceMOD
#'
#' \describe{
#' \item{a_household_rate}{12.1 to 18.2 or (9.1 to 13.8 assuming missing tests were uninfected).}
#' }
#'
#'
#'
#' @export


set_epipars <- function(incubation_distribution = c("pois", "lnorm", "dirac"),
                        incubation_mean = 5,
                        incubation_sigma = 0.44,
                        illness_distribution = c("pois", "lnorm", "dirac"),
                        illness_mean = 15,
                        illness_sigma = 1,
                        a_rate = 0.07,
                        a_workplace_rate = 0.07,
                        a_household_rate = 0.15,
                        a_schools_rate = 0.07,
                        q_workplace = 0.01,
                        q_household = 0.05,
                        q_school = 1/3000,
                        q_school_grade = 1/500,
                        r_distribution = c("cauchy", "lnorm", "pois", "dirac"),
                        r_location = 2/5,
                        r_scale = 1/5,
                        r_schools_distribution = r_distribution,
                        r_schools_location = r_location * 0.25,
                        r_schools_scale = r_scale,
                        r_supermarket_location = r_location,
                        r_supermarket_scale = r_scale,
                        r_work_location = r_location,
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
           checkmate::assert_number(illness_mean, finite = TRUE, lower = 0)
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
           checkmate::assert_number(illness_mean, finite = TRUE, lower = 0)
         })

  checkmate::assert_number(a_workplace_rate, lower = 0, upper = 1)
  checkmate::assert_number(a_household_rate, lower = 0, upper = 1)
  checkmate::assert_number(a_schools_rate, lower = 0, upper = 1)

  checkmate::assert_number(q_household, lower = 0, upper = 1)
  checkmate::assert_number(q_school, lower = 0, upper = 1)
  checkmate::assert_number(q_school_grade, lower = 0, upper = 1)
  q_household <- percentage_to_int32(q_household)
  q_school <- percentage_to_int32(q_school)
  q_school_grade <- percentage_to_int32(q_school_grade)

  q_household <- checkmate::assert_int(q_household, coerce = TRUE)
  q_school <- checkmate::assert_int(q_school, coerce = TRUE)
  q_school_grade <- checkmate::assert_int(q_school_grade, coerce = TRUE)


  r_distribution <- match.arg(r_distribution)
  if (r_distribution == "dirac") {
    if (is.double(r_location) &&
        is.call(substitute(r_location)) &&
        length(substitute(r_location)) == 3L &&
        as.character(substitute(r_location))[[1]] == "/") {
      # e.g. 1/5
      dirac_num <- eval(substitute(r_location)[[2]])
      dirac_per <- eval(substitute(r_location)[[3]])
      checkmate::assert_int(dirac_num, lower = 1)
      checkmate::assert_int(dirac_per, lower = 1)
    } else {
      # integerish
      checkmate::assert_int(r_location, lower = 1)
      checkmate::assert_int(r_supermarket_location, lower = 1)
      checkmate::assert_int(r_schools_location, lower = 1)
    }
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

distrs <- function() {
  c("pois", "lnorm", "dirac", "cauchy")
}

# to integer
match_distr <- function(arg) {
  match(arg, distrs())
}

decode_distr <- function(d) {
  if (is.integer(d)) {
    return(distrs()[d])
  }
  # may already be decoded
  if (!anyNA(match_distr(d))) {
    return(d)
  }

  stop("Internal error: decode_distr() encountered bad encoding.\n\t",
       "d[1] = ", d[1])
}

percentage_to_int32 <- function(p) {
  out <-
    p * 2 * .Machine$integer.max +
    (-.Machine$integer.max)
  out <- as.integer(out)
  stopifnot(!anyNA(out), is.integer(out))
  out
}





