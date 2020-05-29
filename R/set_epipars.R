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
#' positive, and a whole number for `"pois"` and `"dirac"`.
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
#' @param q_household,q_school,q_school_grade,q_workplace Daily transmission probability among household members / students of the same school / students of the same school and same same
#' grade / workers of the same workplace.
#'
#' @param q_supermarket,q_places,q_major_event As above, for supermarket and places.
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
#' @param p_visit_major_event Probability a person attends a major event on a given day.
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


set_epipars <- function(incubation_distribution = c("pois", "lnorm", "dirac", "cauchy"),
                        incubation_mean = 5,
                        incubation_sigma = 0.44,
                        illness_distribution = c("pois", "lnorm", "dirac"),
                        illness_mean = 15,
                        illness_sigma = 1,
                        a_workplace_rate = 0.07,
                        a_household_rate = 0.15,
                        a_schools_rate = 0.07,
                        q_workplace = 0.01,
                        q_household = 0.05,
                        q_school = 1/3000,
                        q_school_grade = 1/500,
                        q_supermarket = 1/500,
                        q_places = 1/500,
                        q_major_event = 1/5000,
                        resistance_threshold = 400L,
                        p_asympto = 0.48,
                        p_critical = 0.02,
                        p_death = 0.01,
                        p_visit_major_event = 1/52) {
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
  checkmate::assert_number(q_supermarket, lower = 0, upper = 1)
  checkmate::assert_number(q_workplace, lower = 0, upper = 1)


  checkmate::assert_number(p_asympto, finite = TRUE, lower = 0, upper = 1)
  checkmate::assert_number(p_critical, finite = TRUE, lower = 0, upper = 1)
  checkmate::assert_number(p_death, finite = TRUE, lower = 0, upper = 1)

  # Convert to int for convenience
  incubation_distribution <- match_distr(incubation_distribution)
  illness_distribution <- match_distr(illness_distribution)

  checkmate::assert_number(resistance_threshold,
                           lower = 0,
                           upper = 1000)
  resistance_threshold <- resistance_threshold / 1000

  lapply(ls(), function(x) {
    if (is.null(get(x))) {
      stop(x, " was NULL.")
    }
  })

  CHECKED <- TRUE

  mget(unique(c("CHECKED", ls(sorted = TRUE))))
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





