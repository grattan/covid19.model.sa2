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
#' @param ResistanceByAge \describe{
#' \item{\code{double(101)}}{For each age, the proportion of individuals who are naturally resistant
#' to the disease. If a person is naturally resistant then an otherwise successful contact with
#' and infected person does not cause an infection. Not only do resistant individuals
#' not fall ill, they are also not infectious.}
#' }
#'
#' @param p_asympto A number in \code{[0, 1]}, the proportion of cases that
#' are asymptomatic.
#' @param p_critical
#' \describe{
#' \item{\code{double(3)}}{A vector with each element in \code{[0, 1]}, the proportion of
#' \strong{symptomatic} patients that require ICU, by age. The first element
#' is the proportion aged 0-49, the second 50-64, and the third, 65+.}
#' }
#' @param p_death
#' \describe{
#' \item{\code{double(3)}}{A vector with each element in \code{[0, 1]}, the proportion of
#' \strong{critical cases} that die, by age.
#' The first element
#' is the proportion aged 0-49, the second 50-64, and the third, 65+.}
#' }
#'
#' @param p_visit_major_event Probability a person attends a major event on a given day.
#'
#' @param supermarket_beta_shape1,supermarket_beta_shape2 Parameters for the
#' \code{rbeta} function that determines the distribution of visit frequency
#' to supermarkets.
#'
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
                        incubation_mean = 8,
                        incubation_sigma = 0.44,
                        illness_distribution = c("pois", "lnorm", "dirac"),
                        illness_mean = 15,
                        illness_sigma = 1,
                        a_workplace_rate = 0.20,
                        a_household_rate = 0.05,
                        a_schools_rate = 0.40,
                        q_workplace = 0.01,
                        q_household = 0.10,
                        q_school = 1/5000,
                        q_school_grade = 1/500,
                        q_supermarket = 1/1000,
                        q_places = 1/1000,
                        q_major_event = 1/10000,
                        ResistanceByAge = seq(0.85, 0.4, length.out = 101),
                        p_asympto = 0.35,
                        p_critical = c(0.017,
                                       0.045,
                                       0.074),
                        p_death = c(0.0294,
                                    0.0444,
                                    0.1757),
                        p_visit_major_event = 1/52,
                        supermarket_beta_shape1 = 3,
                        supermarket_beta_shape2 = 1) {
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

  checkmate::check_double(p_critical,
                          max.len = 3, min.len = 1,
                          lower = 0, upper = 1)
  checkmate::check_double(p_death,
                          max.len = 3, min.len = 1,
                          lower = 0, upper = 1)
  if (length(p_critical) != 3L) {
    if (length(p_critical) != 1L) {
      stop("`p_critical` was length-{length(p_critical)} must be length 3.")
    }
    p_critical <- rep_len(p_critical, 3)
  }
  if (length(p_death) != 3L) {
    if (length(p_death) != 1L) {
      stop("`p_death` was length-{length(p_critical)} must be length 3.")
    }
    p_death <- rep_len(p_death, 3)
  }


  # Convert to int for convenience
  incubation_distribution <- match_distr(incubation_distribution)
  illness_distribution <- match_distr(illness_distribution)

  checkmate::assert_double(ResistanceByAge,
                           lower = 0,
                           upper = 1,
                           finite = TRUE,
                           any.missing = FALSE,
                           len = 101)

  checkmate::assert_number(supermarket_beta_shape1, lower = 0, finite = TRUE)
  checkmate::assert_number(supermarket_beta_shape2, lower = 0, finite = TRUE)

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





