% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_epipars.R
\name{set_epipars}
\alias{set_epipars}
\title{Set epidemiological parameters}
\source{
\url{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30287-5/fulltext}
for the attack rates.

See also AceMOD

\describe{
\item{a_household_rate}{12.1 to 18.2 or (9.1 to 13.8 assuming missing tests were uninfected).}
}
}
\usage{
set_epipars(
  incubation_distribution = c("pois", "lnorm", "dirac", "cauchy"),
  incubation_mean = 8,
  incubation_sigma = 0.44,
  illness_distribution = c("pois", "lnorm", "dirac"),
  illness_mean = 15,
  illness_sigma = 1,
  a_workplace_rate = 0.2,
  a_household_rate = 0.05,
  a_schools_rate = 0.4,
  q_workplace = 0.01,
  q_household = 0.1,
  q_school = 1/5000,
  q_school_grade = 1/500,
  q_supermarket = 1/1000,
  q_places = 1/1000,
  q_major_event = 1/10000,
  ResistanceByAge = seq(0.85, 0.4, length.out = 101),
  p_asympto = 0.35,
  p_critical = c(0.017, 0.045, 0.074),
  p_death = c(0.0294, 0.0444, 0.1757),
  p_visit_major_event = 1/52,
  supermarket_beta_shape1 = 3,
  supermarket_beta_shape2 = 1
)
}
\arguments{
\item{incubation_distribution}{One of `"pois"`, `"lnorm"`, and `"dirac"`,
whether the incubation period is Poisson, log-normal, or constant.}

\item{incubation_mean}{The intended average of the distribution, must be
positive, and a whole number for `"pois"` and `"dirac"`.}

\item{incubation_sigma}{A measure of the spread of the distribution.}

\item{illness_distribution, illness_mean, illness_sigma}{As for \code{incubation} above.}

\item{a_workplace_rate}{\code{double(1)} in \code{[0, 1]}, the proportion
of workplaces in which transmission between colleagues is possible.}

\item{a_household_rate}{\code{double(1)} in \code{[0, 1]}, the
proportion of households in which household transmission is possible.}

\item{a_schools_rate}{\code{double(1)} in \code{[0, 1]}, the proportion of
schools in which tranmission between students is possible.}

\item{q_household, q_school, q_school_grade, q_workplace}{Daily transmission probability among household members / students of the same school / students of the same school and same same
grade / workers of the same workplace.}

\item{q_supermarket, q_places, q_major_event}{As above, for supermarket and places.}

\item{ResistanceByAge}{\describe{
\item{\code{double(101)}}{For each age, the proportion of individuals who are naturally resistant
to the disease. If a person is naturally resistant then an otherwise successful contact with
and infected person does not cause an infection. Not only do resistant individuals
not fall ill, they are also not infectious.}
}}

\item{p_asympto}{A number in \code{[0, 1]}, the proportion of cases that
are asymptomatic.}

\item{p_critical}{\describe{
\item{\code{double(3)}}{A vector with each element in \code{[0, 1]}, the proportion of
\strong{symptomatic} patients that require ICU, by age. The first element
is the proportion aged 0-49, the second 50-64, and the third, 65+.}
}}

\item{p_death}{\describe{
\item{\code{double(3)}}{A vector with each element in \code{[0, 1]}, the proportion of
\strong{critical cases} that die, by age.
The first element
is the proportion aged 0-49, the second 50-64, and the third, 65+.}
}}

\item{p_visit_major_event}{Probability a person attends a major event on a given day.}

\item{supermarket_beta_shape1, supermarket_beta_shape2}{Parameters for the
\code{rbeta} function that determines the distribution of visit frequency
to supermarkets.}
}
\value{
A list of the components, plus an entry \code{CHECKED} having the
value \code{TRUE}.
}
\description{
Used to supply epidemiological parameters to the main
simulate function, with defaults.

Note that all parameters are taken together. In particular, attack probabilities,
transmission probabilities, and reproduction parameters affect the overall
transmission multiplicatively .
}
