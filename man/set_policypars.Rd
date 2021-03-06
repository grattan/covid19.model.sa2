% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_lockdown_triggers__schools.R,
%   R/set_policypars.R
\name{set_policypars}
\alias{set_policypars}
\alias{set_lockdown_triggers__schools}
\alias{set_policy_no_restrictions}
\title{Set triggers for changes in school policy}
\usage{
set_lockdown_triggers__schools(
  do_school_lockdown = TRUE,
  default_schools_with_infections = 4L,
  default_schools_with_infections_geq = 3L,
  default_schools_with_infections_duration_of_lockdown = 28L,
  default_schools_with_any_critical = 1L,
  default_schools_with_any_critical_duration_of_lockdown = 91L
)

set_policypars(
  yday_start = 0L,
  supermarkets_open = TRUE,
  schools_open = FALSE,
  only_Year12 = FALSE,
  school_days_per_wk = NULL,
  do_contact_tracing = TRUE,
  contact_tracing_days_before_test = 0L,
  contact_tracing_days_until_result = 3L,
  contact_tracing_only_sympto = TRUE,
  contact_tracing_success = 0.9,
  tests_by_state = NULL,
  max_persons_per_event = 5L,
  nPersonsByEvent = c(20000L, 5000, 1500L),
  n_major_events_weekday = 2L,
  n_major_events_weekend = 10L,
  max_persons_per_supermarket = 200L,
  cafes_open = TRUE,
  age_based_lockdown = integer(101),
  workplaces_open = FALSE,
  workplace_size_max = 1L,
  workplace_size_beta = 13,
  workplace_size_lmu = -1,
  workplace_size_lsi = -1,
  travel_outside_sa2 = FALSE,
  lockdown_triggers__schools = set_lockdown_triggers__schools(),
  isol_compliance = 0.7
)

set_policy_no_restrictions(...)
}
\arguments{
\item{do_school_lockdown}{If \code{FALSE}, no lockdown occurs and
other arguments will be ignored.}

\item{default_schools_with_infections}{The number of schools with
at least `default_schools_with_infections_geq` infections
to trigger a lockdown.}

\item{default_schools_with_infections_geq}{The minimum number of infections
a school needs to have to trigger a lockdown criterion.}

\item{default_schools_with_infections_duration_of_lockdown}{The duration in
days of the lockdown so triggered.}

\item{default_schools_with_any_critical}{The number of schools with
any critical status pupils.}

\item{default_schools_with_any_critical_duration_of_lockdown}{The duration in
day of the lockdown so triggered.}

\item{yday_start}{Optional. Specify the start date. Used in conjunction with
\code{MultiPolicy} argument to \code{simulate_sa2}.}

\item{supermarkets_open}{\code{TRUE|FALSE}. Should supermarkets remain open?}

\item{schools_open}{\code{FALSE|TRUE}. Should schools remain open?}

\item{only_Year12}{\code{FALSE|TRUE}. If schools open, should they be
restricted to Year 12 students only? No effect if \code{schools_open = FALSE}.}

\item{school_days_per_wk}{Specifies how many days a week pupils attend
school. By default, students attend full-time. Only applied after
\code{schools_open} and \code{only_Year12}. In particular, has no
effect if \code{schools_open = FALSE} and if \code{only_Year12 = TRUE} then
all other students attend 0 times per week.

If a single number, applies to all states. Otherwise must be a
named list or vector.

If a named vector, names must be state names.
(See \code{\link{states}}.) Omitted states take the value 5 (i.e.
full-time).

If a named list, each element must be a length-21 vector which
specifies the number of times per week students of each age 0-20
attend in the given state (with the last
element being the attendance per week of teachers).'}

\item{do_contact_tracing}{\code{TRUE|FALSE}. Should contact tracing occur?
If \code{FALSE} households are not isolated if tested.}

\item{contact_tracing_days_before_test}{The number of days following
the end of the incubation period
before the person gets tested.}

\item{contact_tracing_days_until_result}{The number of days between a test
and the result being known.}

\item{contact_tracing_only_sympto}{\code{TRUE|FALSE} Is contact tracing only
applied to symptomatic cases?}

\item{contact_tracing_success}{\describe{
\item{\code{double(1) : [0, 1]}}{The proportion of contacts successfully traced.}
}}

\item{tests_by_state}{\code{integer(10)} The number of tests per day that states
perform. First entry is the total tests available across Australia.}

\item{max_persons_per_event, n_major_events_weekday, n_major_events_weekend}{Policies around major events. Maximum of 255 major events per day and
values of `max_persons_per_event` below 1000 are rounded to zero.}

\item{nPersonsByEvent}{The number of persons to attend each event.}

\item{max_persons_per_supermarket}{Maximum number of people allowed in a
supermarket (within one hour i.e. concurrently).}

\item{cafes_open}{(TEMPORARY).}

\item{age_based_lockdown}{Integer vector of ages to lockdown. Either a vector
of the ages (0-100) or a length-101 vector specifying the ages to be lockdown
(as 1).}

\item{workplaces_open}{Are workplaces to be open?
Can be logical \code{FALSE} or \code{TRUE} or a number in \code{[0, 1]},
the proportion of workplaces that remain open.}

\item{workplace_size_max}{The maximum size of any workplace (we assume
that everyone interacts on a single day).}

\item{workplace_size_beta, workplace_size_lmu, workplace_size_lsi}{Parameters for the distribution of workplace sizes. \code{_beta} is the
rate distribution for the geometric distribution; \code{_lmu} and
\code{_lsi} are the parameters for the lognormal distribution.}

\item{travel_outside_sa2}{Should travel outside a person's SA2 be modelled?}

\item{lockdown_triggers__schools}{A list constructed by \code{\link{set_lockdown_triggers__schools}}.}

\item{isol_compliance}{Chance of a person in isolation going about their
normal business while in isolation.}

\item{...}{Arguments passed to \code{set_policypars}.}
}
\value{
A list of the components.
}
\description{
Intended to be passed to \code{\link{set_policypars}}.

Used to supply policy parameters to the main
\code{\link{simulate_sa2}} function,
with defaults.
}
