#' Set triggers for changes in school policy
#' @description Intended to be passed to \code{\link{set_policypars}}.
#'
#' @name set_policypars
#'
#' @param do_school_lockdown If \code{FALSE}, no lockdown occurs and
#' other arguments will be ignored.
#'
#' @param default_schools_with_infections The number of schools with
#' at least `default_schools_with_infections_geq` infections
#' to trigger a lockdown.
#' @param default_schools_with_infections_geq The minimum number of infections
#' a school needs to have to trigger a lockdown criterion.
#'
#' @param default_schools_with_infections_duration_of_lockdown The duration in
#' days of the lockdown so triggered.
#'
#' @param default_schools_with_any_critical The number of schools with
#' any critical status pupils.
#' @param default_schools_with_any_critical_duration_of_lockdown The duration in
#' day of the lockdown so triggered.
#'
#' @export set_lockdown_triggers__schools
set_lockdown_triggers__schools <- function(do_school_lockdown = TRUE,
                                           default_schools_with_infections = 4L,
                                           default_schools_with_infections_geq = 3L,
                                           default_schools_with_infections_duration_of_lockdown = 28L,
                                           default_schools_with_any_critical = 1L,
                                           default_schools_with_any_critical_duration_of_lockdown = 91L) {
  Default <- mget(ls())
  setNames(lapply(states(), function(s) unlist(Default)),
           states())
}
