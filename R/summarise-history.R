

new_cases_by_date <- function(.date, .state, incl_overseas = FALSE) {
  if (isTRUE(incl_overseas)) {
    r <- 1
  } else if (isFALSE(incl_overseas)) {
    r <- 1 - p_quarantine_by_date(.date)
  } else {
    stop("`incl_overseas` was not TRUE or FALSE.")
  }


  if (is.null(.state) || identical(.state, "Total") || identical(.state, "all")) {
    out <-
      read_sys("time_series_cases.fst",
               columns = c("Date", "Total")) %>%

      .[, .(NewCases = diff(Total),
            Date = Date[-1])] %>%
      setkeyv("Date") %>%
      .[.(.date)] %>%
      .[["NewCases"]]
    return(as.integer(round(out * r)))
  }

  if (is.integer(.state)) {
    .state <- states()[.state + 1L]
  }

  if (length(.date) != length(.state)) {
    if (length(.date) != 1 && length(.state) != 1) {
      stop("Lengths .date and .state != 1.")
    }
  }




  input <- data.table(Date = as.character(.date),
                      State = .state)
  input[, ordering := .I]

  out <-
    read_sys("time_series_cases.fst") %>%
    .[, Total := NULL] %>%
    melt.data.table(id.vars = c("Date"),
                    variable.factor = FALSE,
                    variable.name = "State",
                    value.name = "CumCases") %>%
    .[, Date := as.character(Date)] %>%
    .[, .(NewCases = diff(CumCases),
          Date = Date[-1]),
      keyby = .(State)] %>%
    setkeyv(c("Date", "State")) %>%
    .[input, on = c("Date", "State")] %>%
    setorderv('ordering') %>%
    .[["NewCases"]]

  return(as.integer(round(out * r)))
}







