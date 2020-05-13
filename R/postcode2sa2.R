

postcode2sa2 <- function(postcode,
                         the_mult = c("first", "all", "last"),
                         n = length(postcode),
                         SA2_BY_POSTCODE = getOption("covid19.model.sa2.SA2_BY_POSTCODE")) {
  SA2_MAINCODE <- N_SA2s_IN_POSTCODE <- NULL
  if (is.null(SA2_BY_POSTCODE)) {
    SA2_BY_POSTCODE <- read_sys("SA2_BY_POSTCODE.fst", fst2_progress = FALSE)
    setkeyv(SA2_BY_POSTCODE, c("POSTCODE", "SA2_MAINCODE"))
    SA2_BY_POSTCODE[, N_SA2s_IN_POSTCODE := .N, keyby = "POSTCODE"]
  }
  the_mult <- match.arg(the_mult)

  if (length(postcode) > 1) {
    DT <- setDT(list(x = postcode))
    out <- DT[, ans := postcode2sa2(.BY[[1]], the_mult = the_mult, n = .N, SA2_BY_POSTCODE), by = "x"][["ans"]]
    return(out)
  }


  if (the_mult == "all") {
    out <- SA2_BY_POSTCODE[.(postcode), POSTCODE]
    return(rep_len(out, n))
  }

  SA2_BY_POSTCODE[.(postcode), on = "POSTCODE", mult = the_mult][["SA2_MAINCODE"]]

}


