

# Useful for tests to have a 'typically' generated file
read_typical <- function() {

  # S$Statuses[, .SD, .SDcols = c(names(S$Statuses)[seq_len(which_first(names(S$Statuses) == "V1"))])] %>% drop_cols(c("pid", "Resistance", "school_id", "work_dzn", "seqN", "HouseholdSize", "V1")) %>% {lapply(X = 1:9, FUN = function(s) invisible(write_fst(.[state == s][, state := NULL], provide.file(paste0("inst/extdata/examples/", "typical-aus-state-", s, ".fst")), compress = 100)))}
  out <-
    rbindlist(lapply(1:9, function(s) {
      read_sys(paste0("examples/typical-aus-state-", s, ".fst"))
    }),
    idcol = "state",
    use.names = TRUE,
    fill = TRUE)
  out[, "pid" := seq_len(.N)]
  setkeyv(out, c("state", "sa2", "hid", "pid"))
  out[, c("seqN", "HouseholdSize") := do_seqN_N(hid, pid)]
  out
}

