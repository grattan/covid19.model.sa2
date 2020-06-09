test_that(paste(as.character(Sys.time()), "seqN_N works"), {
  skip_if_not_installed("data.table")
  skip_if_not_installed("magrittr")
  library(data.table)
  library(magrittr)
  DT <-
    lapply(1:5, function(i) {
      DT <- data.table(pid = rep(i, i),
                       hid = seq_len(i))
    }) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    setkey(hid, pid)

  expect_equal(DT[, c("S", "N") := do_seqN_N(hid, pid)][["S"]],
               DT[, c("S", "N") := list(seq_len(.N), .N), by = "hid"][["S"]])
  expect_equal(DT[, c("S", "N") := do_seqN_N(hid, pid)][["N"]],
               DT[, c("S", "N") := list(seq_len(.N), .N), by = "hid"][["N"]])

  DT[, rr := sample.int(.N)]
  setkey(DT, rr)
  expect_error(DT[, do_seqN_N(hid, pid)], "sorted")
})
