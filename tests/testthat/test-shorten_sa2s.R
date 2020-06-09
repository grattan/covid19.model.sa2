test_that(paste(as.character(Sys.time()), "shorten_sa2 works"), {
  skip_if_not_installed("data.table")
  library(data.table)
  aus <- read_sys("australia.fst", columns = c("state", "sa2", "hid", "pid"))
  sa2s <- as.integer(read_sys("sa2_codes.fst", columns = "sa2")[[1]])
  short_sa2s <- shorten_sa2s_ordered(sa2s)
  expect_true(hasName(aus, "sa2"))
  expect_true(is.data.table(aus))
  expect_false(aus[, is.unsorted(sa2)])
  aus[, short_sa2 := shorten_sa2s_ordered(sa2)]
  expect_equal(aus[, first(short_sa2)], 0L)
  expect_equal(aus[, last(short_sa2)], aus[, short_sa2(last(sa2))])
  expect_equal(aus[, min(diff(short_sa2))], 0L)
})

test_that(paste(as.character(Sys.time()), "sa2_firsts_finals"), {
  library(data.table)
  aus <- read_sys("australia.fst", columns = c("sa2"))
  aus[, short_sa2 := shorten_sa2s_ordered(sa2)]
  firsts_finals <- as.data.table(aus[, sa2_firsts_finals(short_sa2)])
  aus[, cpp_i := .I - 1L]
  first_finals_dt <-
    aus[, .(firsts = first(cpp_i), finals = last(cpp_i)), by = .(sa2)]
  first_finals_dt[, finals := finals + 1L]


  expect_equal(last(firsts_finals$V2), nrow(aus))
  expect_equal(first_finals_dt$firsts, firsts_finals[V1 < V2]$V1)
  expect_equal(first_finals_dt$finals, firsts_finals[V1 < V2]$V2)
})

