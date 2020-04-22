test_that("shorten_sa2 works", {
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
