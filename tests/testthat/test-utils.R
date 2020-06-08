test_that("percentage2int32 works", {
  imin <- percentage_to_int32(0)
  zero <- percentage_to_int32(0.5)
  imax <- percentage_to_int32(1)
  expect_identical(imin, -.Machine$integer.max)
  expect_identical(zero, 0L)
  expect_identical(imax, .Machine$integer.max)
})

test_that("wsamp", {
  withr::with_seed(1, {
  out <- wsamp(1:3, size = 1e4, w = c(2500, 5000, 2500))
  tab <- tabulate(out, nbins = 3L)
  expect_equal(tab, c(2500, 5000, 2500), tol = 0.1)
  })
})


test_that("accel", {
  a <- as.character(seq.Date(Sys.Date(), by = "1 day", length.out = 1000))
  expect_equal(as.Date(a),
               accel(a, as.Date))
})

test_that("ematch", {
  expect_error(ematch(1:10, 2:9), "not matched")
  expect_identical(ematch(1:10, 1:11), 1:10)
})

test_that("which_last", {
  x <- rep_len(1:10, 49)
  expect_equal(which_last(x == 10L), 40L)
  expect_equal(which_last(x == 11L), 0L)
})

test_that("rep_each", {
  expect_identical(rep_each(1:3, 15),
                   rep(1:3, each = 5))

  expect_identical(do_rep(2:5), rep(2:5, 2:5))
})




