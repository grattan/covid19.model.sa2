test_that(paste(as.character(Sys.time()), "percentage2int32 works"), {
  imin <- percentage_to_int32(0)
  zero <- percentage_to_int32(0.5)
  imax <- percentage_to_int32(1)
  expect_identical(imin, -.Machine$integer.max)
  expect_identical(zero, 0L)
  expect_identical(imax, .Machine$integer.max)
})

test_that(paste(as.character(Sys.time()), "wsamp"), {
  withr::with_seed(1, {
  out <- wsamp(1:3, size = 1e4, w = c(2500, 5000, 2500))
  tab <- tabulate(out, nbins = 3L)
  expect_equal(tab, c(2500, 5000, 2500), tol = 0.1)
  })
})


test_that(paste(as.character(Sys.time()), "accel"), {
  a <- as.character(seq.Date(Sys.Date(), by = "1 day", length.out = 1000))
  expect_equal(as.Date(a),
               accel(a, as.Date))
})

test_that(paste(as.character(Sys.time()), "ematch"), {
  expect_error(ematch(1:10, 2:9), "not matched")
  expect_identical(ematch(1:10, 1:11), 1:10)
})

test_that(paste(as.character(Sys.time()), "which_last"), {
  x <- rep_len(1:10, 49)
  expect_equal(which_last(x == 10L), 40L)
  expect_equal(which_last(x == 11L), 0L)
})

test_that(paste(as.character(Sys.time()), "rep_each"), {
  expect_identical(rep_each(1:3, 15),
                   rep(1:3, each = 5))


  expect_identical(do_rep(2:5), rep(2:5, 2:5))
})

test_that(paste(as.character(Sys.time()), "dollars"), {
  out <- list(a = 1, ba = 2)
  b. <- "ba"
  expect_error(dollars(out, b), regexp = "subelement")
  expect_error(dollars(out, b.), regexp = "subelement")

  expect_equal(dollars(out, b., TRY_EVAL = TRUE), 2)
})

test_that(paste(as.character(Sys.time()), "minmax"), {
  x <- do_lemire_rand(100e3)
  min_max_b <- c(min(x), max(x))
  min_max_a <- do_minmax_par(x)
  expect_identical(min_max_a, min_max_b)


  skip_on_cran()
  min_max_p <- do_minmax_par(x, nThread = 2)
  expect_identical(min_max_p, min_max_b)
})

test_that(paste(as.character(Sys.time()), "Modulo"), {
  ax <- c(0:99, 0:99)
  expect_equal(do_ModuloIndex(ax, 4, 100), ax %% 4)
  skip_on_cran()
  expect_equal(do_ModuloIndex(ax, 4, 100, nThread = 2), ax %% 4)
})

test_that(paste(as.character(Sys.time()), "lag"), {
  x <- c(101:151, 101:151)
  skip_if_not_installed("data.table")
  expect_equal(do_lag_int(x), shift(x))
  skip_on_cran()
  expect_equal(do_lag_int(x, nThread = 2), shift(x))
})

test_that("do_exp_dbl2int", {
  xxx <- c(0, 0.5, 1, 1.5)
  out <- do_exp_dbl2int(xxx)
  expect_equal(out, as.integer(exp(xxx)))
  out2 <- do_exp_dbl2int(c(0, 0.5, 1, 1.5), nThread = 2)
  expect_equal(out2, as.integer(exp(xxx)))
})


test_that("sum_le_eq", {
  X <- 1:100
  Y <- rep_len(1:2, 100)
  expect_equal(sum_le_eq(X, 50L, Y, 1L), 25L)
})
