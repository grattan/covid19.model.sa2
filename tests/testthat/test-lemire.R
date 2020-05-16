test_that("lemire works", {
  skip_on_cran()
  a <- do_lemire_rand(10, integer(10))
  expect_equal(length(a), 10)

  b1 <- do_lemire_rand(10, 100:500)
  b2 <- do_lemire_rand(10, 100:500)
  expect_identical(b1, b2)

  skip_on_travis()
  c1 <- do_lemire_rand_par(1e6, integer(1024), nThread = 2L)
  c2 <- do_lemire_rand_par(1e6, integer(1024), nThread = 2L)

  expect_lt(min(c1), 0L)
  expect_gt(max(c2), 0L)

  for (i in 1:50) {
    L1 <- do_lemire_rand_par(25e6, integer(1024), nThread = parallel::detectCores())
    L2 <- do_lemire_rand_par(25e6, integer(1024), nThread = parallel::detectCores())


    expect_lt(min(c1), -1L)
    expect_gt(max(c2), 1L)
  }
})



