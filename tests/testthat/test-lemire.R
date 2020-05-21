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


test_that("lemire_char", {
  skip_on_cran()
  skip_if(is32bit())
  p25 <- lemire_char(8000, 0.25, .Random.seed)
  expect_true(mean(p25) >= 0.2 && mean(p25) <= 0.3)
  withr::with_seed(37, {
    p74 <- lemire_char(1024, 0.037, 1:2048)
    expect_true(mean(p74) >= 0.026 && mean(p74) <= 0.048)
    expect_true(sum(p74) %in% c(37L, 42L))
  })

})



