test_that("do_one_unif works", {
  skip_if(is32bit())
  skip_if_not_installed("withr")
  a <- sapply(1:1e4, function(x) do_one_unif(1L, 1e2L, x %% 2L))
  expect_identical(range(a), c(1L, 1e2L))
  a <- sapply(1:1e4, function(x) do_one_unif(-5L, 1e2L, x %% 2L))
  expect_identical(range(a), c(-5L, 1e2L))
  withr::with_seed(1, {
    updateLemireSeedFromR()
    b1 <- sapply(1:1e4, function(x) do_one_unif(-4L, 100L, x %% 2L))
    set.seed(1)
    updateLemireSeedFromR()
    b2 <- sapply(1:1e4, function(x) do_one_unif(-4L, 100L, x %% 2L))
    expect_identical(b1, b2)

    # Not zero-inflated
    expect_lt(sum(b1 == 0L) / sum(b1 == 1L), 1.5)
  })

})



