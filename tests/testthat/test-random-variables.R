test_that(paste(as.character(Sys.time()), "do_one_unif works"), {
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


test_that(paste(as.character(Sys.time()), "sum_lemire_char"), {
  skip_if_not(is64bit())
  skip_if_not_installed("withr")
  p <- 0.444
  withr::with_seed(1, {
    updateLemireSeedFromR()
    s <- sum_lemire_char(1000, p, nThread = 1L) - 444L
  })
  expect_true(s %in% -15:15)
  skip_on_travis()
  skip_on_cran()
  withr::with_seed(1, {
    updateLemireSeedFromR()
    S <- sum_lemire_char(1000, p, nThread = 2L) - 444L
  })
  expect_true(S %in% -15:15)


})

test_that(paste(as.character(Sys.time()), "test_q_lemire_32"), {
  skip_if_not(is64bit())
  skip_if_not_installed("withr")
  p <- 0.777
  withr::with_seed(1, {
    updateLemireSeedFromR()
    s <- sum(test_q_lemire_32(1000, p, nThread = 1L)) - 777L
  })
  expect_true(s %in% -35:35)

  skip_on_travis()
  skip_on_cran()
  withr::with_seed(1, {
    updateLemireSeedFromR()
    S <- sum(test_q_lemire_32(1000, p, nThread = 2L)) - 777L
  })
  expect_true(S %in% -35:35)

  expect_equal(length(lemire_char(3, 0.5)), 3)
})

test_that("do_updateLemireSeedFromR error", {
  skip_if_not(is64bit())
  expect_error(do_updateLemireSeedFromR(1:10), "42")
})

test_that(paste(as.character(Sys.time()), "RCauchy"), {
  skip_if_not_installed("withr")
  if (is32bit()) {
    expect_warning(do_lemire_char_par(1600),
                   regex = 'do_lemire_char_par not available for 32-bit R')
  } else {
    withr::with_seed(77, {
      rc <- as.integer(abs(rcauchy(1600, 10, 1)))
      updateLemireSeedFromR()
      RC <- RCauchy(U = do_lemire_rand_par(1600), scale = 10, location = 1)
    })
    expect_gte(median(RC), 8)
    expect_gte(median(rc), 8)
    expect_lte(median(rc), 13)
    expect_lte(median(RC), 13)
  }
})

test_that(paste(as.character(Sys.time()), "cf"), {
  skip_if_not(is64bit())
  skip_if_not_installed("withr")
  withr::with_seed(78, {
    a <- cf_mod_lemire(1e6, 0.023, 0)
    b <- cf_mod_lemire(1e6, 0.023, 1)
  })
  expect_lte(abs(a - b), 10e3)
})

test_that(paste(as.character(Sys.time()), "do_lemire_rand"), {
  skip_if(is32bit())
  expect_error(do_lemire_rand(n = 3, fill_if_odd = FALSE), regexp = "n must be positive and even")
  expect_error(do_lemire_rand(n = -8, fill_if_odd = FALSE), regexp = "n must be positive and even")
  expect_equal(length(do_lemire_rand(n = 3, fill_if_odd = TRUE)), 4)
})

test_that(paste(as.character(Sys.time()), "C++ <random>"), {
  skip_on_cran()
  skip_on_travis()
  rc <- prcauchy(10000, 0, 1)
  expect_lt(min(rc), 0)
  expect_gt(max(rc), 0)

  if (requireNamespace("parallel", quietly = TRUE) && parallel::detectCores() > 10) {
    rc <- prcauchy(10000, 0, 1, nThread = 10)
    expect_lt(min(rc), 0)
    expect_gt(max(rc), 0)
  }

  rln <- prlnorm_dbl(10000, 4, 1)
  expect_gt(min(rln), 0)
  avg <- mean(rln)
  expect_true(m2mu(avg) %between% c(4 - 0.5, 4 + 0.5))

  if (requireNamespace("parallel", quietly = TRUE) && parallel::detectCores() > 10) {
    rln <- prlnorm_dbl(10000, 4, 1, nThread = 10)
    expect_gt(min(rln), 0)
    avg <- mean(rln)
    expect_true(m2mu(avg) %between% c(4 - 0.5, 4 + 0.5))
  }

  rlni <- prlnorm_int(10000, 4, 1)
  expect_gte(min(rlni), 0)
  expect_true(is.integer(rlni))
  avg <- mean(rln)
  expect_true(m2mu(avg) %between% c(4 - 0.5, 4 + 0.5))

  if (requireNamespace("parallel", quietly = TRUE) && parallel::detectCores() > 10) {
    rlni <- prlnorm_int(10000, 4, 1, nThread = 10)
    expect_gte(min(rlni), 0)
    expect_true(is.integer(rlni))
    avg <- mean(rln)
    expect_true(m2mu(avg) %between% c(4 - 0.5, 4 + 0.5))
  }

  ansu <- punif_int(1000, 101, 110)
  expect_gte(min(ansu), 101)
  expect_lte(max(ansu), 110)
  if (requireNamespace("parallel", quietly = TRUE) && parallel::detectCores() > 10) {
    ansu <- punif_int(1000, 101, 110, nThread = 10)
    expect_gte(min(ansu), 101)
    expect_lte(max(ansu), 110)
  }

})



