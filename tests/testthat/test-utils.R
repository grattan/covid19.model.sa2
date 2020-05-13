test_that("percentage2int32 works", {
  imin <- percentage_to_int32(0)
  zero <- percentage_to_int32(0.5)
  imax <- percentage_to_int32(1)
  expect_identical(imin, -.Machine$integer.max)
  expect_identical(zero, 0L)
  expect_identical(imax, .Machine$integer.max)
})
