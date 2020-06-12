test_that("postcode2sa2 works", {
  expect_equal(postcode2sa2(c(2000L, 3053L, 3004L)),
               c(117031337L, 206041117L, 206041122L))
})
