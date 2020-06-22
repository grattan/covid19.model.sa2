test_that("postcode2sa2 works", {
  skip_on_travis()
  expect_equal(postcode2sa2(c(2000L, 3053L, 3004L)),
               c(117031337L, 206041117L, 206041122L))
  expect_error(postcode_to_sa2_sorted(c(2000L, 5000L), 1:10, 1:11),
               regexp = "tn !=",
               fixed = TRUE)

  SA2_by_POSTCODE <- read_sys("SA2_by_POSTCODE.fst")
  expect_equal(postcode_to_sa2_sorted(sort(c(2000L, 3053L, 3004L)),
                                      POSTCODE = SA2_by_POSTCODE$POSTCODE,
                                      SA2_MAINCODE = SA2_by_POSTCODE$SA2_MAINCODE),
               c(117031337L, 206041122L, 206041117L))
})
