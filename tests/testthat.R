library(testthat)
library(covid19.model.sa2)

test_check("covid19.model.sa2", reporter = LocationReporter)
