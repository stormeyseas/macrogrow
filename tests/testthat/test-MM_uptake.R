test_that("Error on missing V", {
  expect_error(MM_uptake(conc = 25, K = 95), class = "error_missing_parameter")
})

test_that("Message on missing K", {
  expect_error(MM_uptake(conc = 25, V = 0.5), class = "error_missing_parameter")
})

test_that("Message on missing V and missing K", {
  expect_error(MM_uptake(conc = 25), class = "error_missing_parameter")
})
