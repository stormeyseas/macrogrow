spec_params <- c(V_am = 5, K_am = 140, M_am = 0.003, C_am = 3, V_ni = NA, K_ni = NA, M_ni = 0, C_ni = 0.15)

# Testing uptake shape bits
test_that("Message on missing uptake shape", {
  expect_message(
    get_uptake(conc = 10, uptake_shape = NA, Nform_abbr = "ni", spec_params = spec_params),
  )
})
test_that("Uptake shape is determined and result is correct (linear)", {
  expect_equal(
    get_uptake(conc = 10, uptake_shape = NA, Nform_abbr = "ni", spec_params = spec_params),
    expected = 0.15
  )
})
test_that("Uptake shape is determined and result is correct (MM prioritsed)", {
  expect_equal(
    get_uptake(conc = 10, uptake_shape = NA, Nform_abbr = "am", spec_params = spec_params),
    expected = 0.33333333
  )
})
test_that("Throw error if uptake shape not recognised", {
  expect_error(
    get_uptake(conc = 10, uptake_shape = "spelled wrong", Nform_abbr = "ni", spec_params = spec_params),
    class = "error_bad_parameter"
  )
})

# Testing concentration bits 
test_that("Throw error if concentration is NA", {
  expect_error(
    get_uptake(conc = NA, uptake_shape = "MM", Nform_abbr = "ni", spec_params = spec_params),
    class = "error_bad_parameter"
  )
})

# Testing parameter bits
spec_params <- c(V_am = 5, K_am = 140)

test_that("Throw error if parameter for desired curve is missing", {
  expect_error(
    get_uptake(conc = 10, uptake_shape = "MM", Nform_abbr = "ni", spec_params = spec_params),
    class = "error_missing_parameter"
  )
})
test_that("Throw error if desired parameter is missing", {
  expect_error(
    get_uptake(conc = 10, uptake_shape = "linear", Nform_abbr = "am", spec_params = spec_params),
    class = "error_missing_parameter"
  )
})
test_that("Output provided with bare minimum parameters", {
  expect_equal(
    get_uptake(conc = 10, uptake_shape = "MM", Nform_abbr = "am", spec_params = spec_params),
    expected = 0.33333333
  )
})



