test_that("Temperature effect on growth == 1 when Tc = T_opt", {
  expect_equal(T_lim(TT = 20, spec_params = c(T_opt = 20, T_min = 5, T_max = 30)), 
               expected = 1)
})

test_that("Temperature effect on growth == 0 when Tc >= T_max", {
  expect_equal(T_lim(TT = 4, spec_params = c(T_opt = 20, T_min = 5, T_max = 30)), 
               expected = 0)
})

test_that("Temperature effect on growth == 0 when Tc <= T_min", {
  expect_equal(T_lim(TT = 32, spec_params = c(T_opt = 20, T_min = 5, T_max = 30)), 
               expected = 0)
})

test_that("Error on missing T_opt", {
  expect_error(T_lim(TT = 20, spec_params = c(T_min = 5, T_max = 30)), class = "error_missing_parameter")
})

test_that("Error on missing T_max", {
  expect_error(T_lim(TT = 20, spec_params = c(T_min = 5, T_opt = 30)), class = "error_missing_parameter")
})

test_that("Error on missing T_min", {
  expect_error(T_lim(TT = 20, spec_params = c(T_opt = 5, T_max = 30)), class = "error_missing_parameter")
})

test_that("Error if condition T_min < T_opt < T_max is not met", {
  expect_error(T_lim(TT = 20, spec_params = c(T_opt = 5, T_min = 20, T_max = 30)), class = "error_bad_parameter")
})
