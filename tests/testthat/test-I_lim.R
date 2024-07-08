test_that("Error on missing I_o", {
  expect_error(I_lim(I = 1000, Nf = 100, spec_params = c(a_cs = 0.001, h_a = 1000), site_params = c(d_top = 2, kW = 0.1)), 
               class = "error_missing_parameter")
})

test_that("Error on missing a_cs", {
  expect_error(I_lim(I = 1000, Nf = 100, spec_params = c(I_o = 5, h_a = 1000), site_params = c(d_top = 2, kW = 0.1)), 
               class = "error_missing_parameter")
})

test_that("Error on missing h_a", {
  expect_error(I_lim(I = 1000, Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001), site_params = c(d_top = 2, kW = 0.1)), 
               class = "error_missing_parameter")
})

test_that("Message on missing h_b", {
  expect_message(I_lim(I = 1000, Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001, h_a = 1000, h_c = 0), site_params = c(d_top = 2, kW = 0.1)))
})

test_that("Message on missing h_c", {
  expect_message(I_lim(I = 1000, Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001, h_a = 1000, h_b = 1), site_params = c(d_top = 2, kW = 0.1)))
})

