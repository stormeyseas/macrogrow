test_that("Error on missing Q_min", {
  expect_error(Q_int(Nf = 20, Ns = 25, spec_params = c(Q_max = 5)), class = "error_missing_parameter")
})

test_that("Error on NA Q_min", {
  expect_error(Q_int(Nf = 20, Ns = 25, spec_params = c(Q_min = NA)), class = "error_missing_parameter")
})

test_that("Error on bad Nf value", {
  expect_error(Q_int(Nf = 0, Ns = 25, spec_params = c(Q_min = 5)), class = "error_bad_parameter")
})

test_that("Error on bad Nf value", {
  expect_error(Q_int(Nf = 20, Ns = -1, spec_params = c(Q_min = 5)), class = "error_bad_parameter")
})

test_that("Q_int > Q_min", {
  expect_gt(Q_int(Nf = 20, Ns = 0.1, spec_params = c(Q_min = 5)), 5)
})

