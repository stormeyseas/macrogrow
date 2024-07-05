test_that("Error on missing Q_min", {
  expect_error(QQ(Q = 20, spec_params = c(Q_max = 25)), class = "error_missing_parameter")
})

test_that("Error on NA Q_min", {
  expect_error(QQ(Q = 20, spec_params = c(Q_max = 25, Q_min = NA)), class = "error_missing_parameter")
})

test_that("Error when Q < Q_min", {
  expect_error(QQ(Q = 5, spec_params = c(Q_max = 25, Q_min = 10)), class = "error_bad_parameter")
})

