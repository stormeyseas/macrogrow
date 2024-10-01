test_that("Message on missing h_a", {
  expect_message(
    object = algae_height(Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001, h_b = 1, h_c = 0)),
    class = "error_missing_parameter"
    )
})

test_that("Still runs with missing h_b", {
  expect_s3_class(
    object = algae_height(Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001, h_a = 100, h_c = 0)),
    class = "numeric"
  )
})
test_that("Message on missing h_b", {
  expect_message(
    algae_height(Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001, h_a = 100, h_c = 0)),
    )
})

test_that("Message on missing h_c", {
  expect_message(
    algae_height(Nf = 100, spec_params = c(I_o = 5, a_cs = 0.001, h_a = 100, h_b = 1)),
    )
})

test_that("If only h_max supplied, height = h_max" {
  expect_equal(
    object = algae_height(Nf = 100, spec_params = c(h_max = 20.05)),
    expected = 20.05
    )
  })

