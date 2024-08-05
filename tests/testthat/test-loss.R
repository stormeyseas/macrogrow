test_that("No error on missing D_m", {
  expect_no_error(
    loss(spec_params = c(D_ve = 0.015, D_st = 0, D_lo = 0.05, D_mi = 0.1, D_hi = 0.15))
    )
})

test_that("D_m acts as minimum", {
  expect_gt(
    loss(spec_params = c(D_m = 0.5, D_ve = 0.015, D_st = 0, D_lo = 0.05, D_mi = 0.1, D_hi = 0.15)),
    expect = 0.5
  )
})
