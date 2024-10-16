spec_params <- load("data/asparagopsis.rda")

# Start date
test_that("Message on missing uptake shape", {
  expect_error(
    grow_macroalgae(start = 5),
  )
})



urls <- c(
  "http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html",
  "http://en.wikipedia.org/wiki/Xz",
  "xxxxx"
)

readUrl <- function(url) {
  tryCatch(
    {
      # Just to highlight: if you want to use more than one R expression in the "try" part then you'll have to use curly brackets.
      # 'tryCatch()' will return the last evaluated expression in case the "try" part was completed successfully
      message("This is the 'try' part")
      suppressWarnings(readLines(url))
      # The return value of `readLines()` is the actual value that will be returned in case there is no condition (e.g. warning or error).
    },
    error = function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
      NA
    },
    warning = function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(conditionMessage(cond))
      # Choose a return value in case of warning
      NULL
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally = <expression>' 
      message(paste("Processed URL:", url))
      message("Some other message at the end")
    }
  )
}


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



