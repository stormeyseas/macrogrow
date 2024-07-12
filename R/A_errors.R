#' @title Custom error for missing parameters
#'
#' @param param missing parameter
#' @param place place where parameter should be defined
#'
abort_missing_parameter <- function(param, place) {
  if (is.null(place)) {
    msg <- glue::glue("`{param}` must be defined")
  } else {
    msg <- glue::glue("`{param}` must be defined in {place}")
  }
  
  rlang::abort("error_missing_parameter", 
        message = msg, 
        param = param, 
        place = place
  )
}

