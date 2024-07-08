#' Custom error handling for missing parameters
#'
#' @param param 
#' @param place 
#'
#' @export
#'
abort_missing_parameter <- function(param, place) {
  
  if (is.null(place)) {
    msg <- glue::glue("`{param}` must be defined.")
  } else {
    msg <- glue::glue("`{param}` must be defined in {place}.")
  }
  
  rlang::abort("error_missing_parameter", 
        message = msg, 
        param = param, 
        place = place
  )
}

