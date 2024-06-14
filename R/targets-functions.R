name_vector <- function(x, names) {
  names(x) <- names
  return(x)
}

adj_params <- function(params, num, factor) {
  newparams <- params
  newparams[num] <- newparams[num] * factor
  return(newparams)
}
