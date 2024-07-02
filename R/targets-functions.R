name_vector <- function(x, names) {
  names(x) <- names
  return(x)
}

adj_params <- function(params, num, factor) {
  newparams <- params
  newparams[num] <- newparams[num] * factor
  return(newparams)
}

Rfiles <- list.files("R/", recursive = F, full.names = T)
for (i in 2:length(Rfiles)) {
  source(Rfiles[i])
}
