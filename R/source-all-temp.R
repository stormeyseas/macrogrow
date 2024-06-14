Rfiles <- list.files("R/", recursive = F, full.names = T)
for (i in 2:length(Rfiles)) {
  source(Rfiles[i])
}
