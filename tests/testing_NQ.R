spec_params = c(
  Q_min = 7,
  Q_max = 35,
  N_min = 0.015,
  N_max = 0.045,
  K_c = 6,
  V_am = 120,
  K_am = 60
)

Nf = 500
df <- data.frame(Ns = seq(0, 2500, 10))
df$Qint <- sapply(X = df$Ns, FUN = Q_int, Nf = Nf, spec_params = spec_params)
df$Nint <- sapply(X = df$Ns, FUN = N_int, Nf = Nf, spec_params = spec_params)
df$Qrel <- sapply(X = df$Ns, FUN = Q_rel, Nf = Nf, spec_params = spec_params)
df$Nrel <- sapply(X = df$Ns, FUN = N_rel, Nf = Nf, spec_params = spec_params)
df$Qlim <- sapply(X = df$Ns, FUN = Q_lim, Nf = Nf, spec_params = spec_params)

library(ggplot2)
ggplot(df, aes(x = Ns)) +
  # geom_line(aes(y = Qint)) +
  geom_line(aes(y = Nint)) +
  # geom_line(aes(y = Qrel)) +
  geom_line(aes(y = Nrel)) +
  geom_line(aes(y = Qlim))
  
df$uptake <- (1-df$Qrel) * get_uptake(conc = 20,
                                  uptake_shape = "MM",
                                  Nform_abbr = "am",
                                  spec_params = spec_params)
df$uptake[df$uptake > 20] <- 20
  
ggplot(df, aes(x = Ns, y = uptake)) +
  geom_line()
