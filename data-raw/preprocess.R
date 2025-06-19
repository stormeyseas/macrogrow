# Load raw data from .csv file
df <- read.csv("data-raw/asparagopsis_armata.csv", row.names = 1)

# Apply preprocessing
## Check that units match what model needs, change if necessary
df$unit[df$parameter == "h_max"]

## Save as named vector
a_armata <- df$value
names(a_armata) <- df$param

# Save the cleaned data in the required R package location
usethis::use_data(a_armata, overwrite = T)

# Also set some default site_params
site_params <- c(hz = 30, d_top = 1, hc = 3, kW = 0.6)
usethis::use_data(site_params, overwrite = T)

# And some default vectors
env <- data.frame(t = 1:365)
env$temperature <- 15 + 3.5 * sin(2*pi*env$t/365 + 15) + rnorm(nrow(env), mean = 0, sd = 1.5)
env$light <- 1000 + 500 * sin(2*pi*env$t/365 + 15) + rnorm(nrow(env), mean = 0, sd = 200)
env$kW <- 0.14 + 0.05 * sin(2*pi*env$t/365 + 15) + rnorm(nrow(env), mean = 0.005, sd = 0.01)
env$velocity <- 0.25 + 0.1 * sin(2*pi*env$t/365 + 160) + rnorm(nrow(env), mean = 0, sd = 0.05)
env$salinity <- 35 + 1 * sin(2*pi*env$t/365 + 160) + rnorm(nrow(env), mean = 0, sd = 0.5)
env$nitrate <- 70 + 35 * sin(2*pi*env$t/365 + 160) + rnorm(nrow(env), mean = 0, sd = 10)
env$ammonium <- 15 + 5 * sin(2*pi*env$t/365 + 15) + rnorm(nrow(env), mean = 0, sd = 2)

usethis::use_data(env, overwrite = T)
