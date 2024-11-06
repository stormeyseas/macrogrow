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
