library(targets)
library(qs)

tar_option_set(
  packages = c("tidyverse", "ggplot2", "arrow", "lubridate"), # Packages that your targets need for their tasks.
  # format = "parquet", # Optionally set the default storage format
  controller = crew::crew_controller_local(workers = 5, seconds_idle = 60)
)

# Run the R scripts with your custom functions:
tar_source(files = c(
  "R/environmental-helper-functions.R"
  , "R/growth-helper-functions.R"
  #, "R/special_growth_functions.R"
  , "R/misc-helper-functions.R"
  , "R/growth-model-function.R"
  )
)

# Replace the target list below with your own:
list(
  
  # Define the temporal space
  tar_target(growtime, command = seq(90,110,1), format = "rds")
  
  # Get data, define environmental space (as functions if necessary)
  
  # See data/example.Rmd for processing
  , tar_target(T_data, command = rnorm(length(growtime), mean = 20, sd = 1), format = "rds")
  , tar_target(T_func, command = temp_atsite(tspan = growtime, a = mean(T_data), b = sd(T_data), c = 20, period = 365), format = "rds")
  
  # Replace with actual data
  , tar_target(Nit_data, command = rnorm(length(growtime), mean = 5, sd = 0.5), format = "rds")
  , tar_target(Amm_data, command = rnorm(length(growtime), mean = 2, sd = 0.1), format = "rds")
  , tar_target(Nit_func, command = N_atsite(tspan = growtime, a = mean(Nit_data), b = sd(Nit_data), c = 60, period = 365))
  , tar_target(Amm_func, command = N_atsite(tspan = growtime, a = mean(Amm_data), b = sd(Amm_data), c = 60, period = 365))
  
  # Replace with actual data
  , tar_target(I_data, command = rnorm(length(growtime), mean = 600, sd = 10))
  , tar_target(I_func, command = Irr_atsite(tspan = growtime, a = mean(I_data), b = sd(I_data), c = 15, period = 365))
  
  # Replace with actual data
  , tar_target(U_func, command = relrefresh_atsite(tspan = growtime, a = 0.3, b = -0.15, c = 60, period = 365))

  # Set up culture space
  # , tar_target(site_data, format = "parquet") # replace with data
  , tar_target(site_params, command = c(d_top = 2, hc = 5, farmA = 60 * 50, hz = 35, kW = 0.58), format = "rds")

  # Set up macroalgae species
  # See data/example.Rmd for all sources and data processing
  , tar_target(spec_data, command = read.csv("data-raw/macrocystis.csv") %>% mutate(value = as.numeric(value)), format = "parquet")
  , tar_target(spec_params, name_vector(spec_data$value, spec_data$parameter), format = "rds")
  
  , tar_target(start_date, as.Date(x = "2017-04-26"))
  , tar_target(init_state, c(Nf = 50, Q = 15))
    
  # RUN MODEL
  , tar_target(model_base, 
               grow_macroalgae(start = start_date, 
                               grow_days = length(growtime)-1, 
                               temperature = T_func, light = I_func, velocity = U_func, 
                               nitrate = Nit_func, ammonium = Amm_func, 
                               site_params = site_params, spec_params = spec_params, 
                               initials = init_state),
               format = "parquet")
  , tar_target(output_long, command = pivot_longer(model_base, names_to = "output", values_to = "value", col = (3:ncol(model_base))), format = "parquet")
  
  # Get names for outputs
  , tar_target(output_names, command = colnames(model_base)[3:ncol(model_base)])
  , tar_target(out_plots, command = 
                 ggplot(filter(output_long, output == output_names), aes(x = t, y = value)) +
                 geom_line() + 
                 ggtitle(output_names),
               pattern = output_names,
               iteration = "list", format = "rds")
  
  , tar_target(index, 1:length(spec_params))
  , tar_target(spec_params_low, 
               command = new_params(spec_params, num = index, factor = 0.9),
               pattern = index,
               iteration = "list")
  , tar_target(spec_params_high, 
               command = new_params(spec_params, num = index, factor = 1.1),
               pattern = index,
               iteration = "list")
  
  , tar_target(model_low,
               command = grow_macroalgae(start = start_date, grow_days = length(growtime)-1,
                                         temperature = T_func, light = I_func, velocity = U_func,
                                         nitrate = Nit_func, ammonium = Amm_func,
                                         site_params = site_params, spec_params = spec_params_low,
                                         initials = init_state),
               pattern = spec_params_low, format = "parquet")

  , tar_target(model_high,
               command = grow_macroalgae(start = start_date, grow_days = length(growtime)-1,
                                         temperature = T_func, light = I_func, velocity = U_func,
                                         nitrate = Nit_func, ammonium = Amm_func,
                                         site_params = site_params, spec_params = spec_params_high,
                                         initials = init_state),
               pattern = spec_params_high, format = "parquet")
  
  , tar_target(low_long, pivot_longer(model_low, names_to = "output", values_to = "value", col = (3:ncol(model_low))), format = "parquet")
  , tar_target(high_long, pivot_longer(model_high, names_to = "output", values_to = "value", col = (3:ncol(model_high))), format = "parquet")
  , tar_target(lowhigh_long, format = "parquet", command = merge(low_long, high_long, by = c("t", "date", "output")))

  , tar_target(lowhighbase_long, format = "parquet", command = merge(lowhigh_long, output_long, by = c("t", "date", "output")))
  , tar_target(sensitivities, format = "parquet",
               command = lowhighbase_long %>%
                 filter(output == output_names) %>%
                 mutate(sens = (value.y - value.x)/(0.2*value)),
               pattern = output_names)
  , tar_target(sens_plots,
               ggplot(filter(sensitivities, output == output_names), aes(x = t, y = sens)) +
                 geom_line() + ggtitle(output_names),
               pattern = output_names,
               iteration = "list",
               format = "rds")
)


# For testing:
# source("R/growth-helper-functions.R")
# start <- tar_read(start_date)
# growtime <- tar_read(growtime)
# temperature <- tar_read(T_func)
# light <- tar_read(I_func)
# velocity <- tar_read(U_func)
# nitrate <- tar_read(Nit_func)
# ammonium <- tar_read(Amm_func)
# site_params <- tar_read(site_params)
# spec_params <- tar_read(spec_params)
# initials <- tar_read(init_state)
# grow_days <- length(growtime)-1
