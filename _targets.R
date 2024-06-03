# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(qs)
# library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "ggplot2"), # Packages that your targets need for their tasks.
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}. The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2". Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = c(
  "R/environmental-helper-functions.R"
  , "R/growth-helper-functions.R"
  #, "R/special_growth_functions.R"
  #, "R/misc-helper-functions.R",
  , "R/growth-model-function.R"
  )
)

# Replace the target list below with your own:
list(
  
  # Define the temporal space
  tar_target(growtime, command = seq(90,110,1)),
  
  # Get data, define environmental space (as functions if necessary)
  # tar_target(data, get_data(file)),
  tar_target(T_data, command = rnorm(length(growtime), mean = 20, sd = 1)),
  tar_target(T_func, command = temp_atsite(tspan = growtime, a = mean(T_data), b = sd(T_data), c = 20, period = 365)),
  tar_target(T_plot, command = 
               ggplot(data = data.frame(t = growtime, func = T_func, data = T_data), aes(x = t)) +
               geom_line(aes(y = func)) +
               geom_point(aes(y = data))  ),
  
  tar_target(Nit_data, command = rnorm(length(growtime), mean = 5, sd = 0.5)),
  tar_target(Amm_data, command = rnorm(length(growtime), mean = 2, sd = 0.1)),
  tar_target(Nit_func, command = N_atsite(tspan = growtime, a = mean(Nit_data), b = sd(Nit_data), c = 60, period = 365)),
  tar_target(Amm_func, command = N_atsite(tspan = growtime, a = mean(Amm_data), b = sd(Amm_data), c = 60, period = 365)),
  tar_target(N_plot, command = 
               ggplot(data = data.frame(t = growtime, func = Nit_func+Amm_func, Nit = Nit_data, Amm = Amm_data), aes(x = t)) +
               geom_line(aes(y = func)) + 
               geom_point(aes(y = Amm+Nit)) +
               geom_point(aes(y = Nit), color = "blue") + geom_point(aes(y = Amm), color = "orange")  ),
  
  tar_target(I_data, command = rnorm(length(growtime), mean = 600, sd = 10)),
  tar_target(I_func, command = Irr_atsite(tspan = growtime, a = mean(I_data), b = sd(I_data), c = 15, period = 365)),
  tar_target(I_plot, command =
               ggplot(data = data.frame(t = growtime, func = I_func, data = I_data), aes(x = t)) +
               geom_line(aes(y = func)) + 
               geom_point(aes(y = data))  ),

  tar_target(U_func, command = relrefresh_atsite(tspan = growtime, a = 0.3, b = -0.15, c = 60, period = 365)),
  tar_target(U_plot, command = 
               ggplot(data = data.frame(t = growtime, func = U_func), aes(x = t)) +
               geom_line(aes(y = func))  ),
  
  # Set up culture space
  tar_target(site_params, 
             command = data.frame(parameter = c("d_top", "hc", "farmA", "hz"),
                                  value = c(2.5, 5, 60 * 50, 35))), 

  # Set up macroalgae species
  tar_target(spec_params, 
             read.csv("data-raw/macrocystis.csv") %>% 
               mutate(value = as.numeric(value)))
  
  , # RUN MODEL
  tar_target(model_run,
             command = grow_macroalgae(
               start = as.Date(x = "2017-04-26"),
               grow_days = length(growtime)-1,
               temperature = T_func,
               light = I_func,
               velocity = U_func,
               nitrate = Nit_func,
               ammonium = Amm_func,
               site_params = site_params,
               spec_params = spec_params,
               Nf_start = 50,
               Q_start = 15
               )
  )
  
  # 
  # tar_target(file, "data.csv", format = "file"),
  # tar_target(model, fit_model(data)),
  # tar_target(plot, plot_model(model, data))
)

# Use targets commands to visualise and run
# tar_validate()
# tar_visnetwork()
# tar_make()
# tar_read(spec_params)
