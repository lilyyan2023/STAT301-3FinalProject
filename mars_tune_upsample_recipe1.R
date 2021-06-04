# Multivariate adaptive regression splines (MARS) tuning upsampling recipe 1 ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(themis)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
mars_model <- mars(num_terms = tune(), 
                   prod_degree = tune(), 
                   mode = "classification") %>% 
  set_engine("earth")

# workflow ----
mars_workflow_over_sampling_1 <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(stroke_recipe_over_sampling_1)

# Tuning/fitting ----
tic("Multivariate adaptive regression splines (MARS)")

## set-up tuning grid ----
mars_params <- parameters(mars_model) %>% 
  update(num_terms = num_terms(range = c(1, 10)))
# store regular grid
mars_grid <- grid_regular(mars_params, levels = 5)

## model tuning ----
mars_tuned_over_sampling_1 <- mars_workflow_over_sampling_1 %>% 
  tune_grid(stroke_folds, grid = mars_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
mars_time_over_sampling_1 <- tic.log(format = TRUE)

# Write out results & workflow
save(mars_tuned_over_sampling_1, mars_time_over_sampling_1, 
     mars_workflow_over_sampling_1, 
     file = "mars_tune_upsample_recipe1.rda")
