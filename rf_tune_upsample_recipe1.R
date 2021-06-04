# Random Forest tuning upsampling recipe 1----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(themis)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
rf_model <- rand_forest(mode = "classification",
                        min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger")

# workflow ----
rf_workflow_over_sampling_1 <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(stroke_recipe_over_sampling_1)

# Tuning/fitting ----
tic("Random Forest")

## set-up tuning grid ----
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(1, 15)))
# store regular grid
rf_grid <- grid_regular(rf_params, levels = 5)

## model tuning ----
rf_tuned_over_sampling_1 <- rf_workflow_over_sampling_1 %>% 
  tune_grid(stroke_folds, grid = rf_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
rf_time_over_sampling_1 <- tic.log(format = TRUE)

# Write out results & workflow
save(rf_tuned_over_sampling_1, rf_time_over_sampling_1, 
     rf_workflow_over_sampling_1, 
     file = "rf_tune_upsample_recipe1.rda")