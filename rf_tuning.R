# Random Forest tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(3013)

# load required objects ----

load("data/wildfires_setup.rda")

# Define model ----
rf_model <- rand_forest(mode = "classification",
                        min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger")

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(wildfires_recipe_tree)

# Tuning/fitting ----
tic("Random Forest")

## set-up tuning grid ----
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(1, 28)))
# store regular grid
rf_grid <- grid_regular(rf_params, levels = 5)

## model tuning ----
rf_tuned <- rf_workflow %>% 
  tune_grid(wildfires_folds, grid = rf_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
rf_time <- tic.log(format = TRUE)

# Write out results & workflow
save(rf_tuned, rf_time, rf_workflow, 
     file = "rf_tune.rda")