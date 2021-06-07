# Boosted Tree tuning upsampling recipe 2----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(themis)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
bt_model <- boost_tree(mode = "classification", 
                       mtry = tune(), 
                       min_n = tune(), 
                       learn_rate = tune()) %>% 
  set_engine("xgboost")

# workflow ----
bt_workflow_over_sampling_2 <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(stroke_recipe_over_sampling_2)

# Tuning/fitting ----
tic("Boosted Tree")

## set-up tuning grid ----
bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(1, 15)), 
         learn_rate = learn_rate(range = c(-1, 0)))
# store regular grid
bt_grid <- grid_regular(bt_params, levels = 5)

## model tuning ----
bt_tuned_over_sampling_2 <- bt_workflow_over_sampling_2 %>% 
  tune_grid(stroke_folds, grid = bt_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
bt_time_over_sampling_2 <- tic.log(format = TRUE)

# Write out results & workflow
save(bt_tuned_over_sampling_2, bt_time_over_sampling_2, 
     bt_workflow_over_sampling_2, 
     file = "bt_tune_upsample_recipe2.rda")