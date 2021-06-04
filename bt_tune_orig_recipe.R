# Boosted Tree tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
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
bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(stroke_recipe_orig)

# Tuning/fitting ----
tic("Boosted Tree")

## set-up tuning grid ----
bt_params <- parameters(bt_model) %>% 
  update(mtry = mtry(range = c(1, 15)), 
         learn_rate = learn_rate(range = c(-1, 0)))
# store regular grid
bt_grid <- grid_regular(bt_params, levels = 5)

## model tuning ----
bt_tuned <- bt_workflow %>% 
  tune_grid(stroke_folds, grid = bt_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
bt_time <- tic.log(format = TRUE)

# Write out results & workflow
save(bt_tuned, bt_time, bt_workflow, 
     file = "bt_tune_orig_recipe.rda")