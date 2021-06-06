# Knn tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
nn_model <- nearest_neighbor(mode = "classification", 
                             neighbors = tune()) %>% 
  set_engine("kknn")

# workflow ----
nn_workflow <- workflow() %>% 
  add_model(nn_model) %>% 
  add_recipe(stroke_recipe_orig)

# Tuning/fitting ----
tic("knn")

## set-up tuning grid ----
nn_params <- parameters(nn_model) %>% 
  update(neighbors = neighbors(range = c(1, 15)))

# store regular grid
nn_grid <- grid_regular(nn_params, levels = 10)

## model tuning ----
nn_tuned <- nn_workflow %>% 
  tune_grid(stroke_folds, grid = nn_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
nn_time <- tic.log(format = TRUE)

# Write out results & workflow
save(nn_tuned, nn_time, nn_workflow, 
     file = "knn_tune_orig_recipe.rda")










