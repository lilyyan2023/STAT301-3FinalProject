# Support Vector Machine (radial basis function) tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
svmrbf_model <- svm_rbf(
  mode = "classification",
  cost = tune(),
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab")

# workflow ----
svmrbf_workflow_over_sampling_1 <- workflow() %>% 
  add_model(svmrbf_model) %>% 
  add_recipe(stroke_recipe_over_sampling_1)

# Tuning/fitting ----
tic("Support Vector Machine (radial basis function)")

## set-up tuning grid ----
svmrbf_params <- parameters(svmrbf_model) %>% 
  update(cost = cost(range = c(0, 5)),
         rbf_sigma = rbf_sigma(range = c(0, 1)))

# store regular grid
svmrbf_grid <- grid_regular(svmrbf_params, levels = 5)

## model tuning ----
svmrbf_tuned_over_sampling_1 <- svmrbf_workflow_over_sampling_1 %>% 
  tune_grid(stroke_folds, svmrbf_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
svmrbf_time_over_sampling_1 <- tic.log(format = TRUE)

# Write out results & workflow
save(svmrbf_tuned_over_sampling_1, svmrbf_time_over_sampling_1, 
     svmrbf_workflow_over_sampling_1, 
     file = "svmrbf_tune_orig_recipe.rda")










