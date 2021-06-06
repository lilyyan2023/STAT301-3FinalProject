# Support Vector Machine(Polynomial) tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
svmp_model <- svm_poly(
  mode = "classification",
  cost = tune(),
  degree = tune(),
  scale_factor = tune()
) %>% 
  set_engine("kernlab")

# workflow ----
svmp_workflow <- workflow() %>% 
  add_model(svmp_model) %>% 
  add_recipe(stroke_recipe_orig)

# Tuning/fitting ----
tic("Support Vector Machine(Polynomial)")

## set-up tuning grid ----
svmp_params <- parameters(svmp_model) %>% 
  update(cost = cost(range = c(0, 5)),
         degree = degree(range = c(0, 10)),
         scale_factor = scale_factor(range = c(0, 10)))

# store regular grid
svmp_grid <- grid_regular(svmp_params, levels = 5)

## model tuning ----
svmp_tuned <- svmp_workflow %>% 
  tune_grid(stroke_folds, svmp_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
svmp_time <- tic.log(format = TRUE)

# Write out results & workflow
save(svmp_tuned, svmp_time, svmp_workflow, 
     file = "svmp_tune_orig_recipe.rda")










