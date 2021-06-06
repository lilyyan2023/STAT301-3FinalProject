# Support Vector Machine(Polynomial) upsampling recipe 1 tuning ----

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
svmp_workflow_over_sampling_1 <- workflow() %>% 
  add_model(svmp_model) %>% 
  add_recipe(stroke_recipe_over_sampling_1)

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
svmp_tuned_over_sampling_1 <- svmp_workflow_over_sampling_1 %>% 
  tune_grid(stroke_folds, svmp_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
svmp_time_over_sampling_1 <- tic.log(format = TRUE)

# Write out results & workflow
save(svmp_tuned_over_sampling_1, svmp_time_over_sampling_1, svmp_workflow_over_sampling_1, 
     file = "svmp_tune_upsample_recipe1.rda")












