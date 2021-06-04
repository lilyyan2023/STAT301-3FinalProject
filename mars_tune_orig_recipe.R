# Multivariate adaptive regression splines (MARS) tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
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
mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(stroke_recipe_orig)

# Tuning/fitting ----
tic("Multivariate adaptive regression splines (MARS)")

## set-up tuning grid ----
mars_params <- parameters(mars_model) %>% 
  update(num_terms = num_terms(range = c(1, 10)))
# store regular grid
mars_grid <- grid_regular(mars_params, levels = 5)

## model tuning ----
mars_tuned <- mars_workflow %>% 
  tune_grid(stroke_folds, grid = mars_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
mars_time <- tic.log(format = TRUE)

# Write out results & workflow
save(mars_tuned, mars_time, mars_workflow, 
     file = "mars_tune_orig_recipe.rda")