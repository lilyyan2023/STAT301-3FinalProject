# Elastic Net tuning ----

# Load package(s), Set Seed ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(123)

# load required objects ----

load("data/stroke_setup.rda")

# Define model ----
en_model <- logistic_reg(mode = "classification",
                         penalty = tune(),
                         mixture = tune()) %>% 
  set_engine("glmnet")

# workflow ----
en_workflow <- workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(stroke_recipe_orig)

# Tuning/fitting ----
tic("Elastic Net")

## set-up tuning grid ----
en_params <- parameters(en_model) %>% 
  update(mixture = mixture(range = c(0, 1)))

# store regular grid
en_grid <- grid_regular(en_params, levels = 5)

## model tuning ----
en_tuned <- en_workflow %>% 
  tune_grid(stroke_folds, grid = en_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
en_time <- tic.log(format = TRUE)

# Write out results & workflow
save(en_tuned, en_time, en_workflow, 
     file = "en_tune_orig_recipe.rda")










