# Elastic Net upsampling recipe 2 tuning ----

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
en_workflow_over_sampling_2 <- workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(stroke_recipe_over_sampling_2)

# Tuning/fitting ----
tic("Elastic Net")

## set-up tuning grid ----
en_params <- parameters(en_model) %>% 
  update(mixture = mixture(range = c(0, 1)))

# store regular grid
en_grid <- grid_regular(en_params, levels = 5)

## model tuning ----
en_tuned_over_sampling_2 <- en_workflow_over_sampling_2 %>% 
  tune_grid(stroke_folds, grid = en_grid)

# Pace tuning code in hear
toc(log = TRUE)

# save runtime info
en_time_over_sampling_2 <- tic.log(format = TRUE)

# Write out results & workflow
save(en_tuned_over_sampling_2, en_time_over_sampling_2, en_workflow_over_sampling_2, 
     file = "en_tune_upsample_recipe2.rda")



