# Load Packages and set seed
library(tidyverse)
library(tidymodels)
set.seed(123)
# load data
stroke_data <- readRDS("data/processed/stroke_data.rds")

# Split data
stroke_split <- initial_split(stroke_data, prop = 0.7, strata = stroke)
stroke_train <- training(stroke_split)
stroke_test <- testing(stroke_split)
# Build recipe
# no residence_type possibly
stroke_recipe <- recipe(stroke ~ ., data = stroke_train) %>% 
  step_impute_bag(bmi) %>% # bag impute bmi missing values
  step_log(avg_glucose_level, bmi) %>% 
  step_interact(bmi ~ age) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

prep(stroke_recipe) %>% 
  bake(new_data = NULL)

## random forest model
# Define model
rf_model <- rand_forest(
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>% 
  set_engine("ranger", importance = "impurity")

# Set up tuning grid
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(range = c(1, 20)))

# Define tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# Workflow
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(wildfires_recipe2)







