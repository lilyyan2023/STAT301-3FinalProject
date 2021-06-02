# Load Packages and set seed
library(tidyverse)
library(tidymodels)
library(themis)
set.seed(123)
# load data
stroke_data <- readRDS("data/processed/stroke_data.rds")

# Split data ----
stroke_split <- initial_split(stroke_data, prop = 0.7, strata = stroke)
stroke_train <- training(stroke_split)
stroke_test <- testing(stroke_split)

## Cross-Validation ----
stroke_folds <- 
  vfold_cv(data = stroke_train, v = 5, repeats = 3, strata = stroke)

# Build recipe ----
# original recipe - no over-sampling
stroke_recipe_orig <- recipe(stroke ~ ., data = stroke_train) %>% 
  step_impute_bag(bmi) %>% # bag impute bmi missing values
  step_log(avg_glucose_level, bmi) %>% 
  step_interact(bmi ~ age) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# recipe with 0.2 ratio over-sampling
stroke_recipe_over_sampling_1 <- recipe(stroke ~ ., data = stroke_train) %>% 
  # over-sampling, 0.2 ratio
  step_upsample(stroke, over_ratio = 0.2) %>%
  step_impute_bag(bmi) %>% # bag impute bmi missing values
  step_log(avg_glucose_level, bmi) %>% 
  step_interact(bmi ~ age) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# recipe with 0.5 ratio over-sampling
stroke_recipe_over_sampling_2 <- recipe(stroke ~ ., data = stroke_train) %>% 
  # over-sampling, 0.5 ratio
  step_upsample(stroke, over_ratio = 0.5) %>%
  step_impute_bag(bmi) %>% # bag impute bmi missing values
  step_log(avg_glucose_level, bmi) %>% 
  step_interact(bmi ~ age) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

prep(stroke_recipe_orig) %>% 
  bake(new_data = NULL)

prep(stroke_recipe_over_sampling_1) %>% 
  bake(new_data = NULL)

prep(stroke_recipe_over_sampling_2) %>% 
  bake(new_data = NULL)

# save setup ----
save(stroke_recipe_orig, stroke_train, stroke_test, stroke_folds, 
     stroke_data, file = "data/stroke_setup.rda")







