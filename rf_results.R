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
  add_recipe(stroke_recipe)