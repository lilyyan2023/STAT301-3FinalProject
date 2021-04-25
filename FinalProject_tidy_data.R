
# Tidy data ---------------------------------------------------------------

## Load Packages, set seed ----
library("tidyverse")
library("skimr")
library("janitor")
library("tidymodels")
set.seed(123)

## Load raw dataset ----
stroke_dat <- 
  read_csv("data/unprocessed/healthcare-dataset-stroke-data.csv") %>% 
  clean_names() %>% 
  mutate(
    gender = factor(gender), 
    ever_married = factor(ever_married),
    work_type = factor(work_type),
    residence_type = factor(residence_type),
    smoking_status = factor(smoking_status),
    stroke = factor(stroke, levels = c(1, 0), 
                    labels = c("Yes", "No")),
    hypertension = factor(hypertension, levels = c(1, 0), 
                          labels = c("Yes", "No")), 
    heart_disease = factor(heart_disease, levels = c(1, 0), 
                           labels = c("Yes", "No")), 
    bmi = as.numeric(bmi)
  ) %>% 
  select(-id)

## initial skimming ----
skim_without_charts(stroke_dat)

## save processed data ----
stroke_dat %>% 
  write_rds("data/processed/stroke_data.rds")
