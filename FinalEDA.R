## Load Packages, Set Seed
library(tidyverse)
library(tidymodels)
library(skimr)
library(naniar)
set.seed(123)

stroke_data <- readRDS("data/processed/stroke_data.rds")
## split data ----
stroke_split <- initial_split(stroke_data, prop = 0.7, strata = stroke)
stroke_train <- training(stroke_split)
stroke_test <- testing(stroke_split)
# check dimension
dim(stroke_train)
5110 * 0.7
dim(stroke_test)
5110 * 0.3

## missing data ----

## EDA section ----
 ggplot(stroke_train, aes(x = stroke, fill = gender)) +
   geom_bar(position = "fill") +
   labs(
     x = "whether to have stroke",
     y = "frequency",
     title = "The relationship between whether to have stroke and gender"
   )
 
 ggplot(stroke_train, aes(x = stroke, fill = ever_married)) +
   geom_bar(position = "fill") +
   labs(
     x = "whether to have stroke",
     y = "frequency",
     title = "The relationship between whether to have stroke and whether ever married"
   )
 stroke_train_wt <- stroke_train %>%
   group_by(stroke) %>% 
   count(work_type) %>% 
   mutate(wt_prop = n / sum(n))
 
 ggplot(stroke_train_wt, aes(x = stroke, y = wt_prop, fill = work_type)) +
   geom_col(position = "dodge") +
   labs(
     x = "whether to have stroke",
     y = "proportion",
     title = "The relationship between whether to have stroke and patient work type"
   )
 
 ggplot(stroke_train, aes(x = stroke, fill = residence_type)) +
   geom_bar(position = "fill") +
   labs(
     x = "whether to have stroke",
     y = "frequency",
     title = "The relationship between whether to have stroke and patient residence type"
   )
 
 stroke_st <- stroke_train %>%
   group_by(stroke) %>% 
   count(smoking_status) %>% 
   mutate(st_prop = n / sum(n))
 
 ggplot(stroke_st, aes(x = stroke, y = st_prop, fill = smoking_status)) +
   geom_col(position = "dodge") +
   labs(
     x = "whether to have stroke",
     y = "proportion",
     title = "The relationship between whether to have stroke and patient smoking status"
   )
 stroke_stg <- stroke_train %>%
   group_by(gender) %>% 
   count(smoking_status) %>% 
   mutate(st_prop = n / sum(n))
 
 ggplot(stroke_stg, aes(x = gender, y = st_prop, fill = smoking_status)) +
   geom_col(position = "dodge") +
   labs(
     x = "gender",
     y = "proportion",
     title = "The relationship between patient gender and smoking status"
   )
 stroke_wtr <- stroke_train %>%
   group_by(work_type) %>% 
   count(residence_type) %>% 
   mutate(rt_prop = n / sum(n))
 
 ggplot(stroke_wtr, aes(x = work_type, y = rt_prop, fill = residence_type)) +
   geom_col(position = "dodge") +
   labs(
     x = "work_type",
     y = "proportion",
     title = "The relationship between patient gender and smoking status"
   )

 ggplot(stroke_train, aes(x = stroke)) +
   geom_bar()
 