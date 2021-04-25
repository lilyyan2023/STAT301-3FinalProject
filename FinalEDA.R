library(tidyverse)
library(skimr)
stroke_data <- readRDS("~/Desktop/STAT 301-3/STAT301-3FinalProject/data/processed/stroke_data.rds")
         
 ggplot(stroke_data, aes(x = stroke, fill = gender)) +
   geom_bar(position = "fill") +
   labs(
     x = "whether to have stroke",
     y = "frequency",
     title = "The relationship between whether to have stroke and gender"
   )
 
 ggplot(stroke_data, aes(x = stroke, fill = ever_married)) +
   geom_bar(position = "fill") +
   labs(
     x = "whether to have stroke",
     y = "frequency",
     title = "The relationship between whether to have stroke and whether ever married"
   )
 stroke_data_wt <- stroke_data %>%
   group_by(stroke) %>% 
   count(work_type) %>% 
   mutate(wt_prop = n / sum(n))
 
 ggplot(stroke_data_wt, aes(x = stroke, y = wt_prop, fill = work_type)) +
   geom_col(position = "dodge") +
   labs(
     x = "whether to have stroke",
     y = "proportion",
     title = "The relationship between whether to have stroke and patient work type"
   )
 
 ggplot(stroke_data, aes(x = stroke, fill = residence_type)) +
   geom_bar(position = "fill") +
   labs(
     x = "whether to have stroke",
     y = "frequency",
     title = "The relationship between whether to have stroke and patient residence type"
   )
 
 stroke_st <- stroke_data %>%
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
 stroke_stg <- stroke_data %>%
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
 stroke_wtr <- stroke_data %>%
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

 ggplot(stroke_data, aes(x = stroke)) +
   geom_bar()
 