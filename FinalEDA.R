## Load Packages, Set Seed
library(tidyverse)
library(tidymodels)
library(skimr)
library(naniar)
library(kableExtra)
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

# skim the trainig set
stroke_train %>% skim_without_charts()

## missing data ----

# overview of missingness
stroke_train %>% 
   vis_miss(sort_miss = TRUE)

# overview table
stroke_train %>% 
   miss_var_summary() %>% 
   kbl() %>% 
   kable_classic() %>% 
   kable_styling(bootstrap_options = c("striped", "hover")) %>% 
   row_spec(1, color = "white",
            background = "blue") %>% 
   kableExtra::footnote(
      general = "Number and Percentage of Missing Values in Each Variable"
   )

## EDA section ----
# Categorial variables on its own
ggplot(stroke_train, aes(x = gender)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_dodge(width = 0.9), 
    vjust = -0.25,
    hjust = 0.5
  ) +
  labs(
    title = "Distribution of gender"
  ) +
  theme_minimal()

ggplot(stroke_train, aes(x = ever_married)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_dodge(width = 0.9), 
    vjust = -0.25,
    hjust = 0.5
  ) +
  labs(
    title = "Distribution of ever_married"
  ) +
  theme_minimal()

ggplot(stroke_train, aes(x = smoking_status)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_dodge(width = 0.9), 
    vjust = -0.25,
    hjust = 0.5
  ) +
  labs(
    title = "Distribution of smoking_status"
  ) +
  theme_minimal()

ggplot(stroke_train, aes(x = work_type)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_dodge(width = 0.9), 
    vjust = -0.25,
    hjust = 0.5
  ) +
  labs(
    title = "Distribution of work_type"
  ) +
  theme_minimal()

ggplot(stroke_train, aes(x = residence_type)) +
  geom_bar() +
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_dodge(width = 0.9), 
    vjust = -0.25,
    hjust = 0.5
  ) +
  labs(
    title = "Distribution of residence_type"
  ) +
  theme_minimal()

# Categorical variables and outcome variable 

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
 # among categorical variable 
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
     title = "The relationship between patient work type and residence type"
   )

 stroke_mw <- stroke_train %>%
   group_by(work_type) %>% 
   count(ever_married) %>% 
   mutate(m_prop = n / sum(n))
 
 ggplot(stroke_mw, aes(x = work_type, y = m_prop, fill = ever_married)) +
   geom_col(position = "dodge") +
   labs(
     x = "work_type",
     y = "proportion",
     title = "The relationship between patient work type\nand whether patient has ever married"
   )
 ggplot(stroke_train, aes(x = stroke)) +
   geom_bar()
 