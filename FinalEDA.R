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

# outcome var
# plot
stroke_train %>%
   ggplot(aes(stroke, fill = stroke)) + 
   geom_bar(show.legend = FALSE) + 
   labs(
      x = "Stroke", 
      y = "Number of Observations", 
      title = "Distribution of Response Variable"
   ) + 
   geom_text(
      aes(label = ..count..), 
      stat = "count",
      position = position_dodge(width = 0.9), 
      vjust = -0.25
   )

# Numeric Vars
# correlation plot
stroke_train %>% 
   select(age, avg_glucose_level, bmi) %>% 
   
   drop_na() %>% 
   # compute correlation matrix
   cor() %>% 
   # visualize
   corrplot(type = "upper", 
            title = "Correlations between Numeric Variables", 
            mar = c(0, 0, 1, 0))

# correlation between predictors
stroke_train %>% 
   select(age, avg_glucose_level, bmi) %>% 
   drop_na() %>% 
   # compute correlation matrix
   correlate() %>% 
   # turn into a tibble
   stretch() %>% 
   rename("correlation" = "r") %>% 
   arrange(desc(correlation)) %>%
   # temporary var `row_id` to help removing repeated info
   mutate(row_id = row_number()) %>% 
   # filter out even rows
   drop_na() %>% 
   filter(row_id %% 2 == 0) %>% 
   # remove temporary var
   select(-row_id) %>%
   kbl() %>% 
   kable_classic()

# distribution of age
ggplot(stroke_train, aes(age)) + 
   geom_histogram(binwidth = 1) + 
   labs(
      x = "age", 
      y = "number of observations", 
      title = "Distribution of `age` in the Training Set"
   )

# relation between `age` and `stroke`
ggplot(stroke_train, aes(age, stroke)) + 
   geom_boxplot() + 
   labs(
      x = "Age",
      y = "Stroke",
      title = "The relationship between whether to have stroke and age"
   )

# relation between `age` and `heart_disease`
ggplot(stroke_train, aes(age, heart_disease)) + 
   geom_boxplot() + 
   labs(
      x = "Age",
      y = "Heart Disease",
      title = "The relationship between whether to have heart disease and age"
   )

# distribution of avg_glucose_level
ggplot(stroke_train, aes(avg_glucose_level)) + 
   geom_histogram(binwidth = 5) + 
   labs(
      x = "Average Glucose Level", 
      y = "number of observations", 
      title = "Distribution of `avg_glucose_level` in the Training Set"
   )

# distribution of bmi
ggplot(stroke_train, aes(bmi)) + 
   geom_histogram(binwidth = 1) + 
   labs(
      x = "BMI (Body Mass Index)", 
      y = "number of observations", 
      title = "Distribution of `bmi` in the Training Set"
   )

# relation between `avg_glucose_level` and `stroke`
ggplot(stroke_train, aes(avg_glucose_level, stroke)) + 
   geom_boxplot() + 
   labs(
      x = "Average Glucose Level",
      y = "Stroke",
      title = "The relationship between stroke and average glucose level"
   )

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
 