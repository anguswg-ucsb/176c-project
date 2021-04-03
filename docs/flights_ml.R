library(tidymodels)
library(nycflights13)
library(skimr) # variable summaries


# Precprocess

set.seed(123)

flight_data <- flights %>%
  mutate(
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = as.Date(time_hour)
  ) %>%
  inner_join(weather, by = c("origin", "time_hour")) %>%
  select(dep_time, flight, origin, dest, air_time, distance,
         carrier, date, arr_delay, time_hour) %>%
  na.omit() %>%
  mutate_if(is.character, as.factor)

flight_data %>%
  count(arr_delay) %>%
  mutate(prop = n/sum(n))

set.seed(555)
data_split <- initial_split(flight_data, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)


# recipe
flights_rec <- recipe(arr_delay ~ ., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") %>% # ID role tells recipe to keep variables but don't use as predict or outcome
  step_date(date, features = c("dow", "month")) %>%
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  step_rm(date) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # Create dummy variables for all of the factor or character columns unless they are outcomes.
  step_zv(all_predictors())

# model
lr_mod <-
  logistic_reg() %>%
  set_engine("glm")


# workflow
flights_wflow  <- workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(flights_rec)

# fit
flights_fit <- flights_wflow %>%
  fit(data = train_data)






















