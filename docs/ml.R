
library(tidymodels)
library(parsnip)

# -- PREPROCESSING --
set.seed(4831)
split <- initial_split(mtcars, props = 9/10)
car_train <- training(split)
car_test  <- testing(split)


# The processed versions are:
train_data <- juice(car_rec)
test_data  <- bake(car_rec, car_test)


cars_split <- rsample::initial_split(mtcars)
cars_training <- training(cars_split)
cars_testing <- testing(cars_split)

# recipe
cars_recipe <- recipe(mtcars) %>%
  update_role(
    everything(),
    new_role = "predictor"
  ) %>%
  update_role(
    mpg,
    new_role = "outcome"
  )

car_rec <-
  recipe(mpg ~ ., data = car_train) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep(training = car_train, retain = TRUE)

# model
cars_model <- parsnip::linear_reg() %>%
  set_args(
    penalty      = tune::tune()
  ) %>%
  set_engine("lm") %>%
  set_mode("regression")

set.seed(1432)
cars_model <- parsnip::linear_reg() %>%
  parsnip::set_args(
    penalty      = tune::tune(),
    mixture      = tune()
  ) %>%
  set_engine("lm")
  # fit(mpg ~ ., data = car_train)


# workflow
 cars_wflow <- workflow() %>%
    add_model(cars_model) %>%
    add_recipe(car_rec)



# -- RESAMPLING --

# folds
cars_folds <- vfold_cv(car_train)

# grid
cars_grid <- grid_regular(
  dials::penalty(),
  levels = 5
)

cars_tune <- tune_grid(
  cars_wflow,
  resamples = cars_folds,
  grid = cars_grid
)




### FITTING ###

cars_best <- cars_tune %>%
  show_best() %>%
  `[`(1, )

cars_fit <- finalize_workflow(cars_wflow, cars_best) %>%
  fit(data = car_train)


### VALIDATION ###

cars_validation <- predict(cars_fit, new_data = car_test)

car_accuracy <- yardstick::rmse(
  data = setNames(
    cbind(cars_validation, car_test$mpg),
    c("estimate", "truth")),
  truth= truth,
  estimate = estimate)
df <- (car_accuracy$.estimate)/mean(car_test$mpg)
df*100
iris_validation <- predict(iris_fit, new_data = iris_testing)
​
iris_roc <- yardstick::accuracy(
  data = setNames(
    cbind(iris_validation, iris_testing$Species),
    c("estimate", "truth")
  ),
  truth = truth,
  estimate = estimate






