
library(tidymodels)
library(stringr)

data("Chicago")

us_hol <- timeDate::listHolidays() %>%
  str_subset("(^US)|(Easter)")

chi_rec <- recipe(ridership~., data = Chicago) %>%
  step_holiday(date, holidays = us_hol) %>%
  step_date(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())
# step_normalize(one_of(!!stations))
# step_pca(one_of(!!stations), num_comp = tune())

chi_folds <- rolling_origin(
  Chicago,
  initial = 364 * 15,
  assess = 7 * 4,
  skip = 7 * 4,
  cumulative = FALSE
)

lm(ridership ~ . - date, data = Chicago)
glmn_grid <- expand.grid(
  penalty = 10 ^ seq(-3, -1, length = 20),
  mixture = (0:5) / 5
)

glmn_rec <- chi_rec %>%
  step_normalize(all_predictors())

glmn_mod <- linear_reg(
  penalty = tune(),
  mixture = tune()
)  %>%
  set_engine("glmnet")

ctrl <- control_grid(save_pred = TRUE)

# workflow
glmn_wflow <- workflow() %>%
  add_model(glmn_mod) %>%
  add_recipe(glmn_rec)

glmn_tune <- tune_grid(
  glmn_wflow,
  resamples = chi_folds,
  grid = glmn_grid,
  control = ctrl
)





















