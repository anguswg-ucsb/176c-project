library(tidymodels)

data(ames)

# Data Exploration

# skewed right, more inexpesnive than expensive
hist(ames$Sale_Price, breaks = 50)

# log transform, expensive house predictions won't have undue influence on model
ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50) +
  scale_x_log10()

ames <- ames %>%
  mutate(Sale_Price = log10(Sale_Price))

pts <- ames %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# neighborhoods
plotly::ggplotly(ggplot() +
  geom_sf(data = pts, aes(col = Neighborhood)))

# Preprocessing

# split
set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

# recipe
ames_rec <- recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude + Longitude,
                    data = ames_train )  %>%
  step_log(Gr_Liv_Area, base = 10, id = "my_id") %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )%>%
  step_ns(Latitude, Longitude, deg_free = 20)

# prep
ames_rec_prepped <- prep(ames_rec)
ames_train_prepped <- bake(ames_rec_prepped, new_data = NULL)
ames_test_prepped <- bake(ames_rec_prepped, ames_test)

# linear regression model
lm_model <-
  linear_reg() %>%
  set_engine("lm")

# workflow
lm_wflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

#  fitting random forest model to training set
library(ranger)

# random forest model
rf_model <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# random forest workflow
rf_wflow <- workflow() %>%
  add_formula(
    Sale_Price ~ Neighborhood +
      Gr_Liv_Area +
      Year_Built +
      Bldg_Type +
      Latitude +
      Longitude
  ) %>%
  add_model(rf_model)

# fit random forest model to training data
rf_fit <- rf_wflow %>%
  fit(data = ames_train)

# predict training set to produce “apparent error rate” or “resubstitution error rate”.
# This function creates predictions and formats the results
estimate_perf <- function(model, dat) {
  # Capture the names of the objects used
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)

  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)

  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(Sale_Price)) %>%
    reg_metrics(Sale_Price, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}

# Resubstitution statistics -- RMSE & R2 computed
estimate_perf(rf_fit, ames_train)
estimate_perf(lm_fit, ames_train)


estimate_perf(rf_fit, ames_test)

set.seed(55)
# CROSS-VALIDATION
ames_folds <- vfold_cv(ames_train, v = 10)

# REPEATED CROSS-VALIDATION
ames_folds <- vfold_cv(ames_train, v = 10, repeats = 5)

# ROLLING FORECASTING ORIGIN RESAMPLING
time_slices <-
  tibble(x = 1:365) %>%
  rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)

data_range <- function(x) {
  summarize(x, first = min(x), last = max(x))
}


map_dfr(time_slices$splits, ~   analysis(.x) %>%
          data_range())

map_dfr(time_slices$splits, ~ assessment(.x) %>%
          data_range())

set.seed(12)
val_set <- validation_split(ames_train, prop = 3/4)
val_set

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(130)
rf_res <- rf_wflow %>%
  fit_resamples(resamples = ames_folds, control = keep_pred)

# performance metrics
collect_metrics(rf_res, summarize = FALSE)

# assesment set predictions
assess_res <- collect_predictions(rf_res)

assess_res %>%
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_point(alpha = .15) +
  geom_abline(col = "red") +
  coord_obs_pred() +
  ylab("Predicted")

over_predicted <-
  assess_res %>%
  mutate(residual = Sale_Price - .pred) %>%
  arrange(desc(abs(residual))) %>%
  slice(1)


ames_train %>%
  slice(over_predicted$.row) %>%
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)

val_res <- rf_wflow %>%
  fit_resamples(resamples = val_set)

collect_metrics(val_res)



ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

ames_metrics <- metric_set(rmse, rsq, mae)

ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

lm_form_fit <- lm_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

# bind results with original data
ames_test_small <- ames_test %>% slice(1:5)

ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(lm_form_fit, ames_test_small)) %>%
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

# decision tree model
tree_model <-
  decision_tree(min_n = 2) %>%
  set_engine("rpart") %>%
  set_mode("regression")
tree_fit <-
  tree_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(tree_fit, ames_test_small))



broom::glance(lm_fit)
tidy(lm_fit)
tidy(ames_rec_prepped, id = "my_id")
predict(lm_fit, ames_test_prepped %>% head())






































