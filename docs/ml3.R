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






































