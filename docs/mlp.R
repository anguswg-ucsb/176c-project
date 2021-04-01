library(tidymodels)

# Cleaning

# state GDP per capita
gdp <- readxl::read_xls("data/state_gdp_per_capita.xls")
gdp$state <- gdp$...1
gdp <- gdp %>%
  select(20, 21)
gdp <- gdp %>%
  filter(!state %in% c("District of Columbia", "United States")) %>%
  slice(n = 1:50)
gdp <- rename(gdp, gdp_pc = "2015")

# US arrests data
crime <- USArrests
crime$state <- rownames(crime)
rownames(crime) <- 1:nrow(crime)

# add regions and state gdp per cap
crime$region <- state.region
crime$gdp <- gdp$gdp_pc
crime <- janitor::clean_names(crime)
tmp1 <- scale(crime$gdp)
heatmaply::
hist(crime$assault)
plot(density(crime$assault))

# --- PREPROCESSING ---
set.seed(142)

crime_split <- initial_split(crime, prop = .67, strata = region)
crime_test <- testing(crime_split)
crime_train <- training(crime_split)

# recipe
crime_rec <- recipe(crime) %>%
  update_role(
    urban_pop, rape, assault, gdp, murder,
    new_role = "predictor"
  ) %>%
  update_role(
    region,
    new_role = "outcome"
  )

# model
crime_model <- parsnip::mlp() %>%
  parsnip::set_args(
    hidden_units = tune::tune(),
    penalty      = tune::tune(),
    dropout      = tune::tune(),
    epochs       = tune::tune(),
    activation   = tune::tune()
  ) %>%
    set_engine("nnet") %>%
    set_mode("classification")
head(crime)
# workflow
crime_wflow <- workflow() %>%
  add_model(crime_model) %>%
  add_recipe(crime_rec)

# --- RESAMPLING ---

# folds
crime_folds <- vfold_cv(crime_train)

# grid
crime_grid <- grid_regular(
  dials::hidden_units(),
  dials::penalty(),
  dials::dropout(),
  dials::epochs(),
  dials::activation(),
  levels = 2
)

# tune
crime_tune <- tune_grid(
  crime_wflow,
  resamples = crime_folds,
  grid = crime_grid
)

# --- FITTING ---
crime_best <- crime_tune %>%
  show_best("roc_auc") %>%
  `[`(1, )


crime_fit <- finalize_workflow(crime_wflow, crime_best) %>%
  fit(data = crime_train)

# --- VALIDATION ---
crime_validate <- predict(crime_fit, new_data = crime_test)


crime_roc <- accuracy(
  data = setNames(
    cbind(crime_validate, crime_test$region),
    c("estimate", "truth")
  ),
  truth = truth,
  estimate = estimate
)

tmp2 <- crime_validate %>%
  mutate(est = crime_validate[,1])

tmp1 <- crime_test %>%
  mutate(est = crime_validate[,1])






dt1 <-rpart::rpart(
  formula = region ~ .,
  data    = crime,
  method  = "anova"
)

rpart.plot::rpart.plot(dt1)
rpart::plotcp(dt1)
crime_model <- parsnip::

lm1 <- lm(formula= assault ~ urban_pop + gdp, data = crime_train)
summary(lm1)













