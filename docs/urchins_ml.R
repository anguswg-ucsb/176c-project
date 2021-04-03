library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)


urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
  setNames(c("food_regime", "initial_volume", "width")) %>%
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


# Preprocess
urch_split <- initial_split(urchins)
urch_train <- training(urch_split)
urch_test <- testing(urch_split)

urch_rec <- recipe(width ~ initial_volume * food_regime, data = urch_train)


lm_mod <-
  linear_reg() %>%
  set_engine("lm")

lm_fit <- lm_mod %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

tidy(lm_fit) %>%
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

new_points <- expand.grid(initial_volume = 20,
                          food_regime = c("Initial", "Low", "High"))


mean_pred <- predict(lm_fit, new_data = new_points)

conf_int_pred <- predict(lm_fit, new_data = new_points,  type = "conf_int")























