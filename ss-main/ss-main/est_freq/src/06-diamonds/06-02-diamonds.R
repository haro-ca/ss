# Librerías ====
library(tidyverse)
library(tidymodels)

ggplot(aes(price), data = diamonds) +
     geom_histogram(binwidth = 200) +
     ggtitle('Price')

ggplot(aes(price), data = diamonds) +
     geom_histogram() +
     scale_x_log10()

ggplot(aes(carat, price), data = diamonds) +
     geom_point() +
     scale_y_continuous(trans = log10_trans()) +
     scale_x_continuous(trans = log10_trans())

# Transformación de datos ====
diamantes <- diamonds %>%
     mutate(across(c(cut, color, clarity), ~ factor(as.character(.x))))

set.seed(1234)

split_inicial <- initial_split(diamantes)
training <- training(split_inicial)
test <- testing(split_inicial)

# Carat y x
mod1_recipe <- recipe(price ~ x + y + z + carat, data = training) %>%
     step_log(carat) %>%
     step_normalize(all_numeric())

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <-
     workflow() %>%
     add_recipe(mod1_recipe) %>%
     # add_formula(price ~ x + y + z + carat) %>%
     add_model(lm_model)

prep(mod1_recipe) %>%
     juice()

lm_fit <- fit(lm_wflow, data = training)

preict_mod1 <- predict(lm_fit, training)


# predict_mod1 <- bind_cols(predict_mod1, test %>% select(price))
#
# ggplot(predict_mod1, aes(price, .pred)) %>%
#   geom_abline() %>%
#   geom_point(alpha = 0.3)

# mod2_recipe <- recipe(price ~ carat + cut + color + clarity, data = training) %>%
#   step_dummy(all_nominal()) %>%
#   step_log(price)


mod2_recipe <- recipe(price ~ carat + cut + color + clarity, data = training) %>%
     step_dummy(all_nominal()) %>%
     step_log(carat)


lm_wflow2 <- workflow() %>%
     add_model(lm_model) %>%
     add_recipe(mod2_recipe)

lm_fit_mod2 <- fit(lm_wflow2, data = training)

pred <- predict(lm_fit_mod2, new_data = test)

pred <- bind_cols(pred, test %>% select(price))

mod1 <- lm(price ~ carat, data = training)

lm_fit_mod2 %>%
     pull_workflow_fit() %>%
     tidy()

mod1 %>%
     augment()

# 1. modelo: price ~ 4cs
# a) scatter de residuales / histograma
# b) errorbar de los intervalos de los coeficientes

# 2. log(price) ~ log(carat) + 3cs





