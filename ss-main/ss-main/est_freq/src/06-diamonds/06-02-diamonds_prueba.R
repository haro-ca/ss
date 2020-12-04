# Librerías ====
library(tidyverse)
library(tidymodels)

# Prueba ====
mod <- readRDS("est_freq/models/linear_model.rds")
datos <- read_csv("est_freq/data/raw/diamonds_muestra.csv")

diamantes_slice <- diamonds %>% 
     slice_sample(n = 1000) %>% 
     write_csv("est_freq/data/raw/diamonds_muestra.csv")

diamantes <- diamantes_slice %>% 
     mutate(across(c(cut, color, clarity), ~ factor(as.character(.x))),
            price_log = log(price)) %>% 
     select(carat, cut, clarity, color, price_log)

predict(mod, new_data = diamantes)

pred <- bind_cols(predict(mod, new_data = diamantes, type = 'conf_int',
                          level = 0.80), diamantes %>% select(price_log)) %>% 
     mutate(inside = if_else((price_log >= .pred_lower) & (price_log <= .pred_upper), T, F))
pred %>%
     summarise(aciertos = mean(inside))
pred %>% 
     ggplot(aes(.pred, price_log)) +
     geom_point() + 
     geom_abline(slope = 1, intercept = 0)
