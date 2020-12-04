# Librerías =====
library(tidyverse)
library(tidylog)
library(tidymodels)
library(viridis)

# Parámetros gráficos ====
rojo = '#e63946'
theme_set(ggthemes::theme_clean() + theme(panel.grid.major.y = element_blank()))

# Simulación ====
n <- 1e3
poblacion <- tibble(educacion = sample(seq(6, 23, 1), size = n, replace = T), 
       ingreso = 8000 + 1000 * educacion + rnorm(n, sd = 1400)) %>% 
     filter(ingreso > 0)


poblacion %>% 
     ggplot(aes(educacion, ingreso)) +
     geom_point() +
     geom_smooth(method = 'lm') +
     geom_point(data = tibble(educacion = 20, ingreso = 28102), color = 'red', size = 4)

lm_model <- linear_reg() %>%
     set_engine("lm")

lm_fit <- 
     lm_model %>% 
     fit(ingreso ~ educacion, data = poblacion)

poblacion %>% 
        ggplot() +
        geom_histogram(aes(ingreso))
# CEF 
cef <- poblacion %>% 
     mutate(educacion = factor(educacion)) %>% 
     group_by(educacion) %>% 
     summarise(ingreso = mean(ingreso))

poblacion %>% 
     ggplot() +
     facet_wrap(vars(educacion)) +
     geom_density(aes(ingreso)) +
     geom_vline(aes(xintercept = esperanza_condicional),
                data = cef) +
     scale_x_continuous(labels = scales::dollar)


lm_data <- predict(lm_fit, new_data = tibble(educacion = seq(6, 23, 1))) %>% 
     bind_cols(tibble(educacion = factor(seq(6, 23, 1)))) %>% 
     rename(ingreso = .pred)

poblacion %>% 
     ggplot(aes(x = ingreso, y = educacion)) +
     ggridges::geom_density_ridges_gradient(aes(x = ingreso, y = factor(educacion), 
                                                fill = stat(x)), 
                                   alpha = 0.2) +
     geom_path(group = 1, color = 'white', data = cef) +
     geom_point(color = 'white', data = cef) +
     geom_path(group = 1, color = 'red', data = lm_data) +
     scale_fill_viridis(option = "magma") +
     coord_flip()


# CEF no lineal
diamonds_binned <- diamonds %>% 
                    mutate(carat_bins = santoku::chop(carat, breaks = seq(0, 5.01, by = 0.2)))

lm_fit <- 
     lm_model %>% 
     fit(price ~ carat_bins,
         data = diamonds %>% 
                 mutate(carat_bins = santoku::chop(carat, breaks = seq(0, 5.01, by = 0.2)))) 

lm_data <- predict(lm_fit, new_data = diamonds_binned %>% summarise(carat_bins = levels(carat_bins))) %>% 
             bind_cols(diamonds_binned %>% summarise(carat_bins = levels(carat_bins))) %>% 
             rename(price = .pred)

cef_binned <- diamonds %>% 
        mutate(carat_bins = santoku::chop(carat, breaks = seq(0, 5.01, by = 0.2))) %>% 
        group_by(carat_bins) %>% 
        summarise(price = mean(price))

diamonds %>% 
        mutate(carat_bins = santoku::chop(carat, breaks = seq(0, 5.01, by = 0.2))) %>% 
        ggplot(aes(x = price, y = carat_bins)) +
        ggridges::geom_density_ridges_gradient(aes(fill = stat(x)), 
                                   alpha = 0.2) +
        geom_path(group = 1, color = 'grey', data = cef_binned, size = 1.2) +
        geom_point(color = 'grey', size = 1.2, data = cef_binned) +
        geom_path(group = 1, color = 'red', data = lm_data) +
        scale_fill_viridis(option = "magma") +
        coord_flip()

diamonds %>% 
        ggplot(aes(y = price, x = carat)) +
        geom_point() +
        geom_smooth(method = 'lm') 


