# Library ====
library(tidyverse)
library(tidymodels)
library(rsample)

# Población ====
poblacion <- tibble(x = runif(1000, 0, 10)) %>%
     mutate(error = 10 * runif(n(), -10, 10),
            y = 1 + exp(0.5 * x) + error) %>%
     relocate(y) %>%
     write_csv('data/raw/poblacion.csv')

# Revisión de la función ====
poblacion %>%
     ggplot() +
     aes(x, y) +
     geom_point() +
     geom_line(aes( y = 1 + exp(0.5 * x)), color = 'red')


# Bootstrap

nested_samples <- rsample::mc_cv(poblacion, times = 100, prop = 0.1) %>%
     mutate(id = as.character(parse_number(id))) %>%
     rowwise()

nested_samples %>%
     mutate(training = list(training(splits)),
            modelo = list(t.test(training$y)),
            conf_int = list(broom::tidy(modelo) %>% select(estimate, starts_with('conf')))) %>%
     unnest(conf_int) %>%
     ggplot(aes(x = fct_reorder(id, conf.low))) +
     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
     geom_point(aes(y = estimate), color = 'red') +
     geom_hline(yintercept =  mean(poblacion$y)) +
     theme_classic() +
     theme(axis.text.x = element_blank())

nested_samples %>%
     mutate(training = list(training(splits)),
            modelo = list(cor.test(training$y, training$x)),
            conf_int = list(broom::tidy(modelo))) %>%
     unnest(conf_int)%>%
     ggplot(aes(x = fct_reorder(id, conf.low))) +
     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
     geom_point(aes(y = estimate), color = 'red') +
     geom_hline(yintercept = cor(poblacion$y, poblacion$x)) +
     theme_classic() +
     theme(axis.text.x = element_blank())

nested_samples %>%
     mutate(training = list(training(splits)),
            modelo = list(lm(training$y ~ training$x)),
            coef = list(broom::tidy(modelo) %>%select(term, estimate))) %>%
     unnest(coef) %>%
     pivot_wider(names_from = term, values_from = estimate) %>%
     ggplot(aes(group = id)) +
     geom_abline(intercept = '`(Intercept)`', slope = 'training$x')

