# Library ====
library(tidyverse)

# Población ====
poblacion <- tibble(x = runif(1000, 0, 10)) %>%
     mutate(error = 10 * runif(n(), -10, 10),
            y = 1 + exp(0.5 * x) + error) %>%
     relocate(y) %>%
     write_csv('data/raw/poblacion.csv')

# Revisión de la función
poblacion %>%
     ggplot() +
     aes(x, y) +
     geom_point() +
     geom_line(aes( y = 1 + exp(0.5 * x)), color = 'red')









