# Library ====
library(tidyverse)

# Población ====
poblacion <- tibble(x = 1:200) %>%
     mutate(error = 10e5 * runif(n(), 0, 20),
            y = 1 + exp(0.5 * x) + error) %>%
     relocate(y) %>%
     write_csv('est_freq/data/raw/poblacion.csv')

# Revisión de la función
poblacion %>%
     ggplot() +
     aes(x, y) +
     geom_point() +
     geom_line(aes( y = 1 + exp(0.5 * x)), color = 'red')









