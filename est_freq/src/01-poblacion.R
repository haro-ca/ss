# Library ====
library(tidyverse)

# Población ====
poblacion <- tibble(x = 1:200) %>%
     mutate(error = 10e3 * rt(n(), 20),
            y = log(x) * x ^ 2 + error) %>%
     relocate(y) %>%
     write_csv('data/raw/poblacion.csv')

# Revisión de la función
poblacion %>%
     ggplot() +
     aes(x, y) +
     geom_point() +
     geom_line(aes(y = log(x) * x ^ 2), color = 'red')









