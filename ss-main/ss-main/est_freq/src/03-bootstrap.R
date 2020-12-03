# Librerías ====
library(tidyverse)

# Lectura ====
poblacion <- read_csv("data/raw/poblacion.csv")
media_pob <- poblacion %>% summarise(mean_y = mean(y))

# Crear muestra
muestra <- poblacion %>%
     slice_sample(n = 80)

# Vamos a hacer el Bootstrapping considerando que únicamente tenemos información de la muestra
bootstrap <- map_dbl(1:2e3,
                     ~ muestra %>%
                          slice_sample(n = 80, replace = T) %>%
                          summarise(mean_y = mean(y)) %>%
                          pull())

tibble(y = bootstrap) %>%
     ggplot() +
     geom_histogram(aes(y))

tibble(media = bootstrap) %>%
     ggplot() +
     geom_qq(aes(sample = media), distribution = stats::qunif)

tibble(y = bootstrap) %>%
     summarise(q025 = quantile(y, 0.05),
               q975 = quantile(y, 0.95))
# Se cumple teorema central del límite, la media poblacional se encuentra dentro del intervalo al 90%
# de la media

