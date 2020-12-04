# Librerías ====
library(tidyverse)
library(patchwork)

# Lectura ====
poblacion <- read_csv('est_freq/data/raw/poblacion.csv')

# Muestrea ====
vec_media <- poblacion %>%
     slice_sample(n = 90) %>%
     summarise(mean_y = mean(y)) %>%
     pull()

vec_sd <- poblacion %>%
     slice_sample(n = 90) %>%
     summarise(sd_y = sd(y)) %>%
     pull()

vec_medias <- map_dbl(1:2e3,
        ~ poblacion %>%
             slice_sample(n = 32) %>%
             summarise(mean_y = mean(y)) %>%
             pull())

vec_sd <- map_dbl(1:5e3,
        ~ poblacion %>%
             slice_sample(n = 32) %>%
             summarise(sd_y = sd(y)) %>%
             pull())


poblacion %>%
     summarise(mean(y))

# Media
desv_est <- poblacion %>%
     slice_sample(n = 90) %>%
     summarise(sd_y = sd(y)) %>%
     pull()
desv_est <- poblacion %>%
     slice_sample(n = 90) %>%
     summarise(sd_y = sd(y)) %>%
     pull()

tibble(z = vec_medias) %>%
     ggplot() +
     geom_histogram(aes(x = z)) +
     geom_vline(xintercept = 62e3, color = 'red')

tibble(z = vec_medias) %>%
     summarise(q025 = quantile(z, 0.45),
               q975 = quantile(z, 0.55))

tibble(media = vec_medias) %>%
     ggplot() +
     geom_qq(aes(sample = media), distribution = stats::qnorm)

# Hasta aquí conocemos la población, y sino?
muestra <- poblacion %>%
     slice_sample(n = 80)

muestra %>%
     summarise(mean(y))

pob_gg <- poblacion %>%
     ggplot() +
     geom_histogram(aes(y))

m_gg <- muestra %>%
     ggplot() +
     geom_histogram(aes(y))

(pob_gg + labs(title = 'poblacion')) + (m_gg + labs(title = 'muestra'))


poblacion %>%
     ggplot() +
     geom_qq(aes(sample = y), distribution = stats::qunif,
             color = 'red') +
     geom_qq(aes(sample = y), distribution = stats::qunif,
             data = muestra)


vec_mean_muestra <- map_dbl(1:2e3,
        ~ muestra %>%
             slice_sample(n = 80, replace = T) %>%
             summarise(mean_y = mean(y)) %>%
             pull())

tibble(y = vec_mean_muestra) %>%
     ggplot() +
     geom_histogram(aes(y))

tibble(y = vec_mean_muestra) %>%
     summarise(q025 = quantile(y, 0.05),
               q975 = quantile(y, 0.95))













