# Librerías ====
library(tidyverse)
library(tidymodels)
library(patchwork)

# Simulación ====
n <- 1e7
df <- tibble(respuesta = rnorm(n, 0.001, 10))

df %>%
     ggplot() +
     geom_histogram(aes(respuesta))


df %>% t.test() %>% broom::tidy()

muestreo <- tibble(n = c(10, 50, 100, 1e3, 1.5e3, 5e3, 10e3, 100e3, 1e6, 1e7)) %>%
     rowwise() %>%
     mutate(muestra = list(rnorm(n, 0.001, 0.1)))

muestreo %>%
     mutate(modelo = list(t.test(muestra) %>% tidy())) %>%
     unnest(modelo) %>%
     ggplot() +
     geom_line(aes(x = n, y = p.value)) +
     geom_hline(yintercept = 0.05, color = 'red')


# Empírico ====
split_inicial <- initial_split(diamonds)
training(split_inicial)
testing(split_inicial)

rsample::bootstraps(df, times = 1000) %>%
        mutate(id = parse_number(id)) %>%
        rowwise() %>%
        mutate(data = list(training(splits)),
               media = mean(data$respuesta)) %>%
        ggplot() +
        geom_histogram(aes(media))

observed_statistic_mean <- df %>%
        specify(response = respuesta) %>%
        calculate(stat = "mean")

bootstrap_dist <- df %>%
        specify(response = respuesta) %>%
        hypothesize(null = "point", mu = 0) %>%
        generate(reps = 1000, type = "bootstrap") %>%
        calculate(stat = "mean")

gg1 <- visualize(bootstrap_dist) +
        shade_p_value(obs_stat = observed_statistic_mean, direction = 'two-sided')

bootstrap_dist %>%
        get_p_value(obs_stat = observed_statistic, direction = 'two-sided')

# Teórico ====
observed_statistic <- df %>%
        specify(response = respuesta) %>%
        calculate(stat = "t")

null_dist <- df %>%
        specify(response = respuesta) %>%
        hypothesize(null = "point", mu = 0) %>%
        calculate(stat = "t")


gg2 <- visualise(null_dist, method = 'theoretical') +
        shade_p_value(obs_stat = observed_statistic, direction = 'two-sided')


df %>% t.test()

gg1 / gg2



lm(price ~ carat, data = diamonds)  %>%
        augment() %>%
        ggplot() +
        geom_point(aes(.fitted, y = .resid))

diamonds %>%
        ggplot(aes(y = price, x = carat)) +
        geom_point() +
        geom_smooth(method = 'lm')








