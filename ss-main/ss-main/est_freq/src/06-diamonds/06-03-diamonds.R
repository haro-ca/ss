# Liberías ====
library(tidyverse)
library(tidymodels)
modelo <- read_rds('models/linear_model.rds')


predict(modelo, nuevos_datos)

# Lectura de datos ====
diamantes <- diamonds %>%
     mutate(across(c(cut, color, clarity), ~ factor(as.character(.x))),
            price_log = log(price))


# Construcción primer modelo ====
set.seed(1997)

split_inicial <- initial_split(diamantes)
training <- training(split_inicial)
test <- testing(split_inicial)

lm_model <- linear_reg() %>% set_engine("lm")
lm1_recipe <- recipe(price ~ carat + cut + color + clarity, data = training)

lm1_wflow <- workflow() %>%
     add_recipe(lm1_recipe) %>%
     add_model(lm_model)

lm1_fit <- fit(lm1_wflow, data = training)

lm1 <- lm1_fit %>%
     pull_workflow_fit() %>%
     pluck('fit') %>%
     augment()

lm1_fit %>%
     pull_workflow_fit() %>%
     pluck('fit') %>%
        glance()
# Residuales modelo 1 ====

ggplot(lm1, aes(sample = .resid)) +
        stat_qq() +
        stat_qq_line() +
        ylab("residuales")

ggplot(lm1, aes(.resid)) +
        geom_density() +
        xlab("Residuales")

ggplot(lm1, aes(.fitted, .resid)) +
        geom_point() +
        geom_smooth(method = 'loess', se = F, color = 'red')

ggplot(lm1, aes(.fitted, .std.resid)) +
        geom_point() +
        geom_smooth(method = 'loess', se = F, color = 'red')

lm1 %>%
        filter(.cooksd < 0.0005) %>%
        ggplot(aes(.cooksd)) +
        geom_histogram(binwidth = 1e-5)

# Modelo 2 ====
lm_model <- linear_reg() %>% set_engine("lm")
lm2_recipe <- recipe(price_log ~ carat + cut + color + clarity, data = training) %>%
        step_log(carat)

lm2_wflow <- workflow() %>%
     add_recipe(lm2_recipe) %>%
     add_model(lm_model)

lm2_fit <- fit(lm2_wflow, data = training)

lm2 <- lm2_fit %>%
     pull_workflow_fit() %>%
     pluck('fit') %>%
     augment()

dir.create('models')
write_rds(lm2_fit, 'est_freq/models/linear_model.rds')


# Residuales mod 2 ====

ggplot(lm2, aes(sample = .resid)) +
        stat_qq() +
        stat_qq_line() +
        ylab("Residuales")

ggplot(lm2, aes(.resid)) +
        geom_density() +
        xlab("Residuales")

ggplot(lm2, aes(.fitted, .resid)) +
        geom_jitter() +
        geom_smooth(method = 'loess', se = F, color = 'red')

ggplot(lm2, aes(.fitted, .std.resid)) +
        geom_jitter() +
        geom_smooth(method = 'loess', se = F, color = 'red')

lm2 %>%
        filter(.cooksd < 0.0005) %>%
        ggplot(aes(.cooksd)) +
        geom_histogram(binwidth = 1e-5)


lm2_fit %>% tidy(conf.int = T) %>%
        filter(!str_detect(term, "Intercept")) %>%
        # mutate(conf.low = estimate - t * std.error / sqrt(n),
        #        conf.high = estimate + t * std.error / sqrt(n)) %>%
        ggplot(aes(x = fct_reorder(term, estimate))) +
        geom_point(aes(y = estimate)) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1)



# 1. modelo: price ~ 4cs
# a) scatter de residuales / histograma
# b) errorbar de los intervalos de los coeficientes

# 2. log(price) ~ log(carat) + 3cs
