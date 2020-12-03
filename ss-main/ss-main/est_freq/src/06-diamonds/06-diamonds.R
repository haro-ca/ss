# Librerías
library(tidyverse)
library(tidymodels)

# Initital split
# Ajuste inicial y gràficas de residuales (price ~ tu_criterio)
# Preprocessing (recipes): step_dummy, step_log, step_normalize, step_center, step_interact
# Segundo ajuste y gràficas de residuales
# Opcional: Aproximar el MSE (Cross validation)
# Opcional: Ponerlo en xaringan con el tema de edward rubin

# Manipulación de datos ====
diamantes <- diamonds %>%
     mutate(across(c(cut, color, clarity), ~ factor(as.character(.x))))


# Split inicial ====
split_inicial <- initial_split(diamantes)
training <- training(split_inicial)
test <- testing(split_inicial)

# Correlaciones
cor_x <- str_c("Correlación: ", toString(round(cor(training$price, training$x), 4)))
cor_y <- str_c("Correlación: ", toString(round(cor(training$price, training$y), 4)))
cor_z <- str_c("Correlación: ", toString(round(cor(training$price, training$z), 4)))
cor_carat <- str_c("Correlación: ", toString(round(cor(training$price, training$carat), 4)))


# Ajuste inicial ====
# Primeras gráficas

# Dispersión con X ====
training %>%
     ggplot(aes(price, x)) +
     geom_point() +
     geom_smooth(method = "lm") +
     annotate(geom = "text", x = 15e3, y = 3, label = cor_x, color = "red")

# Dispersión con Y ====
training %>%
     ggplot(aes(price, y)) +
     geom_point() +
     geom_smooth(method = "lm") +
     annotate(geom = "text", x = 15e3, y = 3, label = cor_y, color = "red")

# Dispersión con Z ====
training %>%
     ggplot(aes(price, z)) +
     geom_point() +
     geom_smooth(method = "lm") +
     annotate(geom = "text", x = 15e3, y = 3, label = cor_z, color = "red")

training %>%
     ggplot(aes(price, carat)) +
     geom_point() +
     geom_smooth(method = "lm") +
     annotate(geom = "text", x = 15e3, y = 3.5, label = cor_carat, color = "red")
# Facet

# Modelo 1 ====
model_res1 <- linear_reg() %>%
     set_engine("lm") %>%
     fit(price ~ x + y + z + carat, data = training) %>%
     broom::tidy()

mod1 <- lm(price ~ x + y + z, data = training)
summary(mod1)
par(mfrow = c(2, 2))
plot(mod1)

res_modelo1 <- mod1 %>%
     ggplot(aes(x = .fitted, y = .resid)) +
     geom_point() +
     xlab("Fitted values") + ylab("Residuals") +
     ggtitle("Residuals vs Fitted Plot") +
     theme_bw()

ggsave(res_modelo1, "figures/residuals/residuals_mod1-02.png")
# Existencia de datos atípicos
# R^2 de 0.7849


# Modelo 2 ====
mod2 <- lm(price ~ carat + x + y + z, data = training)
summary(mod2)
par(mfrow = c(2, 2))
plot(mod2)

res_modelo2 <- mod2 %>%
     ggplot(aes(x = .fitted, y = .resid)) +
     geom_point() +
     xlab("Fitted values") + ylab("Residuals") +
     ggtitle("Residuals vs Fitted Plot") +
     theme_bw()


# Modelo 3 C's =====
lin_model <- linear_reg() %>%
     set_engine('lm')

linear_fit <- lin_model %>%
     fit(price ~ cut + color, data = training)

linear_fit %>%
     tidy()

linear_fit %>%
     broom::tidy()

mod3 <- lm(price ~ cut + color + clarity + carat, data = training)
summary(mod3)
par(mfrow = c(2, 2))
plot(mod3)


# Preprocessing ====
# Recipes: step_dummy, step_log, step_normalize, step_center, step_interact
recipe1 <- training %>%
     recipe(price ~ x + y + z + carat, data = training) %>%
     step_log(carat) %>%
     step_normalize(all_numeric())

summary(recipe2)

recipe2 <- training %>%
     recipe(price ~ cut + color + clarity + carat) %>%
     step_dummy(all_nominal()) %>%
     step_interact(~ carat:starts_with(cut))
summary(recipe2)

prep1 <- prep(recipe1, training = training)
prep2 <- prep(recipe2, training = training)

bake1 <- bake(prep1, new_data = training)
bake2 <- bake(prep2, new_data = training)

mod1_ready <- lm(price ~ carat + x + y + z, data = bake1)
summary(mod1_ready)
summary(mod2)





