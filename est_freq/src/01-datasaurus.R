# Librerías ====
library(tidyverse)

# Lectura ====
datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

#
datasaurus %>%
     group_by(dataset) %>%
     summarise(cor = cor(x, y),
               media = mean(x),
               media_y = mean(y),
               var_x = var(x),
               var_y = var(y))
datasaurus %>%
     nest_by(dataset) %>%
     mutate(regresion = list(lm(y ~ x, data = data)),
            conf_int = list(broom::confint_tidy(regresion))) %>%
     unnest(conf_int) %>%
     ggplot() +
     geom_errorbar(aes(x = fct_reorder(dataset, conf.low),
                       ymin = conf.low, ymax = conf.high),
                   width = .1) +
     theme_classic()

datasaurus %>%
     ggplot(aes(x, y)) +
     geom_point() +
     facet_wrap(vars(dataset))

squarewave2 <- function (t) {

     # 0.01  == 100Hz(=2Pi) -> 1 Period of the Squarewave
     # 0.005 == Pi          -> Half Period of the Squarewave
     # if t smaller than a half period -> 1
     # if t greater or equal than half a period -> 0

     ifelse(((t %% 1) < 0.05), 1, 0)
}

plot(squarewave2)

xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
wave.3 <- 0.25 * wave.1 +  * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

tibble(error = rnorm(10e3, 0, 1)) %>%
     mutate(x = runif(n(), 1, 5),
            y = 1.6 * sin(3*x) + 0.5 * sin(20 * x) + error,
            y = case_when(
                 x < 1 ~ y,
                 x < 2 ~ y + 1,
                 x < 3 ~ y + 2,
                 x < 4 ~ y + 3,
                 x < 5 ~ y + 4)) %>%
     ggplot(aes(x, y)) +
     geom_point() +
     geom_smooth(method = 'lm')
exp(c(1, 2, 3))


