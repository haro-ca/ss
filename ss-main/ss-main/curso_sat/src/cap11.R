# Librerías
library(tidyverse)

# Datos
casas <- read_csv("data/raw/AmesHousing.csv")

casas_pob <- casas %>% select(id, precio_miles, nombre_zona)
casas_pob %>% sample_n(20) %>% formatear_tabla()
