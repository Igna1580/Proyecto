library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)

consumo_alcohol <- read.csv("alcohol-consumption-by-country-2023.csv")
consumo_alcohol <- clean_names(consumo_alcohol)
felicidad <- read.csv("WHR2023.csv")
felicidad <- clean_names(felicidad)
felicidad <- felicidad %>% rename(country = country_name)

base_datos <- inner_join(consumo_alcohol, felicidad, by = "country")

colnames(base_datos)


# Crear un gráfico de dispersión entre felicidad y consumo de alcohol promedio
ggplot(base_datos, aes(x = ladder_score, y = both)) +
  geom_point() +
  labs(x = "Felicidad", y = "Consumo de alcohol", title = "Relación entre felicidad y consumo de alcohol promedio")



# Crear un gráfico de barras horizontales ordenado por ladder_score para hombres
ggplot(base_datos, aes(x = reorder(country, ladder_score), y = male)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Consumo de Alcohol en Hombres", y = "País", title = "Consumo de Alcohol en Hombres por País") +
  #theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 4))

# Crear un gráfico de barras horizontales ordenado por ladder_score para mujeres
ggplot(base_datos, aes(x = reorder(country, ladder_score), y = female)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Consumo de Alcohol en Hombres", y = "País", title = "Consumo de Alcohol en Hombres por País") +
  #theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 4))

