library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)

consumo_alcohol <- read.csv("alcohol-consumption-by-country-2023.csv")
consumo_alcohol <- clean_names(consumo_alcohol)
felicidad <- read.csv("WHR2023.csv")
felicidad <- clean_names(felicidad)
felicidad <- felicidad %>% rename(country = country_name)


# Cambiar "Turkiye" a "Turkey" en el data frame 'felicidad'
felicidad <- felicidad %>%
  mutate(country = ifelse(country == "Turkiye", "Turkey", country))

# Cambiar "DR Congo" to "Congo (Brazzaville)" en el data frame 'consumo_alcohol'
consumo_alcohol <- consumo_alcohol %>%
  mutate(country = ifelse(country == "DR Congo", "Congo (Brazzaville)", country))

# lista de países en el WHR que no tenemos la información de el consumo de alcohol: 
# Czechia, 	Taiwan Province of China, Kosovo, Hong Kong S.A.R. of China, State of Palestine, Congo (Kinshasa)


# base de datos unidas
base_datos <- inner_join(consumo_alcohol, felicidad, by = "country")

# Tabla solo la información de consumo de alcohol e índice de felicidad
relacion_AlcoholYFelicidad <- base_datos %>%
  select(country, ladder_score, both)



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
  labs(x = "Consumo de Alcohol en mujeres", y = "País", title = "Consumo de Alcohol en Mujeres por País") +
  #theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 4))

# Crear un gráfico de barras horizontales ordenado por ladder_score por país
ggplot(base_datos, aes(x = reorder(country, ladder_score), y = male)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(x = "Consumo de Alcohol por país", y = "País", title = "Consumo de Alcohol por País") +
  #theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 4))


# Tabla solo la información de consumo de alcohol e índice de felicidad
relacion_AlcoholYFelicidad <- base_datos %>%
  select(country, ladder_score, both)


# Create a new column "Region" based on country
relacion_AlcoholYFelicidad <- relacion_AlcoholYFelicidad %>%
  mutate(Region = ifelse(
    country %in% c(
      "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "India", 
      "Indonesia", "Japan", "Kazakhstan", "North Korea", "South Korea", 
      "Kyrgyzstan", "Laos", "Malaysia", "Maldives", "Mongolia", "Myanmar", 
      "Nepal", "Philippines", "Singapore", "Sri Lanka", "Taiwan", "Tajikistan", 
      "Thailand", "Turkmenistan", "Uzbekistan", "Vietnam"
    ), "Asia",
    ifelse(
      country %in% c(
        "Afghanistan", "Algeria", "Bahrain", "Egypt", "Iran", "Iraq", 
        "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", 
        "Oman", "Pakistan", "Qatar", "Saudi Arabia", "Somalia", "Syria", 
        "Tunisia", "Turkey", "United Arab Emirates", "Yemen"
      ), "Middle East, North Africa, and Greater Arabia",
      ifelse(
        country %in% c(
          "Albania", "Andorra", "Armenia", "Austria", "Belarus", "Belgium", 
          "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
          "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", 
          "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", 
          "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "North Macedonia", 
          "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", 
          "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", 
          "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", 
          "United Kingdom", "Vatican City"
        ), "Europe",
        ifelse(
          country %in% c("Canada", "Greenland", "Mexico", "United States"), "North America",
          ifelse(
            country %in% c(
              "Antigua and Barbuda", "The Bahamas", "Barbados", "Belize", "Costa Rica", 
              "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", 
              "Guatemala", "Haiti", "Honduras", "Jamaica", "Nicaragua", "Panama", 
              "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
              "Trinidad and Tobago"
            ), "Central America and the Caribbean",
            ifelse(
              country %in% c(
                "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"
              ), "South America",
              ifelse(
                country %in% c(
                  "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
                  "Cape Verde", "The Central African Republic", "Chad", "Comoros", 
                  "Republic of the Congo", "Congo (Brazzaville)", 
                  "Ivory Coast", "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", 
                  "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", 
                  "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", 
                  "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
                  "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", 
                  "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", 
                  "Togo", "Uganda", "Zambia", "Zimbabwe"
                ), "Sub-Saharan Africa",
                ifelse(
                  country %in% c(
                    "Australia", "East Timor", "Fiji", "Kiribati", "Marshall Islands", 
                    "The Federated States of Micronesia", "Nauru", "New Zealand", "Palau", 
                    "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", 
                    "Vanuatu"
                  ), "Australia and Oceania",
                  "Other"
                )
              )
            )
          )
        )
      )
    )
  ))


# Filter the data for the "Europe" region
europe_data <- relacion_AlcoholYFelicidad %>%
  filter(Region == "Europe")

# Create a ggplot scatter plot

scatter_plot <- ggplot(europe_data, aes(x = both, y = ladder_score, label = country)) +
  geom_point() +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, size = 3) +  # Add text labels
  labs(x = "Both", y = "Scorelader", title = "Scatter Plot of Both vs. Scorelader in Europe")

# Convert the ggplot scatter plot to a plotly object
interactive_scatter_plot <- ggplotly(scatter_plot)

# Display the interactive scatter plot
interactive_scatter_plot

# Filter the data for the "Asia" region
asia_data <- relacion_AlcoholYFelicidad %>%
  filter(Region == "Asia")

# Create a ggplot scatter plot

scatter_plot_Asia <- ggplot(asia_data, aes(x = both, y = ladder_score, label = country)) +
  geom_point() +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, size = 3) +  # Add text labels
  labs(x = "Both", y = "Scorelader", title = "Scatter Plot of Both vs. Scorelader in Asia")

# Convert the ggplot scatter plot to a plotly object
interactive_scatter_plot_Asia <- ggplotly(scatter_plot_Asia)

# Display the interactive scatter plot
interactive_scatter_plot_Asia

#--------Valeria-------------------------------------------------------------------------







#---------------------------------------------------------------------------------

#--------Jose Ignacio------------------------------------------------------------------------------











#--------------------------------------------------------------------------------------

#------Bryan-------------------------------------------------------------------------------------











#------------------------------------------------------------------------------------------
#---Montse--------------------------------------------------





#-----------------------------------------------------
