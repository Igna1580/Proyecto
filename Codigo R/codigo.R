library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(cowplot)
library(nortest)
library(xtable)


#---CSV y formatos--------------------------------------------------------------
#Abrir y dar formato a las bases de datos

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

# Cerar una nueva columna "Region" basado en el pais
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

# Filtrar para region europea
europe_data <- relacion_AlcoholYFelicidad %>%
  filter(Region == "Europe")
# Filtrar para region asiatica
asia_data <- relacion_AlcoholYFelicidad %>%
  filter(Region == "Asia")


#---Creacion de variables globales----------------------------------------------


#---Graficos--------------------------------------------------------------------

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



# Crear un grafico de dispersion 
scatter_plot <- ggplot(europe_data, aes(x = both, y = ladder_score, label = country)) +
  geom_point() +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, size = 3) +
  labs(x = "Both", y = "Scorelader", title = "Scatter Plot of Both vs. Scorelader in Europe")
# convertirlo a interactivo
interactive_scatter_plot <- ggplotly(scatter_plot)
# Mostrarlo
interactive_scatter_plot


# Crear un grafico de dispersion 
scatter_plot_Asia <- ggplot(asia_data, aes(x = both, y = ladder_score, label = country)) +
  geom_point() +
  geom_text(nudge_x = 0.1, nudge_y = 0.1, size = 3) +  
  labs(x = "Both", y = "Scorelader", title = "Scatter Plot of Both vs. Scorelader in Asia")
interactive_scatter_plot_Asia <- ggplotly(scatter_plot_Asia)
interactive_scatter_plot_Asia

#---Bloque Valeria--------------------------------------------------------------

#Graficos
distribucion_felicidad <- ggplot(relacion_AlcoholYFelicidad2, aes(x=ladder_score)) +
  geom_histogram(binwidth = 0.5, col='black', fill= 'pink')+
  labs(x= "Ladder_Score", y="Frecuencia")
distribucion_felicidad
density_plot_f <- ggplot(data.frame(x = relacion_AlcoholYFelicidad2$ladder_score), aes(x = x)) +
  geom_density(fill = "pink", alpha = 0.5) +
  labs(x = "Valor", y = "Densidad") +
  theme_minimal()
density_plot_f
distribucion_alcohol <- ggplot(relacion_AlcoholYFelicidad2, aes(x=both)) +
  geom_histogram(binwidth = 0.7, col='black', fill= 'purple')+
  labs(x= "Alcohol consumption", y="Frecuencia")
distribucion_alcohol
density_plot_a <- ggplot(data.frame(x = relacion_AlcoholYFelicidad2$both), aes(x = x)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(x = "Valor", y = "Densidad") +
  theme_minimal()
density_plot_a

#verificacion con colas
plot(density(relacion_AlcoholYFelicidad2$both))
plot(density(relacion_AlcoholYFelicidad2$ladder_score))

#pruebas de hipotesis: los datos provienen de una distribución normal
ks_test_felicidad <- ks.test(relacion_AlcoholYFelicidad2$ladder_score, "pnorm", mean = mean(relacion_AlcoholYFelicidad2$ladder_score), sd = sd(relacion_AlcoholYFelicidad2$ladder_score))
print("Resultados del test de Kolmogorov-Smirnov para el grupo 1:")
print(ks_test_felicidad)

ks_test_alcohol <- ks.test(relacion_AlcoholYFelicidad2$both, "pnorm", mean = mean(relacion_AlcoholYFelicidad2$both), sd = sd(relacion_AlcoholYFelicidad2$both))
print("Resultados del test de Kolmogorov-Smirnov para el grupo 2:")
print(ks_test_alcohol)

ad_test_felicidad <- ad.test(relacion_AlcoholYFelicidad2$ladder_score)
print(ad_test_felicidad)
ad_test_alcohol <- ad.test(relacion_AlcoholYFelicidad2$both)
print(ad_test_alcohol) #p<0.05 => se puede rechazar la hipotesis nula (los datos tienen distribucion normal)

lil_test_felicidad <- lillie.test(relacion_AlcoholYFelicidad2$ladder_score)
print(lil_test_felicidad)
lil_test_alcohol <- lillie.test(relacion_AlcoholYFelicidad2$both)
print(lil_test_alcohol) #p<0.05 => se puede rechazar la hipotesis nula (los datos tienen distribucion normal)



#---Bloque Jose Ignacio---------------------------------------------------------

#Buscando el paraemtro de maxima verosimilitud respecto a felicidad
n = length(base_datos$ladder_score)
sum.felicidad = sum(base_datos$ladder_score)
mu = sum.felicidad/n
sigma.cuadrado = (sum((base_datos$ladder_score-mu)^2))/n
theta = seq(1.778, 7.804, length.out = 1000)

#Grafico de Maxima Verosimilitud
ggplot(data.frame(
  theta = theta,
  L = (1/sqrt(2*pi*sigma.cuadrado))*exp((-(theta-mu)^2)/(2*sigma.cuadrado))), 
  aes(x = theta , y = L)) +
  geom_line(linewidth = 2) +
  geom_vline(aes(xintercept = mean(base_datos$ladder_score)), linewidth = 2, color = "red") +
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud con la media de x", x = "Theta", y =
      "Verosimilitud"
  )








#---Bloque Bryan----------------------------------------------------------------


Nombres <- c("NA", "Both", "Male", "Female", "Ladder Score", "Standard Error of Ladder Score",
             "Upperwhisker", "Lowerwhisker", "Logged GDP per Capita", "Social Support",
             "Healthy Life Expectancy", "Freedom to Make Life Choices", "Generosity",
             "Perception of Corruption", "Ladder Score in Dystopia",
             "Explained by Logged GDP per Capita", "Explained by Social Support",
             "Explained by Healthy Life Expectancy", "Explained by Freedom to Make Life Choices", 
             "Explained by Generosity", "Explained by Perception of Corruption","Dystopia Residual" )




Minimo <- 0
Maximo <- 0
Rango <- 0
Media <- 0
Mediana <- 0
PrimerCuartil <- 0
TercerCuartil <- 0
RangoIntercuartil <- 0
DesviacionStandar <- 0
Varianza <- 0
CoeficienteVariacion <- 0


for(i in 2:22){
  Minimo <- append(Minimo, min(base_datos[,i])) 
  Maximo <- append(Maximo, max(base_datos[,i])) 
  Rango <- append(Rango, max(base_datos[,i]) - min(base_datos[,i]))
  Media <- append(Media, mean(base_datos[,i]))
  Mediana <- append(Mediana, median(base_datos[,i]))
  PrimerCuartil <- append(PrimerCuartil, quantile(base_datos[,i], 0.25))
  TercerCuartil <- append(TercerCuartil, quantile(base_datos[,i], 0.75))
  RangoIntercuartil <- append(RangoIntercuartil, IQR(base_datos[,i]))
  DesviacionStandar <- append(DesviacionStandar, sd(base_datos[,i]))
  Varianza <- append(Varianza, var(base_datos[,i]))
  CoeficienteVariacion <- append(CoeficienteVariacion, sd(base_datos[,i])/
                                   mean(base_datos[,i]))
  
}


AnalisisD <- data.frame(Nombres, Minimo, Maximo, Rango, Media, PrimerCuartil,
                        Mediana, TercerCuartil, RangoIntercuartil, DesviacionStandar,
                        Varianza, CoeficienteVariacion)





#---Bloque Montse---------------------------------------------------------------

# Gráfico 1: - Consumo de Alcohol por País, codificado con Escala de Color según Índice de Felicidad
# Para aumentar el tamaño del gráfico 
options(repr.plot.width = 4, repr.plot.height = 10)

# Ordenar el data frame por consumo de alcohol de menor a mayor
relacion_AlcoholYFelicidad2 <- relacion_AlcoholYFelicidad %>%
  arrange(both)

# Crear un gráfico de barras horizontales
options(repr.plot.width = 4, repr.plot.height = 15)  # Aumenta la altura
gráfico_Barras1 <- ggplot(relacion_AlcoholYFelicidad2, aes(x = both, y = reorder(country, both), fill = ladder_score)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Promedio anual de consumo de alcohol puro per cápita en litros",
    y = "País",
    title = "Consumo de alcohol por país ordenado de mayor a menor",
    subtitle = "Codificado con escala de color según Índice de felicidad",
    fill = "Índice de felicidad"
  ) + 
  scale_x_continuous(breaks = 0:13, labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13")) +
  scale_fill_gradient(low = "red", high = "green") +  # Escala de colores de rojo a verde
  theme_minimal() + cowplot::theme_cowplot() + # Aplicar un tema minimalista
  theme(axis.text.y = element_text(size = 7, hjust = 0, color = "black"),
        axis.text.x = element_text(size = 7, hjust = 0, color = "black"),
        axis.title.x = element_text(size = 10, color = "black"),  # Tamaño del título del eje x
        axis.title.y = element_text(size = 10, color = "black"),# Ajustar el tamaño y alineación del texto en el eje y
        plot.title = element_text(size = 11, color = "black"),
        plot.subtitle = element_text(size = 11, color = "black"),# Ajustar el tamaño del título
        legend.text = element_text(size = 10, color = "black"),  # Ajustar el tamaño del texto en el color key
        legend.title = element_text(size = 10, color = "black")) +  # Ajustar el tamaño del título del color key
  guides(fill = guide_colorbar(barwidth = 2, barheight = 6, title.position = "top"))  # Ajustar el tamaño del color key y su posición

# Guardar el gráfico llamado "consumo_MenorAMayor_color"
consumo_MenorAMayor_color <- gráfico_Barras1

# Mostrar el gráfico
print(consumo_MenorAMayor_color)

pdf("grafico_barras.pdf", width = 8.27, height = 11.69)  # Tamaño A4 en pulgadas
print(gráfico_Barras1)
#ggsave("grafico_barras.pdf", width = 8.27, height = 11.69, dpi = 500)


# Gráfico 2: Promedio de Consumo de Alcohol por Región, ordenado por Índice de Felicidad Promedio
  
  # Calcular los promedios por región
  promedios_region <- relacion_AlcoholYFelicidad %>%
  group_by(Region) %>%
  summarise(
    Promedio_Alcohol = mean(both),
    Promedio_Felicidad = mean(ladder_score)
  ) %>%
  arrange(desc(Promedio_Felicidad))  # Ordenar por índice de felicidad promedio de mayor a menor

# Modificacón los nombres de las regiones 
promedios_region$Region <- case_when(
  promedios_region$Region == "Europe" ~ "Europa",
  promedios_region$Region == "Sub-Saharan Africa" ~ "Africa y Africa Sub-sahariana",
  promedios_region$Region == "Asia" ~ "Asia",
  promedios_region$Region == "Australia and Oceania" ~ "AUS y OCE",
  promedios_region$Region == "North America" ~ "Norteamérica",
  promedios_region$Region == "South America" ~ "Suramérica",
  promedios_region$Region == "Middle East, North Africa, and Greater Arabia" ~ "M. Oriente y  N. África",
  promedios_region$Region == "Central America and the Caribbean" ~ "Centroamérica y el Caribe",
  TRUE ~ promedios_region$Region  # Mantener otros nombres sin cambios
)



# Crear el gráfico de barras
grafico_promedios <- ggplot(promedios_region, aes(x = reorder(Region, -Promedio_Felicidad), y = Promedio_Alcohol, fill = Promedio_Felicidad)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Región",
    y = "Promedio de consumo de alcohol",
    title = "Promedio de consumo de alcohol por región",
    subtitle = "Ordenado por Índice de felicidad promedio"
  ) +
  theme_minimal() +
  cowplot::theme_cowplot() +
  theme(
    plot.title = element_text(size = 20),  # Tamaño del título
    plot.subtitle = element_text(size = 18),  # Tamaño del subtítulo
    axis.title.x = element_text(size = 18),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 18),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  # Texto del eje x inclinado
    axis.text.y = element_text(size = 18),  # Tamaño del texto del eje y
    plot.margin = margin(20, 20, 20, 20)  # Márgenes del gráfico
  ) + 
  scale_fill_gradient(low = "red", high = "green", name = "Índice de felicidad") +
  geom_text(aes(label = round(Promedio_Felicidad, 2)), vjust = -0.5)  # Ajustar el tamaño del texto a 18

# Mostrar el gráfico
grafico_promedios

# Ajustar el tamaño del gráfico (ancho y alto)
ggsave(filename = "grafico_barras2.pdf", plot = grafico_promedios, device = "pdf", width = 10, height = 8)



# Gráfico 3: Gráfico de dispersión con línea de regresión

# Crear el gráfico
grafico_dispersion <- ggplot(relacion_AlcoholYFelicidad, aes(x = both, y = ladder_score)) +
  geom_point() + cowplot::theme_cowplot() +  # Gráfico de dispersión
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Línea de regresión
  labs(
    x = "Consumo de alcohol en litros de alcohol puro",
    y = "Índice de felicidad",
    title = "Gráfico de dispersión con línea de regresión",
    subtitle = "Relación Índice de felicidad y consumo de alcohol"
  ) +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18),# Tamaño del título
    axis.title.x = element_text(size = 18),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 18),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 18),  # Tamaño del texto del eje x
    axis.text.y = element_text(size = 18),  # Tamaño del texto del eje y
    plot.margin = margin(20, 20, 20, 20)  # Márgenes del gráfico
  )

# Mostrar el gráfico 
print(grafico_dispersion)
# Ajustar el tamaño del gráfico (ancho y alto)
ggsave(filename = "grafico_dispersion.pdf", plot = grafico_dispersion, device = "pdf", width = 10, height = 8)


# Gráfico 4: Gráfico de Caja y Bigotes del Índice de Felicidad del 30% de los Países con Mayor Consumo de Alcohol

# Ordenar los datos por consumo de alcohol de mayor a menor
relacion_AlcoholYFelicidad2 <- relacion_AlcoholYFelicidad %>%
  arrange(desc(both))

# Calcular el número de países que representa el 30%
n_paises <- nrow(relacion_AlcoholYFelicidad2)
n_paises_30_porcentaje <- round(0.30 * n_paises)

# Seleccionar el 30% superior de países en función del consumo de alcohol
paises_seleccionados <- head(relacion_AlcoholYFelicidad, n_paises_30_porcentaje)

# Crear el gráfico de caja y bigotes del índice de felicidad para los países seleccionados
caja_y_bigotes_felicidad <- ggplot(paises_seleccionados, aes(y = ladder_score)) +
  geom_boxplot() + cowplot::theme_cowplot() +  # Agregar el gráfico de caja y bigotes
  labs(
    y = "Índice de Felicidad",
    title = "Caja y Bigotes del Índice de Felicidad del",
    subtitle = "Considerando el 30% de los Países con Mayor Consumo de Alcohol"
  )

# Mostrar el gráfico de caja y bigotes
print(caja_y_bigotes_felicidad)

# Gráfico 5: Gráfico de Caja y Bigotes del Índice de Felicidad por Región considerando el 80% de países con mayor consumo de alcohol

# Calcular el 80% de los países con mayor consumo de alcohol en cada región

pct <- 0.8  # Porcentaje del 80%
top_countries <- relacion_AlcoholYFelicidad %>%
  group_by(Region) %>%
  arrange(desc(both)) %>%
  slice(1:round(n() * pct)) %>%
  pull(country)

# Filtrar los datos para incluir solo esos países
filtered_data <- relacion_AlcoholYFelicidad %>%
  filter(country %in% top_countries)

# Crear un gráfico de caja y bigotes por región

# Nuevos nombres de las regiones
nuevos_nombres <- c(
  "Europe" = "Europa",
  "Sub-Saharan Africa" = "Africa y Africa Sub-sahariana",
  "Asia" = "Asia",
  "Australia and Oceania" = "AUS y OCE",
  "North America" = "Norteamérica",
  "South America" = "Suramérica",
  "Middle East, North Africa, and Greater Arabia" = "M. Oriente y N. África",
  "Central America and the Caribbean" = "Centroamérica y el Caribe"
)

# Cambiar los nombres de las regiones en los datos
filtered_data$Region <- nuevos_nombres[filtered_data$Region]

# Crear el gráfico de caja y bigotes con los nuevos nombres de regiones en el eje x
caja_y_bigotes_region <- ggplot(filtered_data, aes(x = Region, y = ladder_score, fill = Region)) +
  geom_boxplot(outlier.color = "red") + cowplot::theme_cowplot() +
  labs(
    y = "Índice de Felicidad",
    title = "Gráfico de Caja y Bigotes del Índice de Felicidad por Región",
    subtitle = "Considerando el 80% de países con mayor consumo de alcohol"
  ) +
  theme_minimal() +
  scale_fill_discrete(name = "Región") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + cowplot::theme_cowplot()  # Inclinar el eje x

caja_y_bigotes_region

# otra version 
# Calcular los deciles del consumo de alcohol y asignarlos a una nueva variable
relacion_AlcoholYFelicidad2 <- relacion_AlcoholYFelicidad2 %>%
  mutate(decil_alcohol = ntile(both, 10))

# Crear el gráfico de caja y bigotes por deciles de consumo de alcohol
caja_y_bigotes_deciles <- ggplot(relacion_AlcoholYFelicidad2, aes(x = factor(decil_alcohol), y = ladder_score)) +
  geom_boxplot(outlier.color = "red") + cowplot::theme_cowplot() +
  labs(
    x = "Decil de Consumo de Alcohol",
    y = "Índice de Felicidad",
    title = "Gráfico de Caja y Bigotes del Índice de Felicidad por Decil de Consumo de Alcohol"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  # Inclinar el eje x

# Mostrar el gráfico
print(caja_y_bigotes_deciles)

# Gráfico 6: Índice de Felicidad Promedio por Decil de Consumo de Alcohol

# Calcular el promedio del índice de felicidad para cada decil
promedio_felicidad_por_decil <- relacion_AlcoholYFelicidad2 %>%
  group_by(decil_alcohol) %>%
  summarize(promedio_felicidad = mean(ladder_score))


# Crear un gráfico de línea con límites personalizados en el eje x

# Crear el gráfico de línea con líneas verticales y horizontales de referencia
gráfico_Línea <- ggplot(promedio_felicidad_por_decil, aes(x = decil_alcohol, y = promedio_felicidad)) +
  geom_line() +
  labs(
    x = "Decil",
    y = "Índice de felicidad promedio",
    title = "Índice de felicidad promedio según decil de consumo de alcohol"
  ) +
  theme_minimal() +
  cowplot::theme_cowplot() +
  scale_x_continuous(breaks = 0:10) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Agregar líneas verticales y horizontales
  geom_vline(xintercept = 1:10, linetype = "dashed", color = "gray", linewidth = 0.5) +
  geom_hline(yintercept = seq(4, 8, 1), linetype = "dashed", color = "gray", linewidth= 0.5)

# Ajustar el tamaño del gráfico (ancho y alto)
ggsave(filename = "grafico_linea.pdf", plot = gráfico_Línea, device = "pdf", width = 10, height = 8)


# Cuadro 1: Correlación entre Consumo de alcohol y variables de el índice de felicidad

# Variables a analizar
variables_interes <- c("both", "social_support", "healthy_life_expectancy", "freedom_to_make_life_choices", "generosity", "perceptions_of_corruption", "ladder_score")

# Regiones
regiones_unicas <- c(
  "Europe",
  "Sub-Saharan Africa",
  "Asia",
  "Australia and Oceania",
  "North America",
  "South America",
  "Central America and the Caribbean",
  "Middle East, North Africa, and Greater Arabia"
)

# Crear una lista para almacenar las matrices de correlación por región
correlacion_matrices <- list()

# Bucle para calcular las matrices de correlación por región
for (region in regiones_unicas) {
  # Filtrar los datos para la región actual
  datos_region <- base_datos %>% filter(region == region)
  
  # Calcular la matriz de correlación
  correlacion_matrix <- cor(datos_region[variables_interes], method = "spearman")
  
  # Almacenar la matriz de correlación en la lista
  correlacion_matrices[[region]] <- correlacion_matrix
}

# Calcular la matriz de correlación promedio para todas las regiones
correlacion_matrix_general <- matrix(NA, nrow = length(variables_interes), ncol = 1)
colnames(correlacion_matrix_general) <- c("General")

for (i in seq_along(variables_interes)) {
  variable <- variables_interes[i]
  region_correlacion <- sapply(correlacion_matrices, function(mat) mat[1, i])
  correlacion_matrix_general[i, ] <- mean(region_correlacion, na.rm = TRUE)
}

# Crear un data frame con las correlaciones para todas las regiones, incluyendo "General"
correlacion_datos <- data.frame(Variable = variables_interes)
for (region in regiones_unicas) {
  region_correlacion <- correlacion_matrices[[region]][1, ]  # Correlaciones de "both" con todas las variables
  correlacion_datos[, region] <- region_correlacion
}

# Agregar la columna "General" al data frame
correlacion_datos$General <- correlacion_matrix_general[, "General"]

# Eliminar la fila correspondiente a "both"
correlacion_datos <- correlacion_datos[correlacion_datos$Variable != "both", ]

# Mover la fila "General" al principio
correlacion_datos <- correlacion_datos %>%
  arrange(desc(Variable == "General"))

# Mover la fila "ladderscore" al principio
correlacion_datos <- correlacion_datos %>%
  arrange(desc(Variable == "ladder_score"))

# Nuevos nombres de las variables
nuevos_nombres <- c("Indice de felicidad", "Apoyo social", "Esperanza de vida saludable", "Libertad para la toma de decisiones", "Generosidad","Percepciones de corrupción")

# Cambiar los nombres de las variables
correlacion_datos <- correlacion_datos %>%
  mutate(Variable = nuevos_nombres)

colnames(correlacion_datos)[2] <- "Europa"
colnames(correlacion_datos)[3] <- "Africa Sub-sahariana"
colnames(correlacion_datos)[5] <- "Australia y OCeanía"
colnames(correlacion_datos)[6] <- "Norteamérica"
colnames(correlacion_datos)[7] <- "Suramérica"
colnames(correlacion_datos)[8] <- "Centroamérica y el Caribe"
colnames(correlacion_datos)[9] <- "Medio Oriente y N. Africa
"
# Mostrar el data frame actualizado
print(correlacion_datos)

# version 2: Correlación entre Consumo de alcohol y variables de el índice de felicidad según decil 

# Calcular los deciles del consumo de alcohol y asignarlos a una nueva variable
base_datos2 <- base_datos %>%
  arrange(both) %>%
  mutate(decil_alcohol = ntile(both, 10))

# Crear un data frame para almacenar las correlaciones
correlacion_deciles <- data.frame(Variable = variables_interes)

# Calcular las correlaciones para cada decil y cada variable
for (i in 1:10) {
  decil_data <- base_datos2 %>% filter(decil_alcohol == i)
  correlaciones <- correlaciones <- sapply(variables_interes, function(var) cor(decil_data$both, decil_data[[var]], method = "spearman"))
  correlacion_deciles[paste("Decil_", i, sep = "")] <- correlaciones
}

# Cambiar los nombres de las filas
rownames(correlacion_deciles) <- variables_interes


# Eliminar la fila correspondiente a "both"
correlacion_deciles <- correlacion_deciles[correlacion_deciles$Variable != "both", ]

# Mover la fila "ladder_score" al principio
correlacion_deciles <- correlacion_deciles %>%
  arrange(desc(Variable == "ladder_score"))

# Nuevos nombres de las variables
nuevos_nombres <- c("Indice de felicidad", "Apoyo social", "Esperanza de vida saludable", "Libertad para la toma de decisiones", "Generosidad","Percepciones de corrupción")

# Cambiar los nombres de las variables
correlacion_deciles$Variable <- nuevos_nombres

colnames(correlacion_deciles)[2] <- "Decil 1"
colnames(correlacion_deciles)[3] <- "Decil 2"
colnames(correlacion_deciles)[4] <- "Decil 3"
colnames(correlacion_deciles)[5] <- "Decil 4"
colnames(correlacion_deciles)[6] <- "Decil 5"
colnames(correlacion_deciles)[7] <- "Decil 6"
colnames(correlacion_deciles)[8] <- "Decil 7"
colnames(correlacion_deciles)[9] <- "Decil 8"
colnames(correlacion_deciles)[10] <- "Decil 9"
colnames(correlacion_deciles)[11] <- "Decil 10"


# Transponer el data frame
correlacion_deciles_transpuesto <- correlacion_deciles %>%
  pivot_longer(cols = starts_with("Decil"), names_to = "Decil", values_to = "Valor") %>%
  pivot_wider(names_from = Variable, values_from = Valor)

# Cambiar los nombres de las filas
rownames(correlacion_deciles_transpuesto) <- paste("Decil", 1:10, sep = "_")

# Mostrar el data frame transpuesto
print(correlacion_deciles_transpuesto)

# Cuadro 2: índice de felicidad promedio para cada decil 

# Calcular el índice de felicidad promedio, varianza, máximo y mínimo, y obtener los nombres de los países para cada decil
resumen_felicidad_por_decil <- relacion_AlcoholYFelicidad2 %>%
  group_by(decil_alcohol) %>%
  summarize(
    Promedio_Índice_de_felicidad = mean(ladder_score),
    Varianza_Índice_de_felicidad = var(ladder_score),
    Máximo_Índice_de_felicidad = max(ladder_score),
    Mínimo_Índice_de_felicidad = min(ladder_score)  
  )

# Cambiar los nombres de las columnas
resumen_felicidad_por_decil <- resumen_felicidad_por_decil %>%
  rename(
    "Promedio" = Promedio_Índice_de_felicidad,
    "Varianza" = Varianza_Índice_de_felicidad,
    "Máximo" = Máximo_Índice_de_felicidad,
    "Mínimo" = Mínimo_Índice_de_felicidad,
    "Decil" = decil_alcohol
  )

# Mostrar el cuadro de resumen
print(resumen_felicidad_por_decil)

# Calcular el consumo promedio de alcohol para cada decil
consumo_promedio_por_decil <- relacion_AlcoholYFelicidad2 %>%
  group_by(decil_alcohol) %>%
  summarize(Consumo_Promedio_Alcohol = mean(both))

# Cambiar el nombre de la columna
consumo_promedio_por_decil <- consumo_promedio_por_decil %>%
  rename("Consumo Promedio de Alcohol" = Consumo_Promedio_Alcohol)
consumo_promedio_por_decil <- consumo_promedio_por_decil %>%
  rename("Decil" = decil_alcohol)

# Mostrar el cuadro de resumen
print(consumo_promedio_por_decil)

# Crear la tabla LaTeX
tabla_latex1 <- xtable(
  resumen_felicidad_por_decil,
  caption = "Resumen Estadístico del Índice de Felicidad por Decil de Consumo de Alcohol",
  label = "tab:resumen-felicidad-decil"
)

# Configurar opciones para ajustar la tabla a una sola página
#options(xtable.comment = FALSE)
#print(tabla_latex1, file = "tabla_deciles.tex", floating = FALSE, hline.after = c(-1, 0, nrow(promedio_felicidad_por_decil)))

# Confirmar que se ha creado el archivo "tabla_deciles.tex"
#list.files()

# Cuadro 3: 

# Calcular el índice de felicidad promedio, varianza, máximo y mínimo, y obtener los nombres de los países para cada región
resumen_felicidad_por_region <- relacion_AlcoholYFelicidad2 %>%
  group_by(Region) %>%
  summarize(
    Promedio_Consumo_Alcohol = mean(both),
    Promedio_Índice_de_felicidad = mean(ladder_score),
    Varianza_Índice_de_felicidad = var(ladder_score),
    Máximo_Índice_de_felicidad = max(ladder_score),
    Mínimo_Índice_de_felicidad = min(ladder_score)
  )

# Cambiar el orden de las columnas
resumen_felicidad_por_region <- resumen_felicidad_por_region %>%
  select("Region",
    "Promedio_Consumo_Alcohol",
    "Promedio_Índice_de_felicidad",
    "Varianza_Índice_de_felicidad",
    "Máximo_Índice_de_felicidad",
    "Mínimo_Índice_de_felicidad"
  )

# Renombrar las columnas
colnames(resumen_felicidad_por_region) <- c("Región",
  "Promedio Consumo Alcohol",
  "Promedio Índice de felicidad",
  "Varianza Índice de felicidad",
  "Máximo Índice de felicidad",
  "Mínimo Índice de felicidad"
  
)

# Mostrar el cuadro de resumen
print(resumen_felicidad_por_region)

# Crear la tabla LaTeX
tabla_latex2 <- xtable(
  resumen_felicidad_por_region,
  caption = "Resumen Estadístico del Índice de Felicidad por Región",
  label = "tab:resumen-felicidad-region"
)

# Configurar opciones para ajustar la tabla a una sola página
options(xtable.comment = FALSE)
print(tabla_latex2, file = "tabla_region.tex", floating = FALSE, hline.after = c(-1, 0, nrow(resumen_felicidad_por_region)))

# Confirmar que se ha creado el archivo "tabla_deciles.tex"
#list.files()


# copia de cuadro de Bryan para editarlo y ponerlo en latex



Nombres <- c("NA", "Both", "Male", "Female", "Ladder Score", "Standard Error of Ladder Score",
             "Upperwhisker", "Lowerwhisker", "Logged GDP per Capita", "Social Support",
             "Healthy Life Expectancy", "Freedom to Make Life Choices", "Generosity",
             "Perception of Corruption", "Ladder Score in Dystopia",
             "Explained by Logged GDP per Capita", "Explained by Social Support",
             "Explained by Healthy Life Expectancy", "Explained by Freedom to Make Life Choices", 
             "Explained by Generosity", "Explained by Perception of Corruption","Dystopia Residual" )




Minimo <- 0
Maximo <- 0
Rango <- 0
Media <- 0
Mediana <- 0
PrimerCuartil <- 0
TercerCuartil <- 0
RangoIntercuartil <- 0
DesviacionStandar <- 0
Varianza <- 0
CoeficienteVariacion <- 0


for(i in 2:22){
  Minimo <- append(Minimo, min(base_datos[,i])) 
  Maximo <- append(Maximo, max(base_datos[,i])) 
  Rango <- append(Rango, max(base_datos[,i]) - min(base_datos[,i]))
  Media <- append(Media, mean(base_datos[,i]))
  Mediana <- append(Mediana, median(base_datos[,i]))
  PrimerCuartil <- append(PrimerCuartil, quantile(base_datos[,i], 0.25))
  TercerCuartil <- append(TercerCuartil, quantile(base_datos[,i], 0.75))
  RangoIntercuartil <- append(RangoIntercuartil, IQR(base_datos[,i]))
  DesviacionStandar <- append(DesviacionStandar, sd(base_datos[,i]))
  Varianza <- append(Varianza, var(base_datos[,i]))
  CoeficienteVariacion <- append(CoeficienteVariacion, sd(base_datos[,i])/
                                   mean(base_datos[,i]))
  
}


AnalisisD <- data.frame(Nombres, Minimo, Maximo, Rango, Media, PrimerCuartil,
                        Mediana, TercerCuartil, RangoIntercuartil, DesviacionStandar,
                        Varianza, CoeficienteVariacion)

# Crear la tabla LaTeX
tabla_latex4 <- xtable(
  AnalisisD,
  caption = "Análisis descriptivo completo",
  label = "tab:analisisDescriptivo"
)

# Configurar opciones para ajustar la tabla a una sola página
options(xtable.comment = FALSE)
print(tabla_latex4, file = "analisis_descriptivo.tex", floating = FALSE, hline.after = c(-1, 0, nrow(AnalisisD)))

# Confirmar que se ha creado el archivo "tabla_deciles.tex"
#list.files()


#---Notas y pendientes----------------------------------------------------------
# 
#

