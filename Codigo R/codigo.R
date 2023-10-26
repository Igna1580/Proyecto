
# Relación entre el índice de felicidad y el consumo de alcohol promedio para 131 paises
# Por: Jose Ignacio Rojas, Bryan Campos, Valeria Vásquez, Montserrat Beirute

library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(cowplot)
library(nortest)
library(xtable)
library(moments)


#---Paquetes--------------------------------------------------------------
library(tidyverse) # manipulación, visualización y análisis de datos
library(janitor) # limpiar y organizar datos
library(cowplot) # facilita creación de gráficos 
library(nortest) # realiza pruebas de normalidad estadística
library(xtable) #  crear tablas con formato LaTeX o HTML
library(e1071) # proporciona una amplia variedad de funciones y herramientas
library(boot) # facilita la herramienta de bootstrap
#---CSV y formatos--------------------------------------------------------

# Abrir y dar formato a las bases de datos

# Base de datos de alcohol
consumo_alcohol <- read.csv("alcohol-consumption-by-country-2023.csv")
consumo_alcohol <- clean_names(consumo_alcohol)

# cambiar títulos a español
consumo_alcohol <- consumo_alcohol %>% rename(pais = country)
consumo_alcohol <- consumo_alcohol %>% rename(poblacion_Total_consumo = both)
consumo_alcohol <- consumo_alcohol %>% rename(hombres_consumo =  male)
consumo_alcohol <- consumo_alcohol %>% rename(mujeres_consumo =  female)


# Base de datos felicidad
felicidad <- read.csv("WHR2023.csv")
felicidad <- clean_names(felicidad)

# cambiar titulos a español
felicidad <- felicidad %>% rename(pais = country_name)
felicidad <- felicidad %>% rename(indice_de_felicidad = ladder_score)
felicidad <- felicidad %>% rename(error_estandar_indice_de_felicidad = standard_error_of_ladder_score)
felicidad <- felicidad %>% rename(bigote_superior = upperwhisker)
felicidad <- felicidad %>% rename(bigote_inferior = lowerwhisker)
felicidad <- felicidad %>% rename(log_pib_per_capita = logged_gdp_per_capita)
felicidad <- felicidad %>% rename(apoyo_social = social_support)
felicidad <- felicidad %>% rename(esperanza_de_vida_saludable = healthy_life_expectancy)
felicidad <- felicidad %>% rename(libertad_para_toma_de_decisiones = freedom_to_make_life_choices)
felicidad <- felicidad %>% rename(generosidad = generosity)
felicidad <- felicidad %>% rename(percepcion_de_corrupcion = perceptions_of_corruption)


# Nombres de paises chequeo 

# Cambiar "Turkiye" a "Turkey" en el data frame "felicidad"
felicidad <- felicidad %>%
  mutate(pais = ifelse(pais == "Turkiye", "Turkey", pais))

# Cambiar "DR Congo" to "Congo (Brazzaville)" en el data frame 'consumo_alcohol'
consumo_alcohol <- consumo_alcohol %>%
  mutate(pais = ifelse(pais == "DR Congo", "Congo (Brazzaville)", pais))

# Nota: los siguientes paises sólo tenemos la información de felicidad pero no del consumo de alcohol: Czechia, Taiwan Province of China, Kosovo, Hong Kong S.A.R. of China, State of Palestine, Congo (Kinshasa). Se decide eliminar estos casos. 


# Procedemos a unir las dos bases de datos 
columnas_felicidad <- c("indice_de_felicidad", "error_estandar_indice_de_felicidad", "bigote_superior", "bigote_inferior", "log_pib_per_capita", "apoyo_social", "esperanza_de_vida_saludable", "libertad_para_toma_de_decisiones", "generosidad", "percepcion_de_corrupcion")
base_datos <- inner_join(consumo_alcohol, select(felicidad, pais, all_of(columnas_felicidad)), by = "pais")

# Crear una nueva columna con la region correspondiente a cada país, la llamamos "Region"
base_datos <- base_datos %>%
  mutate(Region = ifelse(
    pais %in% c(
      "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "India", 
      "Indonesia", "Japan", "Kazakhstan", "North Korea", "South Korea", 
      "Kyrgyzstan", "Laos", "Malaysia", "Maldives", "Mongolia", "Myanmar", 
      "Nepal", "Philippines", "Singapore", "Sri Lanka", "Taiwan", "Tajikistan", 
      "Thailand", "Turkmenistan", "Uzbekistan", "Vietnam"
    ), "Asia",
    ifelse(
      pais %in% c(
        "Afghanistan", "Algeria", "Bahrain", "Egypt", "Iran", "Iraq", 
        "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", 
        "Oman", "Pakistan", "Qatar", "Saudi Arabia", "Somalia", "Syria", 
        "Tunisia", "Turkey", "United Arab Emirates", "Yemen"
      ), "Medio Oriente, Norte de Africa y Gran Arabia",
      ifelse(
        pais %in% c(
          "Albania", "Andorra", "Armenia", "Austria", "Belarus", "Belgium", 
          "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
          "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", 
          "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", 
          "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "North Macedonia", 
          "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", 
          "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", 
          "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", 
          "United Kingdom", "Vatican City"
        ), "Europa",
        ifelse(
          pais %in% c("Canada", "Greenland", "Mexico", "United States"), "Norteamerica",
          ifelse(
            pais %in% c(
              "Antigua and Barbuda", "The Bahamas", "Barbados", "Belize", "Costa Rica", 
              "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", 
              "Guatemala", "Haiti", "Honduras", "Jamaica", "Nicaragua", "Panama", 
              "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
              "Trinidad and Tobago"
            ), "Centroamerica y el Caribe",
            ifelse(
              pais %in% c(
                "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"
              ), "Suramerica",
              ifelse(
                pais %in% c(
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
                ), "Africa Sub-sahariana",
                ifelse(
                  pais %in% c(
                    "Australia", "East Timor", "Fiji", "Kiribati", "Marshall Islands", 
                    "The Federated States of Micronesia", "Nauru", "New Zealand", "Palau", 
                    "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", 
                    "Vanuatu"
                  ), "Australia y Oceania",
                  "Otro"
                )
              )
            )
          )
        )
      )
    )
  ))

# Cambiamos los nombres de los paises a español 
base_datos$pais <- recode(base_datos$pais,
                          "Latvia" = "Letonia",
                          "Moldova" = "Moldavia",
                          "Germany" = "Alemania",
                          "Lithuania" = "Lituania",
                          "Ireland" = "Irlanda",
                          "Spain" = "España",
                          "Uganda" = "Uganda",
                          "Bulgaria" = "Bulgaria",
                          "Luxembourg" = "Luxemburgo",
                          "Romania" = "Rumania",
                          "Montenegro" = "Montenegro",
                          "France" = "Francia",
                          "Slovenia" = "Eslovenia",
                          "Portugal" = "Portugal",
                          "Laos" = "Laos",
                          "Tanzania" = "Tanzania",
                          "Austria" = "Austria",
                          "Poland" = "Polonia",
                          "United Kingdom" = "Reino Unido",
                          "Switzerland" = "Suiza",
                          "Hungary" = "Hungría",
                          "Slovakia" = "Eslovaquia",
                          "Burkina Faso" = "Burkina Faso",
                          "Belgium" = "Bélgica",
                          "Cyprus" = "Chipre",
                          "Estonia" = "Estonia",
                          "New Zealand" = "Nueva Zelanda",
                          "Finland" = "Finlandia",
                          "Russia" = "Rusia",
                          "Greece" = "Grecia",
                          "Australia" = "Australia",
                          "Denmark" = "Dinamarca",
                          "Japan" = "Japón",
                          "United States" = "Estados Unidos",
                          "Netherlands" = "Países Bajos",
                          "Georgia" = "Georgia",
                          "South Africa" = "Sudáfrica",
                          "Argentina" = "Argentina",
                          "Iceland" = "Islandia",
                          "Sweden" = "Suecia",
                          "Chile" = "Chile",
                          "Serbia" = "Serbia",
                          "Canada" = "Canadá",
                          "Croatia" = "Croacia",
                          "Thailand" = "Tailandia",
                          "South Korea" = "Corea del Sur",
                          "Ukraine" = "Ucrania",
                          "Malta" = "Malta",
                          "Gabon" = "Gabón",
                          "Italy" = "Italia",
                          "Vietnam" = "Vietnam",
                          "Cambodia" = "Camboya",
                          "Bosnia and Herzegovina" = "Bosnia y Herzegovina",
                          "Panama" = "Panamá",
                          "Brazil" = "Brasil",
                          "Norway" = "Noruega",
                          "Philippines" = "Filipinas",
                          "Paraguay" = "Paraguay",
                          "Uruguay" = "Uruguay",
                          "Albania" = "Albania",
                          "Peru" = "Perú",
                          "Dominican Republic" = "República Dominicana",
                          "Botswana" = "Botsuana",
                          "North Macedonia" = "Macedonia del Norte",
                          "Nigeria" = "Nigeria",
                          "China" = "China",
                          "Mongolia" = "Mongolia",
                          "India" = "India",
                          "Cameroon" = "Camerún",
                          "Colombia" = "Colombia",
                          "Liberia" = "Liberia",
                          "Sierra Leone" = "Sierra Leona",
                          "Nicaragua" = "Nicaragua",
                          "Mexico" = "México",
                          "Kazakhstan" = "Kazajistán",
                          "Kyrgyzstan" = "Kirguistán",
                          "Mauritius" = "Mauricio",
                          "Armenia" = "Armenia",
                          "Zimbabwe" = "Zimbabue",
                          "Zambia" = "Zambia",
                          "Israel" = "Israel",
                          "Jamaica" = "Jamaica",
                          "El Salvador" = "El Salvador",
                          "Malawi" = "Malaui",
                          "Costa Rica" = "Costa Rica",
                          "Honduras" = "Honduras",
                          "Bolivia" = "Bolivia",
                          "United Arab Emirates" = "Emiratos Árabes Unidos",
                          "Venezuela" = "Venezuela",
                          "Gambia" = "Gambia",
                          "Ecuador" = "Ecuador",
                          "Namibia" = "Namibia",
                          "Ivory Coast" = "Costa de Marfil",
                          "Sri Lanka" = "Sri Lanka",
                          "Ghana" = "Ghana",
                          "Mozambique" = "Mozambique",
                          "Togo" = "Togo",
                          "Uzbekistan" = "Uzbekistán",
                          "Benin" = "Benín",
                          "Ethiopia" = "Etiopía",
                          "Kenya" = "Kenia",
                          "Myanmar" = "Myanmar",
                          "Tunisia" = "Túnez",
                          "Singapore" = "Singapur",
                          "Madagascar" = "Madagascar",
                          "Turkey" = "Turquía",
                          "Guatemala" = "Guatemala",
                          "Lebanon" = "Líbano",
                          "Mali" = "Malí",
                          "Chad" = "Chad",
                          "Comoros" = "Comoras",
                          "Bahrain" = "Baréin",
                          "Congo (Brazzaville)" = "Congo (Brazzaville)",
                          "Guinea" = "Guinea",
                          "Iran" = "Irán",
                          "Malaysia" = "Malasia",
                          "Tajikistan" = "Tayikistán",
                          "Senegal" = "Senegal",
                          "Algeria" = "Argelia",
                          "Nepal" = "Nepal",
                          "Niger" = "Níger",
                          "Jordan" = "Jordania",
                          "Morocco" = "Marruecos",
                          "Iraq" = "Irak",
                          "Pakistan" = "Pakistán",
                          "Indonesia" = "Indonesia",
                          "Egypt" = "Egipto",
                          "Afghanistan" = "Afganistán",
                          "Bangladesh" = "Bangladesh",
                          "Saudi Arabia" = "Arabia Saudita",
                          "Mauritania" = "Mauritania"
)

#---Bloque Valeria--------------------------------------------------------------

# Gráfico distribución de datos de felicidad
distribucion_felicidad <- ggplot(base_datos, aes(x=indice_de_felicidad)) +
  geom_histogram(binwidth = 0.5, col='black', fill= 'pink')+
  labs(x= "Indice_de_Felicidad", y="Frecuencia")  +
  cowplot::theme_cowplot()

# distribucion_felicidad

# Gráfico densidad de datos de felicidad
density_plot_f <- ggplot(data.frame(x = base_datos$indice_de_felicidad), aes(x = x)) +
  geom_density(fill = "pink", alpha = 0.5) +
  labs(x = "Valor", y = "Densidad") +
  theme_minimal()  +
  cowplot::theme_cowplot()

# density_plot_f

# Grafico de distribución datos de alcohol 
distribucion_alcohol <- ggplot(base_datos, aes(x=poblacion_Total_consumo)) +
  geom_histogram(binwidth = 0.7, col='black', fill= 'purple')+
  labs(x= "Consumo de alcohol", y="Frecuencia")  +
  cowplot::theme_cowplot()

# distribucion_alcohol

# Grafico de densidad de datos de alcohol
density_plot_a <- ggplot(data.frame(x = base_datos$poblacion_Total_consumo), aes(x = x)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(x = "Valor", y = "Densidad") +
  theme_minimal()  +
  cowplot::theme_cowplot()

# density_plot_a

# Verificación con colas
# plot(density(base_datos$poblacion_Total_consumo))
#  plot(density(base_datos$indice_de_felicidad))


# Pruebas de hipotesis 

# Prueba de Kolmogorov-Smirnov 

# Para datos de felicidad 

# Hipotesis nula (H0): Los datos de felicidad siguen una distribución normal
# Hipotesis alternativa (H1): Los datos de felicidad NO siguen una distribución normal

# ks_test_felicidad <- ks.test(
# base_datos$indice_de_felicidad,  # Los datos que se están probando
# "pnorm",                        # La distribución teórica con la cual estamos comparando que en este caso, una distribución normal estándar
# mean = mean(base_datos$indice_de_felicidad),  # La media  de los datos de felicidad
# sd = sd(base_datos$indice_de_felicidad))       # La desviación estándar de los datos de felicidad

# p-value = 0.6895
# D = 0.062294
# alternative hypothesis: two-sided
# No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Kolmogorov-Smirnov no sugieren que los datos de felicidad sean significativamente diferentes de una distribución normal. 

# Para datos de alcohol 

# Hipotesis nula (H0): Los datos de alcohol siguen una distribución normal
# Hipotesis alternativa (H1): Los datos de alcohol NO siguen una distribución normal

# ks_test_alcohol <- ks.test(base_datos$poblacion_Total_consumo, "pnorm", mean = mean(base_datos$poblacion_Total_consumo), sd = sd(base_datos$poblacion_Total_consumo))

# p-valor = 0.2883
# D = 0.2883
# alternative hypothesis: two-sided
# No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Kolmogorov-Smirnov no sugieren que los datos de alcohol sean significativamente diferentes de una distribución normal. 


# Prueba de Anderson-Darling

# Para datos de felicidad

# Hipotesis nula (H0): Los datos de felicidad siguen una distribución normal
# Hipotesis alternativa (H1): Los datos de felicidad NO siguen una distribución normal

ad_test_felicidad <- ad.test(base_datos$indice_de_felicidad)

# p-value = 0.224
# A = 0.48462
# p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Anderson Darling no sugieren que los datos de felicidad sean significativamente diferentes de una distribución normal. 


# Para datos de alcohol

# Hipotesis nula (H0): Los datos de alcohol siguen una distribución normal
# Hipotesis alternativa (H1): Los datos de alcohol NO siguen una distribución normal

ad_test_alcohol <- ad.test(base_datos$poblacion_Total_consumo)

# p-value = 7.595e-06
# A = 2.2953
# p < 0.05 => Se puede rechazar la hipotesis nula es decir, los resultados de la prueba de Anderson-Darling sugieren que los datos de alcohol son significativamente diferentes de una distribución normal. 

# Prueba de Lilliefors

# Para datos de felicidad 

# Hipotesis nula (H0): Los datos de felicidad siguen una distribución normal
# Hipotesis alternativa (H1): Los datos de felicidad NO siguen una distribución normal

lil_test_felicidad <- lillie.test(base_datos$indice_de_felicidad)

# p-value = 0.2437
# D = 0.062294
# p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Lilliefors no sugieren que los datos de felicidad sean significativamente diferentes de una distribución normal. 

# Para datos de alcohol 

# Hipotesis nula (H0): Los datos de alcohol siguen una distribución normal
# Hipotesis alternativa (H1): Los datos de alcohol NO siguen una distribución normal

lil_test_alcohol <- lillie.test(base_datos$poblacion_Total_consumo)

# p-value = 0.01903
# D = 0.08592
# p < 0.05 => Se puede rechazar la hipotesis nula es decir, los resultados de la prueba de Lilliefors sugieren que los datos de alcohol son significativamente diferentes de una distribución normal. 

# Por lo tanto, según las tres pruebas, los datos de índice de felicidad no sugieren que los datos de felicidad sean significativamente diferentes de una distribución normal. 
# Por su parte, segpun las pruebas de lilliefors y Anderson Darling, los datos de consumo de alcohol son significativamente diferentes de una distribución normal. 
# Como los datos de consumo de alcohol no son normales, entonces para la correlación, debemos usar la correlación de spearman

#---Bloque Jose Ignacio---------------------------------------------------------


# Datos de Felicidad

# Buscando el parametro de máxima verosimilitud respecto a felicidad
n = length(base_datos$indice_de_felicidad)
suma.felicidad = sum(base_datos$indice_de_felicidad)
mu = suma.felicidad/n
sigma.cuadrado = (sum((base_datos$indice_de_felicidad-mu)^2))/n
theta = seq(1.778, 7.804, length.out = 1000)

# Graficos de Máxima Verosimilitud

grafico_Maxima_Verosimilitud <- ggplot(data.frame(
  theta = theta,
  L = (1/sqrt(2*pi*sigma.cuadrado))*exp((-(theta-mu)^2)/(2*sigma.cuadrado))), 
  aes(x = theta , y = L)) +
  geom_line(linewidth = 2) +
  geom_vline(aes(xintercept = mean(base_datos$indice_de_felicidad)), linewidth = 2, color = "red") +
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud con la media de Theta", x = "Theta", y =
      "Verosimilitud")

Grafico_max_ver_felicidad <- ggplot(data = base_datos, aes(x = indice_de_felicidad)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(base_datos$indice_de_felicidad), sd = sd(base_datos$indice_de_felicidad)), size = 1) +
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud respecto al Índice de felicidad", x = "Índice de felicidad", y =
      "Densidad")

Grafico_max_ver_felicidad

# ggsave(filename = "Grafico_max_ver_felicidad.pdf", plot = Grafico_max_ver_felicidad, device = "pdf", width = 10, height = 8)

# Datos de consumo de alcohol 

#Grafico de maxima verosimilitud respecto a consumo de alcohol

Grafico_max_ver_alcohol <- ggplot(data = base_datos, aes(x = poblacion_Total_consumo)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.3, fill = "blue", color = "black", alpha = 0.5) +
  stat_function(fun = dunif, args = list(min = 0, max = 14), size = 1) +  # Distribución uniforme (0,14)
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud respecto al consumo de alcohol", x = "Consumo de alcohol", y =
      "Densidad")

Grafico_max_ver_alcohol

# ggsave(filename = "Grafico_max_ver_alcohol.pdf", plot = Grafico_max_ver_alcohol, device = "pdf", width = 10, height = 8)


# Coeficiente de asimetría de Fisher

coef.asim.Fish <- skewness(base_datos$indice_de_felicidad) 

# coeficiente de asimetría de Fisher: -0.4270003
# No es cercano a 1, por lo que no es simétrica

# Coeficiente de aplastamiento o curtosis de Fisher

curtosis.Fish <- kurtosis(base_datos$indice_de_felicidad) 

# curtosis de Fisher: 0.009130095
# Es casi igual de aplastada que una normal

#Grefico de maxima verosimilitud respecto a felicidad
Grafico_max_ver_felicidad <- ggplot(data = base_datos, aes(x = ladder_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(base_datos$ladder_score), sd = sd(base_datos$ladder_score)), size = 1) +
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud respecto al \níndice de felicidad", x = "Índice de felicidad", y =
      "Densidad"
  )
Grafico_max_ver_felicidad


#Grafico de maxima verosimilitud respecto a consumo de alcohol
Grafico_max_ver_alcohol <- ggplot(data = base_datos, aes(x = both)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.5) +
  geom_density(size = 1) +
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud respecto al \nconsumo de alcohol", x = "Consumo de alcohol", y =
      "Densidad"
  )
Grafico_max_ver_alcohol

#Coeficiente de asimetría de Fisher
coef.asim.Fish <- skewness(base_datos$ladder_score) #No es cercano a 1, no es simetrica

#Coeficiente de aplastamiento o curtosis de Fisher}
curtosis.Fish <- kurtosis(base_datos$ladder_score) #Es casi igual de aplastada que una normal



#---Bloque Bryan----------------------------------------------------------------

# Cuadro de Resumen Estadístico completo 

# Numero de columnas

num_columnas <- ncol(base_datos)

# Nombres de las variables de interés

Nombres <- colnames(base_datos)[2:(num_columnas - 1)]  # Excluye la última columna

# Estadísticos a considerar 

Minimo <- Maximo <- Rango <- Media <- Mediana <- PrimerCuartil <- TercerCuartil <- RangoIntercuartil <- DesviacionEstandar <- Varianza <- CoeficienteVariacion <- vector("numeric", length(Nombres))


# Llenar cada uno de los vectores

for(i in 2:(num_columnas - 1)){  # Excluye la última columna
  Minimo <- append(Minimo, min(base_datos[, i])) 
  Maximo <- append(Maximo, max(base_datos[, i])) 
  Rango <- append(Rango, max(base_datos[, i]) - min(base_datos[, i]))
  Media <- append(Media, mean(base_datos[, i]))
  Mediana <- append(Mediana, median(base_datos[, i]))
  PrimerCuartil <- append(PrimerCuartil, quantile(base_datos[, i], 0.25))
  TercerCuartil <- append(TercerCuartil, quantile(base_datos[, i], 0.75))
  RangoIntercuartil <- append(RangoIntercuartil, IQR(base_datos[, i]))
  DesviacionEstandar <- append(DesviacionEstandar, sd(base_datos[, i]))
  Varianza <- append(Varianza, var(base_datos[, i]))
  CoeficienteVariacion <- append(CoeficienteVariacion, sd(base_datos[, i]) /
                                   mean(base_datos[, i]))}

# Juntamos los vectores en un unico dataframe

AnalisisD <- data.frame(Nombres, Minimo, Maximo, Rango, Media, PrimerCuartil,
                        Mediana, TercerCuartil, RangoIntercuartil, DesviacionEstandar,
                        Varianza, CoeficienteVariacion)

# Redondear las columnas 

columnas_a_redondear <- c("Minimo", "Maximo", "Rango", "Media", "PrimerCuartil",
                          "Mediana", "TercerCuartil", "RangoIntercuartil", "DesviacionEstandar",
                          "Varianza", "CoeficienteVariacion")

AnalisisD[columnas_a_redondear] <- round(AnalisisD[columnas_a_redondear], 2)

# Eliminar filas

AnalisisD <- AnalisisD[-c(1:13), ]


#---Bloque Montse---------------------------------------------------------------

# Gráfico 1: Consumo de alcohol por país, codificado con escala de color según Índice de felicidad

# Para aumentar el tamaño del gráfico 

options(repr.plot.width = 4, repr.plot.height = 15)

# Crear nuevo data frame ordenado según consumo de alcohol de menor a mayor

base_datos2 <- base_datos %>% arrange(poblacion_Total_consumo)

# Crear un gráfico de barras horizontales según consumo de alcohol y codificado por color según índice de felicidad  

gráfico_Barras1 <- ggplot(base_datos2, aes(x = poblacion_Total_consumo, y = reorder(pais, poblacion_Total_consumo), fill = indice_de_felicidad)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Promedio anual de consumo de alcohol puro per cápita en litros",
    y = "País",
    title = "Consumo de alcohol por país ordenado de mayor a menor",
    subtitle = "Codificado con escala de color según Índice de felicidad",
    fill = "Índice de felicidad") + 
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

gráfico_Barras1

# Guarda el gráfico en un archivo PDF
# ggsave("grafico_barras.pdf", plot = gráfico_Barras1, width = 8.27, height = 11.69, dpi = 500)

# Gráfico 2: Promedio de Consumo de Alcohol por Región, ordenado por Índice de Felicidad Promedio

# Calcular los promedios por región

promedios_region <- base_datos %>%
  group_by(Region) %>%summarise(Promedio_Alcohol = mean(poblacion_Total_consumo),
                                Promedio_Felicidad = mean(indice_de_felicidad)) %>%
  arrange(desc(Promedio_Felicidad))  # Ordenar por índice de felicidad promedio de mayor a menor

# Crear el gráfico de barras

grafico_promedios <- ggplot(promedios_region, aes(x = reorder(Region, -Promedio_Felicidad), y = Promedio_Alcohol, fill = Promedio_Felicidad)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Región",
    y = "Promedio de consumo de alcohol",
    title = "Promedio de consumo de alcohol por región",
    subtitle = "Ordenado por Índice de felicidad promedio") +
  theme_minimal() + cowplot::theme_cowplot() +
  theme( plot.title = element_text(size = 20),  # Tamaño del título
         plot.subtitle = element_text(size = 18),  # Tamaño del subtítulo
         axis.title.x = element_text(size = 18),  # Tamaño del título del eje x
         axis.title.y = element_text(size = 18),  # Tamaño del título del eje y
         axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  # Texto del eje x inclinado
         axis.text.y = element_text(size = 18),  # Tamaño del texto del eje y
         plot.margin = margin(20, 20, 20, 20)) +  # Márgenes del gráfico 
  scale_fill_gradient(low = "red", high = "green", name = "Índice de felicidad") +
  geom_text(aes(label = round(Promedio_Felicidad, 2)), vjust = -0.5)  # Ajustar el tamaño del texto a 18



# Mismo gráfico 2 pero para el anteproyecto

# Modificar los nombres de las regiones
promedios_region$Region <- case_when(
  promedios_region$Region == "Europa" ~ "E.",
  promedios_region$Region == "Africa Sub-sahariana" ~ "Á.S.",
  promedios_region$Region == "Asia" ~ "A.",
  promedios_region$Region == "Australia y Oceania" ~ "OCE",
  promedios_region$Region == "Norteamerica" ~ "N. Á.",
  promedios_region$Region == "Suramerica" ~ "S. Á",
  promedios_region$Region == "Medio Oriente, Norte de Africa y Gran Arabia" ~ "M.O./N.Á.",
  promedios_region$Region == "Centroamerica y el Caribe" ~ "C.C.",
  TRUE ~ promedios_region$Region)  # Mantener otros nombres sin cambios

grafico_promedios2 <- ggplot(promedios_region, aes(x = reorder(Region, -Promedio_Felicidad), y = Promedio_Alcohol, fill = Promedio_Felicidad)) +
  geom_bar(stat = "identity") +
  labs(x = "Región",
       y = "Promedio de consumo de alcohol") +
  theme_minimal() +
  cowplot::theme_cowplot() +
  theme(
    plot.margin = margin(20, 20, 20, 20),  # Márgenes del gráfico
    axis.text.x = element_text(angle = 45, hjust = 1)) +  # Ángulo de 45 grados y alineación a la derecha
  scale_fill_gradient(low = "red", high = "green", name = "Índice de felicidad") +
  geom_text(aes(label = round(Promedio_Felicidad, 2)), vjust = -0.5) +  # Ajustar el tamaño del texto a 18
  coord_cartesian(clip = 'off') +  # Evitar que las barras superiores oculten la leyenda
  theme(legend.position = c(0.65, 0.85))  # Posicionar la leyenda en la parte superior y centrada

grafico_promedios2

# ggsave(filename = "grafico_barras2.pdf", plot = grafico_promedios, device = "pdf", width = 10, height = 8)

# Gráfico 3: Gráfico de dispersión con línea de regresión

grafico_dispersion <- ggplot(base_datos, aes(x = poblacion_Total_consumo, y = indice_de_felicidad)) +
  geom_point() + cowplot::theme_cowplot() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Línea de regresión
  labs(
    x = "Consumo de alcohol en litros de alcohol puro",
    y = "Índice de felicidad",
    title = "Gráfico de dispersión con línea de regresión",
    subtitle = "Relación Índice de felicidad y consumo de alcohol") +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18),# Tamaño del título
    axis.title.x = element_text(size = 18),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 18),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 18),  # Tamaño del texto del eje x
    axis.text.y = element_text(size = 18),  # Tamaño del texto del eje y
    plot.margin = margin(20, 20, 20, 20))  # Márgenes del gráfico

# ggsave(filename = "grafico_dispersion.pdf", plot = grafico_dispersion, device = "pdf", width = 4, height = 3)

# Mismo gráfico 3 pero para el anteproyecto

grafico_dispersion2 <- ggplot(base_datos, aes(x = poblacion_Total_consumo, y = indice_de_felicidad)) +
  geom_point() + cowplot::theme_cowplot() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Línea de regresión
  labs(
    x = "Consumo de alcohol puro en litros",
    y = "Índice de felicidad") +
  theme(plot.margin = margin(20, 20, 20, 20))  # Márgenes del gráfico

grafico_dispersion2

# ggsave(filename = "grafico_dispersion2.pdf", plot = grafico_dispersion2, device = "pdf", width = 4, height = 3)

# Gráfico 4: Índice de Felicidad Promedio por Decil de Consumo de Alcohol

# Calcular los deciles del consumo de alcohol y asignarlos a una nueva variable

base_datos2 <- base_datos2 %>% mutate(decil_alcohol = ntile(poblacion_Total_consumo, 10))

# Calcular el promedio, máximo y mínimo del índice de felicidad para cada decil

promedio_felicidad_por_decil <- base_datos2 %>% group_by(decil_alcohol) %>% summarize(promedio_felicidad = mean(indice_de_felicidad))

max_felicidad_por_decil <- base_datos2 %>% group_by(decil_alcohol) %>% summarize(max_felicidad = max(indice_de_felicidad))

min_felicidad_por_decil <- base_datos2 %>% group_by(decil_alcohol) %>% summarize(min_felicidad = min(indice_de_felicidad))


# Crear el gráfico de línea

gráfico_Línea <- ggplot(promedio_felicidad_por_decil, aes(x = decil_alcohol, y = promedio_felicidad)) +
  geom_line() +
  geom_point(data = max_felicidad_por_decil, aes(x = decil_alcohol, y = max_felicidad), color = "green", size = 3, shape = 24) +
  geom_point(data = min_felicidad_por_decil, aes(x = decil_alcohol, y = min_felicidad), color = "red", size = 3, shape = 25) +
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
  geom_vline(xintercept = 1:10, linetype = "dashed", color = "gray", linewidth = 0.5) +
  geom_hline(yintercept = seq(4, 8, 1), linetype = "dashed", color = "gray", linewidth = 0.5)

# Ajustar el tamaño del gráfico (ancho y alto)
# ggsave(filename = "grafico_linea.pdf", plot = gráfico_Línea, device = "pdf", width = 10, height = 8)

# Mismo gráfico 4 pero para el anteproyecto 

gráfico_Línea2 <- ggplot(promedio_felicidad_por_decil, aes(x = decil_alcohol, y = promedio_felicidad)) +
  geom_line() +
  geom_point(data = max_felicidad_por_decil, aes(x = decil_alcohol, y = max_felicidad), color = "green", size = 3, shape = 24) +
  geom_point(data = min_felicidad_por_decil, aes(x = decil_alcohol, y = min_felicidad), color = "red", size = 3, shape = 25) +
  labs(
    x = "Decil",
    y = "Índice de felicidad") +
  theme_minimal() +
  cowplot::theme_cowplot() +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = 0:8) +
  theme(plot.margin = margin(20, 20, 20, 20)) +
  geom_vline(xintercept = 1:10, linetype = "dashed", color = "gray", linewidth = 0.5) +
  geom_hline(yintercept = seq(4, 8, 1), linetype = "dashed", color = "gray", linewidth= 0.5)

# Cuadro 1: Matriz de correlación entre Consumo de alcohol y variables de el índice de felicidad

# Variables a analizar

variables_interes <- c("poblacion_Total_consumo", "indice_de_felicidad", "apoyo_social", "esperanza_de_vida_saludable", "libertad_para_toma_de_decisiones", "generosidad", "percepcion_de_corrupcion")


# Calcular la matriz de correlación de Spearman

correlacion_spearman <- round(cor(base_datos[variables_interes], method = "spearman"), 2)

print(correlacion_spearman)

# Guardar la correlación de spearman entre el consumo de alcohol y todas las variables del indice de felicidad

correlacion_spearman_consumoYvariables <- data.frame(correlacion_spearman = correlacion_spearman[, 1])


# Cuadro 2: Correlación entre Consumo de alcohol y variables de el índice de felicidad para cada región

# Regiones de interés

regiones_unicas <- c("Europa", "Africa Sub-sahariana", "Asia", "Australia y Oceania", "Norteamerica", "Suramerica", "Centroamerica y el Caribe", "Medio Oriente, Norte de Africa y Gran Arabia")

# Calcular la correlación para cada región

correlaciones_Regiones <- data.frame(Variable = variables_interes)

for (region in regiones_unicas) {
  subset_data <- base_datos[base_datos$Region == region, variables_interes]
  correlaciones <- sapply(variables_interes, function(variable) cor(subset_data$poblacion_Total_consumo, subset_data[[variable]], method = "spearman"))
  correlaciones_Regiones[region] <- correlaciones}

# Convertir las correlaciones a tipo numérico

correlaciones_Regiones[, -1] <- lapply(correlaciones_Regiones[, -1], as.numeric)

# Redondear los datos en correlaciones_df a dos decimales

correlaciones_Regiones[, -1] <- round(correlaciones_Regiones[, -1], 2)

# Cuadro 3: Correlación entre consumo de alcohol y variables de índice de felicidad para cada decil 

# Calcular las correlaciones para cada decil

correlaciones_Deciles <- data.frame(Variable = variables_interes)

deciles <- unique(base_datos2$decil_alcohol)

for (decil in deciles) {
  subset_data <- base_datos2[base_datos2$decil_alcohol == decil, variables_interes]
  correlaciones <- sapply(variables_interes, function(variable) {
    cor(subset_data$poblacion_Total_consumo, subset_data[[variable]], method = "spearman")
  })
  correlaciones_Deciles[, as.character(decil)] <- correlaciones
}

print(correlaciones_Deciles)




# Cuadro 4: índice de felicidad promedio para cada decil 

# Calcular el índice de felicidad promedio, varianza, máximo y mínimo, y obtener los nombres de los países para cada decil

resumen_felicidad_por_decil <- base_datos2 %>%
  group_by(decil_alcohol) %>%
  summarize(
    Promedio = round(mean(indice_de_felicidad),2),
    Varianza = round(var(indice_de_felicidad),2),
    Máximo = round(max(indice_de_felicidad),2),
    Mínimo = round(min(indice_de_felicidad),2))

print(resumen_felicidad_por_decil)

# Cuadro 5: Consumo promedio de alcohol para cada decil

# Calcular el consumo promedio de alcohol para cada decil

consumo_promedio_por_decil <- base_datos2 %>%
  group_by(decil_alcohol) %>%
  summarize(Consumo_Promedio_Alcohol = round(mean(poblacion_Total_consumo),2))

print(consumo_promedio_por_decil)

# Crear la tabla LaTeX

# tabla_latex1 <- xtable(
# resumen_felicidad_por_decil,
# caption = "Resumen Estadístico del Índice de Felicidad por Decil de Consumo de Alcohol",
# label = "tab:resumen-felicidad-decil")

# Configurar opciones para ajustar la tabla a una sola página
#options(xtable.comment = FALSE)
#print(tabla_latex1, file = "tabla_deciles.tex", floating = FALSE, hline.after = c(-1, 0, nrow(promedio_felicidad_por_decil)))

# Cuadro 6: Calcular el índice de felicidad promedio, varianza, máximo y mínimo, y obtener los nombres de los países para cada región

resumen_felicidad_por_region <- base_datos2 %>%
  group_by(Region) %>%
  summarize(
    Promedio_Consumo_Alcohol = round(mean(poblacion_Total_consumo),2),
    Promedio_Índice_de_felicidad = round(mean(indice_de_felicidad),2),
    Varianza_Índice_de_felicidad = round(var(indice_de_felicidad),2),
    Máximo_Índice_de_felicidad = round(max(indice_de_felicidad),2),
    Mínimo_Índice_de_felicidad = round(min(indice_de_felicidad),2))

print(resumen_felicidad_por_region)

# Crear la tabla LaTeX
# tabla_latex2 <- xtable(
# resumen_felicidad_por_region,
# caption = "Resumen Estadístico del Índice de Felicidad por Región",
# label = "tab:resumen-felicidad-region")

# Configurar opciones para ajustar la tabla a una sola página

# options(xtable.comment = FALSE)
# print(tabla_latex2, file = "tabla_region.tex", floating = FALSE, hline.after = c(-1, 0, nrow(resumen_felicidad_por_region)))


