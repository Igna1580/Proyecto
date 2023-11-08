# Relación entre el índice de felicidad y el consumo de alcohol promedio para 131 paises
# Por: Jose Ignacio Rojas, Bryan Campos, Valeria Vásquez, Montserrat Beirute

# Objetivo General: Analizar la correlación entre el consumo anual de alcohol en litros de alcohol puro por persona y el Índice de felicidad en distintos países al 2023. 

# En la parte inferior del código, explicamos los tres objetivos específicos de nuestra investigación

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
# Fuente: World Population Review

consumo_alcohol <- read.csv("alcohol-consumption-by-country-2023.csv")
consumo_alcohol <- clean_names(consumo_alcohol)

# cambiar títulos a español
consumo_alcohol <- consumo_alcohol %>% rename(pais = country)
consumo_alcohol <- consumo_alcohol %>% rename(poblacion_Total_consumo = both)
consumo_alcohol <- consumo_alcohol %>% rename(hombres_consumo =  male)
consumo_alcohol <- consumo_alcohol %>% rename(mujeres_consumo =  female)


# Base de datos felicidad
# Fuente:  Gallup World Poll
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
columnas_felicidad <- c("indice_de_felicidad", "error_estandar_indice_de_felicidad", "log_pib_per_capita", "apoyo_social", "esperanza_de_vida_saludable", "libertad_para_toma_de_decisiones", "generosidad", "percepcion_de_corrupcion")
base_datos <- inner_join(consumo_alcohol, select(felicidad, pais, all_of(columnas_felicidad)), by = "pais")

# Contamos con la información 131 países. 
# Decidimos incluir las siguientes columnas para realizar el análisis. 
# A continuación detallamos que información contiene cada columna:

# pais: nombre de los paises 
# poblacion_Total_consumo: consumo anual per cápita en litros de alcohol puro de los países para la población total
# hombres_consumo: consumo anual per cápita en litros de alcohol puro de los países para hombres
# mujeres_consumo: consumo anual per cápita en litros de alcohol puro de los países para mujeres
# indice_de_felicidad: El Índice de felicidad es un índice para cada país publicado anualmente por las Naciones Unidas en el World Happiness Report (WHR) el cual es un promedio nacional de la suma de  respuestas a la pregunta del Gallup World Poll (GWP): 
#                      "Por favor imagina una escalera, con escalones desde el 0 abajo y 10 arriba. El 10 representa la mejor vida posible para ti y el 0 la peor vida posible para ti. ¿En cual escalón dirías que te sientes personalmente ahora mismo?"
# error_estandar_indice_de_felicidad: error estándar del índice de felicidad
# log_pib_per_capita: PIB per cápita se ajustado mediante el logaritmo natural en dólares del año 2017 y se obtiene del Banco Mundial a través de sus World Development Indicators.
# apoyo_social: promedio nacional de respuestas a la pregunta: "¿Si estuvieras en problemas, tendrías amigos o conocidos en los que puedas contar para ayudarte cuando los necesites o no?"
# esperanza_de_vida_saludable: datos de la Organización Mundial de la Salud (OMS)
# libertad_para_toma_de_decisiones: promedio nacional de respuestas a la pregunta: "¿Estás satisfecho o no con la libertad que tienes para elegir qué hacer con tu vida?"
# generosidad: residuo de la regresión del promedio nacional de respuestas a la pregunta: "¿Has donado dinero a la caridad en el último mes" en términos del logaritmo del PIB per cápita
# percepcion_de_corrupcion: promedio nacional de la suma de  respuestas (0=no, 1=si) a la pregunta del Gallup World Poll ¿Esta la corrupción extendida por el gobierno o no?" y "¿La corrupción está muy extendida dentro de las empresas o no?"

# Verificamos la clase para cada columna

clase_columnas <- sapply(base_datos, class)
# todas son numeric excepto por "pais", que es character


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

# 1. Análisis de datos de felicidad 

# - Nos interesa saber la distribución y la densidad de los datos de felicidad

# 1a. Gráfico distribución de datos de felicidad

# - Este gráfico nos permite tener una visión inicial del comportamiento de los datos de felicidad

distribucion_felicidad <- ggplot(base_datos, aes(x=indice_de_felicidad)) +
  geom_histogram(binwidth = 0.5, col='black', fill= 'lightskyblue2')+
  labs(x= "Indice_de_Felicidad", y="Frecuencia")  +
  cowplot::theme_cowplot()

print(distribucion_felicidad)

# 1b. Gráfico densidad de datos de felicidad

# - Este gráfico nos ayuda a ver cómo se agrupan los valores alrededor de ciertos puntos

density_plot_f <- ggplot(data.frame(x = base_datos$indice_de_felicidad), aes(x = x)) +
  geom_density(fill = "lightskyblue2", alpha = 0.5) +
  labs(x = "Valor", y = "Densidad") +
  theme_minimal()  +
  cowplot::theme_cowplot()

print(density_plot_f)

# - A simple vista, los datos parecen seguir una distribución normal, sin embargo debemos hacer pruebas.

# 2. Análisis de datos de alcohol para población total 

# - Nos interesa saber la distribución y la densidad de los datos de alcohol  

# 2a. Gráfico distribución de datos de alcohol

# - Este gráfico nos permite tener una visión inicial del comportamiento de los datos de alcohol

distribucion_alcohol <- ggplot(base_datos, aes(x=poblacion_Total_consumo)) +
  geom_histogram(binwidth = 0.7, col='black', fill= 'red4')+
  labs(x= "Consumo de alcohol", y="Frecuencia")  +
  cowplot::theme_cowplot()

print(distribucion_alcohol)

# 2b. Gráfico densidad de datos de felicidad

# - Este gráfico nos ayuda a ver cómo se agrupan los valores alrededor de ciertos puntos
  
density_plot_a <- ggplot(data.frame(x = base_datos$poblacion_Total_consumo), aes(x = x)) +
  geom_density(fill = "red4", alpha = 0.5) +
  labs(x = "Valor", y = "Densidad") +
  theme_minimal()  +
  cowplot::theme_cowplot()

print(density_plot_a)

# - Los datos de consumo parecen tener una distribución uniforme, sin emabargo, debemos hacer pruebas. 


# 3. Pruebas de hipotesis 

# - Para comprobar que los datos sigan cierta distribución conocida, haremos tres 
#   pruebas de hipotesis para cada uno de los datos de consumo de alcohol de pob-
#   lación total y para los datos de felicidad

# 3a. Prueba de Kolmogorov-Smirnov 

# - La prueba de Kolmogorov-Smornov es una de las pruebas más utilizadas para 
#   determinar la similitud entre dos distribuciones.

# 3a.1 Para datos de felicidad 

# - Hipotesis nula (H0): Los datos de felicidad siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de felicidad NO siguen una distribución normal

# ks_test_felicidad <- ks.test(
# base_datos$indice_de_felicidad,  # Los datos que se están probando
# "pnorm",                        # La distribución teórica con la cual estamos comparando que en este caso, una distribución normal estándar
# mean = mean(base_datos$indice_de_felicidad),  # La media  de los datos de felicidad
# sd = sd(base_datos$indice_de_felicidad))       # La desviación estándar de los datos de felicidad

# - Resultados 

# -- p-value = 0.6895
# -- D = 0.062294
# -- alternative hypothesis: two-sided
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, 
# -- Es decir , los resultados de la prueba de Kolmogorov-Smirnov no sugieren que
#    los datos de felicidad sean significativamente diferentes de una distribución normal. 

# 3a.2 Para datos de alcohol 

# - Hipotesis nula (H0): Los datos de alcohol siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de alcohol NO siguen una distribución normal

# ks_test_alcohol <- ks.test(base_datos$poblacion_Total_consumo, "pnorm", mean = mean(base_datos$poblacion_Total_consumo), sd = sd(base_datos$poblacion_Total_consumo))

# - Resultados  

# -- p-valor = 0.2883
# -- D = 0.2883
# -- alternative hypothesis: two-sided
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula. 
# -- Es decir , los resultados de la prueba de Kolmogorov-Smirnov no sugieren que 
#    los datos de alcohol sean significativamente diferentes de una distribución normal. 


# 3b. Prueba de Anderson-Darling

# - El estadístico AD mide la diferencia cuadrada entre dos distribuciones, se 
#   tiene que entre más pequeño sea AD, mejor es el ajuste entre los datos y la 
#   distribución hipotética, en este caso la normal.

# 3b.1 Para datos de felicidad

# - Hipotesis nula (H0): Los datos de felicidad siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de felicidad NO siguen una distribución normal

ad_test_felicidad <- ad.test(base_datos$indice_de_felicidad)

# - Resultados

# -- p-value = 0.224
# -- A = 0.48462
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula.
# -- Es decir , los resultados de la prueba de Anderson Darling no sugieren que 
#    los datos de felicidad sean significativamente diferentes de una distribución normal. 


# 3b.2 Para datos de alcohol

# - Hipotesis nula (H0): Los datos de alcohol siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de alcohol NO siguen una distribución normal

ad_test_alcohol <- ad.test(base_datos$poblacion_Total_consumo)

# -- p-value = 7.595e-06
# -- A = 2.2953
# -- p < 0.05 => Se puede rechazar la hipotesis nula es decir, los resultados de 
#    la prueba de Anderson-Darling sugieren que los datos de alcohol son signifi-
#    cativamente diferentes de una distribución normal. 

# 3c. Prueba de Lilliefors

# - La prueba de Lilliefors es una modificación de prueba Kolmogorov, de manera 
#   que compara la distribución empírica de los datos con una distribución normal 
#   de parámetros desconocidos y por tanto, toma la media y varianza de la muestra de datos.

# 3c.1 Para datos de felicidad 

# - Hipotesis nula (H0): Los datos de felicidad siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de felicidad NO siguen una distribución normal

lil_test_felicidad <- lillie.test(base_datos$indice_de_felicidad)

# - Resultados 

# -- p-value = 0.2437
# -- D = 0.062294
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, 
# -- Es decir , los resultados de la prueba de Lilliefors no sugieren que los datos de felicidad sean significativamente diferentes de una distribución normal. 

# 3c.2 Para datos de alcohol 

# - Hipotesis nula (H0): Los datos de alcohol siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de alcohol NO siguen una distribución normal

lil_test_alcohol <- lillie.test(base_datos$poblacion_Total_consumo)

# - Resutlados 

# -- p-value = 0.01903
# -- D = 0.08592
# -- p < 0.05 => Se puede rechazar la hipotesis nula es decir, 
#    los resultados de la prueba de Lilliefors sugieren que los datos de alcohol 
#    son significativamente diferentes de una distribución normal. 

# - Por lo tanto, según las tres pruebas, los datos de índice de felicidad no 
#   sugieren que los datos de felicidad sean significativamente diferentes de una distribución normal. 

# - Por su parte, según las pruebas de lilliefors y Anderson Darling, los datos 
#   de consumo de alcohol son significativamente diferentes de una distribución normal. 

# - Como los datos de consumo de alcohol no son normales, entonces para calcular
#   la correlación, debemos usar la correlación de Spearman y no la correlación de Pearson. 

#---Bloque Jose Ignacio---------------------------------------------------------


# 1. Más visualización de los datos de Felicidad

# 1a. Buscamos el parámetro de máxima verosimilitud respecto a datos de felicidad

# -- Enontramos la cantidad de datos de felicidad 
n = length(base_datos$indice_de_felicidad)
# -- n = 131

# -- Sumamos todos los indices de felicidad para cada uno de los países
suma.felicidad = sum(base_datos$indice_de_felicidad)
# - suma.felicidad = 725.781


# -- Calculamos la la media de la muestra
mu = suma.felicidad/n
# -- mu = 5.540313
# -- Entonces el índice de felicidad promedio para los países es de 5.54

# -- Calculamos la varianza muestral 
sigma.cuadrado = (sum((base_datos$indice_de_felicidad-mu)^2))/n
# -- sigma.cuadrado = 1.278235

# -- Calculamos el máximo y el mínimo de los datos de felicidad 
max_felicidad <- max(base_datos$indice_de_felicidad)
min_felicidad <- min(base_datos$indice_de_felicidad)
# -- max_felicidad = 7.804 
# -- min_felicidad = 1.778

# -- Generamos un vector de 1000 datos aleatorios, considerando el valor máximo y el valor mínimo
theta = seq(1.778, 7.804, length.out = 1000)


# 1b- Realizamos gráfico de Máxima Verosimilitud

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

print(grafico_Maxima_Verosimilitud)

Grafico_max_ver_felicidad <- ggplot(data = base_datos, aes(x = indice_de_felicidad)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightskyblue", color = "black", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(base_datos$indice_de_felicidad), sd = sd(base_datos$indice_de_felicidad)), size = 1) +
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud respecto al Índice de felicidad", x = "Índice de felicidad", y =
      "Densidad")

print(Grafico_max_ver_felicidad)

# - En el histograma presentado se puede observar como los datos aparentan tener una distribución parecida a una normal.
# - Lo anterior se complementamos con las pruebas de hipotesis hechas anteriormene en el código. 

# 1c. Guardar el gráfico
# - Para poder guardar este gráfico y subirlo al overleaf se necesitó el siguiente código
# ggsave(filename = "Grafico_max_ver_felicidad.pdf", plot = Grafico_max_ver_felicidad, device = "pdf", width = 10, height = 8)


# 2. Más visualización de los datos de alcohol

# 2a. Grafico de máxima verosimilitud respecto a consumo de alcohol

Grafico_max_ver_alcohol <- ggplot(data = base_datos, aes(x = poblacion_Total_consumo)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.3, fill = "lightblue2", color = "black", alpha = 0.5) +
  stat_function(fun = dunif, args = list(min = 0, max = 13.19), size = 1) +  # Distribución uniforme (0,13.19)
  cowplot::theme_cowplot() +
  labs(
    title = "Verosimilitud respecto al consumo de alcohol", x = "Consumo de alcohol", y =
      "Densidad")

print(Grafico_max_ver_alcohol)

# - En el gráfico, se aprecia que los datos de consumo de alcohol no muestran una distribución conocida
# - Lo anterior sugiere la necesidad de aplicar otros métodos para encontrar una distribución adecuada. 
# - Además, se nota cierto grado de irregularidad en los datos, con valores muy altos y bajos intercalados.


# 2b. Guardar el gráfico 
# ggsave(filename = "Grafico_max_ver_alcohol.pdf", plot = Grafico_max_ver_alcohol, device = "pdf", width = 10, height = 8)


# 3. Coeficiente de asimetría de Fisher

# - Mide la simetría que existe en los datos.

# 3a. Para datos de felicidad 

coef.asim.Fish <- skewness(base_datos$indice_de_felicidad) 

# - Resultados: 

# -- coeficiente de asimetría de Fisher: -0.4270003
# -- Existe una asimetría hacia la izquierda, lo cual es entendible cuando se 
#    toman en cuenta que los datos están acotados y el promedio tiende a estar más a la derecha
# -- No es cercano a 1, por lo que no es simétrica

# - Nota: Este coeficiente de asímetría funciona para datos Normales, por lo que no lo hacemos para los datos de consumo de alcohol. 

# 4. Coeficiente de aplastamiento o curtosis de Fisher

# - Permite determinar el "aplastamiento" de los datos en relación con una distribución normal.
# - Si la curtosis = 3, entonces es tan aplastada como una normal.

# 4a. Para datos de felicidad 

curtosis.Fish <- kurtosis(base_datos$indice_de_felicidad) 

# - Resultados: 

# -- Curtosis de Fisher: 3.055602
# -- Es muy cercano a 3, por lo que existe un parecido a la distribución normal en ese aspecto.

#---Bloque Bryan----------------------------------------------------------------

# 1. Realizamos un cuadro de Resumen Estadístico completo 

# - Numero de columnas
num_columnas <- ncol(base_datos)
# - num_columnas = 13 

# - Nombres de las variables de interés (sólo consideramos las que son del tipo "numeric")

Nombres <- colnames(base_datos)[2:(num_columnas - 1)]  

# - Estadísticos a considerar 

Minimo <- Maximo <- Rango <- Media <- Mediana <- PrimerCuartil <- TercerCuartil <- RangoIntercuartil <- DesviacionEstandar <- Varianza <- CoeficienteVariacion <- vector("numeric", length(Nombres))

# - Llenar cada uno de los vectores

for(i in 2:(num_columnas - 1)){  
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

# - Juntamos los vectores en un único dataframe

AnalisisD <- data.frame(Nombres, Minimo, Maximo, Rango, Media, PrimerCuartil,
                        Mediana, TercerCuartil, RangoIntercuartil, DesviacionEstandar,
                        Varianza, CoeficienteVariacion)

# Redondear las columnas 

columnas_a_redondear <- c("Minimo", "Maximo", "Rango", "Media", "PrimerCuartil",
                          "Mediana", "TercerCuartil", "RangoIntercuartil", "DesviacionEstandar",
                          "Varianza", "CoeficienteVariacion")

AnalisisD[columnas_a_redondear] <- round(AnalisisD[columnas_a_redondear], 2)

# Eliminar filas

AnalisisD <- AnalisisD[-c(1:11), ]


#---Bloque Montse---------------------------------------------------------------

# 1. Realizamos gráficos descriptivos  

# 1a. Gráfico 1: Consumo de alcohol por país, codificado con escala de color según Índice de felicidad

# - Nos interesa hacer un gráfico que nos permita analizar si los países donde se consume más alcohol, tienen índice de felicidad más altos.
# - Las barras en el siguiente gráfico representan la cantidad de alcohol que se consume en cada país.
# - El color de la barra representa el índice de felicidad

# - Para aumentar el tamaño del gráfico 

options(repr.plot.width = 4, repr.plot.height = 15)

# - Creamos un nuevo dataframe ordenado según consumo de alcohol de menor a mayor

base_datos2 <- base_datos %>% arrange(poblacion_Total_consumo)

# - Creamos un gráfico de barras horizontales según consumo de alcohol y codificado por color según índice de felicidad  

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

print(gráfico_Barras1)

# - Del gráfico podemos notar que los países en la parte superior (los que consumen más alcohol), tienen tonos más verdes (índices de felicidad más altos) a excepción de unos cuantos países. 
# - De la misma manera, se evidencian tonos más rojos en la parte inferior del gráfico, a excepción de unos cuantos países.  

# - Guardamos el gráfico en un archivo pdf 

# ggsave("grafico_barras.pdf", plot = gráfico_Barras1, width = 8.27, height = 11.69, dpi = 500)


# 1b. Gráfico 2: Promedio de consumo de alcohol por región, ordenado por Índice de felicidad promedio

# - Nos interesa realizar un análsis por regiones del mundo y ver si agrupamos a los países de esa manera, hay algún tipo de comportamiento relacionada al consumo de alcohol y el índice de felicidad. 

# 1b.1 Versión original 

# - Calculamos los promedios por región y los ordenamos por índice de felicidad promedio de mayor a menor

promedios_region <- base_datos %>%
  group_by(Region) %>% summarise(Promedio_Alcohol = mean(poblacion_Total_consumo),
                                Promedio_Felicidad = mean(indice_de_felicidad)) %>% arrange(desc(Promedio_Felicidad))   

# - Creamos el gráfico de barras

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


# 1b.2 Versión para el anteproyecto 

# - Modificamos los nombres de las regiones para que quepan

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

# - Le quitamos el título pues a la hora de subirlo al overleaf, no queremos que salga el título dos veces

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

print(grafico_promedios2)

# - Del gráfico podemos notar que las tres regiones con el índice de felicidad promedio más alto, tambien tienen el consumo de alcohol más alto. 
# - Por su parte, las dos regiones con el consumo de alcohol más bajos, también tienen el índice de felcidad promedio más bajo. 


# 1b.3 Guardamos el gráfico en un pdf

# ggsave(filename = "grafico_barras2.pdf", plot = grafico_promedios, device = "pdf", width = 10, height = 8)


# 1c. Gráfico 3: Gráfico de dispersión con línea de regresión

# - Nos interesa visualizar el comportamiento de los datos y un grafico de dispersión nos permite relacionar las dos variables de nuestro análisis 
# - Cada punto en el gráfico representa un país y su posición en el mismo indica tanto su nivel de consumo promedio como su Índice de felicidad. 
# - La línea azul trazada, muestra la tendencia general que existe entre el consumo de alcohol y el Índice de felicidad de todos los países.

# 1c.1 Versión original del gráfico 

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

# - Para guardar el gráfico

# ggsave(filename = "grafico_dispersion.pdf", plot = grafico_dispersion, device = "pdf", width = 4, height = 3)

# 1c.2 Versión para el anteproyecto 

grafico_dispersion2 <- ggplot(base_datos, aes(x = poblacion_Total_consumo, y = indice_de_felicidad)) +
  geom_point() + cowplot::theme_cowplot() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Línea de regresión
  labs(
    x = "Consumo de alcohol puro en litros",
    y = "Índice de felicidad") +
  theme(plot.margin = margin(20, 20, 20, 20))  # Márgenes del gráfico

grafico_dispersion2

# - La línea de regresión presenta una ligera pendiente positiva. 
# - Lo anterior indica que de acuerdo con estos datos, existe una relación positiva entre el consumo de alcohol y el Índice de felicidad. 
# - En otras palabras, a medida que aumenta el consumo de alcohol, también tiende a aumentar el Índice de felicidad.

# 1c.3 Para guardar el gráfico (sin título para que no se vea dos veces en el anteproyecto)

# ggsave(filename = "grafico_dispersion2.pdf", plot = grafico_dispersion2, device = "pdf", width = 4, height = 3)


# 1d. Gráfico 4: Índice de felicidad promedio por decil de consumo de alcohol

# - El Gráfico 1 no nos evidencia del todo si los países donde se consume más alcohol tienen índices de felicidad más altos.
# - Por esta razón, decidimos dividir a todos los países en deciles según su consumo de alcohol. 
# - De esta manera, el decil 1 tiene el 10% de los países con el consumo de alcohol más bajo, mientras que el Decil 10, tiene el 10% de los países con el consumo de alcohol más alto. 
# - Ahora, podemos calcular el promedio de índice de felcidad y ver si efectivamente los países donde se consume más alcohol tienden a tener índices de felcidad más altos. 

# 1d.1 Primera versión

# - Primero, calculamos los deciles del consumo de alcohol y les asignamos a una nueva variable

base_datos2 <- base_datos2 %>% mutate(decil_alcohol = ntile(poblacion_Total_consumo, 10))

# - Segundo, calculamos el promedio, máximo y mínimo del índice de felicidad para cada decil

promedio_felicidad_por_decil <- base_datos2 %>% group_by(decil_alcohol) %>% summarize(promedio_felicidad = mean(indice_de_felicidad))

max_felicidad_por_decil <- base_datos2 %>% group_by(decil_alcohol) %>% summarize(max_felicidad = max(indice_de_felicidad))

min_felicidad_por_decil <- base_datos2 %>% group_by(decil_alcohol) %>% summarize(min_felicidad = min(indice_de_felicidad))


# - Tercero creamos un gráfico de línea que representa el índice de felicidad promedio para cada decil de consumo de alcohol.
# - Además, en el gráfico, agregamos los máximos y mínimos de cada decil 

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


# 1d.2 Versión para el anteproyecto (sin título) 

grafico_Linea2 <- ggplot(promedio_felicidad_por_decil, aes(x = decil_alcohol, y = promedio_felicidad)) +
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

print(grafico_Linea2)

# - Del gráfico anterior, se puede observar que a medida que se consideran deciles más altos, 
#   es decir, deciles donde el consumo de alcohol es mayor, el Índice de felicidad aumenta 
#   gradualmente hasta llegar al decil 8, donde se alcanza el Índice de felicidad promedio más alto. 



# 2. Realizamos cuadrados de análisis descriptivo

# En esta sección se presentan los gráficos que hicimos para fundamentar el análisis descriptivo

# 2a. Cuadro 1: Calcular el índice de felicidad promedio, varianza, máximo y mínimo, y obtener los nombres de los países para cada región

# - Nos interesa calcular ciertos estadísticos para conocer como se comporta el índice de felidad y el consumo de alcohol para cada región.
# - Además agregamos una columna para conocer el # de países que tenemos para cada región, pues esto puede influir en nuestra interpretación de los datos. 
# - Este cuadro nos va a ser útil para complementar la información del gráfico 2. 

resumen_felicidad_por_region <- base_datos2 %>%
  group_by(Region) %>%
  summarize(
    Cantidad_Paises = n(),  
    Promedio_Consumo_Alcohol = round(mean(poblacion_Total_consumo), 2),
    Promedio_Índice_de_felicidad = round(mean(indice_de_felicidad), 2),
    Varianza_Índice_de_felicidad = round(var(indice_de_felicidad), 2),
    Máximo_Índice_de_felicidad = round(max(indice_de_felicidad), 2),
    Mínimo_Índice_de_felicidad = round(min(indice_de_felicidad), 2))


print(resumen_felicidad_por_region)

# 2a.1 Resultados importantes: 

# -- Vemos que la región Australia y Oceanía, así como también Norteamérica, tienen pocos países (2 y 3 reespectivamente).
# -- También vemos que tenemos 40 países en la región de Europa. 
# -- Vemos que la varianza más alta en relación al índice de felicidad la tiene la región de Medio Oriente, Norte de Africa y Gran Arabia. Esto tiene sentido cuando analizamos que el I.F. máximo es de 7.47, pero el promedio está en 1.21.
# -- Si comparamos las regiones de Centroamérica y el Caribe con Suramérica que tienen cantidad de países similares, en Suramérica tienen un promedio de cosnumo de alcohol más alto, pero tienen un promedio de I.F. más bajo. 
# -- Si analizamos por regiones, vemos que las regiones de Africa Sub-sahariana, Asia, Centroamerica y el Caribe, Medio Oriente, Norte de Africa y Gran Arabia, tienen un consumo promedio de alcohol en litros de alcohol puro menor al promedio muestral de 6.23. 
# -- Por su parte, las regiones de Africa Sub-sahariana, Asia, Medio Oriente, Norte de Africa y Gran Arabia tienen un índice de felicidad promedio menor al promedio muestral de 5.54. 

# 2a.2 Crear la tabla LaTeX

# tabla_latex2 <- xtable(resumen_felicidad_por_region, caption = "Resumen Estadístico del Índice de Felicidad por Región",label = "tab:resumen-felicidad-region")

# 2a.3 Configurar opciones para ajustar la tabla a una sola página

# options(xtable.comment = FALSE)
# print(tabla_latex2, file = "tabla_region.tex", floating = FALSE, hline.after = c(-1, 0, nrow(resumen_felicidad_por_region)))


# 2b. Cuadro 2: Consumo promedio de alcohol para cada decil

# - Va ser importante poder presentar cual es el consumo promedio de alcohol de cada decil 

# 2b.1 Calcular el consumo promedio de alcohol para cada decil

consumo_promedio_por_decil <- base_datos2 %>%
  group_by(decil_alcohol) %>%
  summarize(Consumo_Promedio_Alcohol = round(mean(poblacion_Total_consumo),2))

print(consumo_promedio_por_decil)

# 2b.2 Resultados interesantes: 

# -- Si se considera una cerveza con un promedio de 12 onzas (aproximadamente 0,355 litros) y un contenido promedio de alcohol puro del 5%, esto equivale a aproximadamente 0,0178 litros de alcohol puro por cerveza. 
# -- Si en el decil 1, se consume un promedio de 0.33 litros de alcohol puro por persona al año, eso significa que la persona promedio en este decil consume alrededor de 18.5 cervezas al año.
# -- Contrario en el decil 10, si el consumo promedio es de 12.6 litros de alcohol puro por persona al año, esto significa que la persona promedio en este decil consume alrededor de 707.865 cervezas al año.
# -- Ahora, una persona en el decil 5, cuyo consumo promedio es de 5.28, consume alrededor de 296 cervezas al año. 
# -- Es importante resaltar que todos esos cálculos se basaron en promedios y en la suposición de que las personas solo consumen cerveza, lo que no refleja la realidad de la mayoría de las personas.

# 2b.3 Crear la tabla LaTeX

# tabla_latex1 <- xtable( resumen_felicidad_por_decil, caption = "Resumen Estadístico del Índice de Felicidad por Decil de Consumo de Alcohol",label = "tab:resumen-felicidad-decil")

# 2b.4 Configurar opciones para ajustar la tabla a una sola página

# options(xtable.comment = FALSE)
# print(tabla_latex1, file = "tabla_deciles.tex", floating = FALSE, hline.after = c(-1, 0, nrow(promedio_felicidad_por_decil)))



# 2c. Cuadro 3: índice de felicidad promedio, varianza, máximo y mínimo para cada decil 

# - Nos parece importante tener un cuadro donde podamos ver algunos estadísticos para cada decil que consideramos relevante para nuestra investigación.
# - Este nos permite complementar con el gráfico 4

# 2c.1 Calcular el índice de felicidad promedio, varianza, máximo y mínimo, y obtener los nombres de los países para cada decil

resumen_felicidad_por_decil <- base_datos2 %>%
  group_by(decil_alcohol) %>%
  summarize(
    Promedio = round(mean(indice_de_felicidad),2),
    Varianza = round(var(indice_de_felicidad),2),
    Máximo = round(max(indice_de_felicidad),2),
    Mínimo = round(min(indice_de_felicidad),2))

print(resumen_felicidad_por_decil)

# 2c.2 Resultados interesantes

# -- Vemos que en general, deciles de mayor consumo tienden a reportar niveles de felicidad promedio más altos en comparación con aquellos en los deciles de menor consumo 
# -- También, vemos que la varianza cambia mucho para cada decil, alcanzando una varianza de 1.67 en el decil 4 y una de 0.54 en el decil 3. 
# -- Notamos que los deciles 1 y 2 muestran los promedios de índice de felicidad más bajos mientras que los deciles 10, 9 y 8 el índice de felicidad promedio más alto. 
# -- El país más feliz, está en el decil 8 (Finlandia)
# -- El país menos feliz, está en el decil 1 (Afganistán)



# 3. Objetivos específicos

# 3a. Objetivo: Encontrar el coeficiente de correlación de Spearman entre el consumo de alcohol y la percepción de felicidad. 

# - Es importante resaltar, que como nuestros datos de alcohol no siguen una distribución normal, no podemos aplicar la correlación de Spearman
# - Por ese motivo, recurrimos a calcular la correlación utilizando el método de Spearman
# - Correlación de Spearman: un método no paramétrico que no requiere suposiciones sobre la distribución de los datos de la muestra. 
#   La correlación de Spearman se utiliza para analizar la asociación entre dos variables cuantitativas, midiendo la fuerza y la dirección de la relación entre ellas. 
#   Esta asociación puede ser lineal o curvilínea, positiva o negativa, y el coeficiente de correlación toma valores entre -1 y 1, donde -1 representa una relación negativa, 1 una relación positiva y 0 una falta de correlación.

# 3a.1 Cuadro 4: Matriz de correlación entre consumo de alcohol y variables de el índice de felicidad

# - Queremos calcular la correlación entre el consumo de alcohol y todas las variables que aparecen en el reporte del índice de felicidad
# - Esto nos permitirá conocer si hay variables que tienen una correlación más significativa entre ellas. 

# 3a.1.1 Variables de ínterés para el cálculo de la correlación

variables_interes <- c("poblacion_Total_consumo", "indice_de_felicidad", "apoyo_social", "esperanza_de_vida_saludable", "libertad_para_toma_de_decisiones", "generosidad", "percepcion_de_corrupcion")

# 3a.1.2 Calcular la matriz de correlación de Spearman

correlacion_spearman <- round(cor(base_datos[variables_interes], method = "spearman"), 2)

print(correlacion_spearman)

# 3a.1.3 Resultados interesantes

# -- La correlación de Spearman entre el consumo de alcohol y el índice de felicidad según los datos obtenidos es de 0.55. 
# -- Ese coeficiente de correlación, según Barrera (2014), se clasifica como una correlación positiva considerable. 
# -- Esperanza de vida saludable y percepción a la corrupción muestran tener la correlación negativa más alta con -0.4. 
# -- Apoyo social y el índice de felicidad muestran tener la correlación positiva más alta con 0.85. 


# 3a.1.4 Guardamos la correlación de spearman entre el consumo de alcohol y todas las variables del indice de felicidad

correlacion_spearman_consumoYvariables <- data.frame(correlacion_spearman = correlacion_spearman[, 1])

# 3a.1.5 Resultados importantes para la presente investigación

# -- La correlación más alta entre el consumo de alcohol y las variables del WHR, es la de apoyo social. 
# -- La segunda correlación más alta entre consumo de alcohol y las variables es el índice de felicidad. 
# -- Sabiendo que la correlación más alta de la matriz de correlación fue la de apoyo social con índice de felicidad, parece que estas tres variables están relacionadas. 
# -- Parece una correlación muy débil y negativa entre el consumo de alcohol y las variables de generosidad y percepción de corrupción
# -- Va ser interesante evaluar la correlción entre consumo de alcohol y apoyo social. 


# 3a.2 Cuadro 5: Correlación entre consumo de alcohol y variables de el índice de felicidad para cada región

# - Queremos ver si dividiendo los países por regiones, encontramos alguna correlación interesante entre el consumo de alcohol y las variables del WHR

# 3a.2.1 Regiones de interés

regiones_unicas <- c("Europa", "Africa Sub-sahariana", "Asia", "Australia y Oceania", "Norteamerica", "Suramerica", "Centroamerica y el Caribe", "Medio Oriente, Norte de Africa y Gran Arabia")

# 3a.2.2 Calcular la correlación de Spearman para cada región

correlaciones_Regiones <- data.frame(Variable = variables_interes)

for (region in regiones_unicas) {
  subset_data <- base_datos[base_datos$Region == region, variables_interes]
  correlaciones <- sapply(variables_interes, function(variable) cor(subset_data$poblacion_Total_consumo, subset_data[[variable]], method = "spearman"))
  correlaciones_Regiones[region] <- correlaciones}

# 3a.2.3 Convertir las correlaciones a tipo numérico

correlaciones_Regiones[, -1] <- lapply(correlaciones_Regiones[, -1], as.numeric)

# 3a.2.4 Redondear los datos a dos decimales

correlaciones_Regiones[, -1] <- round(correlaciones_Regiones[, -1], 2)

print(correlaciones_Regiones)

# 3a.2.5 Resultados interesantes

# -- Europa: La correlación más significativa entre consumo de alcohol de la población total y las variables del WHR es negativa y es generosidad con una correlación débil de -0.30. 
#           Es decir, parece ser a medida que aumenta el consumo de alcohol, la generosidad en esta región es menor. 
# -- Africa Sub-sahariana: En esta región, la correlación más significativa entre el consumo de alcohol y las variables es la de apoyo social. La relación es positiva y de 0.43. 
#                         Entonces en esta región, en cierto grado, a medida que aumenta el consumo de alcohol, aumenta también el apoyo social. 
# -- Asia: En este país, las correlaciones entre consumo de alcoholy las variables del WHR están débiles, siendo la más significativa la de generosidad con -0.29. 
# -- Australia y Oceanía: En esta región, sólo están dos países, sin embargo podemos decir que los dos países muestran el mismo comportamiento entre consumo de alcohol y las demás variables.
#                        En particular, las variables con correlación positiva son el de índice de felicidad y apoyo social.
# -- Norteamérica: En esta región, las variables de apoyo social, índice de felicidad, esperanza de vida saludable y generosidad tienen una correlación positiva de 0.5. 
# -- Suramérica: En esta región, la correlación más significativa entre consumo de alcohol y las variables es el índice de felicidad con 0.73, seguida por apoyo social con 0.58. 
# -- Centroamérica y el Caribe: En esta región, la relación más significativa entre consumo de alcohol y las demás variables es generosidad con -0.57. 
#                              Por su parte, la segunda relación más significativa es la de apoyo social con 0.52.
# -- Medio Oriente, Norte de Africa y Gran Arabia: Para esta región, la correlación más significativa se da para la variable de esperanza de vida saldable con 0.77.

# - Entonces vemos como si analizamos por regiones, se evidencian diferentes correlaciones significativas. 
# - Importante señalar que para ciertas regiones, se da una correlación negativa débil entre consumo de alcohol y generosidad. 


# 3a.3 Cuadro 6: Correlación entre consumo de alcohol y variables de índice de felicidad para cada decil 

# - También queremos ver la correlación entre consumo de alcohol y las demás variables del wHR, para ver si hay patrones diferentes si consideramos diferentes niveles de consumo de alcohol. 

# 3a.3.1 Calcular las correlaciones para cada decil

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

# 3a.3.2 Resultados interesantes

# -- Deciles 1, 2, 4: Para estos deciles, la correlación más significativa entre consumo de alcohol y las variables del WHR es Percepción a la corrupción.
#                     Para decil 1 es de 0.277,  para decil 2 es de 0.505, para decil 3 es de -0.53. 
#                     Note que en el caso de decil 1 y 2. lo que podemos interpretar es que a medida que se consideran consumo de alcohol más altos, la percepción de corrupción de los habitantes de estos países, también aumenta. 
# -- Decil 3: En el caso de este decil, la correlación más signficativa es negativa, por -0.52747253 y correspondiente a la variable de esperanza de vida saludable. 
#             Es decir, a medida de que aumenta el consumo de alcohol para este decil, la esperanza de vida saludable disminuye.            

# -- Decil 5: En este caso, la correlación más significativa corresponde al índice de felicidad y es de -0.3351648. 
# -- Decil 6 y 7: Contrario a lo visto en el decil 5, la correlación más significativa para estos dos deciles también corresponde al índice de felicidad pero esta vez positiva, de 0.73 y 0.65 respectivamente. 
# -- Decil 8: Este decil que es el que vimos que tenía el índice de felicidad más alto, tiene la correlación más significativa de percepción a la corrupción y es negativa -0.49586965. 
# -- Decil 9: La variable con la correlación más significativa es apoyo social y es de -0.40165100. Entonces a medida de que aumenta el consumo de alcohol, disminuye el apoyo social. 
# -- Decil 10: Este decil que es el que consume más alcohol, muestra una no correlación para el índice de felcidad y percepción de la corrupción. 

# - Vemos entonces que todos los deciles tienen comportamientos diferentes en relación a la correlación entre consumo de alcohol y las variables del WHR. 
# - Los resultados del decil 8 son consistentes a lo que habíamos analizado pues este siendo el que vimos que tenia el índice de felcidad promedio más alto, también muestra que a medida de que aumenta el consumo de alcohol, parece ser que los habitantes consideran su país menos corrupto. 


# 3b. Construir la función de distribución del coeficiente de correlación. 

# - Queremos contruir la distribución del coeficiente de correlación de Spearman, lo cual lo haremos utilizando el método de Bootstrap.
# - Para ello, utilizamos la función boot, la cual considera la muestra original como la verdadera. 
# - Nuestra muestra original tiene 131 datos, para los cuales tenemos datos de consumo de alcohol y el índice de felicidad, entonces la función lo que hace es agarrar 131 pares de datos aleatoriamente de la muestra original, los pares pueden repetirse y hace una muestra nueva de 131 datos. 
# - A esa muestra nueva, le calcula el coeficiente de correlación de spearman, y lo almacena en un vector. 
# - Este proceso lo repite 1000 veces y al final utilizamos ese vector para observar a la distribución del coeficiente de correlación. 

set.seed(123) # para que toda las veces que lo corramos obtengamos el mismo boot

bootstrap_distribución_coeficiente <- boot(base_datos[c("poblacion_Total_consumo","indice_de_felicidad")], statistic = function(data, i) {
                                      cor(data[i, "poblacion_Total_consumo"], data[i, "indice_de_felicidad"], method='spearman')}, R = 1000)

# 3b.1 Para visualizar los 1000 coeficientes de correlación

bootstrap_distribución_coeficiente$t

# 3b.2 Creamos un histograma de las muestras bootstrap
  
dist.coef.correlacion.spear <-   hist(bootstrap_distribución_coeficiente$t, main = "Distribución Bootstrap del coeficiente de correlación de Spearman",
      xlab = "Coeficiente de correlación de spearman", col = "lightblue")

# 3b.3 Realizamos pruebas de hipotesis de normalidad 

# - Nos interesa saber si nuestra distribución del coeficiente de correlación sigue una distribución normal
    
# - Hipotesis nula (H0): Los datos de la distribución del coeficiente de correlación de Spearman por metodo bootstrap siguen una distribución normal
# - Hipotesis alternativa (H1): Los datos de la distribución del coeficiente de correlación de Spearman por metodo bootstrap NO siguen una distribución normal
    
# 3b.3.1 Prueba de Kolmogrov-Smirnov
    
ks_test_coeficiente <- ks.test(
  bootstrap_distribución_coeficiente$t,  # Los datos que se están probando
  "pnorm",                        # La distribución teórica con la cual estamos comparando que en este caso, una distribución normal estándar
  mean = mean(bootstrap_distribución_coeficiente$t),  # La media  de los datos de la correlación
  sd = sd(bootstrap_distribución_coeficiente$t)) 

# - Resultados 

# -- p-value = 0.7079
# -- D = 0.022199
# -- alternative hypothesis: two-sided
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Kolmogrov-Smirnov no sugieren que los datos del coeficiente de correlación de spearman sean significativamente diferentes de una distribución normal. 

# 3b.3.2 Prueba de Anderson-Darling
     
ad_test_coeficiente <- ad.test(bootstrap_distribución_coeficiente$t)

# - Resultados

# -- p-value = 0.1579
# -- A = 0.54828
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Anderson Darling no sugieren que los datos del coeficiente de correlación de spearman sean significativamente diferentes de una distribución normal. 
       
       
# 3b.3.3 Prueba de Lilliefors
       
lil_test_felicidad <- lillie.test(bootstrap_distribución_coeficiente$t)
       
# -- p-value = 0.2714
# -- D = 0.022199
# -- p > 0.05 => No tenemos suficiente evidencia para rechazar la hipotesis nula, es decir , los resultados de la prueba de Lilliefors no sugieren que los datos del coeficiente de correlación de spearman sean significativamente diferentes de una distribución normal. 
  
# 3b.4 Conclusión general

# - No tenemos suficiente evidencia para rechazar la hipotesis nula. 


# 3b.5 Calcular los estadísticos relevantes de la distribución del coeficiente de correlación de Spearman

minimo_dist <- round(min(bootstrap_distribución_coeficiente$t),2)
maximo_dist <- round(max(bootstrap_distribución_coeficiente$t),2)
rango_dist <- round(maximo_dist - minimo_dist,2)
media_dist <- round(mean(bootstrap_distribución_coeficiente$t),2)
mediana_dist <- round(median(bootstrap_distribución_coeficiente$t),2)
primer_cuartil_dist <- round(quantile(bootstrap_distribución_coeficiente$t, 0.25),2)
tercer_cuartil_dist <- round(quantile(bootstrap_distribución_coeficiente$t, 0.75),2)
rango_intercuartil_dist <- round(tercer_cuartil_dist - primer_cuartil_dist,2)
desviacion_estandar_dist <- round(sd(bootstrap_distribución_coeficiente$t),2)
varianza_dist <- round(var(bootstrap_distribución_coeficiente$t),2)
coeficiente_variacion_dist <- round(desviacion_estandar_dist / media_dist,2)

# 3b.5.1 Creamos un data frame con los resultados
resultados_estadisticos_dist <- data.frame(
  Mínimo = minimo_dist,
  Máximo = maximo_dist,
  Rango = rango_dist,
  Media = media_dist,
  Mediana = mediana_dist,
  PrimerCuartil = primer_cuartil_dist,
  TercerCuartil = tercer_cuartil_dist,
  RangoIntercuartil = rango_intercuartil_dist,
  DesviacionEstandar = desviacion_estandar_dist,
  Varianza = varianza_dist,
  CoeficienteVariacion = coeficiente_variacion_dist
)

rownames(resultados_estadisticos_dist) <- "Distribución del coeficiente de correlación de Spearman"
print(resultados_estadisticos_dist)

# 3b.2 Resultados interesantes:

# -- La media de la distribución del coeficiente de correlación de Spearman entre el índice de felicidad y el consumo de alcohol es de 0.55. 
# -- Originalmente, el coeficiente de correlación de spearman nos había dado exactamente igual, 0.55. 
# -- El valor mínimo es de 0.34 y el máximo es 0.75. 
# -- La varianza es cercana a 0.



# 3b. Objetivo: Determinar intervalos de confianza para la distribución del coeficiente de correlación.

# - Para cumplir con este objetivo específico, vamos a utilizar dos métodos para encontrar los intervalos de confianza

# 3b.1 Intervalos de confianza para el método bootstrap 

# - Calcular los intervalos de confianza nos permite determinar que tan acertados son los resultados obtenidos.

# 3b.1.1 Calculamos la desviación estándar de los coeficientes de correlación de Spearman
      
sd_boot_spearman <- sd(bootstrap_distribución_coeficiente$t)
     
# 3b.1.2 Almacenamos la correlación entre consumo de alcohol y el índice de felicidad 
      
r <- cor(base_datos2$poblacion_Total_consumo, base_datos2$indice_de_felicidad, method = "spearman")
      
# 3b.1.3 Para un nivel de confianza del 95%, buscamos el valor que deja un 2.5% de probabilidad en la cola derecha de una distribución normal est. 
    
z <- qnorm(0.975)
      
# 3b.1.3 Calculamos los intervalos de confianza para el método de bootstrap
      
lower_bootstrap <- r - z * sd_boot_spearman
# -- Intervalo de confianza inferior = 0.4260598

upper_bootstrap <- r + z * sd_boot_spearman
# -- Intervalo de confianza superior = 0.6785526
      
# -- Interpretación: El intervalo [0.4260598, 0.6785526] contiente el coeficiente 
#    de correlación de spearman entre el consumo de alcohol y el índice de feli-
#    cidad con un nivel de confianza del 95%. 
      
# 3b.1.4 Queremos visualizar la densidad de la distribución del coeficiente de correlación bajo el método bootstrap con los intervalos de confianza 

plot_densidad <- density(bootstrap_distribución_coeficiente$t)
      
     
# - Para guardarlo, abre el dispositivo de salida PDF
# pdf("grafico_distribucion_spearman.pdf", width = 8, height = 6)
      
 plot(plot_densidad, main = "",
     xlab = "Coeficiente de Correlación de Spearman", ylab = "Densidad", col = "blue") +
     polygon(c(lower_bootstrap, plot_densidad$x, upper_bootstrap), 
     c(0, plot_densidad$y, 0), col = "lightblue") +
     abline(v = c(lower_bootstrap, upper_bootstrap), col = "red", lwd = 2, lty = 2) +
     abline(v = media_dist, col = "black", lwd = 1) +
     text(lower_bootstrap, 0.005, "Límite Inferior = 0.43", srt = 90, adj = c(-0.5, -0.5)) +
     text(upper_bootstrap, 0.005, "Límite Superior = 0.69", srt = 90, adj = c(-0.5, -0.5)) +
     text(media_dist, 0.005, "Media = 0.55", srt = 90, adj = c(-0.3, -0.5), col = "black") +
     cowplot::theme_cowplot()
      
      
      # Cierra el dispositivo de salida PDF
      # dev.off()
     
# 3b.2 Intervalos de confianza método delta, ajustado a spearman
      
# - Como nuestros datos de consumo de alcohol no son normales, no podemos utilizar el método delta para calcular los intervalos de confianza. 
# - Sin embargo, utilizando lo establecido por Bonnett (2000), le podemos hacer una modificación a este método para calcular los intervalos de confianza utilizando la correlación de spearman. 

# 3b.2.1 Función inversa de la tangente hiperbólica de la correlación entre consumo de alcohol y el índice de felicidad

# - trabajamos inicialmente en la escala arctan    

d <- atanh(r)
    
# 3b.2.2 Varianza ajustada de los coeficientes de correlación de Spearman
      
var_d <- (1+(r^2)/2)/(nrow(base_datos)-3)
      
# 3b.2.3 Intervalos de confianza inferior y superior, regresamos a la escala original
      
lower_delta <- tanh(d - z*sqrt(var_d))
# -- lower_delta = 0.4100848

upper_delta <- tanh(d + z*sqrt(var_d))
# -- upper_delta <- 0.6683053


# 3b.2.3 Interpretación 

# -- Utilizando el método delta ajustado para el coeficiente de correlación de spearman, 
#    decimos que con una confianza del 95%, el coeficiente de correlación de spearman,
#    está en el intervalo [0.4100848, 0.6683053]

  
# 3c.Diferencias entre los intervalos de confianza 
      
dif_inferior <- abs(lower_bootstrap-lower_delta)
# dif_inferor <- 0.01597501
      
dif_superior <- abs(upper_bootstrap-upper_delta)
# dif_superior <- 0.0102473

# 3c.1 Interpretación: 

# -- Vemos que utilizando ambos métodos para calcular los intervalos de confianza,
#    los resultados fueron muy similares, con diferencias cerca de 0.01 para cada intervalo. 
# -- Entonces, con una confianza del 95%, podemos decir que el coeficiente de correlación de Spearman
#    entre consumo de alcohol y el índice de felicidad está en el intervalo [0.43,0.69]

# -- En relación a nuestro proyecto de investigación, entonces lo que estamos observando es que sí existe 
#    una correlación positiva entre el consumo de alcohol y el índice de felicidad en países alrededor del mundo. 
# -- Estamos viendo que esa correlación, según la clasificación de Barrera (2014), se considera positiva media ó considerable.
  

# 4. Prueba de hipotesis 

# Definir una función para dividir las correlaciones en S0 y S1
dividir_correlaciones <- function(correlaciones, correlacion_media = mean(bootstrap_distribución_coeficiente$t), c = 0.05) {
  S0 <- vector("list", length(correlaciones))
  S1 <- vector("list", length(correlaciones))
  
  for (i in 1:length(correlaciones)) {
    
    if (abs(correlaciones[i] - correlacion_media) <= c) {
      S0[[i]] <- correlaciones[i]
    } else {
      S1[[i]] <- correlaciones[i]
    }
  }
  
  S0 <- unlist(S0, use.names = FALSE)
  S1 <- unlist(S1, use.names = FALSE)
  
  return(list(S0 = S0, S1 = S1))
}

resultados <- dividir_correlaciones(bootstrap_distribución_coeficiente$t,mean(bootstrap_distribución_coeficiente$t), c = 0.05)

probabilidad_S0 <- length(resultados$S0) / length(bootstrap_distribución_coeficiente$t)

# la probabilidad de que una correlación esté en S es de 0.55 cuando c es 0.05


# Otra opción

correlacion_prueba <- cor.test(base_datos$indice_de_felicidad, base_datos$poblacion_Total_consumo,
         alternative = c("two.sided"),
         method = c("spearman"),
         exact = FALSE, conf.level = 0.95, continuity = FALSE)

# rho = 0.5523062 




