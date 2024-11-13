## Importamos csv
# Directorio
setwd("C:/Users/alber/Jupyter Notebooks/Cuadernos/Maestría en Ciencia de Datos/TFM")

# Importar el archivo CSV
data <- read.csv("CEFE.csv")

#============================
# Librerías
library(ggplot2)
library(dplyr)
library(scales)

#============================
# Variable de interés (Frecuencia del consumo/Cantidad de experiencias fenomenológicas)
df <- data.frame(data$X.En.cuantas.ocasiones.has.tenido.experiencias.psicodélicas.)
df

#========================================
# Cambiamos el nombre de la variable
colnames(df) <- "Frequency of experiences"
df

# Otra opción para cambiar el nombre de manera directa
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df <- data.frame(Frequency_of_experiences = data$X.En.cuantas.ocasiones.has.tenido.experiencias.psicodélicas.)

df

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df$Frequency_of_experiences) # Total de valores únicos
unique(df$Frequency_of_experiences) # Valores únicos

df$Frequency_of_experiences <- gsub("Pocas veces (2 - 9 veces)", "Rarely (2 - 9 times)", df$Frequency_of_experiences, fixed = TRUE)
df$Frequency_of_experiences <- gsub("Una vez", "Only once", df$Frequency_of_experiences, fixed = TRUE)
df$Frequency_of_experiences <- gsub("Varias veces (10 - 30 veces)", "Repeatedly (10 - 30 times)", df$Frequency_of_experiences, fixed = TRUE)
df$Frequency_of_experiences <- gsub("Regularmente ( > 30 veces)", "Often ( > 30 times)", df$Frequency_of_experiences, fixed = TRUE)
#Nota: Con esto se reemplazan los valores asegurando coincidencias exactas con fixed = TRUE. 
#Esto se hace cuando los valores en la columna contienen espacios adicionales, caracteres especiales o mayúsculas que no coinciden exactamente con los patrones que estás buscando. 

df

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
conteo <- df %>%
  group_by(Frequency_of_experiences) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

conteo

#=====================================================================================
# Ordenar los niveles de la variable 'Frequency_of_experiences' según el orden deseado
conteo$Frequency_of_experiences <- factor(conteo$Frequency_of_experiences,
                                          levels = c("Only once", "Rarely (2 - 9 times)", "Repeatedly (10 - 30 times)", "Often ( > 30 times)"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(conteo, aes(x = Frequency_of_experiences, y = conteo, fill = Frequency_of_experiences)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.3, end = 0.7) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Frequency of lived experiences",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario






#*******************************************************************************************




#============================
# Variable de interés (Nivel académico)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_1 <- data.frame(Education_level = data$Nivel.de.estudios)

df_1

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_1$Education_level) # Total de valores únicos
unique(df_1$Education_level) # Valores únicos

df_1$Education_level <- gsub("Licenciatura / Grado / Título universitario", "Bachelor's", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Máster / Maestría", "Master", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Formación profesional / Carrera técnica", "Technical career", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Doctorado o superior", "Doctorate or PhD", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Cursos universitarios / Cursando la universidad", "Attending university", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Secundaria", "Middle school", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Bachillerato / Preparatoria", "High school", df_1$Education_level, fixed = TRUE)
df_1$Education_level <- gsub("Especialización en protección de especies y conservación de ecosistemas marinos", "Others", df_1$Education_level, fixed = TRUE)
#Nota: Con esto se reemplazan los valores asegurando coincidencias exactas con fixed = TRUE. 
#Esto se hace cuando los valores en la columna contienen espacios adicionales, caracteres especiales o mayúsculas que no coinciden exactamente con los patrones que estás buscando. 

unique(df_1$Education_level)


#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_1 <- df_1 %>%
  group_by(Education_level) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_1

#=====================================================================================
# Ordenar los niveles de la variable 'Frequency_of_experiences' según el orden deseado
con_1$Education_level <- factor(con_1$Education_level,
                                          levels = c("Middle school",
                                                     "High school",
                                                     "Attending university",
                                                     "Technical career",
                                                     "Bachelor's",
                                                     "Master",
                                                     "Doctorate or PhD", 
                                                     "Others"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_1, aes(x = Education_level, y = conteo, fill = Education_level)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.15, end = 0.85) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Education level",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario





#*******************************************************************************************




#============================
# Variable de interés (Género)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_2 <- data.frame(Genero = data$Género)

df_2

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_2$Genero) # Total de valores únicos
unique(df_2$Genero) # Valores únicos

df_2$Genero <- gsub("Mujer", "Woman", df_2$Genero, fixed = TRUE)
df_2$Genero <- gsub("Hombre", "Man", df_2$Genero, fixed = TRUE)
df_2$Genero <- gsub("No binario", "Non binary", df_2$Genero, fixed = TRUE)
df_2$Genero <- gsub("Nb", "Non binary", df_2$Genero, fixed = TRUE)
df_2$Genero <- gsub("NB (Non binary)", "Non binary", df_2$Genero, fixed = TRUE)
#Nota: Con esto se reemplazan los valores asegurando coincidencias exactas con fixed = TRUE. 
#Esto se hace cuando los valores en la columna contienen espacios adicionales, caracteres especiales o mayúsculas que no coinciden exactamente con los patrones que estás buscando. 

unique(df_2$Genero)


#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_2 <- df_2 %>%
  group_by(Genero) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_2

#=====================================================================================
# Ordenar los niveles de la variable 'Frequency_of_experiences' según el orden deseado
con_2$Genero <- factor(con_2$Genero,
                                levels = c("Man",
                                           "Woman",
                                           "Non binary"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_2, aes(x = Genero, y = conteo, fill = Genero)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.3, end = 0.8) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Gender",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario





#*******************************************************************************************




#============================
# Variable de interés (Prácticas previas al consumo)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_3 <- data.frame(Practicas_prev = data$Si.te.preparaste.antes.de.la.experiencia...qué.prácticas.realizaste.)

df_3

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_3$Practicas_prev) # Total de valores únicos
unique(df_3$Practicas_prev) # Valores únicos

df_3$Practicas_prev <- gsub(";.*", "", df_3$Practicas_prev) 
#Nota: Con gsub(";.*", "", df$columna), buscamos cualquier texto que comience con ";" y todo lo que siga después de este,
#lo reemplazamos por una cadena de texto vacía "". De esta forma, nos quedamos únicamente con el texto antes del ";"

unique(df_3$Practicas_prev)
dplyr::n_distinct(df_3$Practicas_prev)

#Seguimos con la limpieza (agrupamos respuestas similares)
df_3$Practicas_prev <- gsub("Alimentación especial, no consumir ciertas cosas ", "Dieting or fasting", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Dieta (ayuno)", "Dieting or fasting", df_3$Practicas_prev, fixed = TRUE)
df_3$Practicas_prev <- gsub("Me prepare el mismo dìa con buena alimentaciòn e hidrataciòn (segunda experiencia, hongos psilocybes)", "Dieting or fasting", df_3$Practicas_prev, fixed = TRUE)
df_3$Practicas_prev <- gsub("comí menos cantidad y no bebí alcohol", "Dieting or fasting", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Las 24h antes de la de Ayahuasca, una dieta y las 12 horas antes, ayuno. El objetivo era exploración. En cuanto a la experiencia con cannabis no hubo preparación ni tenía propósito alguno ", "Dieting or fasting", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Meditación", "Meditation", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("No me prepare como tal para la experiencia pero si tenía de hábito meditar ", "Meditation", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Psicoterapia", "Psychotherapy", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Rituales (altar, oración)", "Rituals", df_3$Practicas_prev, fixed = TRUE)
df_3$Practicas_prev <- gsub("Cánticos / Música", "Hymns or music", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Técnicas de respiración", "Breathing techniques", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Microdosificación", "Microdosing", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Establecimiento de intenciones", "Establishment intentions", df_3$Practicas_prev)
df_3$Practicas_prev <- gsub("Escritura de pensamiento en un diario", "Thoughts in a journal", df_3$Practicas_prev)

unique(df_3$Practicas_prev)
dplyr::n_distinct(df_3$Practicas_prev)

df_3$Practicas_prev <- gsub("Búsqueda de información en internet", "Others", df_3$Practicas_prev)

unique(df_3$Practicas_prev)
dplyr::n_distinct(df_3$Practicas_prev)

df_3$Practicas_prev[df_3$Practicas_prev == ""] <- "Others"

#Nota: Existe una respuesta vacía que no puede ser moficada de esta manera, dado a que cambiaría todos los registros.
#Por lo tanto, reemplazamos únicamente la respuesta de manera habitual (==)

unique(df_3$Practicas_prev)
dplyr::n_distinct(df_3$Practicas_prev)


#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_3 <- df_3 %>%
  group_by(Practicas_prev) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_3

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_3, aes(x = Practicas_prev, y = conteo, fill = Practicas_prev)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.9, end = 0.1) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Previous practices",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (Dosis consumida)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_4 <- data.frame(Dosis = data$X.Qué.tamaño..porción.o.cantidad.de.dosis.sientes.que.consumiste.)

df_4

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_4$Dosis) # Total de valores únicos
unique(df_4$Dosis) # Valores únicos

df_4$Dosis <- gsub("Baja", "Low", df_4$Dosis, fixed = TRUE)
df_4$Dosis <- gsub("Moderada", "Moderate", df_4$Dosis, fixed = TRUE)
df_4$Dosis <- gsub("Alta", "High", df_4$Dosis, fixed = TRUE)
df_4$Dosis <- gsub("Muy alta", "Very high", df_4$Dosis, fixed = TRUE)
df_4$Dosis <- gsub("Muy baja", "Very low", df_4$Dosis, fixed = TRUE)
#Nota: Con esto se reemplazan los valores asegurando coincidencias exactas con fixed = TRUE. 
#Esto se hace cuando los valores en la columna contienen espacios adicionales, caracteres especiales o mayúsculas que no coinciden exactamente con los patrones que estás buscando. 

unique(df_4$Dosis)

#Seguimos con la limpieza

df_4$Dosis[df_4$Dosis == ""] <- "Unanswered"

#Nota: Existe una respuesta vacía que no puede ser moficada de esta manera, dado a que cambiaría todos los registros.
#Por lo tanto, reemplazamos únicamente la respuesta de manera habitual (==)

unique(df_4$Dosis)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_4 <- df_4 %>%
  group_by(Dosis) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_4

#=====================================================================================
# Ordenar los niveles de la variable 'Frequency_of_experiences' según el orden deseado
con_4$Dosis <- factor(con_4$Dosis,
                       levels = c("Very low",
                                  "Low",
                                  "Moderate",
                                  "High",
                                  "Very high",
                                  "Unanswered"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_4, aes(x = Dosis, y = conteo, fill = Dosis)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.8, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Dose",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (Alegría o felicidad posterior al consumo)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_5 <- data.frame(Alegria_post = data$Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.)

df_5

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_5$Alegria_post) # Total de valores únicos
unique(df_5$Alegria_post) # Valores únicos

df_5$Alegria_post <- gsub("Sí", "Yes", df_5$Alegria_post)

unique(df_5$Alegria_post)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_5 <- df_5 %>%
  group_by(Alegria_post) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_5

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_5, aes(x = Alegria_post, y = conteo, fill = Alegria_post)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Joy or happiness afterwards experience",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario




#*******************************************************************************************




#============================
# Variable de interés (Consideras tu experiencia psicodélica como una de las experiencias más significativas de tu vida)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_6 <- data.frame(Exp_signif = data$X.Considerarías.tu.experiencia.psicodélica.como.una.de.las.experiencias.más.significativas.o.espirituales.de.tu.vida.)

df_6

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_6$Exp_signif) # Total de valores únicos
unique(df_6$Exp_signif) # Valores únicos

df_6$Exp_signif <- gsub("Sí", "Yes", df_6$Exp_signif)

unique(df_6$Exp_signif)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_6 <- df_6 %>%
  group_by(Exp_signif) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_6

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_6, aes(x = Exp_signif, y = conteo, fill = Exp_signif)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you think that this experience is one of the most important experience of your life?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (¿Se produjo esta experiencia? Pérdida de una creencia fuertemente vinculada a la personalidad)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_7 <- data.frame(Cambio_creencia = data$X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.pérdida.de.sí.mismo.o.de.alguna.creencia.fuertemente.vinculada.con.tu.personalidad.)

df_7

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_7$Cambio_creencia) # Total de valores únicos
unique(df_7$Cambio_creencia) # Valores únicos

df_7$Cambio_creencia <- gsub("Sí", "Yes", df_7$Cambio_creencia)

unique(df_7$Cambio_creencia)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_7 <- df_7 %>%
  group_by(Cambio_creencia) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_7

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_7, aes(x = Cambio_creencia, y = conteo, fill = Cambio_creencia)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a change in your most deeply rooted personal beliefs?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (Estabilidad de ánimo posterior al consumo)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_8 <- data.frame(Animo_post = data$Por.favor..responde.los.siguientes.rubros..Estabilidad.del.estado.de.ánimo.)

df_8

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_8$Animo_post) # Total de valores únicos
unique(df_8$Animo_post) # Valores únicos

df_8$Animo_post <- gsub("Mejora significativa", "Significant improvement", df_8$Animo_post)
df_8$Animo_post <- gsub("Mejora moderada", "Modest improvement", df_8$Animo_post)
df_8$Animo_post <- gsub("Sin cambios", "Unchanged", df_8$Animo_post)
df_8$Animo_post <- gsub("Empeorado", "Has deteriorated", df_8$Animo_post)

unique(df_8$Animo_post)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_8 <- df_8 %>%
  group_by(Animo_post) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_8

#=====================================================================================
# Ordenar los niveles de la variable de interés según el orden deseado
con_8$Animo_post <- factor(con_8$Animo_post,
                       levels = c("Significant improvement",
                                  "Modest improvement",
                                  "Unchanged",
                                  "Has deteriorated"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_8, aes(x = Animo_post, y = conteo, fill = Animo_post)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a changes in your mood?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (Relaciones sociales)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_9 <- data.frame(Rel_sociales = data$Por.favor..responde.los.siguientes.rubros..Relaciones.sociales..conexión.con.los.demás..)

df_9

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_9$Rel_sociales) # Total de valores únicos
unique(df_9$Rel_sociales) # Valores únicos

df_9$Rel_sociales <- gsub("Mejora significativa", "Significant improvement", df_9$Rel_sociales)
df_9$Rel_sociales <- gsub("Mejora moderada", "Modest improvement", df_9$Rel_sociales)
df_9$Rel_sociales <- gsub("Sin cambios", "Unchanged", df_9$Rel_sociales)
df_9$Rel_sociales <- gsub("Empeorado", "Has deteriorated", df_9$Rel_sociales)

unique(df_9$Rel_sociales)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_9 <- df_9 %>%
  group_by(Rel_sociales) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_9

#=====================================================================================
# Ordenar los niveles de la variable de interés según el orden deseado
con_9$Rel_sociales <- factor(con_9$Rel_sociales,
                           levels = c("Significant improvement",
                                      "Modest improvement",
                                      "Unchanged",
                                      "Has deteriorated"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_9, aes(x = Rel_sociales, y = conteo, fill = Rel_sociales)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a changes in your relationships?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario


#*******************************************************************************************




#============================
# Variable de interés (Significado existencial)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_10 <- data.frame(Sig_exist = data$Por.favor..responde.los.siguientes.rubros..Significado.existencial.)

df_10

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_10$Sig_exist) # Total de valores únicos
unique(df_10$Sig_exist) # Valores únicos

df_10$Sig_exist <- gsub("Mejora significativa", "Significant improvement", df_10$Sig_exist)
df_10$Sig_exist <- gsub("Mejora moderada", "Modest improvement", df_10$Sig_exist)
df_10$Sig_exist <- gsub("Sin cambios", "Unchanged", df_10$Sig_exist)
df_10$Sig_exist <- gsub("Empeorado", "Has deteriorated", df_10$Sig_exist)

unique(df_10$Sig_exist)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_10 <- df_10 %>%
  group_by(Sig_exist) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_10


#=====================================================================================
# Ordenar los niveles de la variable de interés según el orden deseado
con_10$Sig_exist <- factor(con_10$Sig_exist,
                             levels = c("Significant improvement",
                                        "Modest improvement",
                                        "Unchanged",
                                        "Has deteriorated"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_10, aes(x = Sig_exist, y = conteo, fill = Sig_exist)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a feeling of existential meaning?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (Empatía y compasión)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_11 <- data.frame(Emp_comp = data$Por.favor..responde.los.siguientes.rubros..Empatía.y.compasión.)

df_11

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_11$Emp_comp) # Total de valores únicos
unique(df_11$Emp_comp) # Valores únicos

df_11$Emp_comp <- gsub("Mejora significativa", "Significant improvement", df_11$Emp_comp)
df_11$Emp_comp <- gsub("Mejora moderada", "Modest improvement", df_11$Emp_comp)
df_11$Emp_comp <- gsub("Sin cambios", "Unchanged", df_11$Emp_comp)
df_11$Emp_comp <- gsub("Empeorado", "Has deteriorated", df_11$Emp_comp)

unique(df_11$Emp_comp)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_11 <- df_11 %>%
  group_by(Emp_comp) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_11


#=====================================================================================
# Ordenar los niveles de la variable de interés según el orden deseado
con_11$Emp_comp <- factor(con_11$Emp_comp,
                           levels = c("Significant improvement",
                                      "Modest improvement",
                                      "Unchanged",
                                      "Has deteriorated"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_11, aes(x = Emp_comp, y = conteo, fill = Emp_comp)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a feeling of empathy and compassion?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario


#*******************************************************************************************




#============================
# Variable de interés (Unión con la naturaleza)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_12 <- data.frame(Union_natu = data$X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.unión.con.la.naturaleza.o.el.universo.)

df_12

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_12$Union_natu) # Total de valores únicos
unique(df_12$Union_natu) # Valores únicos

df_12$Union_natu <- gsub("Sí", "Yes", df_12$Union_natu)

unique(df_12$Union_natu)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_12 <- df_12 %>%
  group_by(Union_natu) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_12


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_12, aes(x = Union_natu, y = conteo, fill = Union_natu)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a feeling of harmony with nature?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario


#*******************************************************************************************




#============================
# Variable de interés (Naciemiento)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_13 <- data.frame(Nacimiento = data$X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.del.nacimiento.)

df_13

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_13$Nacimiento) # Total de valores únicos
unique(df_13$Nacimiento) # Valores únicos

df_13$Nacimiento <- gsub("Sí", "Yes", df_13$Nacimiento)

unique(df_13$Nacimiento)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_13 <- df_13 %>%
  group_by(Nacimiento) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_13


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_13, aes(x = Nacimiento, y = conteo, fill = Nacimiento)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced anything like birth?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario


#*******************************************************************************************




#============================
# Variable de interés (Amor o agradecimiento)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_14 <- data.frame(Amor_agrad = data$X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentir.amor.o.agradecimiento.)

df_14

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_14$Amor_agrad) # Total de valores únicos
unique(df_14$Amor_agrad) # Valores únicos

df_14$Amor_agrad <- gsub("Sí", "Yes", df_14$Amor_agrad)

unique(df_14$Amor_agrad)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_14 <- df_14 %>%
  group_by(Amor_agrad) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_14


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_14, aes(x = Amor_agrad, y = conteo, fill = Amor_agrad)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a feeling of love or gratitude?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario


#*******************************************************************************************




#============================
# Variable de interés (Paz o serenidad)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_15 <- data.frame(Paz_serenid = data$X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentir.una.profunda.sensación.de.paz.o.serenidad.)

df_15

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_15$Paz_serenid) # Total de valores únicos
unique(df_15$Paz_serenid) # Valores únicos

df_15$Paz_serenid <- gsub("Sí", "Yes", df_15$Paz_serenid)

unique(df_15$Paz_serenid)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_15 <- df_15 %>%
  group_by(Paz_serenid) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_15


#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_15, aes(x = Paz_serenid, y = conteo, fill = Paz_serenid)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced a feeling of peace or serenity?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(conteo$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario





#*******************************************************************************************


#============================
# Variable de interés (Nivel de satisfacción previo)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_16 <- data.frame(Niv_sat_prev = data$Nivel.de.satisfacción.con.tu.vida..antes.de.la.s..experiencia.s..psicodélica.s.)

df_16

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_16$Niv_sat_prev) # Total de valores únicos
unique(df_16$Niv_sat_prev) # Valores únicos

df_16$Niv_sat_prev <- gsub("Insatisfecho(a)", "Dissatisfied", df_16$Niv_sat_prev, fixed = TRUE)
df_16$Niv_sat_prev <- gsub("Satisfecho(a)", "Satisfied", df_16$Niv_sat_prev, fixed = TRUE)
df_16$Niv_sat_prev <- gsub("Muy insatisfecho(a)", "Very dissatisfied", df_16$Niv_sat_prev, fixed = TRUE)
df_16$Niv_sat_prev <- gsub("Muy satisfecho(a)", "Very satisfied", df_16$Niv_sat_prev, fixed = TRUE)
df_16$Niv_sat_prev <- gsub("Neutro(a) / Ni satisfecho(a) ni insatisfecho(a)", "Neutral / Neither satisfied nor dissatisfied", df_16$Niv_sat_prev, fixed = TRUE)
#Nota: Con esto se reemplazan los valores asegurando coincidencias exactas con fixed = TRUE. 
#Esto se hace cuando los valores en la columna contienen espacios adicionales, caracteres especiales o mayúsculas que no coinciden exactamente con los patrones que estás buscando. 

unique(df_16$Niv_sat_prev)


#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_16 <- df_16 %>%
  group_by(Niv_sat_prev) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_16

#=====================================================================================
# Ordenar los niveles de la variable 'Niv_sat_prev' según el orden deseado
con_16$Niv_sat_prev <- factor(con_16$Niv_sat_prev,
                                levels = c("Very satisfied",
                                           "Satisfied",
                                           "Neutral / Neither satisfied nor dissatisfied",
                                           "Dissatisfied",
                                           "Very dissatisfied"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_16, aes(x = Niv_sat_prev, y = conteo, fill = Niv_sat_prev)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.15, end = 0.85) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Level of satisfaction with your life before the experience",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(con_16$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario





#*******************************************************************************************


#============================
# Variable de interés (Nivel de satisfacción posterior)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
df_17 <- data.frame(Niv_sat_post = data$Nivel.de.satisfacción.con.tu.vida.después.la.experiencia.psicodélica)

df_17

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_17$Niv_sat_post) # Total de valores únicos
unique(df_17$Niv_sat_post) # Valores únicos

df_17$Niv_sat_post <- gsub("Insatisfecho(a)", "Dissatisfied", df_17$Niv_sat_post, fixed = TRUE)
df_17$Niv_sat_post <- gsub("Satisfecho(a)", "Satisfied", df_17$Niv_sat_post, fixed = TRUE)
df_17$Niv_sat_post <- gsub("Muy insatisfecho(a)", "Very dissatisfied", df_17$Niv_sat_post, fixed = TRUE)
df_17$Niv_sat_post <- gsub("Muy satisfecho(a)", "Very satisfied", df_17$Niv_sat_post, fixed = TRUE)
df_17$Niv_sat_post <- gsub("Neutro(a) / Ni satisfecho(a) ni insatisfecho(a)", "Neutral / Neither satisfied nor dissatisfied", df_17$Niv_sat_post, fixed = TRUE)
#Nota: Con esto se reemplazan los valores asegurando coincidencias exactas con fixed = TRUE. 
#Esto se hace cuando los valores en la columna contienen espacios adicionales, caracteres especiales o mayúsculas que no coinciden exactamente con los patrones que estás buscando. 

unique(df_17$Niv_sat_post)


#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_17 <- df_17 %>%
  group_by(Niv_sat_post) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_17

#=====================================================================================
# Ordenar los niveles de la variable 'Niv_sat_prev' según el orden deseado
con_17$Niv_sat_post <- factor(con_17$Niv_sat_post,
                              levels = c("Very satisfied",
                                         "Satisfied",
                                         "Neutral / Neither satisfied nor dissatisfied",
                                         "Dissatisfied",
                                         "Very dissatisfied"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_17, aes(x = Niv_sat_post, y = conteo, fill = Niv_sat_post)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.15, end = 0.85) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Level of satisfaction with your life after the experience",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(con_17$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario





#*******************************************************************************************




#============================
# Variable de interés (Atención plena en la vida cotidiana)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
grep("plena", names(data), value = T, ignore.case = T) # Buscamos la variable de interés con una frase o palabra clave, vinculada al nombre de la variable/columna
df_18 <- data.frame(atenc_plena = data$Por.favor..responde.los.siguientes.rubros..Presencia.y.Mindfulness..atención.plena..en.la.vida.cotidiana.)

df_18

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_18$atenc_plena) # Total de valores únicos
unique(df_18$atenc_plena) # Valores únicos

df_18$atenc_plena <- gsub("Mejora significativa", "Significant improvement", df_18$atenc_plena)
df_18$atenc_plena <- gsub("Mejora moderada", "Modest improvement", df_18$atenc_plena)
df_18$atenc_plena <- gsub("Sin cambios", "Unchanged", df_18$atenc_plena)

unique(df_18$atenc_plena)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_18 <- df_18 %>%
  group_by(atenc_plena) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_18
#=====================================================================================
# Ordenar los niveles de la variable 'Niv_sat_prev' según el orden deseado
con_18$atenc_plena <- factor(con_18$atenc_plena,
                              levels = c("Significant improvement",
                                         "Modest improvement",
                                         "Unchanged"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_18, aes(x = atenc_plena, y = conteo, fill = atenc_plena)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Do you experienced presence and mindfullness in dayily life?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(con_18$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario



#*******************************************************************************************




#============================
# Variable de interés (Autoconciencia e introspección)
# Crear un DataFrame con la variable de interés y cambiamos el nombre al mismo tiempo (de nuestra variable)
grep("introspección", names(data), value = T, ignore.case = T) # Buscamos la variable de interés con una frase o palabra clave, vinculada al nombre de la variable/columna
df_19 <- data.frame(Intros_autocon = data$Por.favor..responde.los.siguientes.rubros..Autoconciencia.e.introspección.)

df_19

#===========================================
# Cambiamos los valores de nuestra variable
dplyr::n_distinct(df_19$Intros_autocon) # Total de valores únicos
unique(df_19$Intros_autocon) # Valores únicos

df_19$Intros_autocon <- gsub("Mejora significativa", "Significant improvement", df_19$Intros_autocon)
df_19$Intros_autocon <- gsub("Mejora moderada", "Modest improvement", df_19$Intros_autocon)
df_19$Intros_autocon <- gsub("Sin cambios", "Unchanged", df_19$Intros_autocon)
df_19$Intros_autocon <- gsub("Empeorado", "Has deteriorated", df_19$Intros_autocon)

unique(df_19$Intros_autocon)

#===============================================================================
# Crear la tabla con las agrupaciones por respuesta y sus proporciones y conteos
con_19 <- df_19 %>%
  group_by(Intros_autocon) %>%
  summarise(conteo = n()) %>%
  mutate(porcentaje = conteo / sum(conteo) * 100)

con_19
#=====================================================================================
# Ordenar los niveles de la variable 'Intros_autocon' según el orden deseado
con_19$Intros_autocon <- factor(con_19$Intros_autocon,
                             levels = c("Significant improvement",
                                        "Modest improvement",
                                        "Unchanged",
                                        "Has deteriorated"))
#Nota: Aunque no se aprecie directamente el orden al observar este dataframe, al momento de graficar sí se aprecia el orden deseado

#===============================================================================
# Graficamos
# Crear el gráfico de barras
ggplot(con_19, aes(x = Intros_autocon, y = conteo, fill = Intros_autocon)) +
  # geom_bar crea las barras y stat = "identity" indica que usamos valores exactos para las alturas de las barras
  geom_bar(stat = "identity", color = "black") +
  
  # Añadir etiquetas de texto sobre las barras, mostrando el conteo y el porcentaje
  geom_text(aes(label = paste0(conteo, " (", round(porcentaje, 1), "%)")),
            vjust = -0.5, size = 4) +
  
  # Escala de relleno en tonos de gris, lo que proporciona un gráfico en blanco y negro
  scale_fill_grey(start = 0.7, end = 0.2) +
  
  # Agregar título, y etiquetas de los ejes en inglés
  labs(title = "Changed your perception of self-awareness and introspection?",  # Título del gráfico
       x = "Responses",                      # Etiqueta del eje X
       y = "Count",                          # Etiqueta del eje Y
       caption = "Note: Absolute count and percentage shown for each category.") +  # Pie de gráfico
  
  # Aplicar el tema 'classic' que es simple y adecuado para normas APA
  theme_classic() +
  
  # Eliminar la leyenda y ajustar el formato del título y etiquetas
  theme(legend.position = "none",  # No mostrar leyenda (opcional)
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y ponerlo en negrita
        axis.title = element_text(face = "bold")) +  # Negrita en títulos de los ejes
  
  # Crear un eje secundario en el eje Y que muestre los porcentajes
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(con_19$conteo) * 100,  # Calcular porcentajes en función del total
                                         name = "Percentage (%)"))  # Etiqueta del eje secundario

