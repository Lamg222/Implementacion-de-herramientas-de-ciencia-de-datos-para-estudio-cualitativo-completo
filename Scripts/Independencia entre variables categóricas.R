#==================================================================================
##Versiones

#Versión de R
version

#Actualizamos R
#install.packages("installr", dependencies = TRUE)
library(installr)
updateR()

#Versión de Rstudio
RStudio.Version()

#==================================================================================
##Importamos csv
# Directorio
setwd("C:/Users/alber/Jupyter Notebooks/Cuadernos/Maestría en Ciencia de Datos/TFM")

# Importar el archivo CSV
data <- read.csv("CEFE.csv")

# Verificar que los datos se han importado correctamente
View(data)

# Dimensiones originales del csv
dim(data)

# Conocer los nombres de las columnas utilizando names()
names(data)

# Conocer los nombres de las columnas utilizando colnames()
colnames(data)

#*******************************************************************************************
##VARIABLE INDEPENDIENTE: "En qué sustancia se basarán tus respuestas"
##VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
#=====================================================================================
##Limpieza

# Quitamos instancias de error
# (Aquellas instancias que que han negado haber tenido un sentimiento de dicha y
#  que hayan respondido "Sí" o "No" en la siguiente pregunta, debido a que sólo debieron contestar
#  aquellas personas que afirmaron la respuesta anterior (haber tenido un sentimiento de dicha))

#SÓLO NOS QUEDAMOS CON LAS INSTANCIAS POSITIVAS (HABER DICHO QUE SÍ A LA SENSACIÓN DE DICHA)
#DEBIDO A QUE ES NUESTRO TEMA DE ESTUDIO

# Cargar la biblioteca dplyr
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 
         #&
           #`En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.` == "Sí")
#No es necesario el segundo filtro
         

# Nuevas dimensiones
dim(datos_filt)
# [102 233]

#Sólo 102 personas han contestado que han tenido una sensación de dicha tras su ingesta
#(pero no se ha afirmado, que haya sido por el uso de psicodélicos (siguiente pregunta))

#========================================================================================================================
##Limpieza

#VARIABLE SOBRE TIPO DE PSICODÉLICO EN EL QUE SE BASARÁ LA RESPUESTA
# Definir una función llamada "obtener_primera_sustancia" que toma un argumento "respuesta"
obtener_primera_sustancia <- function(respuesta) {
  # Dentro de la función, utilizar la función "strsplit" para dividir la cadena "respuesta" en un vector de substrings separados por ";"
  # El resultado es una lista con un elemento, que es el vector de substrings
  # Utilizar [[1]] para acceder al primer elemento de la lista (el vector de substrings)
  # Luego, utilizar [1] para acceder al primer elemento del vector de substrings (la primera sustancia)
  return(strsplit(respuesta, ";")[[1]][1])
}

#Nombre de las primeras 30 variables
names(datos_filt)[1:30]


# Aplicar la función "obtener_primera_sustancia" a cada elemento de la columna "¿En qué sustancia(s) psicodélica(s) se basarán tus respuestas?" del dataframe "df"
# Utilizar "sapply" para aplicar la función a cada elemento de la columna y obtener un vector con los resultados
# Sobrescribir los valores de la columna con los resultados
datos_filt$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..` <- sapply(datos_filt$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`, obtener_primera_sustancia)

# Mostrar los valores únicos de la columna "¿En qué sustancia(s) psicodélica(s) se basarán tus respuestas?" del dataframe "df"
# Utilizar "unique" para obtener un vector con los valores únicos de la columna
# Utilizar "print" para mostrar el resultado
print(unique(datos_filt$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`))

# Ver las frecuencias de los valores únicos (ya limpia la variable)
table(datos_filt$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`)

#Unir sustancias iguales que estén representadas de manera diferente (en este caso psilocibina)
library(stringr)

df_homolog <- datos_filt %>% 
  mutate(`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..` = str_replace(`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`, 
                                 "Psilocibina \\(hongos mágicos\\)", "Psilocibina")) %>%
  mutate(`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..` = str_replace(`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`, 
                                 "Psilocibina \\(Trufas\\)", "Psilocibina"))

table(df_homolog$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`)

# Sólo nos quedamos con las sustancias que tienen representaciones significativas (Para contraste de hipótesis Chi-cuadrado)
# Último ejemplo (final del apartado siguiente)
df_homolog_filtrado <- df_homolog %>% 
  filter(X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas.. %in% 
           c("Ayahuasca (mezcla de DMT y otras plantas)", "LSD (ácido lisérgico)", "Psilocibina"))

table(df_homolog_filtrado$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`)
#=========================================================================================
##Relación entre la variable dependiente
# (Individuos que han experimentado una sensación de dicha y ha sido por el uso de psicodélicos)

##CHI-CUADRADA (sirve para comprobar la independencia de dos variables, con base en la frecuencias entre los sucesos de ambas variables)

#Notas:
# 1. Da error para sucesos (valor) menores a 5 (n<5). Por lo tanto, se usa el contraste de hipótesis de Fisher (fisher.test())
# 2. Asigna probabilidades de ocurrencia iguales para cada valor dentro de la variable. Si se desea especificar probabilidades diferentes, se asigna un vecto con la probabilidad de cada elemento (valor o suceso) con el parámetro "p" 

#Hipótesis
#Ho: Son independientes
#Ha: Existe relación entre las variables

# "Supuestos"
# 1.- Muestreo Aleatorio [n>30 en cada Grupo]
# 2.- Variables Categóricas [Dicotómica Nominal]
# 3.- Ninguna Frecuencia esperada debe ser < 5.0


# Crear la tabla de contingencia
tabla_contingencia <- table(df_homolog$`En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.`, #Variable dependiente (personas que han dicho que si han experimentado sensación de dicha y que ha sido por el uso de psicodélicos)
                            df_homolog$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`) #Variable independiente
print(tabla_contingencia)


# Otra opción
y = df_homolog$`En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.`
x1 = df_homolog$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`

library(gmodels)

# Crea una tabla de contingencia cruzada entre las variables seleccionadas
gmodels::CrossTable(x= x1, 
           # La variable x es la variable de filas
           y= y, 
           # La variable y es la variable de columnas
           digits=3,
           # El número de dígitos significativos para mostrar en la tabla
           expected=FALSE, 
           # No mostrar las frecuencias esperadas en la tabla
           prop.c=FALSE,
           # No mostrar las proporciones de columna en la tabla
           prop.r=FALSE,
           # No mostrar las proporciones de fila en la tabla
           prop.t=FALSE,
           # No mostrar las proporciones totales en la tabla
           prop.chisq=FALSE, 
           # No mostrar la estadística de chi-cuadrado en la tabla
           format=c("SAS"))
# Formato de la tabla, en este caso se utiliza el formato de SAS

# Con algunos ajustes
CrossTable(x=x1, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))
#Como se mencionó, dado a que hay registros con un total de sucesos < 5, tenemos un error en Chi-cuadrado
#Hay que usar otro constraste de hipótesis para esta variable (FISHER).
#SI HAY SUCESOS CON FRECUENCIAS >5, PERO CON FRECUENCIAS ESPERADAS <5, UNA OPCIÓN ES UTILIZAR LA CORRECIÓN DE YALES, CON EL PARÁMETRO "correct = TRUE"

# Se realiza la prueba, aunque sabemos que dará error
# Realizar la prueba de chi-cuadrado
test_chi <- chisq.test(tabla_contingencia, correct = FALSE)

# Mostrar los resultados
print(test_chi)

# Valores esperados
test_chi$expected

# Valor observado
test_chi$observed


##FISHER

#Ho: No hay relación entre las variables
#Ha: Existe relación entre las variables

#Tenemos el error, por lo tanto, utilizamos Fisher
fisher.test(tabla_contingencia)


#ALTERNATIVA (para observar si mejora)
#Misma variable pero seleccionando los elementos más representativos
#Utilizando sólo los elementos con más valores de esta variable
tabla_contingencia_2 <- table(df_homolog_filtrado$`En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.`, #Variable dependiente (personas que han dicho que si han experimentado sensación de dicha y que ha sido por el uso de psicodélicos)
                            df_homolog_filtrado$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`) #Variable independiente
print(tabla_contingencia_2)

# Variables
y = df_homolog_filtrado$`En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.`
x_1 = df_homolog_filtrado$`X.En.qué.sustancia.s..psicodélica.s..se.basarán.tus.respuestas..`

# Visualizamos y aplicamos Chi-cuadrada
CrossTable(x=x_1, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

# Volvemos a corroborar
chisq.test(tabla_contingencia_2, correct = FALSE)

# Test de Fisher
fisher.test(tabla_contingencia_2)

#*********************************************************************************
#VARIABLE INDEPENDIENTE: "Edad" (en quintiles)
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"

#================================================================================
##Preprocesamiento

#1. Selección de instancias

#Únicamente obtenemos las instancias que cumplen con nuestra variable de estudio
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 

#2. Selección de variables
#DataFrame con la variable de estudio y la variable expuesta al contraste de independencia
library(dplyr)

d = datos_filt %>% 
  # selecciona solo los registros filtrados (datos_filt), que nos interesan para nuestro estudio
  select(
    # selecciona la columna "Edad"
    Edad, 
    # selecciona la columna "En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica."
    En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.
  )

dim(d)
View(d)

#3. Segmentamos por quintiles las edades
# Definir los límites de los quintiles y agrupar las edades
d <- d %>%
  # crea una nueva columna en el dataframe d llamada "EdadGrupo"
  mutate(
    EdadGrupo = 
      # utiliza la función cut para dividir la columna "Edad" en grupos
      cut(
        Edad, 
        # establece los límites de cada grupo (breaks)
        breaks = c(18, 25, 32, 39, 46, 55, Inf), 
        # asigna etiquetas a cada grupo
        labels = c("18-24", "25-31", "32-38", "39-45", "46-55", ">55"), 
        # indica que el límite inferior de cada grupo es inclusivo, es decir, que el valor del límite inferior se incluye en el grupo.
        right = FALSE
      )
  )

dim(d)
View(d)
# Crear un nuevo DataFrame solo con las columnas necesarias
d_n <- d %>%
  select(EdadGrupo, En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)

dim(d_n)
View(d_n)

#========================================================================================
##Relación entre las variables

##CHI-CUADRADA

#Hipótesis
#Ho: Son independientes
#Ha: Existe relación entre las variables

# "Supuestos"
# 1.- Muestreo Aleatorio [n>30 en cada Grupo] #No es obligatoria, pero si preferible (lo que si es obligatorio es, por lo menos 5, sino se usa el contraste de Fisher)
# 2.- Variables Categóricas [Dicotómica Nominal]
# 3.- Ninguna Frecuencia esperada debe ser < 5.0

t_cont_edad = table(d_n$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica., #Variable de interés
      d_n$EdadGrupo)

t_cont_edad
#Obtenemos valores con frecuencias < 5 (se usará el contraste de Fisher, pero aún así se ejecuta el ejercicio completo)


#Variables
y = d_n$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.
x_edad = d_n$EdadGrupo

# Visualizamos y aplicamos Chi-cuadrada
CrossTable(x=x_edad, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))
#Obtenemos valores esperados por debajo de 5.0

# Aplicamos directamente el contraste Chi-cuadrado
chisq.test(t_cont_edad, correct = FALSE)
# Con correción de Yates
# Volvemos a corroborar
chisq.test(t_cont_edad, correct = TRUE)
#Debido a la frecuencia en algunos de los conjuntos cruzados < 5


##FISHER

#Ho: No hay relación entre las variables
#Ha: Existe relación entre las variables

# Test de Fisher
fisher.test(t_cont_edad)
#NO SE PUEDE RECHAZAR LA Ho (p-value = 0.7051)


#*********************************************************************************
#VARIABLES SOBRE EXPERIENCIAS DURANTE LA INGESTA DE LA SUSTANCIA PSICODÉLICA
names(data)[60:84]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de variables
var_exp <- datos_filt %>% 
  select(names(datos_filt)[60:84], names(datos_filt)[218])

dim(var_exp)
View(var_exp)


#==========================================================================================
#Tablas de contingencia
# Última variable (dependiente) del DataFrame
ultima_variable <- names(var_exp)[ncol(var_exp)]

# Convertir la última variable a factor (si no lo está ya)
var_exp[[ultima_variable]] <- as.factor(var_exp[[ultima_variable]])

# Crear una lista para almacenar las tablas de contingencia y CrossTables
tablas_contingencia <- list()
cross_tables <- list()

library(gmodels)

# Bucle para generar tablas de contingencia y CrossTable
for (variable in names(var_exp)[-ncol(var_exp)]) {
  # Asegurarse de que la variable esté convertida a factor
  var_exp[[variable]] <- as.factor(var_exp[[variable]])
  
  # Generar tabla de contingencia
  tabla <- table(var_exp[[variable]], var_exp[[ultima_variable]])
  tablas_contingencia[[variable]] <- tabla
  
  # Aplicar CrossTable y almacenar los resultados en una lista
  cross_tables[[variable]] <- capture.output(CrossTable(x = var_exp[[variable]], y = var_exp[[ultima_variable]], 
                                                        digits = 3,
                                                        expected = TRUE, 
                                                        prop.c = FALSE,
                                                        prop.r = FALSE,
                                                        prop.t = FALSE,
                                                        prop.chisq = FALSE, 
                                                        format = c("SAS")))
}

# Visualizar todas las tablas de contingencia y CrossTables
for (variable in names(var_exp)[-ncol(var_exp)]) {
  cat("Tabla de contingencia para", variable, "vs", ultima_variable, "\n")
  print(tablas_contingencia[[variable]])
  cat("\nCrossTable for", variable, "vs", ultima_variable, "\n")
  cat(cross_tables[[variable]], sep = "\n")
  cat("\n\n")
}



#Función para visualizar rangos de tablas (no todas)
ver_tablas <- function(inicio, fin) {
  for (variable in names(var_exp)[inicio:fin]) {
    cat("Tabla de contingencia para", variable, "vs", ultima_variable, "\n")
    print(tablas_contingencia[[variable]])
    cat("\nCrossTable for", variable, "vs", ultima_variable, "\n")
    cat(cross_tables[[variable]], sep = "\n")
    cat("\n\n")
  }
}


#Primeras 10 tablas
ver_tablas(1, 10)

#Tablas de la 11 a la 20
ver_tablas(11, 20)

#Tablas de la 21 a la 25
ver_tablas(21, 25)

#*********************************************************************************
#VARIABLES SOBRE EXPERIENCIAS PSICODÉLICAS PERO QUE NO CUMPLEN CON EL VALOR ESPERADO >5
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
# Vector con los nombres de las 10 variables de interés (experiencias con <5 instancias esperadas)
variables <- c(
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.unión.con.la.naturaleza.o.el.universo.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.certeza.total.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.unidad.cósmica.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.ver.patrones.geométricos.o.fractales.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentirse.conectado.a.todos.los.seres.vivos.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentir.amor.o.agradecimiento.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentir.una.profunda.sensación.de.paz.o.serenidad.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentir.una.profunda.sensación.de.asombro.o.maravilla.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.sentir.una.profunda.intuición.o.revelación.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Sensación.de.alegría.o.felicidad."
)

# Variable de referencia
variable_referencia <- names(datos_filt)[218]

#==========================================================================================
#Tabla de contingencia

# Bucle para generar tablas de contingencia y aplicar el test de Fisher
for (variable in variables) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(datos_filt[[variable]], datos_filt[[variable_referencia]])
  
  # Aplicar el test de Fisher
  test_fisher <- fisher.test(tabla_contingencia)
  
  # Imprimir los resultados
  cat("Variable:", variable, "\n")
  print(tabla_contingencia)
  print(test_fisher)
  cat("\n\n")
}


#*********************************************************************************
#VARIABLE SOBRE ¿CUÁNTAS VECES HAS TENIDO EXPERIENCIAS PSICODÉLICAS?
names(data)[28]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_cuantas <- datos_filt %>% 
  select(names(datos_filt)[28], names(datos_filt)[218])

dim(var_cuantas)
View(var_cuantas)


#==========================================================================================
#Tabla de contingencia
x = var_cuantas$X.En.cuantas.ocasiones.has.tenido.experiencias.psicodélicas.
y = var_cuantas$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
  #Tabla de contingencia
t_contingencia = table(var_cuantas$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.,
                       var_cuantas$X.En.cuantas.ocasiones.has.tenido.experiencias.psicodélicas.)
print(t_contingencia)

  # Test de Fisher
fisher.test(t_contingencia)


#*********************************************************************************
#VARIABLE SOBRE CANTIDAD DE DOSIS
names(data)[32]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[32], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$X.Qué.tamaño..porción.o.cantidad.de.dosis.sientes.que.consumiste.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$X.Qué.tamaño..porción.o.cantidad.de.dosis.sientes.que.consumiste.,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)

# Test de Fisher
fisher.test(t_contingencia)


#*********************************************************************************
#VARIABLE SOBRE COMPORTAMIENTO PREPRARATORIO AL USO
names(data)[39]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[39], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$X.Te.acompañó.un.guía.o.cuidador.a..durante.su.experiencia.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$X.Te.acompañó.un.guía.o.cuidador.a..durante.su.experiencia.,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)

# Test de Fisher
fisher.test(t_contingencia)

#*********************************************************************************
#VARIABLE SOBRE ¿CANTIDAD DE DOSIS?
names(data)[45]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[45], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$X.Tuviste.algún.comportamiento.preparatorio...mental..físico..emocional..espiritual...antes.de.la.experiencia.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$X.Tuviste.algún.comportamiento.preparatorio...mental..físico..emocional..espiritual...antes.de.la.experiencia.,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)

# Test de Fisher
fisher.test(t_contingencia)

#*********************************************************************************
#VARIABLES SOBRE FENÓMENOS SENSORIALES
names(data)[50:58]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de variables
var_feno <- datos_filt %>% 
  select(names(datos_filt)[50:58], names(datos_filt)[218])

dim(var_feno)
View(var_feno)


#==========================================================================================
#Tablas de contingencia
# Última variable (dependiente) del DataFrame
ultima_variable <- names(var_feno)[ncol(var_feno)]

# Convertir la última variable a factor (si no lo está ya)
var_feno[[ultima_variable]] <- as.factor(var_feno[[ultima_variable]])

# Crear una lista para almacenar las tablas de contingencia y CrossTables
tablas_contingencia <- list()
cross_tables <- list()

library(gmodels)

# Bucle para generar tablas de contingencia y CrossTable
for (variable in names(var_feno)[-ncol(var_feno)]) {
  # Asegurarse de que la variable esté convertida a factor
  var_feno[[variable]] <- as.factor(var_feno[[variable]])
  
  # Generar tabla de contingencia
  tabla <- table(var_feno[[variable]], var_feno[[ultima_variable]])
  tablas_contingencia[[variable]] <- tabla
  
  # Aplicar CrossTable y almacenar los resultados en una lista
  cross_tables[[variable]] <- capture.output(CrossTable(x = var_feno[[variable]], y = var_feno[[ultima_variable]], 
                                                        digits = 3,
                                                        expected = TRUE, 
                                                        prop.c = FALSE,
                                                        prop.r = FALSE,
                                                        prop.t = FALSE,
                                                        prop.chisq = FALSE, 
                                                        format = c("SAS")))
}

# Visualizar todas las tablas de contingencia y CrossTables
for (variable in names(var_feno)[-ncol(var_feno)]) {
  cat("Tabla de contingencia para", variable, "vs", ultima_variable, "\n")
  print(tablas_contingencia[[variable]])
  cat("\nCrossTable for", variable, "vs", ultima_variable, "\n")
  cat(cross_tables[[variable]], sep = "\n")
  cat("\n\n")
}



#Función para visualizar rangos de tablas (no todas)
ver_tablas <- function(inicio, fin) {
  for (variable in names(var_feno)[inicio:fin]) {
    cat("Tabla de contingencia para", variable, "vs", ultima_variable, "\n")
    print(tablas_contingencia[[variable]])
    cat("\nCrossTable for", variable, "vs", ultima_variable, "\n")
    cat(cross_tables[[variable]], sep = "\n")
    cat("\n\n")
  }
}


#Las 9 variables
ver_tablas(1, 9)


#Fisher
# Variable de referencia
variable_referencia <- names(datos_filt)[218]

# Selección de variables específicas de 1, 2, 3, 6 y 7
variables_seleccionadas <- names(var_feno)[c(1, 2, 3, 6, 7)]

# Bucle para generar tablas de contingencia y aplicar el test de Fisher
for (variable in variables_seleccionadas) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(datos_filt[[variable]], datos_filt[[variable_referencia]])
  
  # Aplicar el test de Fisher
  test_fisher <- fisher.test(tabla_contingencia)
  
  # Imprimir los resultados
  cat("Variable:", variable, "\n")
  print(tabla_contingencia)
  print(test_fisher)
  cat("\n\n")
}

#*********************************************************************************
#VARIABLE SOBRE NIVEL DE SATISFACCIÓN CON TU VIDA ANTES DE LA(S) EXPERIENCIA(S)
names(data)[136]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[136], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$Nivel.de.satisfacción.con.tu.vida..antes.de.la.s..experiencia.s..psicodélica.s.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$Nivel.de.satisfacción.con.tu.vida..antes.de.la.s..experiencia.s..psicodélica.s.,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)

# Test de Fisher
fisher.test(t_contingencia)


#*********************************************************************************
#VARIABLES SOBRE EXPERIENCIAS PREVIAS
names(data)[143:146]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de variables
var_exp_prev <- datos_filt %>% 
  select(names(datos_filt)[143:146], names(datos_filt)[218])

dim(var_exp_prev)
View(var_exp_prev)


#==========================================================================================
#Tablas de contingencia
# Última variable (dependiente) del DataFrame
ultima_variable <- names(var_exp_prev)[ncol(var_exp_prev)]

# Convertir la última variable a factor (si no lo está ya)
var_exp_prev[[ultima_variable]] <- as.factor(var_exp_prev[[ultima_variable]])

# Crear una lista para almacenar las tablas de contingencia y CrossTables
tablas_contingencia <- list()
cross_tables <- list()

library(gmodels)

# Bucle para generar tablas de contingencia y CrossTable
for (variable in names(var_exp_prev)[-ncol(var_exp_prev)]) {
  # Asegurarse de que la variable esté convertida a factor
  var_exp_prev[[variable]] <- as.factor(var_exp_prev[[variable]])
  
  # Generar tabla de contingencia
  tabla <- table(var_exp_prev[[variable]], var_exp_prev[[ultima_variable]])
  tablas_contingencia[[variable]] <- tabla
  
  # Aplicar CrossTable y almacenar los resultados en una lista
  cross_tables[[variable]] <- capture.output(CrossTable(x = var_exp_prev[[variable]], y = var_exp_prev[[ultima_variable]], 
                                                        digits = 3,
                                                        expected = TRUE, 
                                                        prop.c = FALSE,
                                                        prop.r = FALSE,
                                                        prop.t = FALSE,
                                                        prop.chisq = FALSE, 
                                                        format = c("SAS")))
}

# Visualizar todas las tablas de contingencia y CrossTables
for (variable in names(var_exp_prev)[-ncol(var_exp_prev)]) {
  cat("Tabla de contingencia para", variable, "vs", ultima_variable, "\n")
  print(tablas_contingencia[[variable]])
  cat("\nCrossTable for", variable, "vs", ultima_variable, "\n")
  cat(cross_tables[[variable]], sep = "\n")
  cat("\n\n")
}

#*********************************************************************************
#VARIABLE SOBRE NIVEL DE SATISFACCIÓN CON TU VIDA DESPUÉS DE LA(S) EXPERIENCIA(S)
names(data)[152]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[152], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$Nivel.de.satisfacción.con.tu.vida.después.la.experiencia.psicodélica
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$Nivel.de.satisfacción.con.tu.vida.después.la.experiencia.psicodélica,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)


# Test de Fisher
fisher.test(t_contingencia)

#*********************************************************************************
#VARIABLES SOBRE CAMBIOS GENERALES PERCIBIDOS
names(data)[153:160]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de variables
var_cambios <- datos_filt %>% 
  select(names(datos_filt)[153:160], names(datos_filt)[218])

dim(var_cambios)
View(var_cambios)


#==========================================================================================
#Tablas de contingencia
# Última variable (dependiente) del DataFrame
ultima_variable <- names(var_cambios)[ncol(var_cambios)]

# Convertir la última variable a factor (si no lo está ya)
var_cambios[[ultima_variable]] <- as.factor(var_cambios[[ultima_variable]])

# Crear una lista para almacenar las tablas de contingencia y CrossTables
tablas_contingencia <- list()
cross_tables <- list()

library(gmodels)

# Bucle para generar tablas de contingencia y CrossTable
for (variable in names(var_cambios)[-ncol(var_cambios)]) {
  # Asegurarse de que la variable esté convertida a factor
  var_cambios[[variable]] <- as.factor(var_cambios[[variable]])
  
  # Generar tabla de contingencia
  tabla <- table(var_cambios[[variable]], var_cambios[[ultima_variable]])
  tablas_contingencia[[variable]] <- tabla
  
  # Aplicar CrossTable y almacenar los resultados en una lista
  cross_tables[[variable]] <- capture.output(CrossTable(x = var_cambios[[variable]], y = var_cambios[[ultima_variable]], 
                                                        digits = 3,
                                                        expected = TRUE, 
                                                        prop.c = FALSE,
                                                        prop.r = FALSE,
                                                        prop.t = FALSE,
                                                        prop.chisq = FALSE, 
                                                        format = c("SAS")))
}

# Visualizar todas las tablas de contingencia y CrossTables
for (variable in names(var_cambios)[-ncol(var_cambios)]) {
  cat("Tabla de contingencia para", variable, "vs", ultima_variable, "\n")
  print(tablas_contingencia[[variable]])
  cat("\nCrossTable for", variable, "vs", ultima_variable, "\n")
  cat(cross_tables[[variable]], sep = "\n")
  cat("\n\n")
}

#Fisher
# Vector con los nombres de las 10 variables de interés (experiencias con <5 instancias esperadas)
variables <- c(
  "Por.favor..responde.los.siguientes.rubros..Bienestar.general.",
  "Por.favor..responde.los.siguientes.rubros..Calidad.del.sueño.",
  "Por.favor..responde.los.siguientes.rubros..Estabilidad.del.estado.de.ánimo.",
  "Por.favor..responde.los.siguientes.rubros..Relaciones.sociales..conexión.con.los.demás..",
  "Por.favor..responde.los.siguientes.rubros..Significado.existencial.",
  "Por.favor..responde.los.siguientes.rubros..Autoconciencia.e.introspección.",
  "Por.favor..responde.los.siguientes.rubros..Presencia.y.Mindfulness..atención.plena..en.la.vida.cotidiana.",
  "Por.favor..responde.los.siguientes.rubros..Empatía.y.compasión."
  )

# Variable de referencia
variable_referencia <- names(datos_filt)[218]

#Tabla de contingencia

# Bucle para generar tablas de contingencia y aplicar el test de Fisher
for (variable in variables) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(datos_filt[[variable]], datos_filt[[variable_referencia]])
  
  # Aplicar el test de Fisher
  test_fisher <- fisher.test(tabla_contingencia)
  
  # Imprimir los resultados
  cat("Variable:", variable, "\n")
  print(tabla_contingencia)
  print(test_fisher)
  cat("\n\n")
}

#*********************************************************************************
#VARIABLE SOBRE EL CAMBIO CON LA CONEXIÓN CON LA NATURALEZA DESPUÉS DEL USO DE PSICODÉLICOS
names(data)[168]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[168], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$X.Cambió.tu.conexión.con.la.naturaleza.después.de.la.experiencia.psicodélica.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$X.Cambió.tu.conexión.con.la.naturaleza.después.de.la.experiencia.psicodélica.,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)


# Test de Fisher
fisher.test(t_contingencia)

#*********************************************************************************
#VARIABLE SOBRE EL RECUERDO (FLASHBACK) DE LA EXPERIENCIA
names(data)[208]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[208], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$X.Experimentaste.algún.tipo.de.flashback.o.recuerdo.de.la.experiencia.psicodélica.semanas.o.meses.después.de.la.ingesta.de.psicodélicos.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))


#*********************************************************************************
#VARIABLE SOBRE LA TOMA DE UNA DECISIÓN IMPORTANTE POSTERIOR AL USO DE PSICODÉLICOS
names(data)[213]
#VARIABLE DE ESTUDIO: "SENSACIÓN DE DICHA POR EL USO DE PSICODÉLICOS"
names(data)[218]
#==================================================================================
#Preprocesamiento
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 


#1. Selección de la variable independiente y la dependiente
var_ <- datos_filt %>% 
  select(names(datos_filt)[213], names(datos_filt)[218])

dim(var_)
View(var_)


#==========================================================================================
#Tabla de contingencia
x = var_$X.Tu.experiencia.psicodélica.te.llevó.a.algún.cambio.o.decisión.importante.en.la.vida.
y = var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.

#Chi-cuadrada  
CrossTable(x=x, y=y, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Fisher
#Tabla de contingencia
t_contingencia = table(var_$X.Tu.experiencia.psicodélica.te.llevó.a.algún.cambio.o.decisión.importante.en.la.vida.,
                       var_$En.caso.de.que.tu.respuestas.anterior.haya.sido..Sí....consideras.que.fue.por.la.experiencia.psicodélica.)
print(t_contingencia)


# Test de Fisher
fisher.test(t_contingencia)
