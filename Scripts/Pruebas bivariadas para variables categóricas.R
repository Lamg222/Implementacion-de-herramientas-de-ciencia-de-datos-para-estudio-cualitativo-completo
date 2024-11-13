#Analisis_Estadistico_Bivariado

#Establecer Directorio
#Para descargar la Base de Datos - https://iinei.inei.gob.pe/microdatos/
#Para revisar la Codificación - http://webinei.inei.gob.pe/anda_inei/index.php/catalog/656/datafile/F1

#####Aperturando_data####
install.packages("haven")
install.packages("foreign")

library(haven)
library(foreign)

Data=read.spss("C:/Users/Intimayta1/Desktop/LGBTI/602-Modulo1287.sav", to.data.frame=T, use.value.labels=FALSE)   
View(Data)

#p101 - nivel de educación
#p104 - afiliado a seguro
#p105 - problema de salud 
#p112 - sexo al nacer
#p113 - orientacion sexual
#p114 - identidad de genero
#p116 - expresiones libre
#p118 - familia sabe
#p122 - pareja
#p201 - discriminacion
#edad
#lima
#hacinamiento

Data_F <- subset(Data, select = c(p101, p104, p105, p112, p113, p114, p116, p118, p122, p201, edad, lima, hacinamiento))

#Limpiando_data
Data_F[Data_F == 99] <- ""
Data_F[Data_F == 9] <- ""

####Estructurando_Datos####

Data_F <- Data_F[!is.na(Data_F$p101),]
Data_F <- Data_F[!is.na(Data_F$p104),]

#Grado de Educación
Data_F$"p101" <- factor(Data_F$"p101",
                        levels = c(1,2,3,4,5,6,7,8,9,10,11),
                        labels = c("Sin Educación",
                                   "Educación Inicial",
                                   "Primaria Incompleta",
                                   "Primaria Completa",
                                   "Secundaria Incompleta",
                                   "Secundaria Completa",
                                   "Educación Tecnica Incompleta",
                                   "Educación Tecnica Completa",
                                   "Educación Universitaria Incompleta",
                                   "Educación Universitaria Completa",
                                   "Postgrado")) 

#Seguro de salud
Data_F$"p104" <- factor(Data_F$"p104",
                        levels = c(1,2),
                        labels = c("Si","No")) 

#Problema de salud 
Data_F$"p105" <- factor(Data_F$"p105",
                        levels = c(1,2),
                        labels = c("Si","No")) 

#hacinamiento
Data_F$"hacinamiento" <- factor(Data_F$"hacinamiento",
                                levels = c(0,1),
                                labels = c("Si","No")) 

#lima
Data_F$"lima" <- factor(Data_F$"lima",
                        levels = c(26,27),
                        labels = c("Lima","Otras Regiones")) 

#p201 - discriminacion
Data_F$"p201" <- factor(Data_F$"p201",
                        levels = c(1,2),
                        labels = c("Si","No")) 

#p112 - sexo                                  
Data_F$"p112" <- factor(Data_F$"p112",
                        levels = c(1,2),
                        labels = c("Hombre","Mujer"))

#p113 - orientacion sexual
Data_F$"p113" <- factor(Data_F$"p113",
                        levels = c(1,2,3,4,5,6),
                        labels = c("Heterosexual",
                                   "Homosexual",
                                   "Lesbiana",
                                   "Bisexual",
                                   "Pansexual",
                                   "Asexual")) 

#p114 - identidad de genero
Data_F$"p114" <- factor(Data_F$"p114",
                        levels = c(1,2,3,4,5),
                        labels = c("Transexual Femenino",
                                   "Transexual Masculino",
                                   "No Binario",
                                   "No Transexual",
                                   "Otro")) 

#p116 - expresiones libre
Data_F$"p116" <- factor(Data_F$"p116",
                        levels = c(1,2),
                        labels = c("Si","No")) 

#p118 - familia sabe
Data_F$"p118" <- factor(Data_F$"p118",
                        levels = c(1,2,3),
                        labels = c("Si","No","No lo se"))

#p122 - pareja
Data_F$"p122" <- factor(Data_F$"p122",
                        levels = c(1,2,3,4),
                        labels = c("Sin pareja",
                                   "No convive con pareja",
                                   "Convive con pareja",
                                   "Muchas parejas"))

#Edad
Data_F$edad <- as.numeric(Data_F$edad)

#Resumiendo Variables
table(Data_F$p101)
summary(Data_F$edad)


###PRUEBAS BIVARIADAS ENTRE VARIABLES CATEGORICAS###

#Instalamos los Paquetes
install.packages("crosstable")
install.packages("gmodels")

library("crosstable")
library("gmodels")

#Separamos las Variables
Seguro <- Data_F$p104
Enfermedad <- Data_F$p105 

####TEST DE CHI-CUADRADO DE PEARSON####

"SUPUESTOS"
1.- Muestreo Aleatorio [n>30 en cada Grupo]
2.- Variables Categóricas [Dicotómica Nominal]
3.- Ninguna Frecuencia esperada debe ser < 5.0

"Estructura de Hipotesis"

H1: Existe diferencia entre las proporciones (Proporcion A = Proporcion B)
H0: No existe diferencia entre las proporciones (Proporcion A ≠ Proporcion B)

#Tabla Descriptiva
CrossTable(x=Seguro, y=Enfermedad, 
           digits=3,
           expected=TRUE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))

#Prueba de Chi-Cuadrado Sin Correcion de Yates
chisq.test(x=Seguro, y=Enfermedad, correct = FALSE)

#Prueba de Chi-Cuadrado Con Correcion de Yates
chisq.test(x=Seguro, y=Enfermedad, correct = TRUE) 


#Cuidado con Algunas Situaciones

#Separamos las Variables
Educacion <- Data_F$p101
Hacinamiento <- Data_F$hacinamiento

#Tabla Descriptiva
CrossTable(x=Educacion, y=Hacinamiento, 
           digits=3,
           expected=FALSE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           format=c("SAS"))


####TEST EXACTO DE FISHER####

"SUPUESTOS"
1.- Muestreo Aleatorio [n>30 en cada Grupo]
2.- Variables Categóricas [Dicotómica Nominal]
3.- No importa las Frecuencias esperadas

"Estructura de Hipotesis"

H1: Existe diferencia en la posibilidad que ocurra un evento (Posibilidad A = Posibilidad B)
H0: No existe diferencia en la posibilidad que ocurra un evento (Posibilidad A ≠ Posibilidad B)

#Evaluacion de Supuestos
CrossTable(x=Seguro, y=Enfermedad, 
           digits=3, 
           expected=FALSE, 
           prop.c=FALSE,
           prop.r=FALSE,
           prop.t=FALSE,
           prop.chisq=FALSE, 
           chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
           format=c("SAS"))

#Prueba Exacta de Fisher
fisher.test(table(Seguro, Enfermedad))


###PRUEBAS BIVARIADAS ENTRE VARIABLE CATEGORICA Y VARIABLE NUMÉRICA###


####TEST DE T DE STUDENT####


"SUPUESTOS"
1.- Muestreo Aleatorio [n>30 en cada Grupo]
2.- Variable Numérica con Distribución Normal
3.- Homogeneidad de Varianzas [Prueba F]

"Estructura de Hipotesis"

H1: Existe diferencia entre los promedios (Promedio de A = Promedio de B)
H0: No existe diferencia entre los promedios (Promedio de A ≠ Promedio de B)


#Generar Variable Numerica Economicos Mensuales
Data_F$Ingresos <- rnorm(12026, mean=1200, sd=250)

#Evaluacion de Supuestos

"EVALUACION DE NORMALIDAD"

#Separar Variable Numerica
Ingresos <- Data_F$Ingresos

#Metodo Grafico para Evaluar Forma de Campana de Gauss
Densidad <- seq(min(Ingresos), max(Ingresos), length = 40)
Distribucion <- dnorm(Densidad, mean = mean(Ingresos), sd = sd(Ingresos))

hist(Ingresos, prob = TRUE, 
     ylim = c(0, max(Distribucion)),
     xlab = "Ingresos Economicos",
     ylab = "",
     yaxt = "n",
     main = "Histograma con Curva de Densidad")

lines(density(Ingresos), col = "red", lwd = 2)

#Metodo Estadistico para Evaluar Similitud de Media y Mediana
summary(Ingresos)

#Metodo de Prueba Estadistica para Normalidad

"Prueba de Shapiro-Wilk"
shapiro.test(Ingresos)
---> "Esta prueba solo funciona para grupo con 3 o 5000 integrantes"

"Generando Variable Parcial"
Ingresos_Nuevo <- rnorm(3000, mean=1200, sd=250)

shapiro.test(Ingresos_Nuevo)
--> "Si el valor de p es mayor a 0.050 la variable tiene una distribucion normal"

"Prueba de Kolmogorov-Smirnov"
  ks.test(Ingresos, 'pnorm')
--> "e- es una anotación científica que muestra como el valor es cercano a cero"
--> "Si el valor de p es mayor a 0.050 la variable tiene una distribucion normal"

"HOMOGENEIDAD DE VARIANZAS"

"Prueba F para Homogeneidad de Varianzas"
var.test(Ingresos ~ Seguro, data = Data_F)
--> "Si el valor de p es mayor a 0.050 no hay heterogeneidad entre las varianzas"


#Prueba Estadistica de T de Student
t.test(Ingresos~Seguro, var.equal = TRUE)

#Prueba Estadistica del Test de Welch
t.test(Ingresos~Seguro, var.equal = FALSE)

####TEST DE U DE MANN-WHITNEY####

"SUPUESTOS"
1.- Muestreo Aleatorio [n ≤ 30 en cada Grupo]
2.- Variable Numérica sin Distribución Normal [o Variable Ordinal]
3.- No Importa la Heterogeneidad de Varianzas

"Estructura de Hipotesis"

H1: Existe diferencia entre las medianas (Mediana de A = Mediana de B)
H0: No existe diferencia entre las medianas (Mediana de A ≠ Mediana de B)


"EVALUACION DE NORMALIDAD"

#Separar Variable Numerica
Edad <- Data_F$edad

#Metodo Grafico para Evaluar Forma de Campana de Gauss

hist(Edad, prob = TRUE, 
     xlab = "Edad de los Encuestados",
     ylab = "",
     yaxt = "n",
     main = "Histograma")

#Metodo Estadistico para Evaluar Similitud de Media y Mediana
summary(Edad)

IQR(Data$edad)
range(Data$edad)

#Prueba Estadistica de U de Mann-Whitney

"Seleccionar Variable Discriminacion"
Discriminacion <- Data_F$p201

"Codigo para Prueba Estadistica"
wilcox.test(Edad ~ Discriminacion, data = Data_F, exact = FALSE)

