##Importamos csv
# Directorio
setwd("C:/Users/alber/Jupyter Notebooks/Cuadernos/Maestría en Ciencia de Datos/TFM")

# Importar el archivo CSV
data <- read.csv("CEFE.csv")

#=====================================================================================
#1. Selección de instancias

# Cargar la biblioteca dplyr
library(dplyr)

# Filtrar los registros que han respondido "sí"
datos_filt <- data %>%
  filter(`Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica.` == "Sí") 

#=====================================================================================
#2. Selección de variables

#2.1. Variables con nivel de significancia estadística para contraste de hipótesis Chi-cuadrada
# Crear el data frame d_chi con las variables especificadas
d_chi <- datos_filt %>%
  select(
    exper_perdida_si_mismo = X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.pérdida.de.sí.mismo.o.de.alguna.creencia.fuertemente.vinculada.con.tu.personalidad.,
    practica_yoga_previa = Por.favor..responde.las.siguientes.preguntas...Tuviste.alguna.experiencia.previa.con.prácticas.de.yoga..,
    vivencia_mistica_previa = Por.favor..responde.las.siguientes.preguntas...Experimentaste.alguna.vez.un.acontecimiento.místico..,
    exper_mov_involuntarios = X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Movimientos.involuntarios.,
    exper_muerte = X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.la.muerte.,
    practica_respiracion_previa = Por.favor..responde.las.siguientes.preguntas...Tuviste.experiencias.previas.con.prácticas.de.respiración..
  )

# Agregar la variable dependiente
d_chi$variable_dependiente <- datos_filt[[218]]


#=============================================================================================================================================
#Tablas de contingencia
require(gmodels)
# Crear una lista para almacenar los grados de relación
grados_relacion <- list()

# Bucle para crear las tablas de contingencia y aplicar la prueba Chi-cuadrado
for (variable in colnames(d_chi)[-length(colnames(d_chi))]) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(d_chi[[variable]], d_chi$variable_dependiente)
  
  # Aplicar la prueba Chi-cuadrado
  test_chi <- chisq.test(tabla_contingencia)
  
  # Guardar el grado de relación
  #grados_relacion[[variable]] <- test_chi$statistic
  
  # Crear el gráfico de la tabla de contingencia
  CrossTable(d_chi[[variable]], d_chi$variable_dependiente, 
             digits=3,
             expected=TRUE, 
             prop.c=FALSE,
             prop.r=FALSE,
             prop.t=FALSE,
             prop.chisq=FALSE, 
             format=c("SAS"))
}


#Grado de relación (V de Cramer)
grados_relacion <- list()

for (variable in colnames(d_chi)[-length(colnames(d_chi))]) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(d_chi[[variable]], d_chi$variable_dependiente)
  
  # Grado de relación
  grados_relacion[[variable]] <- vcd::assocstats(tabla_contingencia)$cramer
  
}
  
for (variable in colnames(d_chi)[-length(colnames(d_chi))]) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(d_chi[[variable]], d_chi$variable_dependiente)
  
  # Grado de relación
  grados_relacion[[variable]] <- vcd::assocstats(tabla_contingencia)$cramer
  
  print(paste("Comparación entre:", variable, "vs", "Variable dependiente del estudio"))
  print(paste("Grado de relación:", grados_relacion[[variable]]))
  cat(rep("*", 48))
}  
#===================================================================================
#Grafo
library(igraph)

# Crear un grafo con los grados de relación
nodos <- c(names(grados_relacion), "variable_dependiente")
enlaces <- data.frame(
  from = rep("variable_dependiente", length(grados_relacion)),
  to = names(grados_relacion),
  weight = unlist(grados_relacion)
)

# Crear el objeto igraph
grafo <- graph_from_data_frame(d = enlaces, vertices = nodos, directed = FALSE)

# Asignar colores a los nodos
V(grafo)$color <- ifelse(V(grafo)$name == "variable_dependiente", "red", "blue")

# Plotear el grafo
png(filename = "grafo_relaciones.png", width = 1800, height = 1600)
plot(grafo, edge.width = E(grafo)$weight, vertex.label.color = "black", vertex.size = 15)
dev.off()


print(grados_relacion)
