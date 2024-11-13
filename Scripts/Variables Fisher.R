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

#2.1. Variables con nivel de significancia estadística para contraste de hipótesis Fisher
# Crear el data frame d_fisher con las variables especificadas
# Selección y renombramiento de columnas
datos_nuevos <- datos_filt %>%
  select(
    Cambio_Naturaleza = X.Cambió.tu.conexión.con.la.naturaleza.después.de.la.experiencia.psicodélica.,
    Cambio_Decision = X.Tu.experiencia.psicodélica.te.llevó.a.algún.cambio.o.decisión.importante.en.la.vida.,
    Comportamiento_Preparatorio = X.Tuviste.algún.comportamiento.preparatorio...mental..físico..emocional..espiritual...antes.de.la.experiencia.,
    Satisfaccion_Vida = Nivel.de.satisfacción.con.tu.vida..antes.de.la.s..experiencia.s..psicodélica.s.,
    Estabilidad_Animo = Por.favor..responde.los.siguientes.rubros..Estabilidad.del.estado.de.ánimo.,
    Relaciones_Sociales = Por.favor..responde.los.siguientes.rubros..Relaciones.sociales..conexión.con.los.demás..,
    Empatia_Compasion = Por.favor..responde.los.siguientes.rubros..Empatía.y.compasión.
    )


# Agregar la variable dependiente
datos_nuevos$variable_dependiente <- datos_filt[[218]]


#====================================================================
#Hipótesis del test de Fisher:
  
#Hipótesis nula (H0): No hay asociación entre las dos variables; cualquier asociación observada es puramente por azar.
#Hipótesis alternativa (H1): Existe una asociación significativa entre las dos variables.

# Crear una lista para almacenar los resultados
resultados <- list()

# Bucle para realizar la prueba de Fisher para cada variable independiente
for (columna in names(datos_nuevos)[-length(names(datos_nuevos))]) {  # Todas las columnas excepto la última (Variable_Dependiente)
  tabla_contingencia <- table(datos_nuevos[[columna]], datos_nuevos$variable_dependiente)
  resultado_fisher <- fisher.test(tabla_contingencia, alternative = "two.sided")
  resultados[[columna]] <- list(Odds_Ratio = resultado_fisher$estimate, P_Value = resultado_fisher$p.value)
  
  # Imprimir los resultados
  cat("Variable:", columna, "\n")
  print(tabla_contingencia)
  print(resultado_fisher)
  cat("\n\n")
}

# Mostrar los resultados (otra opción)
for (variable in names(resultados)) {
  print(paste(variable, "- Odds Ratio:", resultados[[variable]]$Odds_Ratio, ", P-Value:", resultados[[variable]]$P_Value))
}

#Grado de relación (V de Cramer)
grados_relacion <- list()


for (variable in colnames(datos_nuevos)[-length(colnames(datos_nuevos))]) {
  # Crear la tabla de contingencia
  tabla_contingencia <- table(datos_nuevos[[variable]], datos_nuevos$variable_dependiente)
  
  # Grado de relación
  grados_relacion[[variable]] <- vcd::assocstats(tabla_contingencia)$cramer
  
  print(paste("Comparación entre:", variable, "vs", "Variable dependiente"))
  print(paste("Grado de relación:", grados_relacion[[variable]]))
}  

