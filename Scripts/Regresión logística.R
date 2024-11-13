## Importamos csv
# Directorio
setwd("C:/Users/alber/Jupyter Notebooks/Cuadernos/Maestría en Ciencia de Datos/TFM")

# Importar el archivo CSV
data <- read.csv("CEFE.csv")

#============================
# Librerías
library(dplyr)
library(MASS)
library(car)
library(caret)
library(pscl)  # Para calcular el pseudo-R2

#============================
# Selección de variables
datos_filt <- data %>%
  filter(Sentido.de.la.dicha....Has.experimentado.una.profunda.sensación.de.dicha.o.alegría.de.tu.vida.después.de.tu.experiencia.psicodélica. == "Sí") # Filtrar los registros que han respondido "sí"

# Variables con niveles estadísticamente significativos de dependencia
# Seleccionar las variables
datos = datos_filt[, c(
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.pérdida.de.sí.mismo.o.de.alguna.creencia.fuertemente.vinculada.con.tu.personalidad.",
  "Por.favor..responde.las.siguientes.preguntas...Tuviste.alguna.experiencia.previa.con.prácticas.de.yoga..",
  "Por.favor..responde.las.siguientes.preguntas...Experimentaste.alguna.vez.un.acontecimiento.místico..",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Movimientos.involuntarios.",
  "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.la.muerte.",
  "Por.favor..responde.las.siguientes.preguntas...Tuviste.experiencias.previas.con.prácticas.de.respiración..",
  "X.Cambió.tu.conexión.con.la.naturaleza.después.de.la.experiencia.psicodélica.",
  "X.Tu.experiencia.psicodélica.te.llevó.a.algún.cambio.o.decisión.importante.en.la.vida.",
  "Nivel.de.satisfacción.con.tu.vida..antes.de.la.s..experiencia.s..psicodélica.s.",
  "Por.favor..responde.los.siguientes.rubros..Estabilidad.del.estado.de.ánimo.",
  "Por.favor..responde.los.siguientes.rubros..Relaciones.sociales..conexión.con.los.demás..",
  "Por.favor..responde.los.siguientes.rubros..Empatía.y.compasión."
)]

#============================
# Preprocesamiento
# Convertir las variables seleccionadas a factores y luego a variables ordinales
datos <- datos %>%
  mutate(across(c(
    "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.pérdida.de.sí.mismo.o.de.alguna.creencia.fuertemente.vinculada.con.tu.personalidad.",
    "Por.favor..responde.las.siguientes.preguntas...Tuviste.alguna.experiencia.previa.con.prácticas.de.yoga..",
    "Por.favor..responde.las.siguientes.preguntas...Experimentaste.alguna.vez.un.acontecimiento.místico..",
    "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Movimientos.involuntarios.",
    "X.Se.produjeron.estas.experiencias.durante.la.experiencia.psicodélica...Experiencia.de.la.muerte.",
    "Por.favor..responde.las.siguientes.preguntas...Tuviste.experiencias.previas.con.prácticas.de.respiración.."
  ), as.factor))

# Renombrar las columnas
colnames(datos) <- c(
  "exper_perdida_si_mismo",
  "practica_yoga_previa",
  "vivencia_mistica_previa",
  "exper_mov_involuntarios",
  "exper_muerte",
  "practica_respiracion_previa",
  "Cambio_Naturaleza",
  "Cambio_Decision",
  "Satisfaccion_Vida_Prev",
  "Estabilidad_Animo",
  "Relaciones_Sociales",
  "Empatia_Compasion"
)

# Convertir las variables dicotómicas a 0 y 1
datos <- datos %>%
  mutate(
    Cambio_Decision = as.numeric(Cambio_Decision == "Sí"),
    practica_yoga_previa = as.numeric(practica_yoga_previa == "Sí"),
    vivencia_mistica_previa = as.numeric(vivencia_mistica_previa == "Sí"),
    practica_respiracion_previa = as.numeric(practica_respiracion_previa == "Sí"),
    exper_perdida_si_mismo = as.numeric(exper_perdida_si_mismo == "Sí"),
    exper_mov_involuntarios = as.numeric(exper_mov_involuntarios == "Sí"),
    exper_muerte = as.numeric(exper_muerte == "Sí")
  )

# Convertir las variables ordinales
datos <- datos %>%
  mutate(
    Cambio_Naturaleza = ordered(Cambio_Naturaleza, levels = c(
      "No, mi conexión con la naturaleza sigue siendo la misma",
      "Sí, cambió muy sutilmente de una manera positiva",
      "Sí, cambió de forma positiva",
      "Sí, ha cambiado mucho en sentido positiva."
    )),
    Estabilidad_Animo = ordered(Estabilidad_Animo, levels = c(
      "Empeorado",
      "Sin cambios",
      "Mejora moderada",
      "Mejora significativa"
    )),
    Relaciones_Sociales = ordered(Relaciones_Sociales, levels = c(
      "Empeorado",
      "Sin cambios",
      "Mejora moderada",
      "Mejora significativa"
    )),
    Empatia_Compasion = ordered(Empatia_Compasion, levels = c(
      "Sin cambios",
      "Mejora moderada",
      "Mejora significativa"
    )),
    Satisfaccion_Vida_Prev = ordered(Satisfaccion_Vida_Prev, levels = c(
      "Muy insatisfecho(a)",
      "Insatisfecho(a)",
      "Neutro(a) / Ni satisfecho(a) ni insatisfecho(a)",
      "Satisfecho(a)",
      "Muy satisfecho(a)"
    ))
  ) %>%
  mutate(
    Cambio_Naturaleza = as.numeric(Cambio_Naturaleza),
    Estabilidad_Animo = as.numeric(Estabilidad_Animo),
    Relaciones_Sociales = as.numeric(Relaciones_Sociales),
    Empatia_Compasion = as.numeric(Empatia_Compasion),
    Satisfaccion_Vida_Prev = as.numeric(Satisfaccion_Vida_Prev)
  )


# La variable dependiente (variable objetivo)
datos$dependiente <- as.factor(datos_filt[[218]])

# Transformar la variable dependiente en una variable numérica binaria
datos$dependiente <- ifelse(datos$dependiente == "Sí", 1, 0)


#============================
# Modelo
# Ajustar un modelo de regresión logística
modelo <- glm(dependiente ~ ., data = datos, family = binomial)

# Resumen del modelo
summary(modelo)

# Calcular el pseudo-R2
pseudo_r2 <- pR2(modelo)

# Mostrar el pseudo-R2
pseudo_r2

#====================================
# Selección hacia adelante y hacia atrás
modelo_step <- stepAIC(modelo, direction = "both", trace = FALSE)

# Resumen del modelo después de la selección de variables
summary(modelo_step)


#Observando todo (eliminación y adición de variables)
modelo_step_1 <- stepAIC(modelo, direction = "both")
summary(modelo_step_1)


step(modelo, direction = "backward", options(scipen=999))

#Observando todo (eliminación de variables)
modelo_step_2 <- stepAIC(modelo, direction = "backward")
summary(modelo_step_2)

#====================================
# Visualizar los coeficientes del modelo final
coeficientes <- coef(summary(modelo_step))
print(coeficientes)

