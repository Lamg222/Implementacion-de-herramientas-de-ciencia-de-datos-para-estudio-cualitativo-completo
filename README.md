# Implementacion de herramientas de ciencia de datos para estudio cualitativo completo

### Estudio

Este estudio contempla un análisis estadístico completo sobre los aspectos fenomenológicos de los estados de conciencia no ordinarios.
El gran avance tecnológico permite realizar estudios completos sobre temas tan controversiales como este.
De igual forma, el valor que esta investigación aporta vas desde la gran heterogeneidad de las preguntas, así como de los participantes del estudio.
Por otro lado, el estudio contempla, en su mayoría, variables categóricas lo cual permite crear una guía completa de exploración al análisis cualitativo de los datos, utilizando técnicas de ciencia de datos (CD). 
Dichas técnicas contemplan ramas de CD como:

1. Preparación y transformación de datos
+ La homologación de respuestas abiertas que representan la misma declaración o afirmación.
+ Codificación de las variables
+ Carga y lectura de la base de datos

2. Presentación y visualización
+ Análisis univariado, bivariado y multivariado, a través de gráficos.
+ Evidencia visual y representación de los resultados

3. Análisis estadístico
+ Evidencias numéricas y de contraste de hipótesis para la exploración inferencial

4. Aprendizaje automático (ML)
+ Preprocesamiento de los datos
+ Aprendizaje del algoritmo
+ Evaluación del algoritmo
+ Técnicas avanzadas de reducción de sobreajuste

5. Análisis de información de grafos
+ Coeficiente de V de Cramer sobre el grado de relación entre las variables independientes del modelo y la variable de estudiada

6. Procedimiento del lenguaje natural (NLP)
+ Representación de las respuestas de preguntas abiertas


### Descripción de los conceptos clave
Las experiencias fenomenológicas se refieren a las vivencias y percepciones subjetivas que el individuo aprecia como una realidad.
Mientras que, los estados de conciencia no ordinarios, se definen como la condición donde la percepción, la cognición (estos procesos estructurales del pensamiento) y el estado de conciencia del individuo, se encuentran alterados o en un estado inducido por alguna actividad como la meditación, experiencias cercanas a la muerte, uso de sustancias psicodélicas, etc. (principalmente, en este caso, se centra el concepto en los procesos introspectivos, todos inducidos por el uso de psicodélicos)


### Instrumento recopilador de información

La creación de un cuestionario que forma parte de una investigación llevada a cabo en colaboración entre la *Universidad Camilo José Cela*, *Inawe (el Observatorio Español de Terapia Asistida con Psicodélicos)* y el *European Institute of Consciousness Research*.
Dicho cuestionario, a la fecha 13/octubre/2024, brinda la apertura a una investigación internacional de carácter técnico especializado, que evalua 233 variables/preguntas a un total de 117 instancias anónimas.

Puedes leer más sobre el proyecto en este enlace:  https://eiocr.com/phenomenology-study.html 

Contacto

Para cualquier consulta sobre el Cuestionario de la Experiencia Fenomenológica con Enteógenos (CEFE) puedes contactarnos a: 

cefe@eiocr.com


### Análisis correlacional

Las hipótesis de la prueba Chi-cuadrada son:

Hipótesis nula (Ho): Las dos variables son independientes, lo que significa que no hay relación entre ellas.

Hipótesis alternativa (Ha): Las dos variables no son independientes, es decir, existe una relación entre ellas.

*Chi-cuadrada*

Los supuestos que deben cumplirse para que el valor de este estadístico sea estadísticamente confiable en el estudio de independencia entre variables son:

I. Que las variables sean categóricas. Es decir, que los valores de las frecuencias observadas provengan de variables cualitativas

II. Que cada observación debe ser independiente. Esto es que, una misma instancia o individuo no debe pertenecer a otro registro evaluado en el estudio.

III. Que la frecuencia esperada de cada celda debe ser por lo menos de 5. Dicho de otra forma, que cada cruce entre las posibles respuestas de ambas variables tenga un valor esperado (E) mayor o igual a una frecuencia de 5.


Total de 12 variables independientes

_Chi-cuadrada_

1. Experiencia de pérdida de sí mismo o de alguna creencia fuertemente vinculada con tu personalidad.
2. Tuviste alguna actividad previa de yoga
3. Experimentaste alguna vez un acontecimiento místico
4. Movimientos involuntarios como experiencia fenomenológica
5. Experiencia fenomenológica vinculada con la muerte.
6. Prácticas de respiración previas a la experiencia fenomenológica.

*Prueba exacta de Fisher*

Hipótesis nula (Ho): Las variables son independientes por lo que una variable no cambia entre los distintos niveles de la otra variable.

Hipótesis alternativa (Ha): Las variables son dependientes, es decir, una variable cambia entre los distintos niveles de la otra variable.

Donde los supuestos que deben cumplirse para la correcta implementación de esta prueba son:

I).	Independencia entre las observaciones. Del mismo modo que con el estadístico de prueba cumulativo de Pearson, cada observación contribuye únicamente a un solo nivel de la variable categórica, representada en la tabla de contingencia

II).	Frecuencias marginales fijas. Es decir, no hay alteraciones en las frecuencias totales de las filas y columnas, al obtener combinaciones diferentes entre los niveles de cada variable.


_Variables de la Prueba exacta de Fisher:_

7. Cambió tu conexión con la naturaleza, después de tu experiencia psicodélica.
8. Tu experiencia psicodélica te llevo a algún cambio o decisión importante en la vida.
9. Cambio en la estabilidad del estado de ánimo posterior a la experiencia psicodélica.
10. Cambio en las relaciones sociales o conexión con los demás
11. Cambio en la sensación de empatía y compasión, después de la experiencia psicodélica.
12. Nivel de satisfacción con tu vida antes de la(s) experiencia(s) psicodélica(s)

## Algoritmo

*Hiperparámetros*

El hiperparámetro “C”, se define como el inverso de la fuerza de regularización y previene el sobreajuste.
El hiperparámetro “penalty”, especifica el tipo de penalización entre la norma L1 y L2, que representan la regularización Lasso (Least Absolute Shrinkage and Selection Operator) y Ridge, respectivamente.
Por último, para el hiperparámetro “solver”, este permite especificar el algoritmo que se utilizará para encontrar los valores de los coeficientes estimados beta, de cada una de las variables independientes del modelo. Estos algoritmos asignan los valores de los coeficientes minimizando el error entre el valor estimado y el valor observado

*Coeficientes estimados beta*
Dado el conjunto de hiperparámetros para este modelo, obtenemos que los coeficientes convergen a cero, debido a la penalización Ridge o norma L2, lo que reafirma que la combinación del conjunto de las 12 variables independientes es poco representativa y es necesario buscar otra combinación o conjunto de variables que tengan una mayor representatividad en sus coeficientes estimados, para la explicación de la variabilidad de la variable dependiente “Consideras que tu sensación de dicha o felicidad se debe a tu experiencia psicodélica”.


*Métricas de evaluación para el modelo de clasificación*

_Precision_
Para esta métrica, el modelo ha obtenido un valor del 100%, respecto a las instancias que vinculan su sensación de dicha o alegría a su experiencia fenomenológica (clase 1). Esto indica que del total de instancias que el modelo clasificó de esta manera no hay instancias de la clase que considera que su percepción de dicha no se relaciona con la experiencia fenomenológica vivida (clase 0) que se hayan estimado como clase 1. Sin embargo, el resultado es de 0.77 para la clase 0, es decir que hay instancias de clase 1 clasificadas como clase 0.

_Recall_
Para este caso, el modelo obtiene 88% de acierto sobre esta métrica para la clase de interés (clase 1). Lo que indica que el modelo no clasificó todas las instancias de individuos que vinculan su sensación de dicha o alegría a su experiencia fenomenológica como instancias de este tipo. Empero, por el lado de las clases que percibieron una sensación de dicha, pero que no vinculan esta sensación a su experiencia fenomenológica, la clasificación fue de 1.00, lo que se refleja en la matriz de confusión (todas las instancias de clase 0 se detectaron correctamente, mientras que el algoritmo clasificó algunas instancias de clase 1 como clase 0).

