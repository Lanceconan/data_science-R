#EJECUTA LAS INSTRUCCIONES UNA POR UNA EN R
# Carga los datos en la sesión de trabajo
  library(rpart) # instalar paquete rpart

  datos <- kyphosis # usar la base de datos de la librería rpart kyphosis
  
#OBTENER INFORMACIÓN GENERAL DE LOS DATOS (outliers que puedan ensusiar la muestra)
  
  summary(datos) # obtener los datos globales de la base de datos.
  
  desvAge.o <- sd(datos$Age)	# desviación estandar de Edad de los pacientes
  desvNumber.o <- sd(datos$Number)	# desviación estandar de Numero de vertebras involucradas
  desvStart.o <- sd(datos$Start) 	# desviación estandar de primera vertebra operada
  
  puntajeAge_z <- (datos$Age) / desvAge.o			# puntaje z de edad
  puntajeNumber_z <- (datos$Number) / desvNumber.o	# puntaje z de numero de vertebras involucradas
  puntajeStart_z <- (datos$Start) / desvStart.o	# puntaje z de primera vertebra operada
  
  
  par(mfrow=c(1,2))
  hist(datos$Age)
  hist(puntajeAge_z) # contrucción de histograma de variable edad
  
  par(mfrow=c(1,2))
  hist(datos$Number)
  hist(puntajeNumber_z) # contrucción de histograma de variable Numero de vertebras

  par(mfrow=c(1,2))
  hist(datos$Start)
  hist(puntajeStart_z) # contrucción de histograma de variable primera vertebra operada
  
  par(mfrow=c(1,2))
  boxplot(datos$Number)
  boxplot(puntajeNumber_z) # boxplot de variable Numero de vertebras
  
  par(mfrow=c(1,2))
  boxplot(datos$Age)
  boxplot(puntajeAge_z) # boxplot de variable primera vertebra operada
  
  par(mfrow=c(1,2))
  boxplot(datos$Start)
  boxplot(puntajeStart_z) # boxplot de variable edad

  #Interpretar esta información
  # https://picandoconr.wordpress.com/2016/02/27/diagrama-de-cajas/
  # https://stepupanalytics.com/outlier-detection-techniques-using-r/
  
# 1) Define el conjunto de datos de prueba y el de validación

  set.seed(1) # usar la semilla uno como dice la instrucción
  ind <- sample(2, length(datos$Kyphosis), replace=TRUE, prob=c(2/3, 1/3)) # crear aleatoriamente 2 grupos con tdos los datos de la base de datos
  table(ind) # muestra los indices de las matrices de entreamiento y de prueba
  datos.trabajo <- datos[ind==1,]
  datos.validacion <- datos[ind==2,]
  datos.trabajo # sirve para conocer el set de entrenamento aleatorio (2/3)
  datos.validacion # sirve para conocer el set de pruebas de validación (1/3)
  dim(datos.trabajo) # dimensiones de la matriz de entrenamiento 
  dim(datos.validacion) # dimensiones de la matriz de pruebas

# 2) Construye el clasificador de Bayes ingenuo
  
  library(e1071)
  fit.NB <- naiveBayes(Kyphosis ~ .,data=datos.trabajo)
  pred.NB <- predict(fit.NB,datos.validacion[,-1],type="raw")


# 3) Construye el clasificador de KN
  
  library(kknn)
  fit.kknn <- kknn(Kyphosis ~ ., datos.trabajo, datos.validacion, distance = 1, kernel = "triangular")
  summary(fit.kknn)


# 4) Compara clasificadores

  fit <- fitted(fit.kknn)
  table(datos.validacion$Kyphosis, fit)

  library(rminer)
  mmetric(datos.validacion[,1],pred.NB,"ACC") # Precision
  mmetric(datos.validacion[,1],pred.NB,"TPR") # Sensibilidad
  mmetric(datos.validacion[,1],pred.NB,"TNR") # Especificidad

