# Librerias necesarias ----
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('pROC')
#install.packages('class')
#install.packages('RWeka')

#importacion de librerias
library('rpart')
library('rpart.plot')
library('pROC')
library('RWeka')
library('class')

# Lectura de los datos ----
datos <- read.csv(
  file = "C:/Users/Yanelly/Documents/asignacion_2/minable.csv",
  header = T)

#Eliminacion de atributos no necesarios
datos[,"cIdentidad"] <- NULL
datos[,"fNacimiento"] <- NULL
datos[,"tGrado"] <- NULL
datos[,"jReprobadas"] <- NULL 
datos[,"eCivil"] <- NULL
datos[,"dHabitacion"] <- NULL
datos[,"cDireccion"] <- NULL 
datos[,"oSolicitudes"] <- NULL 
datos[,"aEconomica"] <- NULL
datos[,"rating"] <- NULL 
datos[,"sugerencias"] <- NULL 

#correción de errores
datos$pReside[2] = 7 #se detecto un string el cual no pertenecía al tipo de dato categórico  correspondiente al atributo, y dado q pertenecia a la categoria "otros" se le asigno elvalor 7 
datos$pReside[26] = 7 #se detecto un string el cual no pertenecía al tipo de dato categórico  correspondiente al atributo y dado q pertenecia a la categoria "otros" se le asigno elvalor 7
datos$pReside = as.integer(datos$pReside) # Si se intenta realizar alguna operacion numerica o logica retorna NA, por lo que se procede a pasarla a formato integer
datos$grOdontologicos = as.integer(datos$grOdontologicos)


#La data de entrada fue divida en tres partes, y se tomo una muestra para entrenamiento y otra para prueba, para finalmente sumar de cada uno de ellos  con la finalidad de que la data de prueba y de entramiento de todo el dataset tuviese una proporcion equitativa de cada uno de los valores del atributo modo de ingreso
data0 = datos[datos$mIngreso == 0,]
data2 = datos[datos$mIngreso == 2,]
data3 = datos[datos$mIngreso == 3,]

#muestra de entrenamiento y prueba para cada una de las partes seleccionadas separandolas en un 20% para pruebas y un 80% para entrenamiento
muestra0 <- sample(nrow(data0), floor(nrow(data0) * 0.8), replace = F, prob= NULL) #se selecciona la muestra para el modo de ingreso 0 
t_ent0 <- data0[muestra0,]
t_prueba0 <- data0[-muestra0,]

muestra2 <- sample(nrow(data2), floor(nrow(data2) * 0.8), replace = F, prob= NULL) #se selecciona la muestra para el modo de ingreso 0 
t_ent2 <- data2[muestra2,]
t_prueba2 <- data2[-muestra2,]

muestra3 <- sample(nrow(data3), floor(nrow(data3) * 0.8), replace = F, prob= NULL) #se selecciona la muestra para el modo de ingreso 0 
t_ent3 <- data3[muestra3,]
t_prueba3 <- data3[-muestra3,]

#Union de todas las muestras de entrenamiento
t_entT = merge(t_ent0, merge(t_ent2, t_ent3 ,all = TRUE), all = TRUE)
#Union de todas las muestras de prueba
t_pruebaT = merge(t_prueba0, merge(t_prueba2, t_prueba3 ,all = TRUE), all = TRUE)


##Arbol de Desición 
#Se crea y grafica el arbol de desicion con la muestra----
tree <- rpart(mIngreso ~ ., data = t_entT , method ="class")
rpart.plot(tree)

predict<-predict(tree, t_pruebaT, type = "class")

matrizConfA<-table(t_pruebaT$mIngreso,predict)
matrizConfA

ra <- roc(t_pruebaT$mIngreso, as.integer(predict), levels=c(0,2,3))
plot(ra)

AciertosA = ((matrizConfA[1,1] + matrizConfA[2,2] + matrizConfA[3,3])/ nrow(t_pruebaT))* 100
AciertosA


#Clasificacion
t_entT$mIngreso = as.factor(t_entT$mIngreso)
t_pruebaT$mIngreso = as.factor(t_pruebaT$mIngreso)

rclasificacion <- JRip(formula = mIngreso ~ ., data = t_entT)
predictC <- predict(rclasificacion, t_pruebaT, type = "class")

matrizConfC <- table(t_pruebaT$mIngreso, predictC)
matrizConfC 

rc <- roc(t_pruebaT$mIngreso, as.integer(predictC), levels=c(0,2,3))
plot(rc)

AciertosC = ((matrizConfC[1,1] + matrizConfC[2,2] + matrizConfC[3,3])/ nrow(t_pruebaT))* 100
AciertosC

#kvecinos

trainLabels <- t_entT$mIngreso
testLabels <- t_pruebaT$mIngreso
  
predictK <- knn(train = t_entT, test = t_pruebaT, cl = trainLabels, k=14)
matrizConfK <- table(testLabels, predictK)
matrizConfK

rk <- roc(t_pruebaT$mIngreso, as.integer(predictK), levels=c(0,2,3))
plot(rk)

AciertosK = ((matrizConfK[1,1] + matrizConfK[2,2] + matrizConfK[3,3])/ nrow(t_pruebaT))* 100
AciertosK

AciertosA
AciertosC
AciertosK


