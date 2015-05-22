library(ggplot2)
library(caret)
library(dplyr)

#En este punto de leer los datos, se pide la dirección en donde el archivo está guardado, reemplace la dirección por la de su computador, aqui y al final del codigo para guardar el .csv


## leer los datos de testing y training

datos <- read.csv("/home/carolinaog/Metodos2015/HW8/CM20151_HW8_CarolinaOrtiz/training_set.csv")
summary(datos)
str(datos)
testing<- read.csv("/home/carolinaog/Metodos2015/HW8/CM20151_HW8_CarolinaOrtiz/test_set.csv")
str(testing)


## convertir variable categórica en variable cuantitativa en dondeel valor más alto es el clima con más eventos
datos$cod_Temp[datos$eventos=="Ninguno"] <- "10"
datos$cod_Temp[datos$eventos=="Lluvia"] <- "20"
datos$cod_Temp[datos$eventos=="Niebla"] <- "30"
datos$cod_Temp[datos$eventos=="Niebla-Lluvia"] <- "40"
datos$cod_Temp[datos$eventos=="Lluvia-Tormenta"] <- "50"
datos$cod_Temp[datos$eventos=="Lluvia-Nieve"] <- "60"
datos$cod_Temp[datos$eventos=="Niebla-Nieve"] <- "70"
datos$cod_Temp[datos$eventos=="Nieve"] <- "80"
datos$cod_Temp[datos$eventos=="Niebla-Lluvia-Nieve"] <- "90"
#media de la temperatura
temp_media <- ((datos[5]+datos[6])/2)
str(temp_media)
datos$temp_media<- c(do.call(rbind, temp_media))
##discriminación de parámetros
varianza <- aov(conteo_ordenes ~ cod_calendario + conteo_restaurantes + temp_media + precipitacion + cod_Temp, data=datos)
summary(varianza)
## plantear el modelo usando train de caret
modFit<- train(conteo_ordenes ~ cod_calendario + conteo_restaurantes + temp_media + cod_Temp, method = "lmStepAIC",data=datos)
## imprimir los parámetros del modelo final
print(modFit)

## seleccionar el modelo final
finMod <- modFit$finalModel
##otros parametros de la función
fitmethod <- modFit$method
results <- modFit$results
params <- modFit$bestTune
calls <- modFit$call
dots <- modFit$dots
metric <- modFit$metric
control <- modFit$control
prepro <- modFit$preProcess
trainData <- modFit$trainingData
resample <- modFit$resample
perfnames <- modFit$perfNames
max <- modFit$maximize
ylim <- modFit$yLimits
time <- modFit$times


elfit <- finMod$fitted

#plotea los residuos vs los valores fitted
plot(finMod,1,pch=19,cex=0.8,col="#00000040")

## Ahora use los colores con las variables que no se usaron en el modelado
qplot(finMod$fitted,finMod$residuals,colour=precipitacion,data=datos) + theme_bw()

##Grafico del fit vs. fecha
qplot(fecha, finMod$fitted,colour=eventos,data=datos) + theme_bw()

# graficar sólo los residuos
plot(finMod$residuals,pch=19)


elfit <- finMod$fitted

ggplot(datos) + geom_point(aes(fecha,elfit, colour=eventos)) + theme_bw()



## El test:
temp_media <- ((testing[5]+testing[6])/2)
str(temp_media)
testing$temp_media<- c(do.call(rbind, temp_media))

## convertir variable categórica en variable cuantitativa en dondeel valor más alto es el clima con más eventos
testing$cod_Temp[testing$eventos=="Ninguno"] <- "10"
testing$cod_Temp[testing$eventos=="Lluvia"] <- "20"
testing$cod_Temp[testing$eventos=="Niebla"] <- "30"
testing$cod_Temp[testing$eventos=="Niebla-Lluvia"] <- "40"
testing$cod_Temp[testing$eventos=="Lluvia-Tormenta"] <- "50"
testing$cod_Temp[testing$eventos=="Lluvia-Nieve"] <- "60"
testing$cod_Temp[testing$eventos=="Niebla-Nieve"] <- "70"
testing$cod_Temp[testing$eventos=="Nieve"] <- "80"
testing$cod_Temp[testing$eventos=="Niebla-Lluvia-Nieve"] <- "90"

## Comparar la predicción en nuestro conjunto de entrenamiento en el dataset de prueba.
conteo_ordenes <- predict(modFit, testing, na.action = na.pass)
##pred$fit

# agregar predicción al dataset
predict_test <- testing
predict_test$conteo_ordenes <- conteo_ordenes
# graficar la predicción, colorear por eventos.
ggplot(predict_test) + geom_point(aes(fecha,conteo_ordenes, colour=eventos))
print(conteo_ordenes)

prediccion <- select(predict_test, fecha, conteo_ordenes)
write.csv(prediccion, "/home/carolinaog/Metodos2015/HW8/CM20151_HW8_CarolinaOrtiz/prediccion1_cortiz84.csv", row.names=FALSE)


##1. Qué otros datos pueden ayudarlo a mejorar su predicción? Otros datos que ayudarían a la predicción
##2. Cuál día de la semana presenta el número mayor de órdenes?
###3. Cuál día de la semana presenta el menor número de órdenes?