library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)
library(hydroGOF)

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base1<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "A")

base<- cbind(base1$Dias.por.Vencer,base1$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

plot(base)

modelsvm<- svm(Rendimiento ~ DiasPorVencer, base)

predYsvm<-  predict(modelsvm, base)
predYsvm/100
points(base$DiasPorVencer, predYsvm, col = "red", pch=16)

#Calculando los parámetros del modelo

#Para el valor de W

W<- t(modelsvm$coefs) %*% modelsvm$SV

#El valor de b

b<- modelsvm$rho

#Cálculo de RMSE del modelo

RMSEsvm<- rmse(predYsvm,base$Rendimiento)

####### Ajustando el modelo SVR mediante la variación de los valores del error máximo permitido y 
####### el parámetro de costo

# Calibrando el modelo

OptModelsvm<- tune(svm, Rendimiento~DiasPorVencer, data=base,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

print(OptModelsvm)

plot(OptModelsvm)  

#Seleccionando el mejor modelo

MejorModelo<- OptModelsvm$best.model

#Prediciendo los rendimientos usando el mejor modelo

PredRendimientosMM<- predict(MejorModelo,base)

#Calculando el error

RMSEMM<- RMSE(PredRendimientosMM,base$Rendimiento)

#Parámetros del mejor modelo

W<- t(MejorModelo$coefs) %*% MejorModelo$SV
as.data.frame(W)
b<- MejorModelo$rho
as.data.frame(b)

#Comparando los modelos

plot(base,pch=16)

points(base$DiasPorVencer, predYsvm, col = "blue", pch=3)
points(base$DiasPorVencer, PredRendimientosMM, col = "red", pch=4)

A<- b + W* (20/360)

