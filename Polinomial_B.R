####### REGRESIÓN POLINOMIAL #######

library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)


ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base2<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "B")

base<- cbind(base2$Dias.por.Vencer,base2$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

plot(base)

Regresion<- lm(base$Rendimiento ~ poly(base$DiasPorVencer, 8, raw = T))
summary(Regresion)

lines(smooth.spline(base$DiasPorVencer, predict(Regresion)),col="red",lwd=3)
