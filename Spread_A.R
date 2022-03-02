############# Probabilidades de Incumplimiento por Calificación ##############

#De Hull & White se tiene: P_{incumplimiento} = Rendimiento / (1 - Tasa de recuperación)
#donde se supondrá la tasa de recuperación de México del 25%

library(dplyr)
library(plyr)

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base1<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "A" & Datos$PAGOS.EXACTOS != 0)

base<- cbind(base1$Dias.por.Vencer,base1$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

####### Proba de Incumplimiento #####

base1$q<- base$Rendimiento / (1- 0.25)

###### Proba de No Incumplimiento ####

base1$p<- 1-base1$q

## A(t) ya viene definido en la base de datos como: Intereses.Devengados

## Sacando v(t) con interés compuesto de $100 pesos para cada i

Iteracion<-NULL
ValorPresente<-NULL

for(j in 1:nrow(base1)){
  
  Suma<-0
  VP<-0
  
  for(i in 1:base1$PAGOS.EXACTOS[j]){
    
    VP[i]<- (1+base$Rendimiento[j])^(i-1)
    
    Suma<-VP[i]+Suma
    
    n<-max(i)
    
  }
  
  Iteracion[j]<-Suma
  
  ValorPresente[j]<-(1+base$Rendimiento[j])^(n)
  
}

base1$v<-Iteracion+ValorPresente


## Sacando u(t)

base1$u<- Iteracion
base1$U<- ValorPresente

## e(t) se supondrá como 10, ya que es un pago acumulado, es decir e(t) = 10

### El spread ####

base$Spread<- (((1-0.25*base1$Intereses.Devengados*0.25) * base1$q * base1$v * (base1$Dias.por.Vencer/360)) / 
                 (base1$q)*(base1$u+10)* (base1$Dias.por.Vencer/360) + base1$p*base1$U)/100

### Gráfica del Spread ###

plot(x= base$DiasPorVencer, y= base$Spread, xlab = "DiasPorVencer", ylab = "Spread")
