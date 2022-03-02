############# Probabilidades de Incumplimiento por Calificación ##############

#De Hull & White se tiene: P_{incumplimiento} = Rendimiento / (1 - Tasa de recuperación)
#donde se supondrá la tasa de recuperación de México del 25%

library(dplyr)
library(plyr)

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base3<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "AA" & Datos$PAGOS.EXACTOS != 0)

base<- cbind(base3$Dias.por.Vencer,base3$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

####### Proba de Incumplimiento #####

base3$q<- base$Rendimiento / (1- 0.25)

###### Proba de No Incumplimiento ####

base3$p<- 1-base3$q

## A(t) ya viene definido en la base de datos como: Intereses.Devengados

## Sacando v(t) con interés compuesto de $100 pesos para cada i

Iteracion<-NULL
ValorPresente<-NULL

for(j in 1:nrow(base3)){
  
  Suma<-0
  VP<-0
  
  for(i in 1:base3$PAGOS.EXACTOS[j]){
    
    VP[i]<- (1+base$Rendimiento[j])^(i-1)
    
    Suma<-VP[i]+Suma
    
    n<-max(i)
    
  }
  
  Iteracion[j]<-Suma
  
  ValorPresente[j]<-(1+base$Rendimiento[j])^(n)
  
}

base3$v<-Iteracion+ValorPresente


## Sacando u(t)

base3$u<- Iteracion
base3$U<- ValorPresente

## e(t) se supondrá como 10, ya que es un pago acumulado, es decir e(t) = 10

### El spread ####

base$Spread<- (((1-0.25*base3$Intereses.Devengados*0.25) * base3$q * base3$v * (base3$Dias.por.Vencer/360)) / 
                 (base3$q)*(base3$u+10)* (base3$Dias.por.Vencer/360) + base3$p*base3$U)/100

### Gráfica del Spread ###

plot(x= base$DiasPorVencer, y= base$Spread, xlab = "DiasPorVencer", ylab = "Spread")
