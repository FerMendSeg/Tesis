############# Probabilidades de Incumplimiento por Calificación ##############

#De Hull & White se tiene: P_{incumplimiento} = Rendimiento / (1 - Tasa de recuperación)
#donde se supondrá la tasa de recuperación de México del 25%

library(dplyr)
library(plyr)

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base4<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "AAA")

base<- cbind(base4$Dias.por.Vencer,base4$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

####### Proba de Incumplimiento #####

base4$q<- base$Rendimiento / (1- 0.25)

###### Proba de No Incumplimiento ####

base4$p<- 1-base4$q

## A(t) ya viene definido en la base de datos como: Intereses.Devengados

## Sacando v(t) con interés compuesto de $100 pesos para cada i

Iteracion<-NULL
ValorPresente<-NULL

for(j in 1:nrow(base4)){
  
  Suma<-0
  VP<-0
  
  for(i in 1:base4$PAGOS.EXACTOS[j]){
    
    VP[i]<- (1+base$Rendimiento[j])^(i-1)
    
    Suma<-VP[i]+Suma
    
    n<-max(i)
    
  }
  
  Iteracion[j]<-Suma
  
  ValorPresente[j]<-(1+base$Rendimiento[j])^(n)
  
}

base4$v<-Iteracion+ValorPresente


## Sacando u(t)

base4$u<- Iteracion
base4$U<- ValorPresente

## e(t) se supondrá como 10, ya que es un pago acumulado, es decir e(t) = 10

### El spread ####

base$Spread<- (((1-0.25*base4$Intereses.Devengados*0.25) * base4$q * base4$v * (base4$Dias.por.Vencer/360)) / 
    (base4$q)*(base4$u+10)* (base4$Dias.por.Vencer/360) + base4$p*base4$U)/100

### Gráfica del Spread ###

plot(x= base$DiasPorVencer, y= base$Spread, xlab = "DiasPorVencer", ylab = "Spread")
