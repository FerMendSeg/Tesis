################## P R O B A B I L I D A D E S D E I N C U M P L I M I E N T O #################################
library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)

### Calificación AAA con diferentes horizontes de tiempo #######

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base4<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "AAA")

base<- cbind(base4$Dias.por.Vencer,base4$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)

Rendimiento<-2.566373 + 3.68415 * (base$DiasPorVencer/360)

## Probabilidad de Incumplimiento ##
lambda <- Rendimiento /(1 - 0.25)
ProbaAAA90<- 1 - exp(-(lambda)*(90)/360)
ProbaAAA180<- 1 - exp(-(lambda)*(180)/360)
ProbaAAA270<- 1 - exp(-(lambda)*(270)/360)
ProbaAAA1Y<- 1 - exp(-(lambda)*(360)/360)
ProbaAAA3Y<- 1 - exp(-(lambda)*(1080)/360)
ProbaAAA5Y<- 1 - exp(-(lambda)*(1800)/360)

par(mfrow=c(1,2))
Noventa<-plot(ProbaAAA90,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
              col= "red", main= "Figura 1. Probabilidad de 
              caer en incumplimiento en 90 días", cex.main=0.9)

Oc<-plot(ProbaAAA180,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
         col= "blue", main= "Figura 2. Probabilidad de 
         caer en incumplimiento en 180 días", cex.main=0.9)

par(mfrow=c(1,2))
Dosc<-plot(ProbaAAA270,xlab = "Bono i", ylab = "Probabilidad", type="p", pch=19, 
           col= "#660033", main= "Figura 3. Probabilidad de 
         caer en incumplimiento en 270 días", cex.main=0.9)

OY<-plot(ProbaAAA1Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#9933CC", main= "Figura 4. Probabilidad de 
         caer en incumplimiento en 1 año", cex.main=0.9)

par(mfrow=c(1,2))
TY<-plot(ProbaAAA3Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#3399CC", main= "Figura 5. Probabilidad de 
         caer en incumplimiento en 3 años", cex.main=0.9)

FY<-plot(ProbaAAA5Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
             col= "#0000CC", main= "Figura 6. Probabilidad de 
             caer en incumplimiento en 5 años", cex.main=0.9)

##################################### Calificación AA #####################################################
base3<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "AA")

base<- cbind(base3$Dias.por.Vencer,base3$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)

Rendimiento<-4.346667 + 2.513864 * (base$DiasPorVencer/360)

## Probabilidad de Incumplimiento ##
lambda <- Rendimiento /(1 - 0.25)
ProbaAAA90<- 1 - exp(-(lambda)*(90)/360)
ProbaAAA180<- 1 - exp(-(lambda)*(180)/360)
ProbaAAA270<- 1 - exp(-(lambda)*(270)/360)
ProbaAAA1Y<- 1 - exp(-(lambda)*(360)/360)
ProbaAAA3Y<- 1 - exp(-(lambda)*(1080)/360)
ProbaAAA5Y<- 1 - exp(-(lambda)*(1800)/360)

par(mfrow=c(1,2))
Noventa<-plot(ProbaAAA90,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
              col= "red", main= "Figura 1. Probabilidad de 
              caer en incumplimiento en 90 días", cex.main=0.9)

Oc<-plot(ProbaAAA180,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
         col= "blue", main= "Figura 2. Probabilidad de 
         caer en incumplimiento en 180 días", cex.main=0.9)

par(mfrow=c(1,2))
Dosc<-plot(ProbaAAA270,xlab = "Bono i", ylab = "Probabilidad", type="p", pch=19, 
           col= "#660033", main= "Figura 3. Probabilidad de 
           caer en incumplimiento en 270 días", cex.main=0.9)

OY<-plot(ProbaAAA1Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#9933CC", main= "Figura 4. Probabilidad de 
         caer en incumplimiento en 1 año", cex.main=0.9)

par(mfrow=c(1,2))
TY<-plot(ProbaAAA3Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#3399CC", main= "Figura 5. Probabilidad de 
         caer en incumplimiento en 3 años", cex.main=0.9)

FY<-plot(ProbaAAA5Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#0000CC", main= "Figura 6. Probabilidad de 
         caer en incumplimiento en 5 años", cex.main=0.9)

############################### Calificación A ############################################################

base1<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "A")

base<- cbind(base1$Dias.por.Vencer,base1$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)

Rendimiento<-0.4030378 + 0.2988597 * (base$DiasPorVencer/360)

## Probabilidad de Incumplimiento ##
lambda <- Rendimiento /(1 - 0.25)
ProbaAAA90<- 1 - exp(-(lambda)*(90)/360)
ProbaAAA180<- 1 - exp(-(lambda)*(180)/360)
ProbaAAA270<- 1 - exp(-(lambda)*(270)/360)
ProbaAAA1Y<- 1 - exp(-(lambda)*(360)/360)
ProbaAAA3Y<- 1 - exp(-(lambda)*(1080)/360)
ProbaAAA5Y<- 1 - exp(-(lambda)*(1800)/360)

par(mfrow=c(1,2))
Noventa<-plot(ProbaAAA90,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
              col= "red", main= "Figura 1. Probabilidad de 
              caer en incumplimiento en 90 días", cex.main=0.9)

Oc<-plot(ProbaAAA180,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
         col= "blue", main= "Figura 2. Probabilidad de 
         caer en incumplimiento en 180 días", cex.main=0.9)

par(mfrow=c(1,2))
Dosc<-plot(ProbaAAA270,xlab = "Bono i", ylab = "Probabilidad", type="p", pch=19, 
           col= "#660033", main= "Figura 3. Probabilidad de 
           caer en incumplimiento en 270 días", cex.main=0.9)

OY<-plot(ProbaAAA1Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#9933CC", main= "Figura 4. Probabilidad de 
         caer en incumplimiento en 1 año", cex.main=0.9)

par(mfrow=c(1,2))
TY<-plot(ProbaAAA3Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#3399CC", main= "Figura 5. Probabilidad de 
         caer en incumplimiento en 3 años", cex.main=0.9)

FY<-plot(ProbaAAA5Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#0000CC", main= "Figura 6. Probabilidad de 
         caer en incumplimiento en 5 años", cex.main=0.9)

############################## Calificación B ##########################################################

base2<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "B")

base<- cbind(base2$Dias.por.Vencer,base2$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)

Rendimiento<-2.972502 + 0.1759612 * (base$DiasPorVencer/360)

## Probabilidad de Incumplimiento ##
lambda <- Rendimiento /(1 - 0.25)
ProbaAAA90<- 1 - exp(-(lambda)*(90)/360)
ProbaAAA180<- 1 - exp(-(lambda)*(180)/360)
ProbaAAA270<- 1 - exp(-(lambda)*(270)/360)
ProbaAAA1Y<- 1 - exp(-(lambda)*(360)/360)
ProbaAAA3Y<- 1 - exp(-(lambda)*(1080)/360)
ProbaAAA5Y<- 1 - exp(-(lambda)*(1800)/360)

par(mfrow=c(1,2))
Noventa<-plot(ProbaAAA90,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
              col= "red", main= "Figura 1. Probabilidad de 
              caer en incumplimiento en 90 días", cex.main=0.9)

Oc<-plot(ProbaAAA180,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19, 
         col= "blue", main= "Figura 2. Probabilidad de 
         caer en incumplimiento en 180 días", cex.main=0.9)

par(mfrow=c(1,2))
Dosc<-plot(ProbaAAA270,xlab = "Bono i", ylab = "Probabilidad", type="p", pch=19, 
           col= "#660033", main= "Figura 3. Probabilidad de 
           caer en incumplimiento en 270 días", cex.main=0.9)

OY<-plot(ProbaAAA1Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#9933CC", main= "Figura 4. Probabilidad de 
         caer en incumplimiento en 1 año", cex.main=0.9)

par(mfrow=c(1,2))
TY<-plot(ProbaAAA3Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#3399CC", main= "Figura 5. Probabilidad de 
         caer en incumplimiento en 3 años", cex.main=0.9)

FY<-plot(ProbaAAA5Y,xlab = "Bono i", ylab = "Probabilidad", type="p",pch=19,
         col= "#0000CC", main= "Figura 6. Probabilidad de 
         caer en incumplimiento en 5 años", cex.main=0.9)
