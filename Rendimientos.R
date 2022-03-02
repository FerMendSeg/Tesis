library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)
library(mlbench)
library(nls2)
library(proto)

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

Datos1<-read.csv(paste(ruta,"Datos_Finales.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)

base1<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "A")

base<- cbind(base1$Dias.por.Vencer,base1$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

########## Aplicando el Cross Validation #############
set.seed(37)

setControl<- trainControl(method = "cv",number = 3,verboseIter = T,savePredictions = T)

fit<-train(Rendimiento ~ DiasPorVencer, base, method = "lm", trControl = setControl)
print(fit)
summary(fit)

#Entonces la ecuación para estimar los rendimientos queda:
# Yi = 1.130e-01 + (-3.204e-06) * DiasPorVencer/360

par(mfrow=c(1,1))
plot(base$DiasPorVencer,base$Rendimiento,xlab = "Dias por Vencer", ylab = "Rendimiento")
abline(lsfit(base$DiasPorVencer,base$Rendimiento),col="red")

#########################################################################

base2<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "B")

base<- cbind(base2$Dias.por.Vencer,base2$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100


########## Aplicando el Cross Validation #############
set.seed(37)

setControl<- trainControl(method = "cv",number = 8,verboseIter = T)

fit<-train(Rendimiento ~ DiasPorVencer, base, method = "lm", trControl = setControl)
print(fit)
summary(fit)

#Entonces la ecuación queda:
# Yi = 1.094e-01 + 1.735e-05 * DiasPorVencer/360
par(mfrow=c(1,1))
plot(base$DiasPorVencer,base$Rendimiento,xlab = "Dias por Vencer", ylab = "Rendimiento")
abline(lsfit(base$DiasPorVencer,base$Rendimiento),col="blue")

#####################################################################

base3<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "AAA")

base<- cbind(base3$Dias.por.Vencer,base3$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100

########## Aplicando el Cross Validation #############
set.seed(37)

setControl<- trainControl(method = "cv",number = 12,verboseIter = T)

fit<-train(Rendimiento ~ DiasPorVencer, base, method = "lm", trControl = setControl)
print(fit)
summary(fit)

#Entonces la ecuación queda:
# Yi = 8.632e-02 + 1.716e-06 * DiasPorVencer/360
par(mfrow=c(1,1))
plot(base$DiasPorVencer,base$Rendimiento,xlab = "Dias por Vencer", ylab = "Rendimiento")
abline(lsfit(base$DiasPorVencer,base$Rendimiento),col="green")

#####################################################################

base4<-filter(Datos, Dias.por.Vencer != 0 & Homologación.Calificación == "AA")

base<- cbind(base4$Dias.por.Vencer,base4$Rendimiento)
colnames(base)<-c("DiasPorVencer","Rendimiento")
base<-as.data.frame(base)
base$Rendimiento<-base$Rendimiento/100


########## Aplicando el Cross Validation #############
set.seed(37)

setControl<- trainControl(method = "cv",number = 16,verboseIter = T)

fit<-train(Rendimiento ~ DiasPorVencer, base, method = "lm", trControl = setControl)
print(fit)
summary(fit)

#Entonces la ecuación queda:
# Yi = 8.223e-02 + 2.767e-05 * DiasPorVencer/360

par(mfrow=c(1,1))
plot(base$DiasPorVencer,base$Rendimiento,xlab = "Dias por Vencer", ylab = "Rendimiento")
abline(lsfit(base$DiasPorVencer,base$Rendimiento),col="orange")

###########################################################################################
#########                 ESTIMACIONES PARA CADA CALIFICACIÓN                    ##########
###########################################################################################

############ Para calificación A para sacar sus rendimientos estimados#############

# Seis Meses

base1$SeisMeses<- 1.130e-01 + (-3.204e-06) * 180/360

# Nueve Meses

base1$NueveMeses<- 1.130e-01 + (-3.204e-06) * 270/360  

# 1 Y

base1$UnAño<- 1.130e-01 + (-3.204e-06) * 360/360

# 3 Y

base1$TresAños<- 1.130e-01 + (-3.204e-06) * 1080/360

# 5 Y

base1$CincoAños<- 1.130e-01 + (-3.204e-06) * 1800/360

# 10 Y

base1$DiezAños<- 1.130e-01 + (-3.204e-06) * 3600/360

base1$Rendimiento <- base1$Rendimiento/100
write.csv(base1, "Rendimientos_A.csv",row.names = F)

############ Para calificación B para sacar sus rendimientos estimados#############

# 6 Meses

base2$SeisMeses<- 1.094e-01 + 1.735e-05 * 180/360 

# 3 Y

base2$TresAños<- 1.094e-01 + 1.735e-05 * 1080/360

# 5 Y

base2$CincoAños<- 1.094e-01 + 1.735e-05 * 1800/360

# 10 Y

base2$DiezAños<- 1.094e-01 + 1.735e-05 * 3600/360

base2$Rendimiento <- base2$Rendimiento/100
write.csv(base2, "Rendimientos_B.csv",row.names = F)

############ Para calificación AAA para sacar sus rendimientos estimados#############

# 3 Y

base3$TresAños<- 8.632e-02 + 1.716e-06 * 1080/360

# 5 Y

base3$CincoAños<- 8.632e-02 + 1.716e-06 * 1800/360

# 10 Y

base3$DiezAños<- 8.632e-02 + 1.716e-06 * 3600/360

base3$Rendimiento <- base3$Rendimiento/100
write.csv(base3, "Rendimientos_AAA.csv",row.names = F)

############ Para calificación AA para sacar sus rendimientos estimados#############

# 3 Y

base4$TresAños<- 8.223e-02 + 2.767e-05 * 1080/360

# 5 Y

base4$CincoAños<- 8.223e-02 + 2.767e-05 * 1800/360

# 10 Y

base4$DiezAños<- 8.223e-02 + 2.767e-05 * 3600/360

base4$Rendimiento <- base4$Rendimiento/100
write.csv(base4, "Rendimientos_AA.csv",row.names = F)
