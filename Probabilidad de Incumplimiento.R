################### PROBABILIDADES DE INCUMPLIMIENTO ##################

#De Hull & White se tiene: P_{incumplimiento} = Rendimiento / (1 - Tasa de recuperación)
#donde se supondrá la tasa de recuperación de México del 25%

library(dplyr)
library(plyr)

ruta<-"C:/Users/Fernanda/Documents/TESIS/CreditDefaulSwaps/"

################################## CALIFICACIÓN AAA #################################################

Datos1<-read.csv(paste(ruta,"Rendimientos_AAA.csv",sep = ""),header=TRUE)

Datos<- as.data.frame(Datos1)
### Tres Meses ###
base1<- filter(Datos, Dias.por.Vencer != 0 & Dias.por.Vencer <= 90)

ProbaInc_TMAAA<- base1$Rendimiento / (1-0.25)
ProbaNoInc_TMAAA<- 1 - ProbaInc_TMA

base1$ProbaIncTM <- ProbaInc_TMAAA
base1$ProbaNoIncTM <- ProbaNoInc_TMAAA

### Seis Meses ###

base2<- filter(Datos, Dias.por.Vencer > 90  & Dias.por.Vencer <= 180)

ProbaInc_SMAAA<- base2$Rendimiento / (1-0.25)
ProbaNoInc_SMAAA<- 1 - ProbaInc_SMAAA

base2$ProbaIncSM <- ProbaInc_SMAAA
base2$ProbaNoIncSM<- ProbaNoInc_SMAAA 

### Nueve Meses ###

base3<- filter(Datos, Dias.por.Vencer > 180 & Dias.por.Vencer <= 270)

ProbaInc_NMAAA<- base3$Rendimiento / (1-0.25)
ProbaNoInc_NMAAA<- 1 - ProbaInc_NMAAA

base3$ProbaIncNM<- ProbaInc_NMAAA
base3$ProbaNoIncNM<- ProbaNoInc_NMAAA 

### 1 Y ###
base4<- filter(Datos, Dias.por.Vencer > 270 & Dias.por.Vencer <= 360)

ProbaInc_1A<- base4$Rendimiento / (1-0.25)
ProbaNoInc_1A<- 1 - ProbaInc_1A

base4$ProbaInc1Y<- ProbaInc_1A
base4$ProbaNoInc1Y<- ProbaNoInc_1A

### 3 Y ###

ProbaInc3A<- Datos$TresAños / (1-0.25)
ProbaNoInc3A<- 1 - ProbaInc3A

### 5 Y ###
ProbaInc5A<- Datos$CincoAños / (1-0.25)
ProbaNoInc5A<- 1 - ProbaInc5A

### 10 Y ###
ProbaInc10A<- Datos$DiezAños / (1-0.25)
ProbaNoInc10A<- 1 - ProbaInc10A


####################################### CALIFICACIÓN AA ################################################
Datos1<-read.csv(paste(ruta,"Rendimientos_AA.csv",sep = ""),header=TRUE)

Datos2<- as.data.frame(Datos1)
### Tres Meses ###
base5<- filter(Datos2, Dias.por.Vencer !=0 & Dias.por.Vencer <=90)

ProbaIncTMAA<- base5$Rendimiento / (1-0.25)
ProbaNoIncTMAA<- 1 - ProbaIncTMAA

base5$ProbaIncTM<- ProbaIncTMAA
base5$ProbaNoIncTM<- ProbaNoIncTMAA

### Seis Meses ###
base6<- filter(Datos2, Dias.por.Vencer > 90 & Dias.por.Vencer <= 180)

ProbaIncSMAA<- base6$Rendimiento / (1-0.25)
ProbaNoIncSMAA<- 1 - ProbaIncSMAA

base6$ProbaIncSM<- ProbaIncSMAA
base6$ProbaNoIncSM<- ProbaNoIncSMAA

### Nueve Meses ###
base7<- filter(Datos2, Dias.por.Vencer > 180 & Dias.por.Vencer <= 270)

ProbaIncNMAA<- base7$Rendimiento / (1-0.25)
ProbaNoIncNMAA<- 1 - ProbaIncNMAA

base7$ProbaIncSM<- ProbaIncNMAA
base7$ProbaNoIncSM<- ProbaNoIncNMAA

### 1 Y ###

base8<- filter(Datos2, Dias.por.Vencer > 270 & Dias.por.Vencer <= 360)

ProbaInc1A<- base8$Rendimiento / (1-0.25)
ProbaNoInc1A<- 1 - ProbaInc1A

base8$ProbaInc1A<- ProbaInc1A
base8$ProbaNoInc1A<- ProbaNoInc1A

### 3 Y ###

ProbaInc3AA<- Datos2$TresAños / (1-0.25)
ProbaNoInc3AA<- 1 - ProbaInc3AA

### 5 Y ###
ProbaInc5AA<- Datos2$CincoAños / (1-0.25)
ProbaNoInc5AA<- 1 - ProbaInc5AA

### 10 Y ###
ProbaInc10AA<- Datos2$DiezAños / (1-0.25)
ProbaNoInc10AA<- 1 - ProbaInc10AA

################################################# CALIFICACIÓN A ########################################
Datos1<-read.csv(paste(ruta,"Rendimientos_A.csv",sep = ""),header=TRUE)

Datos3<- as.data.frame(Datos1)

### Tres Meses ###
base9<- filter(Datos3, Dias.por.Vencer != 0 & Dias.por.Vencer <= 90)

ProbaIncTMA<- base9$Rendimiento / (1-0.25)
ProbaNoIncTMA<- 1 - ProbaIncTMA

base9$ProbaIncTM<- ProbaIncTMA
base9$ProbaNoIncTM<- ProbaNoIncTMA

### Seis Meses ###
ProbaInc6MA<- Datos3$SeisMeses / (1-0.25)
ProbaNoInc6MA<- 1 - ProbaInc6MA

