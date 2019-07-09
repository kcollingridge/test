#######################################  Biodiversity work EM   ###################################
# adaptation of script by Rui Viera #

#### install packages ####

install.packages ("tidyverse")
library (tidyverse)

install.packages("vegan")
library (vegan)

install.packages("iNEXT")
library (iNEXT)


input<-"C:/Users/EM11/Documents/DY99_Specimin_Recording"
setwd(input)

# row.names=1, use this if the first column of your data are the names of your rows (i.e."Sample 1").
#Para que me considere las especies como nombres de la fila tengo que añadir row.names=1. Ahora puedo transponer sin que dé problemas.
Especies<- read.csv(file="Darwin_pivot.csv",sep=",", row.names = 1)

Especies2<- t(Especies)
head(Especies2)


################################## Species Accumulations Curve #############################
#The classic method is "random" which finds the mean SAC and its standard deviation from random
# permutations of the data, or subsampling without replacement (Gotelli & Colwell 2001).
Spa <- specaccum(Especies2, method="random")

plot(Spa, ylab="Number of taxa", xlab="Number of trawls",lwd=2, cex.axis=1.5, cex.lab= 1.5)     #plots the species accumulation curve and the confidence intervals for sites.

#plot(Spa, mci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue") #males a prettier plot

boxplot(Spa, col="yellow", add=TRUE, pch="+")

# this function calculates the number of species for each of the sites
Se <- specnumber(Especies2)

######################################### Rarefacción ####################################################

# Preparo la matriz en el formato que quiere iNEXT.
#Tipo de datos "incidence row"

Gorringe <- Especies[,1:9]
Josephine <- Especies[,10:18]
Seine <- Especies[,19:21]
Oceanic <- Especies[,c(1:3,10,12,19:20)]        
Seamount <- Especies[,c(4:9,11,13:18,21)] 
Epipelagic <- Especies[,c(5,7,11:12,14,16:18,21)]
Mesopelagic <- Especies[,c(1:4,6,8:10,13,15,19:20)]
Day  <- Especies[,c(2:4,6:7,9:11,13,19,21)]
Night <- Especies[,c(1,5,8,12,14:18,20)]


Monte <- list (Gorringe=Gorringe, Josephine=Josephine , Seine=Seine)
Habitat<- list (Oceanic=Oceanic, Seamount= Seamount)
Depth<- list (Epipelagic=Epipelagic, Mesopelagic= Mesopelagic)
Ligth<- list (Day=Day, Night= Night)

INEXT_s<-iNEXT(Monte, q=0, datatype="incidence_raw", endpoint=20)
INEXT_h<-iNEXT(Habitat, q=0, datatype="incidence_raw", endpoint=20)
INEXT_d<-iNEXT(Depth, q=0, datatype="incidence_raw", endpoint=20)
INEXT_l<-iNEXT(Ligth, q=0, datatype="incidence_raw", endpoint=20)



#DataInfo: Summarize the data set.
#T=número de unidades muestrales (en este caso estaciones)
# S.obs =número de especies observadas
#SC =sample coverage estimate

#Para cada monte
ggiNEXT(INEXT_s)
ggiNEXT(INEXT_s, type=2)
ggiNEXT(INEXT_s, type=3)

plot (INEXT) #Consigo los gráficos de este paquete con el clásico diseño de R

#Para cada hábitat
ggiNEXT(INEXT_h)
ggiNEXT(INEXT_h, type=2)
ggiNEXT(INEXT_h, type=3)


#Para meso- e epipelágico
ggiNEXT(INEXT_d)
ggiNEXT(INEXT_d, type=2)
ggiNEXT(INEXT_d, type=3)

#Para dia e noite
ggiNEXT(INEXT_l)
ggiNEXT(INEXT_l, type=2)
ggiNEXT(INEXT_l, type=3)



estimateD()
#This function compute diversity estimateswith q = 0, 1, 2 (all three levels of q are reported)
#for any particular level of sample size or any specified level ofsample coverage for either 
#abundance data or incidence data.


##########################################################################################################
                                        ####   FIN   ####
##########################################################################################################                                         




         