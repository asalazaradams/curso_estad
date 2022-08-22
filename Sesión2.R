#Sesion2

data()
?swiss
attach(swiss)
plot(Fertility,Education)
plot(Fertility,Infant.Mortality)
summary(Infant.Mortality)
hist(Infant.Mortality)
boxplot(Infant.Mortality)

library(ggplot2)
qplot(Infant.Mortality, geom = "histogram")
qplot(Infant.Mortality, geom = "histogram", colour = I("blue"))
qplot(Infant.Mortality, geom = "density", colour=I("green"))
?qplot


#Import dataset
library(readr)
Sonora_censo<- read_csv("~/Documents/R/iter_26_cpv2010/conjunto_de_datos/iter_26_cpv2010.csv")

#Seleccionar solo localidades
Sonora_loc <- Sonora_censo[ which(Sonora_censo$loc!="0000"&  Sonora_censo$loc!="9998" &  Sonora_censo$loc!="9999"), ]
mean(Sonora_loc$pobtot)
median(Sonora_loc$pobtot)

#Seleccionar solo localidades de Hermosillo
Hermosillo_loc <- Sonora_loc[ which(Sonora_loc$nom_mun=="Hermosillo"), ]
summary(Hermosillo_loc$pobtot)

#Ejercicio obtener tabla con la poblaci??n de los municipios
#Calcular % de viviendas con agua, drenaje...por municipio como nueva columna por cada indicador


