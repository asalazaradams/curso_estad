#Datos originales descargados de:
#https://datos.covid-19.conacyt.mx/#DownZCSV
#Seleccionar "todos los datos" y dar click en descargar

setwd("~/Documents/ESTADISTICAS MEXICO/COVID/")
library(readr)
library(dplyr)
COVID19MEX <- read_csv("200724COVID19MEXICO.csv")

#revisamos los resultados:
table(COVID19MEX$RESULTADO)
#Descargamos el diccionario para ver que representa cada categoria.
#Se ubica en la parte superior  derecha del sitio

#Filtramos para conservar solo los casos positivos:
COVID19MEX=COVID19MEX%>%filter(RESULTADO==1)

#recodificamos variables de interés, con la funcion recode de dplyr
COVID19MEX$SEXO = recode (COVID19MEX$SEXO,  `1` = "hombre",`2` = "mujer")

table(COVID19MEX$DIABETES)
COVID19MEX$DIABETES = recode (COVID19MEX$DIABETES,  `1` = "Si",`2` = "No", `98`="No_esp")

table(COVID19MEX$HIPERTENSION)
COVID19MEX$HIPERTENSION = recode (COVID19MEX$HIPERTENSION,  `1` = "Si",`2` = "No", `98`="No_esp")

table(COVID19MEX$OBESIDAD)
COVID19MEX$OBESIDAD = recode (COVID19MEX$OBESIDAD,  `1` = "Si",`2` = "No", `98`="No_esp")

table(COVID19MEX$INTUBADO)
COVID19MEX$INTUBADO = recode (COVID19MEX$INTUBADO,  `1` = "Si",`2` = "No",`97`="No", `99`="No")

#Creamos una variable categórica para indicar 
#que el paciente falleció

#Primero, identifica si no existe fecha de defunción
COVID19MEX$DEF=is.na(COVID19MEX$FECHA_DEF)
#convertimos a formato de caracter
COVID19MEX$DEF=as.character(COVID19MEX$DEF)
#recodificamos: si la fecha de defunción es NA, 
#DEF tendrá un valor de TRUE, por lo tanto, 
#el paciente no ha muerto
#Si es FALSE, quiere decir que si hay feha de defunción

COVID19MEX$DEF = recode (COVID19MEX$DEF,  "TRUE" = "No","FALSE" = "Si")

#Finalmente convertimos la variable en formato factor para que lo
#identifique como variable categorica
COVID19MEX$DEF =as.factor(COVID19MEX$DEF)

boxplot(EDAD~DEF, data=COVID19MEX)

COVID19MEX$TIPO_PACIENTE=as.factor(COVID19MEX$TIPO_PACIENTE)

COVID19MEX=COVID19MEX%>%filter(OBESIDAD!="No_esp",
                               HIPERTENSION!="No_esp",
                               DIABETES!="No_esp")


hist(COVID19MEX$EDAD)
summary(COVID19MEX$EDAD)             
boxplot(COVID19MEX$EDAD)
qqnorm(COVID19MEX$EDAD)
sd(COVID19MEX$EDAD)

modelo1=glm(DEF~EDAD,family=binomial, data=COVID19MEX)
summary(modelo1)

modelo2=glm(DEF~EDAD+SEXO,family=binomial, data=COVID19MEX)
summary(modelo2)

modelo3=glm(DEF~SEXO+EDAD+DIABETES+HIPERTENSION+OBESIDAD,family=binomial, data=COVID19MEX)
summary(modelo3)
exp(0.575)
exp(0.283)

modelo4=glm(DEF~SEXO+EDAD+DIABETES+HIPERTENSION+OBESIDAD+TIPO_PACIENTE+INTUBADO,family=binomial, data=COVID19MEX)
summary(modelo4)
exp(2.65)
exp(1.75)
exp(.36)

