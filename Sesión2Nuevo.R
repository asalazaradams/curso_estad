#Sesion2

#Solución a ejercicio de sesión 1:
nombre=c("Hemotto","Benita","Alfonsina","Dali", "Luna", "Porota")
nombre
peso=c(8,4.5,5,2.35,NA, 2.85)
largo=c(88,78,93,73,57,72)
cola=c(31,26,33,29,23,27)
cuello=c(28,18,25,16,12,18)
cintura=c(52,45,40,28,24,31)

gatos=data.frame(nombre,peso,largo,cola,cuello,cintura)

class(gatos)

#Sesión 2
#Parte 1 - Manejo básico de Data Frames
#introducir una nueva variable "sexo" Completar comando de la siguiente linea:

gatos$sexo=c("Macho","Hembra","Hembra","Macho","Hembra","Hembra")

gatos


#Se puede crear una nueva variable a partir de otras contenidas en el data frame
gatos$cuerpo=gatos$largo-gatos$cola


#Ejercicio: crear variable "pct.cola" que indique que porcentaje 
#del largo del gato corresponde a la cola
gatos$pct.cola=gatos$cola/gatos$largo*100
gatos

#R contiene sus propios conjuntos de datos que pueden ser consultados mediante la siguiente función:
data()


#Podemos consultar la información de cada conjunto de datos. Examinemos el llamado "swiss"
?swiss
swiss
swiss$Fertility
mean (swiss$Fertility)
mean (swiss$Education)
mean (swiss$Agriculture)

?plot

plot(swiss$Fertility,swiss$Education)
plot(gatos$peso,gatos$cuerpo)
plot(swiss$Fertility,swiss$Infant.Mortality)


summary(swiss$Infant.Mortality)

quantile(swiss$Infant.Mortality,
         probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9))

quantile(swiss$Infant.Mortality,
         probs=c(0,0.1,.5,.9,1))

sd(swiss$Infant.Mortality)
var(swiss$Infant.Mortality)

hist(swiss$Infant.Mortality)
boxplot(swiss$Infant.Mortality)
boxplot(swiss$Education)

hist(swiss$Education)
summary(swiss$Education)
sd(swiss$Education)

sd(swiss$Education)/mean(swiss$Education)

sd(swiss$Infant.Mortality)/
  mean(swiss$Infant.Mortality)


library(ggplot2)
qplot(swiss$Fertility,swiss$Education)
qplot(data=swiss,Fertility,Education )
qplot(data=swiss,Infant.Mortality, geom = "histogram")
qplot(data=swiss,Infant.Mortality, geom = "histogram", colour = I("blue"))

ggplot()+geom_point(data = swiss, aes(x = Education))
ggplot()+geom_histogram(data=swiss, aes(x = Education))
ggplot()+geom_histogram(data=swiss, aes(x = Education), bins=8)
ggplot()+geom_histogram(data = swiss, aes(x = Education),
                        color="green",fill= "blue")
ggplot()+geom_boxplot(data = swiss, aes(y = Fertility))

?qplot


#Import dataset
#Importar a través del Environment-Import Dataset

#
house<- read_csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/house-prices.csv")
lapply(house,class)
mean(house$Price)
summary(house$Price)
quantile(house$Price)

ggplot()+geom_boxplot(data = house, 
                      aes(y = Price))

ggplot()+geom_boxplot(data = house, 
                      aes(y = Price,
                      x=Brick))


