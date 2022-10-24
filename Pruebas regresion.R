#Regresión
#Cerveza
beer=read.csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/beer.csv")
#beer=beer[-1,]
plot(beer$PB,beer$Q)
beermodel1=lm(Q~PB,data=beer)
abline(beermodel1)
hist(beermodel1$residuals)
plot(beermodel1)

summary(beermodel1)


library(lmtest)
#Prueba de homoscedasticidad
#Ho: varianza es igual
bptest(beermodel1)

#Prueba de normalidad
#Ho: el error se distribuye normalmente

shapiro.test(beermodel1$residuals)


plot(beer$I,beer$Q)
beermodel2=lm(Q~I,data=beer)
summary(beermodel2)
shapiro.test(beermodel2$residuals)

#Regresión
#Cerveza
beer=read.csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/beer.csv")
#beer=beer[-1,]
plot(beer$PB,beer$Q)
beermodel1=lm(Q~PB,data=beer)
summary(beermodel1)
abline(beermodel1)

plot(beermodel1)


library(lmtest)
#Prueba de homoscedasticidad
#Ho: varianza es igual
bptest(beermodel1)
bptest(beermodel2)

#Prueba de normalidad
#Ho: el error se distribuye normalmente
shapiro.test(beermodel1$residuals)


plot(beer$I,beer$Q)
beermodel2=lm(Q~I,data=beer)
summary(beermodel2)
shapiro.test(beermodel2$residuals)



house=read.csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/house-prices.csv")
#HOUSE
plot(house$Price,house$SqFt)

model.house1= lm(Price~SqFt,data=house)
model.house1
plot(model.house1)
shapiro.test(model.house1$residuals)
bptest(model.house1)

hist(house$Price)
hist(log(house$Price))



library(foreign)
library(sandwich)
library(lmtest)

UN_data=read.csv("https://raw.githubusercontent.com/asalazaradams/data/main/UN-data.csv")

model.1=lm(Fert~GDP,data=UN_data)
plot(model.1)
shapiro.test(model.1$residuals)
bptest(model.1)

model.2=lm(Fert~log(GDP),data=UN_data)
plot(model.2)
shapiro.test(model.2$residuals)
bptest(model.2)

model.3=lm(log(Fert)~log(GDP),data=UN_data)
plot(model.3)
shapiro.test(model.3$residuals)
bptest(model.3)

summary(model.3)

#Errores estandar robustos
coeftest(model.3, vcov = vcovHC(model.3, "HC1"))    # robust; HC1 (Stata default)

