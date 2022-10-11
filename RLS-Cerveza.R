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

#Prueba de normalidad
#Ho: el error se distribuye normalmente
shapiro.test(beermodel1$residuals)


plot(beer$I,beer$Q)
beermodel2=lm(Q~I,data=beer)
summary(beermodel2)
shapiro.test(beermodel2$residuals)
