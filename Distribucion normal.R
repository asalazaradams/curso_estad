curve(dnorm(x, 0, 1), -3, 3)
?dnorm
abline(v = 2, col = "red")
abline(v = -1, col = "red")
?pnorm
#Ejemplo
pnorm(2)
pnorm(0)

pnorm(2)-pnorm(0)
pnorm(1, lower.tail = FALSE)




#Problema1
pnorm(0)-pnorm(-1.5)

#Problema2
pnorm(1, lower.tail = FALSE)

#Problema3
pnorm(1)
pnorm(-2)

pnorm(1)-pnorm(-2)

#problema7
qnorm(.1)


#percentiles
qnorm(.9)
qnorm(.75)


#Area bajo la curva
area=function(a,b){
x=seq(-3,3,length=200)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l")
x=seq(a,b,length=100)
y=dnorm(x,mean=0,sd=1)
polygon(c(a,x,b),c(0,y,0),col="red")}

area(-4,1)

