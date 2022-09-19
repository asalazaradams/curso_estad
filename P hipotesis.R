#Ejemplo 1

x=261
m=270
s=24
n=38
alfa=.05
Z=(x-m)/(s/sqrt(n))
Z

qnorm(alfa)


#Ejemplo 2

x=18.5
m=18
s=3.6
n=36
alfa=.05
t=(x-m)/(s/sqrt(n))

t

qt(alfa,n-1, lower.tail = F)


#Ejemplo 3
x=13.6
m=12
s=4.3
n=64
Z=(x-m)/(s/sqrt(n))
Z

qnorm(alfa/2, lower.tail = F)


#Ejemplo 4
p <- .63
s <- sqrt(p*(1-p))
n <- 250
alfa=.05

Z=qnorm(alfa/2,lower.tail =F)
Z


#Ejemplo 5
pollos=c(4.41,  4.37, 4.33,  4.35, 4.30, 4.39, 4.36, 4.38, 4.40, 4.39)
x=mean(pollos)
x
m=4.35
s=.0339
n=length(pollos)
n
alfa=.01
t=(x-m)/(s/sqrt(n))
t

tt=qt(alfa,n-1,lower.tail =F)
tt

?qt
pt(t,n-1,lower.tail =F )

