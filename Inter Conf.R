#Muestras grandes
x <- 5
s <- 2
n <- 50
alfa=.05
Z=qnorm(1-alfa/2)

error <- Z*s/sqrt(n)
lim.inf <- x-error
lim.sup <- x+error

lim.inf
lim.sup


#Muestras pequeñas, población normal
x <- 5
s <- 2
n <- 20
alfa=.05
t=qt(1-alfa/2,n-1)

error <- qt(.975,n-1)*s/sqrt(n-1)

qt(.975,n-1)

lim.inf <- x-error
lim.sup <- x+error

lim.inf
lim.sup


#proporciones
p <- .63
s <- sqrt(p*(1-p))
n <- 250
alfa=.05
Z=qnorm(1-alfa/2)

error <- Z*s/sqrt(n)

lim.inf <- p-error
lim.sup <- p+error

lim.inf
lim.sup

