?glm
model.logit1=glm(credit~Income, family=binomial (link = "logit"),
    data=income_credit_nuevo)

INGRESO=25
p=exp((-3.5179+0.10541*INGRESO))/(1+exp(-3.5179+0.10541*INGRESO))
p
exp(0.10541)
p5=3.5179/0.10541
p5
summary(model.logit1)
model.logit2=glm(auto~dtime,   family=binomial (link = "logit"),
data=transport)
model.logit2
exp(0.05311)
p50=0.23758/0.05311
p50
MINUTOS=0
pauto=exp((-0.2375+0.05311*MINUTOS))/(1+exp(-0.2375+0.05311*MINUTOS))
pauto
