#Regresion Lineal Simple
agua=read.csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/agua_cons.csv")
cor(agua$consumo,agua$tarifa)
plot(agua$tarifa,agua$consumo)
text(agua$tarifa,agua$consumo,agua$localidad)

modelo1=lm (consumo~tarifa,data=agua)
plot(agua$tarifa,agua$consumo)
abline(modelo1)
modelo1