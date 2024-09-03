german <- read.csv("~/Google Drive/CURSOS/Estadística/CURSO Estadística/German_Credit.csv")

#Usando el archivo del credito alemán "German credit"
#HAcer lo siguiente:

#Use group_by y sumarise para Calcular la 
#media, la desviacion estandar
#del monto de préstamo (loan_amount)
#para cada categoría de empleo (job category)
#calcule también cuantos deudores hay en cada categoría
#Indique que categoría tiene a mayor y 
#cual la menor cantidad de deudores


#Haga los mismos cáclulos pero agrupando por 
#genero/estado civil (gender_status)
#Propósito del crédito (purpose)


library(dplyr)

german %>% filter(loan_amount>2000)
german %>% select(loan_amount)


german %>% group_by(job_category)%>%
  summarise(n=n(),
            mean(loan_amount),
            sd(loan_amount))



german %>% group_by(job_category)%>%
  summarise(n=n(),
            mean(loan_amount),
            mean(age_yrs))

german %>% group_by(purpose)%>%
  summarise(n=n(),
            mean(loan_amount))

german %>% group_by(gender_status)%>%
  summarise(n=n(),
            mean(loan_amount))


summary(German_Credit$loan_amount)

summary(German_Credit$age_yrs)

hist(German_Credit$loan_amount)
boxplot(German_Credit$loan_amount)

boxplot(German_Credit_Preped$age_yrs)
hist(German_Credit_Preped$age_yrs)

boxplot(German_Credit$loan_amount~German_Credit$job_category)

plot(German_Credit$age_yrs,German_Credit$loan_amount)

boxplot(German_Credit$loan_amount~German_Credit$age_yrs)
