setwd("C:/Users/asalazar/Desktop/Rcode")
#INFO
#https://www.inegi.org.mx/programas/enoe/15ymas/default.html#Microdatos
#Documentos: Conociendo la base, 

library(dplyr)
library(survey)
library(foreign)
sdem<-read.dbf("SDEMT319.dbf")

names(sdem)
table(sdem$CLASE2)
sum(sdem$FAC)
class(sdem$EDA)

sdem$EDA=as.numeric(as.character(sdem$EDA))
class(sdem$EDA)

#Población total
#R_DEF:Resultado definitivo de la entrevista "00"=completada

#C_RES: 1=Residente habitual, 2=Ausente definitivo, 3=Nuevo residente

sdem %>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3)%>%
  summarise(POBLACION=sum(FAC))

#edad
#99=no especificada
#98=no especificada para mayores de 12 años o más
#97 o más
#Población de 15 años o más
sdem %>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3,
                    EDA>=15 & EDA<=98)%>%
             summarise(POBLACION_15omas=sum(FAC))


#Población de 15 años o más
sdem %>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3,
                    EDA>=15 & EDA<=98)%>%
  summarise(POBLACION_15omas=sum(FAC))


#Población Economicamente activa
sdem%>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3,
                    EDA>=15 & EDA<=98,
                    CLASE1==1)%>%
  summarise(PEA=sum(FAC))

#PEA Ocupada
sdem%>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3,
                    EDA>=15 & EDA<=98,
                    CLASE1==1,
                    CLASE2==1)%>%
  summarise(PEA_Ocupada=sum(FAC))

#PEA Subordinados y remunerados
sdem%>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3,
                    EDA>=15 & EDA<=98,
                    CLASE1==1,
                    CLASE2==1,
                    POS_OCU==1)%>%
  summarise(PEA_Subor=sum(FAC))

#PEA EMPLEADORES
empleadores=sdem %>% filter(R_DEF=="00",
                    C_RES==1|C_RES==3,
                    EDA>=15 & EDA<=98,
                    CLASE1==1,
                    CLASE2==1,
                    POS_OCU==2)%>%
  summarise(PEA_Empleadores=sum(FAC))

#Seleccionamos solo a la población ocupada, con remuneración
ocupados=sdem %>% 
  filter(CLASE2 == 1,
         ING_X_HRS>0,
         sdem$C_RES==1 | sdem$C_RES==3,
         EDA>=15 & EDA<=98)%>%
  select(FAC,EST_D, UPM,ING_X_HRS,INGOCUP,HRSOCUP, EDA, SEX,ENT)




library(survey)
options(survey.lonely.psu="adjust")

#Especificación de la muestra compleja (estratificada y multietápica)
ds_ocupados<- svydesign(id = ~UPM, strata = ~EST_D, weights = ~FAC, nest=TRUE, data=ocupados)

#Obtener media de ingreso
svymean(~INGOCUP, ds_ocupados)
confint(svymean(~INGOCUP, ds_ocupados))

svyby(~INGOCUP, by=~SEX, design=ds_ocupados, svymean)

svyby(~HRSOCUP, by=~SEX, design=ds_ocupados, svymean)

svyby(~ING_X_HRS, by=~SEX, design=ds_ocupados, svymean)

svyby(~INGOCUP, by=~ENT, design=ds_ocupados, svymean)


confint(svyby(~INGOCUP, by=~SEX, design=ds_ocupados, svymean))
t1<-svyttest(INGOCUP~SEX, ds_ocupados)
t1

confint(svyby(~ING_X_HRS, by=~SEX, design=ds_ocupados, svymean))
t2<-svyttest(ING_X_HRS~SEX, ds_ocupados)
t2



#PEA EMPLEADORES
empleadores=sdem %>% filter(R_DEF=="00",
                            C_RES==1|C_RES==3,
                            EDA>=15 & EDA<=98,
                            CLASE1==1,
                            CLASE2==1,
                            POS_OCU==2)%>%
          select(FAC,EST_D, UPM,ING_X_HRS,INGOCUP,HRSOCUP, EDA, SEX,ENT)



ds_empleadores<- svydesign(id = ~UPM, strata = ~EST_D, weights = ~FAC, nest=TRUE, data=empleadores)

confint(svyby(~INGOCUP, by=~SEX, design=ds_empleadores, svymean))
t1<-svyttest(INGOCUP~SEX, ds_empleadores)
t1

confint(svyby(~ING_X_HRS, by=~SEX, design=ds_empleadores, svymean))
t2<-svyttest(ING_X_HRS~SEX, ds_empleadores)
t2

svyby(~ING_X_HRS, by=~SEX, design=ds_empleadores, svymean)
