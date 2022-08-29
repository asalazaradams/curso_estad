install.packages("dplyr")
library(dplyr)

#install.packages("tibble")
#library(tibble)
data(starwars)
View(starwars)
#cuatro funciones que facilitan el manejo de datos:
#filter, select, group_by, summarize 

#Función filter: permite seleccionar observaciones
#que tenga un valor específico para una variable 

#podemos seleccionar por ejemplo, solo a aquellos que son de la 
#especie (species) droide (Droid)
filter(starwars, species == "Droid")

#otra manera de llamar la función es a través de los "pipes" 
# o tuberías (%>%)

#El codigo siguiente hace exactamente los mismo que el 
starwars %>% filter(species == "Droid")

#Esta sintaxis nos servirá después para elaborar 
#instrucciones más complejas, por lo que será la que
#vamos utilizar en adelante

#las funciones en dplyr generan un archivo 
#temporal denominado tibble con el que podemos trabajar

#Si queremos guardar esos datos debemos crear un nuevo data frame:
swdroids=starwars %>% filter(species == "Droid")
swhuman=starwars %>% filter(species =="Human")

#podemos indicar que excluya a los droides usando !=
starwars %>% 
  filter(species != "Droid")

#podemos seleccionar a droides y humanos
#usando | (que significa "or")

starwars %>% 
  filter(species == "Human" | species== "Droid")


starwars %>% 
  filter(species != "Human" | species != "Droid")

#podemos seleccionar individuos que pesan menos de 80 
starwars %>% 
  filter(mass<80)

#podemos seleccionar humanos que pesan menos de 80
starwars %>% 
  filter(species=="Human", 
         mass<80)

#Ejercicio:
#Seleccione a los individuos que cumplan con 
#las siguientes características:
  #color de pelo: todos, menos negro
  #Estatura:mayor al promedio
  #color de ojos: azul y amarillo

m.height=mean(starwars$height,na.rm = TRUE)

starwars %>% 
  filter(hair_color!="black",
         height>174.358,
         eye_color=="blue"|
         eye_color=="yellow")

#select: nos permite seleccionar solamente 
#las variables uq nos interesan

starwars %>% select(name, species,mass, height )
starwars_X=starwars %>% select(name, species,mass, height )

#dplyr nos permite combinar funciones usando %>%
#vamos a combinar filter y select

starwars %>% 
  filter(species == "Droid")%>%
  select(name, height, mass)

swdroids2=starwars %>% 
  filter(species == "Droid")%>%
  select(name, height, mass)

#summarise nos permite obtener estadísticas de nuestra
starwars %>% summarise (mean.peso = mean(mass,na.rm = TRUE),
                       sd.peso = sd (mass,na.rm=TRUE),
                       mean.estatura= mean(height,na.rm=TRUE))

#combinando summarise con group_by,podemos obtener estadísticas 
#por grupos de acuerdo a alguna característica:

starwars %>%
  group_by(species) %>%
  summarise(
    mass = mean(mass)
    ) 


#ignorar N.A.:

starwars %>%
  group_by(species) %>%
  summarise(
    mass = mean(mass,na.rm = TRUE)) 

swmedias=starwars %>%
  group_by(species) %>%
  summarise(
    n=n(),
    mass = mean(mass,na.rm = TRUE),
    height= mean(height,na.rm=TRUE)
  ) 


#Otra función útil:
#rename
starwars = starwars %>% rename (planeta=homeworld)

read.csv()

write.csv(swmedias, file="swmedias.csv")

setwd("/Users/elcolegiodesonora/Google Drive/CURSOS/Estadística/CURSO Estadística")

getwd()


