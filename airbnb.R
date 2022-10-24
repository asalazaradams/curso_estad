library(readr)
library(dplyr)
library(stringr)

#creamos un dataframe llamado "airbnb" con los datos listings.csv para la ciudad de México de
#http://insideairbnb.com/get-the-data/

airbnb <- read_csv("C:/Users/asalazar/Desktop/CURSO Estadística/listings.csv")

#visualizamos las primeras filas del dataframe
head(airbnb)
#visualizamos los nombres de las variables
names(airbnb)
#visualizamos las primeras filas de la variable "price" y vemos de que clase son:
head(airbnb$price)
class(airbnb$price)



airbnb$price=str_sub(airbnb$price,start=2)
airbnb$price

airbnb$price=gsub(",","",airbnb$price)
airbnb$price

airbnb$price=as.numeric(airbnb$price)
airbnb$price

table(airbnb$room_type)
airbnb2=airbnb%>%select(price,room_type,accommodates, bedrooms,
               neighbourhood_cleansed, amenities )

airbnb=na.omit(airbnb2)

write.csv(airbnb, file="airbnb.csv")
getwd()
summary(airbnb$price)

plot(log(airbnb$price)~airbnb$accommodates)
airbnb$logprice=log(airbnb$price)
model1=lm(logprice~accommodates,data=airbnb)
model2=lm(logprice~accommodates+bedrooms+room_type+neighbourhood_cleansed,data=airbnb)

summary(model1)
summary(model2)
airbnb$neighbourhood_cleansed

head(airbnb$amenities)

#Creamos variables dummy para algunas amenidades:
airbnb$pool=str_detect(airbnb$amenities,"Hot tub|Jetted tub|hot tub|Sauna|Pool|pool")
airbnb$gym=str_detect(airbnb$amenities,"gym|Gym")
airbnb$parking=str_detect(airbnb$amenities,"Free parking on premises")
airbnb$coffee=str_detect(airbnb$amenities,"coffee|Coffee")
airbnb$pets=str_detect(airbnb$amenities,'Pets')
airbnb$washer=str_detect(airbnb$amenities,'washer|Washer')
airbnb$breakfast=str_detect(airbnb$amenities,'Breakfast')

model3=lm(logprice~accommodates+bedrooms+room_type+
            pool+gym+parking+coffee+pets+washer+breakfast+
            neighbourhood_cleansed, data=airbnb)

summary(model3)

airbnb$amenities

#pets=airbnb%>%filter(pets==TRUE)

#pets$amenities

exp(6.29+2*0.115373+0.052755-0.421463+0.200823+0.184888)
