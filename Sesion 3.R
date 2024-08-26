#Importar el dataset
library(readr)
Sonora_censo<- read_csv("~/Documents/R/iter_26_cpv2010/conjunto_de_datos/iter_26_cpv2010.csv")

#Seleccionar solo municipios
library(dbplyr)
#Identificar el formato de cada variable en el data frame
str(Sonora_censo)
?lapply
Sonora_censo[11:200] <- lapply(Sonora_censo[11:200], as.numeric) 
str(Sonora_mun)
Sonora_censo$entidad=as.character(Sonora_censo$entidad)

Sonora_censo$clave_municipio=paste0(Sonora_censo$entidad,Sonora_censo$mun)

Sonora_mun= Sonora_censo[which(Sonora_censo$loc=="0000" & Sonora_censo$mun!="000"),]

Sonora_mun= Sonora_censo%>%
            filter (loc=="0000",
                    mun!="000")%>%
            select(clave_municipio,nom_mun,pobtot)


#Varios datos que deberian ser numericos esten como caracter
#La siguiente linea cambia el formato de las columnas 11 a 200 (a partir de la variable"pobmas)
# de character a numeric, con la funcion "lapply"

Sonora_loc_urb= Sonora_censo%>%
  filter (loc != c("0000","9999","9998"),
          pobtot>=2500)%>%
  select (clave_municipio,nom_mun,pobtot)


#Exportar Sonora_mun como archivo csv
write.csv(Sonora_mun, file = "~/Documents/R/iter_26_cpv2010/conjunto_de_datos/Sonora_mun.csv")
