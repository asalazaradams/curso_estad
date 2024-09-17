library(foreign)

concen<- read.csv("C:/ncv_concentrado_2014_concil_2010.csv")#cambiar a directorio elegido usando setwd()

mean(concen$ing_cor)
sum(concen$ing_cor)

total.hogares=sum(concen$factor_hog)
ing.cor.total=sum(concen$factor_hog*concen$ing_cor)
ing.cor.promedio=ing.cor.total/total.hogares

library(survey)
concen$id_hog=paste(concen$folioviv,concen$foliohog, sep="")
diseño1 =  svydesign(id=concen$id_hog,weights=concen$factor_hog, data=concen)
svymean(~ing_cor,design=diseño1)
svytotal(~ing_cor,design=diseño1)
