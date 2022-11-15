#Wild herbivores

library(readxl)
library(sf)
library(geodata)
#library(reshape2)
library(dplyr)
library(ggplot2)

hjortevilt_metabolsk_vekt<- read_excel("Vertebrates/data/hjortevilt_metabolsk vekt 1907-2015B.xls")
View(hjortevilt_metabolsk_vekt)

#Importing older data with utmark area by kommune
utmarkdat<-read.csv("Vertebrates/data/elg.csv",header=T)
View(utmarkdat)

#Adding utmark area to each kommune
hjortevilt_metabolsk_vekt$utmark<-utmarkdat$utmar[match(hjortevilt_metabolsk_vekt$knr2017,utmarkdat$knr2017)]

#Calculating metabolicbiomass density (kg/km2 of utmark)
hjortevilt_metabolsk_vekt$MetabolicBiomassDensity<-hjortevilt_metabolsk_vekt$antmkg/hjortevilt_metabolsk_vekt$utmark

#2017 list of kommune
norwaykom2017<-st_read('Vertebrates/data/Kommuner_2017')
plot(norwaykom2017["NAVN"])
#Plot a single kommune
plot(norwaykom2017[norwaykom2017$KOMMUNENUM==1622,]["NAVN"])

#Add county numbers
korrespond<-read.csv("Vertebrates/data/Kom2017FylkeKorrsp.csv",header=T,sep=";",encoding = "UTF-8")
View(korrespond)

norwaykom2017$FylkeNr<-korrespond$sourceCode[match(norwaykom2017$KOMMUNENUM,korrespond$targetCode)]
norwaykom2017$FylkeName<-korrespond$sourceName[match(norwaykom2017$KOMMUNENUM,korrespond$targetCode)]

View(norwaykom2017)
ggplot()+geom_sf(data=norwaykom2017,aes(fill=factor(FylkeNr)))
st_write(norwaykom2017,"Vertebrates/data","Norway_adm_sf",driver = "ESRI Shapefile")  

#Join the biomass data to the county data
sf_ViltBio<-full_join(norwaykom2017,hjortevilt_metabolsk_vekt,by=c("KOMMUNENUM"="knr2017"))

#Simplify by selecting 
sf_ViltBio <-sf_ViltBio %>% select(c(kommunenr,NAVN,FylkeNr,FylkeName,art,aar,antkg,antmkg,utmark,MetabolicBiomassDensity))

#Plot one species and year
plot(sf_ViltBio[sf_ViltBio$art=='elg' & sf_ViltBio$aar==2015,]["MetabolicBiomassDensity"],main="Moose 2015")

ggplot()+geom_sf(data=sf_ViltBio[sf_ViltBio$art=='elg' & sf_ViltBio$aar==1907,],mapping=aes(fill=MetabolicBiomassDensity),color=NA)

ggplot()+geom_sf(data=sf_ViltBio[sf_ViltBio$aar<1945,],aes(fill=MetabolicBiomassDensity+0.1))+facet_grid(vars(aar),vars(art))+scale_fill_gradient(trans="log")

ggplot()+geom_sf(data=sf_ViltBio[sf_ViltBio$art=="elg",],mapping=aes(fill=MetabolicBiomassDensity+0.1),color=NA)+facet_wrap(vars(aar))+scale_fill_gradient(trans="log")+ggtitle("Moose")
ggsave("Vertebrates/outputs/Moose_MetbioKom.png")

ggplot()+geom_sf(data=sf_ViltBio[sf_ViltBio$art=="hjort",],aes(fill=MetabolicBiomassDensity+0.1),color=NA)+facet_wrap(vars(aar))+scale_fill_gradient(trans="log")+ggtitle("Red deer")
ggsave("Vertebrates/outputs/RedDeer_MetbioKom.png")

ggplot()+geom_sf(data=sf_ViltBio[sf_ViltBio$art=="roe",],aes(fill=MetabolicBiomassDensity+0.1),color=NA)+facet_wrap(vars(aar))+scale_fill_gradient(trans="log")+ggtitle("Roe deer")
ggsave("Vertebrates/outputs/RoeDeer_MetbioKom.png")

#Aggregate by county
countydat<- sf_ViltBio %>% 
  group_by(FylkeNr,art,aar) %>%
  summarise(TotalMetBio=sum(antmkg),TotalUtmark=sum(utmark))
#countydat<-aggregate.data.frame(sf_ViltBio[,c(antmkg,utmark)])

countydat$MetabolicBiomassDensity<-countydat$TotalMetBio/countydat$TotalUtmark
ggplot()+geom_sf(data=countydat[countydat$art=='elg',],aes(fill=MetabolicBiomassDensity),color=NA)+facet_wrap(vars(aar))+scale_fill_gradient(trans='log')+ggtitle("Moose - county")


#Write county data as a shapefile
write_sf(countydat,"Vertebrates/data/Processed","ViltdataCounty.shp",driver="ESRI Shapefile")
#write kom data as a shapefile
write_sf(sf_ViltBio,"Vertebrates/data/Processed","ViltdataKommune.shp",driver="ESRI Shapefile")
