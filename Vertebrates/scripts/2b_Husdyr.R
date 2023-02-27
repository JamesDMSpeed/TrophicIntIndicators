library(readxl)
library(sf)
library(geodata)
#library(reshape2)
library(dplyr)
library(ggplot2)

#Import spatial polygons of the kommune

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


#Import the livestock data
sau<- read_excel("Vertebrates/data/Livestock/sau 1907-2015.xlsx")
geit<- read_excel("Vertebrates/data/Livestock/geit 1907-2015.xlsx")
storfe<- read_excel("Vertebrates/data/Livestock/storfe 1907-2015.xlsx")
hest<- read_excel("Vertebrates/data/Livestock/hest 1907-2015.xlsx")

#Simplify
sau1<-sau %>% select(c(knr2017,AAR,ART,utmar,TOTBEITE,TOTBAR,TOTBARDAG))
geit1<-geit %>% select(c(knr2017,AAR,ART,utmar,TOTBEITE,TOTBAR,TOTBARDAG))
storfe1<- storfe %>% select(c(knr2017,AAR,ART,utmar,METBEITEOFF,TOTBAR,TOTBARDAG)) #Check METBEITEOFF column with Gunnar
names(storfe1)[names(storfe1)=="METBEITEOFF"]<-"TOTBEITE"
hest1<- hest %>% select(c(knr2017,AAR,ART,utmar,TOTBEITE,TOTBAR,TOTBARDAG))

#Combine into 1
#all_livestock<-left_join(sau1,geit1,by=c("knr2017","AAR","utmar"),suffix=c("sau","geit"))
all_livestock<-bind_rows(sau1,geit1,storfe1,hest1)
as.factor(all_livestock$ART)

#Join the biomass data to the county data
sf_livestock<-full_join(norwaykom2017,all_livestock,by=c("KOMMUNENUM"="knr2017"))

#Test by plotting
plot(sf_livestock[sf_livestock$AAR==2015 & sf_livestock$ART=="sau",]["TOTBAR"],main="Sheep 2015",logz=T)

#Aggregate to counties
county_livestock<- sf_livestock %>% 
  group_by(FylkeNr,ART,AAR) %>%
  summarise(TotalMetBio=sum(TOTBEITE),TotalUtmark=sum(utmar))

county_livestock$MetabolicBiomassDensity<-county_livestock$TotalMetBio/county_livestock$TotalUtmark

ggplot()+geom_sf(data=county_livestock[county_livestock$ART=='geit',],aes(fill=MetabolicBiomassDensity),color=NA)+
  facet_wrap(vars(AAR))+scale_fill_gradient(trans='log')+ggtitle("Goat - county")


#Write county data as a shapefile
write_sf(county_livestock,"Vertebrates/data/Processed","LivestockdataCounty.shp",driver="ESRI Shapefile")
#write kom data as a shapefile
write_sf(sf_livestock,"Vertebrates/data/Processed","LivestockdataKommune.shp",driver="ESRI Shapefile")
