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
tamrein<-read_excel("Vertebrates/data/Livestock/tamrein.xlsx")

#Simplify
sau1<-sau %>% select(c(knr2017,AAR,ART,utmar,TOTBEITE,TOTBAR,TOTBARDAG))
geit1<-geit %>% select(c(knr2017,AAR,ART,utmar,TOTBEITE,TOTBAR,TOTBARDAG))
storfe1<- storfe %>% select(c(knr2017,AAR,ART,utmar,METBEITEOFF,TOTBAR,TOTBARDAG)) #Check METBEITEOFF column with Gunnar
names(storfe1)[names(storfe1)=="METBEITEOFF"]<-"TOTBEITE"
hest1<- hest %>% select(c(knr2017,AAR,ART,utmar,TOTBEITE,TOTBAR,TOTBARDAG))
tamrein1<-tamrein %>% select(c(knr2017,aar,tamrein,TOTBAR,TOTBARDAG))
names(tamrein1)[names(tamrein1)=="aar"]<-"AAR"
names(tamrein1)[names(tamrein1)=="tamrein"]<-"TOTBEITE"
tamrein1$ART<-rep("tamrein",times=nrow(tamrein))
tamrein1$utmar<-sau1$utmar[sau1$knr2017==tamrein1$knr2017]
tamrein1$TOTBEITE[is.na(tamrein1$TOTBEITE)]<-0

#Combine into 1
#all_livestock<-left_join(sau1,geit1,by=c("knr2017","AAR","utmar"),suffix=c("sau","geit"))
all_livestock<-bind_rows(sau1,geit1,storfe1,hest1,tamrein1)
as.factor(all_livestock$ART)

#Join the biomass data to the county data
sf_livestock<-full_join(norwaykom2017,all_livestock,by=c("KOMMUNENUM"="knr2017"))

#Test by plotting
plot(sf_livestock[sf_livestock$AAR==2015 & sf_livestock$ART=="sau",]["TOTBARDAG"],main="Sheep 2015",logz=T)
plot(sf_livestock[sf_livestock$AAR==1907 & sf_livestock$ART=="tamrein",]["TOTBARDAG"],main="Semidom rein 1907",logz=T)
ggplot()+geom_sf(data=sf_livestock[sf_livestock$ART=="tamrein",],aes(fill=TOTBARDAG),colour=NA)+scale_fill_gradient(trans='log')+facet_wrap(vars(AAR))

#Aggregate to counties
county_livestock<- sf_livestock %>% 
  group_by(FylkeNr,ART,AAR) %>%
  summarise(TotalMetBio=sum(TOTBEITE),TotalUtmark=sum(utmar))

county_livestock$MetabolicBiomassDensity<-(county_livestock$TotalMetBio/365)/county_livestock$TotalUtmark

ggplot()+geom_sf(data=county_livestock[county_livestock$ART=='geit',],aes(fill=MetabolicBiomassDensity),color=NA)+
  facet_wrap(vars(AAR))+scale_fill_gradient(trans='log')+ggtitle("Goat - county")
ggplot()+geom_sf(data=county_livestock[county_livestock$ART=='tamrein',],aes(fill=MetabolicBiomassDensity),color=NA)+
  facet_wrap(vars(AAR))+scale_fill_gradient()+ggtitle("SD reindeer - county")


#Check data
ggplot(data=sf_livestock[sf_livestock$ART=="sau",],aes(x=AAR,y=TOTBARDAG))+geom_line(aes(group=NAVN))+facet_wrap(vars(FylkeNr),scales='free_y')+ggtitle("Sheep")
ggplot(data=sf_livestock[sf_livestock$ART=="geit",],aes(x=AAR,y=TOTBARDAG))+geom_line(aes(group=NAVN))+facet_wrap(vars(FylkeNr),scales='free_y')+ggtitle("Goats")
ggplot(data=sf_livestock[sf_livestock$ART=="hest",],aes(x=AAR,y=TOTBARDAG))+geom_line(aes(group=NAVN))+facet_wrap(vars(FylkeNr),scales='free_y')+ggtitle("Horse")
ggplot(data=sf_livestock[sf_livestock$ART=="storf",],aes(x=AAR,y=TOTBARDAG))+geom_line(aes(group=NAVN))+facet_wrap(vars(FylkeNr),scales='free_y')+ggtitle("Cattle")
ggplot(data=sf_livestock[sf_livestock$ART=="tamrein",],aes(x=AAR,y=TOTBARDAG))+geom_line(aes(group=NAVN))+facet_wrap(vars(FylkeNr),scales='free_y')+ggtitle("Semi-domestic reindeer")



#Write county data as a shapefile
write_sf(county_livestock,"Vertebrates/data/Processed","LivestockdataCounty.shp",driver="ESRI Shapefile")
#write kom data as a shapefile
write_sf(sf_livestock,"Vertebrates/data/Processed","LivestockdataKommune.shp",driver="ESRI Shapefile")
