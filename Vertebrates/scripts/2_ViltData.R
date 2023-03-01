#Wild herbivores

library(readxl)
library(sf)
library(geodata)
#library(reshape2)
library(dplyr)
library(ggplot2)
library(data.table)

#Import forest cervid data
hjortevilt_metabolsk_vekt<- read_excel("Vertebrates/data/hjortevilt_metabolsk vekt 1907-2015B.xls")
View(hjortevilt_metabolsk_vekt)

#Importing older data with utmark area by kommune
#utmarkdat<-read.csv("Vertebrates/data/elg.csv",header=T)
utmarkdat<-fread("Vertebrates/data/elg.csv",header=T)
View(utmarkdat)

#Adding utmark area to each kommune
hjortevilt_metabolsk_vekt$utmark<-utmarkdat$utmar[match(hjortevilt_metabolsk_vekt$knr2017,utmarkdat$knr2017)]

#Calculating metabolicbiomass density (kg/km2 of utmark)
hjortevilt_metabolsk_vekt$MetabolicBiomassDensity<-hjortevilt_metabolsk_vekt$antmkg/hjortevilt_metabolsk_vekt$utmark

# Wild reindeer -----------------------------------------------------------


#Import wildreindeer data

#Wild reindeer data only goes back to 1949.
#However 2700 individuals in 1900s (Anderson et al. 2010)
villrein<-read_excel("Vertebrates/data/Livestock/villrein.xlsx")
villrein1 <- villrein %>% select(c(knr2017,kommune,aar,villrein,utmarea,TOTBAR,TOTBARDAG))
names(villrein1)
names(villrein1)<-c('kommunenr','NAVN','aar','individuals','utmark',"TOTBAR", "MetabolicBiomassDensity")
#Animals per year
plot(tapply(villrein1$individuals,villrein1$aar,sum),type='b')

#Here we need to interpolate the number of individuals per year from 1907 (2700) to the observed in 1949 of 25598
annualdecrease<-(25598-2700)/(1949-1907)#Decrease in reindeer of 545 per year from 1949
annualdecrease_pc<-545/25598 #2.1% decrease per year
#In 1938
villrein1938<-villrein1[villrein1$aar==1949,]
villrein1938$aar<-1938
villrein1938$individuals<-(100-2.1*11)/100*villrein1$individuals[villrein1$aar==1949]
villrein1938$MetabolicBiomassDensity<-(100-2.1*11)/100*villrein1$MetabolicBiomassDensity[villrein1$aar==1949]
#In 1929
villrein1927<-villrein1[villrein1$aar==1949,]
villrein1927$aar<-1927
villrein1927$individuals<-(100-2.1*22)/100*villrein1$individuals[villrein1$aar==1949]
villrein1927$MetabolicBiomassDensity<-(100-2.1*22)/100*villrein1$MetabolicBiomassDensity[villrein1$aar==1949]
#In 1917
villrein1917<-villrein1[villrein1$aar==1949,]
villrein1917$aar<-1917
villrein1917$individuals<-(100-2.1*32)/100*villrein1$individuals[villrein1$aar==1949]
villrein1917$MetabolicBiomassDensity<-(100-2.1*32)/100*villrein1$MetabolicBiomassDensity[villrein1$aar==1949]
#In 1907
villrein1907<-villrein1[villrein1$aar==1949,]
villrein1907$aar<-1907
villrein1907$individuals<-(100-2.1*42)/100*villrein1$individuals[villrein1$aar==1949]
villrein1907$MetabolicBiomassDensity<-(100-2.1*42)/100*villrein1$MetabolicBiomassDensity[villrein1$aar==1949]

#Group together
villrein_interp<-bind_rows(villrein1,villrein1938,villrein1927,villrein1917,villrein1907)
View(villrein_interp)
villrein_interp$art<-rep("villrein",times=nrow(villrein_interp))
villrein_interp$antmkg<-villrein_interp$MetabolicBiomassDensity*villrein_interp$utmark

ggplot(droplevels(villrein_interp),aes(x=aar,y= MetabolicBiomassDensity))+geom_line(aes(group=kommunenr))+geom_vline(xintercept=1949,lty=2)

#Combine wildreindeer with other species
hjortevilt1<-hjortevilt_metabolsk_vekt %>% select(c(knr2017,fylkesnr,aar,art,antmkg,utmark,MetabolicBiomassDensity))
#Rename villrein knr
names(villrein_interp)[names(villrein_interp)=="kommunenr"]<-"knr2017"

AllVilt<-bind_rows(hjortevilt1,villrein_interp[names(villrein_interp)%in%names(hjortevilt1)])
AllVilt


#2017 list of kommune
norwaykom2017<-st_read('Vertebrates/data/Kommuner_2017')
plot(norwaykom2017["NAVN"])
#Plot a single kommune
plot(norwaykom2017[norwaykom2017$KOMMUNENUM==1622,]["NAVN"])

#Add county numbers
korrespond<-fread("Vertebrates/data/Kom2017FylkeKorrsp.csv",header=T,sep=";",encoding = "UTF-8")
View(korrespond)

norwaykom2017$FylkeNr<-korrespond$sourceCode[match(norwaykom2017$KOMMUNENUM,korrespond$targetCode)]
norwaykom2017$FylkeName<-korrespond$sourceName[match(norwaykom2017$KOMMUNENUM,korrespond$targetCode)]

View(norwaykom2017)
ggplot()+geom_sf(data=norwaykom2017,aes(fill=factor(FylkeNr)))
st_write(norwaykom2017,"Vertebrates/data","Norway_adm_sf",driver = "ESRI Shapefile")  

#Join the biomass data to the county data
sf_ViltBio<-full_join(norwaykom2017,AllVilt,by=c("KOMMUNENUM"="knr2017"))

#Simplify by selecting 
sf_ViltBio <-sf_ViltBio %>% select(c(kommunenr,NAVN,FylkeNr,FylkeName,art,aar,antmkg,utmark,MetabolicBiomassDensity))

tapply(sf_ViltBio$kommunenr,sf_ViltBio$art,length)
tapply(sf_ViltBio$kommunenr,list(sf_ViltBio$art,sf_ViltBio$aar),length)#Different years for 1927/1929


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
tapply(countydat$MetabolicBiomassDensity,countydat$art,mean,na.rm=T)

ggplot()+geom_sf(data=countydat[countydat$art=='elg',],aes(fill=MetabolicBiomassDensity),color=NA)+facet_wrap(vars(aar))+scale_fill_gradient(trans='log')+ggtitle("Moose - county")
ggplot()+geom_sf(data=countydat[countydat$art=='villrein',],aes(fill=MetabolicBiomassDensity),color=NA)+facet_wrap(vars(aar))+scale_fill_gradient(trans='log')+ggtitle("Wild reindeer - county")


#Write county data as a shapefile
write_sf(countydat,"Vertebrates/data/Processed","ViltdataCounty.shp",driver="ESRI Shapefile")
#write kom data as a shapefile
write_sf(sf_ViltBio,"Vertebrates/data/Processed","ViltdataKommune.shp",driver="ESRI Shapefile")

