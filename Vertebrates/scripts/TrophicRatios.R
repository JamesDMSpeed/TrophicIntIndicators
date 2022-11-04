#TrophicData

rm(list=ls())

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(tidyterra)

# 1 County level analysis of vertebrates-------------------------------------------------------
#Import
#Herbivores

vilt<-st_read("Vertebrates/data/Processed/ViltdataCounty.shp")
names(vilt)[4:6]<-c("TotalMetabolicBiomass","TotalUtmarkArea","MBD")

#Widen up to have seperate columns for each species
#pivot not working so need to use reshape, and then send back to a sf
#tidyr::pivot_wider(data=vilt,names_from = art, values_from = MetabolicBiomassDensity)
widevilt<-reshape(as.data.frame(vilt[,-which(names(vilt)=="TotalMetabolicBiomass")]),v.names = "MBD",direction='wide',idvar=c("aar","FylkeNr"),timevar = "art")
widevilt

countyyr<-select(vilt[vilt$art=='elg',], FylkeNr,aar,TotalUtmarkArea)

viltwide<-full_join(countyyr,widevilt)
viltwide


#Carnivores
carnivores<-read.csv("Vertebrates/data/carnivore_data.csv",header=T,sep=";",dec='.')

#Add county numbers 
carnivores$FylkeNr<-as.factor(carnivores$County)
carnivores$FylkeNr <-recode(carnivores$FylkeNr, "Akershus" =2  ,"Ostfold"=1, "Aust_Agder"=9,       "Buskerud"  =6,       "Finnmark"  =20,       "Hedmark"   =4,       "Hordaland" =12,       "More_og_Romsdal"=15,  "Nord_Trondelag"  =17,
                            "Nordland"  =18,       "Oppland"  =5,        "Rogaland" =11,        
                                   "Sogn_og_Fjordane"=14, "Sor_Trondelag"=16,    "Telemark" =8,        "Troms" =19,           "Vest_Agder"   =10,    "Vestfold"=7    )


#Select relevant years
#Wolf from 2015-16, others from 2015 so need to include both
carnivores$Year[carnivores$Species=="Wolf" & carnivores$Year=="2015/16"]<-2016

carnivores_sameyrs<-carnivores[carnivores$Year %in% c( "1906-10","1916-20","1926-30","1936-40" , "1946-50","1956-60","1966-70", "1976-80","1985/86-89/90","1995/96-99/00","2005/06-09/10","2016" ),] 
carnivores_sameyrs$YearMatch<-recode(carnivores_sameyrs$Year, "1906-10"=1907,"1916-20"=1917,"1926-30"=1929,"1936-40"=1938 , "1946-50"=1949,"1956-60"=1959,"1966-70"=1969, "1976-80"=1979,"1985/86-89/90"=1989,"1995/96-99/00"=1999,
                                                              "2005/06-09/10"=2009, "2016" =2015)



#Make wide by species
carnivore_wide<-pivot_wider(carnivores_sameyrs,id_col=c(Year,YearMatch,County,FylkeNr),names_from = Species,values_from=MBD,names_prefix = "MBD_")

#Join
viltcarn<-full_join(viltwide,carnivore_wide,by=c("aar"="YearMatch","FylkeNr"="FylkeNr"))
viltcarn



# Calculate biomass ratios ------------------------------------------------


#Example with wolf and moose
viltcarn$wolf_moose<-viltcarn$MBD_Wolf/viltcarn$MBD.elg
ggplot()+geom_sf(data=viltcarn,aes(fill=wolf_moose),color=NA)+facet_wrap(vars(aar))+ggtitle("Wolf/Moose MBD ratio")+scale_fill_gradient(trans='log')

#Forest vilt (red deer, roe deer, moose) and carnivores (bear, lynx, wolf)
viltcarn$forest_carn_vilt<-(viltcarn$MBD_Wolf+viltcarn$MBD_Lynx+viltcarn$MBD_Bear)/(viltcarn$MBD.elg+viltcarn$MBD.hjort+viltcarn$MBD.roe)
ggplot()+geom_sf(data=viltcarn,aes(fill=forest_carn_vilt),color=NA)+facet_wrap(vars(aar))+ggtitle("Forest carnivore:Cervids \n Metabolic biomass density ratio")+scale_fill_gradient(trans='log')
ggsave("Vertebrates/outputs/ForestCarnCervids.png")



# 2. Rasterized analyses ------------------------------------------------------

#NPP
#Read in
nppyrfiles<-list.files("Vertebrates/data/NPP/Net_PP_Yearly_500m_v6/Npp",full.names = T)

nppstack<-rast(nppyrfiles)
names(nppstack)<-2000:2021
nppstack[nppstack>32000]<-NA

#Mask out the non-norway regions
nppstack_m<-mask(nppstack,viltKomwide)

ggplot()+geom_spatraster(data=nppstack_m,aes(fill=2000))


#Vilt

viltKom<-st_read("Vertebrates/data/Processed/ViltdataKommune.shp")
names(viltKom)
names(viltKom)[8:10]<-c("TotalMetabolicBiomass","TotalUtmarkArea","MBD")

#Widen up to have seperate columns for each species
#pivot not working so need to use reshape, and then send back to a sf
df1<-viltKom %>% select(kommnnr,art,aar,MBD,geometry   )
viltKomWide<-tidyr::pivot_wider(data=df1,names_from = "art", values_from = "MBD")
#wideviltKom<-reshape(as.data.frame(viltKom[,-which(names(viltKom)=="TotalMetabolicBiomass")]),v.names = "MBD",direction='wide',idvar=c("aar","FylkeNr"),timevar = "art")
#wideviltKom

#viltKomwide<-full_join(countyyr,wideviltKom)
viltKomWide


#Rasterize a given species and year
elg2015<-rasterize(viltKomWide[viltKomWide$aar==2015,],nppstack_m[[1]],field="elg")
ggplot()+geom_spatraster(data=elg2015)

veg_elg2015<-nppstack_m$`2015`/(elg2015+1)
ggplot()+geom_spatraster(data=veg_elg2015)
