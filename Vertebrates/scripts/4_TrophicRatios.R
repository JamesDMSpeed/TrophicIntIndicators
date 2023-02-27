#TrophicData

rm(list=ls())

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(tidyterra)
library(gridExtra)

# 1 County level analysis of vertebrates-------------------------------------------------------
#Import
#Herbivores

vilt<-st_read("Vertebrates/data/Processed/ViltdataCounty.shp")
names(vilt)[4:6]<-c("TotalMetabolicBiomass","TotalUtmarkArea","MBD")
livestock<-st_read("Vertebrates/data/Processed/LivestockdataCounty.shp")
names(livestock)[4:6]<-c("TotalMetabolicBiomass","TotalUtmarkArea","MBD")


#Widen up to have seperate columns for each species
#pivot not working so need to use reshape, and then send back to a sf
#tidyr::pivot_wider(data=vilt,names_from = art, values_from = MetabolicBiomassDensity)
widevilt<-reshape(as.data.frame(vilt[,-which(names(vilt)=="TotalMetabolicBiomass")]),v.names = "MBD",direction='wide',idvar=c("aar","FylkeNr"),timevar = "art")
widevilt
widelivestock<-reshape(as.data.frame(livestock[,-which(names(livestock)=="TotalMetabolicBiomass")]),v.names = "MBD",direction='wide',idvar=c("AAR","FylkeNr"),timevar = "ART")
widelivestock

countyyr<-select(vilt[vilt$art=='elg',], FylkeNr,aar,TotalUtmarkArea)

viltwide<-full_join(countyyr,widevilt)
viltwide
livestockwide<-full_join(countyyr,widelivestock)
livestockwide

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

viltlivestockcarn<-st_join(viltcarn,livestockwide,by=c("aar"=="AAR","FylkeNr"="FylkeNr"))
viltlivestockcarn

  # Calculate biomass ratios ------------------------------------------------


#County level

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
names(nppstack)<-paste0("NPP_",2000:2021)
nppstack[nppstack>32000]<-NA

#Vilt

viltKom<-st_read("Vertebrates/data/Processed/ViltdataKommune.shp")
names(viltKom)
names(viltKom)[8:10]<-c("TotalMetabolicBiomass","TotalUtmarkArea","MBD")

#Widen up to have seperate columns for each species
#pivot not working so need to use reshape, and then send back to a sf
df1<-viltKom %>% select(kommnnr,art,aar,MBD,geometry   )
viltKomWide<-tidyr::pivot_wider(data=df1,names_from = "art", values_from = "MBD")
viltKomWide
#Sum up vilt species biomasses
viltKomWide<-viltKomWide %>% mutate(Vilt= elg+hjort+roe)

#Livestock
livestockKom<-st_read("Vertebrates/data/Processed/LivestockdataKommune.shp")
names(livestockKom)
livestockKom$MBD<-livestockKom$TOTBAR/livestockKom$utmar
#Widen up to have seperate columns for each species
#pivot not working so need to use reshape, and then send back to a sf
dfL1<-livestockKom %>% select(kommunenr,ART,AAR,MBD,geometry   )
livestockKomWide<-tidyr::pivot_wider(data=dfL1,names_from = "ART", values_from = "MBD")
livestockKomWide
#Sum up livestock species biomasses
livestockKomWide<-livestockKomWide %>% mutate(Livestock= sau+geit+hest+storf)



#Mask out the non-norway regions of NPP
nppstack_m<-mask(nppstack,viltKomWide)

ggplot()+geom_spatraster(data=nppstack_m,aes(fill=NPP_2000))+scale_fill_continuous(na.value=NA)


#Rasterize a given species and year
elg2015<-rasterize(viltKomWide[viltKomWide$aar==2015,],nppstack_m[[1]],field="elg")
vilt2015<-rasterize(viltKomWide[viltKomWide$aar==2015,],nppstack_m[[1]],field="Vilt")
livestock2015<-rasterize(livestockKomWide[livestockKomWide$AAR==2015,],nppstack_m[[1]],field='Livestock')
ggplot()+geom_spatraster(data=elg2015)+scale_fill_continuous(na.value=NA)
ggplot()+geom_spatraster(data=livestock2015)+scale_fill_continuous(na.value=NA,trans='log')

#Ratio of vegetation NPP to moose biomass
veg_elg2015<-nppstack_m$`NPP_2015`/(elg2015+1)
ggplot()+geom_spatraster(data=veg_elg2015)+scale_fill_continuous(na.value=NA,trans='log')

#Ratio of vegetation NPP to vilt biomass
veg_vilt2015<-nppstack_m$`NPP_2015`/(vilt2015+1)
ggplot()+geom_spatraster(data=veg_vilt2015)+scale_fill_viridis_c(trans='log',na.value="transparent")+theme_bw()
plot(nppstack_m$`NPP_2015`,vilt2015)

#Rasterize some carnivores
viltcarn<-viltcarn %>% mutate(forestcarns=MBD_Bear+MBD_Wolf+MBD_Lynx)

forestcarn2015_r<-rasterize(viltcarn[viltcarn$aar==2015,],nppstack_m[[1]],field="forestcarns")

vilt_forcar_2015<-vilt2015/(forestcarn2015_r+1)
ggplot()+geom_spatraster(data=vilt_forcar_2015)+scale_fill_viridis_c(breaks=c(0,2,20,200),"Metabolic biomass\nratio",trans='log',na.value=NA)+
  theme_bw()+ggtitle("Forest cervids:carnivore 2015")
ggsave("Vertebrates/outputs/ForestCervidsCarnivores.png")

#Alternative colours
#ggplot()+geom_spatraster(data=vilt_forcar_2015)+scale_fill_continuous(breaks=c(0,2,20,200),"Metabolic biomass\nratio",trans='log',na.value=NA)+
#  theme_bw()+ggtitle("Forest cervids:Forest carnivores - 2015")


g1<-ggplot()+geom_spatraster(data=veg_vilt2015)+scale_fill_viridis_c(breaks=c(0.8,8,80,800,8000),"Biomass ratio",trans='log',na.value="transparent")+
  theme_bw()+ggtitle("NPP:Forest cervids 2015")
g1
g2<-ggplot()+geom_spatraster(data=vilt_forcar_2015)+scale_fill_viridis_c(breaks=c(0,2,20,200),"Metabolic biomass\nratio",trans='log',na.value=NA)+
  theme_bw()+ggtitle("Forest cervids:carnivore 2015")
g2
#Ugly
grid.arrange(g1,g2,ncol=2)


#Rasterize all species and years
#Vilt

viltannual<-pivot_wider(viltKomWide,names_from = "aar",values_from = c("roe","elg",'hjort','Vilt'))

viltspp <- names(viltannual)[3:50]
viltannual_raster <- lapply(viltspp, function(x) {
  rasterize(viltannual, nppstack_m[[1]],
            field = x,
            touches = TRUE
  )
})
viltannual_raster<-do.call("c",viltannual_raster)
viltannual_raster

#Plot from this object
ggplot()+geom_spatraster(data=transmute(viltannual_raster,tt1=Vilt_2015-Vilt_1989))

#Carnivores
v1<-viltcarn
names(v1)[9:12]<-c('bear','wolf','lynx','wolverine')
carnannual<-pivot_wider(select(v1,aar,FylkeNr,bear,wolf,lynx,wolverine),names_from = 'aar',values_from = c('bear','wolf','lynx','wolverine'))

carnspp <- names(carnannual)[3:50]
carnannual_raster <- lapply(carnspp, function(x) {
  rasterize(carnannual, nppstack_m[[1]],
            field = x,
            touches = TRUE
  )
})
carnannual_raster<-do.call("c",carnannual_raster)
carnannual_raster


#Writing Rasters - NB large files
writeRaster(nppstack_m,"Vertebrates/data/TrophicBiomassData/NPP.tiff")
writeRaster(viltannual_raster,"Vertebrates/data/TrophicBiomassData/Vilt.tiff")
writeRaster(carnannual_raster,"Vertebrates/data/TrophicBiomassData/Carnivores.tiff")



