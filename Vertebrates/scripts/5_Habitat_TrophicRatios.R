#Habitat specific trophic ratios

rm(list=ls())

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(tidyterra)
library(gridExtra)



#In this script we make some indicators for specific habitat types
#We do not yet have a suitable habitat map across Norway, so as an interrim solution, we use AR50

#Import AR50 data
#AR50 Shapefile

#ar50shp<-st_read("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\0000_25833_ar50_gdb.gdb","org_ar_ar50_flate",method="ONLY_CCW")
ar50shp<-st_read("Vertebrates/data/AR50/0000_25833_ar50_gdb.gdb","org_ar_ar50_flate")
ar50shp
#ggplot()+geom_sf(data=ar50shp,aes(fill=artype))


#AR50 type raster
#artype50<-rast("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\AR50_artype_25_ETRS_1989_UTM_Zone_33N.tif")
artype50<-rast("Vertebrates/AR50/AR50_artype_25_ETRS_1989_UTM_Zone_33N.tif")
ggplot()+geom_spatraster(data=artype50)+scale_fill_viridis_c(na.value = NA)

#Reclassify
hist(artype50)
artype50_F<-artype50
arTdf<-data.frame(id=c(10,20,30,50,60,70,81,82,128),label=c("BuiltUp","Pastures/Arable","Forest","Open","Mires","Ice","Freshwater","Sea","Unmapped"))
levels(artype50_F)<-arTdf

arcols<-c("black","orange","darkgreen","wheat","lightblue","white","blue","blue",NA)
ggplot()+geom_spatraster(data=artype50_F)+scale_fill_manual(values=arcols,na.value = NA)


#Norway County data for outlines
norcounty_shp<-st_read("Vertebrates/data/Processed/","ViltdataCounty")
#Simplify by county nr
norcounty<-norcounty_shp %>% 
  group_by(FylkeNr) %>%
  summarise(geometry = st_union(geometry)) 

norcounty$CountyName<-norcounty$FylkeNr
norcounty$CountyName<-c("Østfold","Akershus","Oslo","Hedmark","Oppland","Buskerud","Vestfold","Telemark","Aust-Agder","Vest-Agder","Rogaland","Hordaland","Sogn og Fjordane",
                        "Møre og Romsdal","Sør-Trøndelag","Nord-Trøndelag","Nordland","Troms","Finnmark")

#Make spatVect 
norcounty_vect<-vect(norcounty)

ggplot()+geom_sf(data=norcounty)

#Here we read in the rasters of the different trophic biomass levels
npp<-rast("Vertebrates/data/TrophicBiomassData/NPP.tiff")
vilt<-rast("Vertebrates/data/TrophicBiomassData/Vilt.tiff")
carnivores<-rast("Vertebrates/data/TrophicBiomassData/Carnivores.tiff")


#Functions for estimating biomass ratios

#SImple function to add 1 to numerator and denominator of ratios
add1<-function(x)(x+1)

#Years for the animal data
allyears_vect<-c(1907,1917,1929,1938,1949,1959,1969,1979,1989,1999,2009,2015)
allyears<-list(1907,1917,1929,1938,1949,1959,1969,1979,1989,1999,2009,2015)
names(allyears)<-paste0("Y_",allyears)

carnivore_herbivore_biomass_function_allyrs<-function(carnivorerast,herbivorerast,carnivore_species,herbivore_species){
  carnsppyr<-lapply(allyears,function(x)paste(as.list(carnivore_species),x,sep="_"))
  herbsppyr<-lapply(allyears,function(x)paste(as.list(herbivore_species),x,sep="_"))
  #  print(carnsppyr)
  #  print(herbsppyr)
  carnsum<-lapply(carnsppyr,function(x) app(carnivorerast[[names(carnivorerast) %in% x]],sum))
  herbsum<-lapply(herbsppyr,function(x) app(herbivorerast[[names(herbivorerast) %in% x]],sum))
  # herbsum<-sum(herbivorerast[[names(herbivorerast) %in% herbsppyr]])
  # plot(carnsum[[1]]) 
  #plot(herbsum[[1]])
  #  print(carnsum)
  ratio=mapply("/",lapply(herbsum,add1),lapply(carnsum,add1))
  return(rast(ratio))
}

#Function to make indicator of herbivore to vegetation for all herbivore years
#We use 2000 for NPP and 1999 for herbivores
nppyrs<-c(2000,2009,2015)
herbyrs<-c(1999,2009,2015)
herbyrslist<-as.list(herbyrs)
names(herbyrslist)<-paste0("Y_",herbyrs)
vegetation_herbivore_biomass_function_allyears<-function(herbivorerast,npprast,herbivore_species){
  herbsppyr<-lapply(herbyrslist,function(x)paste(as.list(herbivore_species),x,sep="_"))
  nppyr<-npprast[[paste("NPP",vegyrs,sep="_")]]
  print(herbsppyr)
  print(nppyr)
  herbsum<-rast(lapply(herbsppyr,function(x) app(herbivorerast[[names(herbivorerast) %in% x]],sum)))
  print(herbsum)
  # ratio=rast(herbsum)
  #   ratio=mapply("/",lapply(nppyr,add1),lapply(herbsum,add1))
  ratio=(nppyr)/(herbsum+1)
  names(ratio)<-names(herbyrslist)
  return(ratio)
}


#Habitat specific polygons

#Forest
forest_ar50<-ar50shp[ar50shp$artype==30,]

#Mask the trophic biomass layers to the habitat
npp1<-mask(npp,norcounty)#First mask out non-norway cells

forest_npp<-mask(npp1,forest_ar50)#Then mask all non-forest cells
ggplot()+geom_spatraster(data=forest_npp$NPP_2000)+ggtitle("Forest")
forest_vilt<-mask(vilt,forest_ar50)
forest_carnivore<-mask(carnivores,forest_ar50)

plot(2000:2021,global(forest_npp,fun=mean,na.rm=T)$mean,type='b',main="NPP in forest")

#Open areas (both alpine and lowland in AR50)
openarea_ar50<-ar50shp[ar50shp$artype==50,]

openhab_npp<-mask(npp1,openarea_ar50)
ggplot()+geom_spatraster(data=openhab_npp$NPP_2000)+ggtitle("Open")
openhab_vilt<-mask(vilt,openarea_ar50)
openhab_carnivore<-mask(carnivores,openarea_ar50)

#Innmarkbeite
innmark_ar50<-ar50shp[ar50shp$artype==20,]
innmark_npp<-mask(npp1,innmark_ar50)
ggplot()+geom_spatraster(data=innmark_npp$NPP_2000)+ggtitle("Agri")
innmark_vilt<-mask(vilt,innmark_ar50)
innmark_carnivore<-mask(carnivores,innmark_ar50)


# Old stuff - delete ------------------------------------------------------


#

# #Forest
# #1907. 
# #Herbivores: Roe deer, red deer, moose
# #Carnivores: Wolf, Bear, Lynx
# forest1907_C_H<-(forest_carnivore$lynx_1907+forest_carnivore$bear_1907+forest_carnivore$wolf_1907)/(forest_vilt$roe_1907+forest_vilt$hjort_1907+forest_vilt$elg_1907)
# forest2015_V_H<-(forest_npp$NPP_2015+1)/(forest_vilt$roe_2015+forest_vilt$hjort_2015+forest_vilt$elg_2015+1)
# forest2015_H_V<-(forest_vilt$roe_2015+forest_vilt$hjort_2015+forest_vilt$elg_2015+1)/((forest_npp$NPP_2015/1000000000)+1)
# 
# 
# ggplot()+geom_sf(data=norcounty,fill="white",lwd=0.1)+
#   geom_spatraster(data=forest1907_C_H)+scale_fill_gradient(na.value=NA)
# 
# ggplot()+geom_spatraster(data=forest2015_V_H)+scale_fill_gradient(na.value=NA,breaks=c(0,10^7.5,10^8,10^8.5,10^9,10^10))
# ggplot()+geom_spatraster(data=forest2015_H_V)+scale_fill_gradient(na.value=NA,breaks=c(0,0.001,0.1,0.2,0.5,1,10,100),trans='log')
# 
# 
# #Function to make indicators on basis of lists of species for a single year
# carnivore_herbivore_biomass_function<-function(carnivorerast,herbivorerast,year,carnivore_species,herbivore_species){
#   carnsppyr<-paste(carnivore_species,year,sep="_")
#   herbsppyr<-paste(herbivore_species,year,sep="_")
#   print(carnsppyr)
#   print(herbsppyr)
#   carnsum<-sum(carnivorerast[[names(carnivorerast) %in% carnsppyr]])
#   herbsum<-sum(herbivorerast[[names(herbivorerast) %in% herbsppyr]])
#   #plot(carnsum) 
#   carnsum
#   herbsum
#   ratio=(herbsum+1)/(carnsum+1)
#   return(ratio)
#   }
# 
# HC_forest_2015<-carnivore_herbivore_biomass_function(carnivorerast=forest_carnivore,herbivorerast=forest_vilt,
#                                      year=2015,
#                                      carnivore_species=c('lynx','wolf','bear'),
#                                      herbivore_species=c('elg','roe','hjort'))
# 
# HC_forest_1917<-carnivore_herbivore_biomass_function(carnivorerast=forest_carnivore,herbivorerast=forest_vilt,
#                                                      year=1917,
#                                                      carnivore_species=c('lynx','wolf','bear'),
#                                                      herbivore_species=c('elg','roe','hjort'))
# 
# HC_forest<-c(HC_forest_1917,HC_forest_2015)
# names(HC_forest)<-c('1907','2015')
# ggplot()+geom_sf(data=norcounty,fill='white',lwd=0.1)+
#   geom_spatraster(data=HC_forest)+facet_wrap(~lyr)+
#   scale_fill_gradient(na.value=NA)


# #Function to make indicator of herbivore to vegetation
# vegetation_herbivore_biomass_function<-function(herbivorerast,npprast,year,herbivore_species){
#   herbsppyr<-paste(herbivore_species,year,sep="_")
#   nppyr<-npprast[[paste("NPP",year,sep="_")]]
#   print(herbsppyr)
#   herbsum<-sum(herbivorerast[[names(herbivorerast) %in% herbsppyr]])
#   print(herbsum)
#   ratio=(nppyr+1)/(herbsum+1)
# }
#   
# VH_forest_2015<-vegetation_herbivore_biomass_function(herbivorerast = forest_vilt,
#                                                       npprast=forest_npp,
#                                                       year=2015,
#                                                       herbivore_species = c('elg','roe','hjort'))
# ggplot()+geom_sf(data=norcounty,fill='white',lwd=0.1)+
#   geom_spatraster(data=VH_forest_2015)+
#   scale_fill_gradient(na.value=NA,breaks=c(0,10000000,20000000,30000000,10000000000))
# 
# 




# Producing indicators - forest ----------------------------------------------------


#Forest herbivores and vegetation
forest_herbivores_veg<-vegetation_herbivore_biomass_function_allyears(herbivorerast=forest_vilt,
                                                                    npprast=forest_npp,
                                                                    herbivore_species=c("elg","hjort","roe"))

forest_herbivores_veg
ggplot()+geom_sf(data=norcounty,fill="white",lwd=0.1)+
  geom_spatraster(data=forest_herbivores_veg)+facet_grid(~lyr)+
  scale_fill_distiller(palette = "YlOrRd",
                      na.value=NA,
                     # breaks=as.numeric(apply(global(forest_herbivores_veg,quantile,na.rm=T),2,max)),
                      limits=c(0,max(global(forest_herbivores_veg,quantile,probs=0.95,na.rm=T))))

#Simple plot
plot(c(1999,2009,2015),global(forest_herbivores_veg,mean,na.rm=T)$mean,type='b')

#County level data
forest_herb_veg_county<-extract(forest_herbivores_veg,norcounty_vect,mean,na.rm=T)
forest_herb_veg_county$ID<-norcounty_vect$FylkeNr
forest_herb_veg_county$CountyName<-norcounty_vect$CountyName
forest_herb_veg_countyDF<-gather(forest_herb_veg_county,key=Year,Y_1999,Y_2009,Y_2015,value=Ratio,-CountyName)
forest_herb_veg_countyDF$YearN<-as.numeric(substr(forest_herb_veg_countyDF$Year,3,7))
ggplot(data=forest_herb_veg_countyDF,aes(x=YearN,y=Ratio,group=as.factor(ID),color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=forest_herb_veg_countyDF$CountyName)+
  ggtitle("Forest Ratio of NPP:Herbivore biomass")


#Herbivores and carnivores
forest_carnivores_herbivores<-carnivore_herbivore_biomass_function_allyrs(carnivorerast = forest_carnivore,
                                                herbivorerast = forest_vilt,
                                                carnivore_species = c('wolf','lynx','bear'),
                                                herbivore_species = c('elg','roe','hjort'))
forest_carnivores_herbivores

ggplot()+geom_spatraster(data=forest_carnivores_herbivores)+facet_wrap(~lyr)+scale_fill_gradient(na.value=NA)+
  ggtitle("Forest Trophic biomass ratio Herbivores:Carnivores")+theme_bw()

yearlymeans<-global(forest_carnivores_herbivores,fun="mean",na.rm=T)
yearlyquantiles<-global(forest_carnivores_herbivores,fun=quantile,na.rm=T)
plot(allyears_vect,yearlymedian$mean,type='b')
lines(allyears_vect,yearlyquantiles$X50.,type="b",col=2)


forest_carn_herb_county<-extract(forest_carnivores_herbivores,norcounty_vect,mean,na.rm=T)
forest_carn_herb_county$ID<-norcounty_vect$FylkeNr#Replace IDs with Fylke Nrs
forest_carn_herb_county$CountyName<-norcounty_vect$CountyName#Replace IDs with Fylke Nrs
forest_carn_herb_countyDF<-gather(forest_carn_herb_county,key="Year",value="Ratio",-ID,-CountyName)
forest_carn_herb_countyDF$YearN<-as.numeric(substr(forest_carn_herb_countyDF$Year,3,7))
ggplot(data=forest_carn_herb_countyDF,aes(x=YearN,y=Ratio,color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=forest_carn_herb_countyDF$CountyName)+
  ggtitle("Forest Ratio of Herbivore:Carnivore biomass")



# #Indicators - open habitat (alpine and lowland) -------------------------



#Open herbivores and vegetation
open_herbivores_veg<-vegetation_herbivore_biomass_function_allyears(herbivorerast=openhab_vilt,
                                                                      npprast=openhab_npp,
                                                                      herbivore_species=c("hjort","roe"))

ggplot()+geom_sf(data=norcounty,fill="white",lwd=0.1)+
  geom_spatraster(data=open_herbivores_veg)+facet_wrap(~lyr)+scale_fill_gradient(na.value = NA)+
  ggtitle("Open habitat biomass ratio NPP/Herbivores")


open_herb_veg_county<-extract(open_herbivores_veg,norcounty_vect,mean,na.rm=T)
open_herb_veg_county$ID<-norcounty_vect$FylkeNr
open_herb_veg_county$CountyName<-norcounty_vect$CountyName
open_herb_veg_countyDF<-gather(open_herb_veg_county,key=Year,Y_1999,Y_2009,Y_2015,value=Ratio,-CountyName)
open_herb_veg_countyDF$YearN<-as.numeric(substr(open_herb_veg_countyDF$Year,3,7))
ggplot(data=open_herb_veg_countyDF,aes(x=YearN,y=Ratio,group=as.factor(ID),color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=open_herb_veg_countyDF$CountyName)+
  ggtitle("open Ratio of NPP:Herbivore biomass")


open_carnivores_herbivores<-carnivore_herbivore_biomass_function_allyrs(carnivorerast = forest_carnivore,
                                                                       herbivorerast = forest_vilt,
                                                                       carnivore_species = c('wolf','lynx'),
                                                                       herbivore_species = c('roe','hjort'))

ggplot()+geom_sf(data=norcounty,fill="white",lwd=0.1)+
  geom_spatraster(data=open_carnivores_herbivores)+facet_wrap(~lyr)+scale_fill_gradient(na.value=NA)+
  ggtitle("Open habitat trophic biomass ratio Herbivores/Carnivores")

open_carn_herb_county<-extract(open_carnivores_herbivores,norcounty_vect,mean,na.rm=T)
open_carn_herb_county$ID<-norcounty_vect$FylkeNr#Replace IDs with Fylke Nrs
open_carn_herb_county$CountyName<-norcounty_vect$CountyName#Replace IDs with Fylke Nrs
open_carn_herb_countyDF<-gather(open_carn_herb_county,key="Year",value="Ratio",-ID,-CountyName)
open_carn_herb_countyDF$YearN<-as.numeric(substr(open_carn_herb_countyDF$Year,3,7))
ggplot(data=open_carn_herb_countyDF,aes(x=YearN,y=Ratio,color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=open_carn_herb_countyDF$CountyName)+
  ggtitle("Open habitats - Ratio of Herbivore:Carnivore biomass")
