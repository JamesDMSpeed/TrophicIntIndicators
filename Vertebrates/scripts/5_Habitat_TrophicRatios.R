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

ar50shp<-st_read("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\0000_25833_ar50_gdb.gdb","org_ar_ar50_flate",method="ONLY_CCW")
#ar50shp<-st_read("Vertebrates/data/AR50/0000_25833_ar50_gdb.gdb","org_ar_ar50_flate")
ar50shp
#ggplot()+geom_sf(data=ar50shp,aes(fill=artype))


#AR50 type raster
artype50<-rast("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\AR50_artype_25_ETRS_1989_UTM_Zone_33N.tif")
#artype50<-rast("Vertebrates/AR50/AR50_artype_25_ETRS_1989_UTM_Zone_33N.tif")
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
norcounty$CountyName<-c("_stfold","Akershus","Oslo","Hedmark","Oppland","Buskerud","Vestfold","Telemark","Aust-Agder","Vest-Agder","Rogaland","Hordaland","Sogn og Fjordane",
                        "M_re og Romsdal","S_r-Tr_ndelag","Nord-Tr_ndelag","Nordland","Troms","Finnmark")

#Make spatVect 
norcounty_vect<-vect(norcounty)

ggplot()+geom_sf(data=norcounty)

#Here we read in the rasters of the different trophic biomass levels
npp<-rast("Vertebrates/data/TrophicBiomassData/NPP.tiff")
vilt<-rast("Vertebrates/data/TrophicBiomassData/Vilt.tiff")
carnivores<-rast("Vertebrates/data/TrophicBiomassData/Carnivores.tiff")
livestock<-rast("Vertebrates/data/TrophicBiomassData/Livestock.tiff")

#Functions for estimating biomass ratios

#SImple function to add 1 to numerator and denominator of ratios
add1<-function(x)(x+1)

#Years for the animal data
allyears_vect<-c(1907,1917,1929,1938,1949,1959,1969,1979,1989,1999,2009,2015)
allyears<-list(1907,1917,1929,1938,1949,1959,1969,1979,1989,1999,2009,2015)
names(allyears)<-paste0("Y_",allyears)

#Function for estimating biomass Proportion by listing species of herbivores and carnivores
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
  prop=mapply("/",carnsum,lapply(herbsum,add1))
  return(rast(prop))
}
#

#Function for estimating biomass ratio by listing species of herbivores and carnivores
#Function to make indicator of herbivore to vegetation for all herbivore years
#We use 2000 for NPP and 1999 for herbivores
nppyrs<-c(2000,2009,2015)
herbyrs<-c(1999,2009,2015)
herbyrslist<-as.list(herbyrs)
names(herbyrslist)<-paste0("Y_",herbyrs)
#This function produces a ratio of higher level: lower level
#vegetation_herbivore_biomass_function_allyears<-function(herbivorerast,npprast,herbivore_species){
  herbsppyr<-lapply(herbyrslist,function(x)paste(as.list(herbivore_species),x,sep="_"))
  nppyr<-npprast[[paste("NPP",nppyrs,sep="_")]]
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
#This function produces a proportion of lowerlevel/upper level
vegetation_herbivore_biomass_function_allyears<-function(herbivorerast,npprast,herbivore_species){
  herbsppyr<-lapply(herbyrslist,function(x)paste(as.list(herbivore_species),x,sep="_"))
  nppyr<-npprast[[paste("NPP",nppyrs,sep="_")]]
  print(herbsppyr)
  print(nppyr)
  herbsum<-rast(lapply(herbsppyr,function(x) app(herbivorerast[[names(herbivorerast) %in% x]],sum)))
  print(herbsum)
  # ratio=rast(herbsum)
  #   ratio=mapply("/",lapply(nppyr,add1),lapply(herbsum,add1))
  prop=(herbsum)/(nppyr+1)
  names(prop)<-names(herbyrslist)
  return(prop)
}



# Masking layers to different landcovers ----------------------------------
#Habitat specific polygons

#Combine vilt and livestock
allherbivores<-c(vilt,livestock)

#Mask the trophic biomass layers to the habitat
npp1<-mask(npp,norcounty)#First mask out non-norway cells
#Resample the ar50 to the npp cells 
artype50_FR<-project(resample(artype50_F,npp1,method='near'),npp1)
artype50_FR_vert<-project(resample(artype50_F,allherbivores,method='near'),allherbivores)

compareGeom(artype50_FR,allherbivores,ext=T,res=T,rowcol=T)

#Then mask all non-forest cells
forest_npp<-mask(npp1,artype50_FR,maskvalues=30,inverse=T)
ggplot()+geom_spatraster(data=forest_npp$NPP_2000)+ggtitle("Forest")
forest_allherbivores<-mask(allherbivores,artype50_FR_vert,maskvalues=30,inverse=T)
plot(allherbivores$elg_2015)
plot(forest_allherbivores$elg_2015)
forest_carnivore<-mask(carnivores,artype50_FR,maskvalues=30,inverse=T)
plot(forest_carnivore$bear_1907)

plot(2000:2021,global(forest_npp,fun=mean,na.rm=T)$mean,type='b',main="NPP in forest")

#Open areas (both alpine and lowland in AR50)
openhab_npp<-mask(npp1,artype50_FR,maskvalues=50,inverse=T)
ggplot()+geom_spatraster(data=openhab_npp$NPP_2000)+ggtitle("Open")
openhab_allherbivores<-mask(allherbivores,artype50_FR,maskvalues=50,inverse=T)
openhab_carnivore<-mask(carnivores,artype50_FR,maskvalues=50,inverse=T)

#Innmarkbeite
innmark_npp<-mask(npp1,artype50_FR,maskvalues=20,inverse=T)
ggplot()+geom_spatraster(data=innmark_npp$NPP_2000)+ggtitle("Agri")
innmark_allherbivores<-mask(allherbivores,artype50_FR,maskvalues=20,inverse=T)
innmark_carnivore<-mask(carnivores,artype50_FR,maskvalues=50,inverse=T)
ggplot()+geom_spatraster(data=innmark_carnivore$lynx_2015)

# Producing indicators - forest ----------------------------------------------------
#Forest herbivores and vegetation
forest_herbivores_veg<-
  vegetation_herbivore_biomass_function_allyears(herbivorerast=forest_allherbivores,
                                                                    npprast=forest_npp,
                                                                    herbivore_species=c("elg","hjort","roe","storf"))

forest_herbivores_veg
ggplot()+geom_sf(data=norcounty,fill="white",lwd=0.1)+
  geom_spatraster(data=forest_herbivores_veg)+facet_grid(~lyr)+
  scale_fill_distiller(palette = "YlOrRd",
                      na.value=NA,
                     limits=c(0,max(global(forest_herbivores_veg,quantile,probs=0.95,na.rm=T))))+ggtitle("Forest_NPP/Herbivores")
writeRaster(forest_herbivores_veg,"Vertebrates/data/Processed/forest_herbivores_veg.tiff")

#Simple plot
plot(c(1999,2009,2015),global(forest_herbivores_veg,mean,na.rm=T)$mean,type='b')

#County level data
forest_herb_veg_county<-extract(forest_herbivores_veg,norcounty_vect,mean,na.rm=T)
forest_herb_veg_county$ID<-norcounty_vect$FylkeNr
forest_herb_veg_county$CountyName<-norcounty_vect$CountyName
forest_herb_veg_countyDF<-gather(forest_herb_veg_county,key=Year,Y_1999,Y_2009,Y_2015,value=Ratio,-CountyName)
forest_herb_veg_countyDF$YearN<-as.numeric(substr(forest_herb_veg_countyDF$Year,3,7))
ggplot(data=forest_herb_veg_countyDF,aes(x=YearN,y=Ratio,color=CountyName))+geom_line()+scale_color_discrete(labels=forest_herb_veg_countyDF$CountyName)+
  ggtitle("Forest Ratio of NPP:Herbivore biomass")+theme_bw()+
  geom_text(data=forest_herb_veg_countyDF[forest_herb_veg_countyDF$YearN==2015,],aes(label = CountyName, x = 2015, y = Ratio), hjust = -.1) 
  


#Herbivores and carnivores
forest_carnivores_herbivores<-carnivore_herbivore_biomass_function_allyrs(carnivorerast = forest_carnivore,
                                                herbivorerast = forest_allherbivores,
                                                carnivore_species = c('wolf','lynx','bear'),
                                                herbivore_species = c('elg','roe','hjort'))
forest_carnivores_herbivores
writeRaster(forest_carnivores_herbivores,"Vertebrates/data/Processed/forest_carnivores_herbivores.tiff")

ggplot()+geom_sf(data=norcounty,fill="white",lwd=0.1)+
  geom_spatraster(data=forest_carnivores_herbivores)+facet_wrap(~lyr)+
  theme_bw()+ggtitle("Forest_Carnivores/Herbivores")+
  scale_fill_distiller(palette = "YlOrRd",
                       na.value=NA,
                       limits=c(0,max(global(forest_herbivores_veg,quantile,probs=0.95,na.rm=T))))

yearlymeans<-global(forest_carnivores_herbivores,fun="mean",na.rm=T)
yearlyquantiles<-global(forest_carnivores_herbivores,fun=quantile,na.rm=T)
plot(allyears_vect,yearlymeans$mean,type='b')
lines(allyears_vect,yearlyquantiles$X50.,type="b",col=2)


forest_carn_herb_county<-extract(forest_carnivores_herbivores,norcounty_vect,mean,na.rm=T)
forest_carn_herb_county$ID<-norcounty_vect$FylkeNr#Replace IDs with Fylke Nrs
forest_carn_herb_county$CountyName<-norcounty_vect$CountyName#Replace IDs with Fylke Nrs
forest_carn_herb_countyDF<-gather(forest_carn_herb_county,key="Year",value="Ratio",-ID,-CountyName)
forest_carn_herb_countyDF$YearN<-as.numeric(substr(forest_carn_herb_countyDF$Year,3,7))
ggplot(data=forest_carn_herb_countyDF,aes(x=YearN,y=Ratio,color=CountyName))+geom_line()+scale_color_discrete()+
  ggtitle("Forest Ratio of Carnivore/Herbivore biomass")+theme_bw()+
  geom_text(data=forest_carn_herb_countyDF[forest_carn_herb_countyDF$YearN==2015,],aes(label = CountyName, x = 2015, y = Ratio), hjust = -.1) 


# #Indicators - open habitat (alpine and lowland) -------------------------
#Open herbivores and vegetation
open_herbivores_veg<-vegetation_herbivore_biomass_function_allyears(herbivorerast=openhab_allherbivores,
                                                                      npprast=openhab_npp,
                                                                      herbivore_species=c("hjort",'roe',"sau","geit",'storf'))

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


open_carnivores_herbivores<-carnivore_herbivore_biomass_function_allyrs(carnivorerast = openhab_carnivore,
                                                                       herbivorerast = openhab_allherbivores,
                                                                       carnivore_species = c('wolf','lynx'),
                                                                       herbivore_species = c('hjort','roe','sau','geit','hest','storf'))

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





# Scaling of indicators ---------------------------------------------------

#Indicator colors
low <- "red"
high <- "green"

#Linear, greater than 0.05 is good condition
opt_herbnpp<-0.03
opt_carnherb<-0.03
scaled_forest_herbivore_npp<-forest_herbivores_veg/opt_herbnpp
scaled_forest_herbivore_npp[scaled_forest_herbivore_npp>1]<-1

scaled_forest_carnivore_herbivore<-forest_carnivores_herbivores/opt_carnherb
scaled_forest_carnivore_herbivore[scaled_forest_carnivore_herbivore>1]<-1


ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=scaled_forest_herbivore_npp)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  facet_wrap(~lyr)+theme_bw()+
  ggtitle("Herbivores/NPP - Linear scaled to 3%")

ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=scaled_forest_carnivore_herbivore)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  facet_wrap(~lyr)+theme_bw()+
  ggtitle("Carnivores/Herbivores - Linear scaled to 3%")

#COunty summary
scaled_forest_herbivore_npp_county<-terra::extract(scaled_forest_herbivore_npp,norcounty_vect,mean,na.rm=T)
scaled_forest_herbivore_npp_county$ID<-norcounty_vect$FylkeNr#Replace IDs with Fylke Nrs
scaled_forest_herbivore_npp_county$CountyName<-norcounty_vect$CountyName#Replace IDs with Fylke Nrs
scaled_forest_herbivore_npp_countyDF<-gather(scaled_forest_herbivore_npp_county,key="Year",value="Index",-ID,-CountyName)
scaled_forest_herbivore_npp_countyDF$YearN<-as.numeric(substr(scaled_forest_herbivore_npp_countyDF$Year,3,7))
ggplot(data=scaled_forest_herbivore_npp_countyDF,aes(x=YearN,y=Index,color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=scaled_forest_herbivore_npp_countyDF$CountyName)+
  ggtitle("Scaled_Forest - Herbivores/NPP")+theme_bw()+xlim(c(1998,2020))+ylim(c(0,1))+theme(legend.position='none')+
  geom_text(data=scaled_forest_herbivore_npp_countyDF[scaled_forest_herbivore_npp_countyDF$YearN==2015,],aes(label = CountyName, x = 2015, y = Index), hjust = -.1) 

scaled_forest_carnivore_herbivore_county<-terra::extract(scaled_forest_carnivore_herbivore,norcounty_vect,mean,na.rm=T)
scaled_forest_carnivore_herbivore_county$ID<-norcounty_vect$FylkeNr#Replace IDs with Fylke Nrs
scaled_forest_carnivore_herbivore_county$CountyName<-norcounty_vect$CountyName#Replace IDs with Fylke Nrs
scaled_forest_carnivore_herbivore_countyDF<-gather(scaled_forest_carnivore_herbivore_county,key="Year",value="Index",-ID,-CountyName)
scaled_forest_carnivore_herbivore_countyDF$YearN<-as.numeric(substr(scaled_forest_carnivore_herbivore_countyDF$Year,3,7))
ggplot(data=scaled_forest_carnivore_herbivore_countyDF,aes(x=YearN,y=Index,color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=scaled_forest_carnivore_herbivore_countyDF$CountyName)+
  ggtitle("Scaled_Forest - Carnivore/Herbivores")+theme_bw()+xlim(c(1905,2025))+theme(legend.position='none')+
  geom_text(data=scaled_forest_carnivore_herbivore_countyDF[scaled_forest_carnivore_herbivore_countyDF$YearN==2015,],aes(label = CountyName, x = 2015, y = Index), hjust = -.1) 



#Exponential-convex, 5% is optimum. 
exp_scaled_forest_herbivore_npp<-scaled_forest_herbivore_npp^0.5

ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=exp_scaled_forest_herbivore_npp$Y_2015)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  ggtitle("Exponential indicator - Forest Herbivores/NPP")

#Sigmoidal
sig_scaled_forest_herbivore_npp<- 100.68*(1-exp(-5*(scaled_forest_herbivore_npp)^2.5))/100
ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=sig_scaled_forest_herbivore_npp$Y_2015)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  ggtitle("Sigmoid indicator - Forest Herbivores/NPP")

#Two-sided - 0.05 optimum
twoside_scaled_forest_herbivore_npp <- ifel(#Note ifel() is the terra implementation of ifelse()
  forest_herbivores_veg< opt_herbnpp,
  (forest_herbivores_veg)/opt_herbnpp,
  (forest_herbivores_veg-opt_herbnpp)/(1-opt_herbnpp)*(-1)
)

# truncating
twoside_scaled_forest_herbivore_npp[twoside_scaled_forest_herbivore_npp<(-1)] <- 0
twoside_scaled_forest_herbivore_npp[twoside_scaled_forest_herbivore_npp>1] <- 1
hist(twoside_scaled_forest_herbivore_npp)

ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=twoside_scaled_forest_herbivore_npp$Y_2015)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  ggtitle("Twoside indicator - Forest Herbivores/NPP")

ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=twoside_scaled_forest_herbivore_npp)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  ggtitle("Twoside indicator - Forest Herbivores/NPP")+
  facet_wrap(~lyr)

twoside_scaled_forest_herbivore_npp_county<-extract(exp_scaled_forest_herbivore_npp,norcounty_vect,mean,na.rm=T)
twoside_scaled_forest_herbivore_npp_county$ID<-norcounty_vect$FylkeNr#Replace IDs with Fylke Nrs
twoside_scaled_forest_herbivore_npp_county$CountyName<-norcounty_vect$CountyName#Replace IDs with Fylke Nrs
twoside_scaled_forest_herbivore_npp_countyDF<-gather(twoside_scaled_forest_herbivore_npp_county,key="Year",value="Index",-ID,-CountyName)
twoside_scaled_forest_herbivore_npp_countyDF$YearN<-as.numeric(substr(twoside_scaled_forest_herbivore_npp_countyDF$Year,3,7))
ggplot(data=twoside_scaled_forest_herbivore_npp_countyDF,aes(x=YearN,y=Index,color=as.factor(ID)))+geom_line()+scale_color_discrete(labels=twoside_scaled_forest_herbivore_npp_countyDF$CountyName)+
  ggtitle("Twoside Scaled_Forest - Herbivores/NPP")+theme_bw()



twoside_scaled_forest_carnivore_herbivore <- ifel(#Note ifel() is the terra implementation of ifelse()
  forest_carnivores_herbivores< opt_carnherb,
  (forest_carnivores_herbivores)/opt_carnherb,
  (forest_carnivores_herbivores-opt_carnherb)/(1-opt_carnherb)*(-1)+1
)

# truncating
twoside_scaled_forest_carnivore_herbivore[twoside_scaled_forest_carnivore_herbivore<0] <- 0
twoside_scaled_forest_carnivore_herbivore[twoside_scaled_forest_carnivore_herbivore>1] <- 1

ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=twoside_scaled_forest_carnivore_herbivore$Y_1907)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  ggtitle("Twoside indicator - Forest Carnivore/Herbivore 1907")

ggplot()+geom_sf(data=norcounty_shp,fill="white",lwd=0.1)+
  geom_spatraster(data=twoside_scaled_forest_carnivore_herbivore)+
  scale_fill_gradient(low = low, high = high,na.value = NA)+
  ggtitle("Twoside indicator - Forest Carnivore/Herbivore")+
  facet_wrap(~lyr)
