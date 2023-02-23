#Habitat specific trophic ratios

rm(list=ls())

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(terra)
library(tidyterra)
library(gridExtra)



#AR5
#artype5<-rast("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\FKB\\AR5_FKB\\ArealType25_ETRS_1989_UTM_Zone_33N.tif")
#artype5[artype5>=99]<-NA
#ggplot()+geom_spatraster(data=artype5,na.value=125)+scale_fill_binned()#breaks=c(10,13,23,29,31,51,61,71))

st_layers("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\0000_25833_ar50_gdb.gdb")
ar50shp<-st_read("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\0000_25833_ar50_gdb.gdb","org_ar_ar50_flate",method="ONLY_CCW")
ar50shp

#AR50
artype50<-rast("T:\\vm\\alle\\GIS_data\\Norge\\Cartography\\N50\\AR50\\AR50_artype_25_ETRS_1989_UTM_Zone_33N.tif")
#artype50[artype50>=99]<-NA
ggplot()+geom_spatraster(data=artype50)+scale_fill_viridis_c(na.value = NA)

#Reclassify
hist(artype50)
artype50_F<-artype50
arTdf<-data.frame(id=c(10,20,30,50,60,70,81,82,128),label=c("BuiltUp","Pastures/Arable","Forest","Open","Mires","Ice","Freshwater","Sea","Unmapped"))
levels(artype50_F)<-arTdf

arcols<-c("black","orange","darkgreen","wheat","lightblue","white","blue","blue",NA)
ggplot()+geom_spatraster(data=artype50_F)+scale_fill_manual(values=arcols,na.value = NA)


#Read NPP
npp<-rast("Vertebrates/data/TrophicBiomassData/NPP.tiff")
vilt<-rast("Vertebrates/data/TrophicBiomassData/Vilt.tiff")
carnivores<-rast("Vertebrates/data/TrophicBiomassData/Vilt.tiff")



npp_forest<-npp[ar50shp==50]


#Example habitat specific index
vilt_forcar_2015_forests<-mask(vilt_forcar_2015,crop(artype50,vilt_forcar_2015),maskvalues=30,inverse=T)


ggplot()+geom_sf(data=ar50shp,aes(fill=artype))
