#NPP

#Downloading MODIS NPP data

library(MODIStsp)
library(terra)
library(sf)
library(tidyverse)
library(tidyterra)
library(ggplot2)


#Using MODIStsp package

#List product names
MODIStsp_get_prodnames()

#List band names within the NPP product
MODIStsp_get_prodlayers("M*D17A3HGF")$bandnames


#Download 
#Set bounding box (xmin, ymin, xmax,ymax), dates, folder, band and sensor and output format

MODIStsp(gui             = FALSE,
         out_folder      = 'Vertebrates/data/NPP',
         out_folder_mod  = 'Vertebrates/data/NPP',
         selprod         = "Net_PP_GapFil_Yearly_500m (M*D17A3HGF)", 
         bandsel         = 'Npp', 
         sensor          = 'Terra',
         user            = 'james.speed_1' , # your username for NASA http server
         password        = 'ModisPass1',  # your password for NASA http server
         start_date      = '2000.01.01', 
         end_date        = '2022.12.31', 
         verbose         = TRUE,
         bbox            =  c(-76208 ,6450245 , 1114929 , 7939986), #bbox of Norway
         spatmeth        = 'bbox',
         out_format      = 'GTiff',
         compress        = 'LZW',
         out_projsel     = 'User Defined',
         output_proj     = '+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs',
         delete_hdf      = TRUE,
         parallel        = TRUE
)


#Read in
nppyrfiles<-list.files("Vertebrates/data/NPP/Net_PP_Yearly_500m_v6/Npp",full.names = T)

nppstack<-rast(nppyrfiles)
names(nppstack)<-2000:2021
nppstack[nppstack>32000]<-NA
plot(nppstack[[1]],axes=F,main="2000")


ggplot()+geom_spatraster(data=nppstack,aes(fill=2000))
 
