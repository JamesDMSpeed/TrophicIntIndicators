#Utegangarsau

library(dplyr)
library(ggplot2)
library(sf)
library(tidyterra)
library(terra)
library(tidyr)


#Sau data #SSB
utesau<-read.csv("Vertebrates/data/Utegangarsau/Utegangarsau.csv",header=T,sep=";",strip.white = T)
str(utesau)

#Merge with county data
noradm<-st_read("Vertebrates/data","Norway_adm_sf")
ggplot()+geom_sf(data=noradm,mapping=aes(fill=FylkeNr),color=NA)


utesau$FylkeNr<-as.factor(utesau$County)
utesau$FylkeNr <- recode(utesau$County, "Akershus/Oslo" =2  ,"Ostfold"=1, "Aust-Agder"=9,       "Buskerud"  =6,       "Finnmark"  =20,       "Hedmark"   =4,       "Hordaland" =12,       "More_og_Romsdal"=15,  "Nord_Trondelag"  =17,
                            "Nordland"  =18,       "Oppland"  =5,        "Rogaland" =11,        
                            "Sogn og Fjordane"=14, "Sor_Trondelag"=16,    "Telemark" =8,        "Troms" =19,           "Vest-Agder"   =10,    "Vestfold"=7    )

utesaukart<-full_join(noradm,utesau,by="FylkeNr")
#Remove NA value (as Oslo with Akershus)
utesaukart<-utesaukart[!is.na(utesaukart$Year),]


#Maps
ggplot()+geom_sf(data=utesaukart[utesaukart$Year==2012,],aes(fill=Utegangarsau))
ggplot()+geom_sf(data=utesaukart,aes(fill=Utegangarsau),color=NA)+facet_wrap(vars(Year))
ggsave("Vertebrates/outputs/UtesauKart.png")
#Summary line figures
ggplot(data=utesaukart,aes(x=Year,y=Utegangarsau))+geom_line()+facet_wrap(vars(County))
ggsave('Vertebrates/outputs/UtegangSau.png')

#NPP
#Read in
nppyrfiles<-list.files("Vertebrates/data/NPP/Net_PP_Yearly_500m_v6/Npp",full.names = T)

nppstack<-rast(nppyrfiles)
names(nppstack)<-paste0("NPP_",2000:2021)
nppstack[nppstack>32000]<-NA
