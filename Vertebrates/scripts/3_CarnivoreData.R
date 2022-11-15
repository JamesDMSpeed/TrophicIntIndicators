#Carnviore data
library(ggplot2)

carnivores<-read.csv("Vertebrates/data/carnivore_data.csv",header=T,sep=";",dec='.')
View(carnivores)
str(carnivores)

#Make a variable for the first year in the period
carnivores$StartYear<-as.numeric(substr(carnivores$Year,1,4))
View(carnivores)

#National sums
nationalcarnivores<-aggregate(carnivores$Individuals,by=list(carnivores$StartYear,carnivores$Species),FUN="sum")
names(nationalcarnivores)<-c("StartYear","Species","Total_Individuals_mean")
View(nationalcarnivores)

ggplot(data=nationalcarnivores,aes(x=StartYear,y=Total_Individuals_mean)) + geom_path(data=nationalcarnivores,aes(color=Species))



