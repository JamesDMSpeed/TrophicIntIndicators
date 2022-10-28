

library(DataCombine) #DropNA
library(ggplot2) #autoplot
library(zoo) #zoo
library(png) #readPNG
library(cowplot) #ggdraw
library(gridExtra) #grid.arrange
library(grid) #rasterGrob
library(data.table) #setDT
library(raster) #getData
library(paletteer) #paletteer_c
library(magick) #image_write



wf<- read.csv("Vertebrates/data/wolf.csv",header=T)
names(wf)[c(2,14,15,26)]<-c("OEST","MOERE","SOERT","Troendelag")
names(wf)

##Functions_____
fiveyrfun = function(NewColumnName, column) {
  NewColumnName[c(1:5)] = mean(column[c(1:5)])
  NewColumnName[c(6:10)] = mean(column[c(6:10)])
  NewColumnName[c(11:15)] = mean(column[c(11:15)])
  NewColumnName[c(16:20)] = mean(column[c(16:20)])
  NewColumnName[c(21:25)] = mean(column[c(21:25)])
  NewColumnName[c(26:30)] = mean(column[c(26:30)])
  NewColumnName[c(31:35)] = mean(column[c(31:35)])
  NewColumnName[c(36:40)] = mean(column[c(36:40)])
  NewColumnName[c(41:45)] = mean(column[c(41:45)])
  NewColumnName[c(46:50)] = mean(column[c(46:50)])
  NewColumnName[c(51:55)] = mean(column[c(51:55)])
  NewColumnName[c(56:60)] = mean(column[c(56:60)])
  NewColumnName[c(61:65)] = mean(column[c(61:65)])
  NewColumnName[c(66:70)] = mean(column[c(66:70)])
  NewColumnName[c(71:75)] = mean(column[c(71:75)])
  NewColumnName[c(76:80)] = mean(column[c(76:80)])
  NewColumnName[c(81:85)] = mean(column[c(81:85)])
  NewColumnName[c(86:90)] = mean(column[c(86:90)])
  NewColumnName[c(91:95)] = mean(column[c(91:95)])
  NewColumnName[c(96:100)] = mean(column[c(96:100)])
  NewColumnName[c(101:105)] = mean(column[c(101:105)])
  NewColumnName[c(106:110)] = mean(column[c(106:110)])
  NewColumnName[c(111:115)] = mean(column[c(111:115)])
  NewColumnName[c(116:120)] = mean(column[c(116:120)])
  NewColumnName[c(121:125)] = mean(column[c(121:125)])
  NewColumnName[c(126:130)] = mean(column[c(126:130)])
  NewColumnName[c(131:135)] = mean(column[c(131:135)])
  NewColumnName[c(136:140)] = mean(column[c(136:140)])
  NewColumnName[c(141:145)] = mean(column[c(141:145)])
  NewColumnName[c(146:150)] = mean(column[c(146:150)])
  NewColumnName[c(151:155)] = mean(column[c(151:155)])
  NewColumnName[c(156:160)] = mean(column[c(156:160)])
  NewColumnName[c(161:165)] = mean(column[c(161:165)])
  NewColumnName[c(166:170)] = mean(column[c(166:170)])
  NewColumnName[c(171:175)] = mean(column[c(171:175)])
  NewColumnName[c(176)] = column[c(176)]
  return(NewColumnName)
}

fiveyrfun2 = function(NewColumnName, column) {
  NewColumnName[c(1:5)] = mean(column[c(1:5)])
  NewColumnName[c(6:10)] = mean(column[c(6:10)])
  NewColumnName[c(11:15)] = mean(column[c(11:15)])
  NewColumnName[c(16:20)] = mean(column[c(16:20)])
  NewColumnName[c(21:25)] = mean(column[c(21:25)])
  NewColumnName[c(26:30)] = mean(column[c(26:30)])
  NewColumnName[c(31:35)] = mean(column[c(31:35)])
  NewColumnName[c(36:40)] = mean(column[c(36:40)])
  NewColumnName[c(41:45)] = mean(column[c(41:45)])
  NewColumnName[c(46:50)] = mean(column[c(46:50)])
  NewColumnName[c(51:55)] = mean(column[c(51:55)])
  NewColumnName[c(56:60)] = mean(column[c(56:60)])
  NewColumnName[c(61:65)] = mean(column[c(61:65)])
  NewColumnName[c(66:70)] = mean(column[c(66:70)])
  NewColumnName[c(71:75)] = mean(column[c(71:75)])
  NewColumnName[c(76:80)] = mean(column[c(76:80)])
  NewColumnName[c(81:85)] = mean(column[c(81:85)])
  NewColumnName[c(86:90)] = mean(column[c(86:90)])
  NewColumnName[c(91:95)] = mean(column[c(91:95)])
  NewColumnName[c(96:100)] = mean(column[c(96:100)])
  NewColumnName[c(101:105)] = mean(column[c(101:105)])
  NewColumnName[c(106:110)] = mean(column[c(106:110)])
  NewColumnName[c(111:115)] = mean(column[c(111:115)])
  NewColumnName[c(116:120)] = mean(column[c(116:120)])
  NewColumnName[c(121:125)] = mean(column[c(121:125)])
  NewColumnName[c(126:130)] = mean(column[c(126:130)])
  NewColumnName[c(131:135)] = mean(column[c(131:135)])
  NewColumnName[c(136:140)] = mean(column[c(136:140)])
  NewColumnName[c(141:145)] = mean(column[c(141:145)])
  NewColumnName[c(146:150)] = mean(column[c(146:150)])
  NewColumnName[c(151:155)] = mean(column[c(151:155)])
  NewColumnName[c(156:160)] = mean(column[c(156:160)])
  NewColumnName[c(161:165)] = mean(column[c(161:165)])
  NewColumnName[c(166:170)] = mean(column[c(166:170)])
  NewColumnName[c(171)] = column[c(171)]
  NewColumnName[c(172:176)] = NA
  return(NewColumnName)
}

fiveYRfun = function(NewColumnName) {
  NewColumnName[c(1:5)] = "1846-50"
  NewColumnName[c(6:10)] = "1851-55"
  NewColumnName[c(11:15)] = "1856-60"
  NewColumnName[c(16:20)] = "1861-65"
  NewColumnName[c(21:25)] = "1866-70"
  NewColumnName[c(26:30)] = "1871-75"
  NewColumnName[c(31:35)] = "1876-80"
  NewColumnName[c(36:40)] = "1881-85"
  NewColumnName[c(41:45)] = "1886-90"
  NewColumnName[c(46:50)] = "1891-95"
  NewColumnName[c(51:55)] = "1896-1900"
  NewColumnName[c(56:60)] = "1901-05"
  NewColumnName[c(61:65)] = "1906-10"
  NewColumnName[c(66:70)] = "1911-15"
  NewColumnName[c(71:75)] = "1916-20"
  NewColumnName[c(76:80)] = "1921-25"
  NewColumnName[c(81:85)] = "1926-30"
  NewColumnName[c(86:90)] = "1931-35"
  NewColumnName[c(91:95)] = "1936-40"
  NewColumnName[c(96:100)] = "1941-45"
  NewColumnName[c(101:105)] = "1946-50"
  NewColumnName[c(106:110)] = "1951-55"
  NewColumnName[c(111:115)] = "1956-60"
  NewColumnName[c(116:120)] = "1961-65"
  NewColumnName[c(121:125)] = "1966-70"
  NewColumnName[c(126:130)] = "1971-75"
  NewColumnName[c(131:135)] = "1976-80"
  NewColumnName[c(136:140)] = "1980/81-84/85"
  NewColumnName[c(141:145)] = "1985/86-89/90"
  NewColumnName[c(146:150)] = "1990/91-94/95"
  NewColumnName[c(151:155)] = "1995/96-99/00"
  NewColumnName[c(156:160)] = "2000/01-04/05"
  NewColumnName[c(161:165)] = "2005/06-09/10"
  NewColumnName[c(166:170)] = "2010/11-14/15"
  NewColumnName[c(171:175)] = "2015/16-19/20"
  NewColumnName[c(176)] = "2020/21"
  return(NewColumnName)
}

fiveYRfun2 = function(NewColumnName) {
  NewColumnName[c(1:5)] = "1846-50"
  NewColumnName[c(6:10)] = "1851-55"
  NewColumnName[c(11:15)] = "1856-60"
  NewColumnName[c(16:20)] = "1861-65"
  NewColumnName[c(21:25)] = "1866-70"
  NewColumnName[c(26:30)] = "1871-75"
  NewColumnName[c(31:35)] = "1876-80"
  NewColumnName[c(36:40)] = "1881-85"
  NewColumnName[c(41:45)] = "1886-90"
  NewColumnName[c(46:50)] = "1891-95"
  NewColumnName[c(51:55)] = "1896-1900"
  NewColumnName[c(56:60)] = "1901-05"
  NewColumnName[c(61:65)] = "1906-10"
  NewColumnName[c(66:70)] = "1911-15"
  NewColumnName[c(71:75)] = "1916-20"
  NewColumnName[c(76:80)] = "1921-25"
  NewColumnName[c(81:85)] = "1926-30"
  NewColumnName[c(86:90)] = "1931-35"
  NewColumnName[c(91:95)] = "1936-40"
  NewColumnName[c(96:100)] = "1941-45"
  NewColumnName[c(101:105)] = "1946-50"
  NewColumnName[c(106:110)] = "1951-55"
  NewColumnName[c(111:115)] = "1956-60"
  NewColumnName[c(116:120)] = "1961-65"
  NewColumnName[c(121:125)] = "1966-70"
  NewColumnName[c(126:130)] = "1971-75"
  NewColumnName[c(131:135)] = "1976-80"
  NewColumnName[c(136:140)] = "1980/81-84/85"
  NewColumnName[c(141:145)] = "1985/86-89/90"
  NewColumnName[c(146:150)] = "1990/91-94/95"
  NewColumnName[c(151:155)] = "1995/96-99/00"
  NewColumnName[c(156:160)] = "2000/01-04/05"
  NewColumnName[c(161:165)] = "2005/06-09/10"
  NewColumnName[c(166:170)] = "2010/11-14/15"
  NewColumnName[c(171)] = "2015/16"
  NewColumnName[c(172:176)] = NA
  return(NewColumnName)
}

fivelistfun = function(NewListName, column) {
  NewListName = column[c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                         81,86,91,96,101,106,111,116,121,126,131,136,
                         141,146,151,156,161,166,171,176)]
  return(NewListName)
}

fivelistfun2 = function(NewListName, column) {
  NewListName = column[c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                         81,86,91,96,101,106,111,116,121,126,131,136,
                         141,146,151,156,161,166,171)]
  return(NewListName)
}

##100 Replicates Setup_____
myColors = c("#009E73","#009E73","#515151","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black","black","black","black","black","black","black",
             "black","black","black","black", "red")
mySizes = c(0.5,0.25,0.5,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
            0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
            0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
            0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
            0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
            0.2,0.2,0.2,0.5)
myLinetypes = c(1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5)

##Wolf Setup_____
wf$Total = round(wf$Total*1.1) #Add a constant 10% to numbers killed to reflect wounding & loss rate

#Generate r & m csv's first time through
wf_m = matrix(runif(17600, min=0.01, max=0.03), ncol=100) #Create new matrix
wf_r = matrix(runif(17600, min=0.10, max=0.16), ncol=100) #Create new matrix
wf_m = data.frame(wf_m) #Convert matrix to data frame
wf_r = data.frame(wf_r) #Convert matrix to data frame
colnames(wf_m) = c(1:100) #Change the column names
colnames(wf_r) = c(1:100) #Change the column names
write.csv(wf_m, "wf_m.csv") #Write csv
write.csv(wf_r, "wf_r.csv") #Write csv

#Import r & m csv's in subsequent runs
wf_m = read_csv("wf_m.csv")
wf_m = wf_m[-c(1)]
names(wf_m)[1:100] = c(1:100)

wf_r = read.csv("wf_r.csv")
wf_r = wf_r[-c(1)]
names(wf_r)[1:100] = c(1:100)

wf$YEAR5 = fiveYRfun(wf$YEAR5) #Make new Year column with 5yr labels
wf$YEAR5.2 = fiveYRfun2(wf$YEAR5) #Make new Year column with 5yr labels
wf_yr = fivelistfun(wf_yr, wf$YEAR5) #Make year list with just the data we want
wf_yr2 = fivelistfun2(wf_yr2, wf$YEAR5.2) #Make year list with just the data we want
wf_yr2
wf$YEAR5.2



#####National


##Convert Total column to 5 year average
wf$Total_5y = fiveyrfun(wf$Total_5y, wf$Total) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#TOTAL MPM MAX
wf$Total_Max[c(176)] = 114 #Make new column & set the year 2020-21 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$Total_Max[c(21:175)]) {
  if(is.na(val)) wf$Total_Max[c(21:175)] = (wf$Total_Max[c(22:176)] + wf$Total_5y[c(22:176)])/((1-0.05)+(0.30))
}

#TOTAL MPM MIN
wf$Total_Min[c(176)] = 114 #Make new column & set the year 2020-21 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$Total_Min[c(21:175)]) {
  if(is.na(val)) wf$Total_Min[c(21:175)] = (wf$Total_Min[c(22:176)] + wf$Total_5y[c(22:176)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$Total_Max5y = fiveyrfun(wf$Total_Max5y, wf$Total_Max) #Make new column & fill it
wf$Total_Min5y = fiveyrfun(wf$Total_Min5y, wf$Total_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.Total = fivelistfun(a.wf.Total, wf$Total_Max5y) #Max
b.wf.Total = fivelistfun(b.wf.Total, wf$Total_Min5y) #Min
c.wf.Total = fivelistfun(c.wf.Total, wf$Total_5y) #No. harvested

wf_df.Total = data.frame(a.wf.Total, b.wf.Total, c.wf.Total, wf_yr)




####100 Replicates

wf_df.TotalReps1 = data.frame(wf$YEAR5,wf$Total_5y) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.TotalReps1[[paste0(x)]][c(176)] = 114
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.TotalReps1[[paste0(x)]][c(21:175)]) {
    if(is.na(val)) wf_df.TotalReps1[[paste0(x)]][c(21:175)] = (wf_df.TotalReps1[[paste0(x)]][c(22:176)] + wf$Total_5y[c(22:176)])/((1-(wf_m[[paste0(x)]][c(22:176)]))+(wf_r[[paste0(x)]][c(22:176)]))
  }
}

##--
wf_df.TotalReps2 = data.frame(wf$YEAR5,wf$Total_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.TotalReps2[[paste0(x)]] = fiveyrfun(wf_df.TotalReps2[[paste0(x)]], wf_df.TotalReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.TotalReps3 = data.frame(subset(wf_df.Total, select = c(a.wf.Total, b.wf.Total, c.wf.Total))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.TotalReps3[[paste0(x)]] = wf_df.TotalReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                  81,86,91,96,101,106,111,116,121,126,131,136,
                                                                  141,146,151,156,161,166,171,176)]
  x = x + 1
  if (x == 101){
    break
  }
}

##x axis starting at 1866
wf.TOTAL_final = DropNA(wf_df.TotalReps3)
wf.TOTAL_final$Mean = rowMeans(wf.TOTAL_final[c(4:103)]) #Make new column with mean of 100 replicates

#x axis starting at 1846
wf_df.TotalReps3$Mean = rowMeans(wf_df.TotalReps3[c(4:103)])
wf_df.TotalReps3$c.wf.Total[1:4] = NA

##WOLF PLOT X AXIS STARTING AT 1866
autoplot(zoo(wf.TOTAL_final, wf_yr[c(5:36)]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Wolf, National Population Estimate - 5 Year Average") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.Total$wf_yr[c(36)], y=wf_df.Total$a[c(36)],
           color="black") +
  annotate("text", x=32, y=210, 
           label = "114",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.Total[c(5)]) + 30), 
           label = round(a.wf.Total[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.Total[c(5)]) + 30), 
           label = round(b.wf.Total[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)


##WOLF PLOT X AXIS STARTING AT 1846
plot.TOTAL = autoplot(zoo(wf_df.TotalReps3, wf_yr), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("National") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   expand = expansion(add = c(0.5,1))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.Total$wf_yr[c(36)], y=wf_df.Total$a[c(36)],
           color="black") +
  annotate("text", x=36, y=125, 
           label = "114",
           color = "black") +
  annotate("text", x=5, y=(round(a.wf.Total[c(5)]) - 7), 
           label = round(a.wf.Total[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=5, y=(round(b.wf.Total[c(5)]) - 7), 
           label = round(b.wf.Total[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.TOTAL

TOTAL.png = readPNG("Red Counties/Total.png") #Import county png

ggdraw(plot.TOTAL) + 
  draw_image(TOTAL.png, x = 0.48, y = 0.225, width = 0.4) #x=from the left, y=from the bottom

##--



#####Finnmark


##Convert FINN column to 5 year average
wf$FINN_5y = fiveyrfun2(wf$FINN_5y, wf$FINN) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#FINN MPM MAX
wf$FINN_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$FINN_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$FINN_Max[c(21:170)]) {
  if(is.na(val)) wf$FINN_Max[c(21:170)] = (wf$FINN_Max[c(22:171)] + wf$FINN_5y[c(22:171)])/((1-0.05)+(0.30))
}

#FINN MPM MIN
wf$FINN_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$FINN_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$FINN_Min[c(21:170)]) {
  if(is.na(val)) wf$FINN_Min[c(21:170)] = (wf$FINN_Min[c(22:171)] + wf$FINN_5y[c(22:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$FINN_Max5y = fiveyrfun2(wf$FINN_Max5y, wf$FINN_Max) #Make new column & fill it
wf$FINN_Min5y = fiveyrfun2(wf$FINN_Min5y, wf$FINN_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.FINN = fivelistfun2(a.wf.FINN, wf$FINN_Max5y) #Max
b.wf.FINN = fivelistfun2(b.wf.FINN, wf$FINN_Min5y) #Min
c.wf.FINN = fivelistfun2(c.wf.FINN, wf$FINN_5y) #No. harvested

wf_df.FINN = data.frame(a.wf.FINN, b.wf.FINN, c.wf.FINN, wf_yr2)




####100 Replicates

wf_df.FINNReps1 = data.frame(wf$YEAR5.2[1:171],wf$FINN_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.FINNReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.FINNReps1[[paste0(x)]][c(21:170)]) {
    if(is.na(val)) wf_df.FINNReps1[[paste0(x)]][c(21:170)] = (wf_df.FINNReps1[[paste0(x)]][c(22:171)] + wf$FINN_5y[c(22:171)])/((1-(wf_m[[paste0(x)]][c(22:171)]))+(wf_r[[paste0(x)]][c(22:171)]))
  }
}

##--
wf_df.FINNReps2 = data.frame(wf$YEAR5.2,wf$FINN_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.FINNReps2[[paste0(x)]] = fiveyrfun2(wf_df.FINNReps2[[paste0(x)]], wf_df.FINNReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.FINNReps3 = data.frame(subset(wf_df.FINN, select = c(a.wf.FINN, b.wf.FINN, c.wf.FINN))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.FINNReps3[[paste0(x)]] = wf_df.FINNReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

##x axis starting at 1866
wf.FINN_final = DropNA(wf_df.FINNReps3)
wf.FINN_final$Mean = rowMeans(wf.FINN_final[c(4:103)]) #Make new column with mean of 100 replicates

#x axis starting at 1846
wf_df.FINNReps3$Mean = rowMeans(wf_df.FINNReps3[c(4:103)])

##WOLF PLOT X AXIS STARTING AT 1866
autoplot(zoo(wf.FINN_final, wf_yr2[5:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Wolf, Finnmark Population Estimate - 5 Year Average") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.FINN$wf_yr2[c(35)], y=wf_df.FINN$a[c(35)],
           color="black") +
  annotate("text", x=31, y=37, 
           label = "35",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.FINN[c(5)]) + 0.4), 
           label = round(a.wf.FINN[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.FINN[c(5)]) + 0.4), 
           label = round(b.wf.FINN[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)


##WOLF PLOT X AXIS STARTING AT 1846
plot.FINN = autoplot(zoo(wf_df.FINNReps3, wf_yr2), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Finnmark") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.FINN$wf_yr2[c(35)], y=wf_df.FINN$a[c(35)],
           color="black") +
  annotate("text", x=35, y=5, 
           label = "0",
           color = "black") +
  annotate("text", x=5, y=(round(a.wf.FINN[c(5)]) - 3), 
           label = round(a.wf.FINN[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=5, y=(round(b.wf.FINN[c(5)]) - 3), 
           label = round(b.wf.FINN[c(5)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.FINN

FINN.png = readPNG("Red Counties/Finnmark.png") #Import county png

ggdraw(plot.FINN) + 
  draw_image(FINN.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####OEstfold


##Convert OEST column to 5 year average
wf$OEST_5y = fiveyrfun2(wf$OEST_5y, wf$OEST) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#OEST MPM MAX
wf$OEST_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$OEST_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$OEST_Max[c(1:170)]) {
  if(is.na(val)) wf$OEST_Max[c(1:170)] = (wf$OEST_Max[c(2:171)] + wf$OEST_5y[c(2:171)])/((1-0.05)+(0.30))
}

#OEST MPM MIN
wf$OEST_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$OEST_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$OEST_Min[c(1:170)]) {
  if(is.na(val)) wf$OEST_Min[c(1:170)] = (wf$OEST_Min[c(2:171)] + wf$OEST_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$OEST_Max5y = fiveyrfun2(wf$OEST_Max5y, wf$OEST_Max) #Make new column & fill it
wf$OEST_Min5y = fiveyrfun2(wf$OEST_Min5y, wf$OEST_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.OEST = fivelistfun2(a.wf.OEST, wf$OEST_Max5y) #Max
b.wf.OEST = fivelistfun2(b.wf.OEST, wf$OEST_Min5y) #Min
c.wf.OEST = fivelistfun2(c.wf.OEST, wf$OEST_5y) #No. harvested

wf_df.OEST = data.frame(a.wf.OEST, b.wf.OEST, c.wf.OEST, wf_yr2)




####100 Replicates

wf_df.OESTReps1 = data.frame(wf$YEAR5.2[1:171],wf$OEST_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.OESTReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.OESTReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.OESTReps1[[paste0(x)]][c(1:170)] = (wf_df.OESTReps1[[paste0(x)]][c(2:171)] + wf$OEST_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.OESTReps2 = data.frame(wf$YEAR5.2,wf$OEST_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.OESTReps2[[paste0(x)]] = fiveyrfun2(wf_df.OESTReps2[[paste0(x)]], wf_df.OESTReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.OESTReps3 = data.frame(subset(wf_df.OEST, select = c(a.wf.OEST, b.wf.OEST, c.wf.OEST))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.OESTReps3[[paste0(x)]] = wf_df.OESTReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                              81,86,91,96,101,106,111,116,121,126,131,136,
                                                              141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.OEST_final = DropNA(wf_df.OESTReps3)
wf.OEST_final$Mean = rowMeans(wf.OEST_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.OEST = autoplot(zoo(wf.OEST_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("OEstfold") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   expand = expansion(add = c(1,0.5))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.OEST$wf_yr2[c(35)], y=wf_df.OEST$a[c(35)],
           color="black") +
  annotate("text", x=35, y=0.8, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.OEST[c(1)], digits=1) + 0.5), 
           label = round(a.wf.OEST[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=0.8, y=(round(b.wf.OEST[c(1)], digits=1) + 0.3), 
           label = round(b.wf.OEST[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.OEST

OEST.png = readPNG("Red Counties/OEstfold.png") #Import county png

ggdraw(plot.OEST) + 
  draw_image(OEST.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--

#####Akershus


##Convert AKER column to 5 year average
wf$AKER_5y = fiveyrfun2(wf$AKER_5y, wf$AKER) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#AKER MPM MAX
wf$AKER_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$AKER_Max[c(171)] = 11 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$AKER_Max[c(1:170)]) {
  if(is.na(val)) wf$AKER_Max[c(1:170)] = (wf$AKER_Max[c(2:171)] + wf$AKER_5y[c(2:171)])/((1-0.05)+(0.30))
}

#AKER MPM MIN
wf$AKER_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$AKER_Min[c(171)] = 11 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$AKER_Min[c(1:170)]) {
  if(is.na(val)) wf$AKER_Min[c(1:170)] = (wf$AKER_Min[c(2:171)] + wf$AKER_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$AKER_Max5y = fiveyrfun2(wf$AKER_Max5y, wf$AKER_Max) #Make new column & fill it
wf$AKER_Min5y = fiveyrfun2(wf$AKER_Min5y, wf$AKER_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.AKER = fivelistfun2(a.wf.AKER, wf$AKER_Max5y) #Max
b.wf.AKER = fivelistfun2(b.wf.AKER, wf$AKER_Min5y) #Min
c.wf.AKER = fivelistfun2(c.wf.AKER, wf$AKER_5y) #No. harvested

wf_df.AKER = data.frame(a.wf.AKER, b.wf.AKER, c.wf.AKER, wf_yr2)




####100 Replicates

wf_df.AKERReps1 = data.frame(wf$YEAR5.2[1:171],wf$AKER_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.AKERReps1[[paste0(x)]][c(171)] = 11
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.AKERReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.AKERReps1[[paste0(x)]][c(1:170)] = (wf_df.AKERReps1[[paste0(x)]][c(2:171)] + wf$AKER_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.AKERReps2 = data.frame(wf$YEAR5.2,wf$AKER_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.AKERReps2[[paste0(x)]] = fiveyrfun2(wf_df.AKERReps2[[paste0(x)]], wf_df.AKERReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.AKERReps3 = data.frame(subset(wf_df.AKER, select = c(a.wf.AKER, b.wf.AKER, c.wf.AKER))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.AKERReps3[[paste0(x)]] = wf_df.AKERReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.AKER_final = DropNA(wf_df.AKERReps3)
wf.AKER_final$Mean = rowMeans(wf.AKER_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.AKER = autoplot(zoo(wf.AKER_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Akershus") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   expand = expansion(add = c(1,0.5))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.AKER$wf_yr2[c(35)], y=wf_df.AKER$a[c(35)],
           color="black") +
  annotate("text", x=35, y=11.5, 
           label = "11",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.AKER[c(1)], digits=1) + 0.3), 
           label = round(a.wf.AKER[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=0.6, y=(round(b.wf.AKER[c(1)], digits=1) + 0.3), 
           label = round(b.wf.AKER[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.AKER

AKER.png = readPNG("Red Counties/Akershus.png") #Import county png

ggdraw(plot.AKER) + 
  draw_image(AKER.png, x = 0.48, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####Hedmark


##Convert HED column to 5 year average
wf$HED_5y = fiveyrfun2(wf$HED_5y, wf$HED) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#HED MPM MAX
wf$HED_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$HED_Max[c(171)] = 64 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$HED_Max[c(1:170)]) {
  if(is.na(val)) wf$HED_Max[c(1:170)] = (wf$HED_Max[c(2:171)] + wf$HED_5y[c(2:171)])/((1-0.05)+(0.30))
}

#HED MPM MIN
wf$HED_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$HED_Min[c(171)] = 64 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$HED_Min[c(1:170)]) {
  if(is.na(val)) wf$HED_Min[c(1:170)] = (wf$HED_Min[c(2:171)] + wf$HED_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$HED_Max5y = fiveyrfun2(wf$HED_Max5y, wf$HED_Max) #Make new column & fill it
wf$HED_Min5y = fiveyrfun2(wf$HED_Min5y, wf$HED_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.HED = fivelistfun2(a.wf.HED, wf$HED_Max5y) #Max
b.wf.HED = fivelistfun2(b.wf.HED, wf$HED_Min5y) #Min
c.wf.HED = fivelistfun2(c.wf.HED, wf$HED_5y) #No. harvested

wf_df.HED = data.frame(a.wf.HED, b.wf.HED, c.wf.HED, wf_yr2)




####100 Replicates

wf_df.HEDReps1 = data.frame(wf$YEAR5.2[1:171],wf$HED_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.HEDReps1[[paste0(x)]][c(171)] = 64
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.HEDReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.HEDReps1[[paste0(x)]][c(1:170)] = (wf_df.HEDReps1[[paste0(x)]][c(2:171)] + wf$HED_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.HEDReps2 = data.frame(wf$YEAR5.2,wf$HED_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.HEDReps2[[paste0(x)]] = fiveyrfun2(wf_df.HEDReps2[[paste0(x)]], wf_df.HEDReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.HEDReps3 = data.frame(subset(wf_df.HED, select = c(a.wf.HED, b.wf.HED, c.wf.HED))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.HEDReps3[[paste0(x)]] = wf_df.HEDReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                              81,86,91,96,101,106,111,116,121,126,131,136,
                                                              141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.HED_final = DropNA(wf_df.HEDReps3)
wf.HED_final$Mean = rowMeans(wf.HED_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.HED = autoplot(zoo(wf.HED_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Hedmark") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.HED$wf_yr2[c(35)], y=wf_df.HED$a[c(35)],
           color="black") +
  annotate("text", x=35, y=68, 
           label = "64",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.HED[c(1)]) - 1.5), 
           label = round(a.wf.HED[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.HED[c(1)]) - 1.5), 
           label = round(b.wf.HED[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.HED

HED.png = readPNG("Red Counties/Hedmark.png") #Import county png

ggdraw(plot.HED) + 
  draw_image(HED.png, x = 0.48, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--


#####Oppland


##Convert OPPL column to 5 year average
wf$OPPL_5y = fiveyrfun2(wf$OPPL_5y, wf$OPPL) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#OPPL MPM MAX
wf$OPPL_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$OPPL_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$OPPL_Max[c(1:170)]) {
  if(is.na(val)) wf$OPPL_Max[c(1:170)] = (wf$OPPL_Max[c(2:171)] + wf$OPPL_5y[c(2:171)])/((1-0.05)+(0.30))
}

#OPPL MPM MIN
wf$OPPL_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$OPPL_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$OPPL_Min[c(1:170)]) {
  if(is.na(val)) wf$OPPL_Min[c(1:170)] = (wf$OPPL_Min[c(2:171)] + wf$OPPL_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$OPPL_Max5y = fiveyrfun2(wf$OPPL_Max5y, wf$OPPL_Max) #Make new column & fill it
wf$OPPL_Min5y = fiveyrfun2(wf$OPPL_Min5y, wf$OPPL_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.OPPL = fivelistfun2(a.wf.OPPL, wf$OPPL_Max5y) #Max
b.wf.OPPL = fivelistfun2(b.wf.OPPL, wf$OPPL_Min5y) #Min
c.wf.OPPL = fivelistfun2(c.wf.OPPL, wf$OPPL_5y) #No. harvested

wf_df.OPPL = data.frame(a.wf.OPPL, b.wf.OPPL, c.wf.OPPL, wf_yr2)




####100 Replicates

wf_df.OPPLReps1 = data.frame(wf$YEAR5.2[1:171],wf$OPPL_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.OPPLReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.OPPLReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.OPPLReps1[[paste0(x)]][c(1:170)] = (wf_df.OPPLReps1[[paste0(x)]][c(2:171)] + wf$OPPL_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.OPPLReps2 = data.frame(wf$YEAR5.2,wf$OPPL_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.OPPLReps2[[paste0(x)]] = fiveyrfun2(wf_df.OPPLReps2[[paste0(x)]], wf_df.OPPLReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.OPPLReps3 = data.frame(subset(wf_df.OPPL, select = c(a.wf.OPPL, b.wf.OPPL, c.wf.OPPL))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.OPPLReps3[[paste0(x)]] = wf_df.OPPLReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.OPPL_final = DropNA(wf_df.OPPLReps3)
wf.OPPL_final$Mean = rowMeans(wf.OPPL_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.OPPL = autoplot(zoo(wf.OPPL_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Oppland") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.OPPL$wf_yr2[c(35)], y=wf_df.OPPL$a[c(35)],
           color="black") +
  annotate("text", x=35, y=5, 
           label = "0",
           color = "black") +
  annotate("text", x=1.2, y=(round(a.wf.OPPL[c(1)]) + 3), 
           label = round(a.wf.OPPL[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.OPPL[c(1)]) + 3), 
           label = round(b.wf.OPPL[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.OPPL

OPPL.png = readPNG("Red Counties/Oppland.png") #Import county png

ggdraw(plot.OPPL) + 
  draw_image(OPPL.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####Buskerud


##Convert BUSK column to 5 year average
wf$BUSK_5y = fiveyrfun2(wf$BUSK_5y, wf$BUSK) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#BUSK MPM MAX
wf$BUSK_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$BUSK_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$BUSK_Max[c(1:170)]) {
  if(is.na(val)) wf$BUSK_Max[c(1:170)] = (wf$BUSK_Max[c(2:171)] + wf$BUSK_5y[c(2:171)])/((1-0.05)+(0.30))
}

#BUSK MPM MIN
wf$BUSK_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$BUSK_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$BUSK_Min[c(1:170)]) {
  if(is.na(val)) wf$BUSK_Min[c(1:170)] = (wf$BUSK_Min[c(2:171)] + wf$BUSK_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$BUSK_Max5y = fiveyrfun2(wf$BUSK_Max5y, wf$BUSK_Max) #Make new column & fill it
wf$BUSK_Min5y = fiveyrfun2(wf$BUSK_Min5y, wf$BUSK_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.BUSK = fivelistfun2(a.wf.BUSK, wf$BUSK_Max5y) #Max
b.wf.BUSK = fivelistfun2(b.wf.BUSK, wf$BUSK_Min5y) #Min
c.wf.BUSK = fivelistfun2(c.wf.BUSK, wf$BUSK_5y) #No. harvested

wf_df.BUSK = data.frame(a.wf.BUSK, b.wf.BUSK, c.wf.BUSK, wf_yr2)




####100 Replicates

wf_df.BUSKReps1 = data.frame(wf$YEAR5.2[1:171],wf$BUSK_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.BUSKReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.BUSKReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.BUSKReps1[[paste0(x)]][c(1:170)] = (wf_df.BUSKReps1[[paste0(x)]][c(2:171)] + wf$BUSK_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.BUSKReps2 = data.frame(wf$YEAR5.2,wf$BUSK_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.BUSKReps2[[paste0(x)]] = fiveyrfun2(wf_df.BUSKReps2[[paste0(x)]], wf_df.BUSKReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.BUSKReps3 = data.frame(subset(wf_df.BUSK, select = c(a.wf.BUSK, b.wf.BUSK, c.wf.BUSK))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.BUSKReps3[[paste0(x)]] = wf_df.BUSKReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.BUSK_final = DropNA(wf_df.BUSKReps3)
wf.BUSK_final$Mean = rowMeans(wf.BUSK_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.BUSK = autoplot(zoo(wf.BUSK_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Buskerud") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.BUSK$wf_yr2[c(35)], y=wf_df.BUSK$a[c(35)],
           color="black") +
  annotate("text", x=35, y=1.5, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.BUSK[c(1)]) + 1), 
           label = round(a.wf.BUSK[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=0.9, y=(round(b.wf.BUSK[c(1)]) + 0.3), 
           label = round(b.wf.BUSK[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.BUSK

BUSK.png = readPNG("Red Counties/Buskerud.png") #Import county png

ggdraw(plot.BUSK) + 
  draw_image(BUSK.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####Vestfold


##Convert VEST column to 5 year average
wf$VEST_5y = fiveyrfun2(wf$VEST_5y, wf$VEST) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#VEST MPM MAX
wf$VEST_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$VEST_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$VEST_Max[c(1:170)]) {
  if(is.na(val)) wf$VEST_Max[c(1:170)] = (wf$VEST_Max[c(2:171)] + wf$VEST_5y[c(2:171)])/((1-0.05)+(0.30))
}

#VEST MPM MIN
wf$VEST_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$VEST_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$VEST_Min[c(1:170)]) {
  if(is.na(val)) wf$VEST_Min[c(1:170)] = (wf$VEST_Min[c(2:171)] + wf$VEST_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$VEST_Max5y = fiveyrfun2(wf$VEST_Max5y, wf$VEST_Max) #Make new column & fill it
wf$VEST_Min5y = fiveyrfun2(wf$VEST_Min5y, wf$VEST_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.VEST = fivelistfun2(a.wf.VEST, wf$VEST_Max5y) #Max
b.wf.VEST = fivelistfun2(b.wf.VEST, wf$VEST_Min5y) #Min
c.wf.VEST = fivelistfun2(c.wf.VEST, wf$VEST_5y) #No. harvested

wf_df.VEST = data.frame(a.wf.VEST, b.wf.VEST, c.wf.VEST, wf_yr2)




####100 Replicates

wf_df.VESTReps1 = data.frame(wf$YEAR5.2[1:171],wf$VEST_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.VESTReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.VESTReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.VESTReps1[[paste0(x)]][c(1:170)] = (wf_df.VESTReps1[[paste0(x)]][c(2:171)] + wf$VEST_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.VESTReps2 = data.frame(wf$YEAR5.2,wf$VEST_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.VESTReps2[[paste0(x)]] = fiveyrfun2(wf_df.VESTReps2[[paste0(x)]], wf_df.VESTReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.VESTReps3 = data.frame(subset(wf_df.VEST, select = c(a.wf.VEST, b.wf.VEST, c.wf.VEST))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.VESTReps3[[paste0(x)]] = wf_df.VESTReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.VEST_final = DropNA(wf_df.VESTReps3)
wf.VEST_final$Mean = rowMeans(wf.VEST_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.VEST = autoplot(zoo(wf.VEST_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Vestfold") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   expand = expansion(add = c(1,0.5))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.VEST$wf_yr2[c(35)], y=wf_df.VEST$a[c(35)],
           color="black") +
  annotate("text", x=35.2, y=0.05, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.VEST[c(1)], digits=1) + 0.02), 
           label = round(a.wf.VEST[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.VEST[c(1)], digits=1) - 0.07), 
           label = round(b.wf.VEST[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.VEST

VEST.png = readPNG("Red Counties/Vestfold.png") #Import county png

ggdraw(plot.VEST) + 
  draw_image(VEST.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--

#####Telemark


##Convert TELM column to 5 year average
wf$TELM_5y = fiveyrfun2(wf$TELM_5y, wf$TELM) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#TELM MPM MAX
wf$TELM_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$TELM_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$TELM_Max[c(1:170)]) {
  if(is.na(val)) wf$TELM_Max[c(1:170)] = (wf$TELM_Max[c(2:171)] + wf$TELM_5y[c(2:171)])/((1-0.05)+(0.30))
}

#TELM MPM MIN
wf$TELM_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$TELM_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$TELM_Min[c(1:170)]) {
  if(is.na(val)) wf$TELM_Min[c(1:170)] = (wf$TELM_Min[c(2:171)] + wf$TELM_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$TELM_Max5y = fiveyrfun2(wf$TELM_Max5y, wf$TELM_Max) #Make new column & fill it
wf$TELM_Min5y = fiveyrfun2(wf$TELM_Min5y, wf$TELM_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.TELM = fivelistfun2(a.wf.TELM, wf$TELM_Max5y) #Max
b.wf.TELM = fivelistfun2(b.wf.TELM, wf$TELM_Min5y) #Min
c.wf.TELM = fivelistfun2(c.wf.TELM, wf$TELM_5y) #No. harvested

wf_df.TELM = data.frame(a.wf.TELM, b.wf.TELM, c.wf.TELM, wf_yr2)




####100 Replicates

wf_df.TELMReps1 = data.frame(wf$YEAR5.2[1:171],wf$TELM_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.TELMReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.TELMReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.TELMReps1[[paste0(x)]][c(1:170)] = (wf_df.TELMReps1[[paste0(x)]][c(2:171)] + wf$TELM_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.TELMReps2 = data.frame(wf$YEAR5.2,wf$TELM_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.TELMReps2[[paste0(x)]] = fiveyrfun2(wf_df.TELMReps2[[paste0(x)]], wf_df.TELMReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.TELMReps3 = data.frame(subset(wf_df.TELM, select = c(a.wf.TELM, b.wf.TELM, c.wf.TELM))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.TELMReps3[[paste0(x)]] = wf_df.TELMReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.TELM_final = DropNA(wf_df.TELMReps3)
wf.TELM_final$Mean = rowMeans(wf.TELM_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.TELM = autoplot(zoo(wf.TELM_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Telemark") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.TELM$wf_yr2[c(35)], y=wf_df.TELM$a[c(35)],
           color="black") +
  annotate("text", x=35, y=3, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.TELM[c(1)]) + 2), 
           label = round(a.wf.TELM[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.TELM[c(1)]) + 2), 
           label = round(b.wf.TELM[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.TELM

TELM.png = readPNG("Red Counties/Telemark.png") #Import county png

ggdraw(plot.TELM) + 
  draw_image(TELM.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--




#####Aust-Agder


##Convert AU_AG column to 5 year average
wf$AU_AG_5y = fiveyrfun2(wf$AU_AG_5y, wf$AU_AG) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#AU_AG MPM MAX
wf$AU_AG_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$AU_AG_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$AU_AG_Max[c(1:170)]) {
  if(is.na(val)) wf$AU_AG_Max[c(1:170)] = (wf$AU_AG_Max[c(2:171)] + wf$AU_AG_5y[c(2:171)])/((1-0.05)+(0.30))
}

#AU_AG MPM MIN
wf$AU_AG_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$AU_AG_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$AU_AG_Min[c(1:170)]) {
  if(is.na(val)) wf$AU_AG_Min[c(1:170)] = (wf$AU_AG_Min[c(2:171)] + wf$AU_AG_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$AU_AG_Max5y = fiveyrfun2(wf$AU_AG_Max5y, wf$AU_AG_Max) #Make new column & fill it
wf$AU_AG_Min5y = fiveyrfun2(wf$AU_AG_Min5y, wf$AU_AG_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.AU_AG = fivelistfun2(a.wf.AU_AG, wf$AU_AG_Max5y) #Max
b.wf.AU_AG = fivelistfun2(b.wf.AU_AG, wf$AU_AG_Min5y) #Min
c.wf.AU_AG = fivelistfun2(c.wf.AU_AG, wf$AU_AG_5y) #No. harvested

wf_df.AU_AG = data.frame(a.wf.AU_AG, b.wf.AU_AG, c.wf.AU_AG, wf_yr2)




####100 Replicates

wf_df.AU_AGReps1 = data.frame(wf$YEAR5.2[1:171],wf$AU_AG_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.AU_AGReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.AU_AGReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.AU_AGReps1[[paste0(x)]][c(1:170)] = (wf_df.AU_AGReps1[[paste0(x)]][c(2:171)] + wf$AU_AG_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.AU_AGReps2 = data.frame(wf$YEAR5.2,wf$AU_AG_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.AU_AGReps2[[paste0(x)]] = fiveyrfun2(wf_df.AU_AGReps2[[paste0(x)]], wf_df.AU_AGReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.AU_AGReps3 = data.frame(subset(wf_df.AU_AG, select = c(a.wf.AU_AG, b.wf.AU_AG, c.wf.AU_AG))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.AU_AGReps3[[paste0(x)]] = wf_df.AU_AGReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                  81,86,91,96,101,106,111,116,121,126,131,136,
                                                                  141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.AU_AG_final = DropNA(wf_df.AU_AGReps3)
wf.AU_AG_final$Mean = rowMeans(wf.AU_AG_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.AU_AG = autoplot(zoo(wf.AU_AG_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Aust-Agder") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.AU_AG$wf_yr2[c(35)], y=wf_df.AU_AG$a[c(35)],
           color="black") +
  annotate("text", x=35, y=3, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.AU_AG[c(1)]) - 2), 
           label = round(a.wf.AU_AG[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.AU_AG[c(1)]) - 2), 
           label = round(b.wf.AU_AG[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.AU_AG

AU_AG.png = readPNG("Red Counties/Aust_Agder.png") #Import county png

ggdraw(plot.AU_AG) + 
  draw_image(AU_AG.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####Vest-Agder


##Convert VE_AG column to 5 year average
wf$VE_AG_5y = fiveyrfun2(wf$VE_AG_5y, wf$VE_AG) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#VE_AG MPM MAX
wf$VE_AG_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$VE_AG_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$VE_AG_Max[c(1:170)]) {
  if(is.na(val)) wf$VE_AG_Max[c(1:170)] = (wf$VE_AG_Max[c(2:171)] + wf$VE_AG_5y[c(2:171)])/((1-0.05)+(0.30))
}

#VE_AG MPM MIN
wf$VE_AG_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$VE_AG_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$VE_AG_Min[c(1:170)]) {
  if(is.na(val)) wf$VE_AG_Min[c(1:170)] = (wf$VE_AG_Min[c(2:171)] + wf$VE_AG_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$VE_AG_Max5y = fiveyrfun2(wf$VE_AG_Max5y, wf$VE_AG_Max) #Make new column & fill it
wf$VE_AG_Min5y = fiveyrfun2(wf$VE_AG_Min5y, wf$VE_AG_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.VE_AG = fivelistfun2(a.wf.VE_AG, wf$VE_AG_Max5y) #Max
b.wf.VE_AG = fivelistfun2(b.wf.VE_AG, wf$VE_AG_Min5y) #Min
c.wf.VE_AG = fivelistfun2(c.wf.VE_AG, wf$VE_AG_5y) #No. harvested

wf_df.VE_AG = data.frame(a.wf.VE_AG, b.wf.VE_AG, c.wf.VE_AG, wf_yr2)




####100 Replicates

wf_df.VE_AGReps1 = data.frame(wf$YEAR5.2[1:171],wf$VE_AG_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.VE_AGReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.VE_AGReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.VE_AGReps1[[paste0(x)]][c(1:170)] = (wf_df.VE_AGReps1[[paste0(x)]][c(2:171)] + wf$VE_AG_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.VE_AGReps2 = data.frame(wf$YEAR5.2,wf$VE_AG_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.VE_AGReps2[[paste0(x)]] = fiveyrfun2(wf_df.VE_AGReps2[[paste0(x)]], wf_df.VE_AGReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.VE_AGReps3 = data.frame(subset(wf_df.VE_AG, select = c(a.wf.VE_AG, b.wf.VE_AG, c.wf.VE_AG))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.VE_AGReps3[[paste0(x)]] = wf_df.VE_AGReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                  81,86,91,96,101,106,111,116,121,126,131,136,
                                                                  141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.VE_AG_final = DropNA(wf_df.VE_AGReps3)
wf.VE_AG_final$Mean = rowMeans(wf.VE_AG_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.VE_AG = autoplot(zoo(wf.VE_AG_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Vest-Agder") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.VE_AG$wf_yr2[c(35)], y=wf_df.VE_AG$a[c(35)],
           color="black") +
  annotate("text", x=35, y=1.5, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.VE_AG[c(1)]) + 1.5), 
           label = round(a.wf.VE_AG[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.VE_AG[c(1)]) + 1), 
           label = round(b.wf.VE_AG[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.VE_AG

VE_AG.png = readPNG("Red Counties/Vest_Agder.png") #Import county png

ggdraw(plot.VE_AG) + 
  draw_image(VE_AG.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--




#####Rogaland


##Convert ROGL column to 5 year average
wf$ROGL_5y = fiveyrfun2(wf$ROGL_5y, wf$ROGL) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#ROGL MPM MAX
wf$ROGL_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$ROGL_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$ROGL_Max[c(1:170)]) {
  if(is.na(val)) wf$ROGL_Max[c(1:170)] = (wf$ROGL_Max[c(2:171)] + wf$ROGL_5y[c(2:171)])/((1-0.05)+(0.30))
}

#ROGL MPM MIN
wf$ROGL_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$ROGL_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$ROGL_Min[c(1:170)]) {
  if(is.na(val)) wf$ROGL_Min[c(1:170)] = (wf$ROGL_Min[c(2:171)] + wf$ROGL_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$ROGL_Max5y = fiveyrfun2(wf$ROGL_Max5y, wf$ROGL_Max) #Make new column & fill it
wf$ROGL_Min5y = fiveyrfun2(wf$ROGL_Min5y, wf$ROGL_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.ROGL = fivelistfun2(a.wf.ROGL, wf$ROGL_Max5y) #Max
b.wf.ROGL = fivelistfun2(b.wf.ROGL, wf$ROGL_Min5y) #Min
c.wf.ROGL = fivelistfun2(c.wf.ROGL, wf$ROGL_5y) #No. harvested

wf_df.ROGL = data.frame(a.wf.ROGL, b.wf.ROGL, c.wf.ROGL, wf_yr2)




####100 Replicates

wf_df.ROGLReps1 = data.frame(wf$YEAR5.2[1:171],wf$ROGL_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.ROGLReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.ROGLReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.ROGLReps1[[paste0(x)]][c(1:170)] = (wf_df.ROGLReps1[[paste0(x)]][c(2:171)] + wf$ROGL_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.ROGLReps2 = data.frame(wf$YEAR5.2,wf$ROGL_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.ROGLReps2[[paste0(x)]] = fiveyrfun2(wf_df.ROGLReps2[[paste0(x)]], wf_df.ROGLReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.ROGLReps3 = data.frame(subset(wf_df.ROGL, select = c(a.wf.ROGL, b.wf.ROGL, c.wf.ROGL))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.ROGLReps3[[paste0(x)]] = wf_df.ROGLReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.ROGL_final = DropNA(wf_df.ROGLReps3)
wf.ROGL_final$Mean = rowMeans(wf.ROGL_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.ROGL = autoplot(zoo(wf.ROGL_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Rogaland") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.ROGL$wf_yr2[c(35)], y=wf_df.ROGL$a[c(35)],
           color="black") +
  annotate("text", x=35, y=1.5, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.ROGL[c(1)]) + 1.5), 
           label = round(a.wf.ROGL[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.ROGL[c(1)]) - 0.8), 
           label = round(b.wf.ROGL[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.ROGL

ROGL.png = readPNG("Red Counties/Rogaland.png") #Import county png

ggdraw(plot.ROGL) + 
  draw_image(ROGL.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--





#####Hordaland


##Convert HORD column to 5 year average
wf$HORD_5y = fiveyrfun2(wf$HORD_5y, wf$HORD) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#HORD MPM MAX
wf$HORD_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$HORD_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$HORD_Max[c(1:170)]) {
  if(is.na(val)) wf$HORD_Max[c(1:170)] = (wf$HORD_Max[c(2:171)] + wf$HORD_5y[c(2:171)])/((1-0.05)+(0.30))
}

#HORD MPM MIN
wf$HORD_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$HORD_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$HORD_Min[c(1:170)]) {
  if(is.na(val)) wf$HORD_Min[c(1:170)] = (wf$HORD_Min[c(2:171)] + wf$HORD_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$HORD_Max5y = fiveyrfun2(wf$HORD_Max5y, wf$HORD_Max) #Make new column & fill it
wf$HORD_Min5y = fiveyrfun2(wf$HORD_Min5y, wf$HORD_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.HORD = fivelistfun2(a.wf.HORD, wf$HORD_Max5y) #Max
b.wf.HORD = fivelistfun2(b.wf.HORD, wf$HORD_Min5y) #Min
c.wf.HORD = fivelistfun2(c.wf.HORD, wf$HORD_5y) #No. harvested

wf_df.HORD = data.frame(a.wf.HORD, b.wf.HORD, c.wf.HORD, wf_yr2)




####100 Replicates

wf_df.HORDReps1 = data.frame(wf$YEAR5.2[1:171],wf$HORD_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.HORDReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.HORDReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.HORDReps1[[paste0(x)]][c(1:170)] = (wf_df.HORDReps1[[paste0(x)]][c(2:171)] + wf$HORD_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.HORDReps2 = data.frame(wf$YEAR5.2,wf$HORD_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.HORDReps2[[paste0(x)]] = fiveyrfun2(wf_df.HORDReps2[[paste0(x)]], wf_df.HORDReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.HORDReps3 = data.frame(subset(wf_df.HORD, select = c(a.wf.HORD, b.wf.HORD, c.wf.HORD))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.HORDReps3[[paste0(x)]] = wf_df.HORDReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.HORD_final = DropNA(wf_df.HORDReps3)
wf.HORD_final$Mean = rowMeans(wf.HORD_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.HORD = autoplot(zoo(wf.HORD_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Hordaland") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   expand = expansion(add = c(1,0.5))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.HORD$wf_yr2[c(35)], y=wf_df.HORD$a[c(35)],
           color="black") +
  annotate("text", x=35, y=0.08, 
           label = "0",
           color = "black") +
  annotate("text", x=0.65, y=(round(a.wf.HORD[c(1)], digits=1) - 0.05), 
           label = round(a.wf.HORD[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.HORD[c(1)], digits=1) - 0.05), 
           label = round(b.wf.HORD[c(1)], digits=1),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.HORD

HORD.png = readPNG("Red Counties/Hordaland.png") #Import county png

ggdraw(plot.HORD) + 
  draw_image(HORD.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--









#####Sogn og Fjordane


##Convert SOGN column to 5 year average
wf$SOGN_5y = fiveyrfun2(wf$SOGN_5y, wf$SOGN) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#SOGN MPM MAX
wf$SOGN_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$SOGN_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$SOGN_Max[c(1:170)]) {
  if(is.na(val)) wf$SOGN_Max[c(1:170)] = (wf$SOGN_Max[c(2:171)] + wf$SOGN_5y[c(2:171)])/((1-0.05)+(0.30))
}

#SOGN MPM MIN
wf$SOGN_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$SOGN_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$SOGN_Min[c(1:170)]) {
  if(is.na(val)) wf$SOGN_Min[c(1:170)] = (wf$SOGN_Min[c(2:171)] + wf$SOGN_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$SOGN_Max5y = fiveyrfun2(wf$SOGN_Max5y, wf$SOGN_Max) #Make new column & fill it
wf$SOGN_Min5y = fiveyrfun2(wf$SOGN_Min5y, wf$SOGN_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.SOGN = fivelistfun2(a.wf.SOGN, wf$SOGN_Max5y) #Max
b.wf.SOGN = fivelistfun2(b.wf.SOGN, wf$SOGN_Min5y) #Min
c.wf.SOGN = fivelistfun2(c.wf.SOGN, wf$SOGN_5y) #No. harvested

wf_df.SOGN = data.frame(a.wf.SOGN, b.wf.SOGN, c.wf.SOGN, wf_yr2)




####100 Replicates

wf_df.SOGNReps1 = data.frame(wf$YEAR5.2[1:171],wf$SOGN_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.SOGNReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.SOGNReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.SOGNReps1[[paste0(x)]][c(1:170)] = (wf_df.SOGNReps1[[paste0(x)]][c(2:171)] + wf$SOGN_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.SOGNReps2 = data.frame(wf$YEAR5.2,wf$SOGN_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.SOGNReps2[[paste0(x)]] = fiveyrfun2(wf_df.SOGNReps2[[paste0(x)]], wf_df.SOGNReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.SOGNReps3 = data.frame(subset(wf_df.SOGN, select = c(a.wf.SOGN, b.wf.SOGN, c.wf.SOGN))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.SOGNReps3[[paste0(x)]] = wf_df.SOGNReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.SOGN_final = DropNA(wf_df.SOGNReps3)
wf.SOGN_final$Mean = rowMeans(wf.SOGN_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.SOGN = autoplot(zoo(wf.SOGN_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Sogn og Fjordane") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.SOGN$wf_yr2[c(35)], y=wf_df.SOGN$a[c(35)],
           color="black") +
  annotate("text", x=35, y=1.2, 
           label = "0",
           color = "black") +
  annotate("text", x=1.2, y=(round(a.wf.SOGN[c(1)]) - 0.5), 
           label = round(a.wf.SOGN[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.SOGN[c(1)]) - 1), 
           label = round(b.wf.SOGN[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.SOGN

SOGN.png = readPNG("Red Counties/Sogn_og_Fjordane.png") #Import county png

ggdraw(plot.SOGN) + 
  draw_image(SOGN.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--








#####MOEre og Romsdal


##Convert MOERE column to 5 year average
wf$MOERE_5y = fiveyrfun2(wf$MOERE_5y, wf$MOERE) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#MOERE MPM MAX
wf$MOERE_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$MOERE_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$MOERE_Max[c(1:170)]) {
  if(is.na(val)) wf$MOERE_Max[c(1:170)] = (wf$MOERE_Max[c(2:171)] + wf$MOERE_5y[c(2:171)])/((1-0.05)+(0.30))
}

#MOERE MPM MIN
wf$MOERE_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$MOERE_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$MOERE_Min[c(1:170)]) {
  if(is.na(val)) wf$MOERE_Min[c(1:170)] = (wf$MOERE_Min[c(2:171)] + wf$MOERE_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$MOERE_Max5y = fiveyrfun2(wf$MOERE_Max5y, wf$MOERE_Max) #Make new column & fill it
wf$MOERE_Min5y = fiveyrfun2(wf$MOERE_Min5y, wf$MOERE_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.MOERE = fivelistfun2(a.wf.MOERE, wf$MOERE_Max5y) #Max
b.wf.MOERE = fivelistfun2(b.wf.MOERE, wf$MOERE_Min5y) #Min
c.wf.MOERE = fivelistfun2(c.wf.MOERE, wf$MOERE_5y) #No. harvested

wf_df.MOERE = data.frame(a.wf.MOERE, b.wf.MOERE, c.wf.MOERE, wf_yr2)




####100 Replicates

wf_df.MOEREReps1 = data.frame(wf$YEAR5.2[1:171],wf$MOERE_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.MOEREReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.MOEREReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.MOEREReps1[[paste0(x)]][c(1:170)] = (wf_df.MOEREReps1[[paste0(x)]][c(2:171)] + wf$MOERE_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.MOEREReps2 = data.frame(wf$YEAR5.2,wf$MOERE_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.MOEREReps2[[paste0(x)]] = fiveyrfun2(wf_df.MOEREReps2[[paste0(x)]], wf_df.MOEREReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.MOEREReps3 = data.frame(subset(wf_df.MOERE, select = c(a.wf.MOERE, b.wf.MOERE, c.wf.MOERE))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.MOEREReps3[[paste0(x)]] = wf_df.MOEREReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.MOERE_final = DropNA(wf_df.MOEREReps3)
wf.MOERE_final$Mean = rowMeans(wf.MOERE_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.MOERE = autoplot(zoo(wf.MOERE_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("MOEre og Romsdal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.MOERE$wf_yr2[c(35)], y=wf_df.MOERE$a[c(35)],
           color="black") +
  annotate("text", x=35, y=1.5, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.MOERE[c(1)]) + 0.5), 
           label = round(a.wf.MOERE[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=0.9, y=(round(b.wf.MOERE[c(1)]) + 0.5), 
           label = round(b.wf.MOERE[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.MOERE

MOERE.png = readPNG("Red Counties/MOEre_og_Romsdal.png") #Import county png

ggdraw(plot.MOERE) + 
  draw_image(MOERE.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####SOEr-TrOEndelag


##Convert SOERT column to 5 year average
wf$SOERT_5y = fiveyrfun2(wf$SOERT_5y, wf$SOERT) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#SOERT MPM MAX
wf$SOERT_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$SOERT_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$SOERT_Max[c(1:170)]) {
  if(is.na(val)) wf$SOERT_Max[c(1:170)] = (wf$SOERT_Max[c(2:171)] + wf$SOERT_5y[c(2:171)])/((1-0.05)+(0.30))
}

#SOERT MPM MIN
wf$SOERT_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$SOERT_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$SOERT_Min[c(1:170)]) {
  if(is.na(val)) wf$SOERT_Min[c(1:170)] = (wf$SOERT_Min[c(2:171)] + wf$SOERT_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$SOERT_Max5y = fiveyrfun2(wf$SOERT_Max5y, wf$SOERT_Max) #Make new column & fill it
wf$SOERT_Min5y = fiveyrfun2(wf$SOERT_Min5y, wf$SOERT_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.SOERT = fivelistfun2(a.wf.SOERT, wf$SOERT_Max5y) #Max
b.wf.SOERT = fivelistfun2(b.wf.SOERT, wf$SOERT_Min5y) #Min
c.wf.SOERT = fivelistfun2(c.wf.SOERT, wf$SOERT_5y) #No. harvested

wf_df.SOERT = data.frame(a.wf.SOERT, b.wf.SOERT, c.wf.SOERT, wf_yr2)




####100 Replicates

wf_df.SOERTReps1 = data.frame(wf$YEAR5.2[1:171],wf$SOERT_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.SOERTReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.SOERTReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.SOERTReps1[[paste0(x)]][c(1:170)] = (wf_df.SOERTReps1[[paste0(x)]][c(2:171)] + wf$SOERT_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.SOERTReps2 = data.frame(wf$YEAR5.2,wf$SOERT_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.SOERTReps2[[paste0(x)]] = fiveyrfun2(wf_df.SOERTReps2[[paste0(x)]], wf_df.SOERTReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.SOERTReps3 = data.frame(subset(wf_df.SOERT, select = c(a.wf.SOERT, b.wf.SOERT, c.wf.SOERT))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.SOERTReps3[[paste0(x)]] = wf_df.SOERTReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.SOERT_final = DropNA(wf_df.SOERTReps3)
wf.SOERT_final$Mean = rowMeans(wf.SOERT_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.SOERT = autoplot(zoo(wf.SOERT_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("SOEr-TrOEndelag") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.SOERT$wf_yr2[c(35)], y=wf_df.SOERT$a[c(35)],
           color="black") +
  annotate("text", x=35, y=3, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.SOERT[c(1)]) + 2), 
           label = round(a.wf.SOERT[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.SOERT[c(1)]) + 2), 
           label = round(b.wf.SOERT[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.SOERT

SOERT.png = readPNG("Red Counties/SOEr_TrOEndelag.png") #Import county png

ggdraw(plot.SOERT) + 
  draw_image(SOERT.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--



#####Nord-TrOEndelag


##Convert NORT column to 5 year average
wf$NORT_5y = fiveyrfun2(wf$NORT_5y, wf$NORT) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#NORT MPM MAX
wf$NORT_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$NORT_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$NORT_Max[c(1:170)]) {
  if(is.na(val)) wf$NORT_Max[c(1:170)] = (wf$NORT_Max[c(2:171)] + wf$NORT_5y[c(2:171)])/((1-0.05)+(0.30))
}

#NORT MPM MIN
wf$NORT_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$NORT_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$NORT_Min[c(1:170)]) {
  if(is.na(val)) wf$NORT_Min[c(1:170)] = (wf$NORT_Min[c(2:171)] + wf$NORT_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$NORT_Max5y = fiveyrfun2(wf$NORT_Max5y, wf$NORT_Max) #Make new column & fill it
wf$NORT_Min5y = fiveyrfun2(wf$NORT_Min5y, wf$NORT_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.NORT = fivelistfun2(a.wf.NORT, wf$NORT_Max5y) #Max
b.wf.NORT = fivelistfun2(b.wf.NORT, wf$NORT_Min5y) #Min
c.wf.NORT = fivelistfun2(c.wf.NORT, wf$NORT_5y) #No. harvested

wf_df.NORT = data.frame(a.wf.NORT, b.wf.NORT, c.wf.NORT, wf_yr2)




####100 Replicates

wf_df.NORTReps1 = data.frame(wf$YEAR5.2[1:171],wf$NORT_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.NORTReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.NORTReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.NORTReps1[[paste0(x)]][c(1:170)] = (wf_df.NORTReps1[[paste0(x)]][c(2:171)] + wf$NORT_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.NORTReps2 = data.frame(wf$YEAR5.2,wf$NORT_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.NORTReps2[[paste0(x)]] = fiveyrfun2(wf_df.NORTReps2[[paste0(x)]], wf_df.NORTReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.NORTReps3 = data.frame(subset(wf_df.NORT, select = c(a.wf.NORT, b.wf.NORT, c.wf.NORT))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.NORTReps3[[paste0(x)]] = wf_df.NORTReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.NORT_final = DropNA(wf_df.NORTReps3)
wf.NORT_final$Mean = rowMeans(wf.NORT_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.NORT = autoplot(zoo(wf.NORT_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Nord-TrOEndelag") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.NORT$wf_yr2[c(35)], y=wf_df.NORT$a[c(35)],
           color="black") +
  annotate("text", x=35, y=2, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.NORT[c(1)]) + 1), 
           label = round(a.wf.NORT[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.NORT[c(1)]) + 1), 
           label = round(b.wf.NORT[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.NORT

NORT.png = readPNG("Red Counties/Nord_TrOEndelag.png") #Import county png

ggdraw(plot.NORT) + 
  draw_image(NORT.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--




#####Nordland


##Convert NORD column to 5 year average
wf$NORD_5y = fiveyrfun2(wf$NORD_5y, wf$NORD) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#NORD MPM MAX
wf$NORD_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$NORD_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$NORD_Max[c(1:170)]) {
  if(is.na(val)) wf$NORD_Max[c(1:170)] = (wf$NORD_Max[c(2:171)] + wf$NORD_5y[c(2:171)])/((1-0.05)+(0.30))
}

#NORD MPM MIN
wf$NORD_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$NORD_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$NORD_Min[c(1:170)]) {
  if(is.na(val)) wf$NORD_Min[c(1:170)] = (wf$NORD_Min[c(2:171)] + wf$NORD_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$NORD_Max5y = fiveyrfun2(wf$NORD_Max5y, wf$NORD_Max) #Make new column & fill it
wf$NORD_Min5y = fiveyrfun2(wf$NORD_Min5y, wf$NORD_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.NORD = fivelistfun2(a.wf.NORD, wf$NORD_Max5y) #Max
b.wf.NORD = fivelistfun2(b.wf.NORD, wf$NORD_Min5y) #Min
c.wf.NORD = fivelistfun2(c.wf.NORD, wf$NORD_5y) #No. harvested

wf_df.NORD = data.frame(a.wf.NORD, b.wf.NORD, c.wf.NORD, wf_yr2)




####100 Replicates

wf_df.NORDReps1 = data.frame(wf$YEAR5.2[1:171],wf$NORD_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.NORDReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.NORDReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.NORDReps1[[paste0(x)]][c(1:170)] = (wf_df.NORDReps1[[paste0(x)]][c(2:171)] + wf$NORD_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.NORDReps2 = data.frame(wf$YEAR5.2,wf$NORD_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.NORDReps2[[paste0(x)]] = fiveyrfun2(wf_df.NORDReps2[[paste0(x)]], wf_df.NORDReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.NORDReps3 = data.frame(subset(wf_df.NORD, select = c(a.wf.NORD, b.wf.NORD, c.wf.NORD))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.NORDReps3[[paste0(x)]] = wf_df.NORDReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.NORD_final = DropNA(wf_df.NORDReps3)
wf.NORD_final$Mean = rowMeans(wf.NORD_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.NORD = autoplot(zoo(wf.NORD_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Nordland") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   expand = expansion(add = c(1,0.5))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.NORD$wf_yr2[c(35)], y=wf_df.NORD$a[c(35)],
           color="black") +
  annotate("text", x=35, y=2, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.NORD[c(1)]) + 1.5), 
           label = round(a.wf.NORD[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=0.5, y=(round(b.wf.NORD[c(1)]) + 0.8), 
           label = round(b.wf.NORD[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.NORD

NORD.png = readPNG("Red Counties/Nordland.png") #Import county png

ggdraw(plot.NORD) + 
  draw_image(NORD.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--




#####Troms


##Convert TROM column to 5 year average
wf$TROM_5y = fiveyrfun2(wf$TROM_5y, wf$TROM) #Make new column & fill it


##Mykrä & Pohja-Mykrä Model

#TROM MPM MAX
wf$TROM_Max[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$TROM_Max[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$TROM_Max[c(1:170)]) {
  if(is.na(val)) wf$TROM_Max[c(1:170)] = (wf$TROM_Max[c(2:171)] + wf$TROM_5y[c(2:171)])/((1-0.05)+(0.30))
}

#TROM MPM MIN
wf$TROM_Min[c(176)] = NA #Make new column & set the year 2020-21 to NA
wf$TROM_Min[c(171)] = 0 #Set the year 2014-15 based on RovData

#Set the years 2019-20 to 1846:
for (val in wf$TROM_Min[c(1:170)]) {
  if(is.na(val)) wf$TROM_Min[c(1:170)] = (wf$TROM_Min[c(2:171)] + wf$TROM_5y[c(2:171)])/((1-0.01)+(0.45))
}  

##--

##Convert Max & Min columns to 5 year average
wf$TROM_Max5y = fiveyrfun2(wf$TROM_Max5y, wf$TROM_Max) #Make new column & fill it
wf$TROM_Min5y = fiveyrfun2(wf$TROM_Min5y, wf$TROM_Min) #Make new column & fill it

##Create data frame with just the data we want
a.wf.TROM = fivelistfun2(a.wf.TROM, wf$TROM_Max5y) #Max
b.wf.TROM = fivelistfun2(b.wf.TROM, wf$TROM_Min5y) #Min
c.wf.TROM = fivelistfun2(c.wf.TROM, wf$TROM_5y) #No. harvested

wf_df.TROM = data.frame(a.wf.TROM, b.wf.TROM, c.wf.TROM, wf_yr2)




####100 Replicates

wf_df.TROMReps1 = data.frame(wf$YEAR5.2[1:171],wf$TROM_5y[1:171]) #Make new data frame

##Make 100 new columns & set the year 2020-21 based on RovData
x = 1
repeat {
  wf_df.TROMReps1[[paste0(x)]][c(171)] = 0
  x = x + 1
  if (x == 101){
    break
  }
}

#Set the years 2019-20 to 1846:
for (x in  1:100) {
  for (val in wf_df.TROMReps1[[paste0(x)]][c(1:170)]) {
    if(is.na(val)) wf_df.TROMReps1[[paste0(x)]][c(1:170)] = (wf_df.TROMReps1[[paste0(x)]][c(2:171)] + wf$TROM_5y[c(2:171)])/((1-(wf_m[[paste0(x)]][c(2:171)]))+(wf_r[[paste0(x)]][c(2:171)]))
  }
}

##--
wf_df.TROMReps2 = data.frame(wf$YEAR5.2,wf$TROM_5y) #Make new data frame

#Make 100 new columns & fill them
x = 1
repeat {
  wf_df.TROMReps2[[paste0(x)]] = fiveyrfun2(wf_df.TROMReps2[[paste0(x)]], wf_df.TROMReps1[[paste0(x)]])
  x = x + 1
  if (x == 101){
    break
  }
}

wf_df.TROMReps3 = data.frame(subset(wf_df.TROM, select = c(a.wf.TROM, b.wf.TROM, c.wf.TROM))) #Make new data frame

#Add 100 replicates
x = 1
repeat {
  wf_df.TROMReps3[[paste0(x)]] = wf_df.TROMReps2[[paste0(x)]][c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,
                                                                81,86,91,96,101,106,111,116,121,126,131,136,
                                                                141,146,151,156,161,166,171)]
  x = x + 1
  if (x == 101){
    break
  }
}

wf.TROM_final = DropNA(wf_df.TROMReps3)
wf.TROM_final$Mean = rowMeans(wf.TROM_final[c(4:103)]) #Make new column with mean of 100 replicates

#WOLF PLOT
plot.TROM = autoplot(zoo(wf.TROM_final, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Troms") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  annotate("point", x=wf_df.TROM$wf_yr2[c(35)], y=wf_df.TROM$a[c(35)],
           color="black") +
  annotate("text", x=35, y=4, 
           label = "0",
           color = "black") +
  annotate("text", x=1, y=(round(a.wf.TROM[c(1)]) - 2), 
           label = round(a.wf.TROM[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  annotate("text", x=1, y=(round(b.wf.TROM[c(1)]) - 2), 
           label = round(b.wf.TROM[c(1)]),
           color = "#009E73",
           size = 3.1,
           fontface = "bold") +
  scale_color_manual(values = myColors) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes) +
  scale_linetype_manual(values = myLinetypes)

plot.TROM

TROM.png = readPNG("Red Counties/Troms.png") #Import county png

ggdraw(plot.TROM) + 
  draw_image(TROM.png, x = 0.58, y = 0.225, width = 0.4) #x=from the left, y=from the bottom
##--






##Grid with line plots at county level
wf.OEst.png = readPNG("wf plots/wf OEst.png")
wf.aker.png = readPNG("wf plots/wf aker.png")
wf.hed.png = readPNG("wf plots/wf hed.png")
wf.oppl.png = readPNG("wf plots/wf oppl.png")
wf.busk.png = readPNG("wf plots/wf busk.png")
wf.vest.png = readPNG("wf plots/wf vest.png")
wf.telm.png = readPNG("wf plots/wf telm.png")
wf.au_ag.png = readPNG("wf plots/wf au_ag.png")
wf.ve_ag.png = readPNG("wf plots/wf ve_ag.png")
wf.rogl.png = readPNG("wf plots/wf rogl.png")
wf.hord.png = readPNG("wf plots/wf hord.png")
wf.sogn.png = readPNG("wf plots/wf sogn.png")
wf.mOEre.png = readPNG("wf plots/wf mOEre.png")
wf.sOErt.png = readPNG("wf plots/wf sOErt.png")
wf.nort.png = readPNG("wf plots/wf nort.png")
wf.nord.png = readPNG("wf plots/wf nord.png")
wf.trom.png = readPNG("wf plots/wf trom.png")
wf.finn.png = readPNG("wf plots/wf finn.png")
wf.total.png = readPNG("wf plots/wf total.png")

grid.arrange(rasterGrob(wf.OEst.png),rasterGrob(wf.aker.png),rasterGrob(wf.hed.png),
             rasterGrob(wf.oppl.png),rasterGrob(wf.busk.png),rasterGrob(wf.vest.png),
             rasterGrob(wf.telm.png),rasterGrob(wf.au_ag.png),rasterGrob(wf.ve_ag.png),
             rasterGrob(wf.rogl.png),rasterGrob(wf.hord.png),rasterGrob(wf.sogn.png),
             rasterGrob(wf.mOEre.png),rasterGrob(wf.sOErt.png),rasterGrob(wf.nort.png),
             rasterGrob(wf.nord.png),rasterGrob(wf.trom.png),rasterGrob(wf.finn.png),
             ncol=3, 
             top=textGrob("Wolf Population Estimates, County Level",just="center",
                          gp=gpar(fontsize=40)))
#bottom=textGrob("Description...", x=0.07,y=0.8,
#gp=gpar(fontsize=15)))



#Write csvs the first time through (for all county line graph and individual maps)
wf_df.FINNReps3$Mean = rowMeans(wf_df.FINNReps3[c(4:103)]) #Make new column with mean of 100 replicates

WOLF_DATAFRAME = data.frame(wf.AKER_final$Mean,
                            wf.AU_AG_final$Mean,
                            wf.BUSK_final$Mean,
                            wf_df.FINNReps3$Mean,
                            wf.HED_final$Mean,
                            wf.HORD_final$Mean,
                            wf.MOERE_final$Mean,
                            wf.NORD_final$Mean,
                            wf.NORT_final$Mean,
                            wf.OPPL_final$Mean,
                            wf.OEST_final$Mean,
                            wf.ROGL_final$Mean,
                            wf.SOGN_final$Mean,
                            wf.SOERT_final$Mean,
                            wf.TELM_final$Mean,
                            wf.TROM_final$Mean,
                            wf.VE_AG_final$Mean,
                            wf.VEST_final$Mean,
                            wf_yr2)


names(WOLF_DATAFRAME)
names(WOLF_DATAFRAME)[1] = "Akershus"
names(WOLF_DATAFRAME)[2] = "Aust-Agder"
names(WOLF_DATAFRAME)[3] = "Buskerud"
names(WOLF_DATAFRAME)[4] = "Finnmark"
names(WOLF_DATAFRAME)[5] = "Hedmark"
names(WOLF_DATAFRAME)[6] = "Hordaland"
names(WOLF_DATAFRAME)[7] = "MOEre og Romsdal"
names(WOLF_DATAFRAME)[8] =  "Nordland"
names(WOLF_DATAFRAME)[9] =  "Nord-TrOEndelag"
names(WOLF_DATAFRAME)[10] =  "Oppland"
names(WOLF_DATAFRAME)[11] =  "OEstfold"
names(WOLF_DATAFRAME)[12] =  "Rogaland"
names(WOLF_DATAFRAME)[13] =  "Sogn og Fjordane"
names(WOLF_DATAFRAME)[14] =  "SOEr-TrOEndelag"
names(WOLF_DATAFRAME)[15] =  "Telemark"
names(WOLF_DATAFRAME)[16] =  "Troms"
names(WOLF_DATAFRAME)[17] =  "Vest-Agder"
names(WOLF_DATAFRAME)[18] =  "Vestfold"
names(WOLF_DATAFRAME)[19] =  "Year"

#write.csv(WOLF_DATAFRAME, "wf_meanvalues1.csv") #for all county line graph

WOLF_DATAFRAME_T = as.data.frame(t(WOLF_DATAFRAME)) #transpose

names(WOLF_DATAFRAME_T)[1:35] = c("1846-50","1851-55","1856-60","1861-65","1866-70","1871-75","1876-80",
                                  "1881-85","1886-90","1891-95","1896-1900","1901-05","1906-10","1911-15",
                                  "1916-20","1921-25","1926-30","1931-35","1936-40","1941-45","1946-50",
                                  "1951-55","1956-60","1961-65","1966-70","1971-75","1976-80",
                                  "1980/81-84/85","1985/86-89/90","1990/91-94/95","1995/96-99/00",
                                  "2000/01-04/05","2005/06-09/10", "2010/11-14/15","2016")

setDT(WOLF_DATAFRAME_T, keep.rownames = TRUE)[]
names(WOLF_DATAFRAME_T)[1] = "County"

WOLF_DATAFRAME_T[19,] = NA
WOLF_DATAFRAME_T$County[19] = "Oslo"

#write.csv(WOLF_DATAFRAME_T, "wf_meanvalues2.csv") #for maps


##All County Line Graph
wf_meanvalues1 = read.csv("wf_meanvalues1.csv", header=T) 
wf_meanvalues1 = subset(wf_meanvalues1, select = -c(X))

wf_all_counties = data.frame(subset(wf_meanvalues1[1:18])) #Make new data frame
names(wf_all_counties)[2] = "Aust-Agder"
names(wf_all_counties)[7] = "MOEre og Romsdal"
names(wf_all_counties)[9] =  "Nord-TrOEndelag"
names(wf_all_counties)[13] =  "Sogn og Fjordane"
names(wf_all_counties)[14] =  "SOEr-TrOEndelag"
names(wf_all_counties)[17] =  "Vest-Agder"

myColors.AllCounty = c("#FF767D","#FF767D","#FF767D","#C81B2E","#C81B2E","#C81B2E","#69000C","#69000C",
                       "#69000C","#FF767D","#FF767D","#FF767D","#C81B2E","#C81B2E","#C81B2E","#69000C",
                       "#69000C","#69000C")
myLinetypes.AllCounty = c(5,3,1,5,3,1,5,3,1,5,3,1,5,3,1,5,3,1)
mySizes.AllCounty = c(0.3,0.5,0.3,0.3,0.5,0.3,0.3,0.5,0.3,0.75,0.8,0.75,0.75,0.8,0.75,0.75,0.8,0.75)

autoplot(zoo(wf_all_counties, wf_yr2[1:35]), facets = NULL) +
  xlab("Year") + 
  ylab("Individuals") + 
  ggtitle("Wolf Population Estimate, All Counties") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle=67.5,
                                   hjust=1)) +
  scale_color_manual(values = myColors.AllCounty) +
  aes(size = Series, linetype = Series) +
  scale_size_manual(values = mySizes.AllCounty) +
  scale_linetype_manual(values = myLinetypes.AllCounty)
#####-----




##Maps & Gif
wf_meanvalues2 = read.csv("wf_meanvalues2.csv", header=T) 
wf_meanvalues2 = subset(wf_meanvalues2, select = -c(X))

norway= getData('GADM',country='NOR',level=1)
norway$NAME_1
norway$NAME_1[2] = "OEstfold"

merged = merge(norway, wf_meanvalues2, by.x= "NAME_1", by.y= "County")

Finnmark = subset(norway, NAME_1 == "Finnmark")
Oslo = subset(norway, NAME_1 == "Oslo")

#dev.off()

#Make figure with 4 plots
names1= c("1846-50", "1866-70", "1936-40", "2016")
spplot(merged, c(11,15,29,45), as.table=TRUE, col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), 
       at = seq(0,125,5), main="Wolf", names.attr = names1,
       sp.layout= c((list(list(Finnmark, Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey")))))

#Make figure with all plots
names2 = c("1846-50","1851-55","1856-60","1861-65","1866-70","1871-75","1876-80",
           "1881-85","1886-90","1891-95","1896-1900","1901-05","1906-10","1911-15",
           "1916-20","1921-25","1926-30","1931-35","1936-40","1941-45","1946-50",
           "1951-55","1956-60","1961-65","1966-70","1971-75","1976-80",
           "1980/81-84/85","1985/86-89/90","1990/91-94/95","1995/96-99/00",
           "2000/01-04/05","2005/06-09/10", "2010/11-14/15","2016")
spplot(merged[11:45], as.table=TRUE, col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), names.attr = names2,
       sp.layout= c((list(list(Finnmark, Oslo, first=T, fill="grey"))),
                    (list(list(Finnmark, Oslo, first=T, fill="grey"))),
                    (list(list(Finnmark, Oslo, first=T, fill="grey"))),
                    (list(list(Finnmark, Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey"))),
                    (list(list(Oslo, first=T, fill="grey")))))


#Make each plot individually with the same scale to use for an animation
spplot(merged[11], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1846-50",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[12], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1851-55",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[13], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1856-60",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[14], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1861-65",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[15], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1866-70",
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[16], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1871-75", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[17], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1876-80", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[18], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1881-85", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[19], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1886-90", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[20], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1891-95", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[21], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1896-1900", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[22], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1901-05", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[23], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1906-10", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[24], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1911-15", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[25], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1916-20", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[26], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1921-25", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[27], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1926-30", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[28], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1931-35", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[29], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1936-40", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[30], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1941-45", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[31], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1946-50", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[32], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1951-55", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[33], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1956-60", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[34], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1961-65", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[35], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1966-70", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[36], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1971-75", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[37], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1976-80", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[38], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1980/81 - 84/85", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[39], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1985/86 - 89/90", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[40], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1990/91 - 94/95", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[41], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 1995/96 - 99/00", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[42], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 2000/01 - 04/05", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[43], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 2005/06 - 09/10", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[44], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 2010/11 - 14/15", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[45], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="Wolf 2016", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))

#Create gif file
list.files(path='/Users/annasobocinski/Desktop/WD/wf_gif', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("wolf.gif") # write to current dir







#Plots with shorter titles!
spplot(merged[11], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1846-50",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[12], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1851-55",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[13], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1856-60",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[14], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1861-65",
       sp.layout= list(list(Finnmark, Oslo, first=T, fill="grey")))
spplot(merged[15], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1866-70",
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[16], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1871-75", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[17], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1876-80", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[18], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1881-85", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[19], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1886-90", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[20], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1891-95", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[21], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1896-1900", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[22], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1901-05", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[23], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1906-10", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[24], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1911-15", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[25], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1916-20", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[26], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1921-25", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[27], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1926-30", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[28], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1931-35", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[29], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1936-40", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[30], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1941-45", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[31], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1946-50", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[32], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1951-55", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[33], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1956-60", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[34], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1961-65", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[35], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1966-70", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[36], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1971-75", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[37], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1976-80", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[38], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1980/81 - 84/85", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[39], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1985/86 - 89/90", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[40], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1990/91 - 94/95", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[41], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="1995/96 - 99/00", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[42], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="2000/01 - 04/05", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[43], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="2005/06 - 09/10", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[44], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="2010/11 - 14/15", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))
spplot(merged[45], col.regions= rev(paletteer_c("grDevices::Reds 3", 30)), at = seq(0,125,5), main="2016", 
       sp.layout= list(list(Oslo, first=T, fill="grey")))

##Grid with maps at county level
wf.1846.png = readPNG("wf_map_grid/wf 1846.png")
wf.1851.png = readPNG("wf_map_grid/wf 1851.png")
wf.1856.png = readPNG("wf_map_grid/wf 1856.png")
wf.1861.png = readPNG("wf_map_grid/wf 1861.png")
wf.1866.png = readPNG("wf_map_grid/wf 1866.png")
wf.1871.png = readPNG("wf_map_grid/wf 1871.png")
wf.1876.png = readPNG("wf_map_grid/wf 1876.png")
wf.1881.png = readPNG("wf_map_grid/wf 1881.png")
wf.1886.png = readPNG("wf_map_grid/wf 1886.png")
wf.1891.png = readPNG("wf_map_grid/wf 1891.png")
wf.1896.png = readPNG("wf_map_grid/wf 1896.png")
wf.1901.png = readPNG("wf_map_grid/wf 1901.png")
wf.1906.png = readPNG("wf_map_grid/wf 1906.png")
wf.1911.png = readPNG("wf_map_grid/wf 1911.png")
wf.1916.png = readPNG("wf_map_grid/wf 1916.png")
wf.1921.png = readPNG("wf_map_grid/wf 1921.png")
wf.1926.png = readPNG("wf_map_grid/wf 1926.png")
wf.1931.png = readPNG("wf_map_grid/wf 1931.png")
wf.1936.png = readPNG("wf_map_grid/wf 1936.png")
wf.1941.png = readPNG("wf_map_grid/wf 1941.png")
wf.1946.png = readPNG("wf_map_grid/wf 1946.png")
wf.1951.png = readPNG("wf_map_grid/wf 1951.png")
wf.1956.png = readPNG("wf_map_grid/wf 1956.png")
wf.1961.png = readPNG("wf_map_grid/wf 1961.png")
wf.1966.png = readPNG("wf_map_grid/wf 1966.png")
wf.1971.png = readPNG("wf_map_grid/wf 1971.png")
wf.1976.png = readPNG("wf_map_grid/wf 1976.png")
wf.1980.png = readPNG("wf_map_grid/wf 1980.png")
wf.1985.png = readPNG("wf_map_grid/wf 1985.png")
wf.1990.png = readPNG("wf_map_grid/wf 1990.png")
wf.1995.png = readPNG("wf_map_grid/wf 1995.png")
wf.2000.png = readPNG("wf_map_grid/wf 2000.png")
wf.2005.png = readPNG("wf_map_grid/wf 2005.png")
wf.2010.png = readPNG("wf_map_grid/wf 2010.png")
wf.2016.png = readPNG("wf_map_grid/wf 2016.png")

grid.arrange(rasterGrob(wf.1846.png),rasterGrob(wf.1851.png),rasterGrob(wf.1856.png),
             rasterGrob(wf.1861.png),rasterGrob(wf.1866.png),rasterGrob(wf.1871.png),
             rasterGrob(wf.1876.png),rasterGrob(wf.1881.png),rasterGrob(wf.1886.png),
             rasterGrob(wf.1891.png),rasterGrob(wf.1896.png),rasterGrob(wf.1901.png),
             rasterGrob(wf.1906.png),rasterGrob(wf.1911.png),rasterGrob(wf.1916.png),
             rasterGrob(wf.1921.png),rasterGrob(wf.1926.png),rasterGrob(wf.1931.png),
             rasterGrob(wf.1936.png),rasterGrob(wf.1941.png),rasterGrob(wf.1946.png),
             rasterGrob(wf.1951.png),rasterGrob(wf.1956.png),rasterGrob(wf.1961.png),
             rasterGrob(wf.1966.png),rasterGrob(wf.1971.png),rasterGrob(wf.1976.png),
             rasterGrob(wf.1980.png),rasterGrob(wf.1985.png),rasterGrob(wf.1990.png),
             rasterGrob(wf.1995.png),rasterGrob(wf.2000.png),rasterGrob(wf.2005.png),
             rasterGrob(wf.2010.png),rasterGrob(wf.2016.png),
             ncol=5, 
             top=textGrob("Wolf Population Estimates, County Level",just="center",
                          gp=gpar(fontsize=40)))
#bottom=textGrob("Description...", x=0.07,y=0.8,
#gp=gpar(fontsize=15)))
